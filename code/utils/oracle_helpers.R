## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## oracle_helpers.R
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DB_TS_FMT <- "YYYY-MM-DD HH24:MI:SS.FF3 TZH:TZM"

## ---- Basic utilities --------------------------------------------------------

assert_schema <- function(x) {
  if (!is.character(x) || length(x) != 1 || !grepl("^[A-Za-z][A-Za-z0-9_$#]*$", x)) {
    stop("Invalid Oracle schema name: ", x)
  }
  toupper(x)
}

#' Run a statement that returns no rows; stop loudly on any ODBC/Oracle error.
run_sql <- function(channel, sql, what = "SQL statement") {
  res <- RODBC::sqlQuery(channel, sql)
  failed <- identical(res, -1L) || (is.character(res) && length(res) > 0)
  if (failed) {
    stop(sprintf(
      "%s failed:\n%s\n--- SQL ---\n%s",
      what, paste(res, collapse = "\n"), sql
    ), call. = FALSE)
  }
  invisible(res)
}

#' Current database time as a string that Oracle can parse back (with time zone).
get_db_time <- function(channel) {
  res <- RODBC::sqlQuery(
    channel,
    sprintf("SELECT TO_CHAR(SYSTIMESTAMP, '%s') AS T FROM DUAL", DB_TS_FMT),
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(res)) stop("Could not read database time:\n", paste(res, collapse = "\n"))
  trimws(res$T[1])
}

get_table_columns <- function(channel, schema, table) {
  res <- RODBC::sqlQuery(channel, sprintf(
    "SELECT column_name FROM all_tab_columns
     WHERE owner = UPPER('%s') AND table_name = UPPER('%s') ORDER BY column_id",
    schema, table
  ), stringsAsFactors = FALSE)
  if (!is.data.frame(res)) {
    stop(
      "Could not read columns of ", schema, ".", table, ":\n",
      paste(res, collapse = "\n")
    )
  }
  res$COLUMN_NAME
}

load_current <- function(channel, schema, table) {
  res <- RODBC::sqlQuery(channel, sprintf("SELECT * FROM %s.%s", schema, toupper(table)),
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(res)) {
    stop(
      "Could not read ", schema, ".", table, ":\n",
      paste(res, collapse = "\n")
    )
  }
  res
}

#' Uppercase names, factor -> character, ASCII-transliterate, trim, and turn ""
#' into NA (Oracle stores '' as NULL, so "" vs NULL would look like a change).
#' Apply to BOTH new and current data BEFORE comparing them.
clean_for_oracle <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(
        where(is.character),
        ~ dplyr::na_if(stringr::str_trim(iconv(.x, to = "ASCII//TRANSLIT")), "")
      )
    ) %>%
    as.data.frame()
}

## ---- Delta calculation ------------------------------------------------------

prepare_delta <- function(new_df, current_df, key_cols, mode = c("upsert", "insert_only")) {
  mode <- match.arg(mode)
  key_cols <- toupper(key_cols)
  new_clean <- clean_for_oracle(new_df)
  curr_clean <- clean_for_oracle(current_df)

  target_cols <- names(new_clean)
  absent <- setdiff(c(key_cols, target_cols), names(curr_clean))
  if (length(absent)) stop("Current table is missing column(s): ", paste(absent, collapse = ", "))
  if (length(setdiff(key_cols, target_cols))) {
    stop(
      "Key column(s) missing from new data: ",
      paste(setdiff(key_cols, target_cols), collapse = ", ")
    )
  }
  curr_aligned <- curr_clean[, target_cols, drop = FALSE]

  # Harmonize types so integer/numeric/character differences don't cause phantom updates
  for (col in target_cols) {
    if (is.numeric(new_clean[[col]]) || is.numeric(curr_aligned[[col]])) {
      new_clean[[col]] <- as.numeric(new_clean[[col]])
      curr_aligned[[col]] <- as.numeric(curr_aligned[[col]])
    } else if (is.character(new_clean[[col]]) || is.character(curr_aligned[[col]])) {
      new_clean[[col]] <- as.character(new_clean[[col]])
      curr_aligned[[col]] <- as.character(curr_aligned[[col]])
    }
  }

  if (mode == "insert_only") {
    new_clean <- dplyr::distinct(new_clean)
    return(
      dplyr::anti_join(new_clean, curr_aligned, by = key_cols) %>%
        dplyr::mutate(OPERATION = "INSERT")
    )
  }

  # Keys must be unique, otherwise MERGE is ambiguous
  for (nm in c("new", "current")) {
    d <- if (nm == "new") new_clean else curr_aligned
    dups <- d %>%
      dplyr::count(dplyr::across(dplyr::all_of(key_cols))) %>%
      dplyr::filter(n > 1)
    if (nrow(dups) > 0) {
      print(utils::head(dups, 10))
      stop(sprintf("Duplicate id_keys (%s) in %s data.", paste(key_cols, collapse = ", "), nm))
    }
  }

  inserts <- dplyr::anti_join(new_clean, curr_aligned, by = key_cols) %>%
    dplyr::mutate(OPERATION = "INSERT")

  # same key, any column different (this is where ACTIVE flips land)
  updates <- dplyr::semi_join(new_clean, curr_aligned, by = key_cols) %>%
    dplyr::anti_join(curr_aligned, by = target_cols) %>%
    dplyr::mutate(OPERATION = "UPDATE")

  deletes <- dplyr::anti_join(curr_aligned, new_clean, by = key_cols) %>%
    dplyr::mutate(OPERATION = "DELETE")

  dplyr::bind_rows(inserts, updates, deletes)
}

#' Build the per-table payloads (delta + metadata) from the registry.
build_stage_payloads <- function(channel, schema, registry, envir = globalenv(),
                                 max_delete_frac = 0.05, allow_large_deletes = FALSE) {
  schema <- assert_schema(schema)
  payloads <- list()

  for (tbl in names(registry)) {
    spec <- registry[[tbl]]

    if (!exists(spec$new, envir = envir)) {
      stop(sprintf("[%s] R object '%s' not found. Did steps 01-04 run?", tbl, spec$new))
    }
    new_df <- get(spec$new, envir = envir)

    current_df <- if (!is.null(spec$current) && exists(spec$current, envir = envir)) {
      get(spec$current, envir = envir)
    } else {
      message(sprintf("[%s] reading current table from Oracle", tbl))
      load_current(channel, schema, tbl)
    }

    # Catch "column not in production table" (the GROUP_CODE problem) up front
    prod_cols <- get_table_columns(channel, schema, tbl)
    extra <- setdiff(toupper(names(new_df)), prod_cols)
    if (length(extra)) {
      stop(sprintf(
        "[%s] new data has column(s) not in %s.%s: %s",
        tbl, schema, toupper(tbl), paste(extra, collapse = ", ")
      ))
    }

    delta <- prepare_delta(new_df, current_df, spec$id_keys, mode = spec$mode)

    # Guard against catastrophic deletes (e.g. an empty/partial `new` table)
    if (spec$mode == "upsert" && nrow(current_df) > 0) {
      frac <- sum(delta$OPERATION == "DELETE") / nrow(current_df)
      if (frac > max_delete_frac && !allow_large_deletes) {
        stop(sprintf(
          paste0(
            "[%s] would delete %.1f%% of existing rows (limit %.1f%%). ",
            "Check the new data, or set cfg$allow_large_deletes = TRUE."
          ),
          tbl, 100 * frac, 100 * max_delete_frac
        ))
      }
    }

    payloads[[tbl]] <- list(
      table = tbl, mode = spec$mode, id_keys = toupper(spec$id_keys),
      fast_save = if (is.null(spec$fast_save)) TRUE else spec$fast_save,
      cols = setdiff(names(delta), "OPERATION"),
      data = delta
    )
  }
  payloads
}

print_delta_summary <- function(payloads, out_dir = NULL) {
  s <- do.call(rbind, lapply(payloads, function(p) {
    data.frame(
      table   = p$table,
      insert  = sum(p$data$OPERATION == "INSERT"),
      update  = sum(p$data$OPERATION == "UPDATE"),
      delete  = sum(p$data$OPERATION == "DELETE")
    )
  }))
  rownames(s) <- NULL
  cat("\nPlanned changes:\n")
  print(s)
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    for (p in payloads) {
      if (nrow(p$data) > 0) {
        utils::write.csv(p$data, file.path(out_dir, paste0("delta_", p$table, ".csv")),
          row.names = FALSE, na = ""
        )
      }
    }
  }
  invisible(s)
}

## ---- Stage tables (permanent; truncated and reloaded each run) --------------

ensure_stage_table <- function(channel, schema, table) {
  stage <- paste0("STAGE_", toupper(table))
  expected <- c(get_table_columns(channel, schema, table), "OPERATION")
  actual <- get_table_columns(channel, schema, stage)

  if (length(actual) && !setequal(actual, expected)) {
    message(sprintf("--> %s.%s columns are out of date; recreating", schema, stage))
    run_sql(channel, sprintf("DROP TABLE %s.%s PURGE", schema, stage), "drop stage table")
    actual <- character()
  }
  if (!length(actual)) {
    message(sprintf("--> creating %s.%s", schema, stage))
    run_sql(channel, sprintf(
      "CREATE TABLE %1$s.%2$s AS
       SELECT t.*, CAST(NULL AS VARCHAR2(10)) AS OPERATION FROM %1$s.%3$s t WHERE 1 = 0",
      schema, stage, toupper(table)
    ), "create stage table")
  }
  invisible(stage)
}

load_stage_table <- function(channel, schema, payload) {
  stage <- paste0("STAGE_", toupper(payload$table))
  path <- paste0(schema, ".", stage)
  ensure_stage_table(channel, schema, payload$table)
  run_sql(channel, paste("TRUNCATE TABLE", path), paste("truncate", path))

  df <- payload$data
  if (nrow(df) == 0) {
    message("0 records to upload for ", path)
    return(invisible(0L))
  }

  res <- RODBC::sqlSave(channel, df,
    tablename = path, append = TRUE, rownames = FALSE,
    safer = FALSE, fast = payload$fast_save
  )
  if (identical(res, -1L)) stop("sqlSave failed for ", path, ":\n", RODBC::odbcGetErrMsg(channel))

  n <- RODBC::sqlQuery(channel, paste("SELECT COUNT(*) AS N FROM", path))$N[1]
  if (!identical(as.integer(n), nrow(df))) {
    stop(sprintf("%s: uploaded %d rows but table has %s", path, nrow(df), n))
  }
  message(sprintf("%d records uploaded to %s", nrow(df), path))
  invisible(nrow(df))
}

## ---- Generated MERGE SQL (replaces the hand-written merge_*.sql files) ------

# NULL-safe equality for key columns
sql_match <- function(cols, left = "target", right = "source") {
  paste(sprintf(
    "(%1$s.%3$s = %2$s.%3$s OR (%1$s.%3$s IS NULL AND %2$s.%3$s IS NULL))",
    left, right, cols
  ), collapse = "\n        AND ")
}

build_delete_sql <- function(schema, p) {
  sprintf(
    "DELETE FROM %1$s.%2$s target
     WHERE EXISTS (
       SELECT 1 FROM %1$s.STAGE_%2$s source
       WHERE source.OPERATION = 'DELETE'
         AND %3$s
     )", schema, toupper(p$table), sql_match(p$id_keys)
  )
}

build_upsert_sql <- function(schema, p) {
  tbl <- toupper(p$table)
  upd_cols <- setdiff(p$cols, p$id_keys)
  matched <- if (length(upd_cols)) {
    sprintf(
      "WHEN MATCHED THEN UPDATE SET %s\n       WHERE source.OPERATION = 'UPDATE'",
      paste(sprintf("target.%1$s = source.%1$s", upd_cols), collapse = ", ")
    )
  } else {
    ""
  } # all columns are keys (e.g. composition): nothing to update
  sprintf(
    "MERGE INTO %1$s.%2$s target
     USING (SELECT %3$s, OPERATION FROM %1$s.STAGE_%2$s
            WHERE OPERATION IN ('INSERT', 'UPDATE')) source
     ON (%4$s)
     %5$s
     WHEN NOT MATCHED THEN
       INSERT (%3$s) VALUES (%6$s)
       WHERE source.OPERATION = 'INSERT'",
    schema, tbl, paste(p$cols, collapse = ", "), sql_match(p$id_keys), matched,
    paste0("source.", p$cols, collapse = ", ")
  )
}

build_insert_only_sql <- function(schema, p) {
  sprintf(
    "INSERT INTO %1$s.%2$s (%3$s)
     SELECT %3$s FROM %1$s.STAGE_%2$s WHERE OPERATION = 'INSERT'",
    schema, toupper(p$table), paste(p$cols, collapse = ", ")
  )
}

#' Apply all changes in ONE transaction: any failure rolls back everything.
#' Deletes run child -> parent; inserts/updates run parent -> child.
apply_changes <- function(channel, schema, payloads) {
  schema <- assert_schema(schema)
  has <- function(p, op) any(p$data$OPERATION == op)

  if (!any(vapply(payloads, function(p) nrow(p$data) > 0, logical(1)))) {
    message("No records to change in any table.")
    return(invisible(FALSE))
  }

  RODBC::odbcSetAutoCommit(channel, autoCommit = FALSE)
  on.exit(try(RODBC::odbcSetAutoCommit(channel, autoCommit = TRUE), silent = TRUE), add = TRUE)

  tryCatch(
    {
      for (p in rev(payloads)) { # deletes: child -> parent
        if (p$mode == "upsert" && has(p, "DELETE")) {
          message(sprintf("Deleting from %s.%s...", schema, toupper(p$table)))
          run_sql(channel, build_delete_sql(schema, p), paste("delete", p$table))
        }
      }
      for (p in payloads) { # inserts/updates: parent -> child
        if (p$mode == "upsert" && (has(p, "INSERT") || has(p, "UPDATE"))) {
          message(sprintf("Merging into %s.%s...", schema, toupper(p$table)))
          run_sql(channel, build_upsert_sql(schema, p), paste("merge", p$table))
        } else if (p$mode == "insert_only" && has(p, "INSERT")) {
          message(sprintf("Inserting into %s.%s...", schema, toupper(p$table)))
          run_sql(channel, build_insert_only_sql(schema, p), paste("insert", p$table))
        }
      }
      RODBC::odbcEndTran(channel, commit = TRUE)
      message("Transaction committed.")
    },
    error = function(e) {
      try(RODBC::odbcEndTran(channel, commit = FALSE), silent = TRUE)
      stop("All changes rolled back.\n", conditionMessage(e), call. = FALSE)
    }
  )
  invisible(TRUE)
}

## ---- Audit tables + triggers (replaces the loop in create_audit_tables.R) ---

sync_audit_table <- function(channel, schema, table, audit_keys, rebuild = FALSE) {
  schema <- assert_schema(schema)
  table <- toupper(table)
  audit_tbl <- paste0("AUDIT_", table)
  audit_keys <- toupper(audit_keys)

  all_cols <- get_table_columns(channel, schema, table)
  if (!length(all_cols)) stop("Table not found: ", schema, ".", table)
  bad <- setdiff(audit_keys, all_cols)
  if (length(bad)) stop(sprintf("%s: audit key(s) not in table: %s", table, paste(bad, collapse = ", ")))

  key_cols <- all_cols[all_cols %in% audit_keys]
  resp_cols <- all_cols[!all_cols %in% audit_keys]
  curr_cols <- paste0(resp_cols, "_CURRENT")
  upd_cols <- paste0(resp_cols, "_UPDATE")

  # -- 1. Audit table: create, or detect drift ----------------------------------
  hdr_cols <- c("OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME")
  expected <- c(hdr_cols, key_cols, curr_cols, upd_cols)
  actual <- get_table_columns(channel, schema, audit_tbl)

  if (length(actual) && !setequal(actual, expected)) {
    msg <- sprintf(
      paste0(
        "%s.%s columns don't match %s.%s.\n  only in audit table: %s\n",
        "  missing from audit table: %s\n",
        "Set cfg$rebuild_audit <- TRUE to rename the old audit table and create a new one."
      ),
      schema, audit_tbl, schema, table,
      paste(setdiff(actual, expected), collapse = ", "),
      paste(setdiff(expected, actual), collapse = ", ")
    )
    if (!rebuild) stop(msg, call. = FALSE)
    bak <- paste0(substr(audit_tbl, 1, 18), "_BAK", format(Sys.Date(), "%y%m%d"))
    message(sprintf("--> renaming %s to %s", audit_tbl, bak))
    run_sql(channel, sprintf("ALTER TABLE %s.%s RENAME TO %s", schema, audit_tbl, bak), "rename audit table")
    actual <- character()
  }

  if (!length(actual)) {
    message(sprintf("--> creating %s.%s", schema, audit_tbl))
    pair_sql <- as.vector(rbind(
      paste0(resp_cols, " AS ", curr_cols),
      paste0(resp_cols, " AS ", upd_cols)
    ))
    run_sql(
      channel, sprintf(
        "CREATE TABLE %s.%s AS
       SELECT CAST('INSERT' AS VARCHAR2(10)) AS OPERATION_TYPE,
              SYSTIMESTAMP AS OPERATION_TIMESTAMP,
              CAST('USER' AS VARCHAR2(100)) AS USER_NAME,
              %s
       FROM %s.%s WHERE 1 = 0",
        schema, audit_tbl, paste(c(key_cols, pair_sql), collapse = ", "), schema, table
      ),
      paste("create", audit_tbl)
    )

    # CTAS copies NOT NULL; CURRENT is empty on INSERT and UPDATE on DELETE, so relax them
    nn <- RODBC::sqlQuery(channel, sprintf(
      "SELECT column_name FROM all_tab_columns
       WHERE owner = '%s' AND table_name = '%s' AND nullable = 'N'", schema, audit_tbl
    ),
    stringsAsFactors = FALSE
    )
    for (col in intersect(nn$COLUMN_NAME, c(curr_cols, upd_cols))) {
      run_sql(
        channel, sprintf("ALTER TABLE %s.%s MODIFY (%s NULL)", schema, audit_tbl, col),
        "relax NOT NULL"
      )
    }
    run_sql(channel, sprintf("GRANT SELECT ON %s.%s TO PUBLIC", schema, audit_tbl), "grant")
  }

  # -- 2. Trigger ---------------------------------------------------------------
  user_expr <- "COALESCE(SYS_CONTEXT('USERENV', 'PROXY_USER'), SYS_CONTEXT('USERENV', 'SESSION_USER'))"
  ins_stmt <- function(op, cols, vals) {
    stopifnot(length(cols) == length(vals))
    sprintf(
      "    INSERT INTO %s.%s (%s)\n    VALUES (%s);", schema, audit_tbl,
      paste(c(hdr_cols, cols), collapse = ", "),
      paste(c(sprintf("'%s'", op), "SYSTIMESTAMP", user_expr, vals), collapse = ", ")
    )
  }

  both_cols <- as.vector(rbind(curr_cols, upd_cols))
  both_vals <- as.vector(rbind(paste0(":OLD.", resp_cols), paste0(":NEW.", resp_cols)))

  trigger_ddl <- sprintf(
    "CREATE OR REPLACE TRIGGER %1$s.%2$s
AFTER INSERT OR DELETE OR UPDATE ON %1$s.%3$s
FOR EACH ROW
BEGIN
  IF INSERTING THEN
%4$s
  ELSIF DELETING THEN
%5$s
  ELSIF UPDATING THEN
%6$s
  END IF;
END;",
    schema, audit_tbl, table,
    ins_stmt("INSERT", c(key_cols, upd_cols), c(paste0(":NEW.", key_cols), paste0(":NEW.", resp_cols))),
    ins_stmt("DELETE", c(key_cols, curr_cols), c(paste0(":OLD.", key_cols), paste0(":OLD.", resp_cols))),
    ins_stmt("UPDATE", c(key_cols, both_cols), c(paste0(":NEW.", key_cols), both_vals))
  )

  RODBC::sqlQuery(channel, trigger_ddl) # compile errors are read from ALL_ERRORS below
  errs <- RODBC::sqlQuery(channel, sprintf(
    "SELECT line, position, text FROM all_errors
     WHERE owner = '%s' AND name = '%s' AND type = 'TRIGGER' ORDER BY sequence", schema, audit_tbl
  ),
  stringsAsFactors = FALSE
  )
  if (is.data.frame(errs) && nrow(errs) > 0) {
    stop(sprintf(
      "Trigger %s.%s failed to compile:\n%s", schema, audit_tbl,
      paste(sprintf("Line %s, Pos %s: %s", errs$LINE, errs$POSITION, errs$TEXT), collapse = "\n")
    ))
  }
  message(sprintf("--> %s.%s: audit table and trigger OK", schema, table))
  invisible(TRUE)
}

sync_all_audits <- function(channel, schema, registry, rebuild = FALSE) {
  for (tbl in names(registry)) {
    sync_audit_table(channel, schema, tbl, registry[[tbl]]$audit_keys, rebuild = rebuild)
  }
  invisible(TRUE)
}

## ---- Audit summary (uses the same schema + table list as the merge) ---------

summarize_all_updates <- function(channel, schema, tables, time_start, time_end) {
  schema <- assert_schema(schema)
  tables <- toupper(tables)
  sql <- paste(sprintf(
    "SELECT '%1$s' AS SCHEMA_NAME, '%2$s' AS TABLE_NAME, OPERATION_TYPE, COUNT(*) AS NUMBER_RECS
     FROM %1$s.AUDIT_%2$s
     WHERE OPERATION_TIMESTAMP BETWEEN TO_TIMESTAMP_TZ('%3$s', '%5$s')
                                   AND TO_TIMESTAMP_TZ('%4$s', '%5$s')
     GROUP BY OPERATION_TYPE",
    schema, tables, time_start, time_end, DB_TS_FMT
  ), collapse = "\nUNION ALL\n")

  res <- RODBC::sqlQuery(channel, sql, stringsAsFactors = FALSE)
  if (!is.data.frame(res)) stop("Failed to summarize audit tables:\n", paste(res, collapse = "\n"))
  res
}
