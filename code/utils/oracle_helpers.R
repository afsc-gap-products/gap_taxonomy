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

## ---- First-time table creation ---------------------------------------------

infer_oracle_type <- function(x) {
  if (is.integer(x)) {
    return("NUMBER(10)")
  }
  if (is.numeric(x)) {
    return("NUMBER")
  }
  if (is.logical(x)) {
    return("NUMBER(1)")
  }
  if (inherits(x, "POSIXct")) {
    return("TIMESTAMP")
  }
  if (inherits(x, "Date")) {
    return("DATE")
  }
  n <- suppressWarnings(max(nchar(as.character(x), type = "bytes"), na.rm = TRUE))
  if (!is.finite(n)) n <- 0
  sprintf("VARCHAR2(%d)", min(4000, max(50, ceiling(n * 2 / 50) * 50))) # ~2x headroom for growth
}

#' CREATE TABLE statement inferred from the (cleaned) new data.
#' Adds a primary key on id_keys when none of the key values are NA, and an
#' optional foreign key: references = list(cols=, table=, ref_cols=).
build_create_table_sql <- function(schema, table, df, id_keys, references = NULL) {
  table <- toupper(table)
  id_keys <- toupper(id_keys)
  key_ok <- all(vapply(id_keys, function(k) !anyNA(df[[k]]), logical(1)))

  col_sql <- vapply(names(df), function(col) {
    sprintf(
      "  %s %s%s", col, infer_oracle_type(df[[col]]),
      if (key_ok && col %in% id_keys) " NOT NULL" else ""
    )
  }, character(1))

  constraints <- character()
  if (key_ok) {
    constraints <- c(constraints, sprintf(
      "  CONSTRAINT %s PRIMARY KEY (%s)",
      substr(paste0("PK_", table), 1, 30),
      paste(id_keys, collapse = ", ")
    ))
  } else {
    message(sprintf("[%s] id_keys contain NA values: creating WITHOUT a primary key", table))
  }
  if (!is.null(references)) {
    constraints <- c(constraints, sprintf(
      "  CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s.%s (%s)",
      substr(paste0("FK_", table), 1, 30),
      paste(toupper(references$cols), collapse = ", "),
      schema, toupper(references$table),
      paste(toupper(references$ref_cols), collapse = ", ")
    ))
  }
  sprintf("CREATE TABLE %s.%s (\n%s\n)", schema, table, paste(c(col_sql, constraints), collapse = ",\n"))
}

#' Create every table flagged for creation, parent -> child (registry order).
create_missing_tables <- function(channel, payloads) {
  for (p in payloads) {
    if (is.null(p$create_sql)) next
    message(sprintf("Creating %s.%s ...", p$schema, toupper(p$table)))
    run_sql(channel, p$create_sql, paste("create", p$table))
    if (!length(get_table_columns(channel, p$schema, p$table))) {
      stop("Table creation reported success but ", p$schema, ".", p$table, " is not visible.")
    }
  }
  invisible(TRUE)
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

#' Build the per-table payloads (delta + metadata) from a resolved registry.
#' Read-only: nothing is written to Oracle here, so any error aborts the whole run cleanly.
build_stage_payloads <- function(channel, registry, envir = globalenv(),
                                 max_delete_frac = 0.05, allow_large_deletes = FALSE,
                                 create_missing = FALSE) {
  payloads <- list()

  for (tbl in names(registry)) {
    spec <- registry[[tbl]]
    schema <- assert_schema(spec$schema)

    # QA/QC gate (e.g. race_qaqc_passed from script 03)
    if (!is.null(spec$gate)) {
      if (!exists(spec$gate, envir = envir) || !isTRUE(get(spec$gate, envir = envir))) {
        stop(sprintf(paste0(
          "[%s] QA/QC gate '%s' is missing or FALSE. Fix the QA/QC warnings ",
          "(run_taxonomy_qaqc) before updating %s."
        ), tbl, spec$gate, schema))
      }
    }

    if (!exists(spec$new, envir = envir)) {
      stop(sprintf("[%s] R object '%s' not found. Did steps 01-04 run?", tbl, spec$new))
    }
    new_df <- get(spec$new, envir = envir)

    prod_cols <- get_table_columns(channel, schema, tbl)
    create_sql <- NULL

    if (!length(prod_cols)) {
      can_create <- isTRUE(create_missing) && !identical(spec$can_create, FALSE)
      if (!can_create) {
        like <- RODBC::sqlQuery(channel, sprintf(
          "SELECT owner, object_name, object_type FROM all_objects
           WHERE UPPER(object_name) LIKE '%%%s%%' ORDER BY owner, object_name",
          toupper(substr(tbl, 1, 15))
        ), stringsAsFactors = FALSE)
        cand <- if (is.data.frame(like) && nrow(like) > 0) {
          paste(sprintf("  %s.%s (%s)", like$OWNER, like$OBJECT_NAME, like$OBJECT_TYPE), collapse = "\n")
        } else {
          "  (nothing similar visible to this user)"
        }
        stop(sprintf(
          paste0(
            "[%s] %s.%s does not exist (or is not visible to this user).\n",
            "To create it from the new data set cfg$create_missing_tables <- TRUE ",
            "(not allowed for tables marked can_create = FALSE).\nSimilar objects:\n%s"
          ),
          tbl, schema, toupper(tbl), cand
        ), call. = FALSE)
      }
      message(sprintf(
        "[%s] %s.%s does not exist: it will be CREATED and loaded from scratch",
        tbl, schema, toupper(tbl)
      ))
      new_clean <- clean_for_oracle(new_df)
      create_sql <- build_create_table_sql(schema, tbl, new_clean, spec$id_keys, spec$references)
      prod_cols <- names(new_clean)
      current_df <- new_clean[0, , drop = FALSE] # ignore any stale *_current object: everything is an INSERT
    } else {
      if (!is.null(spec$current) && exists(spec$current, envir = envir)) {
        current_df <- get(spec$current, envir = envir)
        # The R object must match Oracle, otherwise the delta is computed against stale data
        n_oracle <- RODBC::sqlQuery(channel, sprintf("SELECT COUNT(*) AS N FROM %s.%s", schema, toupper(tbl)))$N[1]
        if (!identical(as.integer(n_oracle), nrow(current_df))) {
          stop(sprintf(
            paste0(
              "[%s] R object '%s' has %d rows but %s.%s has %s rows in Oracle. ",
              "It is stale (e.g. from before a table reset). Restart R and rerun from run.R ",
              "so steps 01-04 read the current tables."
            ),
            tbl, spec$current, nrow(current_df), schema, toupper(tbl), n_oracle
          ), call. = FALSE)
        }
      } else {
        message(sprintf("[%s] reading current table from Oracle", tbl))
        current_df <- load_current(channel, schema, tbl)
      }
    }

    extra <- setdiff(toupper(names(new_df)), prod_cols)
    if (length(extra)) {
      stop(sprintf(
        "[%s] new data has column(s) not in %s.%s: %s",
        tbl, schema, toupper(tbl), paste(extra, collapse = ", ")
      ))
    }

    delta <- prepare_delta(new_df, current_df, spec$id_keys, mode = spec$mode)

    n_del <- sum(delta$OPERATION == "DELETE")
    if (isTRUE(spec$no_deletes) && n_del > 0) {
      print(utils::head(delta[delta$OPERATION == "DELETE", ], 10))
      stop(sprintf("[%s] %d row(s) would be DELETED, but this table is configured no_deletes.", tbl, n_del))
    }

    # Guard against catastrophic deletes (e.g. an empty/partial `new` table)
    if (spec$mode == "upsert" && nrow(current_df) > 0) {
      frac <- n_del / nrow(current_df)
      if (frac > max_delete_frac && !allow_large_deletes) {
        stop(
          sprintf(
            paste0(
              "[%s] would delete %d of %d existing rows (%.1f%%; limit %.1f%%). ",
              "New data has %d rows (%d inserts, %d updates). ",
              "Check the new data, or set cfg$allow_large_deletes = TRUE."
            ),
            tbl, n_del, nrow(current_df), 100 * frac, 100 * max_delete_frac,
            nrow(new_df), sum(delta$OPERATION == "INSERT"), sum(delta$OPERATION == "UPDATE")
          ),
          call. = FALSE
        )
      }
    }

    payloads[[tbl]] <- list(
      table        = tbl,
      schema       = schema,
      stage_schema = assert_schema(spec$stage_schema),
      audit_schema = assert_schema(spec$audit_schema),
      mode         = spec$mode,
      id_keys      = toupper(spec$id_keys),
      audit_keys   = toupper(spec$audit_keys),
      fast_save    = if (is.null(spec$fast_save)) TRUE else spec$fast_save,
      cols         = setdiff(names(delta), "OPERATION"),
      create_sql   = create_sql,
      data         = delta
    )
  }
  payloads
}

print_delta_summary <- function(payloads, out_dir = NULL) {
  s <- do.call(rbind, lapply(payloads, function(p) {
    data.frame(
      table = paste0(p$schema, ".", toupper(p$table)),
      new_table = !is.null(p$create_sql),
      insert = sum(p$data$OPERATION == "INSERT"),
      update = sum(p$data$OPERATION == "UPDATE"),
      delete = sum(p$data$OPERATION == "DELETE")
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

ensure_stage_table <- function(channel, target_schema, stage_schema, table) {
  stage <- paste0("STAGE_", toupper(table))
  expected <- c(get_table_columns(channel, target_schema, table), "OPERATION")
  actual <- get_table_columns(channel, stage_schema, stage)

  if (length(actual) && !setequal(actual, expected)) {
    message(sprintf("--> %s.%s columns are out of date; recreating", stage_schema, stage))
    run_sql(channel, sprintf("DROP TABLE %s.%s PURGE", stage_schema, stage), "drop stage table")
    actual <- character()
  }
  if (!length(actual)) {
    message(sprintf("--> creating %s.%s", stage_schema, stage))
    run_sql(channel, sprintf(
      "CREATE TABLE %1$s.%2$s AS
       SELECT t.*, CAST(NULL AS VARCHAR2(10)) AS OPERATION FROM %3$s.%4$s t WHERE 1 = 0",
      stage_schema, stage, target_schema, toupper(table)
    ), "create stage table")
  }
  invisible(stage)
}

load_stage_table <- function(channel, payload) {
  stage <- paste0("STAGE_", toupper(payload$table))
  path <- paste0(payload$stage_schema, ".", stage)
  ensure_stage_table(channel, payload$schema, payload$stage_schema, payload$table)
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

## ---- Generated MERGE SQL  ------

# NULL-safe equality for key columns
sql_match <- function(cols, left = "target", right = "source") {
  paste(sprintf(
    "(%1$s.%3$s = %2$s.%3$s OR (%1$s.%3$s IS NULL AND %2$s.%3$s IS NULL))",
    left, right, cols
  ), collapse = "\n        AND ")
}

build_delete_sql <- function(p) {
  sprintf(
    "DELETE FROM %1$s.%2$s target
     WHERE EXISTS (
       SELECT 1 FROM %3$s.STAGE_%2$s source
       WHERE source.OPERATION = 'DELETE'
         AND %4$s
     )", p$schema, toupper(p$table), p$stage_schema, sql_match(p$id_keys)
  )
}

build_upsert_sql <- function(p) {
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
     USING (SELECT %4$s, OPERATION FROM %3$s.STAGE_%2$s
            WHERE OPERATION IN ('INSERT', 'UPDATE')) source
     ON (%5$s)
     %6$s
     WHEN NOT MATCHED THEN
       INSERT (%4$s) VALUES (%7$s)
       WHERE source.OPERATION = 'INSERT'",
    p$schema, tbl, p$stage_schema, paste(p$cols, collapse = ", "), sql_match(p$id_keys), matched,
    paste0("source.", p$cols, collapse = ", ")
  )
}

build_insert_only_sql <- function(p) {
  sprintf(
    "INSERT INTO %1$s.%2$s (%4$s)
     SELECT %4$s FROM %3$s.STAGE_%2$s WHERE OPERATION = 'INSERT'",
    p$schema, toupper(p$table), p$stage_schema, paste(p$cols, collapse = ", ")
  )
}

#' Apply all changes (across all schemas) in ONE transaction: any failure rolls
#' everything back. Deletes run child -> parent; inserts/updates parent -> child.
apply_changes <- function(channel, payloads) {
  has <- function(p, op) any(p$data$OPERATION == op)
  name <- function(p) paste0(p$schema, ".", toupper(p$table))

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
          message("Deleting from ", name(p), "...")
          run_sql(channel, build_delete_sql(p), paste("delete", name(p)))
        }
      }
      for (p in payloads) { # inserts/updates: parent -> child
        if (p$mode == "upsert" && (has(p, "INSERT") || has(p, "UPDATE"))) {
          message("Merging into ", name(p), "...")
          run_sql(channel, build_upsert_sql(p), paste("merge", name(p)))
        } else if (p$mode == "insert_only" && has(p, "INSERT")) {
          message("Inserting into ", name(p), "...")
          run_sql(channel, build_insert_only_sql(p), paste("insert", name(p)))
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

## ---- Audit tables + triggers -------------------------------------------------
## The trigger lives in the table's schema; the AUDIT_ table lives in audit_schema.

sync_audit_table <- function(channel, schema, table, audit_keys,
                             audit_schema = schema, rebuild = FALSE) {
  schema <- assert_schema(schema)
  audit_schema <- assert_schema(audit_schema)
  table <- toupper(table)
  audit_tbl <- paste0("AUDIT_", table)
  audit_keys <- toupper(audit_keys)

  all_cols <- get_table_columns(channel, schema, table)
  if (!length(all_cols)) stop("Table not found: ", schema, ".", table)
  bad <- setdiff(audit_keys, all_cols)
  if (length(bad)) stop(sprintf("%s: audit key(s) not in table: %s", table, paste(bad, collapse = ", ")))

  key_cols <- all_cols[all_cols %in% audit_keys]
  resp_cols <- all_cols[!all_cols %in% audit_keys]
  # sprintf (not paste0): paste0(character(0), "x") returns "x", which broke tables
  # whose columns are ALL keys (e.g. TAXON_GROUP_COMPOSITION)
  curr_cols <- sprintf("%s_CURRENT", resp_cols)
  upd_cols <- sprintf("%s_UPDATE", resp_cols)

  # -- 1. Audit table: create, or detect drift ----------------------------------
  hdr_cols <- c("OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME")
  expected <- c(hdr_cols, key_cols, curr_cols, upd_cols)
  actual <- get_table_columns(channel, audit_schema, audit_tbl)

  if (length(actual) && !setequal(actual, expected)) {
    msg <- sprintf(
      paste0(
        "%s.%s columns don't match %s.%s.\n  only in audit table: %s\n",
        "  missing from audit table: %s\n",
        "Set cfg$rebuild_audit <- TRUE to rename the old audit table and create a new one."
      ),
      audit_schema, audit_tbl, schema, table,
      paste(setdiff(actual, expected), collapse = ", "),
      paste(setdiff(expected, actual), collapse = ", ")
    )
    if (!rebuild) stop(msg, call. = FALSE)
    bak <- paste0(substr(audit_tbl, 1, 18), "_BAK", format(Sys.Date(), "%y%m%d"))
    message(sprintf("--> renaming %s to %s", audit_tbl, bak))
    run_sql(
      channel, sprintf("ALTER TABLE %s.%s RENAME TO %s", audit_schema, audit_tbl, bak),
      "rename audit table"
    )
    actual <- character()
  }

  if (!length(actual)) {
    message(sprintf("--> creating %s.%s", audit_schema, audit_tbl))
    pair_sql <- as.vector(rbind(
      sprintf("%s AS %s", resp_cols, curr_cols),
      sprintf("%s AS %s", resp_cols, upd_cols)
    ))
    run_sql(
      channel, sprintf(
        "CREATE TABLE %s.%s AS
       SELECT CAST('INSERT' AS VARCHAR2(10)) AS OPERATION_TYPE,
              SYSTIMESTAMP AS OPERATION_TIMESTAMP,
              CAST('USER' AS VARCHAR2(100)) AS USER_NAME,
              %s
       FROM %s.%s WHERE 1 = 0",
        audit_schema, audit_tbl, paste(c(key_cols, pair_sql), collapse = ", "), schema, table
      ),
      paste("create", audit_tbl)
    )

    # CTAS copies NOT NULL; CURRENT is empty on INSERT and UPDATE on DELETE, so relax them
    nn <- RODBC::sqlQuery(channel, sprintf(
      "SELECT column_name FROM all_tab_columns
       WHERE owner = '%s' AND table_name = '%s' AND nullable = 'N'", audit_schema, audit_tbl
    ),
    stringsAsFactors = FALSE
    )
    for (col in intersect(nn$COLUMN_NAME, c(curr_cols, upd_cols))) {
      run_sql(
        channel, sprintf("ALTER TABLE %s.%s MODIFY (%s NULL)", audit_schema, audit_tbl, col),
        "relax NOT NULL"
      )
    }
    run_sql(channel, sprintf("GRANT SELECT ON %s.%s TO PUBLIC", audit_schema, audit_tbl), "grant")
  }

  # -- 2. Trigger ---------------------------------------------------------------
  user_expr <- "COALESCE(SYS_CONTEXT('USERENV', 'PROXY_USER'), SYS_CONTEXT('USERENV', 'SESSION_USER'))"
  ins_stmt <- function(op, cols, vals) {
    stopifnot(length(cols) == length(vals))
    sprintf(
      "    INSERT INTO %s.%s (%s)\n    VALUES (%s);", audit_schema, audit_tbl,
      paste(c(hdr_cols, cols), collapse = ", "),
      paste(c(sprintf("'%s'", op), "SYSTIMESTAMP", user_expr, vals), collapse = ", ")
    )
  }

  both_cols <- as.vector(rbind(curr_cols, upd_cols))
  both_vals <- as.vector(rbind(sprintf(":OLD.%s", resp_cols), sprintf(":NEW.%s", resp_cols)))

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
    ins_stmt("INSERT", c(key_cols, upd_cols), c(sprintf(":NEW.%s", key_cols), sprintf(":NEW.%s", resp_cols))),
    ins_stmt("DELETE", c(key_cols, curr_cols), c(sprintf(":OLD.%s", key_cols), sprintf(":OLD.%s", resp_cols))),
    ins_stmt("UPDATE", c(key_cols, both_cols), c(sprintf(":NEW.%s", key_cols), both_vals))
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

sync_all_audits <- function(channel, registry, rebuild = FALSE) {
  for (tbl in names(registry)) {
    sp <- registry[[tbl]]
    sync_audit_table(channel, sp$schema, tbl, sp$audit_keys,
      audit_schema = sp$audit_schema, rebuild = rebuild
    )
  }
  invisible(TRUE)
}

## ---- Audit summary (built from the same payloads that were merged) ----------

summarize_all_updates <- function(channel, payloads, time_start, time_end) {
  sql <- paste(
    vapply(payloads, function(p) {
      sprintf(
        "SELECT '%1$s' AS SCHEMA_NAME, '%2$s' AS TABLE_NAME, OPERATION_TYPE, COUNT(*) AS NUMBER_RECS
     FROM %3$s.AUDIT_%2$s
     WHERE OPERATION_TIMESTAMP BETWEEN TO_TIMESTAMP_TZ('%4$s', '%6$s')
                                   AND TO_TIMESTAMP_TZ('%5$s', '%6$s')
     GROUP BY OPERATION_TYPE",
        p$schema, toupper(p$table), p$audit_schema, time_start, time_end, DB_TS_FMT
      )
    }, character(1)),
    collapse = "\nUNION ALL\n"
  )

  res <- RODBC::sqlQuery(channel, sql, stringsAsFactors = FALSE)
  if (!is.data.frame(res)) stop("Failed to summarize audit tables:\n", paste(res, collapse = "\n"))
  res
}
