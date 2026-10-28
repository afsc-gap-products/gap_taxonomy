## =============================================================================
## ORACLE AUDIT & TABLE DEPLOYMENT HELPERS
## =============================================================================


# ------------------------------------------------------------------------------
# 0. SMALL UTILITIES
# ------------------------------------------------------------------------------


#' @title Check RODBC Result for Failure
#' @description RODBC::sqlQuery() returns character(0) on SUCCESS for statements with no result
#' set (CREATE/ALTER/DROP/INSERT/UPDATE/DELETE) and a non-empty character vector of
#' messages on ERROR. So is.character(res) alone cannot be used as an error test.
#' @param res The result object returned from RODBC::sqlQuery().
#' @return A logical value: `TRUE` if the query failed, `FALSE` otherwise.
odbc_failed <- function(res) {
  (is.character(res) && length(res) > 0) ||
    (is.numeric(res) && !is.data.frame(res) && length(res) == 1 && res == -1)
}


#' @title Execute SQL Statement Safely
#' @description Executes a SQL statement via RODBC and stops execution with a formatted error if it fails.
#' @param channel A RODBC connection channel.
#' @param sql The SQL query string to execute.
#' @param context A character string describing the context of the query, used in error messages.
#' @return Invisibly returns the query result on success.
run_sql <- function(channel, sql, context = "Oracle statement") {
  res <- RODBC::sqlQuery(channel, sql, errors = TRUE)
  if (odbc_failed(res)) {
    stop(context, " failed:\n", paste(res, collapse = "\n"),
      "\nSQL (first 500 chars):\n", substr(sql, 1, 500),
      call. = FALSE
    )
  }
  invisible(res)
}


#' @title Convert Vector to Stable Character
#' @description Converts any R vector to a stable character representation (no scientific notation,
#' consistent date formats). Empty strings are treated as NA (Oracle does this too).
#' @param x An atomic vector (e.g., numeric, character, Date, POSIXt, factor).
#' @return A character vector of the same length, suitably formatted for database entry.
to_chr <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  out <- if (inherits(x, "POSIXt")) {
    format(x, "%Y-%m-%d %H:%M:%S")
  } else if (inherits(x, "Date")) {
    format(x, "%Y-%m-%d")
  } else if (is.numeric(x)) {
    ifelse(x == round(x), sprintf("%.0f", x), as.character(x))
  } else {
    as.character(x)
  }
  out[is.na(x) | out == ""] <- NA_character_
  out
}


#' @title Infer Oracle Data Type
#' @description Maps an R vector class/type to a corresponding Oracle database data type.
#' Sizes text columns from the data (2x headroom) rather than defaulting to 4000 bytes.
#' @param x An R vector.
#' @return A character string specifying the Oracle data type (e.g., "VARCHAR2(255)").
oracle_type <- function(x) {
  if (inherits(x, "POSIXt")) {
    "TIMESTAMP"
  } else if (inherits(x, "Date")) {
    "DATE"
  } else if (is.numeric(x)) {
    "NUMBER"
  } else if (is.logical(x)) {
    "NUMBER(1)"
  } else {
    # Size text columns from the data (2x headroom) rather than always 4000
    w <- max(1L, nchar(as.character(x)[!is.na(x)], type = "bytes"))
    widths <- c(20, 50, 100, 255, 500, 1000, 2000, 4000)
    sprintf("VARCHAR2(%d)", as.integer(c(widths[widths >= 2 * w], 4000)[1]))
  }
}

# ---- Oracle column-type introspection & coercion ----------------------------

#' @title Determine Broad Oracle Data Kind
#' @description Groups precise Oracle data types into broader categories ("numeric", "datetime", "character").
#' @param data_type A character string of the Oracle data type (e.g., "VARCHAR2", "NUMBER").
#' @return A character string describing the generic kind, or `NA_character_` if unmatched.

oracle_kind <- function(data_type) {
  dt <- toupper(data_type)
  if (dt %in% c("NUMBER", "FLOAT", "BINARY_DOUBLE", "BINARY_FLOAT")) {
    "numeric"
  } else if (dt == "DATE" || grepl("^TIMESTAMP", dt)) {
    "datetime"
  } else if (dt %in% c("VARCHAR2", "NVARCHAR2", "CHAR", "NCHAR", "CLOB", "NCLOB")) {
    "character"
  } else {
    NA_character_
  }
}


#' @title Build Oracle Type Specification
#' @description Creates a full type specification usable in SQL CAST() functions, merging length and precision rules.
#' @param data_type The base data type string.
#' @param data_length The byte length of the data.
#' @param char_length The character length of the data.
#' @param precision The numeric precision.
#' @param scale The numeric scale.
#' @return A character string with the type specification, or `NA_character_` if cast is unnecessary or unsupported.
oracle_type_spec <- function(data_type, data_length, char_length, precision, scale) {
  dt <- toupper(data_type)
  if (dt %in% c("VARCHAR2", "NVARCHAR2", "CHAR", "NCHAR")) {
    len <- if (!is.na(char_length) && char_length > 0) char_length else data_length
    sprintf("%s(%d)", dt, as.integer(len))
  } else if (dt == "NUMBER") {
    if (is.na(precision)) {
      "NUMBER"
    } else {
      sprintf("NUMBER(%d,%d)", as.integer(precision), as.integer(ifelse(is.na(scale), 0, scale)))
    }
  } else if (dt %in% c("FLOAT", "BINARY_DOUBLE", "BINARY_FLOAT", "DATE", "CLOB", "NCLOB") || grepl("^TIMESTAMP", dt)) {
    dt
  } else {
    NA_character_
  }
}

#' @title Introspect Oracle Column Types
#' @description Retrieves metadata for an existing Oracle table to extract its column names, types, sizes, and precision.
#' @param channel A RODBC connection channel.
#' @param schema The Oracle schema name.
#' @param table The Oracle table name.
#' @return A data frame containing metadata about the table's columns with added `KIND` and `TYPE_SPEC` fields.
oracle_column_types <- function(channel, schema, table) {
  res <- RODBC::sqlQuery(
    channel,
    paste0(
      "SELECT COLUMN_NAME, DATA_TYPE, DATA_LENGTH, CHAR_LENGTH, DATA_PRECISION, DATA_SCALE ",
      "FROM ALL_TAB_COLUMNS WHERE OWNER = '", toupper(schema),
      "' AND TABLE_NAME = '", toupper(table), "' ORDER BY COLUMN_ID"
    ),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(res)) {
    stop(
      "Failed to read column types for ", schema, ".", toupper(table), ":\n",
      paste(res, collapse = "\n")
    )
  }
  res$COLUMN_NAME <- toupper(res$COLUMN_NAME)
  res$KIND <- vapply(res$DATA_TYPE, oracle_kind, character(1), USE.NAMES = FALSE)
  res$TYPE_SPEC <- vapply(seq_len(nrow(res)), function(i) {
    oracle_type_spec(
      res$DATA_TYPE[i], res$DATA_LENGTH[i], res$CHAR_LENGTH[i],
      res$DATA_PRECISION[i], res$DATA_SCALE[i]
    )
  }, character(1))
  res
}


#' @title Coerce R Data.Frame to Oracle Types
#' @description Coerces each R column to the class matching an existing Oracle column type.
#' Stops rather than silently NA-ing if values cannot be converted, exceed VARCHAR2 lengths, or break precision limits.
#' @param df The data frame to coerce.
#' @param types A data frame of Oracle column specifications as returned by `oracle_column_types()`.
#' @param quiet Logical. If `TRUE`, suppresses messaging about coerced columns.
#' @return The mutated R data frame with column types aligned to Oracle requirements.

coerce_to_oracle_types <- function(df, types, quiet = FALSE) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  converted <- character(0)

  for (col in intersect(names(df), types$COLUMN_NAME)) {
    ty <- types[match(col, types$COLUMN_NAME), ]
    kind <- ty$KIND
    if (is.na(kind)) next

    x <- df[[col]]
    if (is.factor(x)) x <- as.character(x)
    x_blank <- is.na(x) | (is.character(x) & x == "")

    out <- switch(kind,
      numeric = {
        if (is.numeric(x)) {
          x
        } else if (is.logical(x)) {
          as.numeric(x)
        } else {
          suppressWarnings(as.numeric(as.character(x)))
        }
      },
      character = if (is.character(x)) x else to_chr(x),
      datetime = {
        if (inherits(x, "POSIXct")) {
          x
        } else if (inherits(x, "Date")) {
          as.POSIXct(format(x), tz = "")
        } else {
          tryCatch(as.POSIXct(as.character(x), tz = ""),
            error = function(e) .POSIXct(rep(NA_real_, length(x)))
          )
        }
      }
    )
    if (kind == "character") out[x_blank] <- NA_character_

    lost <- which(!x_blank & is.na(out))
    if (length(lost) > 0) {
      stop("Column ", col, " cannot be converted to Oracle type ", ty$TYPE_SPEC,
        ". Offending value(s): ",
        paste(utils::head(unique(as.character(x[lost])), 5), collapse = ", "),
        call. = FALSE
      )
    }
    if (ty$DATA_TYPE %in% c("VARCHAR2", "NVARCHAR2", "CHAR", "NCHAR")) {
      too_long <- which(!is.na(out) & nchar(out, type = "chars") > as.integer(
        if (!is.na(ty$CHAR_LENGTH) && ty$CHAR_LENGTH > 0) ty$CHAR_LENGTH else ty$DATA_LENGTH
      ))
      if (length(too_long) > 0) {
        stop("Column ", col, " has value(s) longer than ", ty$TYPE_SPEC, " (e.g. row ",
          too_long[1], ").",
          call. = FALSE
        )
      }
    }
    if (kind == "numeric" && !is.na(ty$DATA_PRECISION)) {
      lim <- 10^(ty$DATA_PRECISION - ifelse(is.na(ty$DATA_SCALE), 0, ty$DATA_SCALE))
      over <- which(!is.na(out) & abs(out) >= lim)
      if (length(over) > 0) {
        stop("Column ", col, " has value(s) too large for ", ty$TYPE_SPEC,
          " (e.g. ", out[over[1]], ").",
          call. = FALSE
        )
      }
    }

    if (!identical(class(x)[1], class(out)[1])) {
      converted <- c(converted, sprintf("%s: %s -> %s", col, class(x)[1], ty$TYPE_SPEC))
    }
    df[[col]] <- out
  }

  if (!quiet && length(converted) > 0) {
    message("  Coerced to match Oracle column types:\n    ", paste(converted, collapse = "\n    "))
  }
  df
}

#' @title Convert R Value to SQL Literal
#' @description Maps a single R value to a SQL-compatible literal string vector. Null values resolve to 'NULL'.
#' @param x The R vector.
#' @return A character vector of SQL literals.
sql_literal <- function(x) {
  if (inherits(x, "POSIXt")) {
    ifelse(is.na(x), "NULL", paste0("TIMESTAMP '", format(x, "%Y-%m-%d %H:%M:%S"), "'"))
  } else if (is.numeric(x)) {
    v <- to_chr(x)
    ifelse(is.na(v), "NULL", v)
  } else {
    v <- to_chr(x)
    ifelse(is.na(v), "NULL", paste0("'", gsub("'", "''", v, fixed = TRUE), "'"))
  }
}



#' @title Bulk Insert Rows to Oracle
#' @description Uses a multi-row INSERT ALL technique to load batch data efficiently into Oracle.
#' @param channel A RODBC connection channel.
#' @param schema The target schema.
#' @param table The target table.
#' @param df The data frame containing records to insert.
#' @param batch_size Integer. The number of rows per SQL batch statement. Calculated automatically based on 900 column constraint if NULL.
#' @return Invisibly returns the number of rows successfully inserted.
insert_rows <- function(channel, schema, table, df, batch_size = NULL) {
  if (nrow(df) == 0) {
    return(invisible(0L))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  if (is.null(batch_size)) batch_size <- max(1L, floor(900 / ncol(df)))

  cols <- paste(sprintf('"%s"', names(df)), collapse = ", ")
  lits <- do.call(cbind, lapply(df, sql_literal))
  vals <- apply(lits, 1, function(r) paste0("(", paste(r, collapse = ", "), ")"))
  target <- paste0(schema, ".", toupper(table))

  for (start in seq(1, nrow(df), by = batch_size)) {
    idx <- start:min(start + batch_size - 1, nrow(df))
    into <- paste0("INTO ", target, " (", cols, ") VALUES ", vals[idx], collapse = "\n")
    run_sql(channel, paste0("INSERT ALL\n", into, "\nSELECT 1 FROM DUAL"),
      context = paste("Insert into", target)
    )
  }
  invisible(nrow(df))
}


# ------------------------------------------------------------------------------
# 1. IDENTIFY CHANGED ROWS BETWEEN LOCAL AND ORACLE DATAFRAMES
# ------------------------------------------------------------------------------
#' @title Identify Delta Between DataFrames
#' @description Compares a new (local) data frame against an old (remote Oracle snapshot) data frame
#' to determine which rows were added, deleted, or altered based on primary keys.
#' @param old_df The old/baseline data frame.
#' @param new_df The new/updated data frame.
#' @param pk_cols Character vector defining the primary key columns.
#' @param normalize_names Logical. If TRUE, capitalizes column names across both inputs.
#' @return A list containing three data frames: `new_rows`, `deleted_rows`, and `changed_rows`.
find_changed_rows <- function(old_df, new_df, pk_cols, normalize_names = TRUE) {
  stopifnot(is.data.frame(old_df), is.data.frame(new_df))
  old_df <- as.data.frame(old_df, stringsAsFactors = FALSE)
  new_df <- as.data.frame(new_df, stringsAsFactors = FALSE)

  if (normalize_names) {
    names(old_df) <- toupper(names(old_df))
    names(new_df) <- toupper(names(new_df))
    pk_cols <- toupper(pk_cols)
  }
  stopifnot(all(pk_cols %in% names(old_df)), all(pk_cols %in% names(new_df)))

  common_cols <- intersect(names(new_df), names(old_df))
  old_df <- old_df[, common_cols, drop = FALSE]
  new_df <- new_df[, common_cols, drop = FALSE]
  value_cols <- setdiff(common_cols, pk_cols)

  make_key <- function(df, cols) {
    if (nrow(df) == 0) {
      return(character(0))
    }
    if (length(cols) == 0) {
      return(rep("", nrow(df)))
    }
    parts <- lapply(df[cols], function(x) {
      v <- to_chr(x)
      v[is.na(v)] <- "<NA>"
      v
    })
    do.call(paste, c(unname(parts), sep = "\r"))
  }

  new_key <- make_key(new_df, pk_cols)
  old_key <- make_key(old_df, pk_cols)

  if (anyDuplicated(new_key)) {
    dk <- unique(new_key[duplicated(new_key)])
    in_d <- new_key %in% dk
    dd <- new_df[in_d, , drop = FALSE]
    ddk <- new_key[in_d]
    vary <- vapply(value_cols, function(cc) {
      v <- to_chr(dd[[cc]])
      v[is.na(v)] <- "<NA>"
      any(tapply(v, ddk, function(z) length(unique(z)) > 1))
    }, logical(1))
    ex <- dd[order(ddk), , drop = FALSE]
    stop(length(dk), " key value(s) (", paste(pk_cols, collapse = ", "),
      ") occur more than once in the new data. ",
      if (any(vary)) {
        paste0(
          "Columns that differ between the duplicate rows (candidates to add to the key): ",
          paste(names(vary)[vary], collapse = ", "), "."
        )
      } else {
        "The duplicate rows are identical in every column."
      },
      "\nFirst duplicated rows:\n",
      paste(utils::capture.output(print(utils::head(ex, 10), row.names = FALSE)), collapse = "\n"),
      call. = FALSE
    )
  }
  if (anyDuplicated(old_key)) {
    dup <- unique(old_key[duplicated(old_key)])
    ex <- old_df[match(utils::head(dup, 5), old_key), pk_cols, drop = FALSE]
    warning(length(dup), " key value(s) (", paste(pk_cols, collapse = ", "),
      ") appear more than once in the Oracle table. Examples:\n",
      paste(utils::capture.output(print(ex, row.names = FALSE)), collapse = "\n"),
      call. = FALSE
    )
  }

  new_idx <- which(!new_key %in% old_key)
  del_idx <- which(!old_key %in% new_key)
  m <- match(new_key, old_key)
  both <- which(!is.na(m))

  upd_new <- integer(0)
  if (length(value_cols) > 0 && length(both) > 0) {
    new_fp <- make_key(new_df[both, , drop = FALSE], value_cols)
    old_fp <- make_key(old_df[m[both], , drop = FALSE], value_cols)
    upd_new <- both[new_fp != old_fp]
  }

  add_suffix <- function(df, suffix) {
    i <- !names(df) %in% pk_cols
    names(df)[i] <- paste0(names(df)[i], suffix)
    rownames(df) <- NULL
    df
  }

  if (length(upd_new) > 0) {
    nu <- new_df[upd_new, , drop = FALSE]
    oc <- old_df[m[upd_new], , drop = FALSE]
    changed_rows <- nu[, pk_cols, drop = FALSE]
    for (col in value_cols) {
      changed_rows[[paste0(col, "_UPDATE")]] <- nu[[col]]
      changed_rows[[paste0(col, "_CURRENT")]] <- oc[[col]]
    }
    rownames(changed_rows) <- NULL
  } else {
    changed_rows <- add_suffix(new_df[0, , drop = FALSE], "_UPDATE")
  }

  list(
    new_rows     = add_suffix(new_df[new_idx, , drop = FALSE], "_UPDATE"),
    deleted_rows = add_suffix(old_df[del_idx, , drop = FALSE], "_UPDATE"),
    changed_rows = changed_rows
  )
}


# ------------------------------------------------------------------------------
# 2. ORACLE SCHEMA & TABLE VALIDATION / CREATION
# ------------------------------------------------------------------------------
#' @title Check if Oracle Table Exists
#' @description Queries the ALL_TABLES dictionary view to detect table existence.
#' @param channel A RODBC connection channel.
#' @param schema The schema string to check.
#' @param table The table name string.
#' @return Logical. TRUE if the table is found, FALSE otherwise.
oracle_table_exists <- function(channel, schema, table) {
  res <- RODBC::sqlQuery(
    channel,
    glue::glue("
      SELECT COUNT(*) AS N
      FROM ALL_TABLES
      WHERE TABLE_NAME = '{toupper(table)}'
        AND OWNER = '{toupper(schema)}'
    "),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(res)) {
    stop(
      "Could not check whether ", schema, ".", toupper(table), " exists:\n",
      paste(res, collapse = "\n")
    )
  }
  res[[1]][1] > 0
}


#' @title Create Oracle Table
#' @description Issues a CREATE TABLE statement to the database given specific columns and types.
#' @param channel A RODBC connection channel.
#' @param schema Schema string.
#' @param table Table string.
#' @param col_types A named character vector of Oracle column types (e.g. c(COL="NUMBER")).
#' @return Invisibly returns TRUE on creation or FALSE if the table already existed.
create_table <- function(channel, schema, table, col_types) {
  table <- toupper(table)
  cols <- paste(sprintf('"%s" %s', toupper(names(col_types)), col_types),
    collapse = ",\n  "
  )
  sql <- paste0("CREATE TABLE ", schema, ".", table, " (\n  ", cols, "\n)")

  res <- RODBC::sqlQuery(channel, sql)
  if (odbc_failed(res)) {
    if (any(grepl("ORA-00955", res, ignore.case = TRUE))) {
      message("ℹ Table ", schema, ".", table, " already exists. Proceeding...")
      return(invisible(FALSE))
    }
    stop(
      "Oracle Create Table Error in ", schema, ".", table, ":\n",
      paste(res, collapse = "\n")
    )
  }
  invisible(TRUE)
}

# Base (product) table: Oracle types are inferred from the R column classes.
create_base_table <- function(channel, schema, table, df) {
  create_table(channel, schema, table, vapply(df, oracle_type, character(1)))
}

# Adds any columns that are missing from an existing table.
ensure_table_columns <- function(channel, schema, table, col_types) {
  existing <- RODBC::sqlQuery(
    channel,
    glue::glue("
      SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS
      WHERE OWNER = '{toupper(schema)}'
        AND TABLE_NAME = '{toupper(table)}'
    "),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(existing)) {
    stop(
      "Failed to query columns for ", schema, ".", toupper(table), ":\n",
      paste(existing, collapse = "\n")
    )
  }

  names(col_types) <- toupper(names(col_types))
  missing <- setdiff(names(col_types), toupper(existing$COLUMN_NAME))

  for (col in missing) {
    message("  Adding column ", col, " to ", schema, ".", toupper(table))
    res <- RODBC::sqlQuery(
      channel,
      paste0(
        "ALTER TABLE ", schema, ".", toupper(table),
        ' ADD ("', col, '" ', col_types[[col]], ")"
      )
    )
    if (odbc_failed(res) && !any(grepl("ORA-01430", res, ignore.case = TRUE))) {
      stop(
        "Error adding column ", col, " to ", schema, ".", toupper(table), ":\n",
        paste(res, collapse = "\n")
      )
    }
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# 3. STAGING TABLE HELPERS
# ------------------------------------------------------------------------------

build_temp_table_name <- function(tbl) {
  clean_name <- gsub("[^A-Za-z0-9]", "", toupper(tbl))
  paste0("TMP_", substr(clean_name, 1, 10), "_", format(Sys.time(), "%M%S"), "_", sample(100:999, 1))
}

# Looks up the table's real name in the data dictionary (handles lowercase/quoted
# names), commits first (an open transaction can block DROP with ORA-00054), and
# REPORTS any Oracle error instead of hiding it.
drop_temp_table <- function(channel, schema, temp_table_name) {
  try(RODBC::odbcEndTran(channel, commit = TRUE), silent = TRUE)

  found <- RODBC::sqlQuery(
    channel,
    paste0(
      "SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER = '", toupper(schema),
      "' AND UPPER(TABLE_NAME) = '", toupper(temp_table_name), "'"
    ),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(found)) {
    warning("Could not look up temp table ", schema, ".", temp_table_name, ":\n",
      paste(found, collapse = "\n"),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (nrow(found) == 0) {
    return(invisible(TRUE))
  } # nothing to drop

  for (nm in found$TABLE_NAME) {
    res <- RODBC::sqlQuery(channel, paste0("DROP TABLE ", schema, '."', nm, '" PURGE'))
    if (odbc_failed(res)) {
      warning("Failed to drop temp table ", schema, ".", nm, ":\n", paste(res, collapse = "\n"),
        "\nDrop it manually with: DROP TABLE ", schema, ".", nm, " PURGE",
        call. = FALSE
      )
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}

# One-off cleanup of leftovers from earlier failed runs (names like TMP_<TABLE>_<MMSS>[_<nnn>]).
drop_stale_temp_tables <- function(channel, schema) {
  res <- RODBC::sqlQuery(
    channel,
    paste0(
      "SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER = '", toupper(schema),
      "' AND TABLE_NAME LIKE 'TMP%'"
    ),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(res)) stop("Could not list temp tables:\n", paste(res, collapse = "\n"))
  stale <- res$TABLE_NAME[grepl("^TMP_[A-Z0-9]{1,10}_[0-9]{4}(_[0-9]{3})?$", res$TABLE_NAME)]
  for (nm in stale) {
    message("Dropping stale temp table ", schema, ".", nm)
    drop_temp_table(channel, schema, nm)
  }
  invisible(stale)
}


# ------------------------------------------------------------------------------
# 4. SAFE TABLE UPDATE OPERATIONS (DELETE, INSERT, MERGE)
# ------------------------------------------------------------------------------
# Staging-table columns are CAST to the target column's exact Oracle type in every
# statement, so nothing depends on implicit conversion or NLS settings.

col_ref <- function(alias, cols, types = NULL) {
  vapply(cols, function(cc) {
    ref <- paste0(alias, '."', cc, '"')
    if (!is.null(types)) {
      spec <- types$TYPE_SPEC[match(cc, types$COLUMN_NAME)]
      if (length(spec) == 1 && !is.na(spec)) ref <- sprintf("CAST(%s AS %s)", ref, spec)
    }
    ref
  }, character(1), USE.NAMES = FALSE)
}

# NULL-safe comparable expression for key columns
key_expr <- function(cols, alias, types = NULL) {
  paste0("NVL(TO_CHAR(", col_ref(alias, cols, types), "), '__NULL__')")
}

apply_safe_delete <- function(channel, main_schema, main_table, stage_table, pk_cols, types = NULL) {
  pk_cols <- toupper(pk_cols)
  main_keys <- paste(key_expr(pk_cols, "M"), collapse = ", ")
  stg_keys <- paste(key_expr(pk_cols, "S", types), collapse = ", ")

  run_sql(channel, paste0(
    "DELETE FROM ", main_schema, ".", main_table, " M\n",
    "WHERE (", main_keys, ") IN (\n",
    "  SELECT ", stg_keys, "\n  FROM ", main_schema, ".", stage_table, " S\n",
    "  WHERE S.\"STG_ACTION\" = 'DELETE'\n)"
  ), context = paste("Delete from", main_table))
}

apply_safe_insert <- function(channel, main_schema, main_table, stage_table, cols, types = NULL) {
  cols <- toupper(cols)
  cols_quoted <- paste0('"', cols, '"', collapse = ", ")
  select_list <- paste(col_ref("S", cols, types), collapse = ", ")

  run_sql(channel, paste0(
    "INSERT INTO ", main_schema, ".", main_table, " (", cols_quoted, ")\n",
    "SELECT ", select_list, "\nFROM ", main_schema, ".", stage_table, " S\n",
    "WHERE S.\"STG_ACTION\" = 'INSERT'"
  ), context = paste("Insert into", main_table))
}

apply_safe_merge_update <- function(channel, main_schema, main_table, stage_table, cols, pk_cols,
                                    types = NULL) {
  pk_cols <- toupper(pk_cols)
  set_cols <- setdiff(toupper(cols), pk_cols)
  if (length(set_cols) == 0) {
    return(invisible(TRUE))
  }

  set_clause <- paste(sprintf('MAIN."%s" = %s', set_cols, col_ref("STG", set_cols, types)),
    collapse = ", "
  )
  join_clause <- paste(key_expr(pk_cols, "MAIN"), "=", key_expr(pk_cols, "STG", types),
    collapse = " AND "
  )

  run_sql(channel, paste0(
    "MERGE INTO ", main_schema, ".", main_table, " MAIN\n",
    "USING (SELECT * FROM ", main_schema, ".", stage_table, " WHERE \"STG_ACTION\" = 'UPDATE') STG\n",
    "ON (", join_clause, ")\n",
    "WHEN MATCHED THEN UPDATE SET ", set_clause
  ), context = paste("Merge into", main_table))
}

# Stack the three deltas into ONE data frame with an STG_ACTION column
# (INSERT / UPDATE / DELETE) so a single staging table serves all three steps.
build_stage_df <- function(changed_rows, types) {
  prep <- function(df, action) {
    if (nrow(df) == 0) {
      return(NULL)
    }
    df <- as.data.frame(df)
    df <- df[, !grepl("_CURRENT$", names(df)), drop = FALSE]
    names(df) <- sub("_UPDATE$", "", names(df))
    df <- coerce_to_oracle_types(df, types, quiet = TRUE) # same classes for all three sources
    df$STG_ACTION <- action
    df
  }
  dplyr::bind_rows(
    prep(changed_rows$new_rows, "INSERT"),
    prep(changed_rows$deleted_rows, "DELETE"),
    prep(changed_rows$changed_rows, "UPDATE")
  )
}

safe_apply_taxonomy_updates <- function(channel, tbl, changed_rows, schema, pk_cols) {
  main_table <- toupper(tbl)
  pk_cols <- toupper(pk_cols)
  types <- oracle_column_types(channel, schema, main_table)

  n <- c(
    INSERT = nrow(changed_rows$new_rows),
    UPDATE = nrow(changed_rows$changed_rows),
    DELETE = nrow(changed_rows$deleted_rows)
  )
  if (sum(n) == 0) {
    return(invisible(TRUE))
  }

  stage_df <- as.data.frame(build_stage_df(changed_rows, types), stringsAsFactors = FALSE)
  stage_cols <- setdiff(names(stage_df), "STG_ACTION")
  stage_table <- build_temp_table_name(main_table)

  # Staging table = the target's own column types + the action column
  spec <- types$TYPE_SPEC[match(stage_cols, types$COLUMN_NAME)]
  spec[is.na(spec)] <- "VARCHAR2(4000)"
  col_types <- c(stats::setNames(spec, stage_cols), STG_ACTION = "VARCHAR2(10)")

  on.exit(drop_temp_table(channel, schema, stage_table), add = TRUE)
  create_table(channel, schema, stage_table, col_types)
  insert_rows(channel, schema, stage_table, stage_df)

  if (n[["DELETE"]] > 0) apply_safe_delete(channel, schema, main_table, stage_table, pk_cols, types)
  if (n[["INSERT"]] > 0) apply_safe_insert(channel, schema, main_table, stage_table, stage_cols, types)
  if (n[["UPDATE"]] > 0) {
    apply_safe_merge_update(
      channel, schema, main_table, stage_table,
      stage_cols, pk_cols, types
    )
  }

  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# 5. ORACLE-SIDE AUDITING (AUDIT TABLE + ROW-LEVEL TRIGGER)
# ------------------------------------------------------------------------------
# For every base column X the audit table has X_OLD and X_NEW, plus
#   OPERATION_TYPE (INSERT/UPDATE/DELETE), OPERATION_TIMESTAMP, USER_NAME.
#   INSERT: *_OLD are NULL      UPDATE: both filled      DELETE: *_NEW are NULL
# Column types mirror the base table's ACTUAL Oracle types. The trigger runs
# inside the DML statement, so a failed audit insert rolls the DML back.

AUDIT_META_COLS <- c("OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME")

# Triggers have their own namespace, so this can share the audit table's name style
trigger_name_for <- function(base_table) substr(paste0("AUDIT_", toupper(base_table)), 1, 30)

# c("A","B") -> c("A_OLD","A_NEW","B_OLD","B_NEW")
audit_col_names <- function(cols) as.vector(rbind(paste0(cols, "_OLD"), paste0(cols, "_NEW")))

count_table_rows <- function(channel, schema, table) {
  res <- RODBC::sqlQuery(channel, paste0("SELECT COUNT(*) AS N FROM ", schema, ".", toupper(table)),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(res)) {
    stop(
      "Could not count rows in ", schema, ".", table, ":\n",
      paste(res, collapse = "\n")
    )
  }
  as.numeric(res[[1]][1])
}

# Creates the audit table from the base table's columns, or brings an existing
# one in line (adds missing columns; refuses if a text column is too short).
setup_audit_table <- function(channel, base_schema, base_table, audit_schema, audit_table,
                              base_types = NULL) {
  if (is.null(base_types)) base_types <- oracle_column_types(channel, base_schema, base_table)
  if (nrow(base_types) == 0) stop("No columns found for ", base_schema, ".", base_table)

  clash <- intersect(base_types$COLUMN_NAME, AUDIT_META_COLS)
  if (length(clash) > 0) {
    stop("Base table has column(s) that clash with audit columns: ", paste(clash, collapse = ", "))
  }

  # Types the trigger can't copy directly fall back to text (trigger uses TO_CHAR)
  base_spec <- ifelse(is.na(base_types$TYPE_SPEC), "VARCHAR2(4000)", base_types$TYPE_SPEC)
  audit_cols <- audit_col_names(base_types$COLUMN_NAME)
  base_idx <- rep(seq_len(nrow(base_types)), each = 2)

  spec <- c(
    OPERATION_TYPE = "VARCHAR2(10)", OPERATION_TIMESTAMP = "TIMESTAMP",
    USER_NAME = "VARCHAR2(128)",
    stats::setNames(base_spec[base_idx], audit_cols)
  )

  if (!oracle_table_exists(channel, audit_schema, audit_table)) {
    message("Creating audit table: ", audit_schema, ".", audit_table)
    create_table(channel, audit_schema, audit_table, spec)
    return(invisible(base_types))
  }

  ensure_table_columns(channel, audit_schema, audit_table, spec)

  at <- oracle_column_types(channel, audit_schema, audit_table)
  j <- match(audit_cols, at$COLUMN_NAME)
  bt <- base_types[base_idx, ]

  text_types <- c("VARCHAR2", "CHAR", "NVARCHAR2", "NCHAR")
  too_short <- which(bt$DATA_TYPE %in% text_types & !is.na(j) & at$DATA_LENGTH[j] < bt$DATA_LENGTH)
  if (length(too_short) > 0) {
    stop("Audit column(s) narrower than the base table (inserts would fail with ORA-12899): ",
      paste(audit_cols[too_short], collapse = ", "),
      ". ALTER the audit column(s) to be at least as wide.",
      call. = FALSE
    )
  }

  kind_diff <- which(!is.na(j) & !is.na(bt$KIND) & !is.na(at$KIND[j]) & bt$KIND != at$KIND[j])
  if (length(kind_diff) > 0) {
    warning("Audit column type differs from base table for: ",
      paste(audit_cols[kind_diff], collapse = ", "),
      ". The trigger will rely on Oracle's implicit conversion for these.",
      call. = FALSE
    )
  }
  invisible(base_types)
}

# The trigger's owner needs a DIRECT (not role-based) INSERT grant when the audit
# table lives in another schema.
grant_audit_insert <- function(channel, audit_schema, audit_table, trigger_owner) {
  if (toupper(audit_schema) == toupper(trigger_owner)) {
    return(invisible(TRUE))
  }
  stmt <- paste0(
    "GRANT INSERT ON ", toupper(audit_schema), ".", toupper(audit_table),
    " TO ", toupper(trigger_owner)
  )
  res <- RODBC::sqlQuery(channel, stmt)
  if (odbc_failed(res)) {
    warning("Could not run: ", stmt, "\n", paste(res, collapse = "\n"),
      "\nAsk the owner of ", audit_schema, " (or a DBA) to run it, otherwise the trigger will fail.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

build_audit_trigger_sql <- function(base_schema, base_table, audit_schema, audit_table, base_types) {
  cols <- base_types$COLUMN_NAME
  audit_cols <- audit_col_names(cols)
  col_list <- paste(sprintf('"%s"', audit_cols), collapse = ", ")

  val <- function(prefix) {
    ifelse(is.na(base_types$TYPE_SPEC),
      sprintf('TO_CHAR(%s."%s")', prefix, cols),
      sprintf('%s."%s"', prefix, cols)
    )
  }
  vals <- as.vector(rbind(val(":OLD"), val(":NEW"))) # matches audit_col_names() order

  paste0(
    "CREATE OR REPLACE TRIGGER ", toupper(base_schema), ".", trigger_name_for(base_table), "\n",
    "AFTER INSERT OR UPDATE OR DELETE ON ", toupper(base_schema), ".", toupper(base_table), "\n",
    "FOR EACH ROW\n",
    "DECLARE\n",
    "  v_operation VARCHAR2(10);\n",
    "BEGIN\n",
    "  IF INSERTING THEN\n    v_operation := 'INSERT';\n",
    "  ELSIF UPDATING THEN\n    v_operation := 'UPDATE';\n",
    "  ELSE\n    v_operation := 'DELETE';\n",
    "  END IF;\n\n",
    "  -- :OLD is NULL on INSERT and :NEW is NULL on DELETE\n",
    "  INSERT INTO ", toupper(audit_schema), ".", toupper(audit_table), "\n",
    "    (OPERATION_TYPE, OPERATION_TIMESTAMP, USER_NAME, ", col_list, ")\n",
    "  VALUES\n",
    "    (v_operation, SYSTIMESTAMP, SYS_CONTEXT('USERENV', 'SESSION_USER'),\n     ",
    paste(vals, collapse = ",\n     "), ");\n",
    "END;"
  )
}

drop_trigger_if_exists <- function(channel, schema, trigger_name) {
  found <- RODBC::sqlQuery(
    channel,
    paste0(
      "SELECT TRIGGER_NAME FROM ALL_TRIGGERS WHERE OWNER = '", toupper(schema),
      "' AND TRIGGER_NAME = '", toupper(trigger_name), "'"
    ),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(found) || nrow(found) == 0) {
    return(invisible(FALSE))
  }
  res <- RODBC::sqlQuery(channel, paste0("DROP TRIGGER ", toupper(schema), ".", toupper(trigger_name)))
  if (odbc_failed(res)) {
    warning("Could not drop trigger ", schema, ".", trigger_name, ":\n", paste(res, collapse = "\n"),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  message("  Dropped legacy trigger ", schema, ".", trigger_name)
  invisible(TRUE)
}

# (Re)creates the trigger from the CURRENT base-table columns, then verifies it
# compiled. An invalid trigger makes every DML on the table fail (ORA-04098), so
# if it is invalid it is disabled and R stops.
create_audit_trigger <- function(channel, base_schema, base_table, audit_schema, audit_table,
                                 base_types) {
  trg <- trigger_name_for(base_table)

  # Earlier versions of this script named the trigger TRG_<TABLE>[_AUDIT]; remove
  # those so changes aren't logged twice.
  legacy <- unique(c(
    substr(paste0("TRG_", toupper(base_table)), 1, 30),
    paste0("TRG_", toupper(base_table), "_AUDIT")
  ))
  for (nm in setdiff(legacy, trg)) drop_trigger_if_exists(channel, base_schema, nm)

  sql <- build_audit_trigger_sql(base_schema, base_table, audit_schema, audit_table, base_types)
  res <- RODBC::sqlQuery(channel, sql)

  if (odbc_failed(res)) {
    f <- paste0(trg, ".sql")
    writeLines(c(sql, "/"), f)
    stop("Creating trigger ", base_schema, ".", trg, " failed:\n", paste(res, collapse = "\n"),
      "\nThe DDL was saved to '", f, "' so you can run it in SQL Developer.",
      call. = FALSE
    )
  }

  st <- RODBC::sqlQuery(
    channel,
    paste0(
      "SELECT STATUS FROM ALL_OBJECTS WHERE OWNER = '", toupper(base_schema),
      "' AND OBJECT_NAME = '", trg, "' AND OBJECT_TYPE = 'TRIGGER'"
    ),
    stringsAsFactors = FALSE
  )
  if (odbc_failed(st) || nrow(st) == 0 || st$STATUS[1] != "VALID") {
    err <- RODBC::sqlQuery(
      channel,
      paste0(
        "SELECT LINE, TEXT FROM ALL_ERRORS WHERE OWNER = '", toupper(base_schema),
        "' AND NAME = '", trg, "' AND TYPE = 'TRIGGER' ORDER BY SEQUENCE"
      ),
      stringsAsFactors = FALSE
    )
    try(RODBC::sqlQuery(channel, paste0("ALTER TRIGGER ", toupper(base_schema), ".", trg, " DISABLE")),
      silent = TRUE
    )
    f <- paste0(trg, ".sql")
    writeLines(c(sql, "/"), f)
    stop("Trigger ", base_schema, ".", trg, " is not VALID (it was disabled).\n",
      if (is.data.frame(err) && nrow(err) > 0) paste(err$TEXT, collapse = "\n") else "",
      "\nDDL saved to '", f, "'. A common cause is a missing direct INSERT grant on the audit table.",
      call. = FALSE
    )
  }
  message("  Audit trigger ", base_schema, ".", trg, " is VALID -> ", audit_schema, ".", audit_table)
  invisible(TRUE)
}
