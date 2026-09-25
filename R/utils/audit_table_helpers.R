
## ---------------------------------------------------------------------------
## 1. Helper: identify changed rows in tables
## ---------------------------------------------------------------------------

find_changed_rows <- function(old_df,
                              new_df,
                              pk_cols,
                              normalize_names = TRUE) {
  stopifnot(is.data.frame(old_df), is.data.frame(new_df))
  
  if (normalize_names) {
    names(old_df) <- toupper(names(old_df))
    names(new_df) <- toupper(names(new_df))
    pk_cols <- toupper(pk_cols)
  }
  
  stopifnot(all(pk_cols %in% names(old_df)))
  stopifnot(all(pk_cols %in% names(new_df)))
  
  # Align schemas
  common_cols <- intersect(names(old_df), names(new_df))
  old_df <- old_df[, common_cols, drop = FALSE]
  new_df <- new_df[, common_cols, drop = FALSE]
  
  # Force column types to match
  type_map <- vapply(
    common_cols,
    function(col) {
      paste(class(old_df[[col]])[1], class(new_df[[col]])[1], sep = " vs ")
    },
    character(1)
  )
  
  mismatched_types <- common_cols[
    vapply(common_cols,
           function(col) !identical(class(old_df[[col]]), class(new_df[[col]])),
           logical(1))
  ]
  
  if (length(mismatched_types) > 0) {
    message("Coercing mismatched column types:\n",
            paste0("  - ", mismatched_types, ": ", type_map[mismatched_types],
                   collapse = "\n"))
  }
  
  coerce_pair <- function(x, y) {
    # Prefer character if either is character/factor
    if (is.character(x) || is.character(y) ||
        is.factor(x) || is.factor(y)) {
      list(as.character(x), as.character(y))
    } else if (inherits(x, "POSIXct") || inherits(y, "POSIXct") ||
               inherits(x, "Date") || inherits(y, "Date")) {
      list(as.character(x), as.character(y))
    } else if (is.numeric(x) && is.numeric(y)) {
      list(as.numeric(x), as.numeric(y))
    } else {
      # Safe fallback
      list(as.character(x), as.character(y))
    }
  }
  
  for (col in common_cols) {
    coerced <- coerce_pair(old_df[[col]], new_df[[col]])
    old_df[[col]] <- coerced[[1]]
    new_df[[col]] <- coerced[[2]]
  }
  
  # New / deleted rows
  new_rows <- dplyr::anti_join(new_df, old_df, by = pk_cols)
  deleted_rows <- dplyr::anti_join(old_df, new_df, by = pk_cols)
  
  # Changed rows
  if(tbl == "taxonomic_classification") pk_cols <- c("SPECIES_CODE", "DATABASE")
  compare_cols <- setdiff(common_cols, pk_cols)
  
  if (length(compare_cols) == 0) {
    changed_rows <- tibble::tibble()
  } else {
    changed_rows <- setdiff(new_df, old_df) %>% 
      rename_with(~paste0(., "_UPDATE"), -pk_cols) %>% 
      left_join(old_df %>% rename_with(~paste0(., "_CURRENT"), -pk_cols), 
                by = pk_cols)
  }
  
  # Order UPDATE/CURRENT columns next to each other
  sort_update_current_cols <- function(df) {
    
    all_cols <- names(df)
    
    # Keep PKs first
    pk_cols_local <- intersect(pk_cols, all_cols)
    
    base_cols <- unique(
      sub("_(UPDATE|CURRENT)$", "", 
          grep("_(UPDATE|CURRENT)$", all_cols, value = TRUE))
    )
    
    paired_cols <- unlist(
      lapply(base_cols, function(col) {
        c(
          paste0(col, "_UPDATE")[paste0(col, "_UPDATE") %in% all_cols],
          paste0(col, "_CURRENT")[paste0(col, "_CURRENT") %in% all_cols]
        )
      }),
      use.names = FALSE
    )
    
    remaining_cols <- setdiff(all_cols, c(pk_cols_local, paired_cols))
    
    df[, c(pk_cols_local, paired_cols, remaining_cols), drop = FALSE]
  }
  
  
  list(
    new_rows = new_rows %>% 
      rename_with(~paste0(., "_UPDATE"), -pk_cols),
    deleted_rows = deleted_rows %>% 
      rename_with(~paste0(., "_UPDATE"), -pk_cols),
    changed_rows = sort_update_current_cols(changed_rows)
  )
}




## ---------------------------------------------------------------------------
## 2. Build audit table
## ---------------------------------------------------------------------------


build_audit_rows <- function(df, operation, pk_cols) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  df %>%
    dplyr::mutate(
      OPERATION_TYPE = operation,
      OPERATION_TIMESTAMP = Sys.time(),
      USER_NAME = Sys.getenv("USER")
    )
}


# ------------------------------------------------------------------------
# 3. oracle_table_exists()
# ------------------------------------------------------------------------
oracle_table_exists <- function(channel, schema, table) {
  res <- RODBC::sqlQuery(
    channel,
    glue("
      SELECT COUNT(*)
      FROM ALL_TABLES
      WHERE TABLE_NAME = '{toupper(table)}'
        AND OWNER = '{toupper(schema)}'
    ")
  )
  
  !is.character(res) && res[1,1] > 0
}

# ------------------------------------------------------------------------
# 4. ensure_audit_columns()
# ------------------------------------------------------------------------
ensure_audit_columns <- function(channel, schema, table, audit_rows) {
  
  existing <- RODBC::sqlQuery(
    channel,
    glue("
      SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS
      WHERE OWNER = '{toupper(schema)}'
        AND TABLE_NAME = '{toupper(table)}'
    ")
  )
  
  if (is.character(existing)) stop(existing)
  
  missing <- setdiff(names(audit_rows) %>% toupper(),
                     existing$COLUMN_NAME)
  
  if (length(missing) == 0) return(invisible(TRUE))
  
  for (col in missing) {
    RODBC::sqlQuery(
      channel,
      glue("
        ALTER TABLE {schema}.{table}
        ADD {col} VARCHAR2(4000)
      ")
    )
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------
# 5. create_audit_table()
# ------------------------------------------------------------------------
create_audit_table <- function(channel, schema, table, audit_rows) {
  
  cols <- paste(
    sprintf("%s VARCHAR2(4000)", toupper(names(audit_rows))),
    collapse = ",\n"
  )
  
  sql <- glue("
    CREATE TABLE {schema}.{table} (
      {cols}
    )
  ")
  
  res <- RODBC::sqlQuery(channel, sql)
  if (is.character(res)) stop(res)
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------
# 6. build_insert_temp_table()  (for INSERT & MERGE sources)
# ------------------------------------------------------------------------
build_temp_table_name <- function(tbl) {
  paste0("GAP_PRODUCTS_TEMP_", toupper(tbl), "_", format(Sys.time(), "%Y%m%d%H%M%S"))
}

upload_temp_table <- function(channel, df, temp_table_name, schema, metadata_column = NULL) {
  # Upload df as a physical table
  gapindex::upload_oracle(
    x = df,
    table_name = temp_table_name,
    metadata_column = metadata_column,
    table_metadata = paste("Temp table for", temp_table_name),
    channel = channel,
    schema = schema,
    share_with_all_users = FALSE
  )
}

drop_temp_table <- function(channel, schema, temp_table_name) {
  RODBC::sqlDrop(channel, sqtable = glue("{schema}.{temp_table_name}"))
}

# ------------------------------------------------------------------------
# 7. APPLY DELETE via temp table
# ------------------------------------------------------------------------
apply_safe_delete <- function(channel, main_schema, main_table,
                              df, pk_cols) {
  
  if (nrow(df) == 0) return(invisible(TRUE))
  
  temp_table <- build_temp_table_name(main_table)
  
  upload_temp_table(channel, df[pk_cols], temp_table, main_schema)
  
  where_clause <- paste(
    glue("MAIN.{col} = TEMP.{col}", col = pk_cols),
    collapse = " AND "
  )
  
  sql <- glue("
    DELETE FROM {main_schema}.{main_table} MAIN
    WHERE EXISTS (
      SELECT 1 FROM {main_schema}.{temp_table} TEMP
      WHERE {where_clause}
    )
  ")
  
  res <- RODBC::sqlQuery(channel, sql)
  if (is.character(res)) stop(res)
  
  drop_temp_table(channel, main_schema, temp_table)
}

# ------------------------------------------------------------------------
# 8. APPLY INSERT via temp table
# ------------------------------------------------------------------------
apply_safe_insert <- function(channel, main_schema, main_table, df) {
  
  if (nrow(df) == 0) return(invisible(TRUE))
  
  temp_table <- build_temp_table_name(main_table)
  
  upload_temp_table(channel, df, temp_table, main_schema)
  
  cols <- paste(toupper(names(df)), collapse = ", ")
  
  sql <- glue("
    INSERT INTO {main_schema}.{main_table} ({cols})
    SELECT {cols}
    FROM {main_schema}.{temp_table}
  ")
  
  res <- RODBC::sqlQuery(channel, sql)
  if (is.character(res)) stop(res)
  
  drop_temp_table(channel, main_schema, temp_table)
}

# ------------------------------------------------------------------------
# 9. APPLY UPDATE via MERGE (temp table)
# ------------------------------------------------------------------------
apply_safe_merge_update <- function(channel, main_schema, main_table,
                                    df, pk_cols) {
  
  if (nrow(df) == 0) return(invisible(TRUE))
  
  temp_table <- build_temp_table_name(main_table)
  
  upload_temp_table(channel, df, temp_table, main_schema)
  
  set_cols <- setdiff(names(df), pk_cols)
  set_clause <- paste(
    glue("MAIN.{toupper(col)} = TEMP.{toupper(col)}", col = set_cols),
    collapse = ", "
  )
  
  join_clause <- paste(
    glue("MAIN.{col} = TEMP.{col}", col = pk_cols),
    collapse = " AND "
  )
  
  sql <- glue("
    MERGE INTO {main_schema}.{main_table} MAIN
    USING {main_schema}.{temp_table} TEMP
    ON ({join_clause})
    WHEN MATCHED THEN UPDATE SET
      {set_clause}
  ")
  
  res <- RODBC::sqlQuery(channel, sql)
  if (is.character(res)) stop(res)
  
  drop_temp_table(channel, main_schema, temp_table)
}

# ------------------------------------------------------------------------
# 10. safe_apply_taxonomy_updates()
# ------------------------------------------------------------------------
safe_apply_taxonomy_updates <- function(channel, tbl, changed_rows,
                                        schema = "GAP_PRODUCTS",
                                        pk_cols) {
  
  main_table <- toupper(tbl)
  
  # DELETE
  apply_safe_delete(
    channel,
    main_schema = schema,
    main_table = main_table,
    df = changed_rows$deleted_rows,
    pk_cols = pk_cols
  )
  
  # INSERT
  apply_safe_insert(
    channel,
    main_schema = schema,
    main_table = main_table,
    df = changed_rows$new_rows
  )
  
  # UPDATE (MERGE)
  apply_safe_merge_update(
    channel,
    main_schema = schema,
    main_table = main_table,
    df = changed_rows$changed_rows,
    pk_cols = pk_cols
  )
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------
# 11. TESTTHAT UTILITIES
# ------------------------------------------------------------------------
expect_same_columns <- function(df1, df2) {
  testthat::expect_equal(
    sort(names(df1)),
    sort(names(df2))
  )
}

expect_valid_pk <- function(df, pk_cols) {
  testthat::expect_false(anyDuplicated(df[pk_cols]))
}

expect_no_missing_pk <- function(df, pk_cols) {
  for (pk in pk_cols) {
    testthat::expect_false(any(is.na(df[[pk]])))
  }
}
