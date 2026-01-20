# ==============================================================================
# EXCEL AUDIT WORKBOOK GENERATOR - WITH FORMULAS & RAW NUMBERS
# ==============================================================================
# Dashboard: raw numbers with color coding
# Data sheets: summary uses Excel formulas referencing data cells
# Sorting: strictly chronological (year, then month)
# ==============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(scales)
  library(dplyr)
  library(lubridate)
  library(DBI)
  library(RPostgres)
  library(tibble)
})

# ==============================================================================
# STYLES
# ==============================================================================

style_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#0C275C", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE,
  fontSize = 10, fontName = "Arial"
)

style_metric <- createStyle(
  fgFill = "#CEDFF0", halign = "left", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight",
  fontSize = 10, fontName = "Arial"
)

style_metric_italic <- createStyle(
  fgFill = "#CEDFF0", halign = "left", valign = "center",
  textDecoration = "italic", border = "TopBottomLeftRight",
  fontSize = 9, fontName = "Arial"
)

style_value <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial"
)

style_positive <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#006400"
)

style_negative <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#8B0000"
)

style_neutral <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#000000"
)

style_data_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#404040", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)

style_summary_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#0C275C", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)

style_summary_label <- createStyle(
  fgFill = "#D6DCE4", halign = "left", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight"
)

style_summary_value <- createStyle(
  fgFill = "#D6DCE4", halign = "right", valign = "center",
  border = "TopBottomLeftRight", numFmt = "#,##0.0"
)

style_election_label <- createStyle(
  fgFill = "#FFFF00", halign = "left", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight"
)

style_title <- createStyle(
  textDecoration = "bold", fontSize = 14, fontName = "Arial"
)

style_subtitle <- createStyle(
  fontSize = 10, fontName = "Arial", fontColour = "#666666"
)

# Number formats for dashboard
style_value_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  numFmt = "#,##0"
)

style_value_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  numFmt = "#,##0.0"
)

style_positive_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#006400", numFmt = "+#,##0;-#,##0;0"
)

style_negative_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#8B0000", numFmt = "+#,##0;-#,##0;0"
)

style_positive_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#006400", numFmt = "+#,##0.0;-#,##0.0;0.0"
)

style_negative_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#8B0000", numFmt = "+#,##0.0;-#,##0.0;0.0"
)

style_neutral_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#000000", numFmt = "+#,##0;-#,##0;0"
)

style_neutral_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#000000", numFmt = "+#,##0.0;-#,##0.0;0.0"
)

# ==============================================================================
# DATABASE HELPER
# ==============================================================================

fetch_db <- function(query) {
  conn <- DBI::dbConnect(RPostgres::Postgres())
  tryCatch({
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  }, error = function(e) {
    warning("DB error: ", e$message)
    tibble::tibble()
  }, finally = DBI::dbDisconnect(conn))
}

# ==============================================================================
# STRING-BASED LABEL GENERATORS (MATCHING REST OF CODEBASE)
# ==============================================================================

make_lfs_label <- function(end_date) {
  start_date <- end_date %m-% months(2)
  sprintf("%s-%s %s",
          format(start_date, "%b"),
          format(end_date, "%b"),
          format(end_date, "%Y"))
}

make_payroll_label <- function(date) {
  format(date, "%B %Y")
}

make_ymd_label <- function(date) {
  format(date, "%Y-%m-%d")
}

make_datetime_label <- function(date) {
  paste0(format(date, "%Y-%m-%d"), " 00:00:00")
}

# ==============================================================================
# CHRONOLOGICAL SORT HELPER - FIXED FOR YEAR+MONTH
# ==============================================================================

make_sort_key <- function(date_str) {
  x <- as.character(date_str)
  if (is.na(x) || x == "") return(NA_real_)
  
  # Remove provisional/revised markers like "[p]" or "[r]"
  clean_x <- trimws(gsub("\\s*\\[.*\\]\\s*$", "", x))
  
  # Format: "Jul-Sep 2025" -> year * 100 + end_month
  # Check for 3-letter month patterns
  if (grepl("[A-Za-z]{3}.*[A-Za-z]{3}.*[0-9]{4}", clean_x)) {
    month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
    # Extract all 3-letter sequences
    months_found <- regmatches(clean_x, gregexpr("[A-Za-z]{3}", clean_x))[[1]]
    year_found <- regmatches(clean_x, gregexpr("[0-9]{4}", clean_x))[[1]]
    
    if (length(months_found) >= 2 && length(year_found) >= 1) {
      end_month <- month_map[tolower(months_found[2])]
      yr <- as.integer(year_found[1])
      if (!is.na(end_month) && !is.na(yr)) {
        return(yr * 100 + end_month)
      }
    }
  }
  
  # Format: "July 2025"
  d <- suppressWarnings(as.Date(paste0("01 ", clean_x), format = "%d %B %Y"))
  if (!is.na(d)) {
    return(as.numeric(format(d, "%Y")) * 100 + as.numeric(format(d, "%m")))
  }
  
  # Format: "2025-07-01" or "2025-07-01 00:00:00"
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", clean_x)) {
    yr <- as.integer(substr(clean_x, 1, 4))
    mo <- as.integer(substr(clean_x, 6, 7))
    if (!is.na(yr) && !is.na(mo)) {
      return(yr * 100 + mo)
    }
  }
  
  NA_real_
}

sort_chronologically <- function(df) {
  if (nrow(df) == 0 || !"Date" %in% names(df)) return(df)
  
  # Calculate sort keys
  sort_keys <- vapply(df$Date, make_sort_key, numeric(1))
  
  # Sort by key
  df[order(sort_keys, na.last = TRUE), ]
}

# ==============================================================================
# BUILD DASHBOARD SHEET - RAW NUMBERS WITH COLOR CODING
# ==============================================================================

build_dashboard_sheet <- function(wb, lfs_period_label, envir = parent.frame()) {
  
  sheet <- "Dashboard"
  addWorksheet(wb, sheet)
  
  get_val <- function(name) {
    if (exists(name, envir = envir)) get(name, envir = envir) else NA_real_
  }
  
  metrics <- tribble(
    ~Metric, ~type, ~invert, ~italic,
    "Employment (000s) 16+", "count", FALSE, FALSE,
    "Employment rate (16-64)", "rate", FALSE, FALSE,
    "Unemployment, 16+ (000s)", "count", TRUE, FALSE,
    "Unemployment rate, 16+", "rate", TRUE, FALSE,
    "Economic inactivity (000s) (16-64)", "count", TRUE, FALSE,
    "50-64s inactivity (000s)", "count", TRUE, TRUE,
    "Economic inactivity rate (16-64)", "rate", TRUE, FALSE,
    "50-64s inactivity rate", "rate", TRUE, TRUE,
    "Vacancies (000s)", "exempt", NA, FALSE,
    "Payroll employees (000s)", "exempt", FALSE, FALSE,
    "Annual average wages in cash terms (total pay, incl. bonuses)", "wages", FALSE, FALSE,
    "Annual average wages adjusted for CPI inflation (total pay, incl. bonuses)", "wages", FALSE, FALSE
  )
  
  var_map <- list(
    "Employment (000s) 16+" = list(cur = "emp16_cur", dq = "emp16_dq", dy = "emp16_dy", dc = "emp16_dc", de = "emp16_de"),
    "Employment rate (16-64)" = list(cur = "emp_rt_cur", dq = "emp_rt_dq", dy = "emp_rt_dy", dc = "emp_rt_dc", de = "emp_rt_de"),
    "Unemployment, 16+ (000s)" = list(cur = "unemp16_cur", dq = "unemp16_dq", dy = "unemp16_dy", dc = "unemp16_dc", de = "unemp16_de"),
    "Unemployment rate, 16+" = list(cur = "unemp_rt_cur", dq = "unemp_rt_dq", dy = "unemp_rt_dy", dc = "unemp_rt_dc", de = "unemp_rt_de"),
    "Economic inactivity (000s) (16-64)" = list(cur = "inact_cur", dq = "inact_dq", dy = "inact_dy", dc = "inact_dc", de = "inact_de"),
    "50-64s inactivity (000s)" = list(cur = "inact5064_cur", dq = "inact5064_dq", dy = "inact5064_dy", dc = "inact5064_dc", de = "inact5064_de"),
    "Economic inactivity rate (16-64)" = list(cur = "inact_rt_cur", dq = "inact_rt_dq", dy = "inact_rt_dy", dc = "inact_rt_dc", de = "inact_rt_de"),
    "50-64s inactivity rate" = list(cur = "inact5064_rt_cur", dq = "inact5064_rt_dq", dy = "inact5064_rt_dy", dc = "inact5064_rt_dc", de = "inact5064_rt_de"),
    "Vacancies (000s)" = list(cur = "vac_cur", dq = "vac_dq", dy = "vac_dy", dc = "vac_dc", de = "vac_de"),
    "Payroll employees (000s)" = list(cur = "payroll_cur", dq = "payroll_dq", dy = "payroll_dy", dc = "payroll_dc", de = "payroll_de"),
    "Annual average wages in cash terms (total pay, incl. bonuses)" = list(cur = "latest_wages", dq = "wages_change_q", dy = "wages_change_y", dc = "wages_change_covid", de = "wages_change_election"),
    "Annual average wages adjusted for CPI inflation (total pay, incl. bonuses)" = list(cur = "latest_wages_cpi", dq = "wages_cpi_change_q", dy = "wages_cpi_change_y", dc = "wages_cpi_change_covid", de = "wages_cpi_change_election")
  )
  
  # Title
  writeData(wb, sheet, paste0("Key Metrics, ", lfs_period_label), startRow = 1, startCol = 1)
  addStyle(wb, sheet, style_title, rows = 1, cols = 1)
  
  writeData(wb, sheet, paste("Generated:", format(Sys.time(), "%d %B %Y %H:%M")), startRow = 2, startCol = 1)
  addStyle(wb, sheet, style_subtitle, rows = 2, cols = 1)
  
  # Headers
  headers <- c("Metric", "Current", "Change on quarter", "Change on the year",
               "Change since Covid-19", "Change since 2024 election")
  
  header_row <- 4
  for (i in seq_along(headers)) {
    writeData(wb, sheet, headers[i], startRow = header_row, startCol = i)
  }
  addStyle(wb, sheet, style_header, rows = header_row, cols = 1:6, gridExpand = TRUE)
  
  # Data rows
  for (row_idx in seq_len(nrow(metrics))) {
    metric_name <- metrics$Metric[row_idx]
    metric_type <- metrics$type[row_idx]
    is_invert <- metrics$invert[row_idx]
    is_italic <- metrics$italic[row_idx]
    
    vars <- var_map[[metric_name]]
    if (is.null(vars)) next
    
    cur_val <- get_val(vars$cur)
    dq_val <- get_val(vars$dq)
    dy_val <- get_val(vars$dy)
    dc_val <- get_val(vars$dc)
    de_val <- get_val(vars$de)
    
    excel_row <- header_row + row_idx
    
    # Metric name
    writeData(wb, sheet, metric_name, startRow = excel_row, startCol = 1)
    addStyle(wb, sheet, if(is_italic) style_metric_italic else style_metric, rows = excel_row, cols = 1)
    
    # Determine if integer or decimal display
    is_int_type <- metric_type %in% c("count", "exempt", "wages")
    
    # Transform current value for display
    if (metric_type == "count") {
      cur_display <- cur_val / 1000  # Convert to thousands
    } else {
      cur_display <- cur_val
    }
    
    # Write current value (raw number)
    writeData(wb, sheet, cur_display, startRow = excel_row, startCol = 2)
    addStyle(wb, sheet, if(is_int_type) style_value_int else style_value_dec1, rows = excel_row, cols = 2)
    
    # Helper to get style based on value and invert flag
    get_change_style <- function(val, is_int) {
      if (is.na(is_invert)) {
        # Neutral (vacancies)
        return(if(is_int) style_neutral_int else style_neutral_dec1)
      }
      if (is.na(val) || val == 0) {
        return(if(is_int) style_value_int else style_value_dec1)
      }
      is_positive <- val > 0
      if (is_invert) is_positive <- !is_positive
      
      if (is_positive) {
        if(is_int) style_positive_int else style_positive_dec1
      } else {
        if(is_int) style_negative_int else style_negative_dec1
      }
    }
    
    # Transform change values for display
    transform_change <- function(val) {
      if (metric_type == "count") val / 1000 else val
    }
    
    # Write change values (raw numbers with color coding)
    changes <- list(dq_val, dy_val, dc_val, de_val)
    for (i in seq_along(changes)) {
      col <- i + 2
      change_val <- transform_change(changes[[i]])
      writeData(wb, sheet, change_val, startRow = excel_row, startCol = col)
      addStyle(wb, sheet, get_change_style(changes[[i]], is_int_type), rows = excel_row, cols = col)
    }
  }
  
  # Column widths
  setColWidths(wb, sheet, cols = 1, widths = 55)
  setColWidths(wb, sheet, cols = 2:6, widths = 18)
  setRowHeights(wb, sheet, rows = header_row, heights = 30)
  
  # Freeze panes
  freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE, firstActiveRow = 5, firstActiveCol = 2)
  
  wb
}

# ==============================================================================
# BUILD DATA SHEET WITH FORMULA-BASED SUMMARY
# ==============================================================================

build_data_sheet_with_summary <- function(wb, sheet_name, title, data, source_info = NULL,
                                          label_type = "lfs",
                                          anchor_date = NULL,
                                          covid_label = NULL,
                                          election_label = NULL) {
  
  addWorksheet(wb, sheet_name)
  
  if (is.null(data) || nrow(data) == 0) {
    writeData(wb, sheet_name, "No data available", startRow = 1, startCol = 1)
    return(wb)
  }
  
  # Title section
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet_name, style_title, rows = 1, cols = 1)
  
  if (!is.null(source_info)) {
    writeData(wb, sheet_name, paste("Source:", source_info), startRow = 2, startCol = 1)
    addStyle(wb, sheet_name, style_subtitle, rows = 2, cols = 1)
  }
  
  writeData(wb, sheet_name, paste("Rows:", nrow(data), "| Generated:", format(Sys.time(), "%d %B %Y %H:%M")), 
            startRow = 3, startCol = 1)
  addStyle(wb, sheet_name, style_subtitle, rows = 3, cols = 1)
  
  # Get value columns (everything except Date)
  value_cols <- setdiff(names(data), "Date")
  n_cols <- length(value_cols)
  
  # ====================
  # DATA TABLE FIRST (so we know row numbers for formulas)
  # ====================
  
  data_start <- 14  # Leave room for summary above
  
  writeData(wb, sheet_name, "Source Data", startRow = data_start - 1, startCol = 1)
  addStyle(wb, sheet_name, createStyle(textDecoration = "bold", fontSize = 11), rows = data_start - 1, cols = 1)
  
  # Write data
  writeData(wb, sheet_name, data, startRow = data_start, startCol = 1,
            colNames = TRUE, headerStyle = style_data_header)
  
  # ====================
  # SUMMARY SECTION WITH FORMULAS
  # ====================
  
  summary_start <- 5
  can_calc_summary <- !is.null(anchor_date)
  
  if (can_calc_summary) {
    # Generate reference labels based on label type
    if (label_type == "lfs") {
      lab_cur <- make_lfs_label(anchor_date)
      lab_q <- make_lfs_label(anchor_date %m-% months(3))
      lab_y <- make_lfs_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "Dec-Feb 2020"
      lab_election <- if (!is.null(election_label)) election_label else "Apr-Jun 2024"
    } else if (label_type == "payroll") {
      lab_cur <- make_payroll_label(anchor_date)
      lab_q <- make_payroll_label(anchor_date %m-% months(3))
      lab_y <- make_payroll_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "February 2020"
      lab_election <- if (!is.null(election_label)) election_label else "June 2024"
    } else if (label_type == "ymd") {
      lab_cur <- make_ymd_label(anchor_date)
      lab_q <- make_ymd_label(anchor_date %m-% months(3))
      lab_y <- make_ymd_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "2020-02-01"
      lab_election <- if (!is.null(election_label)) election_label else "2024-06-01"
    } else {
      lab_cur <- make_datetime_label(anchor_date)
      lab_q <- make_datetime_label(anchor_date %m-% months(3))
      lab_y <- make_datetime_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "2020-02-01 00:00:00"
      lab_election <- if (!is.null(election_label)) election_label else "2024-06-01 00:00:00"
    }
    
    # Find data row indices by string matching
    find_data_row <- function(label) {
      idx <- which(trimws(data$Date) == trimws(label))
      if (length(idx) > 0) return(idx[1])
      idx <- which(startsWith(trimws(data$Date), trimws(label)))
      if (length(idx) > 0) return(idx[1])
      NA
    }
    
    row_cur <- find_data_row(lab_cur)
    row_q <- find_data_row(lab_q)
    row_y <- find_data_row(lab_y)
    row_covid <- find_data_row(lab_covid)
    row_election <- find_data_row(lab_election)
    
    # Convert data row index to Excel row number (data_start is header row)
    to_excel_row <- function(data_idx) {
      if (is.na(data_idx)) return(NA)
      data_start + data_idx  # +1 for header, but data_idx is 1-based
    }
    
    excel_row_cur <- to_excel_row(row_cur)
    excel_row_q <- to_excel_row(row_q)
    excel_row_y <- to_excel_row(row_y)
    excel_row_covid <- to_excel_row(row_covid)
    excel_row_election <- to_excel_row(row_election)
    
    # Summary header
    writeData(wb, sheet_name, "Summary", startRow = summary_start, startCol = 1)
    for (i in seq_along(value_cols)) {
      writeData(wb, sheet_name, value_cols[i], startRow = summary_start, startCol = i + 1)
    }
    addStyle(wb, sheet_name, style_summary_header, rows = summary_start, cols = 1:(n_cols + 1), gridExpand = TRUE)
    
    # Summary row definitions
    summary_rows <- list(
      list(label = paste0("Current (", lab_cur, ")"), comp_excel_row = NULL, is_current = TRUE),
      list(label = paste0("Change on quarter (vs ", lab_q, ")"), comp_excel_row = excel_row_q, is_current = FALSE),
      list(label = paste0("Change on year (vs ", lab_y, ")"), comp_excel_row = excel_row_y, is_current = FALSE),
      list(label = paste0("Change since Covid (vs ", lab_covid, ")"), comp_excel_row = excel_row_covid, is_current = FALSE),
      list(label = paste0("Change since election (vs ", lab_election, ")"), comp_excel_row = excel_row_election, is_current = FALSE)
    )
    
    for (s in seq_along(summary_rows)) {
      row_num <- summary_start + s
      sr <- summary_rows[[s]]
      
      writeData(wb, sheet_name, sr$label, startRow = row_num, startCol = 1)
      
      # Style - yellow for election row
      if (s == 5) {
        addStyle(wb, sheet_name, style_election_label, rows = row_num, cols = 1)
      } else {
        addStyle(wb, sheet_name, style_summary_label, rows = row_num, cols = 1)
      }
      
      # Write formulas for each value column
      for (i in seq_along(value_cols)) {
        col_letter <- LETTERS[i + 1]  # B, C, D, etc.
        
        if (sr$is_current) {
          # Current: just reference the cell
          if (!is.na(excel_row_cur)) {
            formula <- paste0("=", col_letter, excel_row_cur)
            writeFormula(wb, sheet_name, formula, startRow = row_num, startCol = i + 1)
          }
        } else {
          # Change: current - comparison
          if (!is.na(excel_row_cur) && !is.na(sr$comp_excel_row)) {
            formula <- paste0("=", col_letter, excel_row_cur, "-", col_letter, sr$comp_excel_row)
            writeFormula(wb, sheet_name, formula, startRow = row_num, startCol = i + 1)
          }
        }
        addStyle(wb, sheet_name, style_summary_value, rows = row_num, cols = i + 1)
      }
    }
  } else {
    writeData(wb, sheet_name, "Summary: Not available (no anchor date provided)", 
              startRow = summary_start, startCol = 1)
    addStyle(wb, sheet_name, style_subtitle, rows = summary_start, cols = 1)
  }
  
  # Column widths
  setColWidths(wb, sheet_name, cols = 1, widths = 45)
  for (i in seq_along(value_cols)) {
    max_width <- max(nchar(as.character(value_cols[i])), 12, na.rm = TRUE)
    setColWidths(wb, sheet_name, cols = i + 1, widths = min(max_width + 2, 25))
  }
  
  # Freeze at data header row
  freezePane(wb, sheet_name, firstRow = TRUE, firstActiveRow = data_start + 1)
  
  # Add filter to data
  addFilter(wb, sheet_name, row = data_start, cols = 1:ncol(data))
  
  wb
}

# ==============================================================================
# DATA FETCH FUNCTIONS - WIDE FORMAT WITH CHRONOLOGICAL SORT
# ==============================================================================

fetch_lfs_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__age_group"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c(
    "MGRZ" = "Employment 16+ (000s)",
    "LF24" = "Employment Rate 16-64 (%)",
    "MGSC" = "Unemployment 16+ (000s)",
    "MGSX" = "Unemployment Rate 16+ (%)",
    "LF2M" = "Inactivity 16-64 (000s)",
    "LF2S" = "Inactivity Rate 16-64 (%)",
    "LF2A" = "Inactivity 50-64 (000s)",
    "LF2W" = "Inactivity Rate 50-64 (%)"
  )
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_vacancies_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__vacancies_business"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c(
    "AP2Y" = "Vacancies (000s)",
    "AP3K" = "Quarterly Change (000s)"
  )
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_payroll_wide <- function() {
  query <- 'SELECT time_period, unit_type, value
            FROM "ons"."labour_market__payrolled_employees"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, unit_type) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = unit_type, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_industry_wide <- function() {
  query <- 'SELECT time_period, sic_section, value
            FROM "ons"."labour_market__employees_industry"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  sic_names <- c(
    "A" = "Agriculture", "B" = "Mining", "C" = "Manufacturing",
    "D" = "Electricity", "E" = "Water", "F" = "Construction",
    "G" = "Retail", "H" = "Transport", "I" = "Hospitality",
    "J" = "IT & Comms", "K" = "Finance", "L" = "Real Estate",
    "M" = "Professional", "N" = "Admin", "O" = "Public Admin",
    "P" = "Education", "Q" = "Health", "R" = "Arts",
    "S" = "Other Services"
  )
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(sic_section %in% names(sic_names),
                        paste0(sic_section, "-", sic_names[sic_section]),
                        sic_section)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_wages_total_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__weekly_earnings_total"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c(
    "KAB9" = "AWE Total (£/week)",
    "KAC3" = "AWE Total YoY (%)",
    "KAC6" = "AWE Total Private YoY (%)",
    "KAC9" = "AWE Total Public YoY (%)"
  )
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_wages_regular_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__weekly_earnings_regular"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c(
    "KAI7" = "AWE Regular (£/week)",
    "KAI9" = "AWE Regular YoY (%)",
    "KAJ4" = "AWE Regular Private YoY (%)",
    "KAJ7" = "AWE Regular Public YoY (%)"
  )
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_wages_cpi_wide <- function() {
  query <- 'SELECT time_period, earnings_type, earnings_metric, value
            FROM "ons"."labour_market__weekly_earnings_cpi"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(earnings_type, "-", earnings_metric)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_inactivity_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__inactivity"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c(
    "LF63" = "Student",
    "LF65" = "Family/Home",
    "LF67" = "Temp Sick",
    "LF69" = "Long-term Sick",
    "LFL8" = "Discouraged",
    "LF6B" = "Retired",
    "LF6D" = "Other"
  )
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_redundancy_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__redundancies"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c("BEIR" = "Redundancy Rate (per 1000)")
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_days_lost_wide <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__disputes"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  code_names <- c("BBFW" = "Days Lost (000s)")
  
  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = ifelse(dataset_indentifier_code %in% names(code_names),
                        code_names[dataset_indentifier_code],
                        dataset_indentifier_code)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

fetch_hr1_wide <- function() {
  query <- 'SELECT time_period, region, value
            FROM "ons"."labour_market__redundancies_region"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())
  
  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, region) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = region, values_from = value) %>%
    rename(Date = time_period)
  
  sort_chronologically(result)
}

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

create_audit_workbook <- function(output_path,
                                  calculations_path = "utils/calculations.R",
                                  config_path = "utils/config.R",
                                  verbose = TRUE) {
  
  if (verbose) message("Sourcing calculations...")
  
  calc_env <- new.env()
  
  if (file.exists(config_path)) {
    source(config_path, local = calc_env)
  }
  
  if (file.exists(calculations_path)) {
    source(calculations_path, local = calc_env)
  } else {
    stop("calculations.R not found at: ", calculations_path)
  }
  
  # Get anchor dates from calc_env
  lfs_period_label <- if (exists("lfs_period_label", envir = calc_env)) {
    get("lfs_period_label", envir = calc_env)
  } else {
    format(Sys.Date(), "%B %Y")
  }
  
  lfs_anchor <- if (exists("lfs", envir = calc_env)) {
    get("lfs", envir = calc_env)$emp16$end
  } else NULL
  
  payroll_anchor <- if (exists("payroll", envir = calc_env)) {
    get("payroll", envir = calc_env)$anchor
  } else NULL
  
  wages_anchor <- if (exists("wages_nom", envir = calc_env)) {
    get("wages_nom", envir = calc_env)$anchor
  } else NULL
  
  wb <- createWorkbook()
  
  # 1. DASHBOARD
  if (verbose) message("Building: Dashboard...")
  wb <- build_dashboard_sheet(wb, lfs_period_label, envir = calc_env)
  
  # 2. LFS DATA
  if (verbose) message("Building: LFS Data...")
  data <- tryCatch(fetch_lfs_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "LFS Data", "Labour Force Survey",
                                      data, "labour_market__age_group",
                                      label_type = "lfs", anchor_date = lfs_anchor,
                                      covid_label = "Dec-Feb 2020", election_label = "Apr-Jun 2024")
  
  # 3. VACANCIES
  if (verbose) message("Building: Vacancies...")
  data <- tryCatch(fetch_vacancies_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "Vacancies", "Job Vacancies",
                                      data, "labour_market__vacancies_business",
                                      label_type = "lfs", anchor_date = lfs_anchor,
                                      covid_label = "Jan-Mar 2020", election_label = "Apr-Jun 2024")
  
  # 4. PAYROLL
  if (verbose) message("Building: Payroll...")
  data <- tryCatch(fetch_payroll_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "Payroll", "Payrolled Employees",
                                      data, "labour_market__payrolled_employees",
                                      label_type = "payroll", anchor_date = payroll_anchor,
                                      covid_label = "February 2020", election_label = "June 2024")
  
  # 5. INDUSTRY
  if (verbose) message("Building: Industry...")
  data <- tryCatch(fetch_industry_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "Industry", "Employees by Industry",
                                      data, "labour_market__employees_industry",
                                      label_type = "payroll", anchor_date = payroll_anchor,
                                      covid_label = "February 2020", election_label = "June 2024")
  
  # 6. WAGES TOTAL
  if (verbose) message("Building: AWE Total...")
  data <- tryCatch(fetch_wages_total_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "AWE Total", "AWE - Total Pay",
                                      data, "labour_market__weekly_earnings_total",
                                      label_type = "ymd", anchor_date = wages_anchor,
                                      covid_label = "2020-02-01", election_label = "2024-06-01")
  
  # 7. WAGES REGULAR
  if (verbose) message("Building: AWE Regular...")
  data <- tryCatch(fetch_wages_regular_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "AWE Regular", "AWE - Regular Pay",
                                      data, "labour_market__weekly_earnings_regular",
                                      label_type = "ymd", anchor_date = wages_anchor,
                                      covid_label = "2020-02-01", election_label = "2024-06-01")
  
  # 8. WAGES CPI
  if (verbose) message("Building: AWE Real...")
  data <- tryCatch(fetch_wages_cpi_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "AWE Real", "Real AWE (CPI)",
                                      data, "labour_market__weekly_earnings_cpi",
                                      label_type = "datetime", anchor_date = wages_anchor,
                                      covid_label = "2020-02-01 00:00:00", election_label = "2024-06-01 00:00:00")
  
  # 9. INACTIVITY
  if (verbose) message("Building: Inactivity...")
  data <- tryCatch(fetch_inactivity_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "Inactivity", "Inactivity by Reason",
                                      data, "labour_market__inactivity",
                                      label_type = "lfs", anchor_date = lfs_anchor,
                                      covid_label = "Dec-Feb 2020", election_label = "Apr-Jun 2024")
  
  # 10. REDUNDANCY
  if (verbose) message("Building: Redundancies...")
  data <- tryCatch(fetch_redundancy_wide(), error = function(e) tibble())
  wb <- build_data_sheet_with_summary(wb, "Redundancies", "LFS Redundancy Rate",
                                      data, "labour_market__redundancies",
                                      label_type = "lfs", anchor_date = lfs_anchor,
                                      covid_label = "Dec-Feb 2020", election_label = "Apr-Jun 2024")
  
  # 11. DAYS LOST
  if (verbose) message("Building: Disputes...")
  data <- tryCatch(fetch_days_lost_wide(), error = function(e) tibble())
  days_lost_anchor <- if (exists("days_lost", envir = calc_env)) {
    get("days_lost", envir = calc_env)$anchor
  } else payroll_anchor
  wb <- build_data_sheet_with_summary(wb, "Disputes", "Working Days Lost",
                                      data, "labour_market__disputes",
                                      label_type = "payroll", anchor_date = days_lost_anchor,
                                      covid_label = "February 2020", election_label = "June 2024")
  
  # 12. HR1
  if (verbose) message("Building: HR1...")
  data <- tryCatch(fetch_hr1_wide(), error = function(e) tibble())
  hr1_anchor <- if (exists("hr1", envir = calc_env)) {
    get("hr1", envir = calc_env)$anchor
  } else NULL
  wb <- build_data_sheet_with_summary(wb, "HR1", "HR1 Notifications",
                                      data, "labour_market__redundancies_region",
                                      label_type = "datetime", anchor_date = hr1_anchor,
                                      covid_label = "2020-02-01 00:00:00", election_label = "2024-06-01 00:00:00")
  
  # Save
  saveWorkbook(wb, output_path, overwrite = TRUE)
  if (verbose) message("Created: ", output_path)
  invisible(output_path)
}

# ==============================================================================
# USAGE
# ==============================================================================
create_audit_workbook("LM_Stats_Audit.xlsx")
