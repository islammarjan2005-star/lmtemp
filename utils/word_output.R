# ==============================================================================
# word_output.R
# ==============================================================================
# Fills LMSB_placeholders_v2 placeholders and conditional-colour tokens.
# RUN FROM: Project root directory
# ==============================================================================

suppressPackageStartupMessages({
  library(officer)
  library(scales)
})

# ----------------------------
# Fallback formatters (uses helpers.R versions if present)
# ----------------------------
fmt_one_dec <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x, 1), nsmall = 1, trim = TRUE)
}

.format_int <- function(x) {
  if (exists("format_int", inherits = TRUE)) return(get("format_int", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  scales::comma(round(x), accuracy = 1)
}

.format_pct <- function(x) {
  if (exists("format_pct", inherits = TRUE)) return(get("format_pct", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  paste0(fmt_one_dec(x), "%")
}

.format_pp <- function(x) {
  if (exists("format_pp", inherits = TRUE)) return(get("format_pp", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  sign <- if (x > 0) "+" else if (x < 0) "-" else ""
  paste0(sign, fmt_one_dec(abs(x)), "pp")
}

.format_gbp_signed0 <- function(x) {
  if (exists("format_gbp_signed0", inherits = TRUE)) return(get("format_gbp_signed0", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  sign <- if (x > 0) "+" else if (x < 0) "-" else ""
  paste0(sign, "\u00A3", scales::comma(round(abs(x)), accuracy = 1))
}

fmt_int_signed <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  s <- scales::comma(abs(round(x)), accuracy = 1)
  if (x > 0) paste0("+", s) else if (x < 0) paste0("-", s) else "0"
}

# Counts stored as persons; displayed in 000s
fmt_count_000s_current <- function(x) .format_int(x / 1000)
fmt_count_000s_change <- function(x) fmt_int_signed(x / 1000)

# Payroll/vacancies stored in 000s
fmt_exempt_current <- function(x) .format_int(x)
fmt_exempt_change <- function(x) fmt_int_signed(x)

# manual_month like "oct2025" or "2025-10" -> "October 2025"
manual_month_to_label <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  x <- tolower(as.character(x))
  if (grepl("^[0-9]{4}-[0-9]{2}$", x)) {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    d <- as.Date(sprintf("%s-%s-01", parts[1], parts[2]))
    return(format(d, "%B %Y"))
  }
  if (grepl("^[a-z]{3}[0-9]{4}$", x)) {
    mon <- substr(x, 1, 3)
    yr <- substr(x, 4, 7)
    month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
    if (mon %in% names(month_map)) {
      d <- as.Date(sprintf("%s-%02d-01", yr, month_map[[mon]]))
      return(format(d, "%B %Y"))
    }
  }
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# ----------------------------
# Word replacement helpers
# ----------------------------
replace_all <- function(doc, key, val) {
  if (is.null(val) || length(val) == 0 || is.na(val)) val <- ""
  val <- as.character(val)
  doc <- body_replace_all_text(doc, key, val, fixed = TRUE)
  doc <- tryCatch(headers_replace_all_text(doc, key, val, fixed = TRUE), error = function(e) doc)
  doc <- tryCatch(footers_replace_all_text(doc, key, val, fixed = TRUE), error = function(e) doc)
  doc
}

fill_conditional <- function(doc, base, value_text, value_num, invert = FALSE, neutral = FALSE) {
  value_num <- suppressWarnings(as.numeric(value_num))
  if (is.na(value_num)) value_num <- 0
  
  p <- n <- z <- ""
  
  if (isTRUE(neutral)) {
    z <- value_text
  } else {
    if (value_num > 0) p <- value_text
    if (value_num < 0) n <- value_text
    if (value_num == 0) z <- value_text
    if (isTRUE(invert)) {
      tmp <- p; p <- n; n <- tmp
    }
  }
  
  doc <- replace_all(doc, paste0(base, "_p"), p)
  doc <- replace_all(doc, paste0(base, "_n"), n)
  doc <- replace_all(doc, paste0(base, "_z"), z)
  doc
}

# ----------------------------
# MAIN
# ----------------------------

generate_word_output <- function(template_path = "utils/DB.docx",
                                 output_path = "utils/DBoutput.docx",
                                 calculations_path = "utils/calculations.R",
                                 config_path = "utils/config.R",
                                 summary_path = "sheets/summary.R",
                                 top_ten_path = "sheets/top_ten_stats.R",
                                 manual_month_override = NULL,
                                 manual_month_hr1_override = NULL,
                                 verbose = TRUE) {
  
  # Source config first
  source(config_path, local = FALSE)
  
  if (!is.null(manual_month_override)) manual_month <<- tolower(manual_month_override)
  if (!is.null(manual_month_hr1_override)) manual_month_hr1 <<- tolower(manual_month_hr1_override)
  
  if (verbose && exists("manual_month", inherits = TRUE)) message("[word_output] manual_month = ", manual_month)
  
  # Source calculations (this sources helpers.R and all sheets)
  source(calculations_path, local = FALSE)
  
  # Source summary and top ten
  source(summary_path, local = FALSE)
  source(top_ten_path, local = FALSE)
  summary <- generate_summary()
  top10 <- generate_top_ten()
  
  doc <- read_docx(template_path)
  
  title_label <- if (exists("manual_month", inherits = TRUE)) manual_month_to_label(manual_month) else ""
  doc <- replace_all(doc, "Z1", title_label)
  if (exists("lfs_period_label", inherits = TRUE)) doc <- replace_all(doc, "LFS_PERIOD_LABEL", lfs_period_label)
  
  # Summary lines 1-10 (full text)
  for (i in 1:10) doc <- replace_all(doc, paste0("sl", i), summary[[paste0("line", i)]])

  # Top ten lines
  for (i in 10:1) doc <- replace_all(doc, paste0("tt", i), top10[[paste0("line", i)]])

  doc <- replace_all(doc, "RENDER_DATE", format(Sys.Date(), "%d %B %Y"))

  # ===========================================================================
  # SUMMARY LINE PLACEHOLDERS (individual values with underscores)
  # ===========================================================================

  # LINE 1 - Payroll (N1_*)
  doc <- replace_all(doc, "N1_dir", summary$N1_dir)
  doc <- replace_all(doc, "N1_dir_ing", summary$N1_dir_ing)
  doc <- replace_all(doc, "N1_dy_abs", summary$N1_dy_abs)
  doc <- replace_all(doc, "N1_dy_pct", summary$N1_dy_pct)
  doc <- replace_all(doc, "N1_dq_abs", summary$N1_dq_abs)
  doc <- replace_all(doc, "N1_dq_pct", summary$N1_dq_pct)

  # LINE 2 - Flash (O2_*)
  doc <- replace_all(doc, "O2_label", summary$O2_label)
  doc <- replace_all(doc, "O2_dir", summary$O2_dir)
  doc <- replace_all(doc, "O2_dy_abs", summary$O2_dy_abs)
  doc <- replace_all(doc, "O2_dy_pct", summary$O2_dy_pct)
  doc <- replace_all(doc, "O2_dm_abs", summary$O2_dm_abs)
  doc <- replace_all(doc, "O2_dm_pct", summary$O2_dm_pct)
  doc <- replace_all(doc, "O2_revision", summary$O2_revision)

  # LINE 3 - Workforce Jobs STATIC (P3_*)
  doc <- replace_all(doc, "P3_dq_abs", summary$P3_dq_abs)
  doc <- replace_all(doc, "P3_dq_pct", summary$P3_dq_pct)
  doc <- replace_all(doc, "P3_driver", summary$P3_driver)
  doc <- replace_all(doc, "P3_dy_abs", summary$P3_dy_abs)
  doc <- replace_all(doc, "P3_dy_pct", summary$P3_dy_pct)

  # LINE 4 - Vacancies (Q4_*)
  doc <- replace_all(doc, "Q4_dir_ing", summary$Q4_dir_ing)
  doc <- replace_all(doc, "Q4_dq_abs", summary$Q4_dq_abs)
  doc <- replace_all(doc, "Q4_dq_pct", summary$Q4_dq_pct)
  doc <- replace_all(doc, "Q4_cur", summary$Q4_cur)

  # LINE 5 - LFS Rates (R5_*)
  doc <- replace_all(doc, "R5_emp_dir", summary$R5_emp_dir)
  doc <- replace_all(doc, "R5_emp_cur", summary$R5_emp_cur)
  doc <- replace_all(doc, "R5_emp_dq", summary$R5_emp_dq)
  doc <- replace_all(doc, "R5_unemp_dir", summary$R5_unemp_dir)
  doc <- replace_all(doc, "R5_unemp_cur", summary$R5_unemp_cur)
  doc <- replace_all(doc, "R5_unemp_dq", summary$R5_unemp_dq)
  doc <- replace_all(doc, "R5_inact_dir", summary$R5_inact_dir)
  doc <- replace_all(doc, "R5_inact_cur", summary$R5_inact_cur)
  doc <- replace_all(doc, "R5_inact_dq", summary$R5_inact_dq)

  # LINE 6 - Youth Unemployment STATIC (S6_*)
  doc <- replace_all(doc, "S6_dq", summary$S6_dq)
  doc <- replace_all(doc, "S6_context", summary$S6_context)
  doc <- replace_all(doc, "S6_level", summary$S6_level)

  # LINE 7 - Age Group Payroll STATIC (T7_*)
  doc <- replace_all(doc, "T7_age_25_34", summary$T7_age_25_34)
  doc <- replace_all(doc, "T7_age_50_64", summary$T7_age_50_64)
  doc <- replace_all(doc, "T7_age_18_24", summary$T7_age_18_24)

  # LINE 8 - Wages (U8_*)
  doc <- replace_all(doc, "U8_total_dir", summary$U8_total_dir)
  doc <- replace_all(doc, "U8_total_cur", summary$U8_total_cur)
  doc <- replace_all(doc, "U8_total_dq", summary$U8_total_dq)
  doc <- replace_all(doc, "U8_reg_dir", summary$U8_reg_dir)
  doc <- replace_all(doc, "U8_reg_cur", summary$U8_reg_cur)
  doc <- replace_all(doc, "U8_reg_dq", summary$U8_reg_dq)
  doc <- replace_all(doc, "U8_real_cur", summary$U8_real_cur)

  # LINE 9 - Pay Drivers (V9_*)
  doc <- replace_all(doc, "V9_public", summary$V9_public)
  doc <- replace_all(doc, "V9_private", summary$V9_private)
  doc <- replace_all(doc, "V9_explanation", summary$V9_explanation)

  # LINE 10 - Redundancies (W10_*)
  doc <- replace_all(doc, "W10_lfs_dir", summary$W10_lfs_dir)
  doc <- replace_all(doc, "W10_lfs_cur", summary$W10_lfs_cur)
  doc <- replace_all(doc, "W10_lfs_dq", summary$W10_lfs_dq)
  doc <- replace_all(doc, "W10_hr1_dir", summary$W10_hr1_dir)
  doc <- replace_all(doc, "W10_hr1_dm", summary$W10_hr1_dm)
  
  # CURRENT (no conditional except vacancies)
  doc <- replace_all(doc, "B1", fmt_count_000s_current(emp16_cur))
  doc <- replace_all(doc, "C1", .format_pct(emp_rt_cur))
  
  doc <- replace_all(doc, "D1", fmt_count_000s_current(unemp16_cur))
  doc <- replace_all(doc, "E1", .format_pct(unemp_rt_cur))
  
  doc <- replace_all(doc, "F1", fmt_count_000s_current(inact_cur))
  doc <- replace_all(doc, "G1", fmt_count_000s_current(inact5064_cur))
  doc <- replace_all(doc, "H1", .format_pct(inact_rt_cur))
  doc <- replace_all(doc, "I1", .format_pct(inact5064_rt_cur))
  
  doc <- replace_all(doc, "K1", fmt_exempt_current(payroll_cur))
  
  # Vacancies current is conditional tokens (neutral)
  doc <- fill_conditional(doc, "J1", fmt_exempt_current(vac_cur), 0, neutral = TRUE)
  
  doc <- replace_all(doc, "L1", .format_pct(latest_wages))
  doc <- replace_all(doc, "M1", .format_pct(latest_wages_cpi))
  
  # DQ/DY/DC conditional
  doc <- fill_conditional(doc, "B2", fmt_count_000s_change(emp16_dq), emp16_dq, invert = FALSE)
  doc <- fill_conditional(doc, "B3", fmt_count_000s_change(emp16_dy), emp16_dy, invert = FALSE)
  doc <- fill_conditional(doc, "B4", fmt_count_000s_change(emp16_dc), emp16_dc, invert = FALSE)
  
  doc <- fill_conditional(doc, "C2", .format_pp(emp_rt_dq), emp_rt_dq, invert = FALSE)
  doc <- fill_conditional(doc, "C3", .format_pp(emp_rt_dy), emp_rt_dy, invert = FALSE)
  doc <- fill_conditional(doc, "C4", .format_pp(emp_rt_dc), emp_rt_dc, invert = FALSE)
  
  doc <- fill_conditional(doc, "D2", fmt_count_000s_change(unemp16_dq), unemp16_dq, invert = TRUE)
  doc <- fill_conditional(doc, "D3", fmt_count_000s_change(unemp16_dy), unemp16_dy, invert = TRUE)
  doc <- fill_conditional(doc, "D4", fmt_count_000s_change(unemp16_dc), unemp16_dc, invert = TRUE)
  
  doc <- fill_conditional(doc, "E2", .format_pp(unemp_rt_dq), unemp_rt_dq, invert = TRUE)
  doc <- fill_conditional(doc, "E3", .format_pp(unemp_rt_dy), unemp_rt_dy, invert = TRUE)
  doc <- fill_conditional(doc, "E4", .format_pp(unemp_rt_dc), unemp_rt_dc, invert = TRUE)
  
  doc <- fill_conditional(doc, "F2", fmt_count_000s_change(inact_dq), inact_dq, invert = TRUE)
  doc <- fill_conditional(doc, "F3", fmt_count_000s_change(inact_dy), inact_dy, invert = TRUE)
  doc <- fill_conditional(doc, "F4", fmt_count_000s_change(inact_dc), inact_dc, invert = TRUE)
  
  doc <- fill_conditional(doc, "G2", fmt_count_000s_change(inact5064_dq), inact5064_dq, invert = TRUE)
  doc <- fill_conditional(doc, "G3", fmt_count_000s_change(inact5064_dy), inact5064_dy, invert = TRUE)
  doc <- fill_conditional(doc, "G4", fmt_count_000s_change(inact5064_dc), inact5064_dc, invert = TRUE)
  
  doc <- fill_conditional(doc, "H2", .format_pp(inact_rt_dq), inact_rt_dq, invert = TRUE)
  doc <- fill_conditional(doc, "H3", .format_pp(inact_rt_dy), inact_rt_dy, invert = TRUE)
  doc <- fill_conditional(doc, "H4", .format_pp(inact_rt_dc), inact_rt_dc, invert = TRUE)
  
  doc <- fill_conditional(doc, "I2", .format_pp(inact5064_rt_dq), inact5064_rt_dq, invert = TRUE)
  doc <- fill_conditional(doc, "I3", .format_pp(inact5064_rt_dy), inact5064_rt_dy, invert = TRUE)
  doc <- fill_conditional(doc, "I4", .format_pp(inact5064_rt_dc), inact5064_rt_dc, invert = TRUE)
  
  doc <- fill_conditional(doc, "K2", fmt_exempt_change(payroll_dq), payroll_dq, invert = FALSE)
  doc <- fill_conditional(doc, "K3", fmt_exempt_change(payroll_dy), payroll_dy, invert = FALSE)
  doc <- fill_conditional(doc, "K4", fmt_exempt_change(payroll_dc), payroll_dc, invert = FALSE)
  
  # Vacancies neutral across changes
  doc <- fill_conditional(doc, "J2", fmt_exempt_change(vac_dq), 0, neutral = TRUE)
  doc <- fill_conditional(doc, "J3", fmt_exempt_change(vac_dy), 0, neutral = TRUE)
  doc <- fill_conditional(doc, "J4", fmt_exempt_change(vac_dc), 0, neutral = TRUE)
  
  doc <- fill_conditional(doc, "L2", .format_gbp_signed0(wages_change_q), wages_change_q, invert = FALSE)
  doc <- fill_conditional(doc, "L3", .format_gbp_signed0(wages_change_y), wages_change_y, invert = FALSE)
  doc <- fill_conditional(doc, "L4", .format_gbp_signed0(wages_change_covid), wages_change_covid, invert = FALSE)
  
  doc <- fill_conditional(doc, "M2", .format_gbp_signed0(wages_cpi_change_q), wages_cpi_change_q, invert = FALSE)
  doc <- fill_conditional(doc, "M3", .format_gbp_signed0(wages_cpi_change_y), wages_cpi_change_y, invert = FALSE)
  doc <- fill_conditional(doc, "M4", .format_gbp_signed0(wages_cpi_change_covid), wages_cpi_change_covid, invert = FALSE)
  
  # Election column
  if (exists("emp16_de", inherits = TRUE)) {
    doc <- fill_conditional(doc, "B5", fmt_count_000s_change(emp16_de), emp16_de, invert = FALSE)
    doc <- fill_conditional(doc, "C5", .format_pp(emp_rt_de), emp_rt_de, invert = FALSE)
    
    doc <- fill_conditional(doc, "D5", fmt_count_000s_change(unemp16_de), unemp16_de, invert = TRUE)
    doc <- fill_conditional(doc, "E5", .format_pp(unemp_rt_de), unemp_rt_de, invert = TRUE)
    
    doc <- fill_conditional(doc, "F5", fmt_count_000s_change(inact_de), inact_de, invert = TRUE)
    doc <- fill_conditional(doc, "G5", fmt_count_000s_change(inact5064_de), inact5064_de, invert = TRUE)
    doc <- fill_conditional(doc, "H5", .format_pp(inact_rt_de), inact_rt_de, invert = TRUE)
    doc <- fill_conditional(doc, "I5", .format_pp(inact5064_rt_de), inact5064_rt_de, invert = TRUE)
    
    doc <- fill_conditional(doc, "K5", fmt_exempt_change(payroll_de), payroll_de, invert = FALSE)
    doc <- fill_conditional(doc, "J5", fmt_exempt_change(vac_de), 0, neutral = TRUE)
    
    if (exists("wages_change_election", inherits = TRUE)) {
      doc <- fill_conditional(doc, "L5", .format_gbp_signed0(wages_change_election), wages_change_election, invert = FALSE)
    }
    if (exists("wages_cpi_change_election", inherits = TRUE)) {
      doc <- fill_conditional(doc, "M5", .format_gbp_signed0(wages_cpi_change_election), wages_cpi_change_election, invert = FALSE)
    }
  }
  
  print(doc, target = output_path)
  invisible(output_path)
}

# ==============================================================================
# RUN - From project root directory
# ==============================================================================
 generate_word_output()