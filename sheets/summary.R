# ==============================================================================
# SUMMARY MODULE - Automated narrative for Summary section
# ==============================================================================

library(glue)

# ==============================================================================
# GENERATE SUMMARY - 10 lines matching user specification
# ==============================================================================

generate_summary <- function() {

  # ---------------------------------------------------------------------------
  # HELPER: Direction words
  # ---------------------------------------------------------------------------

  dir_word <- function(x, fall = "fall", rise = "rise") {
    if (is.na(x)) return("change")
    ifelse(x < 0, fall, rise)
  }

  dir_verb <- function(x, fell = "fell", rose = "rose") {
    if (is.na(x)) return("changed")
    ifelse(x < 0, fell, rose)
  }

  dir_ing <- function(x, falling = "falling", rising = "rising") {
    if (is.na(x)) return("changing")
    ifelse(x < 0, falling, rising)
  }

  # ---------------------------------------------------------------------------
  # LINE 1: Payroll (PAYE) - quarterly and annual
  # ---------------------------------------------------------------------------

  payroll_pct_dy <- if (!is.na(payroll_dy) && !is.na(payroll_cur) && payroll_cur != 0) {
    (payroll_dy / (payroll_cur + abs(payroll_dy))) * 100
  } else NA_real_

  payroll_pct_dq <- if (!is.na(payroll_dq) && !is.na(payroll_cur) && payroll_cur != 0) {
    (payroll_dq / (payroll_cur + abs(payroll_dq))) * 100
  } else NA_real_

  line1 <- glue(
    'The number of payrolled employees (PAYE) continued to {dir_word(payroll_dy)}, ',
    '{dir_ing(payroll_dy, "declining", "increasing")} by {fmt_int(abs(payroll_dy))} ',
    '({format(round(abs(payroll_pct_dy), 1), nsmall = 1)}%) on the year, and by ',
    '{fmt_int(abs(payroll_dq))} ({format(round(abs(payroll_pct_dq), 1), nsmall = 1)}%) ',
    'on the quarter in {lfs_period_label} (the period comparable with LFS).'
  )

  # ---------------------------------------------------------------------------
  # LINE 2: Flash estimate
  # ---------------------------------------------------------------------------

  flash_pct_dy <- if (!is.na(payroll_flash_dy) && !is.na(payroll_flash_cur) && payroll_flash_cur != 0) {
    (payroll_flash_dy / (payroll_flash_cur * 1000 + abs(payroll_flash_dy))) * 100
  } else NA_real_

  flash_pct_dm <- if (!is.na(payroll_flash_dm) && !is.na(payroll_flash_cur) && payroll_flash_cur != 0) {
    (payroll_flash_dm / (payroll_flash_cur * 1000 + abs(payroll_flash_dm))) * 100
  } else NA_real_

  flash_revision_note <- "although this is prone to revision (last month's 32k fall was revised down to 22k)"

  line2 <- glue(
    "The 'flash' estimate for {payroll_flash_label} suggests payroll employees ",
    '{dir_verb(payroll_flash_dy)} by {fmt_int(abs(payroll_flash_dy * 1000))} ',
    '({format(round(abs(flash_pct_dy), 1), nsmall = 1)}%) on the year, and ',
    '{fmt_int(abs(payroll_flash_dm * 1000))} ({format(round(abs(flash_pct_dm), 1), nsmall = 1)}%) ',
    'on the month, {flash_revision_note}.'
  )

  # ---------------------------------------------------------------------------
  # LINE 3: Workforce jobs (STATIC)
  # ---------------------------------------------------------------------------

  line3 <- "Workforce jobs data shows a fall of 116,000 jobs on the quarter (-0.3%), mostly driven by a fall in self-employment jobs. On the year, jobs fell by 115,000 (-0.3%)."

  # ---------------------------------------------------------------------------
  # LINE 4: Vacancies
  # ---------------------------------------------------------------------------

  vac_pct_dq <- if (!is.na(vac_dq) && !is.na(vac_cur) && vac_cur != 0) {
    (vac_dq / vac_cur) * 100
  } else NA_real_

  vac_period_label <- lfs_period_label

  line4 <- glue(
    'Vacancies remain broadly unchanged, {dir_ing(vac_dq, "falling", "rising")} ',
    'slightly by {fmt_int(abs(vac_dq))} ({format(round(abs(vac_pct_dq), 1), nsmall = 1)}%) ',
    'on the quarter to {fmt_int(vac_cur)} in {vac_period_label}.'
  )

  # ---------------------------------------------------------------------------
  # LINE 5: LFS Summary
  # ---------------------------------------------------------------------------

  line5 <- glue(
    'Labour Force Survey (LFS) suggests that in {lfs_period_label}, the employment rate ',
    '{dir_verb(emp_rt_dq)} to {format(round(emp_rt_cur, 1), nsmall = 1)}%, ',
    '{ifelse(emp_rt_dq >= 0, "+", "")}{format(round(emp_rt_dq, 1), nsmall = 1)} percentage points ',
    'compared to the previous quarter. On the quarter, unemployment slightly ',
    '{dir_verb(unemp_rt_dq)} to {format(round(unemp_rt_cur, 1), nsmall = 1)}% ',
    '({ifelse(unemp_rt_dq >= 0, "+", "")}{format(round(unemp_rt_dq, 1), nsmall = 1)} percentage points) ',
    'and inactivity {dir_verb(inact_rt_dq)} marginally to {format(round(inact_rt_cur, 1), nsmall = 1)}% ',
    '({ifelse(inact_rt_dq >= 0, "+", "")}{format(round(inact_rt_dq, 1), nsmall = 1)} percentage points).'
  )

  # ---------------------------------------------------------------------------
  # LINE 6: Youth Unemployment (STATIC)
  # ---------------------------------------------------------------------------

  line6 <- "Youth unemployment saw a substantial rise, with the number of unemployed 18-24 year olds rising by 85,000 on the quarter, the largest increase since November 2022, as the number of 18- to 24-year-olds out of work, at 546,000, rose to its highest level since 2015."

  # ---------------------------------------------------------------------------
  # LINE 7: Age Group Payroll (STATIC)
  # ---------------------------------------------------------------------------

  line7 <- "In contrast, the monthly drop in payrolled employees was largest for 25-34 year olds (-132k), followed by 50-64 year olds (-79k), and 18-24 year olds (-11k)."

  # ---------------------------------------------------------------------------
  # LINE 8: Wages
  # ---------------------------------------------------------------------------

  line8 <- glue(
    'Annual wage growth in average weekly earnings (inc. bonuses) {dir_verb(wages_total_qchange)} to ',
    '{format(round(latest_wages, 1), nsmall = 1)}% in {lfs_period_label} ',
    '({ifelse(wages_total_qchange >= 0, "+", "")}{format(round(wages_total_qchange, 1), nsmall = 1)} ',
    'percentage points from the previous 3-month period). Wage growth excl. bonuses also ',
    '{dir_verb(wages_reg_qchange)} to {format(round(latest_regular_cash, 1), nsmall = 1)}% ',
    '({ifelse(wages_reg_qchange >= 0, "+", "")}{format(round(wages_reg_qchange, 1), nsmall = 1)} ',
    'percentage points). Real wage growth (inc. bonuses) ',
    '{ifelse(latest_wages_cpi < wages_cpi_change_q, "dropped", "rose")} to ',
    '{format(round(latest_wages_cpi, 1), nsmall = 1)}%.'
  )

  # ---------------------------------------------------------------------------
  # LINE 9: Pay Growth Drivers
  # ---------------------------------------------------------------------------

  pay_driver_explanation <- "this is largely statistical. Pay agreements in 2025 were paid earlier in the year than 2024, which meant 2024 pay was much lower, causing the annual growth rate to spike (base effects)"

  line9 <- glue(
    'Pay growth was mostly driven by public sector ({format(round(wages_total_public, 1), nsmall = 1)}% ',
    'on the quarter), compared to {format(round(wages_total_private, 1), nsmall = 1)}% in the private ',
    'sector - {pay_driver_explanation}.'
  )

  # ---------------------------------------------------------------------------
  # LINE 10: Redundancies
  # ---------------------------------------------------------------------------

  line10 <- glue(
    'LFS redundancies {dir_verb(redund_dq)} on the quarter to {fmt_int(redund_cur)}k in {lfs_period_label} ',
    '({ifelse(redund_dq >= 0, "+", "")}{fmt_int(redund_dq)}k from the previous quarter). ',
    'HR1 redundancies (notifications of redundancies) also {dir_verb(hr1_dm)} significantly, ',
    '{ifelse(hr1_dm >= 0, "increasing", "decreasing")} by {fmt_int(abs(hr1_dm))} on the month.'
  )

  # ---------------------------------------------------------------------------
  # RETURN: Full lines + individual placeholder values
  # ---------------------------------------------------------------------------

  list(
    # Full generated lines
    line1 = line1,
    line2 = line2,
    line3 = line3,
    line4 = line4,
    line5 = line5,
    line6 = line6,
    line7 = line7,
    line8 = line8,
    line9 = line9,
    line10 = line10,

    # LINE 1 placeholders (Payroll)
    N1_dir = dir_word(payroll_dy),
    N1_dir_ing = dir_ing(payroll_dy, "declining", "increasing"),
    N1_dy_abs = fmt_int(abs(payroll_dy)),
    N1_dy_pct = format(round(abs(payroll_pct_dy), 1), nsmall = 1),
    N1_dq_abs = fmt_int(abs(payroll_dq)),
    N1_dq_pct = format(round(abs(payroll_pct_dq), 1), nsmall = 1),

    # LINE 2 placeholders (Flash)
    O2_label = payroll_flash_label,
    O2_dir = dir_verb(payroll_flash_dy),
    O2_dy_abs = fmt_int(abs(payroll_flash_dy * 1000)),
    O2_dy_pct = format(round(abs(flash_pct_dy), 1), nsmall = 1),
    O2_dm_abs = fmt_int(abs(payroll_flash_dm * 1000)),
    O2_dm_pct = format(round(abs(flash_pct_dm), 1), nsmall = 1),
    O2_revision = flash_revision_note,

    # LINE 3 placeholders (Workforce - STATIC)
    P3_dq_abs = "116,000",
    P3_dq_pct = "0.3",
    P3_driver = "a fall in self-employment jobs",
    P3_dy_abs = "115,000",
    P3_dy_pct = "0.3",

    # LINE 4 placeholders (Vacancies)
    Q4_dir_ing = dir_ing(vac_dq, "falling", "rising"),
    Q4_dq_abs = fmt_int(abs(vac_dq)),
    Q4_dq_pct = format(round(abs(vac_pct_dq), 1), nsmall = 1),
    Q4_cur = fmt_int(vac_cur),

    # LINE 5 placeholders (LFS Rates)
    R5_emp_dir = dir_verb(emp_rt_dq),
    R5_emp_cur = format(round(emp_rt_cur, 1), nsmall = 1),
    R5_emp_dq = paste0(ifelse(emp_rt_dq >= 0, "+", ""), format(round(emp_rt_dq, 1), nsmall = 1)),
    R5_unemp_dir = dir_verb(unemp_rt_dq),
    R5_unemp_cur = format(round(unemp_rt_cur, 1), nsmall = 1),
    R5_unemp_dq = paste0(ifelse(unemp_rt_dq >= 0, "+", ""), format(round(unemp_rt_dq, 1), nsmall = 1)),
    R5_inact_dir = dir_verb(inact_rt_dq),
    R5_inact_cur = format(round(inact_rt_cur, 1), nsmall = 1),
    R5_inact_dq = paste0(ifelse(inact_rt_dq >= 0, "+", ""), format(round(inact_rt_dq, 1), nsmall = 1)),

    # LINE 6 placeholders (Youth - STATIC)
    S6_dq = "85,000",
    S6_context = "the largest increase since November 2022",
    S6_level = "546,000",

    # LINE 7 placeholders (Age Group - STATIC)
    T7_age_25_34 = "-132k",
    T7_age_50_64 = "-79k",
    T7_age_18_24 = "-11k",

    # LINE 8 placeholders (Wages)
    U8_total_dir = dir_verb(wages_total_qchange),
    U8_total_cur = format(round(latest_wages, 1), nsmall = 1),
    U8_total_dq = paste0(ifelse(wages_total_qchange >= 0, "+", ""), format(round(wages_total_qchange, 1), nsmall = 1)),
    U8_reg_dir = dir_verb(wages_reg_qchange),
    U8_reg_cur = format(round(latest_regular_cash, 1), nsmall = 1),
    U8_reg_dq = paste0(ifelse(wages_reg_qchange >= 0, "+", ""), format(round(wages_reg_qchange, 1), nsmall = 1)),
    U8_real_cur = format(round(latest_wages_cpi, 1), nsmall = 1),

    # LINE 9 placeholders (Pay Drivers)
    V9_public = format(round(wages_total_public, 1), nsmall = 1),
    V9_private = format(round(wages_total_private, 1), nsmall = 1),
    V9_explanation = pay_driver_explanation,

    # LINE 10 placeholders (Redundancies)
    W10_lfs_dir = dir_verb(redund_dq),
    W10_lfs_cur = fmt_int(redund_cur),
    W10_lfs_dq = paste0(ifelse(redund_dq >= 0, "+", ""), fmt_int(redund_dq)),
    W10_hr1_dir = dir_verb(hr1_dm),
    W10_hr1_dm = fmt_int(abs(hr1_dm))
  )
}

# ==============================================================================
# PRINT SUMMARY
# ==============================================================================

print_summary <- function(summary) {
  cat('\n')
  cat('1.', summary$line1, '\n\n')
  cat('2.', summary$line2, '\n\n')
  cat('3.', summary$line3, '\n\n')
  cat('4.', summary$line4, '\n\n')
  cat('5.', summary$line5, '\n\n')
  cat('6.', summary$line6, '\n\n')
  cat('7.', summary$line7, '\n\n')
  cat('8.', summary$line8, '\n\n')
  cat('9.', summary$line9, '\n\n')
  cat('10.', summary$line10, '\n\n')
}
