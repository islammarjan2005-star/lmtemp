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
  # Data: payroll_dy, payroll_dq, payroll_cur (all in thousands)

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
  # Data: payroll_flash_dy, payroll_flash_dm (in thousands), payroll_flash_label
  # Static: revision note (manual context)

  flash_pct_dy <- if (!is.na(payroll_flash_dy) && !is.na(payroll_flash_cur) && payroll_flash_cur != 0) {
    (payroll_flash_dy / (payroll_flash_cur * 1000 + abs(payroll_flash_dy))) * 100
  } else NA_real_

  flash_pct_dm <- if (!is.na(payroll_flash_dm) && !is.na(payroll_flash_cur) && payroll_flash_cur != 0) {
    (payroll_flash_dm / (payroll_flash_cur * 1000 + abs(payroll_flash_dm))) * 100
  } else NA_real_

  # Static revision note - update manually each release
  flash_revision_note <- "although this is prone to revision (last month's 32k fall was revised down to 22k)"

  line2 <- glue(
    "The 'flash' estimate for {payroll_flash_label} suggests payroll employees ",
    '{dir_verb(payroll_flash_dy)} by {fmt_int(abs(payroll_flash_dy * 1000))} ',
    '({format(round(abs(flash_pct_dy), 1), nsmall = 1)}%) on the year, and ',
    '{fmt_int(abs(payroll_flash_dm * 1000))} ({format(round(abs(flash_pct_dm), 1), nsmall = 1)}%) ',
    'on the month, {flash_revision_note}.'
  )

  # ---------------------------------------------------------------------------
  # LINE 3: Workforce jobs (STATIC - no data source available)
  # ---------------------------------------------------------------------------
  # Update manually each release

  line3 <- "Workforce jobs data shows a fall of 116,000 jobs on the quarter (-0.3%), mostly driven by a fall in self-employment jobs. On the year, jobs fell by 115,000 (-0.3%)."

  # ---------------------------------------------------------------------------
  # LINE 4: Vacancies
  # ---------------------------------------------------------------------------
  # Data: vac_dq (quarterly change in thousands), vac_cur (level in thousands)
  # vac_dq is already the quarterly change value

  vac_pct_dq <- if (!is.na(vac_dq) && !is.na(vac_cur) && vac_cur != 0) {
    (vac_dq / vac_cur) * 100
  } else NA_real_

  # Build vacancies period label from lfs period
  vac_period_label <- lfs_period_label

  line4 <- glue(
    'Vacancies remain broadly unchanged, {dir_ing(vac_dq, "falling", "rising")} ',
    'slightly by {fmt_int(abs(vac_dq))} ({format(round(abs(vac_pct_dq), 1), nsmall = 1)}%) ',
    'on the quarter to {fmt_int(vac_cur)} in {vac_period_label}.'
  )

  # ---------------------------------------------------------------------------
  # LINE 5: LFS Summary (Employment, Unemployment, Inactivity rates)
  # ---------------------------------------------------------------------------
  # Data: emp_rt_cur, emp_rt_dq, unemp_rt_cur, unemp_rt_dq, inact_rt_cur, inact_rt_dq

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
  # LINE 6: Youth Unemployment (STATIC - no age-specific data available)
  # ---------------------------------------------------------------------------
  # Update manually each release

  line6 <- "Youth unemployment saw a substantial rise, with the number of unemployed 18-24 year olds rising by 85,000 on the quarter, the largest increase since November 2022, as the number of 18- to 24-year-olds out of work, at 546,000, rose to its highest level since 2015."

  # ---------------------------------------------------------------------------
  # LINE 7: Age Group Payroll (STATIC - no age-specific payroll data available)
  # ---------------------------------------------------------------------------
  # Update manually each release

  line7 <- "In contrast, the monthly drop in payrolled employees was largest for 25-34 year olds (-132k), followed by 50-64 year olds (-79k), and 18-24 year olds (-11k)."

  # ---------------------------------------------------------------------------
  # LINE 8: Wages (Total, Regular, Real)
  # ---------------------------------------------------------------------------
  # Data: latest_wages, wages_total_qchange, latest_regular_cash, wages_reg_qchange, latest_wages_cpi

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
  # LINE 9: Pay Growth Drivers (Public vs Private)
  # ---------------------------------------------------------------------------
  # Data: wages_total_public, wages_total_private
  # Static: explanation of base effects (manual context)

  pay_driver_explanation <- "this is largely statistical. Pay agreements in 2025 were paid earlier in the year than 2024, which meant 2024 pay was much lower, causing the annual growth rate to spike (base effects)"

  line9 <- glue(
    'Pay growth was mostly driven by public sector ({format(round(wages_total_public, 1), nsmall = 1)}% ',
    'on the quarter), compared to {format(round(wages_total_private, 1), nsmall = 1)}% in the private ',
    'sector - {pay_driver_explanation}.'
  )

  # ---------------------------------------------------------------------------
  # LINE 10: Redundancies (LFS + HR1)
  # ---------------------------------------------------------------------------
  # Data: redund_cur (rate per 1000), redund_dq, hr1_dm, lfs_period_label
  # Note: redund_cur is in thousands already from BEIR code

  line10 <- glue(
    'LFS redundancies {dir_verb(redund_dq)} on the quarter to {fmt_int(redund_cur)}k in {lfs_period_label} ',
    '({ifelse(redund_dq >= 0, "+", "")}{fmt_int(redund_dq)}k from the previous quarter). ',
    'HR1 redundancies (notifications of redundancies) also {dir_verb(hr1_dm)} significantly, ',
    '{ifelse(hr1_dm >= 0, "increasing", "decreasing")} by {fmt_int(abs(hr1_dm))} on the month.'
  )

  list(
    line1 = line1,
    line2 = line2,
    line3 = line3,
    line4 = line4,
    line5 = line5,
    line6 = line6,
    line7 = line7,
    line8 = line8,
    line9 = line9,
    line10 = line10
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

# ==============================================================================
# WORD DOCUMENT PLACEHOLDERS
# ==============================================================================
# Placeholder codes for Word template (matches word_output.R style)
# Uses letters N-W (unused in dashboard) for summary lines 1-10
# Conditional placeholders use _p/_n/_z suffixes for positive/negative/zero
#
# LINE 1 (Payroll): N prefix
#   N1 = direction word ("fall"/"rise")
#   N2 = direction -ing ("declining"/"increasing")
#   N3 = annual change absolute (e.g. "113,000")
#   N4 = annual change % (e.g. "0.4")
#   N5 = quarterly change absolute (e.g. "24,000")
#   N6 = quarterly change % (e.g. "0.1")
#
# LINE 2 (Flash): O prefix
#   O1 = flash month label (e.g. "November 2025")
#   O2 = direction ("fell"/"rose")
#   O3 = annual change absolute (e.g. "171,000")
#   O4 = annual change % (e.g. "0.6")
#   O5 = monthly change absolute (e.g. "38,000")
#   O6 = monthly change % (e.g. "0.1")
#   O7 = revision note (static, manual update)
#
# LINE 3 (Workforce Jobs - STATIC): P prefix
#   P1 = quarterly change absolute (e.g. "116,000")
#   P2 = quarterly change % (e.g. "0.3")
#   P3 = driver text (e.g. "a fall in self-employment jobs")
#   P4 = annual change absolute (e.g. "115,000")
#   P5 = annual change % (e.g. "0.3")
#
# LINE 4 (Vacancies): Q prefix
#   Q1 = direction -ing ("falling"/"rising")
#   Q2 = quarterly change absolute (e.g. "2,000")
#   Q3 = quarterly change % (e.g. "0.2")
#   Q4 = current level (e.g. "729,000")
#
# LINE 5 (LFS Rates): R prefix
#   R1 = emp rate direction ("fell"/"rose")
#   R2 = emp rate current (e.g. "74.9")
#   R3 = emp rate change with sign (e.g. "-0.3")
#   R4 = unemp rate direction ("fell"/"rose")
#   R5 = unemp rate current (e.g. "5.1")
#   R6 = unemp rate change with sign (e.g. "+0.4")
#   R7 = inact rate direction ("fell"/"rose")
#   R8 = inact rate current (e.g. "21.0")
#   R9 = inact rate change with sign (e.g. "-0.1")
#
# LINE 6 (Youth Unemployment - STATIC): S prefix
#   S1 = youth unemp quarterly change (e.g. "85,000")
#   S2 = historical context (e.g. "the largest increase since November 2022")
#   S3 = youth unemp level (e.g. "546,000")
#
# LINE 7 (Age Group Payroll - STATIC): T prefix
#   T1 = 25-34 monthly change (e.g. "-132k")
#   T2 = 50-64 monthly change (e.g. "-79k")
#   T3 = 18-24 monthly change (e.g. "-11k")
#
# LINE 8 (Wages): U prefix
#   U1 = total wages direction ("fell"/"rose")
#   U2 = total wages current % (e.g. "4.7")
#   U3 = total wages change with sign (e.g. "-0.1")
#   U4 = regular wages direction ("fell"/"rose")
#   U5 = regular wages current % (e.g. "4.6")
#   U6 = regular wages change with sign (e.g. "-0.2")
#   U7 = real wages current % (e.g. "1.0")
#
# LINE 9 (Pay Drivers): V prefix
#   V1 = public sector wage % (e.g. "7.7")
#   V2 = private sector wage % (e.g. "4.7")
#   V3 = explanation (static, manual update)
#
# LINE 10 (Redundancies): W prefix
#   W1 = LFS redund direction ("rose"/"fell")
#   W2 = LFS redund level in k (e.g. "156")
#   W3 = LFS redund change with sign (e.g. "+52")
#   W4 = HR1 direction ("rose"/"fell")
#   W5 = HR1 monthly change (e.g. "5,503")
#
# SHARED:
#   LFS_PERIOD_LABEL = period label (e.g. "August to October 2025")
#
# ==============================================================================
