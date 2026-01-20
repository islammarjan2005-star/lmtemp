
library(shiny)
library(shinythemes)

# =========================
# UI
# =========================

ui <- tagList(
  tags$head(
    tags$style(HTML("
      body { background-color: #f6f8fb; }
      .navbar, .navbar-default { background-color: #0b4f6c !important; border-color: #0b4f6c !important; }
      .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {
        color: #ffffff !important;
      }
      .btn-primary, .btn-default {
        border-radius: 20px;
        padding: 8px 18px;
        font-weight: 600;
      }
      .btn-primary {
        background-color: #0b4f6c;
        border-color: #0b4f6c;
      }
      .btn-primary:hover {
        background-color: #096089;
        border-color: #096089;
      }
      .status-box {
        background: #ffffff;
        border-radius: 8px;
        padding: 12px 16px;
        border-left: 4px solid #0b4f6c;
        font-family: monospace;
      }
      .panel-card {
        background: #ffffff;
        border-radius: 8px;
        padding: 12px 16px;
        border: 1px solid #e0e4ec;
        margin-bottom: 15px;
      }
    "))
  ),
  
  navbarPage(
    title = "Labour Market Dashboard",
    theme = shinytheme("flatly"),
    
    tabPanel(
      title = "Run & Download",
      sidebarLayout(
        sidebarPanel(
          h3("Parameters"),
          helpText("Set the main reference month for the briefing (e.g. nov2025)."),
          
          textInput("manual_month",
                    "manual_month:",
                    value = ""),
          
          hr(),
          h3("Downloads"),
          helpText("Generate fresh Word and Excel outputs using the latest data."),
          
          downloadButton("download_word",  label = "Download Word briefing (.docx)"),
          br(), br(),
          downloadButton("download_excel", label = "Download Excel audit (.xlsx)"),
          
          width = 3
        ),
        mainPanel(
          div(class = "panel-card",
              h3("Status"),
              verbatimTextOutput("status")
          ),
          div(class = "panel-card",
              h3("Tips"),
              p("1. Set the reference month, then hit the download buttons.",
                br(),
                "2. Use the Preview tab to sanity-check the summary, top 10, and dashboard numbers before sending upstream.")
          ),
          width = 9
        )
      )
    ),
    
    tabPanel(
      title = "Preview",
      fluidPage(
        fluidRow(
          column(
            width = 6,
            div(class = "panel-card",
                h3("Summary & Top 10"),
                p("Click the button below to refresh the narrative and top ten stats."),
                actionButton("refresh_preview", "Refresh summary / top 10"),
                br(), br(),
                uiOutput("summary_preview"),
                br(),
                h4("Top 10 stats"),
                tableOutput("topten_preview")
            )
          ),
          column(
            width = 6,
            div(class = "panel-card",
                h3("Dashboard preview"),
                p("Click the button below to refresh dashboard metrics (current, DQ, DY, DC, Election)."),
                actionButton("refresh_dashboard", "Refresh dashboard preview"),
                br(), br(),
                tableOutput("dashboard_preview")
            )
          )
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  # Paths relative to project root
  config_path       <- "utils/config.R"
  calculations_path <- "utils/calculations.R"
  word_script_path  <- "utils/word_output.R"
  excel_script_path <- "sheets/excel_audit.R"
  summary_path      <- "sheets/summary.R"
  top_ten_path      <- "sheets/top_ten_stats.R"
  template_path     <- "utils/DB.docx"
  
  status_text <- reactiveVal("Ready.")
  output$status <- renderText(status_text())
  
  # ---- Set default manual_month from config.R on startup ----
  observe({
    if (file.exists(config_path)) {
      cfg <- new.env()
      sys.source(config_path, envir = cfg)
      if (!is.null(cfg$manual_month)) {
        updateTextInput(session, "manual_month", value = cfg$manual_month)
      }
    }
  })
  
  # =========================
  # DOWNLOAD: WORD
  # =========================
  output$download_word <- downloadHandler(
    filename = function() {
      paste0("Labour_Market_Briefing_", Sys.Date(), ".docx")
    },
    content = function(file) {
      
      withProgress(message = "Generating Word briefing...", value = 0, {
        status_text("Generating Word briefing...")
        
        incProgress(0.2, detail = "Loading word_output.R")
        if (file.exists(word_script_path)) {
          sys.source(word_script_path, envir = .GlobalEnv)
        } else {
          stop("Cannot find ", word_script_path)
        }
        
        if (!exists("generate_word_output", envir = .GlobalEnv)) {
          stop("generate_word_output() not found after sourcing word_output.R")
        }
        
        incProgress(0.6, detail = "Running generator")
        
        generate_word_output(
          template_path             = template_path,
          output_path               = file,
          calculations_path         = calculations_path,
          config_path               = config_path,
          summary_path              = summary_path,
          top_ten_path              = top_ten_path,
          manual_month_override     = if (nzchar(input$manual_month)) input$manual_month else NULL,
          manual_month_hr1_override = NULL,
          verbose                   = TRUE
        )
        
        incProgress(1, detail = "Done")
        
        status_text("Word briefing generated and downloaded.")
      })
    }
  )
  
  # =========================
  # DOWNLOAD: EXCEL
  # =========================
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("LM_Stats_Audit_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      
      withProgress(message = "Generating Excel audit workbook...", value = 0, {
        status_text("Generating Excel audit workbook...")
        
        incProgress(0.2, detail = "Loading excel_audit.R")
        if (file.exists(excel_script_path)) {
          sys.source(excel_script_path, envir = .GlobalEnv)
        } else {
          stop("Cannot find ", excel_script_path)
        }
        
        if (!exists("create_audit_workbook", envir = .GlobalEnv)) {
          stop("create_audit_workbook() not found after sourcing excel_audit.R")
        }
        
        incProgress(0.6, detail = "Running generator")
        
        create_audit_workbook(
          output_path       = file,
          calculations_path = calculations_path,
          config_path       = config_path,
          verbose           = TRUE
        )
        
        incProgress(1, detail = "Done")
        
        status_text("Excel audit workbook generated and downloaded.")
      })
    }
  )
  
  # =========================
  # PREVIEW: SUMMARY + TOP 10
  # =========================
  
  preview_data <- reactiveVal(NULL)
  
  observeEvent(input$refresh_preview, {
    withProgress(message = "Refreshing summary / top 10...", value = 0, {
      status_text("Refreshing summary / top 10...")
      
      env <- new.env()
      
      incProgress(0.2, detail = "Loading config & calculations")
      if (file.exists(config_path))       sys.source(config_path,       envir = env)
      
      # Override manual_month from UI if provided
      if (nzchar(input$manual_month)) {
        env$manual_month <- tolower(input$manual_month)
      }
      
      if (file.exists(calculations_path)) sys.source(calculations_path, envir = env)
      
      incProgress(0.5, detail = "Loading summary & top ten")
      if (file.exists(summary_path)) sys.source(summary_path, envir = env)
      if (file.exists(top_ten_path)) sys.source(top_ten_path, envir = env)
      
      if (!exists("generate_summary", envir = env) ||
          !exists("generate_top_ten", envir = env)) {
        stop("generate_summary() or generate_top_ten() not found.")
      }
      
      incProgress(0.8, detail = "Computing preview text")
      summary <- env$generate_summary()
      top10   <- env$generate_top_ten()
      
      preview_data(list(summary = summary, top10 = top10))
      
      incProgress(1, detail = "Done")
      status_text("Summary / top 10 preview refreshed.")
    })
  })
  
  output$summary_preview <- renderUI({
    pd <- preview_data()
    if (is.null(pd)) {
      return(em("Click 'Refresh summary / top 10' to see the latest text."))
    }
    
    tags$div(
      h4("Summary lines"),
      tags$ol(
        lapply(1:6, function(i) {
          line <- pd$summary[[paste0("line", i)]]
          if (is.null(line)) line <- ""
          tags$li(line)
        })
      )
    )
  })
  
  output$topten_preview <- renderTable({
    pd <- preview_data()
    if (is.null(pd)) return(NULL)
    
    data.frame(
      Rank = 1:10,
      Text = vapply(1:10, function(i) {
        line <- pd$top10[[paste0("line", i)]]
        if (is.null(line)) "" else line
      }, FUN.VALUE = character(1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # =========================
  # DASHBOARD PREVIEW (based on word_output variables)
  # =========================
  
  dashboard_data <- reactiveVal(NULL)
  
  observeEvent(input$refresh_dashboard, {
    withProgress(message = "Refreshing dashboard preview...", value = 0, {
      status_text("Refreshing dashboard preview...")
      
      env <- new.env()
      
      incProgress(0.3, detail = "Loading config & calculations")
      if (file.exists(config_path))       sys.source(config_path,       envir = env)
      
      # Override manual_month from UI if provided
      if (nzchar(input$manual_month)) {
        env$manual_month <- tolower(input$manual_month)
      }
      
      if (file.exists(calculations_path)) sys.source(calculations_path, envir = env)
      
      # helper to get value or NA
      gv <- function(name) {
        if (exists(name, envir = env)) get(name, envir = env) else NA_real_
      }
      
      # build a table mirroring the Word dashboard cells conceptually
      rows <- list(
        "Emp 16+ (000s)"          = c(gv("emp16_cur"),          gv("emp16_dq"),          gv("emp16_dy"),          gv("emp16_dc"),          gv("emp16_de")),
        "Emp rate (%)"           = c(gv("emp_rt_cur"),        gv("emp_rt_dq"),        gv("emp_rt_dy"),        gv("emp_rt_dc"),        gv("emp_rt_de")),
        "Unemp 16+ (000s)"       = c(gv("unemp16_cur"),       gv("unemp16_dq"),       gv("unemp16_dy"),       gv("unemp16_dc"),       gv("unemp16_de")),
        "Unemp rate (%)"         = c(gv("unemp_rt_cur"),      gv("unemp_rt_dq"),      gv("unemp_rt_dy"),      gv("unemp_rt_dc"),      gv("unemp_rt_de")),
        "Inact 16-64 (000s)"     = c(gv("inact_cur"),         gv("inact_dq"),         gv("inact_dy"),         gv("inact_dc"),         gv("inact_de")),
        "Inact 50-64 (000s)"     = c(gv("inact5064_cur"),     gv("inact5064_dq"),     gv("inact5064_dy"),     gv("inact5064_dc"),     gv("inact5064_de")),
        "Inact rate 16-64 (%)"   = c(gv("inact_rt_cur"),      gv("inact_rt_dq"),      gv("inact_rt_dy"),      gv("inact_rt_dc"),      gv("inact_rt_de")),
        "Inact rate 50-64 (%)"   = c(gv("inact5064_rt_cur"),  gv("inact5064_rt_dq"),  gv("inact5064_rt_dy"),  gv("inact5064_rt_dc"),  gv("inact5064_rt_de")),
        "Vacancies (000s)"       = c(gv("vac_cur"),           gv("vac_dq"),           gv("vac_dy"),           gv("vac_dc"),           gv("vac_de")),
        "Payroll (000s)"         = c(gv("payroll_cur"),       gv("payroll_dq"),       gv("payroll_dy"),       gv("payroll_dc"),       gv("payroll_de")),
        "Nominal AWE (total)"    = c(gv("latest_wages"),      gv("wages_change_q"),   gv("wages_change_y"),   gv("wages_change_covid"),   gv("wages_change_election")),
        "Real AWE (CPI)"         = c(gv("latest_wages_cpi"),  gv("wages_cpi_change_q"), gv("wages_cpi_change_y"), gv("wages_cpi_change_covid"), gv("wages_cpi_change_election"))
      )
      
      df <- data.frame(
        Metric   = names(rows),
        Current  = vapply(rows, function(x) x[1], numeric(1)),
        DQ       = vapply(rows, function(x) x[2], numeric(1)),
        DY       = vapply(rows, function(x) x[3], numeric(1)),
        DC       = vapply(rows, function(x) x[4], numeric(1)),
        Election = vapply(rows, function(x) x[5], numeric(1)),
        stringsAsFactors = FALSE
      )
      
      dashboard_data(df)
      
      incProgress(1, detail = "Done")
      status_text("Dashboard preview refreshed.")
    })
  })
  
  output$dashboard_preview <- renderTable({
    df <- dashboard_data()
    if (is.null(df)) {
      return(data.frame(Message = "Click 'Refresh dashboard preview' to load dashboard numbers."))
    }
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui, server)
