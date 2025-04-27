# Global or Top of ETL Outbreak Analytics App.R
#install.packages("purrr")
#install.packages("tigris")
#install.packages("leaflet")
#install.packages("sf")
#install.packages(c("webshot2", "htmlwidgets"))


library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(ggplot2)
library(incidence2)
library(epitrix)
library(plotly)
library(EpiEstim)
library(projections)
library(incidence)
library(distcrete)
library(forcats)
library(ggthemes)
library(purrr)      # for map_dfr
library(tibble)     # for tidy tibbles
library(sf)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)      # for replace_na()
library(viridis)    # optional for better color palettes
library(shinydashboard)
library(stringr)
library(tigris)
library(sf)
library(leaflet)
library(webshot2)
library(htmlwidgets)


options(tigris_use_cache = TRUE)

# Safe intersects helper
safe_intersects <- function(a, b) {
  out <- vector("list", length(a))
  for (i in seq_along(a)) {
    out[[i]] <- tryCatch(
      as.integer(st_intersects(a[i], b)),
      error = function(e) integer(0)
    )
  }
  out
}



zcta_ne   <- readRDS("zcta_ne.rds")
county_ne <- readRDS("county_ne.rds") %>%
  mutate(group = str_to_title(NAME))      # <- add this line



# Define the list of required packages
#required_packages <- c(
#  "shiny",
#  "readxl",
#  "dplyr",
#  "DT",
#  "lubridate",
#  "ggplot2",
#  "incidence2",
#  "epitrix",
#  "plotly",
#  "EpiEstim",
#  "projections",
#  "incidence",
#  "distcrete",
#  "forcats",
#  "ggthemes"  # ggthemes provides additional themes and palettes
#)

# Check for packages that are not installed, and install them
#new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
#if(length(new_packages)) {
#  install.packages(new_packages)
#}

# Load all the required packages
#invisible(lapply(required_packages, library, character.only = TRUE))


key_vars <- c("None" ,"vacc_status", "hospitalized", "working_status","final_status",
              "sex_at_birth","age_group","race","hispanic_latino",
              "city","county","state_form","residence_country",
              "outbreak_related","recent_travel",
              "import_status","us_acquired_detail", "zip")

filter_dataset <- function(df, sel) {
  if (length(sel) == 0) return(df)
  # sel is a named list like list(variable = c(values,…))
  purrr::reduce(names(sel), function(d, v) d[d[[v]] %in% sel[[v]], ], .init = df)
}


ui <- fluidPage(
  titlePanel("Measles Outbreak Management & Analytics ETL Web App (Alpha v2.0)"),
  tags$head(
  tags$style(HTML("
  .leaflet-control.map-title {
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    padding: 6px 12px;
    font-size: 16px;
    color: #333;
  }

 "))
 ),


  
  # Then your Filters Row
  fluidRow(
   # column(3,
         #  selectInput("selected_state", "Select State:", choices = state.abb, selected = "NE")
   # ),
    column(3,
           selectInput("filter_vars", "Filter by Variables:", choices = key_vars, multiple = TRUE, selected = NULL)
    ),
    column(3,
           uiOutput("filter_value_inputs")
    ),

  ),
  
  uiOutput("active_filters"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("crf_file", "Upload Measles Case Excel File", 
                accept = c(".xlsx")),
      helpText("Accepted format: Excel file exported from CRF app (.xlsx)"),

      # Fixed scenarios as strings (we'll convert to numeric later)
     # checkboxGroupInput("fixed_scenarios", "Select Fixed Rt Scenarios:",
                      #   choices = c("1.3", "1.5", "1.6"),
                      #   selected = c("1.3", "1.5", "1.6")),
      
      # Inputs for a custom scenario:
      #textInput("custom_label", "Custom Scenario Label", value = "My Custom Scenario"),
      #numericInput("custom_Rt", "Custom Rt Value", value = NA, min = 0, step = 0.1),
      #actionButton("add_custom", "Add Custom Scenario")
      
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",

                  # ─── TAB 1 ──────────────────────────────────────────────
                  tabPanel("Cleaned Case Records",
                           br(),
                           DTOutput("cleaned_table")          # <-- already built in server
                  ),
                  
                  # ─── TAB 2 ──────────────────────────────────────────────
                  tabPanel("Summary Frequencies",
                           br(),
                           # multi‑select dropdown (defaults to none selected)
                           selectizeInput("sum_vars", "Select variable(s) to summarise:",
                                          choices = c(            # add new vars here anytime
                                            "None" ,"vacc_status", "hospitalized", "working_status","final_status",
                                            "sex_at_birth","age_group","race","hispanic_latino",
                                            "city","county","state_form","residence_country",
                                            "outbreak_related","recent_travel",
                                            "import_status","us_acquired_detail", "zip"    # ← NEW
                                          ),
                                          multiple = TRUE),
                           DTOutput("summary_freq_dt")
                  ),
      
      # ─── TAB 3 ──────────────────────────────────────────────
      tabPanel("Cross‑tabulation",
               br(),
               h5("Fixed: Vaccination Status × Age Group"),
               DTOutput("crosstab_fixed"),
               
               hr(),
               
               h5("Create your own two‑way table"),
               fluidRow(
                 column(6,
                        selectizeInput("cross_var1", "Row variable:",
                                       choices = c( "None" ,"vacc_status", "hospitalized", "working_status","final_status",
                                                    "sex_at_birth","age_group","race","hispanic_latino",
                                                    "city","county","state_form","residence_country",
                                                    "outbreak_related","recent_travel",
                                                    "import_status","us_acquired_detail", "zip" ),
                                       selected = "vacc_status")
                 ),
                 column(6,
                        selectizeInput("cross_var2", "Column variable:",
                                       choices = c( "None" ,"vacc_status", "hospitalized", "working_status","final_status",
                                                   "sex_at_birth","age_group","race","hispanic_latino",
                                                   "city","county","state_form","residence_country",
                                                   "outbreak_related","recent_travel",
                                                   "import_status","us_acquired_detail", "zip" ),
                                       selected = "age_group")
                 )
               ),
               DTOutput("crosstab_custom")
      ),
      
      # ─── TAB 4 ──────────────────────────────────────────────
      tabPanel("Exposure / Contagious Details",
               br(),
               DTOutput("exposure_table")
      ),
      
      # ─── TAB 5 ──────────────────────────────────────────────
      tabPanel("Gantt Chart",
               br(),
               selectizeInput("gantt_var", "Colour bars by:",
                              choices = c( "None" ,"vacc_status", "hospitalized", "working_status","final_status",
                                            "sex_at_birth","age_group","race","hispanic_latino",
                                            "city","county","state_form","residence_country",
                                            "outbreak_related","recent_travel",
                                            "import_status","us_acquired_detail", "zip" ),
                              selected = "working_status"),
               plotlyOutput("gantt_dynamic", height = "450px")
      ),
      
      # ─── TAB 6 ──────────────────────────────────────────────
      tabPanel("Epidemic Curve",
               br(),
               h5("Overall (no stratification)"),
               plotlyOutput("epi_curve_overall", height = "350px"),
               
               hr(),
               h5("Stacked by selected variable"),
               selectizeInput("epi_var", "Choose variable:",
                              choices = c(  "None" ,"vacc_status", "hospitalized", "working_status","final_status",
                                            "sex_at_birth","age_group","race","hispanic_latino",
                                            "city","county","state_form","residence_country",
                                            "outbreak_related","recent_travel",
                                            "import_status","us_acquired_detail", "zip" ),
                              selected = "vacc_status"),
               plotlyOutput("epi_curve_dynamic", height = "350px")
      ),
      
      # ─── TAB 7 ──────────────────────────────────────────────
      tabPanel("Rt Estimation",
               br(),
               #selectizeInput("rt_filter", "Filter data by:",
                            #  choices = key_vars, multiple = TRUE),       # see helper below
               selectInput("rt_var",   "Filter column:",   choices = key_vars),
               uiOutput("rt_val_box"),   # second dropdown appears after a column is chosen
               numericInput("rt_mean_si", "Mean serial interval (days):", value = 12),
               numericInput("rt_sd_si",   "SD serial interval (days):",   value = 3),
               uiOutput("active_filters"),
               
               plotlyOutput("rt_plot_dynamic", height = "350px")
      ),
      
      # ─── TAB 8 ──────────────────────────────────────────────
      tabPanel("Forecasting",
               br(),
               #selectizeInput("fc_filter", "Filter data by:",
                             # choices = key_vars, multiple = TRUE),
               # TAB 8 – Forecasting
               selectInput("fc_var",   "Filter column:",   choices = key_vars),
               uiOutput("fc_val_box"),
               
               numericInput("fc_Rt", "Rt to project with:", value = NA, step = 0.1),
               numericInput("fc_mean_si", "Mean serial interval (days):", value = 12),
               numericInput("fc_sd_si",   "SD serial interval (days):",   value = 3),
               actionButton("run_fc", "Run 14‑day forecast"),
               plotlyOutput("forecast_plot", height = "350px"),
               uiOutput("active_filters")
      ),
      
      # ─── TAB 9 ──────────────────────────────────────────────
      tabPanel("Scenario Modeling",
               br(),
               #selectizeInput("sc_filter", "Filter data by:",
                             # choices = key_vars, multiple = TRUE),
               # TAB 8 – Scenario Modeling
               selectInput("sm_var",   "Filter column:",   choices = key_vars),
               uiOutput("sm_val_box"),
               textInput("sc_Rt_vals",   "Rt values (comma‑sep):",    "0.9,1.1,1.4"),
               textInput("sc_Rt_labels", "Custom labels (⇢ same order, comma‑sep, optional):",
                         "Low,Mid,High"),
               numericInput("sc_mean_si", "Mean serial interval (days):", value = 12),
               numericInput("sc_sd_si",   "SD serial interval (days):",   value = 3),
               
               actionButton("run_sc", "Generate scenarios"),
               plotlyOutput("scenario_plot", height = "350px")
      ),
      
      
      
      tabPanel("Overview Dashboard",
               fluidPage(
                 tags$h2("Nebraska 2025 Measles Outbreak Dashboard"),
                 tags$h5(paste("Last updated", format(Sys.Date(), "%d %B %Y"))),
                 
                 fluidRow(
                   valueBoxOutput("total_cases_box", width = 3),
                   valueBoxOutput("total_hospitalized_box", width = 3),
                   valueBoxOutput("total_deaths_box", width = 3),
                   valueBoxOutput("county_count_box", width = 3)
                 ),
                 
                 fluidRow(
                   box(title = "Vaccination Status Among Cases", width = 5, solidHeader = TRUE,
                       DTOutput("crosstab_fixedd")),
                   box(title = "Cases by Age Group", width = 4, solidHeader = TRUE,
                       DTOutput("age_table")),
                   box(title = "Cases by County at Diagnosis", width = 4, solidHeader = TRUE,
                       DTOutput("county_table"))
                 )
               ),
               titlePanel("Nebraska Measles Outbreak & Surrounding Counties"),
               
               leafletOutput("county_map", height = 850)
               
               
      ),
      
      
      
      # ─── TAB About ──────────────────────────────────────────
      tabPanel("About",
               br(),
               includeMarkdown("about.md")        # keep a tiny markdown file with intro, author, links
      )
      
      
),
  
  # ⬇️ Footer goes here
  tags$hr(),
  tags$div(style = "font-size: 12px; color: gray; text-align: center; padding-bottom: 10px;",
           HTML(paste0(
             "© 2025 ddwarabandam. All rights reserved. ",
             "Designed for Outbreak Management, & rapid analytics of data generated from CDC Measles Standardized Case Investigation Form data (",
             "<a href='https://www.cdc.gov/measles/downloads/2024-dvd-measles-investigation-form.pdf' target='_blank'>source</a>). ",
             "Documentation: ",
             "<a href='https://github.com/ddwarabandam/Measles-ETL-Outbreak-Analytics-Web-App/blob/main/README.md' target='_blank'>README</a>, ",
             "<a href='https://github.com/ddwarabandam/Measles-ETL-Outbreak-Analytics-Web-App.git' target='_blank'>ETL Outbreak Analytics Web App Github Repo</a>. ",
             "<a href='https://github.com/ddwarabandam/Measles-Investigation-Form-Web-App.git' target='_blank'> Measles Investigation Web App AlphaV2.0 Github Repo</a>. ",
             "<a href='https://dattatechddwarabandam.shinyapps.io/Measles-Investigation-Form-WebV2/' target='_blank'> Measles Investigation Web App AlphaV2.0</a>. "
             "<b> Measles Outbreak Management & Analytics Web Apps Toolkit – Alpha v3.1</b>")
           )
           
    )
   )
  )
)



server <- function(input, output, session) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  
  cleaned_data <- reactive({
    
    req(input$crf_file)
    tryCatch({
      
      raw <- read_excel(input$crf_file$datapath)
      
      # Identify date columns
      date_cols <- grep("date", names(raw), ignore.case = TRUE, value = TRUE)
      
      # Step 1: Convert everything to character
      df <- raw %>%
        mutate(across(everything(), as.character))
      
      # Step 2: Convert date fields to Date
      df <- df %>%
        mutate(across(all_of(date_cols), ~ suppressWarnings(as.Date(.))))
      
      # Step 3: Create age_group
      if ("age" %in% names(df)) {
        df <- df %>%
          mutate(age = as.numeric(age),
                 age_group = cut(
                   age,
                   breaks = c(0, 5, 9, 14, 19, 64, Inf),
                   labels = c("0-5", "6-9", "10-14", "15-19", "20-64", "65+"),
                   include.lowest = TRUE, right = TRUE
                 ))
      } else if ("dob" %in% names(df)) {
        # Calculate age from DOB if age is missing
        df <- df %>%
          mutate(dob = as.Date(dob),
                 age_calc = as.integer(floor(as.numeric(Sys.Date() - dob) / 365.25)),
                 age_group = cut(
                   age_calc,
                   breaks = c(0, 5, 9, 14, 19, 64, Inf),
                   labels = c("0-5", "6-9", "10-14", "15-19", "20-64", "65+"),
                   include.lowest = TRUE, right = TRUE
                 ))
        
      }
      
      # ✅ Always create vacc_status (moved outside conditional)
      df <- df %>%
        mutate(
          vaccine_received = tolower(trimws(as.character(vaccine_received))),
          vaccine_doses_num = suppressWarnings(as.numeric(vaccine_doses_num)),
          vacc_status = case_when(
            vaccine_received == "yes" & vaccine_doses_num >= 2 ~ "Fully Vaccinated",
            vaccine_received == "yes" & vaccine_doses_num == 1 ~ "Partially Vaccinated",
            vaccine_received == "yes" & vaccine_doses_num == 0 ~ "Unvaccinated",
            vaccine_received == "no" ~ "Unvaccinated",
            vaccine_received == "unknown" | is.na(vaccine_received) ~ "Unknown",
            TRUE ~ "Unknown"
          ),
          vacc_status = factor(vacc_status, levels = c("Fully Vaccinated", "Partially Vaccinated", "Unvaccinated", "Unknown"))
        )
      
      
      return(df)
      
    }, error = function(e) {
      showNotification(paste("❌ Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  observeEvent(input$map_filter_var, {
    req(input$map_filter_var != " ")
    vals <- cleaned_data()[[input$map_filter_var]] |> unique() |> sort()
    updateSelectizeInput(session, "map_filter_vals", choices = vals, server = TRUE)
  })
  
  output$map_filter_val_box <- renderUI({
    if (input$map_filter_var != " ") {
      selectizeInput("map_filter_vals", "Select value(s):", choices = NULL, multiple = TRUE)
    }
  })
  
  filtered_df <- reactive({
    df <- cleaned_data()
    if (input$map_filter_var != " " && !is.null(input$map_filter_vals)) {
      df <- df[df[[input$map_filter_var]] %in% input$map_filter_vals, ]
    }
    df
  })
  
  output$filter_value_inputs <- renderUI({
    req(input$filter_vars)
    
    lapply(input$filter_vars, function(var) {
      values <- cleaned_data()[[var]] |> unique() |> sort()
      selectizeInput(
        inputId = paste0("filter_", var),
        label = paste("Select", var, ":"),
        choices = values,
        selected = NULL,
        multiple = TRUE
      )
    })
  })
  
  
  multi_filtered_df <- reactive({
    req(cleaned_data())
    if (is.null(input$filter_vars)) return(cleaned_data())
    
    selections <- list()
    for (var in input$filter_vars) {
      val <- input[[paste0("filter_", var)]]
      if (!is.null(val)) {
        selections[[var]] <- val
      }
    }
    
    filter_dataset(cleaned_data(), selections)
  })
  
  observeEvent(input$reset_filters, {
    # Reset main filter_vars
    updateSelectInput(session, "filter_vars", selected = NULL)
    
    # Also reset dynamically created sub-filters
    if (!is.null(input$filter_vars)) {
      for (var in input$filter_vars) {
        updateSelectizeInput(session, paste0("filter_", var), selected = NULL)
      }
    }
  })
  
  multi_filtered_df <- reactive({
    req(cleaned_data())
    
    selections <- list()
    if (!is.null(input$filter_vars)) {
      for (var in input$filter_vars) {
        val <- input[[paste0("filter_", var)]]
        if (!is.null(val)) {
          selections[[var]] <- val
        }
      }
    }
    
    filter_dataset(cleaned_data(), selections)
  })
  
  
  # 2a. Cleaned table – turn on column filters like Excel
  output$cleaned_table <- renderDT({
    dat <- multi_filtered_df(); if (is.null(dat)) return(NULL)
    datatable(dat,
              extensions = "Buttons",
              filter    = "top",          # << enable per‑column search boxes
              options   = list(pageLength = 15, scrollX = TRUE),
              rownames  = FALSE,
              class     = "display nowrap compact")
  })
  
  # 2b. Dynamic summary‑frequency table ---------------------------------
  # helper that returns the long‑format freq dataframe
  summarise_freq <- function(df, vars) {
    if (length(vars) == 0) return(data.frame())
    purrr::map_dfr(vars, function(v) {
      df %>%
        group_by(!!sym(v)) %>%
        summarise(Count = n_distinct(case_id)) %>%
        mutate(Variable = v) %>%
        rename(Category = !!sym(v)) %>%
        select(Variable, Category, Count)
    })
  }
  
  
  # reactive table
  output$summary_freq_dt <- renderDT({
    df <- multi_filtered_df(); req(df)
    freq_df <- summarise_freq(df, input$sum_vars)
    datatable(freq_df,
              extensions = "Buttons",
              options   = list(pageLength = 10, scrollX = TRUE),
              rownames  = FALSE,
              class     = "display compact")
  })
  
  # ---------- 3a. Fixed vaccination × age cross‑tab ----------
  output$crosstab_fixed <- renderDT({
    df <- multi_filtered_df(); req(df)
    if (!all(c("vacc_status", "age_group") %in% names(df)))
      return(DT::datatable(data.frame(Message = "Required columns not found")))
    df %>%
      group_by(vacc_status, age_group) %>%
      summarise(cases = n_distinct(case_id), .groups = 'drop') %>%
      pivot_wider(names_from = age_group, values_from = cases, values_fill = 0)
    tab <- as.data.frame.matrix(table(df$vacc_status, df$age_group, useNA = "ifany"))
    DT::datatable(tab, extensions = "Buttons",
                  options = list(dom = "Bt", buttons = c("copy","csv","excel"),
                                 pageLength = 10, scrollX = TRUE))
  })
  
  # ---------- 3b. User‑defined two‑way cross‑tab ----------
  output$crosstab_custom <- renderDT({
    df <- multi_filtered_df(); req(df, input$cross_var1, input$cross_var2)
    vars <- c(input$cross_var1, input$cross_var2)
    if (!all(vars %in% names(df)))
      return(DT::datatable(data.frame(Message = "Selected variables absent in dataset")))
    
    tab <- df %>%
      group_by(.data[[vars[1]]], .data[[vars[2]]]) %>%
      summarise(cases = n_distinct(case_id), .groups = 'drop') %>%
      pivot_wider(names_from = vars[2], values_from = cases, values_fill = 0)
    
    DT::datatable(tab, extensions = "Buttons",
                  options = list(dom = "Bt", buttons = c("copy","csv","excel"),
                                 pageLength = 10, scrollX = TRUE))
  })
  
  
  # ---------- 4. Exposure / Contagious period details ----------
  output$exposure_table <- renderDT({
    df <- multi_filtered_df(); req(df)
    
    # add contagious & exposure window if symptom_onset_date exists
    if ("symptom_onset_date" %in% names(df)) {
      df <- df |>
        mutate(symptom_onset_date = as.Date(symptom_onset_date),
               contagious_start  = symptom_onset_date - 4,
               contagious_end    = symptom_onset_date + 4,
               exposure_start_date = symptom_onset_date - 21,
               exposure_end_date = symptom_onset_date - 5,
               contagious_period = paste(contagious_start, contagious_end, sep = " to "))
    }
    if ("symptom_onset_date" %in% names(df) &
        "exposure_start_date" %in% names(df) & "exposure_end_date" %in% names(df)) {
      df <- df |> mutate(exposure_period =
                           paste(exposure_start_date, exposure_end_date, sep = " to "))
    }
    
    keep_cols <- c("case_id", "nndss_id" , "vaccine_received", "vaccine_doses_num", "contagious_period","exposure_period",
                   "working_status","final_status","recent_travel",
                   "travel_depart_date","travel_return_date","countries_visited",
                   "exposure_source_contact","source_case_id",
                   "comments_source_case","exposure_period_contact_details",
                   "contagious_period_contact_details",
                   "household_contact_details","additional_contact_comments")
    view_df <- df[, intersect(keep_cols, names(df)), drop = FALSE]
    
    DT::datatable(view_df,
                  extensions = "Buttons",
                  filter = "top",
                  options = list(dom = "Blftipr", buttons = c("copy","csv","excel"),
                                 pageLength = 15, scrollX = TRUE),
                  rownames = FALSE,
                  class = "display nowrap compact")
  })
  
  
  # ---------- 5. Dynamic Gantt Chart ------------------------------------
  output$gantt_dynamic <- renderPlotly({
    df <- multi_filtered_df(); req(df, input$gantt_var)
    req(all(c("case_id","symptom_onset_date") %in% names(df)))
    
    df <- df |>
      mutate(symptom_onset_date = as.Date(symptom_onset_date),
             contagious_start   = symptom_onset_date - 4,
             contagious_end     = symptom_onset_date + 4,
             cat_var            = .data[[input$gantt_var]])
    
    # replace NA with "Unknown" for clarity
    df$cat_var[is.na(df$cat_var)] <- "Unknown"
    
    df <- df |>
      mutate(label = paste0("Case: ", case_id,
                            "<br>", input$gantt_var, ": ", cat_var,
                            "<br>Contagious: ", contagious_start,
                            " → ", contagious_end),
             mid_point = as.Date((as.numeric(contagious_start) +
                                    as.numeric(contagious_end)) / 2,
                                 origin = "1970-01-01"))
    
    p <- ggplot(df, aes(y = reorder(case_id, contagious_start),
                        x = contagious_start, xend = contagious_end,
                        color = cat_var)) +
      geom_segment(aes(yend = case_id), linewidth = 4) +
      geom_point(aes(x = mid_point, text = label), size = 0, alpha = 0) +
      labs(title = paste("Contagious Period (±4 days) coloured by", input$gantt_var),
           x = "Date", y = "Case ID", color = input$gantt_var) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> plotly::layout(legend = list(orientation = "h"))
  })
  
  # ---------- 6a. Overall epidemic curve (no strat) ----------------------
  output$epi_curve_overall <- renderPlotly({
    df <- multi_filtered_df(); req(df)
    req("symptom_onset_date" %in% names(df))
    
    daily <- df |>
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) |>
      filter(!is.na(symptom_onset_date)) |>
      count(symptom_onset_date, name = "incidence")
    
    p <- ggplot(daily, aes(symptom_onset_date, incidence)) +
      geom_col(fill = "#1f77b4", alpha = 0.8) +
      labs(x = "Symptom onset date", y = "Number of cases",
           title = "Epidemic curve (overall)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------- 6b. Stacked curve by user‑selected var ---------------------
  output$epi_curve_dynamic <- renderPlotly({
    df <- multi_filtered_df(); req(df, input$epi_var)
    req("symptom_onset_date" %in% names(df))
    
    df <- df |>
      mutate(symptom_onset_date = as.Date(symptom_onset_date),
             strat = .data[[input$epi_var]]) |>
      filter(!is.na(symptom_onset_date))
    
    # handle datasets with missing selected variable
    if (!"strat" %in% names(df)) {
      return(plotly::plot_ly() |> plotly::add_text(x = 0, y = 0,
                                                   text = "Selected variable not in dataset"))
    }
    
    daily <- df |> count(symptom_onset_date, strat, name = "incidence")
    
    p <- ggplot(daily,
                aes(symptom_onset_date, incidence, fill = strat)) +
      geom_col(position = "stack", alpha = 0.9) +
      labs(x = "Symptom onset date", y = "Number of cases",
           title = paste("Epidemic curve stacked by", input$epi_var),
           fill = input$epi_var) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
 
  # ─────────────────────────────────────────────────────────────
  # Helpers -----------------------------------------------------
  
  # build a distcrete gamma serial‑interval from mean / SD
  make_si <- function(mean_si, sd_si) {
    distcrete::distcrete("gamma",
                         shape = (mean_si^2) / (sd_si^2),
                         rate  =  mean_si    / (sd_si^2),
                         w = 0, interval = 1)
  }
  
  # estimate mean Rt over the most recent 7‑day window
  mean_rt_last <- function(inc_vec, mean_si, sd_si) {
    if (length(inc_vec) < 8) return(NA_real_)
    res <- EpiEstim::estimate_R(
      incid = inc_vec,
      method = "parametric_si",
      config = EpiEstim::make_config(list(
        mean_si = mean_si,
        std_si  = sd_si,
        t_start = seq_len(length(inc_vec) - 7),
        t_end   = seq(8, length(inc_vec))
      ))
    )
    tail(res$R$`Mean(R)`, 1)
  }
  
  # summarise projections into quantiles
  proj_summary_df <- function(proj_mat, dates) {
    data.frame(
      date   = dates,
      lower  = apply(proj_mat, 1, quantile, 0.025, na.rm = TRUE),
      median = apply(proj_mat, 1, quantile, 0.50,  na.rm = TRUE),
      upper  = apply(proj_mat, 1, quantile, 0.975, na.rm = TRUE)
    )
    
  }
  
  # ---------- Safe Rt estimation helper --------------------------------------
  estimate_rt_df <- function(daily_inc, mean_si = 12, sd_si = 3) {
    I <- daily_inc$incidence
    if (length(I) < 8) return(NULL)              # not enough data
    t_start <- seq(2, length(I) - 7)
    t_end   <- t_start + 7
    tryCatch(
      EpiEstim::estimate_R(
        incid  = I,
        method = "parametric_si",
        config = EpiEstim::make_config(list(
          mean_si = mean_si,
          std_si  = sd_si,
          t_start = t_start,
          t_end   = t_end
        ))
      ),
      error = function(e) NULL
    )
  }
  
  
  observeEvent(input$rt_var, {
    req(input$rt_var)
    df <- isolate(cleaned_data())
    if (!is.null(df) && input$rt_var %in% names(df)) {
      vals <- sort(unique(df[[input$rt_var]]))
      updateSelectizeInput(session, "rt_vals", choices = vals, server = TRUE)
    }
  })
  
  
  output$rt_val_box <- renderUI({
    selectizeInput("rt_vals", "Choose value(s):", choices = NULL,
                   multiple = TRUE)
  })
  
  
  # ---------- Filtered Estimation of Rt Plot ----------------------------------------------
  output$rt_plot_dynamic <- renderPlotly({
    df0 <- multi_filtered_df()
    if (length(input$rt_vals) > 0)
      df0 <- df0[df0[[input$rt_var]] %in% input$rt_vals, ]
    
    req("symptom_onset_date" %in% names(df0))
    daily <- df0 |>
      dplyr::mutate(symptom_onset_date = as.Date(symptom_onset_date)) |>
      dplyr::filter(!is.na(symptom_onset_date)) |>
      dplyr::count(symptom_onset_date, name = "incidence") |>
      dplyr::arrange(symptom_onset_date)
    
    est <- estimate_rt_df(daily,
                          mean_si = input$rt_mean_si %||% 12,
                          sd_si   = input$rt_sd_si   %||% 3)

    validate(need(!is.null(est), "Need ≥ 8 daily observations after filtering."))
    

    
    
    Rt_df <- as.data.frame(est$R)
    Rt_df$mid_date <- seq(min(daily$symptom_onset_date),
                          by = "days", length.out = nrow(daily))[Rt_df$t_end]
    
    p <- ggplot(Rt_df, aes(mid_date)) +
      geom_ribbon(aes(ymin = `Quantile.0.025(R)`,
                      ymax = `Quantile.0.975(R)`,
                      fill = "95% Credible Interval"), alpha = 0.3) +
      geom_line(aes(y = `Mean(R)`, color = "Mean"), linewidth = 1.2) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_fill_manual(name = NULL, values = c("95% Credible Interval" = "tomato")) +
      scale_color_manual(name = NULL, values = c("Mean" = "tomato4")) +
      labs(title = "Filtered Rt Estimate",
           x = "Date", y = "Rt") +
      theme_minimal()
    
    
    ggplotly(p)
  })
  
  observeEvent(input$fc_var, {
    req(input$fc_var)
    df <- isolate(cleaned_data())
    if (!is.null(df) && input$fc_var %in% names(df)) {
      vals <- sort(unique(df[[input$fc_var]]))
      updateSelectizeInput(session, "fc_vals", choices = vals, server = TRUE)
    }
  })
  
  
  output$fc_val_box <- renderUI({
    selectizeInput("fc_vals", "Choose value(s):", choices = NULL,
                   multiple = TRUE)
  })
  
  
  # ---------- Forecast observer (crash‑proof) -------------------------------
  observeEvent(input$run_fc, {
    
    df0 <- multi_filtered_df()
    if (length(input$fc_vals) > 0)
      df0 <- df0[df0[[input$fc_var]] %in% input$fc_vals, ]
    
    
    req("symptom_onset_date" %in% names(df0))
    daily <- df0 |>
      dplyr::mutate(symptom_onset_date = as.Date(symptom_onset_date)) |>
      dplyr::filter(!is.na(symptom_onset_date)) |>
      dplyr::count(symptom_onset_date, name = "incidence") |>
      dplyr::arrange(symptom_onset_date)
    req(nrow(daily) > 0)
    
    mean_si <- input$fc_mean_si
    sd_si   <- input$fc_sd_si
    est <- estimate_rt_df(daily, mean_si, sd_si)
    last_rt <- if (!is.null(est)) tail(est$R$`Mean(R)`, 1) else NA_real_
    
    Rt_val <- ifelse(is.na(input$fc_Rt), last_rt, input$fc_Rt)
    validate(need(!is.na(Rt_val) && Rt_val > 0,
                  "Supply a positive Rt value or provide ≥ 8 days to derive one."))
    
    incid_obj <- incidence::incidence(
      dates = rep(daily$symptom_onset_date, daily$incidence), interval = 1
    )
    gen_time <- make_si(mean_si, sd_si)
    
    proj <- projections::project(
      x = incid_obj, R = Rt_val, si = gen_time,
      n_sim = 5000, n_days = 14
    )
    sum_df <- proj_summary_df(as.matrix(proj), attr(proj, "dates"))
    
    p <- ggplot(sum_df, aes(date)) +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = "lightblue", alpha = 0.5) +
      geom_line(aes(y = median), colour = "blue", linewidth = 1.2) +
      labs(title = paste0("14‑day forecast (Rt = ", round(Rt_val, 2), ")"),
           x = "Date", y = "Predicted incidence") +
      theme_minimal()
    
    output$forecast_plot <- renderPlotly({ ggplotly(p) })
  })
  
    
  observeEvent(input$sm_var, {
    req(input$sm_var)
    df <- isolate(multi_filtered_df())
    if (!is.null(df) && input$sm_var %in% names(df)) {
      vals <- sort(unique(df[[input$sm_var]]))
      updateSelectizeInput(session, "sm_vals", choices = vals, server = TRUE)
    }
  })
  
  
  output$sm_val_box <- renderUI({
    selectizeInput("sm_vals", "Choose value(s):", choices = NULL,
                   multiple = TRUE)
  })
  

  # -----------------------------------------------------------------------------
  # Scenario projections --------------------------------------------------------
  observeEvent(input$run_sc, {
    
    df0 <- multi_filtered_df()
    if (length(input$sm_vals) > 0)
      df0 <- df0[df0[[input$sm_var]] %in% input$sm_vals, ]
    
    req("symptom_onset_date" %in% names(df0))
    daily <- df0 |>
      dplyr::mutate(symptom_onset_date = as.Date(symptom_onset_date)) |>
      dplyr::filter(!is.na(symptom_onset_date)) |>
      dplyr::count(symptom_onset_date, name = "inc")
    req(nrow(daily) > 0)
    
    incid_obj <- incidence::incidence(
      dates = rep(daily$symptom_onset_date, daily$inc),
      interval = 1
    )
    
    mean_si <- input$sc_mean_si %||% 12
    sd_si   <- input$sc_sd_si   %||% 3
    gen_time <- make_si(mean_si, sd_si)
    
    validate(
      need(mean_si > 0 && sd_si > 0, "Mean and SD of SI must be positive.")
    )
    
    
    Rt_raw  <- unlist(strsplit(input$sc_Rt_vals,   "[,;\n]+"))
    Lab_raw <- unlist(strsplit(input$sc_Rt_labels, "[,;\n]+"))
    
    Rt_vec  <- as.numeric(trimws(Rt_raw))
    lab_vec <- trimws(Lab_raw)
    
    Rt_vec <- Rt_vec[!is.na(Rt_vec) & Rt_vec > 0]
    lab_vec <- lab_vec[lab_vec != ""]  # remove blanks
    
    # Match lengths: fallback to default labels
    if (length(lab_vec) != length(Rt_vec)) {
      lab_vec <- paste0("Rt = ", Rt_vec)
    }
    
    # Reorder scenarios by Rt values
    scenario_levels <- lab_vec[order(Rt_vec)]
    
    
    Rt_vec <- Rt_vec[!is.na(Rt_vec) & Rt_vec > 0]
    validate(need(length(Rt_vec) > 0, "Enter ≥ 1 positive Rt value."))
    
    # recycle / default labels
    if (length(lab_vec) < length(Rt_vec) || length(lab_vec) == 0)
      lab_vec <- paste0("Rt = ", Rt_vec)
    
    # Reorder scenarios by Rt values
    scenario_levels <- lab_vec[order(Rt_vec)]
    
    scen_dfs <- purrr::map2_dfr(Rt_vec, lab_vec, function(r, lab) {
      proj <- projections::project(
        x = incid_obj, R = r, si = gen_time,
        n_sim = 5000, n_days = 14
      )
      df <- proj_summary_df(as.matrix(proj), attr(proj, "dates"))
      df$scenario <- lab
      df$scenario <- factor(lab, levels = scenario_levels)
      
      df
    })
    
    
    all_df <- scen_dfs
    
    
    p <- ggplot(all_df, aes(date)) +
      geom_ribbon(aes(ymin = lower, ymax = upper,
                      fill = "95% CI"), alpha = 0.35) +
      geom_line(aes(y = median, colour = "Median"), linewidth = 1.1) +
      facet_wrap(~ scenario, scales = "free_y") +
      scale_fill_manual(values = "lightblue", guide = "none") +
      scale_colour_manual(values = "blue",  guide = "none") +
      labs(title = "14‑day projections under alternative Rt scenarios",
           x = "Date", y = "Predicted incidence") +
      theme_minimal()
    
    output$scenario_plot <- renderPlotly({ ggplotly(p) })
  })
  
  

  
 # state_counties <- reactive({
  #  counties(state = input$selected_state, cb = TRUE, class = "sf") %>%
  #    mutate(group = str_to_title(NAME))
 # })
  

  # Save full built leaflet map inside reactive
  built_leaflet_map <- reactive({
    df <- multi_filtered_df(); req(df)
   # counties_sf <- state_counties()
    
    # 1. Load Nebraska counties
    ne_counties <- counties(state = "NE", cb = TRUE, class = "sf") %>%
     mutate(group = str_to_title(NAME))
    
    # 2. Load case data
    count_df <- df %>%
      filter(!is.na(county)) %>%
      mutate(county = str_to_title(county)) %>%
      group_by(county) %>%
      summarise(cases = n_distinct(case_id)) %>% 
      mutate(label = case_when(
        cases == 0 ~ "0",
        cases <  6 ~ "1–5",
        TRUE       ~ as.character(cases)
      )) %>%
      rename(group = county)
    
    # 3. Join shapefile with case counts
    map_data <- ne_counties  %>%
      left_join(count_df, by = "group") %>%
      replace_na(list(cases = 0, label = "0"))
    
    # 4. Identify outbreak and surrounding counties
    outbreak_counties <- map_data %>% filter(cases > 0) %>% pull(group)
    outbreak_geom <- map_data %>% filter(group %in% outbreak_counties) %>% st_union()
    
    neighbor_flags <- lengths(safe_intersects(map_data$geometry, outbreak_geom)) > 0 &
      !(map_data$group %in% outbreak_counties)
    
    map_data <- map_data %>%
      mutate(
        category = case_when(
          group %in% outbreak_counties ~ "Outbreak County",
          neighbor_flags               ~ "Surrounding County",
          TRUE                         ~ "No Cases"
        ),
        popup_text = paste0(
          "<strong>", group, "</strong><br/>",
          "Outbreak MMR Dose Recommended: ", ifelse(category == "No Cases", "No", "Yes"), "<br/>",
          "Number of measles cases: ", label
        )
      )
    
    # 5. Build palette
    pal <- colorFactor(
      palette = c("Outbreak County" = "#f05e23",
                  "Surrounding County" = "#2c7fb8",
                  "No Cases" = "#ffffff"),
      domain = c("Outbreak County", "Surrounding County", "No Cases")
    )
    
    # 6. Nebraska state outline
    ne_state <- states(cb = TRUE, class = "sf") %>%
      filter(STUSPS == "NE")
    
    # 7. Labels data
    label_centroids <- st_centroid(map_data)
    
    # 8. Render map
    leaflet_object <- leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # Nebraska border (bold red outline)
      addPolylines(data = ne_state,
                   color = "red",
                   weight = 4,
                   fill = FALSE,
                   opacity = 1) %>%
      
      # County polygons with category shading
      addPolygons(
        fillColor   = ~pal(category),
        color       = "black",        # bold borders
        weight      = 2.5,
        fillOpacity = 0.8,
        label       = ~lapply(popup_text, HTML),
        highlight = highlightOptions(
          weight = 3,
          color = "#333",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      
      # County labels (larger text)
      addLabelOnlyMarkers(data = label_centroids,
                          label = ~group,
                          labelOptions = labelOptions(
                            noHide = TRUE,
                            direction = "center",
                            textOnly = TRUE,
                            style = list(
                              "color" = "#000000",
                              "font-family" = "Arial",
                              "font-size" = "12px",
                              "font-weight" = "bold"
                            )
                          )) %>%
      
      # Legend
      addLegend("bottomright", pal = pal, values = ~category,
                title = "County Category", opacity = 0.95)
    
    leaflet_object

  })
  
  output$county_map <- renderLeaflet({
    built_leaflet_map()
  })
  
  output$download_map_png <- downloadHandler(
    filename = function() {
      paste0("measles_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      saveWidget(built_leaflet_map(), "temp_map.html", selfcontained = TRUE)
      webshot2::webshot("temp_map.html", file = file, vwidth = 1400, vheight = 900, cliprect = "viewport")
      file.remove("temp_map.html")
    }
  )
  
  
  
  
  
  output$total_cases_box <- renderValueBox({
    total <- n_distinct(multi_filtered_df()$case_id)
    valueBox(total, "Total Cases", color = "blue")
  })
  
  output$total_hospitalized_box <- renderValueBox({
    count <- multi_filtered_df() |> filter(hospitalized == "Yes") |> summarise(n = n_distinct(case_id)) |> pull(n)
    valueBox(count, "Total Hospitalized", color = "orange")
  })
  
  output$total_deaths_box <- renderValueBox({
    count <- multi_filtered_df() |> 
      filter(final_status == "Death") |> 
      summarise(n = n_distinct(case_id)) |> 
      pull(n)
    valueBox(count, "Total Deaths", color = "red")
  })
  
  output$county_count_box <- renderValueBox({
    count <- multi_filtered_df() |> pull(county) |> str_to_title() |> unique() |> length()
    valueBox(count, "Counties with Cases", color = "green")
  })
  
  
  output$age_table <- renderDT({
    multi_filtered_df() |> 
      group_by(age_group) |>
      summarise(Cases = n_distinct(case_id)) |> 
      arrange(desc(Cases)) |>
      datatable(options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  output$county_table <- renderDT({
    multi_filtered_df() |>
      mutate(county = str_to_title(county)) |>
      group_by(county) |>
      summarise(raw_count = n_distinct(case_id)) |>
      mutate(Cases = ifelse(raw_count < 6, "1–5", as.character(raw_count))) |>
      select(county, Cases) |>
      arrange(desc(Cases)) |>
      datatable(options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  output$crosstab_fixedd <- renderDT({
    df <- multi_filtered_df(); req(df)
    if (!all(c("vacc_status", "age_group") %in% names(df)))
      return(DT::datatable(data.frame(Message = "Required columns not found")))
    
    tab <- df |>
      group_by(vacc_status, age_group) |>
      summarise(cases = n_distinct(case_id), .groups = 'drop') |>
      pivot_wider(names_from = age_group, values_from = cases, values_fill = 0)
    
    DT::datatable(tab, extensions = "Buttons",
                  options = list(dom = "Bt", buttons = c("copy","csv","excel"),
                                 pageLength = 10, scrollX = TRUE))
  })
  
  
  output$active_filters <- renderUI({
    req(input$filter_vars)
    HTML(paste0("<b>Current Filters:</b><br/>", 
                paste(purrr::map_chr(input$filter_vars, function(v) {
                  vals <- input[[paste0("filter_", v)]]
                  paste(v, ": ", paste(vals, collapse = ", "))
                }), collapse = "<br/>")))
  })
  
 
}


shinyApp(ui = ui, server = server)
