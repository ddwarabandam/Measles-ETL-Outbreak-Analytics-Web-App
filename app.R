
library(shiny)
library(readxl)
library(dplyr)
library(DT)
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




ui <- fluidPage(
  titlePanel("Measles Cases ETL Web App: Upload & Clean, Visualize, Epidemic Modeling, Custom Scenario Projections"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("crf_file", "Upload Measles Case Excel File", 
                accept = c(".xlsx")),
      helpText("Accepted format: Excel file exported from CRF app (.xlsx)"),
     # selectInput("epi_stratify_by", "Stratify Epidemic Curve By:",
                #  choices = c("None", "age_group", "sex_at_birth", "vacc_status",
                 #             "race", "hispanic_latino", "city", "county",
                  #            "residence_country", "state_form",
                 #             "outbreak_related", "recent_travel"),
                 # selected = "None"),
      # Fixed scenarios as strings (we'll convert to numeric later)
      checkboxGroupInput("fixed_scenarios", "Select Fixed Rt Scenarios:",
                         choices = c("1.3", "1.5", "1.6"),
                         selected = c("1.3", "1.5", "1.6")),
      
      # Inputs for a custom scenario:
      textInput("custom_label", "Custom Scenario Label", value = "My Custom Scenario"),
      numericInput("custom_Rt", "Custom Rt Value", value = NA, min = 0, step = 0.1),
      actionButton("add_custom", "Add Custom Scenario")
      
      
      
      
    ),
    
    mainPanel(
      h4("Cleaned Case Records"),
      DTOutput("cleaned_table"),
      h4("Summary Frequencies for Key Variables"),
      tableOutput("summary_freq"),
      
      h4("Vaccination Status × Age Group Cross-tab"),
      tableOutput("cross_tab"),
      h4("Epidemic Curve by Symptom Onset Date"),
      plotlyOutput("epi_curve", height = "400px"),
      #h4("Gantt Chart of Case Infectious Periods"),
      #plotOutput("gantt_chart", height = "350px"),
      #h4("Gantt Chart (Tooltips)- Infectious Period of Cases: Unsorted"),
      #plotlyOutput("gantt_plot", height = "400px"),
      h4("Gantt Chart (Tooltips)- Infectious Period of Cases: Sorted by Onset Date"),
      plotlyOutput("gantt_ploty", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Vaccination Status"),
      plotlyOutput("epi_curve_vacc_status", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Age Group"),
      plotlyOutput("epi_curve_age_group", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Gender at Birth"),
      plotlyOutput("epi_curve_sex", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, city"),
      plotlyOutput("epi_curve_city", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, County"),
      plotlyOutput("epi_curve_county", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, State"),
      plotlyOutput("epi_curve_state", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, race"),
      plotlyOutput("epi_curve_race", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Ethnicity"),
      plotlyOutput("epi_curve_hispanic", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Outbreak-related"),
      plotlyOutput("epi_curve_outbreak", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Travel-linked"),
      plotlyOutput("epi_curve_travel", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Import Status"),
      plotlyOutput("epi_curve_import_status", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Working Status"),
      plotlyOutput("epi_curve_working_status", height = "400px"),
      h4("Epidemic Curve by Symptom Onset Date, Final Status"),
      plotlyOutput("epi_curve_final_status", height = "400px"),
      h4("Estimated Reproduction Number (Rt)over Time"),
      plotlyOutput("rt_plot", height = "400px"),
      h4("14-Day Forecast Projection (Assuming R = 2.0), 5000 Sims"),
      plotlyOutput("projection_plot", height = "400px"),
      h4("Multiple Fixed Rt Scenario Projection, 5000 Sims"),
      plotlyOutput("facet_fixed_plot", height = "400px"),
      h4("Multiple Custom Rt Scenario Projection, 5000 Sims"),
      plotlyOutput("facet_custom_plot", height = "400px"),

      
     
      
      
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
  
  
  # Render the cleaned table using DT
  output$cleaned_table <- renderDT({
    dat <- cleaned_data()
    if (is.null(dat)) return(NULL)
    
    datatable(dat,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              class = "display nowrap compact")
  })
  
  
  # Summary Frequencies
  output$summary_freq <- renderTable({
    df <- cleaned_data()
    req(df)
    
    summary_cols <- c("vacc_status", "working_status", "final_status", "sex_at_birth",
                      "age_group", "race", "hispanic_latino",
                      "city", "county", "state_form", "residence_country",
                      "outbreak_related", "recent_travel")
    
    summary_freq_df <- data.frame(Variable = character(),
                                  Category = character(),
                                  Count = numeric(),
                                  stringsAsFactors = FALSE)
    
    for(col_name in summary_cols) {
      if(!col_name %in% names(df)) next
      freq_tbl <- table(df[[col_name]], useNA = "ifany")
      tmp <- data.frame(
        Variable = col_name,
        Category = names(freq_tbl),
        Count = as.numeric(freq_tbl),
        stringsAsFactors = FALSE
      )
      summary_freq_df <- rbind(summary_freq_df, tmp)
    }
    
    if(nrow(summary_freq_df) == 0){
      return(data.frame(Message = "No summary columns found in the dataset."))
    }
    
    summary_freq_df
  })
  
  # Cross-tab: vacc_status × age_group
  output$cross_tab <- renderTable({
    df <- cleaned_data()
    req(df)
    
    if (!all(c("vacc_status","age_group") %in% names(df))) {
      return(data.frame(Message = "No vacc_status × age_group cross-tab, columns not found."))
    }
    
    tab <- table(df$vacc_status, df$age_group, useNA = "ifany")
    df_tab <- as.data.frame(tab)
    
    # Rename the columns for clarity
    names(df_tab) <- c("Vaccination Status", "Age Group", "Count")
    
    df_tab
  })
  
  # ------------------------------
  # Epidemic Curve with Stratification
  # ------------------------------
  output$epi_curve <- renderPlotly({
    df <- cleaned_data()   # our cleaned dataset
    req(df)
    
    # Ensure symptom_onset_date is properly formatted
    df <- df %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    if(nrow(df) == 0) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="No valid symptom_onset_date values."))
    }
    
    # Get stratification input
    group_var <- input$epi_stratify %||% "None"
    
    if(is.null(group_var) || group_var == "None" || !(group_var %in% names(df))) {
      # Without stratification: aggregate daily counts
      daily_counts <- df %>%
        group_by(symptom_onset_date) %>%
        summarise(incidence = n(), .groups = "drop") %>%
        arrange(symptom_onset_date)
      
      p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence)) +
        geom_col(alpha = 0.7, fill = "#1f77b4") +
        labs(title = "Epidemic Curve by Symptom Onset Date",
             x = "Symptom Onset Date", y = "Number of Cases") +
        theme_minimal()
      
      return(ggplotly(p))
    }
    
    # With stratification
    df <- df %>% filter(!is.na(!!sym(group_var)))
    # Group by symptom_onset_date and stratification variable
    strat_counts <- df %>%
      group_by(symptom_onset_date, strat = !!sym(group_var)) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p_strat <- ggplot(strat_counts, aes(x = symptom_onset_date, y = incidence, fill = strat)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = paste("Epidemic Curve by", group_var),
           x = "Symptom Onset Date", y = "Number of Cases", fill = group_var) +
      theme_minimal()
    
    ggplotly(p_strat)
  })
  
  
  
  output$epi_curve_vacc_status <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    # Ensure vacc_status exists; if not, derive it
    if (!("vacc_status" %in% names(df)) && "vaccine_received" %in% names(df)) {
      df <- df %>%
        mutate(vaccine_doses_num = as.numeric(vaccine_doses_num),
               vacc_status = case_when(
                 vaccine_received == "Yes" & vaccine_doses_num >= 2 ~ "Fully Vaccinated",
                 vaccine_received == "Yes" & vaccine_doses_num == 1 ~ "Partially Vaccinated",
                 vaccine_received == "No" ~ "Unvaccinated",
                 TRUE ~ "Unknown"
               ))
    }
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, vacc_status) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = vacc_status)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Vaccination Status)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Vaccination Status") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$epi_curve_outbreak <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, outbreak_related) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = outbreak_related)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Outbreak Related)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Outbreak Related") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_import_status <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, import_status) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = import_status)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Import Status)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Import Status") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_travel <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, recent_travel) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = recent_travel)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Recent Travel)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Recent Travel") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_working_status <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, working_status) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = working_status)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Working Status)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Working Status") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_final_status <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, final_status) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = final_status)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Final Case Status)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Final Case Status") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_age_group <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    # Ensure age_group exists; if not, derive it from 'age' or calculate from 'dob'
    if (!("age_group" %in% names(df))) {
      if ("age" %in% names(df)) {
        df <- df %>%
          mutate(age = as.numeric(age),
                 age_group = cut(age,
                                 breaks = c(0, 5, 9, 14, 19, 64, Inf),
                                 labels = c("0-5", "6-9", "10-14", "15-19", "20-64", "65+"),
                                 include.lowest = TRUE, right = TRUE))
      } else if ("dob" %in% names(df)) {
        df <- df %>%
          mutate(dob = as.Date(dob),
                 age_calc = as.integer(floor(as.numeric(Sys.Date() - dob)/365.25)),
                 age_group = cut(age_calc,
                                 breaks = c(0, 5, 9, 14, 19, 64, Inf),
                                 labels = c("0-5", "6-9", "10-14", "15-19", "20-64", "65+"),
                                 include.lowest = TRUE, right = TRUE))
      } else {
        df$age_group <- "Unknown"
      }
    }
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, age_group) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = age_group)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Age Group)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Age Group") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_sex <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, sex_at_birth) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = sex_at_birth)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Sex at Birth)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Sex at Birth") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$epi_curve_race <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, race) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = race)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Race)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Race") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_hispanic <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, hispanic_latino) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = hispanic_latino)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (Hispanic/Latino)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "Hispanic/Latino") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_city <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, city) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = city)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (City)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "City") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_county <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, county) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = county)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (County)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "County") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$epi_curve_state <- renderPlotly({
    df <- cleaned_data() %>%
      mutate(
        symptom_onset_date = as.Date(symptom_onset_date),
        state_form = as.factor(state_form)  # Force state_form to factor
      ) %>%
      filter(!is.na(symptom_onset_date))
    
    daily_counts <- df %>%
      group_by(symptom_onset_date, state_form) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    p <- ggplot(daily_counts, aes(x = symptom_onset_date, y = incidence, fill = state_form)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Epidemic Curve by Symptom Onset Date (State)",
           x = "Symptom Onset Date", y = "Number of Cases", fill = "State") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$gantt_ploty <- renderPlotly({
    df <- cleaned_data()
    req(df)
    
    # Check that required columns exist
    if (!("symptom_onset_date" %in% names(df)) || !("case_id" %in% names(df))) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x = 0, y = 0, text = "Required columns missing (symptom_onset_date or case_id)."))
    }
    
    # Ensure symptom_onset_date is Date class
    df <- df %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date))
    
    # Calculate contagious period: 4 days before and after symptom onset
    df_gantt <- df %>% 
      filter(!is.na(symptom_onset_date)) %>%
      mutate(contagious_start = symptom_onset_date - 4,
             contagious_end   = symptom_onset_date + 4)
    
    # If vacc_status is missing, derive it from vaccine_received and vaccine_doses_num
    if (!("vacc_status" %in% names(df_gantt)) && "vaccine_received" %in% names(df_gantt)) {
      df_gantt <- df_gantt %>%
        mutate(vaccine_doses_num = as.numeric(vaccine_doses_num),
               vacc_status = case_when(
                 vaccine_received == "Yes" & vaccine_doses_num >= 2 ~ "Fully Vaccinated",
                 vaccine_received == "Yes" & vaccine_doses_num == 1 ~ "Partially Vaccinated",
                 vaccine_received == "No" ~ "Unvaccinated",
                 TRUE ~ "Unknown"
               ))
    }
    
    # Ensure final_status exists; if missing, assign a default value
    if (!("final_status" %in% names(df_gantt))) {
      df_gantt <- df_gantt %>% mutate(final_status = "Not Specified")
    }
    
    # Create a label that includes contagious period, vacc_status, and final_status
    df_gantt <- df_gantt %>%
      mutate(label = paste("Case:", case_id,
                           "<br>Vaccination:", vacc_status,
                           "<br>Final Status:", final_status,
                           "<br>Contagious Start:", contagious_start,
                           "<br>Contagious End:", contagious_end))
    
    # Compute the midpoint of the contagious period. Since these are Date objects,
    # convert to numeric (days since 1970-01-01), compute the average, then convert back.
    df_gantt <- df_gantt %>%
      mutate(mid_point = as.Date((as.numeric(contagious_start) + as.numeric(contagious_end)) / 2, 
                                 origin = "1970-01-01"))
    
    # Build the Gantt chart:
    # - geom_segment draws the visible bars (without text aesthetic)
    # - geom_point with size 0 carries the tooltip text
    p_gantt <- ggplot(df_gantt, aes(y = reorder(case_id, contagious_start))) +
      geom_segment(aes(x = contagious_start, xend = contagious_end,
                       y = case_id, yend = case_id),
                   linewidth = 4, alpha = 0.8, color = "steelblue") +
      geom_point(aes(x = mid_point, text = label), size = 0, alpha = 0) +
      labs(title = "Contagious Period (±4 days from Symptom Onset)",
           x = "Date", y = "Case ID") +
      theme_minimal()
    
    ggplotly(p_gantt, tooltip = "text")
  })
  
  
  
  # ------------------------------
  # Enhanced Gantt Chart with Labels
  # ------------------------------
  output$gantt_plot <- renderPlotly({
    df <- cleaned_data()
    req(df)
    
    # Ensure required columns exist: symptom_onset_date and case_id
    if (!("symptom_onset_date" %in% names(df)) || !("case_id" %in% names(df))) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x=0, y=0, text="Required columns missing (symptom_onset_date or case_id)."))
    }
    
    # Ensure symptom_onset_date is Date class
    df <- df %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date))
    
    # Calculate contagious period: 4 days before and after symptom onset
    df_gantt <- df %>%
      filter(!is.na(symptom_onset_date)) %>%
      mutate(contagious_start = symptom_onset_date - 4,
             contagious_end = symptom_onset_date + 4)
    
    # If vaccination status is not in data, calculate it
    if (!("vacc_status" %in% names(df_gantt)) && "vaccine_received" %in% names(df_gantt)) {
      df_gantt <- df_gantt %>%
        mutate(vaccine_doses_num = as.numeric(vaccine_doses_num),
               vacc_status = case_when(
                 vaccine_received == "Yes" & vaccine_doses_num >= 2 ~ "Fully Vaccinated",
                 vaccine_received == "Yes" & vaccine_doses_num == 1 ~ "Partially Vaccinated",
                 vaccine_received == "No" ~ "Unvaccinated",
                 TRUE ~ "Unknown"
               ))
    }
    
    # For final case status, assume column "final_status" exists
    if (!("final_status" %in% names(df_gantt))) {
      df_gantt <- df_gantt %>%
        mutate(final_status = "Not Specified")
    }
    
    # Create a label for each case showing: case_id, vacc_status, and final_status
    df_gantt <- df_gantt %>%
      mutate(label = paste("Case:", case_id, "<br>",
                           "Vaccination:", vacc_status, "<br>",
                           "Final Status:", final_status))
    
    # Create the Gantt chart using ggplot2 and add interactive text labels
    p_gantt <- ggplot(df_gantt, aes(y = reorder(case_id, contagious_start))) +
      geom_segment(aes(x = contagious_start, xend = contagious_end,
                       y = case_id, yend = case_id,
                       text = label),
                   linewidth = 4, alpha = 0.8, color = "steelblue") +
      labs(title = "Contagious Period (±4 days from Symptom Onset)",
           x = "Date", y = "Case ID") +
      theme_minimal()
    
    ggplotly(p_gantt, tooltip = "text")
  })
  
  
  
  
  
  output$rt_plot <- renderPlotly({
    df <- cleaned_data()
    
    if (!"symptom_onset_date" %in% names(df)) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x = 0, y = 0, text = "No 'symptom_onset_date' column found."))
    }
    
    df_rt <- df %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date)) %>%
      group_by(symptom_onset_date) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    if (nrow(df_rt) < 8) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x = 0, y = 0, text = "Not enough data for Rt estimation."))
    }
    
    I <- df_rt$incidence
    t_start <- seq(2, length(I) - 7)
    t_end   <- t_start + 7
    
    # Serial interval settings
    mean_si <- 12
    std_si <- 3
    
    # Use tryCatch to safely handle errors
    res_R <- tryCatch({
      estimate_R(
        incid = I,
        method = "parametric_si",
        config = make_config(list(
          mean_si = mean_si,
          std_si = std_si,
          t_start = t_start,
          t_end = t_end
        ))
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(res_R) || is.null(res_R$R)) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x = 0, y = 0, text = "Rt estimation failed."))
    }
    
    df_R <- as.data.frame(res_R$R)
    if (nrow(df_R) == 0) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x = 0, y = 0, text = "No Rt data to plot."))
    }
    
    # Map R output to dates
    min_date <- min(df_rt$symptom_onset_date)
    date_seq <- seq(min_date, by = "days", length.out = length(I))
    df_R$mid_date <- date_seq[df_R$t_end]
    
    p_rt <- ggplot(df_R, aes(x = mid_date)) +
      geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`, fill = "95% Credible Interval"),
                  alpha = 0.3) +
      geom_line(aes(y = `Mean(R)`, color = "Mean Rt"), size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
      labs(
        title = "Estimated Reproduction Number (Rt)",
        x = "Date",
        y = "Rt",
        color = "Legend",
        fill = " "
      ) +
      scale_color_manual(values = c("Mean Rt" = "tomato")) +
      scale_fill_manual(values = c("95% Credible Interval" = "tomato")) +
      theme_minimal()
    
    
    ggplotly(p_rt)
  })
  
  
  
  
  
  output$projection_plot <- renderPlotly({
    df <- cleaned_data()
    req(df)
    
    if (!("symptom_onset_date" %in% names(df))) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x = 0, y = 0, text = "No 'symptom_onset_date' column available."))
    }
    
    df_rt <- df %>%
      filter(!is.na(symptom_onset_date)) %>%
      group_by(symptom_onset_date) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    if (nrow(df_rt) < 1) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x = 0, y = 0, text = "Not enough data to run projection."))
    }
    
    # Build incidence object from daily counts
    incid_obj <- incidence::incidence(
      dates = rep(df_rt$symptom_onset_date, df_rt$incidence),
      interval = 1
    )
    
    # Define serial interval for measles (using a gamma distribution)
    gen_time <- distcrete::distcrete(
      "gamma",
      shape = (12^2) / (3^2),
      rate  = 12 / (3^2),
      w = 0,
      interval = 1
    )
    
    # Generate projections: simulate 5000 scenarios for 14 days ahead assuming R = 2.0
    proj <- project(
      x = incid_obj,
      R = 2.0,
      si = gen_time,
      n_sim = 5000,
      n_days = 14
    )
    
    # Convert the projection object to a matrix
    proj_mat <- as.matrix(proj)
    dates_proj <- attr(proj, "dates")
    
    # Calculate quantiles manually for each day (row)
    proj_summary <- data.frame(
      date = dates_proj,
      lower = apply(proj_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
      median = apply(proj_mat, 1, quantile, probs = 0.5, na.rm = TRUE),
      upper = apply(proj_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
    
    # Create a cleaner ggplot: just a line for the median and a ribbon for the CI
    p_proj <- ggplot(proj_summary, aes(x = date)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.5) +
      geom_line(aes(y = median, color = "Median Projection"), size = 1.5) +
      labs(
        title = "14-Day Projection (R = 2.0)",
        x = "Date",
        y = "Predicted Incidence",
        color = "Legend",
        fill = " "
      ) +
      scale_color_manual(values = c("Median Projection" = "blue")) +
      scale_fill_manual(values = c("95% Credible Interval" = "lightblue")) +
      scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.title = element_text(size = 12))
    
    
    ggplotly(p_proj)
  })
  
  
  # Initialize a reactive value to store custom scenarios as a list of lists.
  # Each scenario is a list with elements: label (character) and Rt (numeric).
  # Initialize a reactive value to store custom scenarios
  custom_scenarios <- reactiveVal(list())
  
  # Observer to handle custom scenario addition
  observeEvent(input$add_custom, {
    if (!is.na(input$custom_Rt) && input$custom_Rt > 0) {
      
      # Clean up the optional custom label
      cleaned_label <- trimws(input$custom_label)
      
      # Force a consistent label format
      scenario_label <- if (cleaned_label != "") {
        paste0(cleaned_label, " (Rt = ", input$custom_Rt, ")")
      } else {
        paste0("Fixed Rt = ", input$custom_Rt)
      }
      
      # Append the new scenario
      current <- custom_scenarios()
      new_scenario <- list(label = scenario_label, Rt = as.numeric(input$custom_Rt))
      current[[length(current) + 1]] <- new_scenario
      custom_scenarios(current)
      
      showNotification(paste("Added scenario:", scenario_label))
      
    } else {
      showNotification("Please enter a valid custom Rt value.", type = "error")
    }
  })
  
  
  # Combine this with your existing custom_scenarios() setup
  # and the rest of your ETL code.
  
  output$facet_fixed_plot <- renderPlotly({
    req(cleaned_data())
    
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date)) %>%
      group_by(symptom_onset_date) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    if (nrow(df) < 1) return(plotly::plot_ly() %>% plotly::add_text(x=0, y=0, text="No data."))
    
    incid_obj <- incidence::incidence(
      dates = rep(df$symptom_onset_date, df$incidence), interval = 1
    )
    
    gen_time <- distcrete::distcrete("gamma",
                                     shape = (12^2)/(3^2), rate = 12/(3^2), w = 0, interval = 1
    )
    
    fixed_vals <- c(1.3, 1.5, 1.6)
    fixed_scenarios <- lapply(fixed_vals, function(x) list(label = paste("Fixed Rt =", x), Rt = x))
    
    fixed_summaries <- lapply(fixed_scenarios, function(scen) {
      proj <- project(x = incid_obj, R = scen$Rt, si = gen_time, n_sim = 1000, n_days = 14)
      proj_mat <- as.matrix(proj)
      dates_proj <- attr(proj, "dates")
      data.frame(
        date = dates_proj,
        lower = apply(proj_mat, 1, quantile, probs = 0.025, na.rm=TRUE),
        median = apply(proj_mat, 1, quantile, probs = 0.5, na.rm=TRUE),
        upper = apply(proj_mat, 1, quantile, probs = 0.975, na.rm=TRUE),
        scenario = scen$label
      )
    })
    
    # Add numeric Rt for sorting
    df_fixed_all <- do.call(rbind, fixed_summaries) %>%
      mutate(Rt_val = as.numeric(sub("Fixed Rt = ", "", scenario)))
    
    p <- ggplot(df_fixed_all, aes(x = date)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.4) +
      geom_line(aes(y = median, color = "Median Projection"), size = 1.2) +
      facet_wrap(~ fct_reorder(scenario, Rt_val)) +  # Sort facet panels
      scale_fill_manual(values = c("95% Credible Interval" = "lightblue")) +
      scale_color_manual(name = "Legend", values = c("Median Projection" = "blue")) +
      labs(
        title = "Faceted 14-Day Projection - Fixed Rt Values",
        x = "Date", y = "Predicted Incidence",
        fill = "95% Credible Interval",
        color = "Median Projection"
      ) +
      theme_minimal()
    
    
    ggplotly(p)
  })
  
  ### Each panel represents a different Rt scenario.
  
  ### The shaded area is the 95% confidence interval of projected cases.
  
  ### The solid line is the median projected incidence across 1000 simulations.
  
  ### Comparing facets shows how more contagious (higher Rt) scenarios lead to faster, larger outbreaks.
  
  
  
  output$facet_custom_plot <- renderPlotly({
    req(cleaned_data())
    df <- cleaned_data() %>%
      mutate(symptom_onset_date = as.Date(symptom_onset_date)) %>%
      filter(!is.na(symptom_onset_date)) %>%
      group_by(symptom_onset_date) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(symptom_onset_date)
    
    if (nrow(df) < 1) return(plotly::plot_ly() %>% plotly::add_text(x=0, y=0, text="No data."))
    
    incid_obj <- incidence::incidence(
      dates = rep(df$symptom_onset_date, df$incidence), interval = 1
    )
    
    gen_time <- distcrete::distcrete("gamma",
                                     shape = (12^2)/(3^2), rate = 12/(3^2), w = 0, interval = 1
    )
    
    scenarios <- custom_scenarios()
    if (length(scenarios) == 0) {
      return(plotly::plot_ly() %>% plotly::add_text(x=0, y=0, text="No custom scenarios."))
    }
    
    custom_summaries <- lapply(scenarios, function(scen) {
      proj <- project(x = incid_obj, R = scen$Rt, si = gen_time, n_sim = 1000, n_days = 14)
      proj_mat <- as.matrix(proj)
      dates_proj <- attr(proj, "dates")
      data.frame(
        date = dates_proj,
        lower = apply(proj_mat, 1, quantile, probs = 0.025, na.rm=TRUE),
        median = apply(proj_mat, 1, quantile, probs = 0.5, na.rm=TRUE),
        upper = apply(proj_mat, 1, quantile, probs = 0.975, na.rm=TRUE),
        scenario = scen$label
      )
    })
    
    df_custom_all <- do.call(rbind, custom_summaries) %>%
      mutate(Rt_val = as.numeric(gsub("[^0-9.]", "", gsub(".*Rt = ([0-9.]+).*", "\\1", scenario))))
    
    p <- ggplot(df_custom_all, aes(x = date)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.4) +
      geom_line(aes(y = median, color = "Median Projection"), size = 1.2) +
      facet_wrap(~ fct_reorder(scenario, Rt_val)) +
      scale_fill_manual(values = c("95% Credible Interval" = "lightblue")) +
      scale_color_manual(name = "Legend", values = c("Median Projection" = "blue")) +
      labs(
        title = "Faceted 14-Day Projection - Custom Rt Scenarios",
        x = "Date", y = "Predicted Incidence",
        fill = "95% Credible Interval",
        color = "Median Projection"
      ) +
      theme_minimal()
    
    
    ggplotly(p)
  })
  
  
  
}


shinyApp(ui = ui, server = server)
