# Load required libraries
# install.packages("charlatan") # Remove the hash tag to install the charlatan package for fake data generation library
library(charlatan)
library(openxlsx)
library(dplyr)
library(lubridate)

set.seed(123)
n <- 100  # Number of sample cases
n_demo <- 100

# Define helper function to generate random dates between two dates
random_date <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  start + sample(0:(as.integer(end - start)), 1)
}

# Set date range for report/investigation dates
today <- Sys.Date()
start_date <- today - 180  # past 6 months

# Generate dummy data for each column

df_sample <- data.frame(
  # --- 1. Case Info ---
  case_id = paste0("NE-2024-", sprintf("%05d", 1000 + 0:(n-1))),
  state_case_id = paste0("24ME", 1000 + 0:(n-1)),
  lab_id = paste0("LAB", sprintf("%05d", 10000 + 0:(n-1))),
  nndss_id = as.character(sample(1000000000:9999999999, n, replace = TRUE)),
  patient_name = ch_name(n = n),
  report_date = replicate(n, as.character(random_date(start_date, today))),
  invest_start_date = replicate(n, as.character(random_date(start_date, today))),
  db_entry_date = replicate(n, as.character(random_date(start_date, today))),
  db_lastentry_date = replicate(n, as.character(random_date(start_date, today))),
  interviewer_name = ch_name(n = n),
  interviewer_contact = ch_phone_number(n = n),
  working_status = sample(c("Confirmed - Lab", "Confirmed - Epi", "Probable", "Suspect", "Not a Case", "Unknown"), n, replace = TRUE),
  call_log             = paste("Example Call attempts information", 1:n),
  
  # --- 2. Demographics ---
  respondent_name = ch_name(n = n),
  respondent_relation = sample(c("Mother", "Father", "Sibling", "Other"), n, replace = TRUE),
  address = rep("123 Main St", n_demo),
  city = sample(c("Hastings", "Lincoln", "Omaha"), n, replace = TRUE),
  county = sample(c("Adams", "Lancaster", "Douglas"), n, replace = TRUE),
  state_form = sample(c("Nebraska", "Iowa", "Kansas"), n, replace = TRUE),
  zip = as.character(sample(60000:69999, n, replace = TRUE)),
  telephone = ch_phone_number(n = n),
  residence_country = sample(c("United States", "Canada", "Mexico"), n, replace = TRUE),
  sex_at_birth = sample(c("Male", "Female", "No answer"), n, replace = TRUE),
  age = sample(1:90, n, replace = TRUE),
  dob = replicate(n, as.character(random_date("1930-01-01", as.character(today - 365)))),
  hispanic_latino = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  race = sample(c("White", "Black/African American", "Asian/Pacific Islander", 
                  "American Indian/Alaska Native", "Other", "Unknown"), n, replace = TRUE),
  
  # --- 3. Clinical ---
  symptom_onset_date = replicate(n, as.character(random_date(start_date, today))),
  symptom_rash = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  rash_onset_date = replicate(n, as.character(random_date(start_date, today))),
  rash_generalized = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  rash_desc = paste("Rash description", 1:n),
  rash_current = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  rash_duration_days = sample(c(NA, 3, 5, 7), n, replace = TRUE),
  symptom_fever = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  fever_onset_date = replicate(n, as.character(random_date(start_date, today))),
  max_temp = sample(c(98.6, 99.1, 100.4, 101.5, 102.2, NA), n, replace = TRUE),
  temp_scale = sample(c("°F", "°C"), n, replace = TRUE),
  symptom_cough = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_coryza = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_conjunctivitis = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_otitis = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_pneumonia = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_diarrhea = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_vomiting = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_dehydration = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_low_platelets = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_encephalitis = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  symptom_other = paste("Other symptoms info", 1:n),
  
  # --- 4. Exposure/Contact ---
  recent_travel = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  travel_depart_date = replicate(n, as.character(random_date(start_date, today))),
  travel_return_date = replicate(n, as.character(random_date(start_date, today))),
  countries_visited = sample(c("India", "Italy", "Mexico", "None", "France"), n, replace = TRUE),
  known_source_contact = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  source_case_id = paste0("NE-2024-", sprintf("%05d", 900 + 0:(n-1))),
  source_exposure_details = paste("Exposure details", 1:n),
  community_exposed_contacts = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  community_contact_details = paste("Community contact details", 1:n),
  household_contacts_details = paste("Household contacts info", 1:n),
  additional_contact_comments = paste("Additional comments", 1:n),
  
  # --- 5. Vaccination & PEP ---
  vaccine_received = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  vaccine_doses_num = sample(c(0, 1, 2, NA), n, replace = TRUE),
  vaccine_dose1_date = replicate(n, as.character(random_date(start_date - 300, start_date))),
  vaccine_dose2_date = replicate(n, as.character(random_date(start_date - 300, start_date))),
  vaccine_dose3_date = replicate(n, as.character(random_date(start_date - 300, start_date))),
  pep_received = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  pep_type = sample(c("Vaccine", "Immunoglobulin (IG)", "Unknown"), n, replace = TRUE),
  pep_date = replicate(n, as.character(random_date(start_date, today))),
  pep_within_timeframe = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  ig_admin_method = sample(c("Intramuscular", "Intravenous", "Unknown"), n, replace = TRUE),
  
  # --- 6. Healthcare & Labs ---
  healthcare_visited = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  healthcare_location = sample(c("Clinic", "ER", "Hospital"), n, replace = TRUE),
  healthcare_visit_date = replicate(n, as.character(random_date(start_date, today))),
  healthcare_facility = paste("Facility", 1:n),
  hospitalized = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  hospital_name = paste("Hospital", 1:n),
  hospital_admit_date = replicate(n, as.character(random_date(start_date, today))),
  hospital_discharge_date = replicate(n, as.character(random_date(start_date, today))),
  patient_death = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  death_date = replicate(n, as.character(random_date(start_date, today))),
  testing_performed = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  specimen_permission = sample(c("Yes", "No"), n, replace = TRUE),
  test_pcr_result = sample(c("Positive", "Negative", "Pending"), n, replace = TRUE),
  test_pcr_date = replicate(n, as.character(random_date(start_date, today))),
  test_pcr_lab = paste("PCR Lab", 1:n),
  test_igm_result = sample(c("Positive", "Negative", "Pending"), n, replace = TRUE),
  test_igm_date = replicate(n, as.character(random_date(start_date, today))),
  test_igm_lab = paste("IgM Lab", 1:n),
  test_igg_acute_result = sample(c("Positive", "Negative", "Pending"), n, replace = TRUE),
  test_igg_acute_date = replicate(n, as.character(random_date(start_date, today))),
  test_igg_acute_lab = paste("IgG Acute Lab", 1:n),
  test_igg_conval_result = sample(c("Positive", "Negative", "Pending"), n, replace = TRUE),
  test_igg_conval_date = replicate(n, as.character(random_date(start_date, today))),
  test_igg_conval_lab = paste("IgG Conval Lab", 1:n),
  test_genotype_result = sample(c("D8", "B3", "Unknown"), n, replace = TRUE),
  test_genotype_date = replicate(n, as.character(random_date(start_date, today))),
  test_genotype_lab = paste("Genotype Lab", 1:n),
  test_meva_result = sample(c("Positive", "Negative", "Pending"), n, replace = TRUE),
  test_meva_date = replicate(n, as.character(random_date(start_date, today))),
  test_meva_lab = paste("MeVA Lab", 1:n),
  additional_comments = paste("Additional comments", 1:n),
  
  # --- 7. Final Status ---
  final_status = sample(c("Confirmed - Lab", "Confirmed - Epi", "Probable", "Suspect", "Not a Case", "Unknown"), n, replace = TRUE),
  outbreak_related = sample(c("Yes", "No", "Unknown"), n, replace = TRUE),
  outbreak_name = paste("Outbreak", 1:n),
  import_status = sample(c("International importation", "U.S. acquired"), n, replace = TRUE),
  us_acquired_detail = sample(c("Import-linked case", "Imported-virus case", "Endemic case", "Unknown source case"), n, replace = TRUE),
  
  stringsAsFactors = FALSE
)

# Define the output path; update this path if needed.
output_path <- "C:/Users/aksha/Downloads/COPH/APEX/Tech Start Up-Datta Data LLC/measlesapp/crfetllatest"

# Save the data frame as an Excel file
openxlsx::write.xlsx(df_sample, file = output_path, rowNames = FALSE)

cat("Dummy sample data saved to:", output_path, "\n")
