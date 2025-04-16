# 📦 Required packages
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("dplyr")) install.packages("dplyr")

library(readxl)
library(writexl)
library(dplyr)

# 📁 Set folder path where individual CRF files are stored
folder_path <- "insert filepath"

# 📂 Get all Excel files in the folder
files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# 🧱 Read and combine all files
combined_df <- do.call(rbind, lapply(files, function(file) {
  tryCatch(
    read_excel(file),
    error = function(e) {
      message(paste("Skipping file due to error:", file))
      return(NULL)
    }
  )
}))

# 💾 Save to a master Excel file
output_file <- file.path(folder_path, "measles_cases_combined.xlsx")
write_xlsx(combined_df, output_file)

message("✅ Successfully combined all case files into: ", output_file)
