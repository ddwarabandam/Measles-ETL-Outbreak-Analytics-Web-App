# Measles Outbreak Management and Analytics Web App (Alpha v3.1)
This web application is designed to support fast and accurate measles case investigations and contact tracing.
It streamlines case data entry, organizes key information, and prepares datasets for automated analysis through the ETL and outbreak analytics companion app.
Together, these tools help public health teams summarize cases, model epidemic growth, forecast outbreaks, and guide timely decision-making.

This application was built by **Devi Dwarabandam, MPH, a-IPC, BDS**, Epidemiologist & R Developer, for efficient outbreak data management and modeling of measles case investigations.

### 🔍 Features

- **ETL & Cleaning:** Upload CRF data, auto-parse fields, derive age/vaccination status
- **Visual Analytics:** Epidemic curves, cross-tabulations, exposure & contagious period timelines
- **Gantt View:** Case-level contagious periods with custom coloring
- **Rt Estimation:** Using [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/) with user-defined Serial Interval
- **Forecasting:** 14-day projections with adjustable Rt and SI
- **Scenario Modeling:** Run multiple Rt values with optional labels, faceted outputs
New features added!

### 📊 Modeling Assumptions

- Rt estimation uses **Bayesian methods** (EpiEstim), showing **95% Credible Intervals (CrI)**
- Forecasts and projections assume Gamma-distributed serial intervals
- Data-driven insights depend on complete and timely symptom onset information

### 📎 Resources

- [CDC Measles Investigation Form PDF](https://www.cdc.gov/measles/downloads/2024-dvd-measles-investigation-form.pdf)
- [Measles Investigation Form Web App AlphaV2.0](https://dattatechddwarabandam.shinyapps.io/Measles-Investigation-Form-WebV2/)
- [Measles ETL Outbreak Analytics Web App GitHub Repo](https://github.com/ddwarabandam/Measles-ETL-Outbreak-Analytics-Web-App.git)
- [Measles Investigation Web App GitHub Repo](https://github.com/ddwarabandam/Measles-Investigation-Form-Web-App.git)
- [Read Me](https://github.com/ddwarabandam/Measles-ETL-Outbreak-Analytics-Web-App/blob/main/README.md)

---

*© 2025 ddwarabandam. Built using R, Shiny, and CDC Measles Investigation Form.*
