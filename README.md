# 🚉 Ridership Analysis & Visualization Using R + Shiny

![R](https://img.shields.io/badge/R-4.3.3-blue?logo=r)
![Shiny](https://img.shields.io/badge/Shiny-App-blueviolet?logo=shiny)
![Dashboard](https://img.shields.io/badge/Dashboard-Interactive-green)
![Deployed](https://img.shields.io/badge/Deployment-shinyapps.io-success)
![Statistical Analysis](https://img.shields.io/badge/Statistics-Wilcoxon%20%7C%20ANOVA-lightgrey)

## 📌 Project Overview

This project analyzes public transport ridership data to detect seasonal patterns, holiday effects, and weekday variations. It employs **statistical tests**, **interactive dashboards**, and **data visualization** to deliver insights on commuter behavior.

The project was developed in **R** using a combination of **`dplyr`**, **`ggplot2`**, **`shiny`**, and **`leaflet`**, and is deployed live via shinyapps.io.

🔗 **Live Demo**: [Shiny App](https://tlililoukillaatarrproject.shinyapps.io/ShinyApp_RProject/)

---

## 👨‍💻 Authors

- **Mohamed Amine Tlili**
- **Mohamed Aziz Loukil**
- **Mohamed Laatar**

---

## 📊 Project Goals

- Merge and clean large-scale `.txt` ridership datasets
- Identify the impact of holidays vs non-holidays on validations
- Analyze differences across weekdays
- Visualize spatio-temporal ridership trends
- Build and deploy an interactive dashboard for exploration

---

## 📁 Dataset Summary

- 🔢 **Files**: `.txt` data files from 2018 to 2023 (12 NB_FER + 12 PROFIL_FER)
- 🧭 **Sources**: Metro & train ridership logs
- 📍 **Columns**: Stop ID, line code, timestamp, validation counts, holiday category, time slot
- ✅ Data cleaning included:
  - Removing null-only rows
  - Normalizing separator characters
  - Unifying column names
  - Handling encoding issues

---

## 📈 Statistical Methods

### 📌 Normality Test
- Histogram of `Total_NB_VALD` revealed lack of normality ⇒ parametric tests not valid

### 📌 Wilcoxon Rank Sum Test
- Used to compare **Holiday** vs **Non-Holiday** ridership
- 🔍 Result: `p-value < 2.2e-16` ⇒ statistically significant median difference

### 📌 ANOVA Test
- Used to analyze **Weekday-based** variation
- ✔️ Valid despite non-normality due to large sample size
- 🔍 Result: `F(6, 1.5M) = 6160.6, p < 2.2e-16` ⇒ strong difference between weekdays

---

## 🖥️ Shiny App Features

| Module         | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| 📅 Date Filter   | Select data by year or semester                                             |
| 📍 Map View      | Interactive map of stations using `leaflet`                                |
| 📊 Plot Section  | Weekly and daily validation bar charts with ggplot2                        |
| 📋 Table Viewer  | Show summary statistics by time slot or holiday status                     |
| 📈 Statistical View | Output of Wilcoxon and ANOVA tests with p-values and effect sizes        |

---

## 🧰 Tech Stack

- **R (4.3.3)**
- 📦 `dplyr`, `readr`, `shiny`, `shinydashboard`, `ggplot2`, `leaflet`
- 📦 `sf`, `lubridate`, `scales`, `timeDate`, `readxl`, `stringi`

---

## 🚀 How to Run Locally

```r
# 1. Install required packages
install.packages(c("shiny", "shinydashboard", "ggplot2", "leaflet", "dplyr", 
                   "lubridate", "scales", "readxl", "stringi", "sf", "timeDate"))

# 2. Clone the repository
git clone https://github.com/YourUsername/Ridership-ShinyApp.git
setwd("Ridership-ShinyApp")

# 3. Run the app
shiny::runApp()

