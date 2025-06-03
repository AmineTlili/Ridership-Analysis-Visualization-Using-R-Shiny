#Loading Necessary Libraries
############
#### Load necessary libraries
library(readr)
library(dplyr)
library(shiny)
library(ggplot2)
library(leaflet)
library(stringi)
library(readxl)
library(sf) 
library(shinydashboard)
library(scales)
library(lubridate)
library(timeDate)

###########
#### Reading Files
###########
#Directory where the CSV files are located
data_directory <- "./data/NB_FER"

#all TXT files in the specified directory
txt_files <- list.files(path = data_directory, full.names = TRUE)

print(txt_files)

###########
### Merging Files
###########

# Create an empty data frame to store the merged data
merged_data <- data.frame()
merged_data
# Loop through each TXT file and merge it into the main dataset
for (txt_file in txt_files) {
  # Read the first few lines to determine the separator
  first_lines <- readLines(txt_file, n = 5)
  possible_separators <- c("\t", ";")  
  
  # Variable to control whether to continue to the next separator
  continue_next_separator <- TRUE
  
  # Try each possible separator
  for (separator in possible_separators) {
    if (!continue_next_separator) {
      break
    }
    
    tryCatch(
      {
        year_data <- read.delim(txt_file, header = TRUE, sep = separator, stringsAsFactors = FALSE)
        
        if ("lda" %in% names(year_data)) {
          names(year_data)[names(year_data) == "lda"] <- "ID_REFA_LDA"
        }
        # Convert relevant columns to character
        year_data$ID_REFA_LDA <- as.character(year_data$ID_REFA_LDA)
        year_data$NB_VALD <- as.character(year_data$NB_VALD)
        
        # Merge data
        merged_data <- bind_rows(merged_data, year_data)
        
        # Set the variable to false to break out of the loop
        continue_next_separator <- FALSE
      },
      error = function(e) {
        # Continue to the next separator if an error occurs
        continue_next_separator <- TRUE
      }
    )
  }
}

#all TXT files in the specified directory
txt_files <- list.files(path = data_directory, full.names = TRUE)

print(txt_files)


# Directory where the PROFIL_FER .txt files are located
data_directory <- "./data/PROFIL_FER"

# Get a list of all .txt files in the specified directory
txt_files <- list.files(path = data_directory, full.names = TRUE)

print(txt_files) 

# Specify the expected column structure for PROFIL_FER files
expected_columns <- c(
  "CODE_STIF_TRNS",
  "CODE_STIF_RES",
  "CODE_STIF_ARRET",
  "LIBELLE_ARRET",
  "ID_ZDC",
  "CAT_JOUR",
  "TRNC_HORR_60",
  "pourc_validations"
)

# Create an empty data frame with the expected columns
merged_data1 <- data.frame(matrix(ncol = length(expected_columns), nrow = 0))
colnames(merged_data1) <- expected_columns

# Loop through each TXT file and merge it into the main dataset
for (txt_file in txt_files) {
  # Read the first few lines to determine the separator
  first_lines <- readLines(txt_file, n = 5)
  possible_separators <- c("\t", ";")  
  
  # Variable to control whether to continue to the next separator
  continue_next_separator <- TRUE
  
  # Try each possible separator
  for (separator in possible_separators) {
    if (!continue_next_separator) {
      break
    }
    
    tryCatch(
      {
        # Load the data
        year_data <- read.delim(txt_file, header = TRUE, sep = separator, stringsAsFactors = FALSE)
        
        # Align column names to the expected structure
        missing_cols <- setdiff(expected_columns, names(year_data))
        if (length(missing_cols) > 0) {
          year_data[missing_cols] <- NA  # Add missing columns as NA
        }
        
        year_data <- year_data[expected_columns]  # Reorder columns
        
        # Merge into the main dataset
        merged_data1 <- bind_rows(merged_data1, year_data)
        
        # Set the variable to false to break out of the loop
        continue_next_separator <- FALSE
      },
      error = function(e) {
        # Continue to the next separator if an error occurs
        continue_next_separator <- TRUE
      }
    )
  }
}

# Save the merged data to a CSV file
write.csv(merged_data1, "merged_PROFIL_FER.csv", row.names = FALSE)
merged_PROFIL_FER <- read.csv("merged_PROFIL_FER.csv",encoding = "latin1")
merged_PROFIL_FER <- subset(merged_PROFIL_FER, select = -ID_ZDC)


merged_PROFIL_FER <- merged_PROFIL_FER[!apply(is.na(merged_PROFIL_FER), 1, all), ]

merged_PROFIL_FER


#################
### Checking Nas
#################

sum(is.na(merged_data$ID_REFA_LDA))
# Initialize a list to store NA counts
na_counts <- list()

# Loop through all columns in the dataset
for (col_name in colnames(merged_data)) {
  na_counts[[col_name]] <- sum(is.na(merged_data[[col_name]]))
}

# Convert the list to a data frame for better visualization
na_summary <- data.frame(
  Column = names(na_counts),
  NA_Count = unlist(na_counts)
)

# Print the summary
print(na_summary)
count_invalid_rows <- function(data) {
  # Vérifier les colonnes spécifiées
  cols_to_check <- c("CODE_STIF_TRNS", "CODE_STIF_RES", "CODE_STIF_ARRET", "NB_VALD")
  
  # Vérifier si les colonnes existent dans le dataframe
  if (!all(cols_to_check %in% colnames(data))) {
    stop("Les colonnes spécifiées ne sont pas toutes présentes dans le dataframe.")
  }
  
  # Fonction pour détecter les caractères non numériques ou les espaces
  is_invalid <- function(column) {
    grepl("[^0-9]", column) | grepl("\\s", column) # Non numérique ou contient un espace
  }
  
  # Identifier les lignes contenant des valeurs invalides dans les colonnes spécifiées
  invalid_rows <- data %>%
    filter(if_any(all_of(cols_to_check), is_invalid))
  
  # Retourner le nombre de lignes invalides
  return(nrow(invalid_rows))
}

# Exemple d'utilisation
count_invalid_rows(merged_data)
sum(merged_data$CODE_STIF_RES == "ND ")
sum(merged_data$CODE_STIF_ARRET == "ND")
sum(merged_data$LIBELLE_ARRET == "Inconnu")
is_invalid <- function(column) {
  grepl("[^0-9]", column) | grepl("\\s", column) # Non-numeric or contains a space
}

# Specify the columns to apply the transformation
columns_to_check <- c("CODE_STIF_TRNS", "CODE_STIF_RES", "CODE_STIF_ARRET","NB_VALD")

# Apply the function only on the selected columns
merged_data[columns_to_check] <- lapply(merged_data[columns_to_check], function(col) {
  col[is_invalid(col)] <- NA
  return(col)
})
merged_data$LIBELLE_ARRET[merged_data$LIBELLE_ARRET == "Inconnu"] <- NA
# Initialize a list to store NA counts
na_counts <- list()

# Loop through all columns in the dataset
for (col_name in colnames(merged_data)) {
  na_counts[[col_name]] <- sum(is.na(merged_data[[col_name]]))
}

# Convert the list to a data frame for better visualization
na_summary <- data.frame(
  Column = names(na_counts),
  NA_Count = unlist(na_counts)
)

# Print the summary
print(na_summary)
str(merged_data)
sum(merged_data$CATEGORIE_TITRE == "?", na.rm = TRUE)
sum(merged_data$ID_REFA_LDA == "?", na.rm = TRUE)
sum(merged_data$NB_VALD == "Moins de 5", na.rm = TRUE)
merged_data$CODE_STIF_RES <- as.integer(merged_data$CODE_STIF_RES)
merged_data$CODE_STIF_TRNS <- as.integer(merged_data$CODE_STIF_TRNS)
merged_data$CODE_STIF_ARRET <- as.integer(merged_data$CODE_STIF_ARRET)
merged_data$NB_VALD <- as.integer(merged_data$NB_VALD)
# Replace "?" with NON DEFINI in the column CATEGORIE_TITRE
merged_data <- merged_data %>%mutate(CATEGORIE_TITRE = ifelse(CATEGORIE_TITRE == "?", "NON DEFINI", CATEGORIE_TITRE))
# Initialize a list to store NA counts
na_counts <- list()

# Loop through all columns in the dataset
for (col_name in colnames(merged_data)) {
  na_counts[[col_name]] <- sum(is.na(merged_data[[col_name]]))
}

# Convert the list to a data frame for better visualization
na_summary <- data.frame(
  Column = names(na_counts),
  NA_Count = unlist(na_counts)
)

# Print the summary
print(na_summary)
# Count the number of missing values in every column
missing_values <- colSums(is.na(merged_data))

# Display the result
print(missing_values)
# Delete missing values
merged_data <- na.omit(merged_data)

# Count the number of missing values in every column
missing_values <- colSums(is.na(merged_data))

# Display the result
print(missing_values)
merged_data <- merged_data[!duplicated(merged_data), ]

#################
### Checking for outliers
#################

summary(merged_data$NB_VALD)
summary(merged_data$NB_VALD)
nb <- merged_data$NB_VALD
# Sort the column
sorted_column <- sort(nb)
# Calculate the 90th percentile
percentile_90 <- quantile(sorted_column, 0.90)
# Print the result
print(paste("90th Percentile:", percentile_90))
# Calculate unique quantiles
quantiles <- unique(quantile(merged_data$NB_VALD, probs = seq(0, 1, by = 0.1)))

# Assign each record to a quantile range with range labels
merged_data$Quantile_Range <- cut(
  merged_data$NB_VALD,
  breaks = quantiles,
  include.lowest = TRUE,
  labels = paste0("[", head(quantiles, -1), " - ", tail(quantiles, -1), "]")
)

# Count the number of records and calculate percentages
quantile_distribution <- merged_data %>%
  group_by(Quantile_Range) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Plot the distribution of NB_VALD based on ranges in percentages
ggplot(quantile_distribution, aes(x = Quantile_Range, y = Percentage, fill = Quantile_Range)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Distribution of NB_VALD Based on Ranges (Percentage)",
    x = "NB_VALD Ranges",
    y = "Percentage of Records (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Aggregate the data
aggregated_data <- merged_data %>%
  group_by(JOUR, CODE_STIF_TRNS, CODE_STIF_RES, CODE_STIF_ARRET, LIBELLE_ARRET, ID_REFA_LDA) %>%
  summarise(
    AMETHYSTE = sum(NB_VALD[CATEGORIE_TITRE == "AMETHYSTE"], na.rm = TRUE),
    AUTRE_TITRE = sum(NB_VALD[CATEGORIE_TITRE == "AUTRE TITRE"], na.rm = TRUE),
    FGT = sum(NB_VALD[CATEGORIE_TITRE == "FGT"], na.rm = TRUE),
    IMAGINE_R = sum(NB_VALD[CATEGORIE_TITRE == "IMAGINE R"], na.rm = TRUE),
    NAVIGO = sum(NB_VALD[CATEGORIE_TITRE == "NAVIGO"], na.rm = TRUE),
    NAVIGO_JOUR = sum(NB_VALD[CATEGORIE_TITRE == "NAVIGO JOUR"], na.rm = TRUE),
    NON_DEFINI = sum(NB_VALD[CATEGORIE_TITRE == "NON DEFINI"], na.rm = TRUE),
    TST = sum(NB_VALD[CATEGORIE_TITRE == "TST"], na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  ) %>%
  mutate(Total_NB_VALD = rowSums(across(AMETHYSTE:TST), na.rm = TRUE))

# View the resulting data
print(aggregated_data)
new_data <- read_excel("./data/zones-d-arrets.xlsx")
head(new_data)
# Count the number of missing values in every column
missing_values1 <- colSums(is.na(new_data))

# Display the result
print(missing_values1)
# Filter the dataset to keep only 'railStation' and 'metroStation' in the 'ZdAType' column
filtered_data <- new_data %>%
  filter(ZdAType %in% c("railStation", "metroStation"))

# View the filtered data
head(filtered_data)
# Initialize a list to store NA counts
na_counts <- list()

# Loop through all columns in the dataset
for (col_name in colnames(filtered_data)) {
  na_counts[[col_name]] <- sum(is.na(filtered_data[[col_name]]))
}

# Convert the list to a data frame for better visualization
na_summary <- data.frame(
  Column = names(na_counts),
  NA_Count = unlist(na_counts)
)

# Print the summary
print(na_summary)
# Group merged_data by "ID_REFA_LDA"
final_data <- aggregated_data %>%
  inner_join(filtered_data, by = c("ID_REFA_LDA" = "ZdCId"))

# View the resulting dataset
head(final_data)
result <- final_data
# Calculate the percentage for each value in the 'ZdAType' column
percentage_summary <- result %>%
  group_by(ZdAType) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Filter to show only 'railStation' and 'metroStation'
percentage_summary_filtered <- percentage_summary %>%
  filter(ZdAType %in% c("railStation", "metroStation"))

# View the results
print(percentage_summary_filtered)