################################################################
### This is the R script accompanying the blog post on Data for Public Health
### Accessible at https://dataforpublichealth.com/post-details/chi-square-tests-brfss-weights
################################################################


################################################################
### PART 1: Chi-Square Test for Asthma Reporting Over Time
################################################################

# Load necessary libraries
library(dplyr)         # For data manipulation
library(janitor)       # For cleaning and summarizing data
library(gt)            # For good-looking tables
library(ggplot2)       # For visualization

# ------------------- 1. Load the Data -------------------
# Load the dataframe (grab from https://dataforpublichealth.com/post-details/wrangling-brfss-2011-2023)
data <- read.csv("D:/d4ph/brfss/analysis/data/brfss_36_2019to2023.csv") # Update path as needed

# Load the RDS file containing mappings
label_map <- readRDS("D:/d4ph/brfss/analysis/data/BRFSS_label_map_2011-2023.rds")

# ------------------- 2. Inspect Data -------------------
# View column names and structure
print("Structure of Data:")
str(data)

# Focus on ASTHNOW (asthma status) and year (time)
data_selected <- data %>%
# Filter for non-missing values in ASTHNOW and year columns
    filter(!is.na(ASTHNOW), !is.na(year)) %>%
# Filter for age group 18-49 years and ASTHNOW values 1 or 2
    filter(x_AGEG5YR < 7, ASTHMA3 %in% c(1, 2)) %>% 
    select(year, x_AGEG5YR, ASTHNOW)

# ------------------- 3. Map Labels -------------------
# If mapping exists, replace codes with descriptive labels
# Assuming `label_map` is a list with variable-value mappings
asthma_labels <- label_map[["ASTHNOW"]]

data_selected <- data_selected %>%
  mutate(ASTHNOW = factor(ASTHNOW, levels = names(asthma_labels), labels = asthma_labels))

# ------------------- 4. Prepare the Contingency Table -------------------
# Create a table of ASTHNOW responses by YEAR
asthma_table <- data_selected %>%
  tabyl(year, ASTHNOW) %>%
  adorn_totals(c("row", "col"))  # Add totals

# Remove "Don't know/Not sure" and "Refused" columns from table (for demonstration purposes)
asthma_table <- asthma_table[, -c(4, 5)]

print("Contingency Table of ASTHNOW Over Years:")
print(asthma_table)

# ------------------- 5. Perform the Chi-Square Test -------------------
# Prepare a function to run chi-square test for each combination of years
run_chi_square <- function(data, year1, year2) {
  subset_data <- data %>% filter(year %in% c(year1, year2))
  chi_table <- table(subset_data$year, subset_data$ASTHNOW)
  chisq.test(chi_table)
}

# Get unique years
years <- unique(data_selected$year)

# Initialize a list to store results
chi_results <- list()

# Run chi-square tests for each combination of years
for (i in 1:(length(years) - 1)) {
  for (j in (i + 1):length(years)) {
    year1 <- years[i]
    year2 <- years[j]
    chi_result <- run_chi_square(data_selected, year1, year2)
    chi_results[[paste(year1, "vs", year2)]] <- chi_result
  }
}

# Output Chi-Square test results
chi_results_table <- tibble::tibble(
  Comparison = names(chi_results),
  p_value = sapply(chi_results, function(x) x$p.value),
  statistic = sapply(chi_results, function(x) x$statistic)
)

# If p values are < 0.001, set them to 0.001 without trailing 0s (for readability)
chi_results_table$p_value <- ifelse(chi_results_table$p_value < 0.001,
                                     "p < 0.001",
                                     formatC(chi_results_table$p_value, format = "f", digits = 3))

print("Chi-Square Test Results for Each Combination of Years:")
#print(chi_results_table)
gt(chi_results_table)


# ------------------- 6. Format Results in Table -------------------
# Display contingency table and Chi-Square results using gt
asthma_gt <- gt(asthma_table) %>%
  tab_header(
    title = "Contingency Table: ASTHNOW Over Years",
    subtitle = "Chi-Square Test for Change in Asthma Reporting Over Time"
  )

print(asthma_gt)

# ------------------- 7. Visualization (Optional) -------------------
# Plot asthma proportions over years (only "Yes" and "No" responses)
asthma_plot <- data_selected %>%
    filter(ASTHNOW %in% c("Yes", "No")) %>%
    group_by(year, ASTHNOW) %>%
    summarize(count = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(proportion = count / sum(count)) %>%
    ggplot(aes(x = factor(year), y = proportion, fill = ASTHNOW)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
        title = "Proportion of ASTHNOW Responses Over Years",
        x = "Year",
        y = "Proportion",
        fill = "Asthma Status"
    ) +
    theme_minimal()

print(asthma_plot)

################################################################
### PART 2 (using the same imports from Part 1)
################################################################

# Import base BRFSS data for 2019 and 2023
brfss_2019 <- read.csv("D:/d4ph/brfss/analysis/data/2019_342.csv")
brfss_2023 <- read.csv("D:/d4ph/brfss/analysis/data/2023_350.csv")

# Only keep the columns of interest (GENHLTH, x_LLCPWT)
brfss_2019 <- brfss_2019[, c("GENHLTH", "x_LLCPWT")]
brfss_2023 <- brfss_2023[, c("GENHLTH", "x_LLCPWT")]

# Add a column for year
brfss_2019$year <- 2019
brfss_2023$year <- 2023

# Combine the datasets
data <- rbind(brfss_2019, brfss_2023)

# Map the GENHLTH values to labels
genhlth_labels <- label_map[["GENHLTH"]]
data$GENHLTH <- factor(data$GENHLTH, levels = names(genhlth_labels), labels = genhlth_labels)

# Create a contingency table of GENHLTH responses by year
genhlth_table <- data %>%
  tabyl(year, GENHLTH) %>%
  adorn_totals(c("row", "col"))

# Print the contingency table
gt(genhlth_table) %>%
    fmt_number(
        columns = everything(),
        decimals = 0,
        use_seps = TRUE
    )

### Recreate table using weights instead of counted respondent counts ### 
# Load required libraries
library(tidyverse)
library(srvyr)

# Combine the datasets
data <- rbind(brfss_2019, brfss_2023)

# Map the GENHLTH values to labels
genhlth_labels <- label_map[["GENHLTH"]]
data$GENHLTH <- factor(data$GENHLTH, levels = names(genhlth_labels), labels = genhlth_labels)

# Step 1: Filter valid GENHLTH responses
data_clean <- data %>%
  filter(GENHLTH %in% c("Excellent", "Very good", "Good", "Fair", "Poor")) 

# Step 2: Convert to survey design
data_svy <- data_clean %>%
  as_survey_design(weights = x_LLCPWT)

# Step 3: Summarize only the weighted sums
genhlth_table_wt <- data_svy %>%
  group_by(GENHLTH, year) %>%
  summarise(weighted_sum = survey_total(vartype = NULL), .groups = "drop") %>% # Drop SE calculation
  pivot_wider(
    names_from = year,
    values_from = weighted_sum,
    values_fill = 0  # Fill blanks with 0
  ) 

# Step 4: Format the table
genhlth_table_wt %>%
  gt() %>%
  cols_label(
    GENHLTH = "General Health",
    `2019` = "2019 Weighted Sum",
    `2023` = "2023 Weighted Sum"
  ) %>%
  fmt_number(
    columns = c(`2019`, `2023`),
    decimals = 0
  ) %>%
  tab_header(
    title = "Weighted Sums of General Health Responses",
    subtitle = "Comparison of 2019 and 2023 BRFSS Data"
  )

### Manual calculation of chi-square test for weighted data (using the weighted sums)
# Create a contingency table with weighted sums
weighted_sums <- matrix(
  c(43798579, 40236000,  # Excellent
    79226332, 78790995,  # Very good
    81467949, 85239202,  # Good
    35556252, 37778702,  # Fair
    11811356, 11291592), # Poor
  nrow = 5, byrow = TRUE,
  dimnames = list(
    c("Excellent", "Very good", "Good", "Fair", "Poor"), # Rows
    c("2019", "2023")                                     # Columns
  )
)

# Print the contingency table
print("Contingency Table of Weighted Sums:")
print(weighted_sums)

# Conduct the Chi-square test
chi_square_test <- chisq.test(weighted_sums)

# Print the test results
print("Chi-square Test Results:")
print(chi_square_test)

### Programmatically Conducting our Chi-Square Test
# Step 1: Extract the weighted sums
# Assuming genhlth_table_wt is your tibble with weighted sums
weighted_sums_table <- genhlth_table_wt %>%
  select(GENHLTH, `2019`, `2023`) %>%  # Keep only relevant columns
  column_to_rownames("GENHLTH") %>%    # Use GENHLTH as row names
  as.matrix()                         # Convert to matrix format

# Step 2: Print the contingency table
print("Contingency Table of Weighted Sums:")
print(weighted_sums_table)

# Step 3: Perform the Chi-square test
chi_square_test <- chisq.test(weighted_sums_table)

# Step 4: Print the results
print("Chi-square Test Results:")
print(chi_square_test)

### POST HOC TEST (Bonferroni-adjusted Pairwise Proportion) to see where the difference(s) are coming from

## Manual Calculation
# Step 1: Create contingency table
weighted_sums <- matrix(
  c(43798579, 40236000,  # Excellent
    79226332, 78790995,  # Very good
    81467949, 85239202,  # Good
    35556252, 37778702,  # Fair
    11811356, 11291592), # Poor
  nrow = 5, byrow = TRUE,
  dimnames = list(
    c("Excellent", "Very good", "Good", "Fair", "Poor"),  # General Health
    c("2019", "2023")                                      # Years
  )
)

# Step 2: Total counts for each year
total_2019 <- sum(weighted_sums[, "2019"])
total_2023 <- sum(weighted_sums[, "2023"])

# Step 3: Run pairwise proportion tests for each General Health group
pairwise_pvals <- data.frame(
  General_Health = rownames(weighted_sums),
  p_value = sapply(1:nrow(weighted_sums), function(i) {
    if (weighted_sums[i, "2019"] > 0 & weighted_sums[i, "2023"] > 0) {  # Ensure valid counts
      prop.test(
        x = c(weighted_sums[i, "2019"], weighted_sums[i, "2023"]),
        n = c(total_2019, total_2023),
        correct = FALSE
      )$p.value
    } else {
      NA  # Return NA if data is insufficient
    }
  })
)

# Step 4: Adjust p-values for multiple comparisons
pairwise_pvals$p_adjusted <- p.adjust(pairwise_pvals$p_value, method = "bonferroni")

# Step 5: Display the results in a clean table
library(gt)
pairwise_pvals %>%
  gt() %>%
  fmt_number(columns = c(p_value, p_adjusted), decimals = 4) %>%
  tab_header(
    title = "Post-hoc Pairwise Proportion Tests",
    subtitle = "Bonferroni-Adjusted p-values for General Health Between 2019 and 2023"
  ) %>%
  data_color(
    columns = p_adjusted,
    fn = scales::col_numeric(
      palette = c("lightblue", "white", "red"),
      domain = c(0, 0.05)
    )
  )


## Programmatically (using the prior chi-square test results)
# Step 1: Extract observed values from the contingency table
observed_counts <- weighted_sums_table

# Step 2: Calculate row totals and column totals
row_totals <- rowSums(observed_counts)
col_totals <- colSums(observed_counts)
grand_total <- sum(observed_counts)

# Step 3: Calculate expected values manually
expected_values <- outer(row_totals, col_totals) / grand_total

# Step 4: Perform pairwise comparisons using prop.test
# Prepare an empty data frame to store the results
pairwise_results <- data.frame(
  Comparison = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Step 5: Loop through row pairs for pairwise tests
row_names <- rownames(observed_counts)
for (i in 1:(nrow(observed_counts) - 1)) {
  for (j in (i + 1):nrow(observed_counts)) {
    # Extract counts for the two groups being compared
    group1 <- observed_counts[i, ]
    group2 <- observed_counts[j, ]
    
    # Combine observed counts for pairwise test
    test_result <- prop.test(
      x = c(group1[1], group2[1], group1[2], group2[2]),  # Counts for 2019 and 2023
      n = c(sum(group1), sum(group2), sum(group1), sum(group2)),  # Totals
      correct = FALSE
    )
    
    # Store the p-value
    pairwise_results <- rbind(pairwise_results, data.frame(
      Comparison = paste(row_names[i], "vs", row_names[j]),
      p_value = test_result$p.value
    ))
  }
}

# Step 6: Adjust p-values for multiple comparisons using Bonferroni correction
pairwise_results$p_adjusted <- p.adjust(pairwise_results$p_value, method = "bonferroni")

# Step 7: Display the results
library(gt)
pairwise_results %>%
  gt() %>%
  fmt_number(columns = c(p_value, p_adjusted), decimals = 4) %>%
  tab_header(
    title = "Post-hoc Pairwise Comparisons (Bonferroni Adjusted)",
    subtitle = "Statistical Differences Between General Health Categories"
  ) %>%
  data_color(
    columns = p_adjusted,
    fn = scales::col_numeric(
      palette = c("lightblue", "white", "red"),
      domain = c(0, 0.05)
    )
  )

