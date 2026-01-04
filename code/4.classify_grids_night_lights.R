# ============================================================================
# Classifying Geographic Grids Using Night Light Values
# ============================================================================
# This script classifies our geographic grids into three categories (low, medium,
# and high) based on their night light intensity. This classification will be
# used to train a Convolutional Neural Network (CNN) that can predict night light
# intensity from satellite imagery.

# Load Required Packages ---------------------------------------------------
# We use pacman for easy package management
pacman::p_load(
  mclust,    # for Gaussian mixture modeling
  readr,     # for reading/writing CSV files
  dplyr,     # for data manipulation
  tidyr      # for data cleaning
)

# Set Up File Paths ------------------------------------------------------
base_path = "/Users/floramaenapoles/Downloads/data"
grids_path = file.path(base_path, "grids")

# Read and Merge Input Data ---------------------------------------------
# Load our two input datasets:
# 1. Geographic grid information with coordinates and admin codes
indonesia_grid = read_csv(file.path(grids_path, "indonesia_grid.csv"))

# 2. Night light and population values for each grid
nl_pop_values = read_csv(file.path(grids_path, "idn_nl_pop.csv"))

# Merge the datasets using the grid ID
indonesia_grid_all = indonesia_grid %>%
  left_join(nl_pop_values, by = "id")

# Verify our merged dataset has all expected columns
cat("Columns in merged dataset:\n")
print(colnames(indonesia_grid_all))

# Clean Night Light Values ---------------------------------------------
# First, let's look at our data
cat("\nNumber of grids:", nrow(indonesia_grid_all), "\n")
cat("\nSummary of night light values:\n")
print(summary(indonesia_grid_all$nl_value))

# Check for negative night light values
# (these can occur due to sensor noise but aren't meaningful)
negative_values = length(which(indonesia_grid_all$nl_value < 0))

# If we found negative values, replace them with the smallest non-negative value
# Note: We don't use zero because it can cause issues with the classification
if (negative_values > 0) {
  # Find the smallest non-negative value
  min_value = indonesia_grid_all %>% 
    filter(nl_value >= 0) %>% 
    select(nl_value) %>% 
    min()
  
  # Replace negative values
  indonesia_grid_all = indonesia_grid_all %>%
    mutate(nl_value = if_else(nl_value <= 0, min_value, nl_value))
}

# Classify Grids Using Gaussian Mixture Model ----------------------------
# Extract just the night light values for classification
indonesia_nl_values = indonesia_grid_all$nl_value

# Use Mclust to fit a Gaussian mixture model with 3 components
# Parameters:
# - G = 3: we want three clusters (low, medium, high)
# - modelName = "V": allows for different variances in each cluster
#   (this makes sense as urban areas might be more variable than rural ones)
fit = Mclust(indonesia_nl_values, G=3, modelName="V")

# Look at the model results
cat("\nModel Summary:\n")
print(summary(fit))

# Add Classifications to Our Data --------------------------------------
# Add the cluster assignments (1, 2, or 3) to our main dataset
indonesia_grid_all = indonesia_grid_all %>%
  mutate(nl_class = fit$classification)

# Examine our classification results
cat("\nClassification Summary:\n")
print(indonesia_grid_all %>%
        group_by(nl_class) %>%
        summarize(
          min_nl = min(nl_value),
          max_nl = max(nl_value),
          number_of_grids = n()
        ))

# Save Results --------------------------------------------------------
# Save the classified grids for use in training our CNN
write_csv(indonesia_grid_all, 
          file.path(grids_path, "indonesia_grids_classified.csv"))

# Note on Regional Codes ----------------------------------------------
# For reference, Indonesian province codes used in admin codes:
# - ID35: East Java (Jawa Timur)
# - ID33: Central Java (Jawa Tengah)
# - ID32: West Java (Jawa Barat)
