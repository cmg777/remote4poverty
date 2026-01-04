# ============================================================================
# Estimating and Predicting Grid-Level Poverty
# ============================================================================
# This script is the final step in our poverty mapping process. We will:
# 1. Train a Random Forest model using our CNN features
# 2. Predict poverty rates for each grid cell
# 3. Calibrate our predictions using official poverty statistics

# Load Required Packages ------------------------------------------------
pacman::p_load(
  ranger,        # for Random Forest implementation
  tuneRanger,    # for tuning Random Forest parameters
  readr,         # for reading CSV files
  readxl,        # for reading Excel files
  sf,           # for spatial data handling
  arrow,        # for reading Feather files (efficient data storage)
  terra,        # for raster operations
  dplyr,        # for data manipulation
  tidyr,        # for data cleaning
  ggplot2       # for plotting
)

# Set Up Paths and Load Data -------------------------------------------
base_path = "/Users/floramaenapoles/Downloads/data"

# Load three key datasets:
# 1. Our grid information
grid_data = read_csv(file.path(base_path, "grids", "idn_grid_data.csv"))

# 2. Official poverty statistics at admin4 level
pov_data = read_rds(file.path(base_path, "poverty", "idn_adm4_pov.rds"))

# 3. CNN features extracted from satellite imagery
feature_data = read_feather(file.path(base_path, 
                                      "extracted_features", "features_indonesia.feather"))

# Prepare Data for Modeling --------------------------------------------
# Format filenames to match our CNN output format
grid_data = grid_data %>%
  mutate(file_name = sprintf("%06d.jpg", id))

# Give our CNN features meaningful names (feature_0 through feature_511)
colnames(feature_data)[1:(ncol(feature_data)-1)] = 
  paste0("feature_", colnames(feature_data)[1:(ncol(feature_data)-1)])

# Combine grid information with CNN features
grid_features_combined = merge(grid_data, feature_data, by = "file_name")

# Calculate average features for each administrative region
grid_features_aggregated_mean = grid_features_combined %>%
  select(adm4_code, feature_0:feature_511) %>%
  group_by(adm4_code) %>%
  summarize_all(mean)

# Prepare poverty data and combine with features
pov_data = pov_data %>%
  select(adm4_code, pov_rate_2019) %>%
  rename(pov_rate = pov_rate_2019) %>%
  mutate(adm4_code = paste0("ID", adm4_code))

# Merge poverty data with aggregated features
grid_features_aggregated_mean_pov = merge(grid_features_aggregated_mean, 
                                          pov_data, 
                                          by = "adm4_code")

# Organize columns and remove any rows with missing values
grid_features_aggregated_mean_pov = grid_features_aggregated_mean_pov %>%
  relocate(pov_rate)

grid_features_aggregated_mean_pov_no_na = grid_features_aggregated_mean_pov %>%
  select(-adm4_code) %>%
  drop_na()

# Train Random Forest Model --------------------------------------------
# Use tuneMtryFast to find optimal parameters and train the model
# This step might take ~10 minutes
rf_model = tuneMtryFast(
  data = grid_features_aggregated_mean_pov_no_na,
  dependent.variable.name = "pov_rate",
  mtryStart = floor(length(grid_features_aggregated_mean_pov_no_na) / 3),
  stepFactor = 1.2,
  improve = 0.01,
  trace = TRUE,
  doBest = TRUE,
  importance = "impurity",
  replace = TRUE,
  seed = 123  # for reproducibility
)

# Examine model results
print(rf_model)

# Create variable importance plot
var_importance = importance(rf_model)
var_importance_df = data.frame(Variable = names(var_importance), 
                               Importance = var_importance)
var_importance_df = var_importance_df[order(var_importance_df$Importance, 
                                            decreasing = TRUE), ]

# Plot top 30 most important features
variable_importance_plot = var_importance_df %>%
  slice_head(n = 30) %>%
  ggplot(data = ., aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", 
       y = "Importance", 
       title = "Top 30 Most Important Features")

print(variable_importance_plot)

# Make Grid-Level Predictions ------------------------------------------
# Prepare data for predictions
grid_features_for_projections = grid_features_combined %>%
  select(id, X, Y, adm4_code, feature_0:feature_511)

grid_features_for_projections_no_na = grid_features_for_projections %>%
  drop_na()

# Make predictions using our trained model
y_grid_rf = predict(rf_model, 
                    grid_features_for_projections_no_na[, 5:ncol(grid_features_for_projections_no_na)])

# Check prediction summary
summary(y_grid_rf$predictions)

# Combine predictions with grid identifiers
poverty_map = bind_cols(grid_features_for_projections_no_na, 
                        y_grid_rf$predictions)

poverty_map = poverty_map %>%
  select(id, ...517) %>%
  rename(pov_rate = ...517)

# Calibrate Predictions -----------------------------------------------
# This step ensures our predictions match official statistics when aggregated
  
  # Create base grid-level dataset
  grid_level_base = grid_features_for_projections %>%
    select(id, X, Y, adm4_code)
  
  grid_level_all = merge(grid_level_base, poverty_map, by = "id", all.x = TRUE)
  
  grid_level_all_raster = grid_level_all %>%
    select(X, Y, id, pov_rate)
  
  # Get population data for weighting
  grid_level_pop = grid_data %>%
    select(id, population_2020, adm4_code)
  
  # Combine predictions with population data
  grid_level_all_raster_calibrated = merge(grid_level_all_raster, 
                                           grid_level_pop, 
                                           by = "id")
  
  # Calculate admin-level estimates
  adm4_pov_estimate = grid_level_all_raster_calibrated %>%
    mutate(pov = pov_rate * population_2020) %>%
    group_by(adm4_code) %>%
    summarize(population = sum(population_2020, na.rm = TRUE),
              pov = sum(pov, na.rm = TRUE))
  
  # Calculate scaling factors
  adm4_pov_estimate_merged = merge(adm4_pov_estimate, pov_data, 
                                   by = "adm4_code")
  
  adm4_pov_scaling_factors = adm4_pov_estimate_merged %>%
    mutate(pov_total = pov_rate * population) %>%
    mutate(pov_scaling = pov_total / pov) %>%
    select(adm4_code, pov_scaling)
  
  # Apply scaling factors and clean up results
  grid_level_all_raster_calibrated = merge(grid_level_all_raster_calibrated, 
                                           adm4_pov_scaling_factors,
                                           by = "adm4_code")
  
  grid_level_all_raster_calibrated = grid_level_all_raster_calibrated %>%
    mutate(pov_rate_calibrated = if_else(!is.na(pov_scaling) & 
                                           !is.infinite(pov_scaling),
                                         pov_rate * pov_scaling,
                                         pov_rate)) %>%
    mutate(pov_rate_calibrated = if_else(pov_rate_calibrated > 1, 
                                         1, 
                                         pov_rate_calibrated)) %>%
    select(X, Y, id, pov_rate, pov_rate_calibrated)
  
  # Create and Save Final Raster -----------------------------------------
  # Convert our results to a raster format
  poverty_raster = rast(grid_level_all_raster_calibrated, 
                        type = "xyz", 
                        crs = "epsg:4326")
  
  # Save the final poverty map
  writeRaster(poverty_raster, 
              file.path(base_path, "idn_poverty_raster.tif"), 
              overwrite = TRUE, 
              datatype = "FLT8S")