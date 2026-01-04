# ============================================================================
# Visualizing Predicted Poverty Rates
# ============================================================================
# This script creates maps to visualize our poverty predictions. We'll create
# two maps side by side:
# 1. Raw predictions (not calibrated)
# 2. Calibrated predictions (adjusted to match official statistics)

# Load Required Packages ------------------------------------------------
pacman::p_load(
  readr,        # for reading CSV files
  ggplot2,      # for creating plots
  terra,        # for handling raster data
  dplyr,        # for data manipulation
  tidyr         # for reshaping data
)

# Set Up Path and Load Data -------------------------------------------
base_path = "/Users/floramaenapoles/Downloads/data"

# Load our poverty predictions raster
# This file contains both raw and calibrated predictions
poverty_raster = rast(file.path(base_path, "idn_poverty_raster.tif"))

# Convert Raster to Data Frame ----------------------------------------
# Convert raster to data frame with coordinates
poverty_df = as.data.frame(poverty_raster, xy = TRUE)

# Reshape Data for Plotting -------------------------------------------
# Convert data from wide to long format for side-by-side plotting
poverty_df_long = poverty_df %>%
  # Gather all columns except coordinates and ID into long format
  pivot_longer(
    cols = !c(x, y, id),
    names_to = "method",
    values_to = "poverty_rate"
  ) %>%
  # Give methods more readable names
  mutate(
    method = if_else(
      method == "pov_rate",
      "Not Calibrated",   # Raw predictions
      "Calibrated"        # Adjusted predictions
    )
  )

# Create Visualization ------------------------------------------------
# Create two maps side by side using ggplot2
poverty_map = poverty_df_long %>%
  ggplot() +
  # Create raster visualization
  geom_raster(aes(x = x, y = y, fill = poverty_rate)) +
  # Create two panels (calibrated vs not calibrated)
  facet_wrap(method ~ .) +
  # Keep map proportions correct
  coord_equal() +
  # Use the 'turbo' color scheme (good for showing poverty intensity)
  scale_fill_viridis_c(
    option = "turbo",     # Color scheme
    na.value = NA         # Make NA values transparent
  ) +
  # Remove unnecessary grid lines and axes
  theme_void() +
  # Add titles and customize appearance
  labs(
    fill = "Poverty Rate",
    title = "Predicted Poverty Rates"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    strip.text.x = element_text(size = 15)
  )

# Display the map
print(poverty_map)

# Note: The 'turbo' color scheme is used because it:
# 1. Is colorblind-friendly
# 2. Shows clear progression from low to high values
# 3. Makes it easy to spot patterns and clusters