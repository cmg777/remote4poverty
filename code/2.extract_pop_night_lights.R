# ============================================================================
# Extracting Population and Night Light Data for Geographic Grids
# ============================================================================
# This script combines our geographic grids with two key datasets:
# 1. Night light intensity from VIIRS (satellite data)
# 2. Population density from WorldPop
# Both datasets are from 2019.

# Load Required Packages ---------------------------------------------------
# We use pacman for easy package management
pacman::p_load(
  terra,     # for handling all our spatial raster operations
  dplyr,     # for data manipulation
  readr      # for reading/writing CSV files
)

# Set Up Paths and Read Input Files ---------------------------------------
base_path = "/Users/floramaenapoles/Downloads/data"

# Read our three main input files:
# 1. Our grid system (2,560 km resolution)
indonesia_grid_raster = rast(file.path(base_path, "grids", "indonesia_grid_raster.tif"))

# 2. Night light data (original resolution ~500 meters)
nl_2019 = rast(file.path(base_path, "night_lights", "VNL_v21_npp_2019_idn.tif"))

# 3. Population data (original resolution ~100 meters)
pop_2019 = rast(file.path(base_path, "population_density", "idn_ppp_2019_UNadj.tif"))

# Align Spatial Extents -------------------------------------------------
# Crop both datasets to our area of interest (East Java)
# This speeds up processing by removing unnecessary areas
nl_idn = crop(nl_2019, indonesia_grid_raster)
pop_idn = crop(pop_2019, indonesia_grid_raster)

# Handle Resolution Differences -----------------------------------------
# Our data has three different resolutions:
# - Population: ~100 meters
# - Night lights: ~500 meters
# - Our grids: 2,560 meters
#
# We need to bring everything to our grid resolution in two steps:
# Step 1: Aggregate (combine) smaller cells into larger ones

# For night lights: take the mean value of all cells
# Factor of 5 brings 500m → ~2,500m (close to our 2,560m)
nl_idn = aggregate(nl_idn, fact = 5, fun = "mean", na.rm = TRUE)

# For population: sum all people in each larger cell
# Factor of 24 brings 100m → ~2,400m (close to our 2,560m)
pop_idn = aggregate(pop_idn, fact = 24, fun = "sum", na.rm = TRUE)

# Step 2: Resample to exactly match our grid system
# This ensures all datasets align perfectly
nl_idn = resample(nl_idn, indonesia_grid_raster, method = "bilinear")
pop_idn = resample(pop_idn, indonesia_grid_raster, method = "bilinear")

# Combine Datasets ----------------------------------------------------
# Stack all three rasters (they now have identical grid sizes)
nl_pop_stack = c(indonesia_grid_raster, nl_idn, pop_idn)

# Convert to a data frame for easier analysis
nl_pop_df = as.data.frame(nl_pop_stack)

# Clean Up Results --------------------------------------------------
# Remove grid cells that aren't in our area of interest
# (these have NA for grid ID, typically ocean areas)
nl_pop_df = nl_pop_df %>%
  filter(!is.na(id))

# Give our columns clearer names
nl_pop_df = nl_pop_df %>%
  rename(
    nl_value = VNL_v21_npp_2019_global_vcmslcfg_c202205302300.average.dat,
    population = idn_ppp_2019_UNadj
  )

# Save Final Results ------------------------------------------------
# Save as CSV for use in subsequent analysis steps
write_csv(nl_pop_df, file.path(base_path, "grids", "idn_nl_pop.csv"))
