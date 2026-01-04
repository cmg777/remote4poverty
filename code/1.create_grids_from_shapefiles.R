# ============================================================================
# Creating Geographic Grids from Shapefiles
# ============================================================================
# This script demonstrates how to create standardized geographic grids from 
# administrative boundary data. We'll create grids with a resolution of 2,560 
# kilometers (2,560 meters).

# Load Required Packages ---------------------------------------------------
# We use pacman for easy package management. It will install packages if needed,
# then load them.
pacman::p_load(
  sf,        # for handling spatial vector data (like shapefiles)
  terra,     # for handling spatial raster data (like our grids)
  dplyr,     # for data manipulation
  readr      # for reading/writing CSV files
)

# Set Up File Paths ------------------------------------------------------
# Define where our input and output files will be stored
base_path = "/Users/floramaenapoles/Downloads/data"
shapefiles_path = file.path(base_path, "shapefiles")
grids_path = file.path(base_path, "grids")

# Define Coordinate Systems ----------------------------------------------
# We'll work with two coordinate systems:
# 1. WGS 84 (World Geodetic System 1984) - our base coordinate system
#    This is the standard system used for GPS and web mapping
base_crs = 4326                                # EPSG code for WGS 84
base_crs_raster = paste0("EPSG:", base_crs)   # Format needed for raster creation

# 2. UTM (Universal Transverse Mercator) - for accurate distance measurements
#    UTM divides the world into zones and uses meters as units
#    For Indonesia (East Java), we use zone 49S (EPSG: 23846)
indonesia_utm_crs = 23846

# Set Grid Resolution ---------------------------------------------------
# Define the size of each grid cell in meters
# Here we use 2,560 kilometers = 2,560 meters
resolution_meters = 2560

# Read Input Data ------------------------------------------------------
# Load our shapefile (administrative boundaries)
# The sf package reads this into a 'simple features' object
indonesia_sf = st_read(file.path(shapefiles_path, "indonesia_east_java_adm4_clean.shp"))

# Create Geographic Grids ----------------------------------------------
# Step 1: Convert shapefile to UTM projection for accurate measurements
indonesia_sf_utm = st_transform(indonesia_sf, crs = indonesia_utm_crs)

# Step 2: Create a template raster (grid) in UTM coordinates
#         This ensures our grid cells are exactly resolution_meters × resolution_meters
template_raster = rast(ext(indonesia_sf_utm), 
                       resolution = resolution_meters,
                       crs = paste0("EPSG:", indonesia_utm_crs))

# Step 3: Project our template back to WGS 84 for global compatibility
indonesia_raster = project(template_raster, base_crs_raster)

# Step 4: Prepare the administrative codes for our grids
#         Convert admin codes to numeric format for the raster
indonesia_sf = indonesia_sf %>%
  mutate(adm4_code = as.numeric(adm4_code))

# Step 5: Convert shapefile to vector format for terra package
indonesia_sf_vect = vect(indonesia_sf)

# Step 6: Fill our raster with administrative codes
#         Each grid cell gets the code of the admin area it falls in
indonesia_raster = rasterize(indonesia_sf_vect, indonesia_raster, field = "adm4_code")

# Save Initial Results -------------------------------------------------
# Save our base raster with administrative codes
writeRaster(indonesia_raster, 
            file.path(grids_path, "indonesia_raster_template.tif"), 
            overwrite = TRUE)

# Create Sequential Grid IDs -------------------------------------------
# Create a version of our raster where each cell gets a unique number
indonesia_grid_raster = indonesia_raster
values(indonesia_grid_raster)[!is.na(values(indonesia_grid_raster))] = 
  1:sum(!is.na(values(indonesia_raster)))
names(indonesia_grid_raster) = "id"

# Save the sequentially numbered grid
writeRaster(indonesia_grid_raster, 
            file.path(grids_path, "indonesia_grid_raster.tif"), 
            overwrite = TRUE)

# Create Point Data for Each Grid -------------------------------------
# Convert our raster to a dataframe with coordinates
indonesia_grid = as.data.frame(indonesia_raster, xy = TRUE)

# Keep only grid cells that fall within Indonesia
# (remove cells with NA admin codes, which are outside our area of interest)
indonesia_grid = indonesia_grid[!is.na(indonesia_grid$adm4_code),]

# Convert to spatial points with WGS 84 coordinates
indonesia_grid_sf = st_as_sf(indonesia_grid, 
                             coords = c("x", "y"), 
                             crs = base_crs)

# Prepare Final Output -----------------------------------------------
# Combine coordinates with administrative codes
indonesia_grid = as_tibble(cbind(st_coordinates(indonesia_grid_sf), 
                                 admin_code = indonesia_grid_sf$adm4_code))  # Named column in cbind

# Create final dataset with unique IDs and formatted admin codes
indonesia_grid = indonesia_grid %>%
  mutate(id = row_number(),                            # Add sequential ID
         adm4_code = paste0("ID", admin_code)) %>%     # Format admin code
  select(-admin_code) %>%                              # Remove temporary column
  relocate(id)                                         # Move ID to first column

# Save Final Outputs ------------------------------------------------
# Save as CSV for further analysis
write_csv(indonesia_grid, file.path(grids_path, "indonesia_grid.csv"))

# Save as shapefile for GIS applications (like Google Earth Engine)
indonesia_grid_sf = st_as_sf(indonesia_grid, 
                             coords = c("X", "Y"), 
                             crs = base_crs)
st_write(indonesia_grid_sf, 
         file.path(grids_path, "indonesia_grid.shp"), 
         delete_layer = TRUE)
