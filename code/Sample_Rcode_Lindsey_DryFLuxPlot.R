# Load necessary libraries
library(terra)
library(tidyverse)
library(raster)
library(sf)

# Define paths to files - Remember to update these paths according to your local setup

setwd("C:/Users/Asus PC/OneDrive - University of Arizona/Desktop/DRYFLUX")

pathtoDryFluxGPP_SingleMonth <- "./data/DRYFLUX_Outputs_MB/DF_X2000Apr2000.tif"  # Single month GPP file
pathtoDryFluxGPP_YearFolder <- "./data/DRYFLUX_Outputs_MB/DryFlux2000"  # Folder with all monthly GPP files for a year
pathtoDrylandMask <- "./data/spatial/Drylands_dataset_2007/drylands_UNCCD_CBD_july2014.shp"  # Dryland mask file - it says 2014 but it's used for all years

#Examples for my system
#pathtoDryFluxGPP_SingleMonth <- "/.../DryFlux_spatial/DryFlux zip/DryFlux2001/DF_X2001Jan2001.tif"  
#pathtoDryFluxGPP_YearFolder <- "/.../DryFlux_spatial/DryFlux zip/DryFlux2001" 
#pathtoDrylandMask <- "/.../DryFlux_spatial/DrylandsMask/drylands_UNCCD_CBD_july2014.shp"

# If you just want to plot a single month
# Read the GPP raster file and the dryland mask shapefile
gppRaster <- rast(pathtoDryFluxGPP_SingleMonth)
drylandMask <- vect(pathtoDrylandMask)

# Apply the mask and plot
# This operation will overlay the dryland mask on the GPP raster
# and mask out (set to NA) the values of GPP outside the drylands
gppDrylandMasked <- mask(gppRaster, drylandMask)

# Plotting the masked GPP data
plot(gppDrylandMasked, main = "Dryland GPP - Apr 2000")

#add color customization if you want
plot(gppDrylandMasked, main = "Dryland GPP - Apr 2000", col= rev(topo.colors(50)))

# If needed, adjust plotting parameters according to your preferences or requirements

# For plotting all the files for a given year

# List all TIF files in the folder
allFiles <- list.files(pathtoDryFluxGPP_YearFolder, pattern = "DF_X\\d{4}[A-Za-z]{3}\\d{4}\\.tif$", full.names = TRUE)

# Create a data frame with file paths and extract year and month
filesDF <- tibble(filePath = allFiles) %>%
  mutate(fileName = basename(filePath),
         year = str_extract(fileName, "\\d{4}"),
         month = str_extract(fileName, "[A-Za-z]{3}"),
         date = make_date(year, match(month, month.abb), 1)) %>%
  arrange(date)

# Adjust plot layout to fit all monthly plots nicely

par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))  # Adjust as needed for your display

# Loop through the sorted files, mask, and plot each
for (i in 1:nrow(filesDF)) {
  # Read the current GPP raster file
  gppRaster <- rast(filesDF$filePath[i])
  
  # Apply the dryland mask (ensure drylandMask is defined elsewhere in your script)
  gppDrylandMasked <- mask(gppRaster, drylandMask)
  
  # Plotting the masked GPP data with a dynamic title including year and month
  plotTitle <- sprintf("Dryland GPP - %s %s",filesDF$month[i], filesDF$year[i])
  plot(gppDrylandMasked, main = plotTitle)
  
  # Optional: Save plot to file
  # ggsave(filename = sprintf("GPP_Plot_%s_%s.png", filesDF$year[i], month.abb[month(gppDrylandMasked)]), plot = last_plot())
}

#===============AVG GPP per pixel using all raster outputs provided=============
pathtoDryFluxGPP_AvgOutputFolder <- "./data/DRYFLUX_Avg_Outputs"  # Folder to store outputs from the code below that averages monthly and yearly dryflux outputs
pathtoDryFluxGPP_YearFolder <- "./data/DRYFLUX_Outputs_MB"  # Folder with all annual folders

# Making a list of folders in DF output folder. List should be as long as the number of years of annual data (currently 17)
annual_files <- list.dirs(pathtoDryFluxGPP_YearFolder, full.names = TRUE, recursive = FALSE)

# Loop through each year folder to create annual averages and store them in the avg output folder
for (i in annual_files) {
  # Get year # from each folder name
  year <- basename(i)
  
  # Get list of monthly rasters in folder for each year; create a list to store rasters for averaging 
  monthly_rasters <- list.files(i, pattern = "DF_X\\d{4}[A-Za-z]{3}\\d{4}\\.tif$", full.names = TRUE)
  rast_for_avg <- list()
  
  # Loop through each month in the annual folder
  for (i in monthly_rasters) {
    # Read data in each raster and add to list (rast_for_avg) created above
    rast_data <- rast(i)
    rast_for_avg[[length(rast_for_avg) + 1]] <- rast_data
  }
  
  # Create raster stack and compute avg per pixel 
  stacked_rast <- rast(rast_for_avg)
  avg_rast <- mean(stacked_rast)
  
  # Specify the output file name for the average raster
  output_file_for_avg <- file.path(pathtoDryFluxGPP_AvgOutputFolder, paste0("average_", year, ".tif"))
  
  # Write the avg raster to a new tif file
  writeRaster(avg_rast, filename = output_file_for_avg, overwrite = TRUE)
  
  # Just extra- printing a message to confirm create of each avg raster
  cat("Average raster for", year, "has been created.\n")
}



#================================================================================
# Creating total avg (avgeraging annual averages into one raster). Drylands mask applied to total avg

pathtoDryFluxGPP_AvgOutputFolder <- "./data/DRYFLUX_Avg_Outputs"  # Folder to store outputs from the code below that averages monthly and yearly dryflux outputs
pathtoDryFluxGPP_YearFolder <- "./data/DRYFLUX_Avg_Outputs/Annual_Averages"  # Folder with all annual folders
pathtoDrylandMask <- "./data/spatial/Drylands_dataset_2007/drylands_UNCCD_CBD_july2014.shp"  # Dryland mask file - it says 2014 but it's used for all years

# Making a list of folders in DF output folder. List should be as long as the number of years of annual data (currently 17)
annual_files <- list.files(pathtoDryFluxGPP_YearFolder, full.names = TRUE, recursive = FALSE)

# Loop through each year raster to create a single average for 2000-2016 and store it in the avg output folder
for (i in annual_files) {
  
  #load raster data for each annual tif and store 
  rast_data <- rast(i)
  rast_for_avg[[length(rast_for_avg) + 1]] <- rast_data
  
  # Create raster stack and compute avg per pixel 
  stacked_rast <- rast(rast_for_avg)
  avg_rast <- mean(stacked_rast)
  
  # Load in and apply dryland mask to total avg raster
  dryland_mask <- vect(pathtoDrylandMask)
  avg_rast_masked <- mask(avg_rast, dryland_mask)
  
  # Specify the output file name for the average raster
  output_file_for_avg <- file.path(pathtoDryFluxGPP_AvgOutputFolder, "2000_2016_masked_total_average.tif")
  
  # Write the avg raster to a new tif file
  writeRaster(avg_rast_masked, filename = output_file_for_avg, overwrite = TRUE)
}

#==================Plotting Total Avg Output====================================

pathtoDryFluxGPP_TotalAvgOutput <- "./data/DRYFLUX_Avg_Outputs/2000_2016_masked_total_average.tif"
gppTA <- rast(pathtoDryFluxGPP_TotalAvgOutput)
plot(gppTA, main = "Average Dryland GPP: 2000-2016", col= rev(topo.colors(50)))

#==================Applying Arizona Mask and Plotting===========================
esa_crs <- "+proj=longlat +datum=WGS84"

make_az_sf <- function(esa_crs) {
  maps::map("state", "arizona", plot = FALSE, fill = TRUE) |> 
    sf::st_as_sf() |> 
    sf::st_transform(esa_crs)
}

az_shape <- make_az_sf(esa_crs)

cropped_raster <- terra::crop(gppTA, az_shape)
plot(cropped_raster, main = "Arizona Average Dryland GPP: 2000-2016", col= rev(topo.colors(50)))

setwd("./data/DRYFLUX_Avg_Outputs")
writeRaster(cropped_raster, filename = "az_gppTA.tif", overwrite = TRUE)
