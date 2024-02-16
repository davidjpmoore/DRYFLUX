# Load necessary libraries
library(terra)
library(tidyverse)

# Define paths to files - Remember to update these paths according to your local setup

pathtoDryFluxGPP_SingleMonth <- "path/to/single/month/file.tif"  # Single month GPP file
pathtoDryFluxGPP_YearFolder <- "path/to/yearly/files/folder"  # Folder with all monthly GPP files for a year
pathtoDrylandMask <- "path/to/dryland/mask/file.shp"  # Dryland mask file - it says 2014 but it's used for all years

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
plot(gppDrylandMasked, main = "Dryland GPP - Jan 2001")

#add color customization if you want
plot(gppDrylandMasked, main = "Dryland GPP - Jan 2001", col= rev(topo.colors(50)))

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

