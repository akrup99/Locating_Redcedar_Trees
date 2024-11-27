# Ally Kruper
# last updated 1/29/24
# Code to clip and convert LAZ files to las files for importing into FUSION for individual matching

# Install below packages first if not installed already

# Note: This code is not the most efficient way to do this clipping (would now use plot centers and would create a buffer around plot center in ArcGIS Pro -->
# would then clip the LAZ files to those extents). This is just what I did at the time. This also only shows one plot because
# I would replace the plot that I was working with with a new plot once I was done with it (re-using the same code and just changing the 
# file loaded), something else I wouldn't do again.


# loading packages
library(lidR)
library(mapview)
library(sf)
library(raster)

#Reading in las files as a LAS catalog (just means that there's more than one file there) (can also be laz files)
# Make sure that all of the desired LAS/LAZ files are in one folder, and that you are reading in that folder
ctg<- readLAScatalog("data/raw_laz_files/plots8_9_14")


# Need to check and change the project to match what it was taken in (26910) (only do this if needed)
# #Shows that original laz files don't have associated projection
print(ctg)
# 
# #Change the projection
st_crs(ctg)<- 26910 
# 
# #Shows that projection change was successful 
print(ctg)

#Plot locations of lidar on actual map
plot(ctg, mapview = TRUE)

#Plot locations of lidar on R's plotting system (which is in NAD 83 / UTM zone 10N)
plot(ctg)

#Use locator (optional, only if helpful) to determine where to clip las files (use combo of R's plotting and location on actual map)
locator(4)

#Clip las files based on where wanting (i.e. where the stem maps are) --> making relatively tight b/c plot adjustment already done
lasclip <- clip_rectangle(ctg, 438309, 5244258, 438377, 5244330)


#Write the las file using whatever name makes the most sense organization-wise 
writeLAS(lasNorm, file = file.path("data/Clipped_las_files_for_FUSION", "plots26_27.las"), index = FALSE)



## Realized FUSION needs the las filed NOT normalized --> going back and re-doing the plots, clipping to the extent of 
# the old las files
ctg<- readLAScatalog("data/raw_laz_files/plots8_9_14")

oldLAS <- readLAS("data/Clipped_las_files_for_FUSION/plots8_9.las")

st_crs(ctg)<- 26910 
print(ctg)

extent(oldLAS)

# lasclip <- clip_roi(ctg, geometry = extent(oldLAS))
lasclip <- clip_rectangle(ctg, 411008, 5295150, 411090, 5295309)

writeLAS(lasclip, file = file.path("data/Clipped_las_files_for_FUSION_notNorm", "plot_8_larger.las"), index = FALSE)


