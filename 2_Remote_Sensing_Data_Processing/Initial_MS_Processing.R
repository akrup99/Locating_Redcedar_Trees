# CODE 4
# This code uses the raw image files, stretches them, and combines them into the MS metrics indices

# The following is all from Bob McGaughey, and can be found on his Github: https://github.com/bmcgaughey1/DroneLidar2023/tree/main
# This is combined pieces from 3 scripts in that repository: FileSystem.R, CompositeImages.R, and PlotBooks.R

##### Code from FileSystem.R -- Sets up file system ####
dataFolder <- "~/Western redcedar project/data/MS_Data"

plotNumbers <- c(8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)

imagePlotFolders <- c(
  "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_10"
  , "OESF_Cedar_Plot_14"
  , "ONF_Cedar_Plot_15_16"
  , "ONF_Cedar_Plot_15_16"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_23_24_25"
  , "ONF_Cedar_Plot_23_24_25"
  , "ONF_Cedar_Plot_23_24_25"
  , "ONF_Cedar_Plot_26_27"
  , "ONF_Cedar_Plot_26_27"
)

imageFileBaseNames <- c(
  "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_8_9"
  , "OESF_Cedar_Plot_10"
  , "OESF_Cedar_Plot_14"
  , "ONF_Cedar_Plot_15_16"
  , "ONF_Cedar_Plot_15_16"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_17_thru_22"
  , "ONF_Cedar_Plot_23_24_25"
  , "ONF_Cedar_Plot_23_24_25"
  , "ONF_Cedar_Plot_23_24_25"
  , "ONF_Cedar_Plot_26_27"
  , "ONF_Cedar_Plot_26_27"
)

# these are the names used to identify the bands. The file names for the bands are formed
# using the imageFileBaseNames[n] + "_" + bandNames[n] + "_reflectance.tif"
bandNames <- c(
  "red"
  , "green"
  , "blue"
  , "nir"
  , "rededge"
  , "panchro"
  , "LWIR"
)



#### Code from CompositeImages.R -- Creates composite images ####
library(terra)

# new stretch function...just a wrapper around terra's stretch function that
# uses all cells and defaults to the 99.99 percentile instead of the maximum value
# to scale values from 0-255
stretchq <- function(
    x,
    maxq = 0.9999)
{
  invisible(terra::stretch(x, minq = 0.0, maxq = maxq, maxcell = dim(x)[1] * dim(x[2])))
}

# new stretch function to produce 16-bit values. I left this one so it uses the
# 99.9 percentile but scales from 0-65534 instead of 0-65535. ArcPro seemed to
# be treating values of 65535 as invalid.
stretch16 <- function (
    x,
    maxq = 0.999  # same as maximum value
)
{
  # check for maxq = 1.0...this is the maximum and it may be faster to compute
  # the maximum directly
  if (maxq == 1.0) {
    q <- terra::global(x, max, na.rm = TRUE)
  } else {
    # compute the quantile
    q <- terra::global(x, quantile, na.rm = TRUE, probs = c(maxq))
  }
  # q is a data frame so you have to add the subscripts to get the numeric value
  
  # truncate values to the target quantile value
  x <- terra::ifel(x > q[1,1], q[1,1], x)
  
  # do the linear stretch
  x <- x / q[1,1] * 65534
}

# alpha is used for some of the composite images defined in:
# Xie, Qiaoyun & Dash, Jadu & Huang, Wenjiang & Peng, Dailiang & Qin, Qiming &
# Mortimer, Hugh & Casa, Raffaele & Pignatti, Stefano & Laneve, Giovanni &
# Pascucci, Simone & Dong, Yingying & Ye, Huichun. (2018). Vegetation Indices
# Combining the Red and Red-Edge Spectral Information for Leaf Area Index
# Retrieval. IEEE Journal of Selected Topics in Applied Earth Observations and
# Remote Sensing. 11. 10.1109/JSTARS.2018.2813281.  
alpha <- 0.4

# you can use the following line to set a specific plot (by index into lists in FileSystem.R)
# then you can run individual lines to test things without using the loop to process all data
# thePlot <- 1

checkForFiles <- TRUE

for (thePlot in 1:length(imagePlotFolders)) {
  # check to see if we already have the images...needed since a single images covers multiple plots
  if (checkForFiles) {
    if (file.exists(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_NIR.tif")))
      next
  }
  
  baseName <- paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_")
  
  imageFile <- paste0(baseName, bandNames[1], "_reflectance.tif")
  red <- rast(imageFile)
  red <- stretch16(red)
  
  imageFile <- paste0(baseName, bandNames[2], "_reflectance.tif")
  green <- rast(imageFile)
  green <- stretch16(green)
  
  imageFile <- paste0(baseName, bandNames[3], "_reflectance.tif")
  blue <- rast(imageFile)
  blue <- stretch16(blue)
  
  imageFile <- paste0(baseName, bandNames[4], "_reflectance.tif")
  nir <- rast(imageFile)
  nir <- stretch16(nir)
  
  imageFile <- paste0(baseName, bandNames[5], "_reflectance.tif")
  rededge <- rast(imageFile)
  rededge <- stretch16(rededge)
  
  imageFile <- paste0(baseName, bandNames[6], "_reflectance.tif")
  panchro <- rast(imageFile)
  panchro <- stretch16(panchro)
  
  imageFile <- paste0(baseName, bandNames[7], "_reflectance.tif")
  lwir <- rast(imageFile)
  lwir <- stretch16(lwir)
  
  #Xie, Qiaoyun & Dash, Jadu & Huang, Wenjiang & Peng, Dailiang & Qin, Qiming &
  #Mortimer, Hugh & Casa, Raffaele & Pignatti, Stefano & Laneve, Giovanni &
  #Pascucci, Simone & Dong, Yingying & Ye, Huichun. (2018). Vegetation Indices
  #Combining the Red and Red-Edge Spectral Information for Leaf Area Index
  #Retrieval. IEEE Journal of Selected Topics in Applied Earth Observations and
  #Remote Sensing. 11. 10.1109/JSTARS.2018.2813281.  
  rgb <- rast(list(red, green, blue))
  fcnir <- rast(list(nir, red, green))
  fcrededge <- rast(list(rededge, red, green))
  nvdinir <- (nir - red) / (nir + red)
  nvdirededge <- (nir - rededge) / (nir + rededge)
  msr <- ((nir / red) - 1) / sqrt((nir / red) + 1)
  msrrededge <- ((nir / rededge) - 1) / sqrt((nir / rededge) + 1)
  cigreen <- nir / green -1
  cirededge <- nir / rededge - 1
  nvdiredrededge <- (nir - (alpha * red + (1 - alpha) * rededge)) / (nir + (alpha * red + (1 - alpha) * rededge))
  msrredrededge <- (nir / (alpha * red + (1 - alpha) * rededge) - 1) / sqrt(nir / (alpha * red + (1 - alpha) * rededge) + 1)
  ciredrededge <- nir / (alpha * red + (1 - alpha) * rededge) - 1
  
  # extra combinations
  nir_re_g <- rast(list(nir, rededge, green))
  
  #nvdinir <- stretch(nvdinir)
  #nvdirededge <- stretch(nvdirededge)
  
  # # get center of extent...this can be used to plot a small portion of the images to check for detail
  # buf <- 10
  # e <- ext(red)
  # center <- c((e[1] + e[2]) / 2, (e[3] + e[4]) / 2)
  # newExtent <- ext(c(center[1] - buf, center[1] + buf, center[2] - buf, center[2] + buf))
  # 
  # plotRGB(crop(rgb, newExtent))
  # plotRGB(crop(fcnir, newExtent))
  # plotRGB(crop(fcrededge, newExtent))
  # 
  # plotRGB(fcnir)
  # 
  # plot(crop(panchro, newExtent), col = grDevices::gray.colors(255))
  
  # we need a world file to use these images with FUSION. This is done using the gdal options. You can use
  # WORLDFILE=YES but the file extension will be .wld which FUSION won't recognize. TFW-YES produces a world file
  # with .tfw extension which FUSION will recognize and use.
  #
  # write off composite images...convert to 8-bit integer data type to save space. Original image bands used 4-byte floating
  # point type but band DNs ranged form 0.0 - 1.0 so lots of wasted space.
  writeRaster(fcnir, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_NIR.tif"), gdal = "TFW=YES", datatype = "INT2U", overwrite = TRUE)
  writeRaster(fcrededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_rededge.tif"), gdal = "TFW=YES", datatype = "INT2U", overwrite = TRUE)
  writeRaster(rgb, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_RGB.tif"), gdal = "TFW=YES", datatype = "INT2U", overwrite = TRUE)
  writeRaster(nvdirededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nvdirededge.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(nvdinir, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nvdinir.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(msr, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_msr.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(msrrededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_msrrededge.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(cigreen, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_cigreen.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(cirededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_cirededge.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(nvdiredrededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nvdiredrededge.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(msrredrededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_msrredrededge.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(ciredrededge, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_ciredrededge.tif"), gdal = "TFW=YES", datatype = "FLT4S", overwrite = TRUE)
  writeRaster(nir_re_g, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nir_re_g.tif"), gdal = "TFW=YES", datatype = "INT2U", overwrite = TRUE)
  
  #writeRaster(fcnir, paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_NIR.bmp"), gdal = "WORLDFILE=YES", datatype = "INT1U", overwrite = TRUE)
}


#### Code from PlotBooks.R -- Creates PDFs of the different plots; took out part about clipping to DTM and Hillshade ####

checkForFiles <- TRUE
doHillshade <- FALSE

for (thePlot in 1:length(imagePlotFolders)) {
  bookFileName <- paste0(dataFolder, "/", imageFileBaseNames[thePlot], "_Images.pdf")
  
  # check to see if we already have the images...needed since a single images covers multiple plots
  if (checkForFiles) {
    if (file.exists(bookFileName))
      next
  }
  
  # read composite images and grayscale
  baseName <- paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_")
  
  imageFile <- paste0(baseName, bandNames[6], "_reflectance.tif")
  panchro <- rast(imageFile)
  panchro <- stretch16(panchro)
  
  rgb <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_RGB.tif"))
  fcnir <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_NIR.tif"))
  fcrededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_rededge.tif"))
  nvdirededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nvdirededge.tif"))
  nvdinir <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nvdinir.tif"))
  msr <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_msr.tif"))
  msrrededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_msrrededge.tif"))
  cigreen <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_cigreen.tif"))
  cirededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_cirededge.tif"))
  nvdiredrededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nvdiredrededge.tif"))
  msrredrededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_msrredrededge.tif"))
  ciredrededge <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_ciredrededge.tif"))
  nir_re_g <- rast(paste0(dataFolder, "/", imagePlotFolders[thePlot], "/", imageFileBaseNames[thePlot], "_nir_re_g.tif"))
  
  # # get image extent to clip DTM since DTM covers more than 1 plot
  # e <- ext(panchro)
  # 
  # if (doHillshade) {
  #   DTMFileName <- paste0(dataFolder, "/", plotFolders[thePlot], "/ground.dtm")
  #   DTM <- readDTM(DTMFileName, type = "terra", epsg = 26910)
  #   DTM <- crop(DTM, e)
  #   
  #   # do hillshade using multiple angles
  #   alt <- disagg(DTM, 10, method="bilinear")
  #   slope <- terrain(alt, "slope", unit="radians")
  #   aspect <- terrain(alt, "aspect", unit="radians")
  #   h <- shade(slope, aspect, angle = c(45, 45, 45, 80), direction = c(315, 0, 45, 135))
  #   h <- Reduce(mean, h)
  # }
  
  margins <- c(2, 4, 2, 6)
  
  # if dimensions of the area are wider than they are tall, legend is clipped off
  pdf(bookFileName)
  if (doHillshade) plot(h, col=grey(0:100/100), legend=FALSE, mar=c(2,2,2,4), main = "Hillshade")
  
  #  l <- global(panchro, quantile, na.rm = TRUE, probs = c(0.01))
  #  h <- global(panchro, quantile, na.rm = TRUE, probs = c(0.99))
  #  plot(panchro, range = c(l[[1]], h[[1]]), col = grDevices::gray.colors(255), legend = FALSE, axes = FALSE, mar = c(0, 0, 2, 0), main = "panchromatic")
  plot(panchro, col = grDevices::gray.colors(255), legend = FALSE, axes = TRUE, mar = margins, main = "panchromatic")
  
  plotRGB(rgb, mar = margins, axes = TRUE, main = "RGB")
  plotRGB(fcnir, mar = margins, axes = TRUE, main = "False color NIR")
  plotRGB(fcrededge, mar = margins, axes = TRUE, main = "False color rededge")
  
  l <- global(nvdinir, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(nvdinir, quantile, na.rm = TRUE, probs = c(0.99))
  plot(nvdinir, range = c(l[[1]], h[[1]]), mar = margins, main = "NIR NVDI")
  
  l <- global(nvdirededge, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(nvdirededge, quantile, na.rm = TRUE, probs = c(0.99))
  plot(nvdirededge, range = c(l[[1]], h[[1]]), mar = margins, main = "rededge NVDI")
  
  l <- global(msr, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(msr, quantile, na.rm = TRUE, probs = c(0.99))
  plot(msr, range = c(l[[1]], h[[1]]), mar = margins, main = "MSR")
  
  l <- global(msrrededge, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(msrrededge, quantile, na.rm = TRUE, probs = c(0.99))
  plot(msrrededge, range = c(l[[1]], h[[1]]), mar = margins, main = "rededge MSR")
  
  l <- global(cigreen, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(cigreen, quantile, na.rm = TRUE, probs = c(0.99))
  plot(cigreen, range = c(l[[1]], h[[1]]), mar = margins, main = "green CI")
  
  l <- global(cigreen, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(cigreen, quantile, na.rm = TRUE, probs = c(0.99))
  plot(cirededge, range = c(l[[1]], h[[1]]), mar = margins, main = "rededge CI")
  
  l <- global(nvdiredrededge, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(nvdiredrededge, quantile, na.rm = TRUE, probs = c(0.99))
  plot(nvdiredrededge, range = c(l[[1]], h[[1]]), mar = margins, main = "red and rededge NVDI")
  
  l <- global(msrredrededge, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(msrredrededge, quantile, na.rm = TRUE, probs = c(0.99))
  plot(msrredrededge, range = c(l[[1]], h[[1]]), mar = margins, main = "red and rededge MSR")
  
  l <- global(ciredrededge, quantile, na.rm = TRUE, probs = c(0.01))
  h <- global(ciredrededge, quantile, na.rm = TRUE, probs = c(0.99))
  plot(ciredrededge, range = c(l[[1]], h[[1]]), mar = margins, main = "red and rededge modified CI")
  
  plotRGB(nir_re_g, mar = margins, axes = TRUE, main = "False color NIR-rededge-green")
  dev.off()
}









