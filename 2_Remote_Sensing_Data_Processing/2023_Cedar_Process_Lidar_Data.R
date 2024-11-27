# CODE 6
# This code segments and runs FUSION CloudMetrics on full lidar extent (to be used when predicting species outside of plots)

# Ally Kruper
# last updated 3/11/24
# Goal: Process lidar data for 2023 cedar plots (calculate cloud metrics)

## Code is from Bob's code!! Slightly modified for this year's data
## Github link: https://github.com/bmcgaughey1/DroneLidarCode/blob/main/Rcode/Process2022Data.R

# processing for 2023 Cedar drone lidar data

# installing and loading the required packages
install.packages("devtools")
devtools::install_github("bmcgaughey1/fusionwrapr")
install.packages("sf")
install.packages("raster")

library(fusionwrapr)
library(sf)
library(raster)

# Ally comment: need to set FUSION path
setFUSIONpath("F:/workspace/OESF/AllyCedarFUSIONWork/FUSION")


renameCheck <- function(from = "", to = "") {
  if (file.rename(from, to) == FALSE)
    cat("Move failed for: ", from, "\n")
}


# projection for for UTM10...this is used for several outputs
# working directoryt is set at "F:/workspace/OESF/AllyCedarFUSIONWork"
prjFile <- "UTM10.prj"

# read in the list of project folders
# Ally note: Note that the working directly is set to "F:/workspace/OESF/AllyCedarFUSIONWork". dirlist.text file was manually created
dirList <- "dirlist.txt"
dirs <- read.csv2(dirList, header = FALSE)

# fix backslashes
dirs <- lapply(dirs[,1], function(x) {gsub("\\\\", "/", x)})


# create subfolders under each project area. The actual processing code will ensure that these folders exist
# and create them if not. However, I liked the idea of having the folder structure in place before starting the
# processing. It makes it a little easier to see if things worked by simply looking for files in each folder.
for (i in 1:length(dirs)) {
  #for (i in 1:2) {
  verifyFolder(paste0(dirs[i], "/DSM"))
  verifyFolder(paste0(dirs[i], "/ground"))
  verifyFolder(paste0(dirs[i], "/LAS"))
  verifyFolder(paste0(dirs[i], "/Processing"))
  verifyFolder(paste0(dirs[i], "/Processing/CHM"))
  verifyFolder(paste0(dirs[i], "/Processing/CSM"))
  verifyFolder(paste0(dirs[i], "/Processing/TreeTops"))
  verifyFolder(paste0(dirs[i], "/Processing/TreeTops_normalized"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/TAOpts"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/TAOpts_GroundBiased"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/TAOpts_normalized"))
  
  # move DSM files...not all projects have 0.5m DSMs (Ally comment: going to comment this part out b/c don't have DSMs)
  # renameCheck(paste0(dirs[i], "/DSM_.5m.laz"), paste0(dirs[i], "/DSM/DSM_.5m.laz"))
  # renameCheck(paste0(dirs[i], "/DSM_.5m.tif"), paste0(dirs[i], "/DSM/DSM_.5m.tif"))
  # renameCheck(paste0(dirs[i], "/DSM_1m.laz"), paste0(dirs[i], "/DSM/DSM_1m.laz"))
  # renameCheck(paste0(dirs[i], "/DSM_1m.tif"), paste0(dirs[i], "/DSM/DSM_1m.tif"))
  
  # move ground files
 # renameCheck(paste0(dirs[i], "/DTM_1m.laz"), paste0(dirs[i], "/ground/DTM_1m.laz"))
  renameCheck(paste0(dirs[i], "/DTM_1m.tif"), paste0(dirs[i], "/ground/DTM_1m.tif"))
  
  # move contour shapefiles and KML file...uses a rename command to "move" the files # Ally comment: just going to do this manually (think I have it set up different)
  #t <- basename(Sys.glob(paste0(dirs[i], "/*Contour.*")))
  #for (j in 1:length(t)) {
   # renameCheck(paste0(dirs[i], "/", t[j]), paste0(dirs[i], "/ground/", t[j]))
  # }
}

# Ally comment: above code failed to move the DTMs --> just going to do that manually; also renamed files manually
# convert DTM to PLANS format needed for FUSION
for (i in 1:length(dirs)) {
  #for (i in 1:2) {
  r <- raster(paste0(dirs[i], "/ground/DTM_1m.tif"))
  writeDTM(r, paste0(dirs[i], "/ground/ground.dtm"),
           xyunits = "M",
           zunits = "M",
           coordsys = 1,
           zone = 10,
           horizdatum = 2,
           vertdatum = 2)
}

# we should have clean data ready for further processing...

# loop through folders and do processing. This code is copied from the processing script from the 2021 data with
# just a few modifications.
#
# this loop should start at 1 unless processing was interrupted by a reboot.

# Ally note: For OESF_Cedar_Plots_8_9_14 --> too big to run FUSION commands on --> skipped that originally, see below this for loop for a loop specifically for that set


for (i in 3:length(dirs)) {
  #for (i in 1:1) {
  # set up folder info
  dataFolder <- dirs[i]
  groundFileSpec <- paste0(dirs[i], "/ground/ground.dtm")
  
  outputFolder <- paste0(dirs[i], "/Processing")
  
  # *****************************************************************************
  # *****************************************************************************
  # build CSM and CHM, do segmentation and compute metrics for Tree Approximate Objects (TAOs)
  # *****************************************************************************
  # *****************************************************************************
  # header things for batch file
  batchFile <- paste0(outputFolder, "/DoProcessing.bat")
  
  # set default behavior for commands...basically telling the fusionwrapr package to create a
  # batch file instead of running the commands directly.
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)
  
  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))
  
  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)
  
  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Processing.log"), logClear = TRUE)
  
  # add comment
  addToCommandFile("Start of processing commands")
  
  # create ground model...already have these for all areas in ground folder
  # however, the naming for the models isn't consistent
  
  # create CSM and CHM
  # Resolution and smoothing with the canopy surfaces have a major effect on the segmentation
  # behavior. I started with 0.5m cells and no smoothing
  CanopyModel(paste0(outputFolder, "/CSM/CSM.dtm")
              , 0.5
              , "M"
              , "M"
              , 1
              , 10
              , 2
              , 2
              , paste0(dataFolder, "/*.laz")
              , smooth = 3
              , peaks = FALSE
              , class = "~7,18"
  )
  
  CanopyModel(paste0(outputFolder, "/CHM/CHM.dtm")
              , 0.5
              , "M"
              , "M"
              , 1
              , 10
              , 2
              , 2
              , paste0(dataFolder, "/*.laz")
              , ground = groundFileSpec
              , smooth = 3
              , peaks = FALSE
              , class = "~7,18"
  )
  
  # run segmentation to produce normalized TAO clips
  # omit the ground points (class 2)
  TreeSeg(paste0(outputFolder, "/CHM/CHM.dtm")
          , 2
          , paste0(outputFolder, "/Trees/trees_normalized.csv")
          , shape = TRUE
          , ptheight = TRUE
          , points = paste0(dataFolder, "/*.laz")
          , class = "~2"
          , clipfolder = paste0(outputFolder, "/Trees/TAOpts_normalized")
          , ground = groundFileSpec
          , comment = "Create normalized TAO point clips"
          , projection = prjFile
  )
  
  # run segmentation to produce non-normalized TAO clips
  # omit the ground points (class 2)
  TreeSeg(paste0(outputFolder, "/CHM/CHM.dtm")
          , 2
          , paste0(outputFolder, "/Trees/trees.csv")
          , shape = TRUE
          , points = paste0(dataFolder, "/*.laz")
          , class = "~2"
          , clipfolder = paste0(outputFolder, "/Trees/TAOpts")
          , comment = "Create TAO point clips with point elevations"
          , projection = prjFile
  )
  
  # compute metrics for both sets of TAOs
  # use height thresholds for normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_normalized/*.lda")
               , paste0(outputFolder, "/TAO_normalized_metrics.csv")
               , new = TRUE
               , minht = 2.0
               , above = 2.0
               , rid = TRUE
               , pa = TRUE
  )
  
  # no height thresholds for non-normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts/*.lda")
               , paste0(outputFolder, "/TAO_metrics.csv")
               , new = TRUE
               , rid = TRUE
               , pa = TRUE
  )
  
  useLogFile("")
  
  # run the batch file
  runCommandFile()
  
  # *****************************************************************************
  # *****************************************************************************
  # clip the upper portion of the TAOs, compute metrics and get the highest
  # point for each treetop
  # *****************************************************************************
  # *****************************************************************************
  # do the treetops...upper portion of crown
  
  # reset options
  resetGlobalCommandOptions()
  
  batchFile <- paste0(outputFolder, "/DoProcessing2.bat")
  
  # set default behavior for commands
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)
  
  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))
  
  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)
  
  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Processing.log"), logClear = TRUE)
  
  # add comment
  addToCommandFile("Start of processing commands")
  
  topDepth <- 3
  
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  
  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth
  
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(outputFolder, "/TreeTops/", m$FileTitle[i], ".lda")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
    )
  }
  
  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(outputFolder, "/TreeTops_normalized/", m$FileTitle[i], ".lda")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
    )
  }
  
  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/TreeTops_normalized/", "*.lda")
               , paste0(outputFolder, "/TreeTops_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  CloudMetrics(paste0(outputFolder, "/TreeTops/", "*.lda")
               , paste0(outputFolder, "/TreeTop_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  # get the highpoint for trees...takes advantage of the /highpoint option in cloudmetrics
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts/*.lda")
               , paste0(outputFolder, "/TAO_metrics_highpts.csv")
               , new = TRUE
               , rid = TRUE
               , highpoint = TRUE
  )
  
  useLogFile("")
  
  # run the batch file
  runCommandFile()
  
  # reset options...FUSION commands below will run directly
  resetGlobalCommandOptions()
  
  # *****************************************************************************
  # *****************************************************************************
  # merge information from segmentation, metrics, and actual highpoint XY
  # *****************************************************************************
  # *****************************************************************************
  
  # use the /highpoint option with cloudmetrics to get the XY of the highest point in a TAO or treetop
  treeHighPtFile <- paste0(outputFolder, "/TAO_metrics_highpts.csv")
  thp <- read.csv(treeHighPtFile)
  
  # get surface values (ground elevations)
  thp <- GetSurfaceValues(thp
                          , xLabel = "High.point.X"
                          , yLabel = "High.point.Y"
                          , idLabel = "GroundElev"
                          , surfaceFile = groundFileSpec
  )
  
  # *****************************************************************************
  # use the ground elevation to bias the points for the TAOs
  # *****************************************************************************
  # ***** this could be better placed in the processing order but we need the ground elevation under the high point
  files <- Sys.glob(paste0(outputFolder, "/Trees/TAOpts/*.lda"))
  for (i in 1:length(files)) {
    ClipData(thp$DataFile[i]
             , paste0(outputFolder, "/Trees/TAOpts_GroundBiased/", basename(thp$DataFile[i]))
             , biaselev = -thp$GroundElev[i]
             , class = "~2"
    )
  }
  
  # no height thresholds for ground-normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_GroundBiased/*.lda")
               , paste0(outputFolder, "/TAO_GroundBiased_metrics.csv")
               , new = TRUE
               , rid = TRUE
               , pa = TRUE
  )
  
  # match TAOs from the segmentation with the metrics...need to do this for normalized
  # and non-normalized trees
  treeInfoFile <- paste0(outputFolder, "/Trees/trees_Polygons.csv")
  t <- read.csv(treeInfoFile)
  m <- read.csv(paste0(outputFolder, "/TreeTop_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TreeTop_metrics_merged.csv"), row.names = FALSE)
  
  m <- read.csv(paste0(outputFolder, "/TreeTops_normalized_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TreeTops_normalized_metrics_merged.csv"), row.names = FALSE)
  
  # do the full tree metrics
  # ***** these are for the TAOs normalized using terrain...not simple bias for ground under high point
  m <- read.csv(paste0(outputFolder, "/TAO_normalized_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TAO_normalized_metrics_merged.csv"), row.names = FALSE)
  
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TAO_metrics_merged.csv"), row.names = FALSE)
  
  # *****************************************************************************
  # *****************************************************************************
  # compute crown base height using the individual percentile data (P1,P2,P3,...)
  # *****************************************************************************
  # *****************************************************************************
  # read the percentile data
  PercentileData <- read.csv(paste0(outputFolder, "/TAO_GroundBiased_metrics_percentile.csv"), stringsAsFactors = FALSE)
  
  # Normalize the percentile heights using the 99th percentile. Output has the object identifier (extra column at beginning).
  # percentile heights start in column 4 and P99 is column 103
  # normalized in this context means divided by the P99 value...not normalized relative to ground elevation
  #
  # Basic idea is to look at the "plot" of normalized percentile heights (relative to P99) and find the
  # segment with the steepest slope. The base height is then set to the height at the upper end of this segment.
  #
  # I found a similar method in the literature but don't have the reference handy. The method below seems to work
  # pretty well provided the TAOs are actual trees. When the TAOs are only partial trees or several trees, the
  # result isn't as good (but still not too bad).
  NormalizedPercentileData <- PercentileData[, 4:103] / PercentileData[, 103]
  NormalizedPercentileData$Identifier <- PercentileData$Identifier
  NormalizedPercentileData$Plot <- PercentileData$DataFile
  NormalizedPercentileData$Label <- basename(PercentileData$DataFile)
  NormalizedPercentileData$cbh <- 0.0
  
  x <- c(1:99)
  for (i in 1:nrow(PercentileData)) {
    slopes <- vector()
    # compute slopes
    for (j in 2:99) {
      x1 <- x[j - 1]
      x2 <- x[j]
      y1 <- NormalizedPercentileData[i, j -1]
      y2 <- NormalizedPercentileData[i, j]
      slope_i <- (y2-y1)/(x2-x1)
      slopes <- append(slopes, slope_i)
    }
    
    # get max slope
    maxSlopeIndex <- which.max(slopes)
    cbhIndex <- maxSlopeIndex + 1
    NormalizedPercentileData$cbh[i] <- PercentileData[i, cbhIndex + 4]
  }
  
  # cbh for trees is saved with the NormalizedPercentileData
  # I should probably save this to a separate file...
  
  # *****************************************************************************
  # *****************************************************************************
  # work with crown polygons to get centroid and crown dimensions...
  # this gives us a polygon area to use to compute a crown diameter.
  # The centroid of the crown polygons provide an alternate location for the
  # trees. However, I found that the actual highpoint location from the
  # segmentation was a better match for field data.
  # *****************************************************************************
  # *****************************************************************************
  library(sf)
  treePolyFile <- paste0(outputFolder, "/Trees/trees_Polygons.shp")
  
  # read crown polygons
  treePolys <- st_read(treePolyFile)
  
  # this prevents a warning
  st_agr(treePolys) <- "constant"
  
  # compute centroids and get them as simple table
  centroids <- st_coordinates(st_centroid(treePolys))
  
  # rename columns
  colnames(centroids) <- c("centroidX", "centroidY")
  
  # add centroids to polygon attributes
  treePolys <- cbind(treePolys, centroids)
  
  # merge with high point data including ground elevation under high XY
  treePolys <- merge(treePolys
                     , thp[, c("Identifier", "High.point.X", "High.point.Y", "High.point.elevation", "GroundElev")]
                     , by.x = "BasinID"
                     , by.y = "Identifier")
  
  # merge in cbh
  treePolys <- merge(treePolys
                     , NormalizedPercentileData[, c("Identifier", "cbh")]
                     , by.x = "BasinID"
                     , by.y = "Identifier")
  
  # compute crown diameter using polygon area
  treePolys$CrownDiaByArea <- sqrt(treePolys$PolyArea / pi) * 2.0
  
  # write geopackage...preserves column names that are too long for shapefiles
  st_write(treePolys, paste0(outputFolder, "/Trees/trees_Polygons.gpkg"), append = FALSE)
  
  # build new data frame for FUSION tree objects...allows use to display tree models in the point cloud.
  # FUSION tree objects:
  # ID, X, Y, Z, ht, cbh, min crown dia, max crown dia, rotation, R, G, B
  #
  # options for XY: high point, centroid of crown polygon, detected stem location
  # options for Z: 0.0, ground elevation under XY point
  # options for crown diameters: diameter of circle with same area as crown polygon (rotation=0)
  #       major/minor axis of ellipse fit to crown polygon (rotation=computed)
  #       W-E and S-N using crown polygon  (rotation=0)
  #
  # high point XY seems to align better with stem hits
  tp <- st_drop_geometry(treePolys)
  fTrees <- data.frame(ID = tp$BasinID
                       , X = tp$High.point.X
                       , Y = tp$High.point.Y
                       #                     , X = tp$centroidX
                       #                     , Y = tp$centroidY
                       , Z = 0.0
                       , ht = tp$High.point.elevation - tp$GroundElev
                       , cbh = tp$cbh
                       , minDia = tp$CrownDiaByArea
                       , maxDia = tp$CrownDiaByArea
                       , rotation = 0.0
                       , R = 255
                       , G = 127
                       , B = 0)
  write.csv(fTrees, paste0(outputFolder, "/Trees/FUSIONtrees.csv"), row.names = FALSE)
}


#### Ally comment: did plots 11, 12, 13 separately b/c flown different year --> below is code for those plots ####
## plots 11, 12, 13 too big (too many points) --> need to split them up
library(lidR)
OESF_Cedar_Plots_11_12_13 <- readLAScatalog("F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13")

plot(OESF_Cedar_Plots_11_12_13)
locator(1)

OESF_Cedar_Plots_11_12_13_pt1 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 401400,5277400,401600,5278100)
print(OESF_Cedar_Plots_11_12_13_pt1)
writeLAS(OESF_Cedar_Plots_11_12_13_pt1, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))

OESF_Cedar_Plots_11_12_13_pt2 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 401600,5277400,401700,5278100)
print(OESF_Cedar_Plots_11_12_13_pt2)
writeLAS(OESF_Cedar_Plots_11_12_13_pt2, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))

OESF_Cedar_Plots_11_12_13_pt3 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 401700,5277400,401800,5278100)
print(OESF_Cedar_Plots_11_12_13_pt3)
writeLAS(OESF_Cedar_Plots_11_12_13_pt3, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))

OESF_Cedar_Plots_11_12_13_pt4 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 401800,5277400,401900,5278100)
print(OESF_Cedar_Plots_11_12_13_pt4)
writeLAS(OESF_Cedar_Plots_11_12_13_pt4, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))

OESF_Cedar_Plots_11_12_13_pt5 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 401900,5277400,402000,5278100)
print(OESF_Cedar_Plots_11_12_13_pt5)
writeLAS(OESF_Cedar_Plots_11_12_13_pt5, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))

OESF_Cedar_Plots_11_12_13_pt6 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 402000,5277400,402200,5278100)
print(OESF_Cedar_Plots_11_12_13_pt6)
writeLAS(OESF_Cedar_Plots_11_12_13_pt6, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))

OESF_Cedar_Plots_11_12_13_pt7 <- clip_rectangle(OESF_Cedar_Plots_11_12_13, 402200,5277400,402400,5278100)
print(OESF_Cedar_Plots_11_12_13_pt7)
writeLAS(OESF_Cedar_Plots_11_12_13_pt7, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13", fileext = ".laz"))



# ok, now doing the actually processing with the different plot parts 

# installing and loading the required packages

library(fusionwrapr)
library(sf)
library(raster)

# Ally comment: need to set FUSION path
setFUSIONpath("F:/workspace/OESF/AllyCedarFUSIONWork/FUSION")


renameCheck <- function(from = "", to = "") {
  if (file.rename(from, to) == FALSE)
    cat("Move failed for: ", from, "\n")
}

# manually created a directory list for projects
# The following command will get you a list of all folder names. This can then be edited to
# remove the subfolder names under each project. Somewhat tedious but easier than writing code
# to do the same thing (or at least more straight forward).
# dir /s /a:d /b


# projection for for UTM10...this is used for several outputs
# working directoryt is set at "F:/workspace/OESF/AllyCedarFUSIONWork"
prjFile <- "UTM10.prj"


# Ally note: Note that the working directly is set to "F:/workspace/OESF/AllyCedarFUSIONWork"
  # making new dirs list based off of the different Plots_11_12_13 parts
dirs <- c("F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt1", 
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt2", 
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt3", 
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt4",
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt5",
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt6",
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt7")


# create subfolders under each project area. The actual processing code will ensure that these folders exist
# and create them if not. However, I liked the idea of having the folder structure in place before starting the
# processing. It makes it a little easier to see if things worked by simply looking for files in each folder.
for (i in 1:length(dirs)) {
  #for (i in 1:2) {
  verifyFolder(paste0(dirs[i], "/DSM"))
  verifyFolder(paste0(dirs[i], "/ground"))
  verifyFolder(paste0(dirs[i], "/LAS"))
  verifyFolder(paste0(dirs[i], "/Processing"))
  verifyFolder(paste0(dirs[i], "/Processing/CHM"))
  verifyFolder(paste0(dirs[i], "/Processing/CSM"))
  verifyFolder(paste0(dirs[i], "/Processing/TreeTops"))
  verifyFolder(paste0(dirs[i], "/Processing/TreeTops_normalized"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/TAOpts"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/TAOpts_GroundBiased"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/TAOpts_normalized"))
  
  # move DSM files...not all projects have 0.5m DSMs (Ally comment: going to comment this part out b/c don't have DSMs)
  # renameCheck(paste0(dirs[i], "/DSM_.5m.laz"), paste0(dirs[i], "/DSM/DSM_.5m.laz"))
  # renameCheck(paste0(dirs[i], "/DSM_.5m.tif"), paste0(dirs[i], "/DSM/DSM_.5m.tif"))
  # renameCheck(paste0(dirs[i], "/DSM_1m.laz"), paste0(dirs[i], "/DSM/DSM_1m.laz"))
  # renameCheck(paste0(dirs[i], "/DSM_1m.tif"), paste0(dirs[i], "/DSM/DSM_1m.tif"))
  
  # move ground files
  # renameCheck(paste0(dirs[i], "/DTM_1m.laz"), paste0(dirs[i], "/ground/DTM_1m.laz"))
  # renameCheck(paste0(dirs[i], "/DTM_1m.tif"), paste0(dirs[i], "/ground/DTM_1m.tif"))
  
  # move contour shapefiles and KML file...uses a rename command to "move" the files # Ally comment: just going to do this manually (think I have it set up different)
  #t <- basename(Sys.glob(paste0(dirs[i], "/*Contour.*")))
  #for (j in 1:length(t)) {
  # renameCheck(paste0(dirs[i], "/", t[j]), paste0(dirs[i], "/ground/", t[j]))
  # }
}

# Ally comment: above code failed to move the DTMs --> just going to do that manually; also renamed files manually
# convert DTM to PLANS format needed for FUSION
for (i in 1:length(dirs)) {
  #for (i in 1:2) {
  r <- raster(paste0(dirs[i], "/ground/DTM_1m.tif"))
  writeDTM(r, paste0(dirs[i], "/ground/ground.dtm"),
           xyunits = "M",
           zunits = "M",
           coordsys = 1,
           zone = 10,
           horizdatum = 2,
           vertdatum = 2)
}




# we should have clean data ready for further processing...

# loop through folders and do processing. This code is copied from the processing script from the 2021 data with
# just a few modifications.
#
# this loop should start at 1 unless processing was interrupted by a reboot.


for (i in 1:length(dirs)) {
  #for (i in 1:1) {
  # set up folder info
  dataFolder <- dirs[i]
  groundFileSpec <- paste0(dirs[i], "/ground/ground.dtm")
  
  outputFolder <- paste0(dirs[i], "/Processing")
  
  # *****************************************************************************
  # *****************************************************************************
  # build CSM and CHM, do segmentation and compute metrics for Tree Approximate Objects (TAOs)
  # *****************************************************************************
  # *****************************************************************************
  # header things for batch file
  batchFile <- paste0(outputFolder, "/DoProcessing.bat")
  
  # set default behavior for commands...basically telling the fusionwrapr package to create a
  # batch file instead of running the commands directly.
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)
  
  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))
  
  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)
  
  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Processing.log"), logClear = TRUE)
  
  # add comment
  addToCommandFile("Start of processing commands")
  
  # create ground model...already have these for all areas in ground folder
  # however, the naming for the models isn't consistent
  
  # create CSM and CHM
  # Resolution and smoothing with the canopy surfaces have a major effect on the segmentation
  # behavior. I started with 0.5m cells and no smoothing
  CanopyModel(paste0(outputFolder, "/CSM/CSM.dtm")
              , 0.5
              , "M"
              , "M"
              , 1
              , 10
              , 2
              , 2
              , paste0(dataFolder, "/*.laz")
              , smooth = 3
              , peaks = FALSE
              , class = "~7,18"
  )
  
  CanopyModel(paste0(outputFolder, "/CHM/CHM.dtm")
              , 0.5
              , "M"
              , "M"
              , 1
              , 10
              , 2
              , 2
              , paste0(dataFolder, "/*.laz")
              , ground = groundFileSpec
              , smooth = 3
              , peaks = FALSE
              , class = "~7,18"
  )
  
  # run segmentation to produce normalized TAO clips
  # omit the ground points (class 2)
  TreeSeg(paste0(outputFolder, "/CHM/CHM.dtm")
          , 2
          , paste0(outputFolder, "/Trees/trees_normalized.csv")
          , shape = TRUE
          , ptheight = TRUE
          , points = paste0(dataFolder, "/*.laz")
          , class = "~2"
          , clipfolder = paste0(outputFolder, "/Trees/TAOpts_normalized")
          , ground = groundFileSpec
          , comment = "Create normalized TAO point clips"
          , projection = prjFile
  )
  
  # run segmentation to produce non-normalized TAO clips
  # omit the ground points (class 2)
  TreeSeg(paste0(outputFolder, "/CHM/CHM.dtm")
          , 2
          , paste0(outputFolder, "/Trees/trees.csv")
          , shape = TRUE
          , points = paste0(dataFolder, "/*.laz")
          , class = "~2"
          , clipfolder = paste0(outputFolder, "/Trees/TAOpts")
          , comment = "Create TAO point clips with point elevations"
          , projection = prjFile
  )
  
  # compute metrics for both sets of TAOs
  # use height thresholds for normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_normalized/*.lda")
               , paste0(outputFolder, "/TAO_normalized_metrics.csv")
               , new = TRUE
               , minht = 2.0
               , above = 2.0
               , rid = TRUE
               , pa = TRUE
  )
  
  # no height thresholds for non-normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts/*.lda")
               , paste0(outputFolder, "/TAO_metrics.csv")
               , new = TRUE
               , rid = TRUE
               , pa = TRUE
  )
  
  useLogFile("")
  
  # run the batch file
  runCommandFile()
  
  # *****************************************************************************
  # *****************************************************************************
  # clip the upper portion of the TAOs, compute metrics and get the highest
  # point for each treetop
  # *****************************************************************************
  # *****************************************************************************
  # do the treetops...upper portion of crown
  
  # reset options
  resetGlobalCommandOptions()
  
  batchFile <- paste0(outputFolder, "/DoProcessing2.bat")
  
  # set default behavior for commands
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)
  
  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))
  
  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)
  
  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Processing.log"), logClear = TRUE)
  
  # add comment
  addToCommandFile("Start of processing commands")
  
  topDepth <- 3
  
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  
  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth
  
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(outputFolder, "/TreeTops/", m$FileTitle[i], ".lda")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
    )
  }
  
  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(outputFolder, "/TreeTops_normalized/", m$FileTitle[i], ".lda")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
    )
  }
  
  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/TreeTops_normalized/", "*.lda")
               , paste0(outputFolder, "/TreeTops_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  CloudMetrics(paste0(outputFolder, "/TreeTops/", "*.lda")
               , paste0(outputFolder, "/TreeTop_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  # get the highpoint for trees...takes advantage of the /highpoint option in cloudmetrics
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts/*.lda")
               , paste0(outputFolder, "/TAO_metrics_highpts.csv")
               , new = TRUE
               , rid = TRUE
               , highpoint = TRUE
  )
  
  useLogFile("")
  
  # run the batch file
  runCommandFile()
  
  # reset options...FUSION commands below will run directly
  resetGlobalCommandOptions()
  
  # *****************************************************************************
  # *****************************************************************************
  # merge information from segmentation, metrics, and actual highpoint XY
  # *****************************************************************************
  # *****************************************************************************
  
  # use the /highpoint option with cloudmetrics to get the XY of the highest point in a TAO or treetop
  treeHighPtFile <- paste0(outputFolder, "/TAO_metrics_highpts.csv")
  thp <- read.csv(treeHighPtFile)
  
  # get surface values (ground elevations)
  thp <- GetSurfaceValues(thp
                          , xLabel = "High.point.X"
                          , yLabel = "High.point.Y"
                          , idLabel = "GroundElev"
                          , surfaceFile = groundFileSpec
  )
  
  # *****************************************************************************
  # use the ground elevation to bias the points for the TAOs
  # *****************************************************************************
  # ***** this could be better placed in the processing order but we need the ground elevation under the high point
  files <- Sys.glob(paste0(outputFolder, "/Trees/TAOpts/*.lda"))
  for (i in 1:length(files)) {
    ClipData(thp$DataFile[i]
             , paste0(outputFolder, "/Trees/TAOpts_GroundBiased/", basename(thp$DataFile[i]))
             , biaselev = -thp$GroundElev[i]
             , class = "~2"
    )
  }
  
  # no height thresholds for ground-normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_GroundBiased/*.lda")
               , paste0(outputFolder, "/TAO_GroundBiased_metrics.csv")
               , new = TRUE
               , rid = TRUE
               , pa = TRUE
  )
  
  # match TAOs from the segmentation with the metrics...need to do this for normalized
  # and non-normalized trees
  treeInfoFile <- paste0(outputFolder, "/Trees/trees_Polygons.csv")
  t <- read.csv(treeInfoFile)
  m <- read.csv(paste0(outputFolder, "/TreeTop_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TreeTop_metrics_merged.csv"), row.names = FALSE)
  
  m <- read.csv(paste0(outputFolder, "/TreeTops_normalized_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TreeTops_normalized_metrics_merged.csv"), row.names = FALSE)
  
  # do the full tree metrics
  # ***** these are for the TAOs normalized using terrain...not simple bias for ground under high point
  m <- read.csv(paste0(outputFolder, "/TAO_normalized_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TAO_normalized_metrics_merged.csv"), row.names = FALSE)
  
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TAO_metrics_merged.csv"), row.names = FALSE)
  
  # *****************************************************************************
  # *****************************************************************************
  # compute crown base height using the individual percentile data (P1,P2,P3,...)
  # *****************************************************************************
  # *****************************************************************************
  # read the percentile data
  PercentileData <- read.csv(paste0(outputFolder, "/TAO_GroundBiased_metrics_percentile.csv"), stringsAsFactors = FALSE)
  
  # Normalize the percentile heights using the 99th percentile. Output has the object identifier (extra column at beginning).
  # percentile heights start in column 4 and P99 is column 103
  # normalized in this context means divided by the P99 value...not normalized relative to ground elevation
  #
  # Basic idea is to look at the "plot" of normalized percentile heights (relative to P99) and find the
  # segment with the steepest slope. The base height is then set to the height at the upper end of this segment.
  #
  # I found a similar method in the literature but don't have the reference handy. The method below seems to work
  # pretty well provided the TAOs are actual trees. When the TAOs are only partial trees or several trees, the
  # result isn't as good (but still not too bad).
  NormalizedPercentileData <- PercentileData[, 4:103] / PercentileData[, 103]
  NormalizedPercentileData$Identifier <- PercentileData$Identifier
  NormalizedPercentileData$Plot <- PercentileData$DataFile
  NormalizedPercentileData$Label <- basename(PercentileData$DataFile)
  NormalizedPercentileData$cbh <- 0.0
  
  x <- c(1:99)
  for (i in 1:nrow(PercentileData)) {
    slopes <- vector()
    # compute slopes
    for (j in 2:99) {
      x1 <- x[j - 1]
      x2 <- x[j]
      y1 <- NormalizedPercentileData[i, j -1]
      y2 <- NormalizedPercentileData[i, j]
      slope_i <- (y2-y1)/(x2-x1)
      slopes <- append(slopes, slope_i)
    }
    
    # get max slope
    maxSlopeIndex <- which.max(slopes)
    cbhIndex <- maxSlopeIndex + 1
    NormalizedPercentileData$cbh[i] <- PercentileData[i, cbhIndex + 4]
  }
  
  # cbh for trees is saved with the NormalizedPercentileData
  # I should probably save this to a separate file...
  
  # *****************************************************************************
  # *****************************************************************************
  # work with crown polygons to get centroid and crown dimensions...
  # this gives us a polygon area to use to compute a crown diameter.
  # The centroid of the crown polygons provide an alternate location for the
  # trees. However, I found that the actual highpoint location from the
  # segmentation was a better match for field data.
  # *****************************************************************************
  # *****************************************************************************
  library(sf)
  treePolyFile <- paste0(outputFolder, "/Trees/trees_Polygons.shp")
  
  # read crown polygons
  treePolys <- st_read(treePolyFile)
  
  # this prevents a warning
  st_agr(treePolys) <- "constant"
  
  # compute centroids and get them as simple table
  centroids <- st_coordinates(st_centroid(treePolys))
  
  # rename columns
  colnames(centroids) <- c("centroidX", "centroidY")
  
  # add centroids to polygon attributes
  treePolys <- cbind(treePolys, centroids)
  
  # merge with high point data including ground elevation under high XY
  treePolys <- merge(treePolys
                     , thp[, c("Identifier", "High.point.X", "High.point.Y", "High.point.elevation", "GroundElev")]
                     , by.x = "BasinID"
                     , by.y = "Identifier")
  
  # merge in cbh
  treePolys <- merge(treePolys
                     , NormalizedPercentileData[, c("Identifier", "cbh")]
                     , by.x = "BasinID"
                     , by.y = "Identifier")
  
  # compute crown diameter using polygon area
  treePolys$CrownDiaByArea <- sqrt(treePolys$PolyArea / pi) * 2.0
  
  # write geopackage...preserves column names that are too long for shapefiles
  st_write(treePolys, paste0(outputFolder, "/Trees/trees_Polygons.gpkg"), append = FALSE)
  
  # build new data frame for FUSION tree objects...allows use to display tree models in the point cloud.
  # FUSION tree objects:
  # ID, X, Y, Z, ht, cbh, min crown dia, max crown dia, rotation, R, G, B
  #
  # options for XY: high point, centroid of crown polygon, detected stem location
  # options for Z: 0.0, ground elevation under XY point
  # options for crown diameters: diameter of circle with same area as crown polygon (rotation=0)
  #       major/minor axis of ellipse fit to crown polygon (rotation=computed)
  #       W-E and S-N using crown polygon  (rotation=0)
  #
  # high point XY seems to align better with stem hits
  tp <- st_drop_geometry(treePolys)
  fTrees <- data.frame(ID = tp$BasinID
                       , X = tp$High.point.X
                       , Y = tp$High.point.Y
                       #                     , X = tp$centroidX
                       #                     , Y = tp$centroidY
                       , Z = 0.0
                       , ht = tp$High.point.elevation - tp$GroundElev
                       , cbh = tp$cbh
                       , minDia = tp$CrownDiaByArea
                       , maxDia = tp$CrownDiaByArea
                       , rotation = 0.0
                       , R = 255
                       , G = 127
                       , B = 0)
  write.csv(fTrees, paste0(outputFolder, "/Trees/FUSIONtrees.csv"), row.names = FALSE)
}



#### Ally note: below is code specifically for OESF_Cedar_Plots_8_9_14, which was too big to run all at once (needed to be broken up) ####

# Ally comment: clipping the laz files into different parts
OESF_Cedar_Plots_8_9_14 <- readLAScatalog("F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14")
locator(1)

OESF_Cedar_Plots_8_9_14_pt1 <- clip_rectangle(OESF_Cedar_Plots_8_9_14, 410831,5294832,411321,5294996)
print(OESF_Cedar_Plots_8_9_14_pt1)
writeLAS(OESF_Cedar_Plots_8_9_14_pt1, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14", fileext = ".laz"))

OESF_Cedar_Plots_8_9_14_pt2 <- clip_rectangle(OESF_Cedar_Plots_8_9_14, 410831,5294996,411321,5295169)
print(OESF_Cedar_Plots_8_9_14_pt2)
writeLAS(OESF_Cedar_Plots_8_9_14_pt2, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14", fileext = ".laz"))

OESF_Cedar_Plots_8_9_14_pt3 <- clip_rectangle(OESF_Cedar_Plots_8_9_14, 410831,5295169,411321,5295308)
print(OESF_Cedar_Plots_8_9_14_pt3)
writeLAS(OESF_Cedar_Plots_8_9_14_pt3, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14", fileext = ".laz"))

OESF_Cedar_Plots_8_9_14_pt4 <- clip_rectangle(OESF_Cedar_Plots_8_9_14, 410831,5295308,411321,5295391)
print(OESF_Cedar_Plots_8_9_14_pt4)
writeLAS(OESF_Cedar_Plots_8_9_14_pt4, tempfile(tmpdir = "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14", fileext = ".laz"))

# NOTE: I can't seem to go above the 5295391 line because I keep getting the error "gpstime contains some NAs" and I can't figure out how to fix that.
  # Just going to drop the data at the top of the lidar coverage for now; hopefully that doesn't cut off part of the plot at the top

# Made new folders; 1 for each new part. Copy and pasted folder system and ground files over from original plot 

# making new dirs list for new OESF_Cedar_Plots_8_9_14 folders

dirs <- c("F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt1", 
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt2", 
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt3", 
          "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt4")

for (i in 1:length(dirs)) {
  #for (i in 1:1) {
  # set up folder info
  dataFolder <- dirs[i]
  groundFileSpec <- paste0(dirs[i], "/ground/ground.dtm")
  
  outputFolder <- paste0(dirs[i], "/Processing")
  
  # *****************************************************************************
  # *****************************************************************************
  # build CSM and CHM, do segmentation and compute metrics for Tree Approximate Objects (TAOs)
  # *****************************************************************************
  # *****************************************************************************
  # header things for batch file
  batchFile <- paste0(outputFolder, "/DoProcessing.bat")
  
  # set default behavior for commands...basically telling the fusionwrapr package to create a
  # batch file instead of running the commands directly.
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)
  
  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))
  
  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)
  
  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Processing.log"), logClear = TRUE)
  
  # add comment
  addToCommandFile("Start of processing commands")
  
  # create ground model...already have these for all areas in ground folder
  # however, the naming for the models isn't consistent
  
  # create CSM and CHM
  # Resolution and smoothing with the canopy surfaces have a major effect on the segmentation
  # behavior. I started with 0.5m cells and no smoothing
  CanopyModel(paste0(outputFolder, "/CSM/CSM.dtm")
              , 0.5
              , "M"
              , "M"
              , 1
              , 10
              , 2
              , 2
              , paste0(dataFolder, "/*.laz")
              , smooth = 3
              , peaks = FALSE
              , class = "~7,18"
  )
  
  CanopyModel(paste0(outputFolder, "/CHM/CHM.dtm")
              , 0.5
              , "M"
              , "M"
              , 1
              , 10
              , 2
              , 2
              , paste0(dataFolder, "/*.laz")
              , ground = groundFileSpec
              , smooth = 3
              , peaks = FALSE
              , class = "~7,18"
  )
  
  # run segmentation to produce normalized TAO clips
  # omit the ground points (class 2)
  TreeSeg(paste0(outputFolder, "/CHM/CHM.dtm")
          , 2
          , paste0(outputFolder, "/Trees/trees_normalized.csv")
          , shape = TRUE
          , ptheight = TRUE
          , points = paste0(dataFolder, "/*.laz")
          , class = "~2"
          , clipfolder = paste0(outputFolder, "/Trees/TAOpts_normalized")
          , ground = groundFileSpec
          , comment = "Create normalized TAO point clips"
          , projection = prjFile
  )
  
  # run segmentation to produce non-normalized TAO clips
  # omit the ground points (class 2)
  TreeSeg(paste0(outputFolder, "/CHM/CHM.dtm")
          , 2
          , paste0(outputFolder, "/Trees/trees.csv")
          , shape = TRUE
          , points = paste0(dataFolder, "/*.laz")
          , class = "~2"
          , clipfolder = paste0(outputFolder, "/Trees/TAOpts")
          , comment = "Create TAO point clips with point elevations"
          , projection = prjFile
  )
  
  # compute metrics for both sets of TAOs
  # use height thresholds for normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_normalized/*.lda")
               , paste0(outputFolder, "/TAO_normalized_metrics.csv")
               , new = TRUE
               , minht = 2.0
               , above = 2.0
               , rid = TRUE
               , pa = TRUE
  )
  
  # no height thresholds for non-normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts/*.lda")
               , paste0(outputFolder, "/TAO_metrics.csv")
               , new = TRUE
               , rid = TRUE
               , pa = TRUE
  )
  
  useLogFile("")
  
  # run the batch file
  runCommandFile()
  
  # *****************************************************************************
  # *****************************************************************************
  # clip the upper portion of the TAOs, compute metrics and get the highest
  # point for each treetop
  # *****************************************************************************
  # *****************************************************************************
  # do the treetops...upper portion of crown
  
  # reset options
  resetGlobalCommandOptions()
  
  batchFile <- paste0(outputFolder, "/DoProcessing2.bat")
  
  # set default behavior for commands
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)
  
  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))
  
  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)
  
  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Processing.log"), logClear = TRUE)
  
  # add comment
  addToCommandFile("Start of processing commands")
  
  topDepth <- 3
  
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  
  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth
  
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(outputFolder, "/TreeTops/", m$FileTitle[i], ".lda")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
    )
  }
  
  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(outputFolder, "/TreeTops_normalized/", m$FileTitle[i], ".lda")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
    )
  }
  
  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/TreeTops_normalized/", "*.lda")
               , paste0(outputFolder, "/TreeTops_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  CloudMetrics(paste0(outputFolder, "/TreeTops/", "*.lda")
               , paste0(outputFolder, "/TreeTop_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  # get the highpoint for trees...takes advantage of the /highpoint option in cloudmetrics
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts/*.lda")
               , paste0(outputFolder, "/TAO_metrics_highpts.csv")
               , new = TRUE
               , rid = TRUE
               , highpoint = TRUE
  )
  
  useLogFile("")
  
  # run the batch file
  runCommandFile()
  
  # reset options...FUSION commands below will run directly
  resetGlobalCommandOptions()
  
  # *****************************************************************************
  # *****************************************************************************
  # merge information from segmentation, metrics, and actual highpoint XY
  # *****************************************************************************
  # *****************************************************************************
  
  # use the /highpoint option with cloudmetrics to get the XY of the highest point in a TAO or treetop
  treeHighPtFile <- paste0(outputFolder, "/TAO_metrics_highpts.csv")
  thp <- read.csv(treeHighPtFile)
  
  # get surface values (ground elevations)
  thp <- GetSurfaceValues(thp
                          , xLabel = "High.point.X"
                          , yLabel = "High.point.Y"
                          , idLabel = "GroundElev"
                          , surfaceFile = groundFileSpec
  )
  
  # *****************************************************************************
  # use the ground elevation to bias the points for the TAOs
  # *****************************************************************************
  # ***** this could be better placed in the processing order but we need the ground elevation under the high point
  files <- Sys.glob(paste0(outputFolder, "/Trees/TAOpts/*.lda"))
  for (i in 1:length(files)) {
    ClipData(thp$DataFile[i]
             , paste0(outputFolder, "/Trees/TAOpts_GroundBiased/", basename(thp$DataFile[i]))
             , biaselev = -thp$GroundElev[i]
             , class = "~2"
    )
  }
  
  # no height thresholds for ground-normalized points...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_GroundBiased/*.lda")
               , paste0(outputFolder, "/TAO_GroundBiased_metrics.csv")
               , new = TRUE
               , rid = TRUE
               , pa = TRUE
  )
  
  # match TAOs from the segmentation with the metrics...need to do this for normalized
  # and non-normalized trees
  treeInfoFile <- paste0(outputFolder, "/Trees/trees_Polygons.csv")
  t <- read.csv(treeInfoFile)
  m <- read.csv(paste0(outputFolder, "/TreeTop_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TreeTop_metrics_merged.csv"), row.names = FALSE)
  
  m <- read.csv(paste0(outputFolder, "/TreeTops_normalized_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TreeTops_normalized_metrics_merged.csv"), row.names = FALSE)
  
  # do the full tree metrics
  # ***** these are for the TAOs normalized using terrain...not simple bias for ground under high point
  m <- read.csv(paste0(outputFolder, "/TAO_normalized_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TAO_normalized_metrics_merged.csv"), row.names = FALSE)
  
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  
  merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  merged <- merge(merged, thp, by = "Identifier")
  write.csv(merged, paste0(outputFolder, "/TAO_metrics_merged.csv"), row.names = FALSE)
  
  # *****************************************************************************
  # *****************************************************************************
  # compute crown base height using the individual percentile data (P1,P2,P3,...)
  # *****************************************************************************
  # *****************************************************************************
  # read the percentile data
  PercentileData <- read.csv(paste0(outputFolder, "/TAO_GroundBiased_metrics_percentile.csv"), stringsAsFactors = FALSE)
  
  # Normalize the percentile heights using the 99th percentile. Output has the object identifier (extra column at beginning).
  # percentile heights start in column 4 and P99 is column 103
  # normalized in this context means divided by the P99 value...not normalized relative to ground elevation
  #
  # Basic idea is to look at the "plot" of normalized percentile heights (relative to P99) and find the
  # segment with the steepest slope. The base height is then set to the height at the upper end of this segment.
  #
  # I found a similar method in the literature but don't have the reference handy. The method below seems to work
  # pretty well provided the TAOs are actual trees. When the TAOs are only partial trees or several trees, the
  # result isn't as good (but still not too bad).
  NormalizedPercentileData <- PercentileData[, 4:103] / PercentileData[, 103]
  NormalizedPercentileData$Identifier <- PercentileData$Identifier
  NormalizedPercentileData$Plot <- PercentileData$DataFile
  NormalizedPercentileData$Label <- basename(PercentileData$DataFile)
  NormalizedPercentileData$cbh <- 0.0
  
  x <- c(1:99)
  for (i in 1:nrow(PercentileData)) {
    slopes <- vector()
    # compute slopes
    for (j in 2:99) {
      x1 <- x[j - 1]
      x2 <- x[j]
      y1 <- NormalizedPercentileData[i, j -1]
      y2 <- NormalizedPercentileData[i, j]
      slope_i <- (y2-y1)/(x2-x1)
      slopes <- append(slopes, slope_i)
    }
    
    # get max slope
    maxSlopeIndex <- which.max(slopes)
    cbhIndex <- maxSlopeIndex + 1
    NormalizedPercentileData$cbh[i] <- PercentileData[i, cbhIndex + 4]
  }
  
  # cbh for trees is saved with the NormalizedPercentileData
  # I should probably save this to a separate file...
  
  # *****************************************************************************
  # *****************************************************************************
  # work with crown polygons to get centroid and crown dimensions...
  # this gives us a polygon area to use to compute a crown diameter.
  # The centroid of the crown polygons provide an alternate location for the
  # trees. However, I found that the actual highpoint location from the
  # segmentation was a better match for field data.
  # *****************************************************************************
  # *****************************************************************************
  library(sf)
  treePolyFile <- paste0(outputFolder, "/Trees/trees_Polygons.shp")
  
  # read crown polygons
  treePolys <- st_read(treePolyFile)
  
  # this prevents a warning
  st_agr(treePolys) <- "constant"
  
  # compute centroids and get them as simple table
  centroids <- st_coordinates(st_centroid(treePolys))
  
  # rename columns
  colnames(centroids) <- c("centroidX", "centroidY")
  
  # add centroids to polygon attributes
  treePolys <- cbind(treePolys, centroids)
  
  # merge with high point data including ground elevation under high XY
  treePolys <- merge(treePolys
                     , thp[, c("Identifier", "High.point.X", "High.point.Y", "High.point.elevation", "GroundElev")]
                     , by.x = "BasinID"
                     , by.y = "Identifier")
  
  # merge in cbh
  treePolys <- merge(treePolys
                     , NormalizedPercentileData[, c("Identifier", "cbh")]
                     , by.x = "BasinID"
                     , by.y = "Identifier")
  
  # compute crown diameter using polygon area
  treePolys$CrownDiaByArea <- sqrt(treePolys$PolyArea / pi) * 2.0
  
  # write geopackage...preserves column names that are too long for shapefiles
  st_write(treePolys, paste0(outputFolder, "/Trees/trees_Polygons.gpkg"), append = FALSE)
  
  # build new data frame for FUSION tree objects...allows use to display tree models in the point cloud.
  # FUSION tree objects:
  # ID, X, Y, Z, ht, cbh, min crown dia, max crown dia, rotation, R, G, B
  #
  # options for XY: high point, centroid of crown polygon, detected stem location
  # options for Z: 0.0, ground elevation under XY point
  # options for crown diameters: diameter of circle with same area as crown polygon (rotation=0)
  #       major/minor axis of ellipse fit to crown polygon (rotation=computed)
  #       W-E and S-N using crown polygon  (rotation=0)
  #
  # high point XY seems to align better with stem hits
  tp <- st_drop_geometry(treePolys)
  fTrees <- data.frame(ID = tp$BasinID
                       , X = tp$High.point.X
                       , Y = tp$High.point.Y
                       #                     , X = tp$centroidX
                       #                     , Y = tp$centroidY
                       , Z = 0.0
                       , ht = tp$High.point.elevation - tp$GroundElev
                       , cbh = tp$cbh
                       , minDia = tp$CrownDiaByArea
                       , maxDia = tp$CrownDiaByArea
                       , rotation = 0.0
                       , R = 255
                       , G = 127
                       , B = 0)
  write.csv(fTrees, paste0(outputFolder, "/Trees/FUSIONtrees.csv"), row.names = FALSE)
}





