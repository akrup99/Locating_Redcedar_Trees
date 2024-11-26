# Ally Kruper
# Last updated 4/2/2024
# Goal: Process matched ground to lidar trees for cloud metrics

# THIS CODE IS MODIFIED BY CODE ORIGINALLY WRITTEN BY BOB MCGAUGHEY THAT CAN BE FOUND ON HIS GITHUB HERE: https://github.com/bmcgaughey1/DroneLidarCode/blob/main/Rcode/ClipAdjTreeData.R


# Unfortunately, reclipping using the leaning trees is not a fast process!!
library(lidR)
library(tictoc)
library(fusionwrapr)
setwd("F:/workspace/OESF/AllyCedarFUSIONWork/")
setFUSIONpath("F:/workspace/OESF/AllyCedarFUSIONWork/FUSION")

dist3d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  v3 <- cross3d_prod(v1,v2)
  area <- sqrt(sum(v3*v3))/2
  d <- 2*area/sqrt(sum(v1*v1))
}

cross3d_prod <- function(v1,v2){
  v3 <- vector()
  v3[1] <- v1[2]*v2[3]-v1[3]*v2[2]
  v3[2] <- v1[3]*v2[1]-v1[1]*v2[3]
  v3[3] <- v1[1]*v2[2]-v1[2]*v2[1]
  return(v3)
}


#### Ally comment: code for averaging FUSION tree positions from 2021 data --> not relavent for cedar data (commenting out) ####
# Ally comment: the below code is on combining mine and Bob's adjusted trees from 2021 --> didn't do that for cedars (2021 locations ended up pretty close --> 
  # only I did the cedar adjustments) --> commenting out that code

# 
# # this file has all of the field trees that were adjusted by Ally and Bob. Not
# # all of these were successfully matched to a lidar-segmented tree so there are more trees
# # here that the file that has lidar metrics used to copy the segmented trees later in this code.
# allTreesOriginal <- read.csv(file = "extras/AdjustedTrees_AllPlots.csv", stringsAsFactors = FALSE)
# 
# # drop any trees where the base locations differ by more than 1m
# distThreshold <- 1
# allTrees <- allTreesOriginal[allTreesOriginal$diff <= distThreshold, ]
# 
# # drop trees where we had a height difference of 1m or more
# heightThreshold <- 1
# allTrees <- allTrees[allTrees$heightdiff <= heightThreshold, ]
# 
# # 624 original trees that were adjusted
# # 580 trees with adjusted base locations within 1m
# # 575 trees with adjusted base locations within 1m and heights within 1m
# 
# # compute averages
# allTrees$aveBaseX <- (allTrees$X.Ally + allTrees$X.Bob) / 2
# allTrees$aveBaseY <- (allTrees$Y.Ally + allTrees$Y.Bob) / 2
# allTrees$aveBaseElevation <- (allTrees$Elevation.Ally + allTrees$Elevation.Bob) / 2
# allTrees$aveHeight <- (allTrees$Total.Height.Ally + allTrees$Total.Height.Bob) / 2
# 
# for (i in 1:nrow(allTrees)) {
#   if (allTrees$Elevation.Ally[i] < 0 || allTrees$Elevation.Bob[i] < 0) {
#     allTrees$aveBaseElevation[i] <- max(allTrees$Elevation.Ally[i], allTrees$Elevation.Bob[i])
#   }
# }
# allTrees$aveTopX <- (allTrees$TopX.Ally + allTrees$TopX.Bob) / 2
# allTrees$aveTopY <- (allTrees$TopY.Ally + allTrees$TopY.Bob) / 2
# allTrees$topElevation <- allTrees$aveBaseElevation + allTrees$aveHeight
# 
# allTrees$aveCrownDia <- (allTrees$Max.Crown.Diameter.Ally + allTrees$Max.Crown.Diameter.Bob + allTrees$Min.Crown.Diameter.Ally + allTrees$Min.Crown.Diameter.Bob) / 4

#### Ally comment: setting up data ####
# Ally comment: first, need to combine all of my individual FUSIONtree files into one and call it "allTrees" (for simplicity, what Bob calls it)
## NOTE: Going to leave out plots 11, 12, and 13 for now becasue lidar was flown in different year --> need to figure out best way to get that info and compile it -->
# will run plots 11, 12, 13 separately 

plot_5 <- read.csv("FUSIONtrees/plot_5_field_FUSIONtrees.csv", header = TRUE)
plot_6 <- read.csv("FUSIONtrees/plot_6_field_FUSIONtrees.csv", header = TRUE)
plot_7 <- read.csv("FUSIONtrees/plot_7_field_FUSIONtrees.csv", header = TRUE)
plot_8 <- read.csv("FUSIONtrees/plot_8_field_FUSIONtrees.csv", header = TRUE)
plot_9 <- read.csv("FUSIONtrees/plot_9_field_FUSIONtrees.csv", header = TRUE)
plot_10 <- read.csv("FUSIONtrees/plot_10_field_FUSIONtrees.csv", header = TRUE)
plot_14 <- read.csv("FUSIONtrees/plot_14_field_FUSIONtrees.csv", header = TRUE)
plot_15 <- read.csv("FUSIONtrees/plot_15_field_FUSIONtrees.csv", header = TRUE)
plot_16 <- read.csv("FUSIONtrees/plot_16_field_FUSIONtrees.csv", header = TRUE)
plot_17 <- read.csv("FUSIONtrees/plot_17_field_FUSIONtrees.csv", header = TRUE)
plot_18 <- read.csv("FUSIONtrees/plot_18_field_FUSIONtrees.csv", header = TRUE)
plot_19 <- read.csv("FUSIONtrees/plot_19_field_FUSIONtrees.csv", header = TRUE)
plot_20 <- read.csv("FUSIONtrees/plot_20_field_FUSIONtrees.csv", header = TRUE)
plot_21 <- read.csv("FUSIONtrees/plot_21_field_FUSIONtrees.csv", header = TRUE)
plot_22 <- read.csv("FUSIONtrees/plot_22_field_FUSIONtrees.csv", header = TRUE)
plot_23 <- read.csv("FUSIONtrees/plot_23_field_FUSIONtrees.csv", header = TRUE)
plot_24 <- read.csv("FUSIONtrees/plot_24_field_FUSIONtrees.csv", header = TRUE)
plot_25 <- read.csv("FUSIONtrees/plot_25_field_FUSIONtrees.csv", header = TRUE)
plot_26 <- read.csv("FUSIONtrees/plot_26_field_FUSIONtrees.csv", header = TRUE)
plot_27 <- read.csv("FUSIONtrees/plot_27_field_FUSIONtrees.csv", header = TRUE)

# a couple of plots for some reason have "Lean.Angle" instaed of "Lean.Angle.From.Vertical" --> need to fix that to combine
colnames(plot_9)[14] <- "Lean.Angle.From.Vertical"
colnames(plot_15)[14] <- "Lean.Angle.From.Vertical"
colnames(plot_18)[14] <- "Lean.Angle.From.Vertical"
colnames(plot_23)[14] <- "Lean.Angle.From.Vertical"

allTrees <- rbind(plot_5, plot_6, plot_7, plot_8, plot_9, plot_10, plot_14, plot_15, plot_16, plot_17, plot_18, plot_19, plot_20, 
                  plot_21, plot_22, plot_23, plot_24, plot_25, plot_26, plot_27)


 # setting up variables, imitating what Bob did
allTrees$BaseX <- allTrees$X
allTrees$BaseY <- allTrees$Y
allTrees$BaseElevation <- allTrees$Elevation
allTrees$Height <- allTrees$Total.Height
allTrees$topElevation <- allTrees$BaseElevation + allTrees$Height
allTrees$CrownDia <- allTrees$Max.Crown.Diameter + allTrees$Min.Crown.Diameter / 2
allTrees$TopX <- allTrees$X + (cos((450 - allTrees$Lean.Azimuth) * pi / 180.0) * sin(allTrees$Lean.Angle * pi / 180.0) * allTrees$Total.Height)
allTrees$TopY <- allTrees$Y + (sin((450 - allTrees$Lean.Azimuth) * pi / 180.0) * sin(allTrees$Lean.Angle * pi / 180.0) * allTrees$Total.Height)



# need to add back in Plot_Number
library(stringr)
Plot_Number <- str_replace(allTrees$Tree.ID, pattern = "(\\d+)_\\d+_(\\w\\w\\w\\w)", replacement = "\\1")
allTrees <- cbind(allTrees, Plot_Number)
# also species
Species <- str_replace(allTrees$Tree.ID,  pattern = "\\d+_\\d+_(\\w\\w\\w\\w)", replacement = "\\1")
allTrees <- cbind(allTrees, Species)

# also tag number
Tag_Num <- str_replace(allTrees$Tree.ID, pattern = "\\d+_(\\d+)_(\\w\\w\\w\\w)", replacement = "\\1")
allTrees <- cbind(allTrees, Tag_Num)

# removing trees with status codes 1, 2, and 4 (not LIDAR visible, buried in canopy, not correct species)
allTrees <- subset(allTrees, Status.Code != 1)
allTrees <- subset (allTrees, Status.Code != 2)
allTrees <- subset(allTrees, Status.Code != 4)

# end Ally's comments

# build a list of folders for plots...hand coded :-(
folders <- data.frame(Plot_Number = unique(allTrees$Plot_Number), Folder = "") 
folders$Folder <- c(
   "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt2/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt2/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt2/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt3/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt3/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plot_10/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_8_9_14_pt1/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_15_16/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_15_16/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_17_thru_22/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_17_thru_22/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_17_thru_22/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_17_thru_22/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_17_thru_22/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_17_thru_22/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_23_24_25/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_23_24_25/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_23_24_25/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_26_27/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/ONF_Cedar_Plot_26_27/"
)


#### Ally comment: doing the work ####
# idea is to clip points using the slanted tree represented by the base and top location
# and a crown diameter. This is done using the directed distance from a line but may require
# some tricks to clip the points below the base location and above the top location.
#
# work through a list of point files and then merge the clips into a single file

# Ally comment: code keeps failling after plot 10 (at plot 15) tried skipping it --> still failed --> tried only 1 laz file at a time --> this ran a lot faster, but through
    # that process, realized that the code is really including a lot more than the individual tree and that is increasing processing time --> decreasing the size of the 
    # bounding box (see code below --> going to comment out this code for now)
# going to do plot 15 only
# laz_dir <- c("ONF_Cedar_Plot_15_16_001.laz", "ONF_Cedar_Plot_15_16_002.laz", "ONF_Cedar_Plot_15_16_003.laz", "ONF_Cedar_Plot_15_16_004.laz", "ONF_Cedar_Plot_15_16_005.laz", "ONF_Cedar_Plot_15_16_006.laz")
# laz_dir_noExt <- c("laz_001", "laz_002", "laz_003", "laz_004", "laz_005", "laz_006")
#   
# for (i in 1:length(laz_dir)) {
#   tic(paste0("Clipping trees for plot ", folders$Plot_Number[2]))
#   trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[2],]
#   
#   pointFolder <- folders$Folder[2]
#   
#   # create output folder for new tree clips
#   verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees"))
#   
#   #lazFiles <- list.files(pointFolder, pattern = ".laz")
#   
#   tic(paste0("Plot ", folders$Plot_Number[2], ":Reading LAS catalog..."))
#   #pts <- readLAScatalog(pointFolder, select = "*.laz")
#   pts <- readLAS(paste0(pointFolder, laz_dir[i]))
#   toc()
# 
#   #  for (tree in 1:1) {
#   for (tree in 1:1) {
#     # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
#     b <- c(trees$BaseX[tree], trees$BaseY[tree], trees$BaseElevation[tree]) # base
#     c <- c(trees$TopX[tree], trees$TopY[tree], trees$topElevation[tree]) # top
#     
#     # build bounding box for tree
#     xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 4 # this was originally 2 --> changing b/c seems like too big of boxes
#     yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 4
#     xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 4
#     yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 4
#     
#     tic(paste0("Plot ", folders$Plot_Number[2], " Tree ", tree, ":Clipping to rough bounding box..."))
#     tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
#     toc()
#     
#     # set the classification value to 1 for points within the cylinder and 0 for points outside
#     # downside to this si that we lose the original classification code
#     tic(paste0("Plot ", folders$Plot_Number[2], " Tree ", tree, ":Clipping to tree..."))
#     for (l in 1:tpts@header$`Number of point records`) {
#       a <- c(tpts@data$X[l], tpts@data$Y[l], tpts@data$Z[l])
#       d <- dist3d(a,b,c)
#       if (d <= trees$CrownDia[tree] / 2) {
#         tpts@data$Classification[l] <- 1
#       }
#       else {
#         tpts@data$Classification[l] <- 0
#       }
#     }
#     toc()
#     
#     # keep only class 1 points
#     tpts <- filter_poi(tpts, Classification == 1)
#     
#     # reset the point classification to an integer type, not sure why it was changed
#     tpts@data$Classification <- as.integer(tpts@data$Classification)
#     
#     #plot(tpts)
#     
#     # write tree points
#     writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/", "Plot_", trees$Plot_Number[tree], "_Tree_", trees$Tag_Num[tree], laz_dir_noExt[i], "dia4", ".las"))
#   }
#   toc()
# }
# 
# # Ally Comment: below is the originally modified code that results in clipping to bounding boxes --> ultimately changed this to cone clipping for final results 
#   # (see below for final cone clipping code) (was better at excluding nearby branches coming in from other trees. which was a particular problem for OESF cedars)
# #for (plot in 1:1) {
# for (plot in 1:nrow(folders)) {
#   tic(paste0("Clipping trees for plot ", folders$Plot_Number[plot]))
#   trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[plot],]
# 
#   pointFolder <- folders$Folder[plot]
# 
#   # create output folder for new tree clips
#   verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees"))
# 
#   #lazFiles <- list.files(pointFolder, pattern = ".laz")
# 
#   tic(paste0("Plot ", folders$Plot_Number[plot], ":Reading LAS catalog..."))
#   pts <- readLAScatalog(pointFolder, select = "*.laz")
#   toc()
# 
# #  for (tree in 1:1) {
#   for (tree in 1:nrow(trees)) {
#     # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
#     b <- c(trees$BaseX[tree], trees$BaseY[tree], trees$BaseElevation[tree]) # base
#     c <- c(trees$TopX[tree], trees$TopY[tree], trees$topElevation[tree]) # top
# 
#     # build bounding box for tree
#     # Ally comment: was originally divide the crown diameter by 4 --> that was taking too long to process (i.e. overnight for a single tree) and was getting the surrounding
#       # trees for the larger trees --> adding an if/else statement and for the trees larger than 15cm DBH, going to divide diameter by 4 instead of 2 (tried 4 for plots 
#       # and that seemed to work well for large trees; too small for small trees)
#    
#      if (trees$DBH[tree] <= 0.15) {
#       xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 2 
#       yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 2
#       xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 2
#       yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 2
#       
#     } else {
#       
#       xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 4 
#       yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 4
#       xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 4
#       yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 4
#     }
# 
#     tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to rough bounding box..."))
#     tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
#     toc()
# 
#     # set the classification value to 1 for points within the cylinder and 0 for points outside
#     # downside to this si that we lose the original classification code
#     tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to tree..."))
#     for (i in 1:tpts@header$`Number of point records`) {
#       a <- c(tpts@data$X[i], tpts@data$Y[i], tpts@data$Z[i])
#       d <- dist3d(a,b,c)
#       if (d <= trees$CrownDia[tree] / 2) {
#         tpts@data$Classification[i] <- 1
#       }
#       else {
#         tpts@data$Classification[i] <- 0
#       }
#     }
#     toc()
# 
#     # keep only class 1 points
#     tpts <- filter_poi(tpts, Classification == 1)
# 
#     # reset the point classification to an integer type, not sure why it was changed
#     tpts@data$Classification <- as.integer(tpts@data$Classification)
# 
#     #plot(tpts)
# 
#     # write tree points
#     writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/", "Plot_", trees$Plot_Number[tree], "_Tree_", trees$Tag_Num[tree], ".las"))
#   }
#   toc()
# }

# Ally comment: Below is code for clipping trees to a cone instead of a bounding box. Was a little too harsh at the top of trees --> added 0.5 to the top of trees so cone is 
  # starting slightly more above trees; also was clipping TSHE too much  do to droopy top --> added if/else statement with bigger angles for TSHE; also added if/else
  # statements for bigger trees (2 levels for TSHE, 3 for THPL because gets so big at ONF)

for (plot in 5:nrow(folders)) { # starting at 5 b/c mysteriously failed at plot 9 tree 206 due to "0 points" --> removed that tree (gave it anomaly 1)
  tic(paste0("Clipping trees for plot; cone clip ", folders$Plot_Number[plot]))
  trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[plot],]
  
  pointFolder <- folders$Folder[plot]
  
  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final"))
  
  #lazFiles <- list.files(pointFolder, pattern = ".laz")
  
  tic(paste0("Plot ", folders$Plot_Number[plot], ":Reading LAS catalog..."))
  pts <- readLAScatalog(pointFolder, select = "*.laz")
  toc()
  
  # for (tree in 1:1) {
  for (tree in 1:nrow(trees)) {
    # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
    b <- c(trees$BaseX[tree], trees$BaseY[tree], trees$BaseElevation[tree]) # base
    c <- c(trees$TopX[tree], trees$TopY[tree], trees$topElevation[tree]) # top
    
    # build bounding box for tree
    # Ally comment: was originally divide the crown diameter by 4 --> that was taking too long to process (i.e. overnight for a single tree) and was getting the surrounding
    # trees for the larger trees --> adding an if/else statement and for the trees larger than 15cm DBH, going to divide diameter by 4 instead of 2 (tried 4 for plots 
    # and that seemed to work well for large trees; too small for small trees)
    
    if (trees$DBH[tree] <= 0.15) {
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 2 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 2
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 2
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 2
      
    } else {
      
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 4 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 4
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 4
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 4
    }
    
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to rough bounding box..."))
    tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
    toc()
    
    
    # set the classification value to 1 for points within the cone and 0 for points outside
    # downside to this si that we lose the original classification code
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to tree cone..."))
    if (trees$Species[tree] == "TSHE") {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-45/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      } else {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-60/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      }
    } else {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-30/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          }else {
            tpts@data$Classification[i] <- 2 }
        }
      } else {
        if (trees$DBH[tree] <= 0.60) {
          for (i in 1:tpts@header$`Number of point records`) {
            
            # compute horizontal and vertical distance from apex XY to each point
            hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
            vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
            
            # precompute some things
            tanAngle <- tan(-45/2 * pi/180)
            
            if (hdist <= (tanAngle * vdist)) {
              tpts@data$Classification[i] <- 1
            }else {
              tpts@data$Classification[i] <- 2 }
          }
        } else {
          for (i in 1:tpts@header$`Number of point records`) {
            
            # compute horizontal and vertical distance from apex XY to each point
            hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
            vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
            
            # precompute some things
            tanAngle <- tan(-60/2 * pi/180)
            
            if (hdist <= (tanAngle * vdist)) {
              tpts@data$Classification[i] <- 1
            } else {
              tpts@data$Classification[i] <- 2}
          }
        }
      }
    }
    
    
    
    toc()
    
    # keep only class 1 points
    tpts <- filter_poi(tpts, Classification == 1)
    
    # reset the point classification to an integer type, not sure why it was changed
    tpts@data$Classification <- as.integer(tpts@data$Classification)
    
    #plot(tpts)
    
    # write tree points
    writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/", "Plot_", trees$Plot_Number[tree], "_TreeCone_", trees$Tag_Num[tree], ".las"))
  }
  toc()
}


# Ally comment: I *think* that the following code primarily exists because Bob was re-running the analysis and wanted to test another old file to double check things. I 
  # am not going to run this code because I don't have that different data file with a BasinID number. Not running this code appears like it will not impact thing down the line
# # match trees on plots to their segmented tree and copy the segmented trees to a new folder and relabel
# # them to match the labeling used above.
# #
# # NOTE: there are fewer segmented trees than clipped trees from the code above due to differences in the CSV files
# # used. The file used here are the field trees that were actually matched to lidar segmented trees
# #
# # Use a different data file (AdjustedField_T3_Training_TreeTops_AllPlots.csv) that has the basin ID and test to see that I get the same set of trees.
# #
# # Not sure of the source for the crown widths in the adjusted tree files. I don't remember if
# tTrees <- read.csv(file = "extras/AdjustedField_T3_Training_TreeTops_AllPlots.csv", stringsAsFactors = FALSE)
# 
# # drop any trees where the base locations differ by more than 1m
# distThreshold <- 1
# tTrees <- tTrees[tTrees$diff <= distThreshold, ]
# 
# # drop trees where we had a height difference of 1m or more
# heightThreshold <- 1
# tTrees <- tTrees[tTrees$heightdiff <= heightThreshold, ]
# 
# for (plot in 1:nrow(folders)) {
#   trees <- tTrees[tTrees$Plot_Number == folders$Plot_Number[plot],]
# 
#   pointFolder <- folders$Folder[plot]
# 
#   # create output folder for new tree clips
#   verifyFolder(paste0(pointFolder, "Processing/SegmentedTrees"))
# 
#   for (tree in 1:nrow(trees)) {
#     file.copy(paste0(pointFolder, "Processing/Trees/TAOpts_GroundBiased/", "Trees_Clip_", sprintf("%07i", trees$BasinID[tree]), ".lda"),
#               paste0(pointFolder, "Processing/SegmentedTrees/", "Plot_", trees$Plot_Number[tree], "_Tree_", trees$Tag_Num[tree], ".lda"),
#               overwrite = TRUE)
#   }
# }



# compute the metrics for the upper portion of the crowns for the new tree clips. May also want to reduce the size of the cylinder
# This would be faster if I just use the top XY location and ignore lean
# ideally, the format of the metrics would match that in AdjustedField_T3_Training_TreeTops_AllPlots.csv so I can use the
# same model fitting code
# using the original tree file of adjusted trees to compute metrics

# Ally comment: just going to keep the allTrees object already in use
# allTrees <- read.csv(file = "extras/AdjustedTrees_AllPlots.csv", stringsAsFactors = FALSE)

# Ally comment: following is commented out because only applies if two people are doing the processing
# # drop any trees where the base locations differ by more than 1m
# distThreshold <- 1
# allTrees <- allTrees[allTrees$diff <= distThreshold, ]
# 
# # drop trees where we had a height difference of 1m or more
# heightThreshold <- 1
# allTrees <- allTrees[allTrees$heightdiff <= heightThreshold, ]

# # compute averages
# allTrees$BaseX <- allTrees$X 
# allTrees$BaseY <- allTrees$Y
# allTrees$BaseElevation <- allTrees$Elevation
# allTrees$Height <- allTrees$Total.Height
# 
# for (i in 1:nrow(allTrees)) {
#   if (allTrees$Elevation.Ally[i] < 0 || allTrees$Elevation.Bob[i] < 0) {
#     allTrees$aveBaseElevation[i] <- max(allTrees$Elevation.Ally[i], allTrees$Elevation.Bob[i])
#   }
# }
# allTrees$TopX <- allTrees$TopX
# allTrees$TopY <- allTrees$TopY
# allTrees$topElevation <- allTrees$BaseElevation + allTrees$Height
# 
# allTrees$CrownDia <- allTrees$Max.Crown.Diameter + allTrees$Min.Crown.Diameter / 2

# reset options
resetGlobalCommandOptions()

# set default behavior for commands
setGlobalCommandOptions(runCmd = TRUE, saveCmd = FALSE, echoCmd = FALSE)


## Ally comment: not doing below because some trees have larger trees leaning slightly into their bounding box --> would skew the max elevation --> just going to do 'small cylinder' section below

# topDepth <- 3
# 
# # work through the folders and run metrics for adjusted tree clips
# #for (plot in 1:2) {
# for (plot in 1:nrow(folders)) {
#     tic(paste0("Computing metrics for plot ", folders$Plot_Number[plot]))
#   pointFolder <- folders$Folder[plot]
# 
#   # create output folder for new tree clips
#   outFile <- paste0(pointFolder, "Processing/AdjustedTrees/metrics.csv")
# 
#   CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/", "Plot_", folders$Plot_Number[plot], "*.las")
#                , outFile
#                , new = TRUE
#                , rid = TRUE
#   )
# 
#   # read the metrics to get the high point
#   # read the metrics for the non-normalized points and get the highest elevation
#   m <- read.csv(outFile, stringsAsFactors = FALSE)
# 
#   # compute the elevation for the base of the upper portion
#   m$SampleBaseElev <- m$Elev.maximum - topDepth
# 
#   # build commands to clip to upper portion of each TAO
#   for (i in 1:nrow(m)) {
#     ClipData(m$DataFile[i]
#              , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops/", m$FileTitle[i], ".las")
#              , zmin = m$SampleBaseElev[i]
#              , zmax = m$Elev.maximum[i]
#     )
#   }
# 
#   # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
#   # bias is added to point height so it needs to be negative
#   # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
#   for (i in 1:nrow(m)) {
#     ClipData(m$DataFile[i]
#              , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized/", m$FileTitle[i], ".las")
#              , zmin = 0
#              , zmax = topDepth
#              , biaselev = -m$SampleBaseElev[i]
#     )
#   }
# 
#   # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
#   CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
#                , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized", "/TreeTops_normalized_metrics.csv")
#                , new = TRUE
#                , rid = TRUE
#   )
# 
#   m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized", "/TreeTops_normalized_metrics.csv"), stringsAsFactors = FALSE)
#   m$Plot_Number <- folders$Plot_Number[plot]
# 
#   # merge metrics and field data
#   merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))
# 
#   write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized",
#                            "/Leaning_TreeTops_normalized_metrics_Plot",
#                            sprintf("%02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)
# 
#   if (plot == 1) {
#     allMerged <- merged
#   }
#   else {
#     allMerged <- rbind(allMerged, merged)
#   }
# }
# 
# # write off combined file
# write.csv(allMerged, paste0("F:workspace/OESF/AllyCedarFUSIONWork", "/Leaning_TreeTops_normalized_metrics.csv"), row.names = FALSE)
# 





# small cylinder clips

# work through the folders, clip smaller cylinder for the tree and compute metrics

# Ally comment: going to have the sample radius clipped to change based on size of tree 

sampleRadius <- 1
topDepth <- 3

#for (plot in 1:2) {
for (plot in 1:nrow(folders)) {
  tic(paste0("Computing metrics for plot ", folders$Plot_Number[plot]))
  pointFolder <- folders$Folder[plot]

  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/metrics.csv")

  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )

  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)

  
  # this is the fix for 10/25/2023 problem...keeping code above to get FileTitle
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/", m$FileTitle[i], ".las")
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }

  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/SmallCylindermetrics.csv")

  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )

  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)

  # ERROR: 10/25/2023: original code used the leaning tree clip with an estimated crow width
  # to get the max elevation. If there are branches from an adjacent tree above the target
  # tree, there may be no points in the small cylinder clip so we loose the tree.
  # Logic should be using the max elevation in the 1m cylinder, not the entire tree clip.
  #
  # easy fix is to do the small cylinder clips and keep all points, run cloudmetrics to get
  # the max elevation, then use this to clip the top

  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth

  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder/", m$FileTitle[i], ".las")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }

  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized/", m$FileTitle[i], ".las")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }

  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
               , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )

  m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
  m$Plot_Number <- folders$Plot_Number[plot]

  # merge metrics and field data
  merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))

  write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized",
                           "/Leaning_TreeTops_SmallCylinder_normalized_metrics_Plot",
                           sprintf("%s02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)

  if (plot == 1) {
    allMerged <- merged
  }
  else {
    allMerged <- rbind(allMerged, merged)
  }
}

# write off combined file
write.csv(allMerged, file = "Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv", row.names = FALSE)

#### Code to get small cylinder data (variable radius) from cedar data instead of just top 3m ####
sampleRadius <- 1
topDepth <- 3

#for (plot in 1:2) {
for (plot in 1:nrow(folders)) {
  pointFolder <- folders$Folder[plot]
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/SmallCylindermetrics.csv")
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )
  m <- read.csv(outFile, stringsAsFactors = FALSE)
  
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    

    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized/", m$FileTitle[i], ".las")
             , zmin = 0
             , zmax = m$Elev.maximum[i] - m$Elev.minimum[i]
             , biaselev = -m$Elev.minimum[i]
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }
  
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
               , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized", "/Trees_SmallCylinder_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized", "/Trees_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
  m$Plot_Number <- folders$Plot_Number[plot]
  
  # merge metrics and field data
  merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))
  
  write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized",
                           "/Leaning_Trees_SmallCylinder_normalized_metrics_Plot",
                           sprintf("%s02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)
  
  if (plot == 1) {
    allMerged <- merged
  }
  else {
    allMerged <- rbind(allMerged, merged)
  }
}
# write off combined file
write.csv(allMerged, file = "Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv", row.names = FALSE)




#### Below is code specifically for plots 11, 12, 13, which were flown with lidar from previous years and need a little extra work putting everything together file-wise ####
  # (also, like with plots_8_9_14, the files for plots_11_12_13 were too large --> needed to broken up into pieces)
  # plots 11 and 12 are in pt. 5; plot 13 is in pt. 6

# going to do cone clipping (like with the other plots)

library(lidR)
library(tictoc)
library(fusionwrapr)
setwd("F:/workspace/OESF/AllyCedarFUSIONWork/")
setFUSIONpath("F:/workspace/OESF/AllyCedarFUSIONWork/FUSION")

dist3d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  v3 <- cross3d_prod(v1,v2)
  area <- sqrt(sum(v3*v3))/2
  d <- 2*area/sqrt(sum(v1*v1))
}

cross3d_prod <- function(v1,v2){
  v3 <- vector()
  v3[1] <- v1[2]*v2[3]-v1[3]*v2[2]
  v3[2] <- v1[3]*v2[1]-v1[1]*v2[3]
  v3[3] <- v1[1]*v2[2]-v1[2]*v2[1]
  return(v3)
}

## setting up the data
plot_11 <- read.csv("FUSIONtrees/Plots with drone data flown in the past/plot_11_field_FUSIONtrees.csv", header = TRUE)
plot_12 <- read.csv("FUSIONtrees/Plots with drone data flown in the past/plot_12_field_FUSIONtrees.csv", header = TRUE)
plot_13 <- read.csv("FUSIONtrees/Plots with drone data flown in the past/plot_13_field_FUSIONtrees.csv", header = TRUE)


allTrees <- rbind(plot_11, plot_12, plot_13)


# setting up variables, imitating what Bob did
allTrees$BaseX <- allTrees$X
allTrees$BaseY <- allTrees$Y
allTrees$BaseElevation <- allTrees$Elevation
allTrees$Height <- allTrees$Total.Height
allTrees$topElevation <- allTrees$BaseElevation + allTrees$Height
allTrees$CrownDia <- allTrees$Max.Crown.Diameter + allTrees$Min.Crown.Diameter / 2
allTrees$TopX <- allTrees$X + (cos((450 - allTrees$Lean.Azimuth) * pi / 180.0) * sin(allTrees$Lean.Angle * pi / 180.0) * allTrees$Total.Height)
allTrees$TopY <- allTrees$Y + (sin((450 - allTrees$Lean.Azimuth) * pi / 180.0) * sin(allTrees$Lean.Angle * pi / 180.0) * allTrees$Total.Height)


# need to add back in Plot_Number
library(stringr)
Plot_Number <- str_replace(allTrees$Tree.ID, pattern = "(\\d+)_\\d+_(\\w\\w\\w\\w)", replacement = "\\1")
allTrees <- cbind(allTrees, Plot_Number)
# also species
Species <- str_replace(allTrees$Tree.ID,  pattern = "\\d+_\\d+_(\\w\\w\\w\\w)", replacement = "\\1")
allTrees <- cbind(allTrees, Species)

# also tag number
Tag_Num <- str_replace(allTrees$Tree.ID, pattern = "\\d+_(\\d+)_(\\w\\w\\w\\w)", replacement = "\\1")
allTrees <- cbind(allTrees, Tag_Num)

# removing trees with status codes 1, 2, and 4 (not LIDAR visible, buried in canopy, not correct species)
allTrees <- subset(allTrees, Status.Code != 1)
allTrees <- subset (allTrees, Status.Code != 2)
allTrees <- subset(allTrees, Status.Code != 4)

# original DTM used in FUSION tree matching must have had some NoData holes in the DTMs because there are some trees with elevations of 
# -1 --> going to sort those out (mostly happened in plot 11, approximately 13 trees)
allTrees <- allTrees[which(allTrees$BaseElevation > 0),]

# end Ally's comments

# build a list of folders for plots...hand coded :-(
folders <- data.frame(Plot_Number = unique(allTrees$Plot_Number), Folder = "") 
folders$Folder <- c(
  "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt5/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt5/"
  , "F:/workspace/OESF/AllyCedarFUSIONWork/Cedar_Plots/OESF_Cedar_Plots_11_12_13_pt6/"
)




## initial cone clipping

for (plot in 1:nrow(folders)) { 
  tic(paste0("Clipping trees for plot; cone clip ", folders$Plot_Number[plot]))
  trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[plot],]
  
  pointFolder <- folders$Folder[plot]
  
  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final"))
  
  #lazFiles <- list.files(pointFolder, pattern = ".laz")
  
  tic(paste0("Plot ", folders$Plot_Number[plot], ":Reading LAS catalog..."))
  pts <- readLAScatalog(pointFolder, select = "*.laz")
  toc()
  
  # for (tree in 1:1) {
  for (tree in 1:nrow(trees)) {
    # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
    b <- c(trees$BaseX[tree], trees$BaseY[tree], trees$BaseElevation[tree]) # base
    c <- c(trees$TopX[tree], trees$TopY[tree], trees$topElevation[tree]) # top
    
    # build bounding box for tree
    # Ally comment: was originally divide the crown diameter by 4 --> that was taking too long to process (i.e. overnight for a single tree) and was getting the surrounding
    # trees for the larger trees --> adding an if/else statement and for the trees larger than 15cm DBH, going to divide diameter by 4 instead of 2 (tried 4 for plots 
    # and that seemed to work well for large trees; too small for small trees)
    
    if (trees$DBH[tree] <= 0.15) {
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 2 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 2
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 2
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 2
      
    } else {
      
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 4 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 4
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 4
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 4
    }
    
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to rough bounding box..."))
    tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
    toc()
    
    
    # set the classification value to 1 for points within the cone and 0 for points outside
    # downside to this si that we lose the original classification code
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to tree cone..."))
    if (trees$Species[tree] == "TSHE") {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-45/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      } else {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-60/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      }
    } else {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-30/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          }else {
            tpts@data$Classification[i] <- 2 }
        }
      } else {
        if (trees$DBH[tree] <= 0.60) {
          for (i in 1:tpts@header$`Number of point records`) {
            
            # compute horizontal and vertical distance from apex XY to each point
            hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
            vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
            
            # precompute some things
            tanAngle <- tan(-45/2 * pi/180)
            
            if (hdist <= (tanAngle * vdist)) {
              tpts@data$Classification[i] <- 1
            }else {
              tpts@data$Classification[i] <- 2 }
          }
        } else {
          for (i in 1:tpts@header$`Number of point records`) {
            
            # compute horizontal and vertical distance from apex XY to each point
            hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
            vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
            
            # precompute some things
            tanAngle <- tan(-60/2 * pi/180)
            
            if (hdist <= (tanAngle * vdist)) {
              tpts@data$Classification[i] <- 1
            } else {
              tpts@data$Classification[i] <- 2}
          }
        }
      }
    }
    
    
    
    toc()
    
    # keep only class 1 points
    tpts <- filter_poi(tpts, Classification == 1)
    
    # reset the point classification to an integer type, not sure why it was changed
    tpts@data$Classification <- as.integer(tpts@data$Classification)
    
    #plot(tpts)
    
    # write tree points
    writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/", "Plot_", trees$Plot_Number[tree], "_TreeCone_", trees$Tag_Num[tree], ".las"))
  }
  toc()
}


## further clipping to small cylinder and top 3m --> also cloud metric computing
# reset options
resetGlobalCommandOptions()

# set default behavior for commands
setGlobalCommandOptions(runCmd = TRUE, saveCmd = FALSE, echoCmd = FALSE)

topDepth <- 3

#for (plot in 1:2) {
for (plot in 1:nrow(folders)) {
  tic(paste0("Computing metrics for plot ", folders$Plot_Number[plot]))
  pointFolder <- folders$Folder[plot]
  
  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/metrics.csv")
  
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )
  
  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)
  
  
  # this is the fix for 10/25/2023 problem...keeping code above to get FileTitle
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/", m$FileTitle[i], ".las")
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }
  
  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/SmallCylindermetrics.csv")
  
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )
  
  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)
  
  # ERROR: 10/25/2023: original code used the leaning tree clip with an estimated crow width
  # to get the max elevation. If there are branches from an adjacent tree above the target
  # tree, there may be no points in the small cylinder clip so we loose the tree.
  # Logic should be using the max elevation in the 1m cylinder, not the entire tree clip.
  #
  # easy fix is to do the small cylinder clips and keep all points, run cloudmetrics to get
  # the max elevation, then use this to clip the top
  
  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth
  
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder/", m$FileTitle[i], ".las")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }
  
  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized/", m$FileTitle[i], ".las")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }
  
  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
               , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
  m$Plot_Number <- folders$Plot_Number[plot]
  
  # merge metrics and field data
  merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))
  
  write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/TreeTops_SmallCylinder_normalized",
                           "/Leaning_TreeTops_SmallCylinder_normalized_metrics_Plot",
                           sprintf("%s02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)
  
  if (plot == 1) {
    allMerged <- merged
  }
  else {
    allMerged <- rbind(allMerged, merged)
  }
}

# write off combined file
write.csv(allMerged, file = "Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics_plots_11_12_13.csv", row.names = FALSE)


## getting the cylinder data from those plots (instead of just top 3m)
sampleRadius <- 1
topDepth <- 3

#for (plot in 1:2) {
for (plot in 1:nrow(folders)) {
  pointFolder <- folders$Folder[plot]
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/SmallCylindermetrics.csv")
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )
  m <- read.csv(outFile, stringsAsFactors = FALSE)
  
  for (i in 1:nrow(m)) {
    # Ally comment: setting sample radius based on DBH
    if (allTrees$DBH[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] <= 0.15) {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 3 
    } else {
      sampleRadius <- allTrees$CrownDia[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] / 6 
    }
    
    
    
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized/", m$FileTitle[i], ".las")
             , zmin = 0
             , zmax = m$Elev.maximum[i] - m$Elev.minimum[i]
             , biaselev = -m$Elev.minimum[i]
             , minx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$TopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$TopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }
  
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
               , paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized", "/Trees_SmallCylinder_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )
  
  m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized", "/Trees_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
  m$Plot_Number <- folders$Plot_Number[plot]
  
  # merge metrics and field data
  merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))
  
  write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip_final/Trees_SmallCylinder_normalized",
                           "/Leaning_Trees_SmallCylinder_normalized_metrics_Plot",
                           sprintf("%s02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)
  
  if (plot == 1) {
    allMerged <- merged
  }
  else {
    allMerged <- rbind(allMerged, merged)
  }
}
# write off combined file
write.csv(allMerged, file = "Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics_plots11_12_13.csv", row.names = FALSE)






#### Below is code that can be ignored -- was experiencing with different settings for the cone clipping ####
## Ally comment: below is idea to clip the trees in an upsidedown cone instead of a cylinder due to the issues surrounding nearby branches
  # from other trees coming into the cylinder clippings

# borrowing and editing some of Bob's code on his Github labeled "StemDetection.R". Going to combine that code with some of the logic from code above.

# Bob's coneClip function
coneClip <- function(
    pts,
    coneX,
    coneY,
    coneZ,
    angle = -30, # in Bob original code, this is positive --> changed to negative to get cone upside-down
    inside = TRUE,
    xLabel = "x",
    yLabel = "y",
    zLabel = "z"
) {
  ipts <- pts[, c(xlabel, ylabel, zlabel)]
  colnames(ipts) <- c("x", "y", "z")
  
  # compute horizontal and vertical distance from apex XY to each point
  ipts$hdist <- sqrt((ipts$x - coneX)^2 + (ipts$y - coneY)^2)
  ipts$vdist <- ipts$z - coneZ
  
  # precompute some things
  tanAngle <- tan(angle/2 * pi/180)
  
  # do the clip
  if(inside) {
    cpts <- ipts[which(ipts$hdist <= (tanAngle * ipts$vdist)), ] 
  } else {
    cpts <- ipts[which(ipts$hdist > (tanAngle * ipts$vdist)), ]
  }
  
  return(cpts)
  
}


# Ally comment: going to modify the following code that is the first for loop of this R scrip to clip to a cone instead of a bounding box
for (plot in 1:1) {
  tic(paste0("Clipping trees for plot; cone clip ", folders$Plot_Number[plot]))
  trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[plot],]
  
  pointFolder <- folders$Folder[plot]
  
  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip"))
  
  #lazFiles <- list.files(pointFolder, pattern = ".laz")
  
  tic(paste0("Plot ", folders$Plot_Number[plot], ":Reading LAS catalog..."))
  pts <- readLAScatalog(pointFolder, select = "*.laz")
  toc()
  
    # for (tree in 1:1) {
  for (tree in 1:nrow(trees)) {
    # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
    b <- c(trees$BaseX[tree], trees$BaseY[tree], trees$BaseElevation[tree]) # base
    c <- c(trees$TopX[tree], trees$TopY[tree], trees$topElevation[tree]) # top
    
    # build bounding box for tree
    # Ally comment: was originally divide the crown diameter by 4 --> that was taking too long to process (i.e. overnight for a single tree) and was getting the surrounding
    # trees for the larger trees --> adding an if/else statement and for the trees larger than 15cm DBH, going to divide diameter by 4 instead of 2 (tried 4 for plots 
    # and that seemed to work well for large trees; too small for small trees)
    
    if (trees$DBH[tree] <= 0.15) {
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 2 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 2
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 2
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 2
      
    } else {
      
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 4 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 4
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 4
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 4
    }
    
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to rough bounding box..."))
    tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
    toc()

    
  # set the classification value to 1 for points within the cone and 0 for points outside
  # downside to this si that we lose the original classification code
  tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to tree cone..."))
  for (i in 1:tpts@header$`Number of point records`) {

    # compute horizontal and vertical distance from apex XY to each point
    hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
    vdist <- tpts@data$Z[i] - trees$topElevation[tree]

    # precompute some things
    tanAngle <- tan(-30/2 * pi/180)

    if (hdist <= (tanAngle * vdist)) {
      tpts@data$Classification[i] <- 1
    }
    else {
      tpts@data$Classification[i] <- 2
    }
  }
  toc()

  # keep only class 1 points
  tpts <- filter_poi(tpts, Classification == 1)

  # reset the point classification to an integer type, not sure why it was changed
  tpts@data$Classification <- as.integer(tpts@data$Classification)

  #plot(tpts)

  # write tree points
  writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip", "Plot_", trees$Plot_Number[tree], "_TreeCone_", trees$Tag_Num[tree], ".las"))
   }
 toc()
}



# Ally comment: going to try same thing as above, but making the angle bigger for larger trees (so don't cut off tree points) and making angle bigger for TSHE to not
  # cut off droopy leader
for (plot in 2:2) {
  tic(paste0("Clipping trees for plot; cone clip ", folders$Plot_Number[plot]))
  trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[plot],]
  
  pointFolder <- folders$Folder[plot]
  
  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees/ConeClip2"))
  
  #lazFiles <- list.files(pointFolder, pattern = ".laz")
  
  tic(paste0("Plot ", folders$Plot_Number[plot], ":Reading LAS catalog..."))
  pts <- readLAScatalog(pointFolder, select = "*.laz")
  toc()
  
  # for (tree in 1:1) {
  for (tree in 1:nrow(trees)) {
    # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
    b <- c(trees$BaseX[tree], trees$BaseY[tree], trees$BaseElevation[tree]) # base
    c <- c(trees$TopX[tree], trees$TopY[tree], trees$topElevation[tree]) # top
    
    # build bounding box for tree
    # Ally comment: was originally divide the crown diameter by 4 --> that was taking too long to process (i.e. overnight for a single tree) and was getting the surrounding
    # trees for the larger trees --> adding an if/else statement and for the trees larger than 15cm DBH, going to divide diameter by 4 instead of 2 (tried 4 for plots 
    # and that seemed to work well for large trees; too small for small trees)
    
    if (trees$DBH[tree] <= 0.15) {
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 2 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 2
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 2
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 2
      
    } else {
      
      xLeft <- min(trees$BaseX[tree], trees$TopX[tree]) - trees$CrownDia[tree] / 4 
      yBottom <- min(trees$BaseY[tree], trees$TopY[tree]) - trees$CrownDia[tree] / 4
      xRight <- max(trees$BaseX[tree], trees$TopX[tree]) + trees$CrownDia[tree] / 4
      yTop <- max(trees$BaseY[tree], trees$TopY[tree]) + trees$CrownDia[tree] / 4
    }
    
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to rough bounding box..."))
    tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
    toc()
    
    
    # set the classification value to 1 for points within the cone and 0 for points outside
    # downside to this si that we lose the original classification code
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to tree cone..."))
    if (trees$Species[tree] == "TSHE") {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - trees$topElevation[tree]
          
          # precompute some things
          tanAngle <- tan(-45/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      } else {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - trees$topElevation[tree]
          
          # precompute some things
          tanAngle <- tan(-60/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      }
      } else {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - trees$topElevation[tree]
          
          # precompute some things
          tanAngle <- tan(-30/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          }else {
            tpts@data$Classification[i] <- 2 }
        }
      } else {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$TopX[tree])^2 + (tpts@data$Y[i] - trees$TopY[tree])^2)
          vdist <- tpts@data$Z[i] - trees$topElevation[tree]
          
          # precompute some things
          tanAngle <- tan(-45/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      }
      }
    
    
    
    toc()
    
    # keep only class 1 points
    tpts <- filter_poi(tpts, Classification == 1)
    
    # reset the point classification to an integer type, not sure why it was changed
    tpts@data$Classification <- as.integer(tpts@data$Classification)
    
    #plot(tpts)
    
    # write tree points
    writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/ConeClip2/", "Plot_", trees$Plot_Number[tree], "_TreeCone_", trees$Tag_Num[tree], ".las"))
  }
  toc()
}



  
#### Below is code used to process past DF/WH data that Bob used in the paper,  cone clipped to use with the cedar data ####
# data can be found in the "TreeClips" folder of the UAS_Lidar_Species_classification repository, link here: https://github.com/bmcgaughey1/UAS_Lidar_Species_Classification/tree/main/TreeClips
# data is already clipped for each tree; just clipped as cylinder
# data is not normalized

# just doing the cone clipping for now, not actual extraction of metrics

  
library(lidR)
library(tictoc)
library(fusionwrapr)
library(naturalsort)
setwd("F:/workspace/OESF/AllyCedarFUSIONWork/")
setFUSIONpath("F:/workspace/OESF/AllyCedarFUSIONWork/FUSION")

dist3d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  v3 <- cross3d_prod(v1,v2)
  area <- sqrt(sum(v3*v3))/2
  d <- 2*area/sqrt(sum(v1*v1))
}

cross3d_prod <- function(v1,v2){
  v3 <- vector()
  v3[1] <- v1[2]*v2[3]-v1[3]*v2[2]
  v3[2] <- v1[3]*v2[1]-v1[1]*v2[3]
  v3[3] <- v1[1]*v2[2]-v1[2]*v2[1]
  return(v3)
}



# Ally comment: Below is code for clipping trees to a cone instead of a bounding box. Was a little too harsh at the top of trees --> added 0.5 to the top of trees so cone is 
# starting slightly more above trees; also was clipping TSHE too much  do to droopy top --> added if/else statement with bigger angles for TSHE; also added if/else
# statements for bigger trees (2 levels for TSHE, 3 for THPL because gets so big at ONF)

# loading in the tree data (from Bob)
trees <- read.csv("F:/workspace/OESF/AllyCedarFUSIONWork/PSMEandTSHEfromBobData/TreeData.csv", header = TRUE)
# DBH is in cm --> need to make in m 
trees$DBH_cm <- trees$DBH_cm / 100
names(trees)[names(trees) == "DBH_cm"] <- "DBH"

# folder with LAZ files in it (one for each individual tree)
pointFolder <- "F:/workspace/OESF/AllyCedarFUSIONWork/PSMEandTSHEfromBobData/TreeClips/"

lazFiles <- list.files(pointFolder, pattern = ".laz") 
lazFiles <- naturalsort(lazFiles) # IMPORTANT THAT THE ORDER OF THESE FILES MATCHES THE ORDER OF TREES IN THE 'TREES' FILE

  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "ConeClip_final"))

    # for (tree in 1:1) {
  for (tree in 1:nrow(trees)) {
    
    pts <- readLAS(paste0(pointFolder, lazFiles[tree]))
    # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
    b <- c(trees$aveBaseX[tree], trees$aveBaseY[tree], trees$aveBaseElevation[tree]) # base
    c <- c(trees$aveTopX[tree], trees$aveTopY[tree], trees$topElevation[tree]) # top
    
    # build bounding box for tree
    # Ally comment: was originally divide the crown diameter by 4 --> that was taking too long to process (i.e. overnight for a single tree) and was getting the surrounding
    # trees for the larger trees --> adding an if/else statement and for the trees larger than 15cm DBH, going to divide diameter by 4 instead of 2 (tried 4 for plots 
    # and that seemed to work well for large trees; too small for small trees)
    
    tic("clipping to bounding box")
    
    if (trees$DBH[tree] <= 0.15) {
      xLeft <- min(trees$aveBaseX[tree], trees$aveTopX[tree]) - trees$aveCrownDia[tree] / 2 
      yBottom <- min(trees$aveBaseY[tree], trees$aveTopY[tree]) - trees$aveCrownDia[tree] / 2
      xRight <- max(trees$aveBaseX[tree], trees$aveTopX[tree]) + trees$aveCrownDia[tree] / 2
      yTop <- max(trees$aveBaseY[tree], trees$aveTopY[tree]) + trees$aveCrownDia[tree] / 2
      
    } else {
      
      xLeft <- min(trees$aveBaseX[tree], trees$aveTopX[tree]) - trees$aveCrownDia[tree] / 4 
      yBottom <- min(trees$aveBaseY[tree], trees$aveTopY[tree]) - trees$aveCrownDia[tree] / 4
      xRight <- max(trees$aveBaseX[tree], trees$aveTopX[tree]) + trees$aveCrownDia[tree] / 4
      yTop <- max(trees$aveBaseY[tree], trees$aveTopY[tree]) + trees$aveCrownDia[tree] / 4
    }
    
    tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
toc()
    
    # set the classification value to 1 for points within the cone and 0 for points outside
    # downside to this si that we lose the original classification code
tic("clipping to cone")

    if (trees$Species[tree] == "TSHE") {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$aveTopX[tree])^2 + (tpts@data$Y[i] - trees$aveTopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-45/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      } else {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$aveTopX[tree])^2 + (tpts@data$Y[i] - trees$aveTopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-60/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          } else {
            tpts@data$Classification[i] <- 2}
        }
      }
    } else {
      if (trees$DBH[tree] <= 0.15) {
        for (i in 1:tpts@header$`Number of point records`) {
          
          # compute horizontal and vertical distance from apex XY to each point
          hdist <- sqrt((tpts@data$X[i]- trees$aveTopX[tree])^2 + (tpts@data$Y[i] - trees$aveTopY[tree])^2)
          vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
          
          # precompute some things
          tanAngle <- tan(-30/2 * pi/180)
          
          if (hdist <= (tanAngle * vdist)) {
            tpts@data$Classification[i] <- 1
          }else {
            tpts@data$Classification[i] <- 2 }
        }
      } else {
        if (trees$DBH[tree] <= 0.60) {
          for (i in 1:tpts@header$`Number of point records`) {
            
            # compute horizontal and vertical distance from apex XY to each point
            hdist <- sqrt((tpts@data$X[i]- trees$aveTopX[tree])^2 + (tpts@data$Y[i] - trees$aveTopY[tree])^2)
            vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
            
            # precompute some things
            tanAngle <- tan(-45/2 * pi/180)
            
            if (hdist <= (tanAngle * vdist)) {
              tpts@data$Classification[i] <- 1
            }else {
              tpts@data$Classification[i] <- 2 }
          }
        } else {
          for (i in 1:tpts@header$`Number of point records`) {
            
            # compute horizontal and vertical distance from apex XY to each point
            hdist <- sqrt((tpts@data$X[i]- trees$aveTopX[tree])^2 + (tpts@data$Y[i] - trees$aveTopY[tree])^2)
            vdist <- tpts@data$Z[i] - (trees$topElevation[tree] + 0.5)
            
            # precompute some things
            tanAngle <- tan(-60/2 * pi/180)
            
            if (hdist <= (tanAngle * vdist)) {
              tpts@data$Classification[i] <- 1
            } else {
              tpts@data$Classification[i] <- 2}
          }
        }
      }
    }
    
    
  

    # keep only class 1 points
    tpts <- filter_poi(tpts, Classification == 1)
    
    # reset the point classification to an integer type, not sure why it was changed
    tpts@data$Classification <- as.integer(tpts@data$Classification)
    
    #plot(tpts)
  toc()  
    # write tree points
    writeLAS(tpts, paste0(pointFolder, "ConeClip_final/", "Plot_", trees$Plot_Number[tree], "_TreeCone_", trees$Tag_Num[tree], ".las"))
  }

  
  
  
#### Getting cylinder (variable radius) and top 3m info from the DF/WH data ####
  library(naturalsort)
  # reset options
  resetGlobalCommandOptions()
  
  # set default behavior for commands
  setGlobalCommandOptions(runCmd = TRUE, saveCmd = FALSE, echoCmd = FALSE)
  
  
  sampleRadius <- 1
  topDepth <- 3
  

  # loading in the tree data (from Bob)
  allTrees <- read.csv("F:/workspace/OESF/AllyCedarFUSIONWork/PSMEandTSHEfromBobData/TreeData.csv", header = TRUE)
  # DBH is in cm --> need to make in m 
  allTrees$DBH_cm <- allTrees$DBH_cm / 100
  names(allTrees)[names(allTrees) == "DBH_cm"] <- "DBH"
  
  # folder with LAZ files in it (one for each individual tree)
  pointFolder <- "F:/workspace/OESF/AllyCedarFUSIONWork/PSMEandTSHEfromBobData/TreeClips/ConeClip_final"
  
  
  lasFiles <- list.files(pointFolder, pattern = ".las") 
  lasFiles <- naturalsort(lasFiles) # IMPORTANT THAT THE ORDER OF THESE FILES MATCHES THE ORDER OF TREES IN THE 'ALLTREES' FILE
  
  # going to add FileTile column to all trees list --> will need later
  FileTile <- paste0("Plot_", allTrees$Plot_Number, "_TreeCone_", allTrees$Tag_Num)
  allTrees <- cbind(allTrees, FileTile)
    # create output folder for new tree clips
    outFile <- paste0(pointFolder, "/metrics.csv")
    
    # making list of las files --> ended up not using (couldn't get it to work --> went another direction)
   # lasFilesList <- paste0(pointFolder, "/", lasFiles)
    #write.table(lasFilesList, file = "lasFilesList.txt", sep = ",", row.names = FALSE, quote = FALSE)

        CloudMetrics(paste0(pointFolder, "/", "*.las")
                 , outFile
                 , new = TRUE
                 , rid = TRUE)
    
    # read the metrics to get the high point
    # read the metrics for the non-normalized points and get the highest elevation
    m <- read.csv(outFile, stringsAsFactors = FALSE)

    
    # this is the fix for 10/25/2023 problem...keeping code above to get FileTitle
    # build commands to clip to upper portion of each TAO
    for (i in 1:nrow(m)) {
      # Ally comment: setting sample radius based on DBH
      if (allTrees$DBH[allTrees$FileTile == m$FileTitle[i]] <= 0.15) {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 3 
      } else {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 6 
      }
      
      ClipData(m$DataFile[i]
               , paste0(pointFolder, "/Trees_SmallCylinder/", m$FileTitle[i], ".las")
               , minx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , miny = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , maxx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , maxy = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , shape = 1
      )
    }
    
    # create output folder for new tree clips
    outFile <- paste0(pointFolder, "/Trees_SmallCylinder/SmallCylindermetrics.csv")
    
    CloudMetrics(paste0(pointFolder, "/Trees_SmallCylinder/", "*.las")
                 , outFile
                 , new = TRUE
                 , rid = TRUE
    )
    
    m <- read.csv(outFile, stringsAsFactors = FALSE)
    
    
    # Ally comment: normalizing small cylinder trees
    for (i in 1:nrow(m)) {
      # Ally comment: setting sample radius based on DBH
      if (allTrees$DBH[allTrees$FileTile == m$FileTitle[i]] <= 0.15) {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 3 
      } else {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 6 
      }
      
      ClipData(m$DataFile[i]
               , paste0(pointFolder, "/Trees_SmallCylinder_normalized/", m$FileTitle[i], ".las")
               , zmin = 0
               , zmax = m$Elev.maximum[i] - m$Elev.minimum[i]
               , biaselev = -m$Elev.minimum[i]
               , minx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , miny = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , maxx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , maxy = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , shape = 1
      )
    }
    
    # compute metrics for trees...use rid=TRUE to parse tree number from end of point file name
    CloudMetrics(paste0(pointFolder, "/Trees_SmallCylinder_normalized/", "*.las")
                 , paste0(pointFolder, "/Trees_SmallCylinder_normalized", "/Trees_SmallCylinder_normalized_metrics.csv")
                 , new = TRUE
                 , rid = TRUE
    )
    
    m <- read.csv(paste0(pointFolder, "/Trees_SmallCylinder_normalized", "/Trees_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
   
    # merge metrics and field data
    merged <- merge(allTrees, m, by.x = "FileTile", by.y = "FileTitle")
    
    write.csv(merged, paste0(pointFolder, "/Trees_SmallCylinder_normalized",
                             "/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv"), row.names = FALSE)
    
    
    
    # now, doing the top 3m
    
    # read the metrics to get the high point
    # read the metrics for the non-normalized points and get the highest elevation
    outFile <- paste0(pointFolder, "/Trees_SmallCylinder/SmallCylindermetrics.csv")
    m <- read.csv(outFile, stringsAsFactors = FALSE)
    
    # ERROR: 10/25/2023: original code used the leaning tree clip with an estimated crow width
    # to get the max elevation. If there are branches from an adjacent tree above the target
    # tree, there may be no points in the small cylinder clip so we loose the tree.
    # Logic should be using the max elevation in the 1m cylinder, not the entire tree clip.
    #
    # easy fix is to do the small cylinder clips and keep all points, run cloudmetrics to get
    # the max elevation, then use this to clip the top
    
    # compute the elevation for the base of the upper portion
    m$SampleBaseElev <- m$Elev.maximum - topDepth
    
    # build commands to clip to upper portion of each TAO
    for (i in 1:nrow(m)) {
      # Ally comment: setting sample radius based on DBH
      if (allTrees$DBH[allTrees$FileTile == m$FileTitle[i]] <= 0.15) {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 3 
      } else {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 6 
      }
      
      ClipData(m$DataFile[i]
               , paste0(pointFolder, "/TreeTops_SmallCylinder/", m$FileTitle[i], ".las")
               , zmin = m$SampleBaseElev[i]
               , zmax = m$Elev.maximum[i]
               , minx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , miny = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , maxx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , maxy = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , shape = 1
      )
    }
    
    # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
    # bias is added to point height so it needs to be negative
    # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
    for (i in 1:nrow(m)) {
      # Ally comment: setting sample radius based on DBH
      if (allTrees$DBH[allTrees$FileTile == m$FileTitle[i]] <= 0.15) {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 3 
      } else {
        sampleRadius <- allTrees$aveCrownDia[allTrees$FileTile == m$FileTitle[i]] / 6 
      }
      
      ClipData(m$DataFile[i]
               , paste0(pointFolder, "/TreeTops_SmallCylinder_normalized/", m$FileTitle[i], ".las")
               , zmin = 0
               , zmax = topDepth
               , biaselev = -m$SampleBaseElev[i]
               , minx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , miny = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] - sampleRadius
               , maxx = allTrees$aveTopX[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , maxy = allTrees$aveTopY[allTrees$FileTile == m$FileTitle[i]] + sampleRadius
               , shape = 1
      )
    }
    
    # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
    CloudMetrics(paste0(pointFolder, "/TreeTops_SmallCylinder_normalized/", "*.las")
                 , paste0(pointFolder, "/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv")
                 , new = TRUE
                 , rid = TRUE
    )
    
    m <- read.csv(paste0(pointFolder, "/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
    
    # merge metrics and field data
    merged <- merge(allTrees, m, by.x = "FileTile", by.y = "FileTitle")
    
    write.csv(merged, paste0(pointFolder, "/TreeTops_SmallCylinder_normalized",
                             "/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv"), row.names = FALSE)
    


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

 
