# Ally Kruper
# Using code obtained by Bob McGaughey, added to / edited by Ally Kruper
# Last updated 1/9/2024
# Cedar plots 2023

# Code to convert stem map data from field to map coordinates

#### Install / Load Packages ####
install.packages("readxl")
library(readxl)

####Functions ####
# function to take azimuth and distance for trees...computes XY location relative to (0,0)
# return is a data frame with X and Y columns
computeLocalTreePositions <- function(
    trees,
    xRef = 0,                  # actual X for turning point 
    yRef = 0,                  # actual Y for turning point
    azLabel = "Azimuth",       # column label in trees for azimuth
    distLabel = "Distance",    # column label in trees for distance
    dbhLabel = "DBH_cm",        # column label for tree DBH
    dbhConversionFactor = 0.01, # conversion factor to convert DBH to same units as distances
    declination = 0,            # declination...not sure but think E declination is positive (Seattle is about 16 degrees)
    adjustForDBH = FALSE,        # flag indicating DBH/2 should be added to distances <-- this should be true for all 2023 plots
    adjustForHuman = FALSE,       # Indicating whether something should be added for person standing in front of tree with Haglof
    HumanAdjustment = 0         # Amount to adjust for person standing holding Haglof in front of tree
) {
  # adjust distance to account for offset to tree center
  dist <- trees[, distLabel]
  if (adjustForDBH) dist <- dist + (trees[, dbhLabel] * dbhConversionFactor / 2.0)
  if (adjustForHuman) dist <- dist + HumanAdjustment

  # compute XY using distance, azimuth...optional adjust for declination and offset to reference location
  X <- cos((270 - (trees[, azLabel] - declination)) * pi / 180.0) * dist + xRef # used to be 450 (from Bob) --> that would be if it was turning point to tree
  Y <- sin((270 - (trees[, azLabel] - declination)) * pi / 180.0) * dist + yRef # used to be 450 (from Bob) --> that would be if it was turning point to tree
  
  t <- cbind(X, Y)
  colnames(t) <- c("X", "Y")
  
  return(t)
}



# function to translate trees to actual plot location...this could be used instead of 
# using xRef and yRef parameters in computeLocalTreePositions()
moveTreesToPlotXY <- function(
    trees,
    xRef = 0,
    yRef = 0,
    xLabel = "X",
    yLabel = "Y"
) {
  # translate tree positions relative to (0,0) to actual plot locations
  trees[, xLabel] <- trees[, xLabel] + xRef
  trees[, yLabel] <- trees[, yLabel] + yRef
  
  return(trees)
}


#### Prepping data ####
# read data from excel sheet
allTrees <- read.csv("data/Entered_Cedar_T3_2023_TreeDataSheets_CompiledTrees_Cleaned.csv", header = TRUE)
allTrees[,-1] # removing first "X" column because I can't figure out why R added that
allTrees <- allTrees[,-1]

# Cleaning up Azimuth column to change "NA" values to 0 (they will still be on stem map, just in wrong location)
allTrees[,"Azimuth"]
allTrees[which(is.na(allTrees$Azimuth)), "Azimuth"] <- 0 


# Cleaning up Distance column to change "NA" values to 0 (they will still be on stem map, 
# just in wrong location, all gathered at (0,0))
allTrees[,"Dist_m"]
allTrees[which(is.na(allTrees$Dist_m)), "Dist_m"] <- 0 

# Because of the DBH adjustment for stem mapping, need to temporarily give NA DBH's a DBH --> going to do 22.02, 
# as that is mean DBH
allTrees[which(is.na(allTrees$DBH_cm)), "DBH_cm"] <- 22.02



#### Making local (no Javad offset yet) stem map, using loop option ####
# Can extract trees from a single plot at a time (see below after this loop, has more detailed instructions), 
# or can just use loop created below

for(i in unique(allTrees[,"Plot_Num"])) {
  trees <- allTrees[allTrees$Plot_Num == i,]
  
  treePos <- computeLocalTreePositions(trees, xRef = 0, yRef = 0, azLabel = "Azimuth", distLabel = "Dist_m", 
            declination = 0, adjustForDBH = TRUE, adjustForHuman = TRUE, HumanAdjustment = 0.2)
  
  trees <- cbind(trees, treePos)
  
  plot(trees$X, trees$Y, asp = 1, pch = 16, col = as.factor(trees$Species), cex = trees$DBH_cm/10)
  points(0, 0, pch = 3, cex = 2)
  legend('topright', legend = levels(as.factor(trees$Species)), col = 1:10, pch = 16)
  title(main = i)
  
  trees[which(trees$DBH_cm == 22.02), "DBH_cm"] <- NA # only needed if changed DBH due to NAs in DBH (changing DBH back to NA)
  
  filename <- paste("data/stem_maps/plot_",i,"_localXY_270.csv", sep = "")
  # write.csv(x = trees, file = filename)
}



#### Making local (no Javad offset yet) plot-by-plot option (more detailed instructions, no loop) ####
# extract trees for a single plot...choices are 8-35, excluding 34
# You could also do locations for all trees in all plots since the distances and azimuths are
# relative to the turning point for each plot but this would "stack" all the trees on (0,0)
trees <- allTrees[allTrees$Plot_Num == "8",]


# compute tree positions...
# If the turning point was not an actual tree (e.g., plot "20"), you will get a point at (0,0).
# Ultimately you would also have the javad data post-processed and available as a table
# or spreadsheet with the Plot_number field and X and Y values. These would be passed using 
# xRef = plotX and yRef = plotY.
treePos <- computeLocalTreePositions(trees, xRef = 0, yRef = 0, azLabel = "Azimuth", distLabel = "Dist_m", declination = 0, adjustForDBH = TRUE, adjustForHuman = TRUE, HumanAdjustment = 0.2)


# add X & Y columns to trees
trees <- cbind(trees, treePos)

# draw a pretty picture...DBH scaling is not to scale
plot(trees$X, trees$Y, asp = 1, pch = 16, col = as.factor(trees$Species), cex = trees$DBH_cm/10)
points(0, 0, pch = 3, cex = 2)
legend('topright', legend = levels(as.factor(trees$Species)), col = 1:10, pch = 16)
title(main = 8)

# Writing file to csv
write.csv(x = trees, file = "data/stem_maps/plot_8_localXY.csv")


#### Moving stem maps to Javad location -- When Javad is at the TP, which did not occur for any 2023 plots####
## Foloowing code is only for plots that have the Javad at the TP!! (see JavadOffset code below for plots where Javad is not at TP tree)
# locading javad data
Javad <- read.csv("T3_2022/javad_data_22.csv")


trees <- moveTreesToPlotXY(trees, Javad[which(Javad$Plot_Number == "53"), 6], Javad[which(Javad$Plot_Number == "53"), 7])
trees
#draw a pretty picture...DBH scaling is not to scale
# plot(trees$X, trees$Y, asp = 1, pch = 16, col = as.factor(trees$Species), cex = trees$DBH_cm/10)
# points(348649.0, 4154873.3, pch = 3, cex = 2)
# legend('topright', legend = levels(as.factor(trees$Species)), col = 1:10, pch = 16)

write.csv(x = trees, file = "T3_2022/Stem Maps/Plot 53.csv")





#### Moving stem maps to Javad location -- When Javad is NOT at the TP, as was the case for all 2023 plots #### 

# Calculating Javad's local location on the XY plot
JavadXY <- read.csv("data/Entered_Cedar_T3_2023_TreeDataSheets_Compiled_spatial_cleaned.csv", header = TRUE, nrows = 27)

JavadXY[which(is.na(JavadXY$Javad_to_Transp_Dist_m)), "Javad_to_Transp_Dist_m"] <- 0  #if NAs in Dist or Azimuth --> 
# change to zero. Will be in wrong place but if kept as NA will be dropped from dataset
JavadXY[which(is.na(JavadXY$Javad_to_Transp_Az)), "Javad_to_Transp_Az"] <- 0   

localJavadPos <- computeLocalTreePositions(JavadXY, xRef = 0, yRef = 0, azLabel = "Javad_to_Transp_Az", distLabel = "Javad_to_Transp_Dist_m", declination = 0)

JavadXY <- cbind(JavadXY, localJavadPos)



# Re-writing stem maps with trees moved based on Javad to TP locations (using ONLY the Javad to TP info -- no other trees)


for(i in unique(allTrees[,"Plot_Num"])) {
  tree_plot <- read.csv(paste("data/stem_maps/plot_",i,"_localXY_270.csv", sep = ""))
  tree_plot[,"X"] <- (tree_plot[,"X"] - JavadXY[JavadXY$Plot_Num == i, "X"]) + JavadXY[JavadXY$Plot_Num == i, "UTMN83_X"]
  tree_plot[,"Y"] <- (tree_plot[,"Y"] - JavadXY[JavadXY$Plot_Num == i, "Y"]) + JavadXY[JavadXY$Plot_Num == i, "UTMN83_Y"]
  
  plot(tree_plot$X, tree_plot$Y, asp = 1, pch = 16, col = as.factor(trees$Species), cex = trees$DBH_cm/10)
  points(0, 0, pch = 3, cex = 2)
  legend('topright', legend = levels(as.factor(trees$Species)), col = 1:10, pch = 16)
  title(main = i)
  
  filename <- paste("data/stem_maps/270_plot_",i,".csv", sep = "")
  write.csv(x = tree_plot, file = filename)
}


#### Specifically for plots 64, 65, 66 (cedars 5-7) that were completed last field year (2022) and had Javad @ plot center ####

# read data from excel sheet
allTrees <- read.csv("data/T3_2022_Tree_DataEntry_Compiled_Data_Only646566.csv", header = TRUE)
JavadXY <- read.csv("data/T3_2022_Spatial_DataEntry_Compiled_Data_Only646566.csv")

# changing the main compute tree positions function back to 450 degrees b/c plots were taken Javad to tree
computeLocalTreePositions <- function(
    trees,
    xRef = 0,                  # actual X for turning point 
    yRef = 0,                  # actual Y for turning point
    azLabel = "Azimuth",       # column label in trees for azimuth
    distLabel = "Distance",    # column label in trees for distance
    dbhLabel = "DBH_cm",        # column label for tree DBH
    dbhConversionFactor = 0.01, # conversion factor to convert DBH to same units as distances
    declination = 0,            # declination...not sure but think E declination is positive (Seattle is about 16 degrees)
    adjustForDBH = FALSE,        # flag indicating DBH/2 should be added to distances <-- this should be true for all 2023 plots
    adjustForHuman = FALSE,       # Indicating whether something should be added for person standing in front of tree with Haglof
    HumanAdjustment = 0         # Amount to adjust for person standing holding Haglof in front of tree
) {
  # adjust distance to account for offset to tree center
  dist <- trees[, distLabel]
  if (adjustForDBH) dist <- dist + (trees[, dbhLabel] * dbhConversionFactor / 2.0)
  if (adjustForHuman) dist <- dist + HumanAdjustment
  
  # compute XY using distance, azimuth...optional adjust for declination and offset to reference location
  X <- cos((450 - (trees[, azLabel] - declination)) * pi / 180.0) * dist + xRef # used to be 450 (from Bob) --> that would be if it was turning point to tree
  Y <- sin((450 - (trees[, azLabel] - declination)) * pi / 180.0) * dist + yRef # used to be 450 (from Bob) --> that would be if it was turning point to tree
  
  t <- cbind(X, Y)
  colnames(t) <- c("X", "Y")
  
  return(t)
}

# doing everything as one big loop 

for(i in unique(allTrees[,"Plot_Num"])) {
  trees <- allTrees[allTrees$Plot_Num == i,]
  
  treePos <- computeLocalTreePositions(trees, xRef = 0, yRef = 0, azLabel = "Azimuth", distLabel = "Distance", 
                                       declination = 0, adjustForDBH = TRUE, adjustForHuman = FALSE, HumanAdjustment = 0.0)
  
  trees <- cbind(trees, treePos)
  
  plot(trees$X, trees$Y, asp = 1, pch = 16, col = as.factor(trees$Species), cex = trees$DBH_cm/10)
  points(0, 0, pch = 3, cex = 2)
  legend('topright', legend = levels(as.factor(trees$Species)), col = 1:10, pch = 16)
  title(main = i)
  
  trees[,"X"] <- trees[,"X"] + JavadXY[JavadXY$Plot_Num == i, "UTMN83_X"]
  trees[,"Y"] <- trees[,"Y"] + JavadXY[JavadXY$Plot_Num == i, "UTMN83_Y"]
  
  plot(trees$X, trees$Y, asp = 1, pch = 16, col = as.factor(trees$Species), cex = trees$DBH_cm/10)
  points(0, 0, pch = 3, cex = 2)
  legend('topright', legend = levels(as.factor(trees$Species)), col = 1:10, pch = 16)
  title(main = i)
  
  filename <- paste("data/stem_maps/plot_", i - 59 ,".csv", sep = "") # switches plot numbers to be based off of cedar plot numbering
  write.csv(x = trees, file = filename)
}
