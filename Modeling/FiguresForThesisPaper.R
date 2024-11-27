# CODE 9
# Using the Random Forest models to predict cedar locations outside of field plot boundaries to the extent of the lidar and/or MS data,
# and create figures for paper.

# Ally Kruper
# Created 5/8/24


#### Comparison of species and DBHs accross the years 2021 and 2023 ####
# this code is based off of code from Bob paper for ease: https://github.com/bmcgaughey1/DroneLidarCode/blob/main/Rcode/FieldDataSummary.R

library(ggplot2)
library(RColorBrewer)
library(egg)
library(readxl)

allTrees <- read.csv("data/Entered_Cedar_T3_2023_TreeDataSheets_CompiledTrees_Cleaned.csv", header = TRUE)
BobTrees <- read_excel("2021_T3_Upland_Trees.xlsx", sheet = "Sheet1", col_names = TRUE)

# dropping UNKN and labeling dead (anomaly 1) as 'Snag
# also remove others due to small number of trees: MAFU (3), POTR (2), PREM (1), SASC (1)

allTrees <- subset(allTrees, Species != "POTR")
allTrees <- subset(allTrees, Species != "PREM")
allTrees <- subset(allTrees, Species != "SASC")
allTrees <- subset(allTrees, Species != "MAFU")
allTrees <- subset(allTrees, Species != "UNKN")

# labeling dead trees as "snag"
DeadTrees <- which(allTrees$X1 == 1)
allTrees$Species[DeadTrees] <- "Snag"

# doing same thing as above for the 2021 data
unique(BobTrees$Species)
BobTrees <- subset(BobTrees, Species != "ACMA")
BobTrees <- subset(BobTrees, Species != "UNKN")
BobTrees <- subset(BobTrees, Species != "UKWN")


BobDeadTrees <- which(BobTrees$X1 == 1)
BobTrees$Species[BobDeadTrees] <- "Snag"

# also remove trees without DBH
allTrees <- allTrees[allTrees$X != 795, ]
allTrees <- allTrees[allTrees$X != 796, ]
allTrees <- allTrees[allTrees$X != 881, ]

# all BobTrees appear to have DBHs
# need to make DBH_cm for BobTrees into numeric
BobTrees$DBH_cm <- as.numeric(BobTrees$DBH_cm)
unique(BobTrees$Species)

# for the OESF site
OESF <- ggplot(data = allTrees[allTrees$Site == "T3",], aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'black', binwidth = 2, show.legend = FALSE) +
  xlab("Diameter at breast height (cm)") +
  ylab("Frequency") +
  xlim(0, 85) +
  ylim(0, 150) +
  scale_fill_manual(values = c("#B3DE69", "#FDB462", "#FB8072", "#80B1D3", "#FCCDE5", "#FFFFB3", "#8DD3C7",
                               "#BEBADA", "#D9D9D9"),
                    labels = c("Silver fir", "Vine maple", "Red alder", "Sitka spruce", "Douglas-fir", "Cascara buckthorn", "Snag", "Western redcedar", "Western hemlock")) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text=element_text(size=12)) 
# + theme(legend.position = c(0.85, 0.7),
#       text = element_text(size=18))

# for the ONF site

ONF <- ggplot(data = allTrees[allTrees$Site == "ONF",], aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'black', binwidth = 10, show.legend = FALSE) +
  xlab("Diameter at breast height (cm)") +
  ylab("Frequency") +
  ylim(0, 150) +
  scale_fill_manual(values = c("#B3DE69", "#FDB462", "#FB8072", "#FCCDE5", "#FFFFB3", "#8DD3C7",
                               "#BEBADA", "#D9D9D9"),
                    labels = c("Silver fir", "Vine maple", "Red alder", "Douglas-fir", "Cascara buckthorn", "Snag", "Western redcedar", "Western hemlock")) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text=element_text(size=12)) 

#  theme(legend.position = c(0.85, 0.7),
#       text = element_text(size=18))

BobData <- ggplot(data = BobTrees, aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'black', binwidth = 2, show.legend = TRUE) +
  xlab("Diameter at breast height (cm)") +
  ylab("Frequency") +  
  xlim(0, 85) +
  ylim(0, 150) +
  scale_fill_manual(values = c("#B3DE69", "#FDB462", "#FB8072", "#80B1D3", "#FCCDE5", "#FFFFB3", "#8DD3C7",
                               "#BEBADA", "#D9D9D9")
                    , labels = c("Silver fir", "Vine maple", "Red alder", "Sitka spruce", "Douglas-fir", "Cascara buckthorn", "Snag", "Western redcedar", "Western hemlock")
                   ) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text=element_text(size=12)) 

#### Map of predicted cedar locations; OESF site, 2021 and 2023 data, LiDAR only, top 3m ####

# Using the OESF only, lidar only, top 3m model (highest accuracy) model
final_tree_top3_OESF <- final_wf %>%
  fit(ModelData)

# bringing in the rest of the data to predict on
# is in many different files from the lidar processing --> need to bring them in and merge them
# need to bring them in based on year --> normalize intensity --> then can combine into one dataset
FileList_2021 <- list.files("data/LidarMetrics/LidarTrees/TreeTops/2021")

# starting with 2021 data
# loading in the first file to kick things off
LidarTrees_2021 <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2021/", FileList_2021[1]), header = TRUE)


for (i in 2:5) { # keeps failing at 6 (Plots_16_31) for some reason --> going to split up the data import
  file <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2021/", FileList_2021[i]), header = TRUE, sep = ",")
  
  LidarTrees_2021 <- rbind(LidarTrees_2021, file)
} 

for (i in 7:12) { 
  file <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2021/", FileList_2021[i]), header = TRUE, sep = ",")
  
  LidarTrees_2021 <- rbind(LidarTrees_2021, file)
} 
# now doing the one missed (Plots16_31)
file <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2021/", "Plots16_31_TreeTops_normalized_metrics_merged.csv"), header = TRUE, sep = ",")
LidarTrees_2021 <- rbind(LidarTrees_2021, file)

# normalizing the 2021 mean intensity data
LidarTrees_2021[, "Int.mean"] <- (LidarTrees_2021[, "Int.mean"] - 192.6818) / 17.02009777


# now the 2023 data
FileList_2023 <- list.files("data/LidarMetrics/LidarTrees/TreeTops/2023")
LidarTrees_2023 <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2023/", FileList_2023[1]), header = TRUE)

for (i in 2:length(FileList_2023)) { 
  file <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2023/", FileList_2023[i]), header = TRUE, sep = ",")
  
  LidarTrees_2023 <- rbind(LidarTrees_2023, file)
} 
# normalizing the 2023 mean intensity data
LidarTrees_2023[, "Int.mean"] <- (LidarTrees_2023[, "Int.mean"] - 119.895) / 57.01547338

# now combing the two years
LidarTrees <- rbind(LidarTrees_2021, LidarTrees_2023)

#double checking no oddities in data
summary(LidarTrees)

# some lidar trees seem to have failed (likely not a tree where tree was identified) --> removing trees with 0s as values
LidarTrees <- LidarTrees[-which(LidarTrees$Elev.CV == 0),]

# some trees have an extremely small number of returns --> likely not actually a tree --> removing those
LidarTrees <- LidarTrees[which(LidarTrees$Total.return.count > 1500),]

# now double checking the data again
summary(LidarTrees)

# need to make column names to match that of the train/test data 
names(LidarTrees)[names(LidarTrees) == "GridHighX"] <- "X"
names(LidarTrees)[names(LidarTrees) == "GridHighY"] <- "Y"
names(LidarTrees)[names(LidarTrees) == "GroundElev"] <- "BaseElevation"
names(LidarTrees)[names(LidarTrees) == "GridMaxHt"] <- "Height"
names(LidarTrees)[names(LidarTrees) == "DataFile.x"] <- "Tree.ID"
LidarTrees[ , 'BaseX'] = NA
LidarTrees[ , 'BaseY'] = NA
LidarTrees[ , 'CrownDia'] = NA
LidarTrees[ , 'TopX'] = NA
LidarTrees[ , 'TopY'] = NA
LidarTrees[ , 'Year'] = NA
LidarTrees[ , 'Plot_Number'] = NA
LidarTrees[ , 'Tag_Num'] = NA
LidarTrees[ , 'DataFile'] = NA
LidarTrees[ , 'DBH'] = NA
LidarTrees[ , 'topElevation'] = NA
LidarTrees$Elev.maximum <- as.integer(LidarTrees$Elev.maximum) # model keeps getting mad for that Elev.max is an integer
# in the original ModelData and not in this data (gets dropped later anyway)
LidarTrees$Int.skewness <- as.numeric(LidarTrees$Int.skewness) # similar to above
LidarTrees$Int.kurtosis <- as.numeric(LidarTrees$Int.kurtosis)  # similar to above
LidarTrees$Int.L.skewness <- as.numeric(LidarTrees$Int.L.skewness)  # similar to above
LidarTrees$Int.L.kurtosis <- as.numeric(LidarTrees$Int.L.kurtosis)  # similar to above


LidarTrees <- LidarTrees[ , colnames(LidarTrees) %in% colnames(ModelData)]
pred <- predict(final_tree_top3_OESF, LidarTrees) # does prediction

augment(final_tree_top3_OESF, LidarTrees) # pulls what the trees predicted either way

pred %>%
  bind_cols(LidarTrees) %>%
  ggplot(aes(X, Y, color = .pred_class))  +
  geom_point(size = 0.5, alpha = 0.5) # graphing prediction

final_pred <- augment(final_tree_top3_OESF, LidarTrees)

# exporting final prediction
# write.csv(final_pred, file = "Expanding the Model Work/Data/FieldTreesfromPrediction.csv")

# graph, colored based on THPL prediction level
colnames(final_pred)[1] <- "Predicted Species"

library(ggspatial)

final_pred %>%
  ggplot(aes(X, Y, color = `Predicted Species`))  +
  geom_point(size = 1) +
  scale_color_manual(values = c("black", "#de2d26")) +
  coord_cartesian( xlim = c(412900, 413200), ylim = c(5280750, 5281100), expand = TRUE, default = FALSE, clip = "on") + # zoomed in 
  guides(color = guide_legend(override.aes = list(size = 5)))  +
  theme_bw(base_size = 14) +
  ggspatial::annotation_scale(
    bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"))









