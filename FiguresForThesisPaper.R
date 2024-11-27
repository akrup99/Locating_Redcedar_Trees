# Ally Kruper
# Created 5/8/24

# The purpose of this R script is a place to put code for making the figures in my thesis paper

#### Comparison of species and DBHs accross the years 2021 and 2023 ####
  # this code is based off of code from Bob paper for ease: https://github.com/bmcgaughey1/DroneLidarCode/blob/main/Rcode/FieldDataSummary.R
  # (going to use his figure from the paper for the 2021 data)

library(ggplot2)
library(RColorBrewer)


allTrees <- read.csv("data/Entered_Cedar_T3_2023_TreeDataSheets_CompiledTrees_Cleaned.csv", header = TRUE)

# droping UNKN and labeling dead (anomaly 1) as 'Snag
# also remove others due to small number of trees: MAFU (3), POTR (2), PREM (1), SASC (1)

allTrees <- subset(allTrees, Species != "POTR")
allTrees <- subset(allTrees, Species != "PREM")
allTrees <- subset(allTrees, Species != "SASC")
allTrees <- subset(allTrees, Species != "MAFU")
allTrees <- subset(allTrees, Species != "UNKN")

# labeling dead trees as "snag"
DeadTrees <- which(allTrees$X1 == 1)
allTrees$Species[DeadTrees] <- "Snag"


# also remove trees without DBH
allTrees <- allTrees[allTrees$X != 795, ]
allTrees <- allTrees[allTrees$X != 796, ]
allTrees <- allTrees[allTrees$X != 881, ]

# for the OESF site
ggplot(data = allTrees[allTrees$Site == "T3",], aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'black', binwidth = 2, show.legend = FALSE) +
  xlab("Diameter at breast height (cm)") +
  ylab("Frequency") +
  scale_fill_manual(values = c("#B3DE69", "#FDB462", "#FB8072", "#80B1D3", "#FCCDE5", "#FFFFB3", "#8DD3C7",
                              "#BEBADA", "#D9D9D9"),
                    labels = c("Silver fir", "Vine maple", "Red alder", "Sitka spruce", "Douglas-fir", "Cascara buckthorn", "Snag", "Western redcedar", "Western hemlock")) +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10), 
        axis.text=element_text(size=10)) 
  # + theme(legend.position = c(0.85, 0.7),
  #       text = element_text(size=18))

# for the ONF site

ggplot(data = allTrees[allTrees$Site == "ONF",], aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'black', binwidth = 7, show.legend = FALSE) +
  xlab("Diameter at breast height (cm)") +
  ylab("Frequency") +
  scale_fill_manual(values = c("#B3DE69", "#FDB462", "#FB8072", "#FCCDE5", "#FFFFB3", "#8DD3C7",
                               "#BEBADA", "#D9D9D9"),
                    labels = c("Silver fir", "Vine maple", "Red alder", "Douglas-fir", "Cascara buckthorn", "Snag", "Western redcedar", "Western hemlock")) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text=element_text(size=12)) 

#  theme(legend.position = c(0.85, 0.7),
  #       text = element_text(size=18))


#### Comparison of species and DBHs accross the years 2021 and 2023 for research paper ####
# this code is based off of code from Bob paper for ease: https://github.com/bmcgaughey1/DroneLidarCode/blob/main/Rcode/FieldDataSummary.R
# Need to add the data that Bob used

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



#### Histogram of status codes resulting from FUSION indv. matching ####
plot_5 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_5_field_FUSIONtrees.csv", header = TRUE)
plot_6 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_6_field_FUSIONtrees.csv", header = TRUE)
plot_7 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_7_field_FUSIONtrees.csv", header = TRUE)
plot_8 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_8_field_FUSIONtrees.csv", header = TRUE)
plot_9 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_9_field_FUSIONtrees.csv", header = TRUE)
plot_10 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_10_field_FUSIONtrees.csv", header = TRUE)
plot_11 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_11_field_FUSIONtrees.csv", header = TRUE)
plot_12 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_12_field_FUSIONtrees.csv", header = TRUE)
plot_13 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_13_field_FUSIONtrees.csv", header = TRUE)
plot_14 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_14_field_FUSIONtrees.csv", header = TRUE)
plot_15 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_15_field_FUSIONtrees.csv", header = TRUE)
plot_16 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_16_field_FUSIONtrees.csv", header = TRUE)
plot_17 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_17_field_FUSIONtrees.csv", header = TRUE)
plot_18 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_18_field_FUSIONtrees.csv", header = TRUE)
plot_19 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_19_field_FUSIONtrees.csv", header = TRUE)
plot_20 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_20_field_FUSIONtrees.csv", header = TRUE)
plot_21 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_21_field_FUSIONtrees.csv", header = TRUE)
plot_22 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_22_field_FUSIONtrees.csv", header = TRUE)
plot_23 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_23_field_FUSIONtrees.csv", header = TRUE)
plot_24 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_24_field_FUSIONtrees.csv", header = TRUE)
plot_25 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_25_field_FUSIONtrees.csv", header = TRUE)
plot_26 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_26_field_FUSIONtrees.csv", header = TRUE)
plot_27 <- read.csv("data/FUSION_trees_after_ind_adjustment/plot_27_field_FUSIONtrees.csv", header = TRUE)


# a couple of plots for some reason have "Lean.Angle" instead of "Lean.Angle.From.Vertical" --> need to fix that to combine
colnames(plot_5) == colnames(plot_27)

colnames(plot_9)[14] <- "Lean.Angle.From.Vertical"
colnames(plot_15)[14] <- "Lean.Angle.From.Vertical"
colnames(plot_18)[14] <- "Lean.Angle.From.Vertical"
colnames(plot_23)[14] <- "Lean.Angle.From.Vertical"

all_trees <- rbind(plot_5, plot_6, plot_7, plot_8, plot_9, plot_10, plot_11, plot_12, plot_13, plot_14, plot_15, plot_16, plot_17, plot_18,
                   plot_19, plot_20, plot_21, plot_22, plot_23, plot_24, plot_25, plot_26, plot_27)


# want to separate back out species (new column)
library(stringr)
Species <- str_replace(all_trees$Tree.ID, pattern = "\\d+_\\d+_(\\w\\w\\w\\w)", replacement = "\\1") 
all_trees <- cbind(all_trees, Species)

# also tree plot
Plot_Num <- str_replace(all_trees$Tree.ID, pattern = "(\\d+)_\\d+_\\w\\w\\w\\w", replacement = "\\1") 
all_trees <- cbind(all_trees, Plot_Num)


ggplot(data = all_trees, aes(x = Status.Code, fill = Species)) +
  geom_histogram(colour = 'black', bins = 5) +
  xlab("Status Code") +
  ylab("Frequency") +
  scale_fill_brewer(palette = "Paired", 
                    labels = c("Silver fir", "Vine maple", "Red alder", "Pacific crabapple", "Sitka spruce", "Douglas-fir", "Cascara buckthorn", "Western redcedar", "Western hemlock", "UNKN")) +
  theme(legend.position = c(0.85, 0.7),
        text = element_text(size=18))


#### Histogram of cedar vs. other for modeling; OESF and ONF #####
# reading in data
ModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")

# need to filter out "failed" trees (trees with total lidar returns less than 350 --> those tops were clearly missedd and aren't acutally trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 300), ]


ModelData_WH_DF <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics_DF_WH.csv")

head(ModelData) # glimpse of data
head(ModelData_WH_DF) # glimpse of data --> need to combine with Model Data

# renaming a couple of the ModelData_WH_DF columns names to better match the ModelData names
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Xfield"] <- "X"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Yfield"] <- "Y"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseX"] <- "BaseX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseY"] <- "BaseY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseElevation"] <- "BaseElevation"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveHeight"] <- "Height"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopX"] <- "TopX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopY"] <- "TopY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveCrownDia"] <- "CrownDia"

# adding 100 to each of the WH/DF plot numbers so can later easily separate plots based on year
ModelData_WH_DF$Plot_Number <- ModelData_WH_DF$Plot_Number + 100

# new dataframes with only the column names that are the same between the dataframes
ModelData_WH_DF <- ModelData_WH_DF[ , colnames(ModelData_WH_DF) %in% colnames(ModelData)]
ModelData <- ModelData[ , colnames(ModelData) %in% colnames(ModelData_WH_DF)]

# combining the two datasets 
ModelData <- rbind(ModelData, ModelData_WH_DF)

# adding in site
ModelData[ , 'Area'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15 | ModelData$Plot_Num[i] > 100) {
    ModelData$Area[i] <- "OESF"
  } else {
    ModelData$Area[i] <- "ONF"
  }
}

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

# THPL vs. Other
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ggplot(data = ModelData, aes(x = Species, fill = Area)) +
  geom_histogram(colour = 'black', stat = "count") +
  xlab("Species") +
  ylab("Frequency") +
  scale_fill_manual(values = c("bisque2", "bisque3"),
                    labels = c("OESF", "ONF")) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20), 
        axis.text=element_text(size=18),
        legend.position = c(0.85, 0.7),
        text = element_text(size=18)) 


#### Map of predicted cedar locations; OESF site, 2021 and 2023 data, LiDAR only, top 3m ####
# Using the OESF only, lidar only, top 3m model (highest accuracy) model
final_tree_top3_OESF <- extract_workflow(final_res)

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


# graph, colored based on THPL prediction level
colnames(final_pred)[1] <- "Predicted Species"

library(ggspatial)

final_pred %>%
  ggplot(aes(X, Y, color = `Predicted Species`))  +
  geom_point(size = 0.7, alpha = 0.5) +
  scale_color_manual(values = c("grey40", "red")) +
  coord_cartesian( xlim = c(412900, 413200), ylim = c(5280750, 5281100), expand = TRUE, default = FALSE, clip = "on") + # zoomed in 
  guides(color = guide_legend(override.aes = list(size = 5)))  +
  theme_bw(base_size = 14) +
  ggspatial::annotation_scale(
    bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"))

#### Edited for paper: Map of predicted cedar locations; OESF site, 2021 and 2023 data, LiDAR only, top 3m ####
# Using the OESF only, lidar only, top 3m model (highest accuracy) model
final_tree_top3_OESF <- extract_workflow(final_res)

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

#### Edited for paper: 10-FOLD CV Map of predicted cedar locations; OESF site, 2021 and 2023 data, LiDAR only, top 3m ####
## same as in thesis, but change changed the aestetics and using different model!!! Final paper will use 10-fold CV -->
  # that model is slightly different than the 75/25 one as it is trained on all model data

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



#### Map of predicted cedar locations; ONF site,2023 data, LiDAR and MS, top 3m ####
#final_tree_top3_ONF_lidarMS <- extract_workflow(final_res)

# bringing in the rest of the data to predict on
# lidar data is in many different files from the lidar processing --> need to bring them in and merge them, and subset by plot
# then need to merge in the MS data
FileList_2023 <- list.files("data/LidarMetrics/LidarTrees/TreeTops/2023")
LidarTrees_2023 <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2023/", FileList_2023[1]), header = TRUE)

for (i in 2:length(FileList_2023)) { 
  file <- read.csv(paste0("data/LidarMetrics/LidarTrees/TreeTops/2023/", FileList_2023[i]), header = TRUE, sep = ",")
  
  LidarTrees_2023 <- rbind(LidarTrees_2023, file)
} 

# going to remove all trees not in ONF (this seemed like a pain to do in R, so I looked up the row number where the ONF data starts)
LidarTrees_2023 <- LidarTrees_2023[19409:22522,]

# renaming it
LidarTrees <- LidarTrees_2023

# double checking for weirdness in data
summary(LidarTrees)

# some lidar trees seem to have failed (likely not a tree where tree was identified) --> removing trees with 0s as values
LidarTrees <- LidarTrees[-which(LidarTrees$Elev.CV == 0),]

# some trees have an extremely small number of returns --> likely not actually a tree --> removing those
LidarTrees <- LidarTrees[which(LidarTrees$Total.return.count > 1500),]

# now double checking the data again
summary(LidarTrees)

# loading in the MS data
MSTrees <- read.csv("data/MS_Data/all_outside_plot_trees_w_MS_values_cleaned_for_thesis.csv", header = TRUE)

# checking for weirdness in MS data
summary(MSTrees)

# merging MS and Lidar trees together
all_trees <- merge(LidarTrees, MSTrees, by = c("GridHighX", "GridHighY"), all = FALSE)

# need to make column names to match that of the train/test data 
names(all_trees)[names(all_trees) == "GridHighX"] <- "X"
names(all_trees)[names(all_trees) == "GridHighY"] <- "Y"
names(all_trees)[names(all_trees) == "GroundElev"] <- "BaseElevation"
names(all_trees)[names(all_trees) == "GridMaxHt"] <- "Height"
names(all_trees)[names(all_trees) == "DataFile.x"] <- "Tree.ID"
all_trees[ , 'BaseX'] = NA
all_trees[ , 'BaseY'] = NA
all_trees[ , 'CrownDia'] = NA
all_trees[ , 'TopX'] = NA
all_trees[ , 'TopY'] = NA
all_trees[ , 'Year'] = NA
all_trees[ , 'Plot_Number'] = NA
all_trees[ , 'Tag_Num'] = NA
all_trees[ , 'DataFile'] = NA
all_trees[ , 'DBH'] = NA
all_trees[ , 'Total.Height'] = NA
all_trees[ , 'Height.To.Crown.Base'] = NA
all_trees[ , 'Max.Crown.Diameter'] = NA
all_trees[ , 'Min.Crown.Diameter'] = NA
all_trees[ , 'Crown.Rotation'] = NA
all_trees[ , 'R'] = NA
all_trees[ , 'G'] = NA
all_trees[ , 'B'] = NA
all_trees[ , 'Lean.Angle.From.Vertical'] = NA
all_trees[ , 'Lean.Azimuth'] = NA
all_trees[ , 'topElevation'] = NA
all_trees[ , 'Height'] = NA
all_trees[ , 'Elevation'] = NA
all_trees[ , 'Status.Code'] = NA

all_trees$Elev.maximum <- as.integer(all_trees$Elev.maximum) # model keeps getting mad for that Elev.max is an integer
# in the original ModelData and not in this data (gets dropped later anyway)
all_trees$Int.skewness <- as.numeric(all_trees$Int.skewness) # similar to above
all_trees$Int.kurtosis <- as.numeric(all_trees$Int.kurtosis)  # similar to above
all_trees$Int.L.skewness <- as.numeric(all_trees$Int.L.skewness)  # similar to above
all_trees$Int.L.kurtosis <- as.numeric(all_trees$Int.L.kurtosis)  # similar to above


all_trees <- all_trees[ , colnames(all_trees) %in% colnames(ModelData)]

pred <- predict(final_tree_top3_ONF_lidarMS, all_trees) # does prediction

augment(final_tree_top3_ONF_lidarMS, all_trees) # pulls what the trees predicted either way

pred %>%
  bind_cols(all_trees) %>%
  ggplot(aes(X, Y, color = .pred_class))  +
  geom_point(size = 0.5, alpha = 0.5) # graphing prediction

final_pred <- augment(final_tree_top3_ONF_lidarMS, all_trees)

# graph, colored based on THPL prediction level
colnames(final_pred)[1] <- "Predicted Species"

library(ggspatial)

final_pred %>%
  ggplot(aes(X, Y, color = `Predicted Species`))  +
  geom_point(size = 0.7, alpha = 0.5) +
  scale_color_manual(values = c("grey40", "red")) +
  coord_cartesian( xlim = c(443320, 443600), ylim = c(5245500, 5245850), expand = TRUE, default = FALSE, clip = "on") + # zoomed in 
  guides(color = guide_legend(override.aes = list(size = 5)))  +
  theme_bw(base_size = 14) +
  ggspatial::annotation_scale(
    bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"))

##### MS for health work: boxplots ####

# loading in data
all_trees <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv", header = TRUE)

all_trees[ , 'Area'] = NA

for (i in 1:length(all_trees$Tree.ID)) {
  if(all_trees$Plot_Num[i] < 15) {
    all_trees$Area[i] <- "OESF"
  } else {
    all_trees$Area[i] <- "ONF"
  }
}

# removing shadow trees
all_trees <- rbind(all_trees[which(all_trees$Area == "OESF" & all_trees$DBH > 0.20),], all_trees[which(all_trees$Area == "ONF" & all_trees$DBH > 0.30),])

# removing all tree species other than THPL and TSHE
all_trees <- all_trees[which(all_trees$Species == "THPL" | all_trees$Species == "TSHE"),]

# boxplots of different MS values across the different sites and species
library(ggplot2)

# nvdirededge
ggplot(all_trees,
       aes(x = Area, y = nvdirededge, fill = Species)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("sienna3", "mediumseagreen")) +
  ylab("Red-edge NVDI") +
theme(axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18), 
      axis.text=element_text(size=14),
      legend.position = c(0.91, 0.75),
      text = element_text(size=16)) 

# msr
ggplot(all_trees,
       aes(x = Area, y = msr, fill = Species)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("sienna3", "mediumseagreen")) +
  ylab("MSR") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        axis.text=element_text(size=14),
        legend.position = c(0.91, 0.75),
        text = element_text(size=16)) 




#### MS for health work: PCA and ANOVAs ####

library(vegan)
trees <- all_trees[,]
# creating new dataframe with only the explan variables
trees_explan <- cbind(trees[, 1:8], trees[, 23])
# creating new dataframe with only the response variables
trees_resp <- trees[, 9:22]
# relativizing by range because 1) data is on very different scales; and 2) some values are lower than 1
trees_resp <- decostand(x = trees_resp, method = "range")
# applying Euclidean distance measure
trees_dist <- vegdist(x = trees_resp, method = "euclidean")

# ONF
trees_ONF <- trees[trees$Area == "ONF", ]
trees_explan_ONF <- cbind(trees_ONF[, 1:8], trees_ONF[, 23])
trees_resp_ONF <- trees_ONF[, 9:22]
trees_resp_ONF <- decostand(x = trees_resp_ONF, method = "range")
trees_dist_ONF <- vegdist(x = trees_resp_ONF, method = "euclidean")
# OESF
trees_OESF <- trees[trees$Area == "OESF", ]
trees_explan_OESF <-cbind(trees_OESF[, 1:8], trees_OESF[, 23])
trees_resp_OESF <- trees_OESF[, 9:22]
trees_resp_OESF <- decostand(x = trees_resp_OESF, method = "range")
trees_dist_OESF <- vegdist(x = trees_resp_OESF, method = "euclidean")


library(ggordiplots)
library(ggbiplot)

# setting up and quick look at the PCA
trees_PCA <- princomp(trees_resp, cor = TRUE)
# summary of PCA
summary_PCA <- summary(trees_PCA, loadings = TRUE, cutoff = 0)
summary_PCA$loadings
# write.table(summary_PCA$loadings, file = "tables/summary_PCA.csv")
plot(trees_PCA,
     main = "PCA")

# plotting
biplot(trees_PCA)

ggbiplot(trees_PCA, alpha = 0, var.axes = FALSE) +
  geom_point(aes(colour = trees$Species, shape = trees$Area)) +
  scale_colour_manual(name = "Species",
                      labels = c("THPL", "TSHE"),
                      values = c("sienna3", "mediumseagreen")) +
  scale_shape_manual(name = "Study Area",
                     labels = c("OESF", "ONF"),
                     values = c(6, 17)) +
  xlab("Principal Component 1 (72% variance explained)") +
  ylab("Principal Component 2 (17% variance explained)") +
  guides(color = guide_legend(override.aes = list(size = 3)))  +
  theme_bw(base_size = 13) 


# looking at site for Comp.2 --> testing using ANOVA
trees_PCA_Scores <- data.frame(trees_PCA$scores, Area = trees$Area)
adonis2(trees_PCA_Scores$Comp.2 ~ Area,
        data = trees_PCA_Scores,
        method = "euc")

ggplot(data = trees_PCA_Scores, aes(x = Area, y = Comp.2)) +
  geom_boxplot() +
  geom_jitter(aes(colour = Area), width = 0.3, height = 0) +
  scale_colour_manual(values = c("bisque2", "bisque3"),
                    labels = c("OESF", "ONF")) +
  ylab("Principal Component 2 Scores") +
  guides(color = guide_legend(override.aes = list(size = 3)))  +
  theme_bw(base_size = 14) 

# focusing on just first PC
trees_PCA_Scores <- data.frame(trees_PCA$scores, Species = trees$Species)
adonis2(trees_PCA_Scores$Comp.1 ~ Species,
        data = trees_PCA_Scores,
        method = "euc")

# results are significant but only explain about 2% of the variance --> going to subset by plot and re-run
ggplot(data = trees_PCA_Scores, aes(x = Species, y = Comp.1)) +
  geom_boxplot() +
  geom_jitter(aes(colour = Species), width = 0.3, height = 0) +
  theme_bw() +
  ggtitle("PCA Comp.1 Species Comparison") +
  scale_colour_manual(values = c("sienna3", "mediumseagreen"))

# first, have to add in site to dataframe with trees_PCA scores
trees_PCA_Scores <- data.frame(trees_PCA$scores, Species = trees$Species, Site = trees$Site)
trees_PCA_scores_ONF <- trees_PCA_Scores[trees_PCA_Scores$Site == "ONF",]
adonis2(trees_PCA_scores_ONF$Comp.1 ~ Species,
        data = trees_PCA_scores_ONF,
        method = "euc")

ggplot(data = trees_PCA_scores_ONF, aes(x = Species, y = Comp.1)) +
  geom_boxplot() +
  geom_jitter(aes(colour = Species), width = 0.3, height = 0) +
  scale_colour_manual(values = c("sienna3", "mediumseagreen"),
                      labels = c("THPL", "TSHE")) +
  ylab("Principal Component 1 Scores") +
  guides(color = guide_legend(override.aes = list(size = 3)))  +
  theme_bw(base_size = 14) 

# next, OESF
trees_PCA_scores_OESF <- trees_PCA_Scores[trees_PCA_Scores$Site == "OESF",]
adonis2(trees_PCA_scores_OESF$Comp.1 ~ Species,
        data = trees_PCA_scores_OESF,
        method = "euc")

ggplot(data = trees_PCA_scores_OESF, aes(x = Species, y = Comp.1)) +
  geom_boxplot() +
  geom_jitter(aes(colour = Species), width = 0.3, height = 0) +
  scale_colour_manual(values = c("sienna3", "mediumseagreen"),
                      labels = c("THPL", "TSHE")) +
  ylab("Principal Component 1 Scores") +
  guides(color = guide_legend(override.aes = list(size = 3)))  +
  theme_bw(base_size = 14) 



#### Intensity correction figures ####

ModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")
ModelData_WH_DF <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics_DF_WH.csv")

# need to change plot numbers for DF/WH so that they are not the same as the Cedar data ones
ModelData_WH_DF$Plot_Number <- ModelData_WH_DF$Plot_Number + 100
head(ModelData) # glimpse of data
head(ModelData_WH_DF) # glimpse of data --> need to combine with Model Data

# renaming a couple of the ModelData_WH_DF columns names to better match the ModelData names
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Xfield"] <- "X"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Yfield"] <- "Y"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseX"] <- "BaseX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseY"] <- "BaseY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseElevation"] <- "BaseElevation"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveHeight"] <- "Height"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopX"] <- "TopX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopY"] <- "TopY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveCrownDia"] <- "CrownDia"



# new dataframes with only the column names that are the same between the dataframes
ModelData_WH_DF <- ModelData_WH_DF[ , colnames(ModelData_WH_DF) %in% colnames(ModelData)]
ModelData <- ModelData[ , colnames(ModelData) %in% colnames(ModelData_WH_DF)]

# combining the two datasets 
ModelData <- rbind(ModelData, ModelData_WH_DF)

library(ggplot2)
ModelData$Plot_Number <- as.character(ModelData$Plot_Number) 


# adding in Year as variable

ModelData[ , 'Year'] = NA

ModelData$Plot_Number <- as.integer(ModelData$Plot_Number)

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Number[i] == 11 | ModelData$Plot_Number[i] == 12 | ModelData$Plot_Number[i] == 13) {
    ModelData$Year[i] <- "2021"
  } else {
    if(ModelData$Plot_Number[i] > 30) {
      ModelData$Year[i] <- "2021" } else {
        ModelData$Year[i] <- "2023"
      }
  }
}

# comparing across the years
ggplot(ModelData,
       aes(x = Year, y = Int.mean)) + 
  geom_boxplot() +
  ylab("Mean Intensity") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text=element_text(size=12),
        text = element_text(size=12)) 



# normalizing data using mean and standard dev intensity values that were calculated on full dataset (z-score normalization)
ModelData[which(ModelData$Year == "2021"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2021"), "Int.mean"] - 192.6818) / 17.02009777
ModelData[which(ModelData$Year == "2023"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2023"), "Int.mean"] - 119.895) / 57.01547338


ggplot(ModelData[which(ModelData$Species == "TSHE"),],
       aes(x = Year, y = Int.mean, fill = Species)) + 
  geom_boxplot()  +
  scale_fill_manual(values = "mediumseagreen") +
  ylab("Mean Intensity") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        legend.position = c(0.91, 0.75),
        axis.text=element_text(size=12),
        text = element_text(size=12)) 














