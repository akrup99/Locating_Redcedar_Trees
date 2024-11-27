# Code by Ally Kruper
# Last updated 1/24/24

# Purpose: taking .csv files of cedar tree plots that have gone through original plot 
# adjustment in ArcGIS Pro and need to be prepped to be imported into FUSION for individual
# tree adjustment

# Specifically: need to add height (from equations from FVS Pacific), live crown min and max (from ratio), etc; add colors based on species and 
# if lidar visble; add new column for anomalies (to be changed in FUSION) based on
# previous anomaly codes


#### Step 1: Load and double check .csv files coming in from ArcGIS Pro ####
# first, did a little bit of re-formatting in excel to have 2022 plots 5, 6, and 7 match the others (could have done in R, quicker in Excel)
# also, ArcGIS added some random columns through that process (Object ID, etc) that I deleted
# loading in all of the csv's
plot_5 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_5.csv", header = TRUE)
plot_6 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_6.csv", header = TRUE)
plot_7 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_7.csv", header = TRUE)
plot_8 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_8.csv", header = TRUE)
plot_9 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_9.csv", header = TRUE)
plot_10 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_10.csv", header = TRUE)
plot_11 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_11.csv", header = TRUE)
plot_12 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_12.csv", header = TRUE)
plot_13 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_13.csv", header = TRUE)
plot_14 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_14.csv", header = TRUE)
plot_15 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_15.csv", header = TRUE)
plot_16 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_16.csv", header = TRUE)
plot_17 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_17.csv", header = TRUE)
plot_18 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_18.csv", header = TRUE)
plot_19 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_19.csv", header = TRUE)
plot_20 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_20.csv", header = TRUE)
plot_21 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_21.csv", header = TRUE)
plot_22 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_22.csv", header = TRUE)
plot_23 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_23.csv", header = TRUE)
plot_24 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_24.csv", header = TRUE)
plot_25 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_25.csv", header = TRUE)
plot_26 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_26.csv", header = TRUE)
plot_27 <- read.csv("data/plot_adjusted_from_ArcPro/PlotAdjusted_plot_27.csv", header = TRUE)

# combining them all together for easier manipulation
all_plots <- rbind(plot_5, plot_6, plot_7, plot_8, plot_9, plot_10, plot_11, plot_12, plot_13, plot_14, plot_15, plot_16, plot_17, plot_18,
      plot_19, plot_20, plot_21, plot_22, plot_23, plot_24, plot_25, plot_26, plot_27)

# double checking the data for weirdness introduced in data or mismatching rows/columns
unique(all_plots$Plot_Num)

range(na.omit(all_plots$DBH_cm))
range(na.omit(all_plots$Dist_m))
range(na.omit(all_plots$Azimuth))
range(all_plots$X)
range(all_plots$Y)

##### Step 2: Calculate heights based on species ####
# Using equations based on FVS Pacific Northwest Coast (PN) Variant

# First, checking which species we need equations for
unique(all_plots$Species)
 # "THPL" "TSHE" "ACCI" "ALRU" "PSME" "RHPU" "UNKN" "MAFU" "PISI" "ABAM"

# There are 4 species that FVS doesn't have specific equations for (ACCI, RHPU, UNKN, MAFU)
  # below is how a quick look to see how that could potential impact the data
  # based on the number of and average size of the trees
length(all_plots[which(all_plots$Species == "ACCI"), "Species"]) 
mean(all_plots[which(all_plots$Species == "ACCI"), "DBH_cm"])
  # there are 22 total ACCI trees in the data, average DBH 7cm

length(all_plots[which(all_plots$Species == "RHPU"), "Species"]) 
mean(all_plots[which(all_plots$Species == "RHPU"), "DBH_cm"])
  # there are 38 total RHPU trees in the data, average DBH 6.2cm

length(all_plots[which(all_plots$Species == "UNKN"), "Species"]) 
mean(all_plots[which(all_plots$Species == "UNKN"), "DBH_cm"])
range(all_plots[which(all_plots$Species == "UNKN"), "DBH_cm"])
  # there are 7 total UNKN trees in the data, average DBH 19cm,
  # range is 6.7 - 49.5cm DBH

length(all_plots[which(all_plots$Species == "MAFU"), "Species"]) 
mean(all_plots[which(all_plots$Species == "MAFU"), "DBH_cm"])
  # there are 3 total MAFU trees in the data, average DBH 5.3cm

# going to use coefficients from FVS location 609: Olympic National Forest
# there is an "OTHER" option that I am going to use for 
  # those species that don't have their own equation (ACCI, RHPU, UNKN, and MAFU)

# equations assume DBH is in inches (not cm) --> need to change that
all_plots[,"DBH_cm"] <- all_plots[,"DBH_cm"] / 2.54
colnames(all_plots) <- (c("Site", "Year", "Plot_Num", "Tag_Num", "Species", "DBH_in", "LiDAR_Y_N", "Dist_m", "Azimuth", "Anom_Num", "X1_Anom", "X2_Anom", "X3_Anom", "X4_Anom", "Health_Num", "X", "Y"))
# updated colnames to reflect the fact that DBH is now in inches

# implementing equations; 
    # different equations for >= 3.0in DBH and for <3.0in DBH
    # coefficients in equations depend on species

all_plots[, 'Height_ft'] = NA # creating new empty column for the heights (guessing in feet?) to go in

all_plots[which(is.na(all_plots$DBH_in)), "DBH_in"] <- 8.892359 # need to do something where we don't have DBH --> 
                                                                   # going to set it to 8.892359 (average DBH)

eq.coef <- data.frame( # making dataframe with all the species and associated coefficients used for the equations
  Species = c("THPL", "TSHE", "ALRU", "PSME", "PISI", "ABAM", "ACCI", "RHPU", "UNKN", "MAFU"),
  p2 = c(665.0944, 609.4235, 139.4551, 1091.853, 3844.388, 697.6316, 1709.723, 1709.723, 1709.723, 1709.723),
  p3 = c(5.5002, 5.5919, 4.6989, 5.2936, 7.068, 6.6807, 5.8887, 5.8887, 5.8887, 5.8887),
  p4 = c(-0.3246, -0.3841, -0.7682, -0.2648, -0.2122, -0.4161, -0.2286, -0.2286, -0.2286, -0.2286)
)

# The under 3 inches kept giving negative numbers and the over 3in equation seemed to work fine for those
  # smaller trees anyway, so I ended up doing all trees with that equation. However, I kept the if/else statement just in case that changes
for(i in 1:nrow(all_plots)) { # runs the following code for functionally every tree in the dataset
  species <- all_plots[i, "Species"] # finds out which species is associated with the tree in question
  p2 <- eq.coef[which(eq.coef$Species == species), "p2"] # based on the tree species, pulls p2 from the coefficient dataset
  p3 <- eq.coef[which(eq.coef$Species == species), "p3"] # based on the tree species, pulls p3 from the coefficient dataset
  p4 <- eq.coef[which(eq.coef$Species == species), "p4"] # based on the tree species, pulls p4 from the coefficient dataset
  if(all_plots[i, "DBH_in"] >= 3.0) { # for trees great than/equal to 3 inches
    all_plots[i,"Height_ft"] <- 4.5 + p2 * exp(-p3*all_plots[i,"DBH_in"]^p4)
  } else {
    all_plots[i,"Height_ft"] <- 4.5 + p2 * exp(-p3*all_plots[i,"DBH_in"]^p4) # just going to do this equation for both over and under 3inches
    # all_plots[i, "Height_ft"] <- ((4.5 + p2 * exp(-p3*3.0^p4) - 4.51) * (all_plots[i,"DBH_in"] - 3) / 2.7) + 4.51 # this is the equation for under 3 inches -->
      # kept giving negatives --> just going to do all DBHs through the above 3in equation (seems to work fine)
  }
}

#### Step 3: Set up current columns to match how Bob sets up his (add R,B,G, Status Code) #####
# Giving all Lidar Not visible and dead trees status code 1 (won't be moved); code 0 will be moved
all_plots[, 'statusCode'] = NA # making new column for status code

# need to first change the NAs in X2_Anom, X3_Anom, and X4_Anom to 0 so they can be indexed
all_plots[which(is.na(all_plots$X2_Anom)), "X2_Anom"] <- 0
all_plots[which(is.na(all_plots$X3_Anom)), "X3_Anom"] <- 0
all_plots[which(is.na(all_plots$X4_Anom)), "X4_Anom"] <- 0

for(i in 1:nrow(all_plots)) {
  if(all_plots[i, "LiDAR_Y_N"] == "N" | all_plots[i, "X1_Anom"] == 1 | all_plots[i, "X2_Anom"] == 1 | all_plots[i, "X3_Anom"] == 1 | all_plots[i, "X4_Anom"] == 1) {
    all_plots[i, "statusCode"] <- 1
  } else {
    all_plots[i, "statusCode"] <- 0
  }
    
}


# Next, adding R, B, G values (so that each tree can have a different color based on species)
# THPL = Orange; TSHE = blue-green, ALRU = purple, PSME = green, PISI = blue, ABAM = silver, ACCI = light green,
# RHPU = Yellow, UNKN = black,  MAFU = pink, status code 1 (won't be moved) = red
color.codes <- data.frame( # making dataframe with all the species and associated color codes
  Species = c("THPL", "TSHE", "ALRU", "PSME", "PISI", "ABAM", "ACCI", "RHPU", "UNKN", "MAFU"),
  R = c(255, 32, 102, 71, 0, 128, 178, 204, 0, 204),
  B = c(0, 158, 204, 73, 153, 128, 102, 0, 0, 102),
  G = c(128, 144, 0, 183, 0, 128, 255, 204, 0, 0)
)

# making new columns for the colors 
all_plots[, 'R'] = NA
all_plots[, 'G'] = NA
all_plots[, 'B'] = NA

for(i in 1:nrow(all_plots)) { # runs the following code for functionally every tree in the dataset
  if(all_plots[i, "statusCode"] == 0) {
    species <- all_plots[i, "Species"] # finds out which species is associated with the tree in question
    all_plots[i, "R"] <- color.codes[which(color.codes$Species == species), "R"] 
    all_plots[i, "B"]  <- color.codes[which(color.codes$Species == species), "B"] 
    all_plots[i, "G"]  <- color.codes[which(color.codes$Species == species), "G"] 
  } else {
    all_plots[i, "R"] <- 204
    all_plots[i, "B"] <- 0
    all_plots[i, "G"] <- 0
  }
}



# Also, need to convert DBH and height back to meters
all_plots[,"DBH_in"] <- all_plots[,"DBH_in"] * 2.54
all_plots[,"Height_ft"] <- all_plots[,"Height_ft"] / 3.281
colnames(all_plots) <- (c("Site", "Year", "Plot_Num", "Tag_Num", "Species", "DBH_cm", "LiDAR_Y_N", "Dist_m", "Azimuth", "Anom_Num", "X1_Anom", "X2_Anom", "X3_Anom", "X4_Anom", "Health_Num", "X", "Y",
                          "Height_m", "statusCode", "R", "G", "B"))


# Also, combining plot #, tag #, and tree species all into one "TreeID" column
all_plots[, 'TreeID'] = NA # adding new empty TreeID column

for(i in 1:nrow(all_plots)) {
  all_plots[i,"TreeID"] <- paste(all_plots[i, "Plot_Num"], all_plots[i, "Tag_Num"], all_plots[i, "Species"], sep = "_")
}

#### Step 4: Add columns and data for CBH, min crown diameter, etc. ####
## Modified from code from Bob (DBH to height is from FVS, the rest (the crown ratios) are simple ratio, not from equation):
for(i in unique(all_plots[,"Plot_Num"])) {
  FUSIONtrees <- data.frame(
    TreeID = all_plots[all_plots$Plot_Num == i,"TreeID"],
    X = all_plots[all_plots$Plot_Num == i,"X"],
    Y = all_plots[all_plots$Plot_Num == i,"Y"],
    Elevation = 0.0,
    Height_m = all_plots[all_plots$Plot_Num == i,"Height_m"],
    CBH_m = all_plots[all_plots$Plot_Num == i,"Height_m"] * 0.6,
    MinCrownDia_m = all_plots[all_plots$Plot_Num == i,"Height_m"] * 0.16,
    MaxCrownDia_m = all_plots[all_plots$Plot_Num == i,"Height_m"] * 0.16,
    rotation = 0.0,
    R = all_plots[all_plots$Plot_Num == i,"R"],
    G = all_plots[all_plots$Plot_Num == i,"G"],
    B = all_plots[all_plots$Plot_Num == i,"B"],
    DBH_m = all_plots[all_plots$Plot_Num == i,"DBH_cm"] / 100, # need to convert DBH to meters to match the units of the lidar data
    LeanFromVertical = 0,
    LeanAzimuth = 0,
    StatusCode = all_plots[all_plots$Plot_Num == i,"statusCode"]
  )
  
  filename <- paste("data/FUSIONtrees_forIndAdjust/plot_",i,"_field_FUSIONtrees.csv", sep = "")
  write.csv(x = FUSIONtrees, file = filename)
}


