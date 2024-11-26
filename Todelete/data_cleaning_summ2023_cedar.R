## Cleaning the western redcedar plots data from summer 2023
# Original code from Courtney Bobsin, modified and cleaning by Ally Kruper
# Last modified 10/30/23

# inputing data
cedar_data <- read.csv("data/Entered_Cedar_T3_2023_TreeDataSheets_CompiledTrees_Cleaned.csv", nrows = 1437)


#### checking categorical variables ####

# Species
unique(cedar_data$Species)


# Site, Year
unique(cedar_data$Site)
unique(cedar_data$Year)

# Plot number
unique(cedar_data$Plot_Num)



# Tag

# Column names are weird --> going to change them
colnames(cedar_data) <- (c("Site", "Year", "Plot_Num", "Tag_Num", "Species", "DBH_cm", "LiDAR_Y_N", "Dist_m", "Azimuth", "Anom_Num", "X1_Anom", "X2_Anom", "X3_Anom", "X4_Anom", "Health_Num"))
unique(cedar_data$Tag_Num)

# Lidar Y/N
unique(cedar_data$LiDAR_Y_N)


# Anomaly num
unique(cedar_data$Anom_Num)


# Health code
unique(cedar_data$Health_Num)



#### checking continuous variables ####

# DBH
is.numeric(cedar_data$DBH_cm)
unique(cedar_data$DBH_cm)
cedar_data[which(cedar_data$DBH_cm == ""), "DBH_cm"] <- NA

range(na.omit(cedar_data$DBH_cm))
cedar_data[which(cedar_data$DBH_cm == "<10"), "DBH_cm"] <- 10.01 # Changing all <10 to 10.01
min(na.omit(cedar_data$DBH_cm))
max(na.omit(cedar_data$DBH_cm))

cedar_data$DBH_cm <- as.numeric(cedar_data$DBH_cm)
mean(na.omit(cedar_data$DBH_cm))


# Dist_m
is.numeric(cedar_data$Dist_m)

range(na.omit(cedar_data$Dist_m))
min(na.omit(cedar_data$Dist_m))
max(na.omit(cedar_data$Dist_m))
mean(na.omit(cedar_data$Dist_m))

# Azimuth
is.numeric(cedar_data$Azimuth)

range(na.omit(cedar_data$Azimuth))
min(na.omit(cedar_data$Azimuth))
max(na.omit(cedar_data$Azimuth))
mean(na.omit(cedar_data$Azimuth))

# 1st, 2nd, 3rd, 4th Anomaly Number
is.numeric(cedar_data$X1_Anom)
range(na.omit(cedar_data$X1_Anom))

is.numeric(cedar_data$X2_Anom)
range(na.omit(cedar_data$X2_Anom))

is.numeric(cedar_data$X3_Anom)
range(na.omit(cedar_data$X3_Anom))

is.numeric(cedar_data$X4_Anom)
range(na.omit(cedar_data$X4_Anom))


#### Export new updated .csv ####
write.csv(cedar_data, file = "data/Entered_Cedar_T3_2023_TreeDataSheets_CompiledTrees_Cleaned.csv")





