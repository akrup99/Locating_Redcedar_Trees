# CODE 7
# This code uses 1) the field tree locations individually adjusted using the lidar; 2) the automatically segmented trees from the lidar  
# code "2023_Cedar_Process_Lidar_Data" (trees outside of field plots); and 3) the processed MS images 

# Ally Kruper
# Last updated 2/26/24
# Code to clip MS data to the tree points and extract data from MS rasters

# NOTE: Between this step and the "Initial MS Processing Bob Code", I had to shift the MS rasters to match 
# the lidar CHMs. This was done because the Lidar sensor had a superior GPS --> lidar CHMs considered the "truth"
# in therms of exact tree locations --> shifted MS rasters as a whole to match lidar CHMs. Shift was between 0-4m in each direction
# (x and y), and typically on 1-2m 

# The tree location data at this point is from individual tree matching within FUSION.

### NOTE: I fully admit that this code is relatively very convoluted, and I may in the future re-do parts of it to simplify it. The specification of the Terra package (particularly  
  # surrounding object types) led me to take different paths than I orginally expected --> this is not the simplist way to do things, but it is how I did it.

#### Step 1: Consolidating and prepping tree data ####
# Currently have all of tree data in different .csv files based in individual plots --> going to combine
# into one object for this purpose
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

# want to subset to get rid of trees not found/matched in lidar (status codes 1 and 2) and those with probably incorrect species (status code 4)
all_trees <- subset(all_trees, Status.Code != 1)
all_trees <- subset(all_trees, Status.Code != 2)
all_trees <- subset(all_trees, Status.Code != 4)

barplot(table(all_trees$Species),
              ylab = "Frequency",
              xlab = "Species")
#### Step 2: Extracting data from MS Imagery####
# Ended up just re-organizing my files outside of R Studio (was taking too long in R Studio). Did it based on raster type (ex. RGB; nir)

## The following is most certainly not the best way to do things, but it is what I did.
# Goal: for each plot and for each image, create a shapefile --> can import that shapefile into ArcGIS Pro to double check it, and to convert to .csv to re-combine all of the plots together

cigreen_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cigreen", full.names = TRUE)
cirededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cirededge", full.names = TRUE)
ciredrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/ciredrededge", full.names = TRUE) 
fcnir_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/fcnir", full.names = TRUE)
fcrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/fcrededge", full.names = TRUE)
msr_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msr", full.names = TRUE)
msrrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msrrededge", full.names = TRUE)
msrredrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msrredrededge", full.names = TRUE)
nir_re_g_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nir_re_g", full.names = TRUE)
nvdinir_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdinir", full.names = TRUE)
nvdirededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdirededge", full.names = TRUE)
nvdiredrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdiredrededge", full.names = TRUE)
rgb_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/rgb", full.names = TRUE)

OESF_Cedar_Plot_8_9 <- c(7, 8, 9)
OESF_Cedar_Plot_10 <- c(10)
OESF_Cedar_Plot_14 <- c(5, 6, 14)
ONF_Cedar_Plot_15_16 <- c(15, 16)
ONF_Cedar_17_thru_22 <- c(17, 18, 19, 20, 21, 22)
ONF_Cedar_Plot_23_24_25 <- c(23, 24, 25)
ONF_Cedar_Plot_26_27 <- c(26, 27)

# list of cedar plots (in vectors based on which image each plot is in); order of list corresponds to order of files 
# from list.files function above
CedarPlotList <- list(OESF_Cedar_Plot_10, OESF_Cedar_Plot_14, OESF_Cedar_Plot_8_9, ONF_Cedar_Plot_15_16, 
                      ONF_Cedar_17_thru_22, ONF_Cedar_Plot_23_24_25, ONF_Cedar_Plot_26_27)


# Going to process each image type at a time.
library(terra)

# Cigreen
for (i in 1:length(cigreen_filelist)) {
  cigreen <- rast(cigreen_filelist[i])
    
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_cigreen <- terra::extract(x = cigreen, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
     writeVector(extraction_cigreen, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cigreen/", "Plot_", CedarPlotList[[i]][l], "_", "cigreen"), overwrite = TRUE)
    
       }
}

# Cirededge
for (i in 1:length(cirededge_filelist)) {
  cirededge <- rast(cirededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_cirededge <- terra::extract(x = cirededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_cirededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cirededge/", "Plot_", CedarPlotList[[i]][l], "_", "cirededge"), overwrite = TRUE)
    
    plot(extraction_cirededge)
  }
}

# Ciredrededge
for (i in 1:length(ciredrededge_filelist)) {
  ciredrededge <- rast(ciredrededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_ciredrededge <- terra::extract(x = ciredrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_ciredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/ciredrededge/", "Plot_", CedarPlotList[[i]][l], "_", "ciredrededge"), overwrite = TRUE)
    
    plot(extraction_ciredrededge)
  }
}

# fcnir
for (i in 1:length(fcnir_filelist)) {
  fcnir <- rast(fcnir_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_fcnir <- terra::extract(x = fcnir, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_fcnir, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/fcnir/", "Plot_", CedarPlotList[[i]][l], "_", "fcnir"), overwrite = TRUE)
    
    plot(extraction_fcnir)
  }
}

# fcrededge
for (i in 1:length(fcrededge_filelist)) {
  fcrededge <- rast(fcrededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_fcrededge <- terra::extract(x = fcrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_fcrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/fcrededge/", "Plot_", CedarPlotList[[i]][l], "_", "fcrededge"), overwrite = TRUE)
    
    plot(extraction_fcrededge)
  }
}

# msr
for (i in 1:length(msr_filelist)) {
  msr <- rast(msr_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_msr <- terra::extract(x = msr, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_msr, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msr/", "Plot_", CedarPlotList[[i]][l], "_", "msr"), overwrite = TRUE)
    
    plot(extraction_msr)
  }
}

# msrrededge
for (i in 1:length(msrrededge_filelist)) {
  msrrededge <- rast(msrrededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_msrrededge <- terra::extract(x = msrrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_msrrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msrrededge/", "Plot_", CedarPlotList[[i]][l], "_", "msrrededge"), overwrite = TRUE)
    
    plot(extraction_msrrededge)
  }
}

# msrredrededge
for (i in 1:length(msrredrededge_filelist)) {
  msrredrededge <- rast(msrredrededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_msrredrededge <- terra::extract(x = msrredrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_msrredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msrredrededge/", "Plot_", CedarPlotList[[i]][l], "_", "msrredrededge"), overwrite = TRUE)
    
    plot(extraction_msrredrededge)
  }
}

# nir_re_g
for (i in 1:length(nir_re_g_filelist)) {
  nir_re_g <- rast(nir_re_g_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_nir_re_g <- terra::extract(x = nir_re_g, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_nir_re_g, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nir_re_g/", "Plot_", CedarPlotList[[i]][l], "_", "nir_re_g"), overwrite = TRUE)
    
    plot(extraction_nir_re_g)
  }
}

# nvdinir
for (i in 1:length(nvdinir_filelist)) {
  nvdinir <- rast(nvdinir_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_nvdinir <- terra::extract(x = nvdinir, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_nvdinir, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdinir/", "Plot_", CedarPlotList[[i]][l], "_", "nvdinir"), overwrite = TRUE)
    
    plot(extraction_nvdinir)
  }
}

# nvdirededge
for (i in 1:length(nvdirededge_filelist)) {
  nvdirededge <- rast(nvdirededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_nvdirededge <- terra::extract(x = nvdirededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_nvdirededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdirededge/", "Plot_", CedarPlotList[[i]][l], "_", "nvdirededge"), overwrite = TRUE)
    
    plot(extraction_nvdirededge)
  }
}

# nvdiredrededge
for (i in 1:length(nvdiredrededge_filelist)) {
  nvdiredrededge <- rast(nvdiredrededge_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_nvdiredrededge <- terra::extract(x = nvdiredrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_nvdiredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdiredrededge/", "Plot_", CedarPlotList[[i]][l], "_", "nvdiredrededge"), overwrite = TRUE)
    
    plot(extraction_nvdiredrededge)
  }
}

# rgb
for (i in 1:length(rgb_filelist)) {
  rgb <- rast(rgb_filelist[i])
  
  for (l in 1:length(CedarPlotList[[i]])) {
    X <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "X"]
    Y <- all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == CedarPlotList[[i]][l], "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_rgb <- terra::extract(x = rgb, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_rgb, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/rgb/", "Plot_", CedarPlotList[[i]][l], "_", "rgb"), overwrite = TRUE)
    
    plot(extraction_rgb)
  }
}


##### Step 3: Combining Extracted Data into One Giant Data Matrix ####
# Between last step and this one, imported all of the shapefiles into ArcGIS Pro --> used ArcGIS Pro to 
  # export the attribute table of each shapefile as a .csv file (couldn't figure out how to do that in R)

# Currently, all .csv's are organized in files based on image (ie. cigreen, etc.)

# removing plot 11, 12, 13 from all_trees list because don't have MS imagery for them (should have done that before)
all_trees <- all_trees[-which(all_trees$Plot_Num == 11),]
all_trees <- all_trees[-which(all_trees$Plot_Num == 12),]
all_trees <- all_trees[-which(all_trees$Plot_Num == 13),]


## cigreen

all_trees[ , 'cigreen'] = NA

# making a list of all of the file names for cigreen (one for each plot)
cigreen_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/cigreen", full.names = TRUE)

for (i in 1:length(cigreen_folders)) {
  cigreen_csv <- read.csv(cigreen_folders[i], header = TRUE)
  plot_num <- cigreen_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    cigreen_values <- cigreen_csv[,"OESF_Cedar"] 
    
    } else {
      cigreen_values <- cigreen_csv[,"ONF_Cedar_"]
    }
  
  all_trees[all_trees$Plot_Num == plot_num, "cigreen"] <- cigreen_values
  
}

all_trees[, "cigreen"]


## cirededge

all_trees[ , 'cirededge'] = NA
cirededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/cirededge", full.names = TRUE)
for (i in 1:length(cirededge_folders)) {
  cirededge_csv <- read.csv(cirededge_folders[i], header = TRUE)
  plot_num <- cirededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    cirededge_values <- cirededge_csv[,"OESF_Cedar"] 
  } else {
    cirededge_values <- cirededge_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "cirededge"] <- cirededge_values
}
all_trees[, "cirededge"]


## ciredrededge
all_trees[ , 'ciredrededge'] = NA
ciredrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/ciredrededge", full.names = TRUE)
for (i in 1:length(ciredrededge_folders)) {
  ciredrededge_csv <- read.csv(ciredrededge_folders[i], header = TRUE)
  plot_num <- ciredrededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    ciredrededge_values <- ciredrededge_csv[,"OESF_Cedar"] 
  } else {
    ciredrededge_values <- ciredrededge_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "ciredrededge"] <- ciredrededge_values
}
all_trees[, "ciredrededge"]

## fcnir
all_trees[ , 'fcnir_band1'] = NA
all_trees[ , 'fcnir_band2'] = NA
all_trees[ , 'fcnir_band3'] = NA
fcnir_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/fcnir", full.names = TRUE)
for (i in 1:length(fcnir_folders)) {
  fcnir_csv <- read.csv(fcnir_folders[i], header = TRUE)
  plot_num <- fcnir_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    fcnir_values <- fcnir_csv[,"OESF_Ceda0"] 
  } else {
    fcnir_values <- fcnir_csv[,"ONF_Cedar0"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "fcnir_band1"] <- fcnir_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    fcnir_values <- fcnir_csv[,"OESF_Ceda1"] 
  } else {
    fcnir_values <- fcnir_csv[,"ONF_Cedar1"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "fcnir_band2"] <- fcnir_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    fcnir_values <- fcnir_csv[,"OESF_Ceda2"] 
  } else {
    fcnir_values <- fcnir_csv[,"ONF_Cedar2"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "fcnir_band3"] <- fcnir_values
}
all_trees[, "fcnir_band1"]
all_trees[, "fcnir_band2"]
all_trees[, "fcnir_band3"]

## fcrededge
all_trees[ , 'fcrededge_band1'] = NA
all_trees[ , 'fcrededge_band2'] = NA
all_trees[ , 'fcrededge_band3'] = NA
fcrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/fcrededge", full.names = TRUE)
for (i in 1:length(fcrededge_folders)) {
  fcrededge_csv <- read.csv(fcrededge_folders[i], header = TRUE)
  plot_num <- fcrededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    fcrededge_values <- fcrededge_csv[,"OESF_Ceda0"] 
  } else {
    fcrededge_values <- fcrededge_csv[,"ONF_Cedar0"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "fcrededge_band1"] <- fcrededge_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    fcrededge_values <- fcrededge_csv[,"OESF_Ceda1"] 
  } else {
    fcrededge_values <- fcrededge_csv[,"ONF_Cedar1"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "fcrededge_band2"] <- fcrededge_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    fcrededge_values <- fcrededge_csv[,"OESF_Ceda2"] 
  } else {
    fcrededge_values <- fcrededge_csv[,"ONF_Cedar2"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "fcrededge_band3"] <- fcrededge_values
}
all_trees[, "fcrededge_band1"]
all_trees[, "fcrededge_band2"]
all_trees[, "fcrededge_band3"]


## msr
all_trees[ , 'msr'] = NA
msr_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/msr", full.names = TRUE)
for (i in 1:length(msr_folders)) {
  msr_csv <- read.csv(msr_folders[i], header = TRUE)
  plot_num <- msr_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    msr_values <- msr_csv[,"OESF_Cedar"] 
  } else {
    msr_values <- msr_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "msr"] <- msr_values
}
all_trees[, "msr"]

## msrrededge
all_trees[ , 'msrrededge'] = NA
msrrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/msrrededge", full.names = TRUE)
for (i in 1:length(msrrededge_folders)) {
  msrrededge_csv <- read.csv(msrrededge_folders[i], header = TRUE)
  plot_num <- msrrededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    msrrededge_values <- msrrededge_csv[,"OESF_Cedar"] 
  } else {
    msrrededge_values <- msrrededge_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "msrrededge"] <- msrrededge_values
}
all_trees[, "msrrededge"]

## msrredrededge
all_trees[ , 'msrredrededge'] = NA
msrredrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/msrredrededge", full.names = TRUE)
for (i in 1:length(msrredrededge_folders)) {
  msrredrededge_csv <- read.csv(msrredrededge_folders[i], header = TRUE)
  plot_num <- msrredrededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    msrredrededge_values <- msrredrededge_csv[,"OESF_Cedar"] 
  } else {
    msrredrededge_values <- msrredrededge_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "msrredrededge"] <- msrredrededge_values
}
all_trees[, "msrredrededge"]


## nir_re_g
all_trees[ , 'nir_re_g_band1'] = NA
all_trees[ , 'nir_re_g_band2'] = NA
all_trees[ , 'nir_re_g_band3'] = NA
nir_re_g_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/nir_re_g", full.names = TRUE)
for (i in 1:length(nir_re_g_folders)) {
  nir_re_g_csv <- read.csv(nir_re_g_folders[i], header = TRUE)
  plot_num <- nir_re_g_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    nir_re_g_values <- nir_re_g_csv[,"OESF_Ceda0"] 
  } else {
    nir_re_g_values <- nir_re_g_csv[,"ONF_Cedar0"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "nir_re_g_band1"] <- nir_re_g_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    nir_re_g_values <- nir_re_g_csv[,"OESF_Ceda1"] 
  } else {
    nir_re_g_values <- nir_re_g_csv[,"ONF_Cedar1"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "nir_re_g_band2"] <- nir_re_g_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    nir_re_g_values <- nir_re_g_csv[,"OESF_Ceda2"] 
  } else {
    nir_re_g_values <- nir_re_g_csv[,"ONF_Cedar2"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "nir_re_g_band3"] <- nir_re_g_values
}
all_trees[, "nir_re_g_band1"]
all_trees[, "nir_re_g_band2"]
all_trees[, "nir_re_g_band3"]

## nvdinir
all_trees[ , 'nvdinir'] = NA
nvdinir_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/nvdinir", full.names = TRUE)
for (i in 1:length(nvdinir_folders)) {
  nvdinir_csv <- read.csv(nvdinir_folders[i], header = TRUE)
  plot_num <- nvdinir_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    nvdinir_values <- nvdinir_csv[,"OESF_Cedar"] 
  } else {
    nvdinir_values <- nvdinir_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "nvdinir"] <- nvdinir_values
}
all_trees[, "nvdinir"]

## nvdirededge
all_trees[ , 'nvdirededge'] = NA
nvdirededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/nvdirededge", full.names = TRUE)
for (i in 1:length(nvdirededge_folders)) {
  nvdirededge_csv <- read.csv(nvdirededge_folders[i], header = TRUE)
  plot_num <- nvdirededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    nvdirededge_values <- nvdirededge_csv[,"OESF_Cedar"] 
  } else {
    nvdirededge_values <- nvdirededge_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "nvdirededge"] <- nvdirededge_values
}
all_trees[, "nvdirededge"]

## nvdiredrededge
all_trees[ , 'nvdiredrededge'] = NA
nvdiredrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/nvdiredrededge", full.names = TRUE)
for (i in 1:length(nvdiredrededge_folders)) {
  nvdiredrededge_csv <- read.csv(nvdiredrededge_folders[i], header = TRUE)
  plot_num <- nvdiredrededge_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    nvdiredrededge_values <- nvdiredrededge_csv[,"OESF_Cedar"] 
  } else {
    nvdiredrededge_values <- nvdiredrededge_csv[,"ONF_Cedar_"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "nvdiredrededge"] <- nvdiredrededge_values
}
all_trees[, "nvdiredrededge"]

## rgb
all_trees[ , 'rgb_band1'] = NA
all_trees[ , 'rgb_band2'] = NA
all_trees[ , 'rgb_band3'] = NA
rgb_folders <- list.files("~/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/rgb", full.names = TRUE)
for (i in 1:length(rgb_folders)) {
  rgb_csv <- read.csv(rgb_folders[i], header = TRUE)
  plot_num <- rgb_csv[1,"Plot_Num"]
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    rgb_values <- rgb_csv[,"OESF_Ceda0"] 
  } else {
    rgb_values <- rgb_csv[,"ONF_Cedar0"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "rgb_band1"] <- rgb_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    rgb_values <- rgb_csv[,"OESF_Ceda1"] 
  } else {
    rgb_values <- rgb_csv[,"ONF_Cedar1"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "rgb_band2"] <- rgb_values
  
  if(plot_num == 5 | plot_num == 6 | plot_num == 7 | plot_num == 8 |plot_num == 9 | plot_num == 10 | plot_num == 14) {
    rgb_values <- rgb_csv[,"OESF_Ceda2"] 
  } else {
    rgb_values <- rgb_csv[,"ONF_Cedar2"]
  }
  all_trees[all_trees$Plot_Num == plot_num, "rgb_band3"] <- rgb_values
}
all_trees[, "rgb_band1"]
all_trees[, "rgb_band2"]
all_trees[, "rgb_band3"]


# write.csv(all_trees, file = "data/MS_Data/all_trees_w_MS_values.csv")


#### Cleaning data ####
all_trees <- read.csv("data/MS_data/all_trees_w_MS_values.csv")
head(all_trees)


# going to remove trees 6_37_THPL and 6_38_THPL b/c failed to get any metrics on them (37 is outside of extent and 38 is right on edge and deeply shadowed)
all_trees <- all_trees[-which(all_trees$Tree.ID == "6_37_THPL"), ]
all_trees <- all_trees[-which(all_trees$Tree.ID == "6_38_THPL"), ]

# Plots 15-16 failed to process for the ciredrededge (file was accidentally in wrong location (two images flipped locations)--> resulted in zeros b/c was pulling from wrong image --> no
#  coverage there) --> going to re-run that data
  ciredrededge_15_16 <- rast("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cirededge/ONF_Cedar_Plot_15_16_ciredrededge_adjusted.tif")
  
    X <- all_trees[all_trees$Plot_Num == 15 | all_trees$Plot_Num == 16, "X"]
    Y <- all_trees[all_trees$Plot_Num == 15 | all_trees$Plot_Num == 16, "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == 15 | all_trees$Plot_Num == 16, ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == 15 | all_trees$Plot_Num == 16, "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_ciredrededge <- terra::extract(x = ciredrededge_15_16, 
                                              y = XY, 
                                              fun = mean,
                                              method = "simple",
                                              bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  #  writeVector(extraction_ciredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Shapefiles_with_values_extracted/Plots_15_16_redo_ciredrededge"), overwrite = TRUE)
    
    plot(extraction_ciredrededge)


# Plots 17-22 failed to process for the cirededge (file was accidentally in wrong location --> resulted in zeros b/c was pulling from wrong image --> no
      #  coverage there)--> going to re-run that data
    
    cirededge_17_22 <- rast("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/ciredrededge/ONF_Cedar_Plot_17_thru_22_cirededge_adjusted.tif")
    
    X <- all_trees[all_trees$Plot_Num == 17 | all_trees$Plot_Num == 18 | all_trees$Plot_Num == 19 | all_trees$Plot_Num == 20 | all_trees$Plot_Num == 21 | all_trees$Plot_Num == 22, "X"]
    Y <- all_trees[all_trees$Plot_Num == 17 | all_trees$Plot_Num == 18 | all_trees$Plot_Num == 19 | all_trees$Plot_Num == 20 | all_trees$Plot_Num == 21 | all_trees$Plot_Num == 22, "Y"]
    XY <- cbind(X, Y)
    XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = all_trees[all_trees$Plot_Num == 17 | all_trees$Plot_Num == 18 | all_trees$Plot_Num == 19 | all_trees$Plot_Num == 20 | all_trees$Plot_Num == 21 | all_trees$Plot_Num == 22, ])
    XY <- buffer(x = XY, width = (all_trees[all_trees$Plot_Num == 17 | all_trees$Plot_Num == 18 | all_trees$Plot_Num == 19 | all_trees$Plot_Num == 20 | all_trees$Plot_Num == 21 | all_trees$Plot_Num == 22, "Min.Crown.Diameter"]/4)) # setting buffer based on a quarter of the min crow diamter (very conservative)
    
    extraction_cirededge <- terra::extract(x = cirededge_17_22, 
                                              y = XY, 
                                              fun = mean,
                                              method = "simple",
                                              bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  #  writeVector(extraction_cirededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/Shapefiles_with_values_extracted/Plots_17_22_redo_cirededge"), overwrite = TRUE)
    
    plot(extraction_cirededge)
    head(extraction_cirededge)

# now have those re-do plots as shapefiles --> imported into ArcGIS Pro to export as .csv --> then imported those .csv's back into 
      # R to be combined with all_trees file
Plots_15_16_ciredrededge <- read.csv("/Users/Ally/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/Re-do plots/Plots_15_16__redo_ciredrededge.csv")
  head(Plots_15_16_ciredrededge)  # need to pull column ONF_Cedar0
  
Plots_17_22_cirededge <- read.csv("/Users/Ally/Western redcedar project/data/MS_Data/CSVs_MS_Data_extracted/Re-do plots/Plots_17_22_redo_cirededge.csv")
  head(Plots_17_22_cirededge) # also need to pull column ONF_Cedar0
  
all_trees[which(all_trees$Plot_Num == 15 | all_trees$Plot_Num == 16),"ciredrededge"] <- Plots_15_16_ciredrededge[which(Plots_15_16_ciredrededge$Plot_Num == 15 | Plots_15_16_ciredrededge$Plot_Num == 16),"ONF_Cedar0"]
 
all_trees[which(all_trees$Plot_Num == 17 | all_trees$Plot_Num == 18 | all_trees$Plot_Num == 19 | all_trees$Plot_Num == 20 | all_trees$Plot_Num == 21 | all_trees$Plot_Num == 22),"cirededge"] <- 
Plots_17_22_cirededge[which(Plots_17_22_cirededge$Plot_Num == 17 | Plots_17_22_cirededge$Plot_Num == 18 | Plots_17_22_cirededge$Plot_Num == 19 | Plots_17_22_cirededge$Plot_Num == 20 | Plots_17_22_cirededge$Plot_Num == 21 | Plots_17_22_cirededge$Plot_Num == 22),"ONF_Cedar0"]


# removing tree 18_21_ALRU because multiple weird things seemed to happen to that tree; honestly not sure why
all_trees <- all_trees[-which(all_trees$Tree.ID == "18_21_ALRU"), ]



# removing now unnecessary explanatory variables (X.1, X, Y, Height.to.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter, Crown.Rotation,
# R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, Status.Code)
all_trees <- subset(all_trees, select = -c(X.1, X, Y, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter, Crown.Rotation, 
                                           R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth))

# rgb, fcnir, fcrededge,  and nir_re_g are really just different combinations of bands --> for simplicity for this purpose, going to remove 
# that aspect for now (just going to use single band values and not try and combine bands). Also going to change some names to make this more clear
# rgb is red, green, blue bands --> keeping all 3 bands, just renaming
# fcnir is nir, red, green bands --> keeping only first band (nir)
# fcrededge is rededge, red, green --> keeping only first band (rededge)
# nir_re_g is nir, rededge, green --> not keeping any
all_trees <- subset(all_trees, select = -c(fcnir_band2, fcnir_band3, fcrededge_band2, fcrededge_band3, nir_re_g_band1, nir_re_g_band2, nir_re_g_band3))

colnames(all_trees)[colnames(all_trees) == "rgb_band1"] ="red"
colnames(all_trees)[colnames(all_trees) == "rgb_band2"] ="green"
colnames(all_trees)[colnames(all_trees) == "rgb_band3"] ="blue"
colnames(all_trees)[colnames(all_trees) == "fcnir_band1"] ="nir"
colnames(all_trees)[colnames(all_trees) == "fcrededge_band1"] ="rededge"


# checking for other random zeros and infinities
range(all_trees$cigreen)
all_trees$cigreen == 0

range(all_trees$nir) 
all_trees$nir == 0

range(all_trees$rededge) 
all_trees$rededge == 0

range(all_trees$msr) 
all_trees$msr == 0 ## HAS SOME TRUES --> going to drop those trees; looked at their details and them in ArcGIS Pro and not sure why outliers
all_trees[all_trees$msr == 0, "Tree.ID"]
all_trees <- all_trees[-which(all_trees$Tree.ID == "15_123_ACCI"), ]
all_trees <- all_trees[-which(all_trees$Tree.ID == "17_113_THPL"), ]
all_trees <- all_trees[-which(all_trees$Tree.ID == "20_9_THPL"), ]
all_trees <- all_trees[-which(all_trees$Tree.ID == "20_30_TSHE"), ]


range(all_trees$msrrededge) 
all_trees$msrrededge == 0

range(all_trees$msrredrededge) 
all_trees$msrredrededge == 0

range(all_trees$red) 
all_trees$red == 0

range(all_trees$green) 
all_trees$green == 0

range(all_trees$blue) 
all_trees$blue == 0

range(all_trees$nvdinir) 
all_trees$nvdinir == 0

range(all_trees$nvdirededge) 
all_trees$nvdirededge == 0

range(all_trees$nvdiredrededge) 
all_trees$nvdiredrededge == 0


# writing off a .csv
# write.csv(all_trees, "data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")




#### Shadows ####
# it looks like (looking at the MS imagery) that shadows are impacting the shorter trees --> in particular this would impact the TSHE in ONF
  # going to remove trees under 20cm DBH for OESF and trees under 30cm DBH for OESF

all_trees <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_502.csv", header = TRUE)

all_trees[ , 'Site'] = NA

for (i in 1:length(all_trees$Tree.ID)) {
  if(all_trees$Plot_Num[i] < 15) {
    all_trees$Site[i] <- "OESF"
  } else {
    all_trees$Site[i] <- "ONF"
  }
}

all_trees <- rbind(all_trees[which(all_trees$Site == "OESF" & all_trees$DBH > 0.20),], all_trees[which(all_trees$Site == "ONF" & all_trees$DBH > 0.30),])

response_vars <- all_trees[,9:20]

cedars <- all_trees[all_trees$Species == "THPL",]


# taking a peak at data and impact of removing smaller trees on the relationship between TSHE/THPL and Site; using boxplots
library(ggplot2)

ggplot(cedars,
       aes(x = Site, y = nvdirededge)) + 
  geom_boxplot(aes(color = Site)) +
  ggtitle("Site comparison - Cedars only")

ggplot(cedars[cedars$Site == "ONF",],
       aes(x = Status.Code, y = nvdirededge)) + 
  geom_boxplot(aes(color = Status.Code)) +
  ggtitle("Dead top comparison - ONF")

ggplot(all_trees,
       aes(x = Species, y = nvdirededge)) + 
  geom_boxplot(aes(color = Species)) +
  ggtitle("Species comparison - both sites")

ggplot(all_trees[all_trees$Site == "OESF",],
       aes(x = Species, y = nvdirededge)) + 
  geom_boxplot(aes(color = Species)) +
  ggtitle("Species comparison - OESF")

ggplot(all_trees[all_trees$Site == "ONF",],
       aes(x = Species, y = nvdirededge)) + 
  geom_boxplot(aes(color = Species)) +
  ggtitle("Species comparison - ONF")

ggplot(all_trees,
       aes(x = Species, y = nvdirededge, fill = Site)) + 
  geom_boxplot() +
  ggtitle("Species and Site")


#### Extracting MS Data for trees outside of plots #####
# Going to use the same process as extraction the matched trees inside the plots, but going to
  # use the trees_normalized_basin_stats file that was a by-product of the LiDAR processing
    # (has data on individual tree location and GridMaxHt, which can use as proxy for tree height)

# trying to figure out what equation should use for buffer given GridMaxHt
    # going to use existing plot data -> figure out a general relationship between height and DBH with all species combined
        # that I can then use to use height to estimate DBH --> diameter --> crown diameter --> buffer for MS data extraction

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

plot(all_trees$Total.Height, all_trees$DBH)

# it looks the relationship is relatively linear until about 45m height --> then it becomes linear still but much steeper.
  # final decision: how two different buffers: one for the OESF sites and the other for the ONF sites. ONF sites should 
  # have the tree heights divided by a greater number.


plot_10 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plot10.csv", header = TRUE)
plot_14 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_8_9_14_pt1.csv", header = TRUE)
plots_8_9_14_pt2 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_8_9_14_pt2.csv", header = TRUE)
plots_8_9_14_pt3 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_8_9_14_pt3.csv", header = TRUE)
plots_8_9_14_pt4 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_8_9_14_pt4.csv", header = TRUE)
plots_15_16 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_15_16.csv", header = TRUE)
plot_17_thru_22 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_17_thru_22.csv", header = TRUE)
plots_23_24_25 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_23_24_25.csv", header = TRUE)
plots_26_27 <- read.csv("data/LiDARTreePointsforMSExtraction/CSVs/trees_normalized_Basin_Stats_Plots_26_27.csv", header = TRUE)

# combing the 8_9 parts in to one file
plots_8_9 <- rbind(plots_8_9_14_pt2, plots_8_9_14_pt3, plots_8_9_14_pt4)


## The following is most certainly not the best way to do things, but it is what I did.
# Goal: for each plot and for each image, create a shapefile --> can import that shapefile into ArcGIS Pro to double check it, and to convert to .csv to re-combine all of the plots together

cigreen_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cigreen", full.names = TRUE)
cirededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/cirededge", full.names = TRUE)
ciredrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/ciredrededge", full.names = TRUE) 
fcnir_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/fcnir", full.names = TRUE)
fcrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/fcrededge", full.names = TRUE)
msr_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msr", full.names = TRUE)
msrrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msrrededge", full.names = TRUE)
msrredrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/msrredrededge", full.names = TRUE)
nir_re_g_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nir_re_g", full.names = TRUE)
nvdinir_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdinir", full.names = TRUE)
nvdirededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdirededge", full.names = TRUE)
nvdiredrededge_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/nvdiredrededge", full.names = TRUE)
rgb_filelist <- list.files("~/Western redcedar project/data/MS_Data/Post-processed images (adjusted)/rgb", full.names = TRUE)

# Going to process each image type at a time.
library(terra)

# adding buffer number to each tree in each file
plots_8_9[ , 'buffer'] = NA

for (i in 1:length(plots_8_9$BasinID)) {
  if(plots_8_9$GridMaxHt[i] < 45) {
    plots_8_9$buffer[i] <- plots_8_9$GridMaxHt[i] / 30
  } else {
    plots_8_9$buffer[i] <- plots_8_9$GridMaxHt[i] / 50
  }
}

plot_10[ , 'buffer'] = NA

for (i in 1:length(plot_10$BasinID)) {
  if(plot_10$GridMaxHt[i] < 45) {
    plot_10$buffer[i] <- plot_10$GridMaxHt[i] / 30
  } else {
    plot_10$buffer[i] <- plot_10$GridMaxHt[i] / 50
  }
}

plot_14[ , 'buffer'] = NA

for (i in 1:length(plot_14$BasinID)) {
  if(plot_14$GridMaxHt[i] < 45) {
    plot_14$buffer[i] <- plot_14$GridMaxHt[i] / 30
  } else {
    plot_14$buffer[i] <- plot_14$GridMaxHt[i] / 50
  }
}

plots_15_16[ , 'buffer'] = NA

for (i in 1:length(plots_15_16$BasinID)) {
  if(plots_15_16$GridMaxHt[i] < 45) {
    plots_15_16$buffer[i] <- plots_15_16$GridMaxHt[i] / 30
  } else {
    plots_15_16$buffer[i] <- plots_15_16$GridMaxHt[i] / 50
  }
}

plot_17_thru_22[ , 'buffer'] = NA

for (i in 1:length(plot_17_thru_22$BasinID)) {
  if(plot_17_thru_22$GridMaxHt[i] < 45) {
    plot_17_thru_22$buffer[i] <- plot_17_thru_22$GridMaxHt[i] / 30
  } else {
    plot_17_thru_22$buffer[i] <- plot_17_thru_22$GridMaxHt[i] / 50
  }
}

plots_23_24_25[ , 'buffer'] = NA

for (i in 1:length(plots_23_24_25$BasinID)) {
  if(plots_23_24_25$GridMaxHt[i] < 45) {
    plots_23_24_25$buffer[i] <- plots_23_24_25$GridMaxHt[i] / 30
  } else {
    plots_23_24_25$buffer[i] <- plots_23_24_25$GridMaxHt[i] / 50
  }
}

plots_26_27[ , 'buffer'] = NA

for (i in 1:length(plots_26_27$BasinID)) {
  if(plots_26_27$GridMaxHt[i] < 45) {
    plots_26_27$buffer[i] <- plots_26_27$GridMaxHt[i] / 30
  } else {
    plots_26_27$buffer[i] <- plots_26_27$GridMaxHt[i] / 50
  }
}

# list of cedar plots; order of list corresponds to order of files from list.files function above (IMPORTANT)
CedarPlotList <- list(plot_10, plot_14, plots_8_9, plots_15_16, plot_17_thru_22, plots_23_24_25, plots_26_27)
# list of names for file labeling
FilesCedarPlotList <- c("plot_10", "plot_14", "plots_8_9", "plots_15_16", "plot_17_thru_22", "plots_23_24_25", "plots_26_27")


# Cigreen
for (i in 1:length(cigreen_filelist)) {
  cigreen <- rast(cigreen_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
    extraction_cigreen <- terra::extract(x = cigreen, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
    
    # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
    writeVector(extraction_cigreen, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "cigreen"), overwrite = TRUE)
}

# Cirededge
for (i in 1:length(cirededge_filelist)) {
  cirededge <- rast(cirededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_cirededge <- terra::extract(x = cirededge, 
                                       y = XY, 
                                       fun = mean,
                                       method = "simple",
                                       bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_cirededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "cirededge"), overwrite = TRUE)
}

# Ciredrededge
for (i in 1:length(ciredrededge_filelist)) {
  ciredrededge <- rast(ciredrededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_ciredrededge <- terra::extract(x = ciredrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_ciredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "ciredrededge"), overwrite = TRUE)
}

# fcnir
for (i in 1:length(fcnir_filelist)) {
  fcnir <- rast(fcnir_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_fcnir <- terra::extract(x = fcnir, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_fcnir, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "fcnir"), overwrite = TRUE)
}

# fcrededge
for (i in 1:length(fcrededge_filelist)) {
  fcrededge <- rast(fcrededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_fcrededge <- terra::extract(x = fcrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_fcrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "fcrededge"), overwrite = TRUE)
}

# msr
for (i in 1:length(msr_filelist)) {
  msr <- rast(msr_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_msr <- terra::extract(x = msr, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_msr, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "msr"), overwrite = TRUE)
}

# msrrededge
for (i in 1:length(msrrededge_filelist)) {
  msrrededge <- rast(msrrededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_msrrededge <- terra::extract(x = msrrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_msrrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "msrrededge"), overwrite = TRUE)
}

# msrredrededge
for (i in 1:length(msrredrededge_filelist)) {
  msrredrededge <- rast(msrredrededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_msrredrededge <- terra::extract(x = msrredrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_msrredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "msrredrededge"), overwrite = TRUE)
}

# nir_re_g
for (i in 1:length(nir_re_g_filelist)) {
  nir_re_g <- rast(nir_re_g_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_nir_re_g <- terra::extract(x = nir_re_g, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_nir_re_g, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "nir_re_g"), overwrite = TRUE)
}

# nvdinir
for (i in 1:length(nvdinir_filelist)) {
  nvdinir <- rast(nvdinir_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_nvdinir <- terra::extract(x = nvdinir, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_nvdinir, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "nvdinir"), overwrite = TRUE)
}

# nvdirededge
for (i in 1:length(nvdirededge_filelist)) {
  nvdirededge <- rast(nvdirededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_nvdirededge <- terra::extract(x = nvdirededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_nvdirededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "nvdirededge"), overwrite = TRUE)
}

# nvdiredrededge
for (i in 1:length(nvdiredrededge_filelist)) {
  nvdiredrededge <- rast(nvdiredrededge_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_nvdiredrededge <- terra::extract(x = nvdiredrededge, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_nvdiredrededge, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "nvdiredrededge"), overwrite = TRUE)
}

# rgb
for (i in 1:length(rgb_filelist)) {
  rgb <- rast(rgb_filelist[i])
  
  X <- CedarPlotList[[i]][2]
  Y <- CedarPlotList[[i]][3]
  XY <- cbind(X, Y)
  XY <- as.matrix(XY)
  XY <- vect(x = XY, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", atts = data.frame(CedarPlotList[[i]]))
  buffer <- CedarPlotList[[i]][8]
  buffer <- as.matrix(buffer)
  XY <- buffer(x = XY, width = buffer)
  
  extraction_rgb <- terra::extract(x = rgb, 
                                         y = XY, 
                                         fun = mean,
                                         method = "simple",
                                         bind = TRUE)
  
  # writting the results to a shapefile (can then visualize it and export to .csv in ArcGIS Pro)
  writeVector(extraction_rgb, paste0("/Users/Ally/Western redcedar project/data/MS_Data/OutsidePlotTrees_Shapefiles_wValuesExtracted/", FilesCedarPlotList[i], "_", "rgb"), overwrite = TRUE)
}

# Between last step and this one, imported all of the shapefiles into ArcGIS Pro --> used ArcGIS Pro to 
# export the attribute table of each shapefile as a .csv file (couldn't figure out how to do that in R)

# Currently, all .csv's are organized in files based on image (ie. cigreen, etc.)

# making a list of all of the file names for cigreen (one for each plot)
cigreen_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/cigreen", full.names = TRUE)


# kicking things off
all_trees_cigreen <- read.csv(cigreen_folders[1], header = TRUE)
colnames(all_trees_cigreen)[10] <- "cigreen"

for (i in 2:length(cigreen_folders)) {
  cigreen <- read.csv(cigreen_folders[i], header = TRUE)
  colnames(cigreen)[10] <- "cigreen"
  all_trees_cigreen <- rbind(all_trees_cigreen, cigreen)
} 




## cirededge


cirededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/cirededge", full.names = TRUE)

all_trees_cirededge <- read.csv(cirededge_folders[1], header = TRUE)
colnames(all_trees_cirededge)[10] <- "cirededge"

for (i in 2:length(cirededge_folders)) {
  cirededge <- read.csv(cigreen_folders[i], header = TRUE)
  colnames(cirededge)[10] <- "cirededge"
  all_trees_cirededge <- rbind(all_trees_cirededge, cirededge)
} 


## ciredrededge

ciredrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/ciredrededge", full.names = TRUE)

all_trees_ciredrededge <- read.csv(ciredrededge_folders[1], header = TRUE)
colnames(all_trees_ciredrededge)[10] <- "ciredrededge"

for (i in 2:length(ciredrededge_folders)) {
  ciredrededge <- read.csv(ciredrededge_folders[i], header = TRUE)
  colnames(ciredrededge)[10] <- "ciredrededge"
  all_trees_ciredrededge <- rbind(all_trees_ciredrededge, ciredrededge)
} 


## fcnir

fcnir_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/fcnir", full.names = TRUE)

all_trees_fcnir<- read.csv(fcnir_folders[1], header = TRUE)
colnames(all_trees_fcnir)[10] <- "fcnir_band1"
colnames(all_trees_fcnir)[11] <- "fcnir_band2"
colnames(all_trees_fcnir)[12] <- "fcnir_band3"

for (i in 2:length(fcnir_folders)) {
  fcnir <- read.csv(fcnir_folders[i], header = TRUE)
  colnames(fcnir)[10] <- "fcnir_band1"
  colnames(fcnir)[11] <- "fcnir_band2"
  colnames(fcnir)[12] <- "fcnir_band3"
  all_trees_fcnir <- rbind(all_trees_fcnir, fcnir)
} 



## fcrededge

fcrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/fcrededge", full.names = TRUE)


all_trees_fcrededge<- read.csv(fcrededge_folders[1], header = TRUE)
colnames(all_trees_fcrededge)[10] <- "fcrededge_band1"
colnames(all_trees_fcrededge)[11] <- "fcrededge_band2"
colnames(all_trees_fcrededge)[12] <- "fcrededge_band3"

for (i in 2:length(fcrededge_folders)) {
  fcrededge <- read.csv(fcrededge_folders[i], header = TRUE)
  colnames(fcrededge)[10] <- "fcrededge_band1"
  colnames(fcrededge)[11] <- "fcrededge_band2"
  colnames(fcrededge)[12] <- "fcrededge_band3"
  all_trees_fcrededge <- rbind(all_trees_fcrededge, fcrededge)
} 


## msr

msr_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/msr", full.names = TRUE)

all_trees_msr <- read.csv(msr_folders[1], header = TRUE)
colnames(all_trees_msr)[10] <- "msr"


for (i in 2:length(msr_folders)) {
  msr <- read.csv(msr_folders[i], header = TRUE)
  colnames(msr)[10] <- "msr"
  all_trees_msr <- rbind(all_trees_msr, msr)
} 

## msrrededge

msrrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/msrrededge", full.names = TRUE)

all_trees_msrrededge <- read.csv(msrrededge_folders[1], header = TRUE)
colnames(all_trees_msrrededge)[10] <- "msrrededge"

for (i in 2:length(msrrededge_folders)) {
  msrrededge <- read.csv(msrrededge_folders[i], header = TRUE)
  colnames(msrrededge)[10] <- "msrrededge"
  all_trees_msrrededge <- rbind(all_trees_msrrededge, msrrededge)
} 

## msrredrededge
msrredrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/msrredrededge", full.names = TRUE)

all_trees_msrredrededge <- read.csv(msrredrededge_folders[1], header = TRUE)
colnames(all_trees_msrredrededge)[10] <- "msrredrededge"

for (i in 2:length(msrredrededge_folders)) {
  msrredrededge <- read.csv(msrredrededge_folders[i], header = TRUE)
  colnames(msrredrededge)[10] <- "msrredrededge"
  all_trees_msrredrededge <- rbind(all_trees_msrredrededge, msrredrededge)
} 


## nir_re_g

nir_re_g_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/nir_re_g", full.names = TRUE)

all_trees_nir_re_g <- read.csv(nir_re_g_folders[1], header = TRUE)
colnames(all_trees_nir_re_g)[10] <- "nir_re_g_band1"
colnames(all_trees_nir_re_g)[11] <- "nir_re_g_band2"
colnames(all_trees_nir_re_g)[12] <- "nir_re_g_band3"

for (i in 2:length(nir_re_g_folders)) {
  nir_re_g <- read.csv(nir_re_g_folders[i], header = TRUE)
  colnames(nir_re_g)[10] <- "nir_re_g_band1"
  colnames(nir_re_g)[11] <- "nir_re_g_band2"
  colnames(nir_re_g)[12] <- "nir_re_g_band3"
  all_trees_nir_re_g <- rbind(all_trees_nir_re_g, nir_re_g)
} 

## nvdinir

nvdinir_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/nvdinir", full.names = TRUE)

all_trees_nvdinir <- read.csv(nvdinir_folders[1], header = TRUE)
colnames(all_trees_nvdinir)[10] <- "nvdinir"

for (i in 2:length(nvdinir_folders)) {
  nvdinir <- read.csv(nvdinir_folders[i], header = TRUE)
  colnames(nvdinir)[10] <- "nvdinir"
  all_trees_nvdinir <- rbind(all_trees_nvdinir, nvdinir)
} 

## nvdirededge
nvdirededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/nvdirededge", full.names = TRUE)

all_trees_nvdirededge <- read.csv(nvdirededge_folders[1], header = TRUE)
colnames(all_trees_nvdirededge)[10] <- "nvdirededge"

for (i in 2:length(nvdirededge_folders)) {
  nvdirededge <- read.csv(nvdirededge_folders[i], header = TRUE)
  colnames(nvdirededge)[10] <- "nvdirededge"
  all_trees_nvdirededge <- rbind(all_trees_nvdirededge, nvdirededge)
} 

## nvdiredrededge
nvdiredrededge_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/nvdiredrededge", full.names = TRUE)

all_trees_nvdiredrededge <- read.csv(nvdiredrededge_folders[1], header = TRUE)
colnames(all_trees_nvdiredrededge)[10] <- "nvdiredrededge"

for (i in 2:length(nvdiredrededge_folders)) {
  nvdiredrededge <- read.csv(nvdiredrededge_folders[i], header = TRUE)
  colnames(nvdiredrededge)[10] <- "nvdiredrededge"
  all_trees_nvdiredrededge <- rbind(all_trees_nvdiredrededge, nvdiredrededge)
} 

## rgb
rgb_folders <- list.files("~/Western redcedar project/data/MS_Data/OutsidePlotTrees_CSVs_wValuesExtracted/rgb", full.names = TRUE)

all_trees_rgb <- read.csv(rgb_folders[1], header = TRUE)
colnames(all_trees_rgb)[10] <- "rgb_band1"
colnames(all_trees_rgb)[11] <- "rgb_band2"
colnames(all_trees_rgb)[12] <- "rgb_band3"

for (i in 2:length(rgb_folders)) {
  rgb <- read.csv(rgb_folders[i], header = TRUE)
  colnames(rgb)[10] <- "rgb_band1"
  colnames(rgb)[11] <- "rgb_band2"
  colnames(rgb)[12] <- "rgb_band3"
  all_trees_rgb <- rbind(all_trees_rgb, rgb)
} 


# combing them all together
# couldn't get merge to work (think it's a weird formatting issue) --> combing them all in excel and then re-importing them
# write.csv(all_trees_cigreen, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_cigreen.csv")
# write.csv(all_trees_cirededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_cirededge.csv")
# write.csv(all_trees_ciredrededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_ciredrededge.csv")
# write.csv(all_trees_fcnir, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_fcnir.csv")
# write.csv(all_trees_fcrededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_fcrededge.csv")
# write.csv(all_trees_msr, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_msr.csv")
# write.csv(all_trees_msrrededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_msrrededge.csv")
# write.csv(all_trees_msrredrededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_msrredrededge.csv")
# write.csv(all_trees_nir_re_g, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_nir_re_g.csv")
# write.csv(all_trees_nvdinir, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_nvdinir.csv")
# write.csv(all_trees_nvdirededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_nvdirededge.csv")
# write.csv(all_trees_nvdiredrededge, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_nvdiredrededge.csv")
# write.csv(all_trees_rgb, file = "data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees_rgb.csv")

all_trees <- read.csv("data/MS_data/OutsidePlotTrees_CSVs_wValuesExtracted/all_trees.csv", header = TRUE)


## Cleaning data


# rgb, fcnir, fcrededge,  and nir_re_g are really just different combinations of bands --> for simplicity for this purpose, going to remove 
# that aspect for now (just going to use single band values and not try and combine bands). Also going to change some names to make this more clear
# rgb is red, green, blue bands --> keeping all 3 bands, just renaming
# fcnir is nir, red, green bands --> keeping only first band (nir)
# fcrededge is rededge, red, green --> keeping only first band (rededge)
# nir_re_g is nir, rededge, green --> not keeping any
all_trees <- subset(all_trees, select = -c(fcnir_band2, fcnir_band3, fcrededge_band2, fcrededge_band3, nir_re_g_band1, nir_re_g_band2, nir_re_g_band3))

colnames(all_trees)[colnames(all_trees) == "rgb_band1"] ="red"
colnames(all_trees)[colnames(all_trees) == "rgb_band2"] ="green"
colnames(all_trees)[colnames(all_trees) == "rgb_band3"] ="blue"
colnames(all_trees)[colnames(all_trees) == "fcnir_band1"] ="nir"
colnames(all_trees)[colnames(all_trees) == "fcrededge_band1"] ="rededge"


# checking for other random zeros and infinities and removing them --> happened with some amount of frequency b/c the lidar 
  # coverage was typically bigger than the MS coverage
range(all_trees$cigreen)
all_trees$cigreen == 0
all_trees <- all_trees[-which(all_trees$cigreen == 0),]

range(all_trees$nir) 
which(all_trees$nir == 0)

range(all_trees$rededge) 
which(all_trees$rededge == 0)

range(all_trees$msr) 
which(all_trees$msr == 0)



range(all_trees$msrrededge) 
which(all_trees$msrrededge == 0)

range(all_trees$msrredrededge) 
which(all_trees$msrredrededge == 0)

range(all_trees$red) 
which(all_trees$red == 0)

range(all_trees$green) 
which(all_trees$green == 0)

range(all_trees$blue) 
which(all_trees$blue == 0)

range(all_trees$nvdinir) 
which(all_trees$nvdinir == 0)

range(all_trees$nvdirededge) 
which(all_trees$nvdirededge == 0)

range(all_trees$nvdiredrededge) 
which(all_trees$nvdiredrededge == 0)


# writing off a .csv
# write.csv(all_trees, "data/MS_Data/all_outside_plot_trees_w_MS_values_cleaned_for_thesis.csv")

































