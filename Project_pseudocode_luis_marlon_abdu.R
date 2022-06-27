###
# Niche modeling most of the steps in R
###

# THINGS TO DO

# Preprocessing
## Occurrence data cleaning

setwd("C:/D_marginatus_D_reticulatus")
# Download data from GBIf

if(!require(dismo)){
  install.packages("dismo")
  library(dismo)
}

if(!require(raster)){
  install.packages("raster")
  library(raster)
}

if(!require(sp)){
  install.packages("sp")
  library(sp)
}

if(!require(maptools)){
  install.packages("maptools")
  library(maptools)
}

if(!require(maps)){
  install.packages("maps")
  library(maps)
}

if(!require(rgeos)){
  install.packages("rgeos")
  library(rgeos)
}

if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}

if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

if(!require(hsi)){
  devtools::install_github("luismurao/hsi")
  library(hsi)
}
gpclibpermit()

# with gbif package

Dv=gbif("Dermacentor marginatus", geo = TRUE)
write.csv(Dv, "data.csv")

# clean data
Dv2 = cbind(Dv$lon,Dv$lat)

# remove replicates
Dv3 = unique(Dv2)

# remove NAS

De_va = na.omit(Dv3)

write.csv(De_va, "D.marginatus_cleaned.csv")

# load data combined

setwd("C:/D_marginatus_D_reticulatus/cleaned_data")

DM <- read.csv("D_marginatus_lastcleaned.csv")
DR <- read.csv("D_reticulatus_lastcleaned.csv")

## plot data on map

map("world")
axis(side = 1)
axis(side = 2)

points(DM[, 2:3])  # keep points between lon -9 and 45


map("world")
axis(side = 1)
axis(side = 2)

points(DR[, 2:3]) # keep points between lon -5 and 40, lat 5 and 60

## correnctions

DM1 <- DM[DM$log < 45 & DM$log > -9, ]
DR1 <- DR[DR$lon < 40 & DR$lon > -5 & DR$lat < 60 & DR$lat > 5, ]

## Write data

write.csv(DM1,"DM1.csv", row.names = FALSE)
write.csv(DR1,"DR1.csv", row.names = FALSE)

## Thinning occurrence data
# Package needed
if(!require(spThin)){
  install.packages("spThin")
  library(spThin)
}


# directory 
setwd("C:/D_marginatus_D_reticulatus")

# simple example

## Using DM1 & DR1

# cat<-read.csv("DM1.csv")

## thinning records
help(thin) # function's help

thinnedDM1 <- thin( loc.data = DM1, lat.col = "lat", long.col = "log", 
                      spec.col = "species", thin.par = 50, reps = 50, 
                      locs.thinned.list.return = TRUE, write.files = TRUE, 
                      max.files = 1, out.dir = "C:/D_marginatus_D_reticulatus/thinning_data50km", 
                      out.base = "DM1_thined", write.log.file = TRUE,
                      log.file = "DM1_thined_full_log_file.txt" )

thinnedDR1 <- thin( loc.data = DR1, lat.col = "lat", long.col = "lon", 
                    spec.col = "species", thin.par = 50, reps = 50, 
                    locs.thinned.list.return = TRUE, write.files = TRUE, 
                    max.files = 1, out.dir = "C:/D_marginatus_D_reticulatus/thinning_data50km", 
                    out.base = "DR1_thined", write.log.file = TRUE,
                    log.file = "DR1_thined_full_log_file.txt" )

#####
# example considering differences in environmental heterogeneity
# in area of records distribution

## categories of heterogeneity need to be addigned previously

## reading data
tal<-read.csv ("tal.csv")

## thinning records
help(thin)  # function's help

## class 1 of heterogeity distance of thinning 20 km

## thinning records
thinnedtal_a <- thin( loc.data = tal[ which(tal$Territorio == 1) , ], lat.col = "Latitude", 
                      long.col = "Longitude", spec.col = "Species", thin.par = 20, reps = 50, 
                      locs.thinned.list.return = TRUE, write.files = TRUE, max.files = 5, 
                      out.dir = "D:/R/Peltophryne_cu/1a_spThin/TAL1", out.base = "tal_a_thined", 
                      write.log.file = TRUE, log.file = "tala_thined_full_log_file.txt" )

## class 2 of heterogeity distance of thinning 10 km

## thinning records
thinnedtal_b <- thin( loc.data = tal[ which(tal$Territorio == 2) , ], lat.col = "Latitude", 
                      long.col = "Longitude", spec.col = "Species", thin.par = 10, reps = 50, 
                      locs.thinned.list.return = TRUE, write.files = TRUE, max.files = 5, 
                      out.dir = "D:/R/Peltophryne_cu/1a_spThin/TAL1", out.base = "tal_b_thined", 
                      write.log.file = TRUE, log.file = "talb_thined_full_log_file.txt" )


## Design the M

### create buffers of 200 km

#### convert points to spatial points

Dm <- read.csv("thinning_data50km/DM1_thined_thin1.csv")
WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
occ_sp <- sp::SpatialPointsDataFrame(coords = Dm[, 2:3], data = Dm,
                                     proj4string = WGS84)

plot(occ_sp)
map("world", add = T)

## second species ##

Dr <- read.csv("thinning_data50km/DR1_thined_thin1.csv")
WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
occ_sp2 <- sp::SpatialPointsDataFrame(coords = Dr[, 2:3], data = Dr,
                                     proj4string = WGS84)

plot(occ_sp2)
map("world", add = T)

#### project from log and lat to projections which keeps area correct.

centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                      " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

occ_pr <- sp::spTransform(occ_sp, AEQD)

## second species ##
centroid <- rgeos::gCentroid(occ_sp2, byid = FALSE)

AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                      " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

occ_pr2 <- sp::spTransform(occ_sp2, AEQD)

#### creat a buffer

buff_area <- rgeos::gBuffer(occ_pr, width = 200000) ## 200000 meters buffer distance

buff_area1 <- raster::disaggregate(buff_area)

## second species ##

buff_area <- rgeos::gBuffer(occ_pr2, width = 200000) ## 200000 meters buffer distance

buff_area2 <- raster::disaggregate(buff_area)


occ_prdm <- sp::spTransform(buff_area1 , WGS84)
occ_prdr <- sp::spTransform(buff_area2, WGS84)

plot(occ_prdr)
plot(occ_prdm)

#### save shapefile

## Create file
dir.create("M_area")

occ_prdm <- SpatialPolygonsDataFrame(occ_prdm, data = data.frame(c(200, 200)), match.ID = F)

occ_prdr <- SpatialPolygonsDataFrame(occ_prdr, data = data.frame(c(200, 200)), match.ID = F)

rgdal::writeOGR(occ_prdm, ".", "M_area/M_Dm", driver = "ESRI Shapefile")

rgdal::writeOGR(occ_prdr, ".", "M_area/M_Dr", driver = "ESRI Shapefile")



## Download variables
WC_data <- getData('worldclim', var='bio', res=10)
plot(WC_data[[1]])


## Mask variables ## first we use crop then we mask the area ##
maskDm <- mask(crop(WC_data, occ_prdm), occ_prdm)
maskDr <- mask(crop(WC_data, occ_prdr), occ_prdr)

maskDm <- maskDm[[c(-8, -9, -18, -19)]]
maskDr <- maskDr[[c(-8, -9, -18, -19)]]

## save data
dir.create("Dm_ENM")
dir.create("Dm_ENM/Temp_variablesM")
dir.create("Dm_ENM/Prec_variablesM")
dir.create("Dr_ENM")
dir.create("Dr_ENM/Temp_variablesM")
dir.create("Dr_ENM/Prec_variablesM")

namedm <- c(paste("Dm_ENM/Temp_variablesM", paste0(gsub("bio", "bio_", names(maskDm[[1:9]])), ".asc"), sep = "/"), 
            paste("Dm_ENM/Prec_variablesM", paste0(gsub("bio", "bio_", names(maskDm[[10:15]])), ".asc"), sep = "/"))

namedr <- c(paste("Dr_ENM/Temp_variablesM", paste0(gsub("bio", "bio_", names(maskDr[[1:9]])), ".asc"), sep = "/"),
            paste("Dr_ENM/Prec_variablesM", paste0(gsub("bio", "bio_", names(maskDr[[10:15]])), ".asc"), sep = "/"))

for (i in 1:length(namedm)) {
  writeRaster(maskDm[[i]], namedm[i], format = "ascii")
  writeRaster(maskDr[[i]], namedr[i], format = "ascii")
}


## Select variables (PCA)

source("https://raw.githubusercontent.com/marlonecobos/ENM_manuals/master/Variables_processing/kuenm_rpca.R")

var_folder <- "Dm_ENM/Prec_variablesM" # name of folder with variables to be combined in distinct sets
proj_folder <- "Dm_ENM/Prec_f" # name of the folder containing one or more folders with variables for other scenarios
out_folder <- "Dm_ENM/PCAprec_results" # name of folder that will contain the sets 
in_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil 
out_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil
n_pcs <- 3 # number of pcs you want as rasters, if not defined all pcs are returned as rasters

kuenm_rpca(vars.folder = var_folder, in.format = "ascii", out.format = "ascii", project = TRUE, 
           proj.vars = proj_folder, n.pcs = n_pcs, out.dir = out_folder)

#####

var_folder <- "Dm_ENM/Temp_variablesM" # name of folder with variables to be combined in distinct sets
proj_folder <- "Dm_ENM/Temp_f" # name of the folder containing one or more folders with variables for other scenarios
out_folder <- "Dm_ENM/PCATemp_results" # name of folder that will contain the sets 
in_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil 
out_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil
n_pcs <- 3 # number of pcs you want as rasters, if not defined all pcs are returned as rasters

kuenm_rpca(vars.folder = var_folder, in.format = "ascii", out.format = "ascii", project = TRUE, 
           proj.vars = proj_folder, n.pcs = n_pcs, out.dir = out_folder)

####### second species

var_folder <- "Dr_ENM/Prec_variablesM" # name of folder with variables to be combined in distinct sets
proj_folder <- "Dr_ENM/Prec_f" # name of the folder containing one or more folders with variables for other scenarios
out_folder <- "Dr_ENM/PCAprec_results" # name of folder that will contain the sets 
in_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil 
out_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil
n_pcs <- 3 # number of pcs you want as rasters, if not defined all pcs are returned as rasters

kuenm_rpca(vars.folder = var_folder, in.format = "ascii", out.format = "ascii", project = TRUE, 
           proj.vars = proj_folder, n.pcs = n_pcs, out.dir = out_folder)

##### 

var_folder <- "Dr_ENM/Temp_variablesM" # name of folder with variables to be combined in distinct sets
proj_folder <- "Dr_ENM/Temp_f" # name of the folder containing one or more folders with variables for other scenarios
out_folder <- "Dr_ENM/PCATemp_results" # name of folder that will contain the sets 
in_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil 
out_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil
n_pcs <- 3 # number of pcs you want as rasters, if not defined all pcs are returned as rasters

kuenm_rpca(vars.folder = var_folder, in.format = "ascii", out.format = "ascii", project = TRUE, 
           proj.vars = proj_folder, n.pcs = n_pcs, out.dir = out_folder)


# Ecological niche modeling

## Prepare the data

### M variables
dir.create("Dm_ENM/M_variables")

#### manually done

## second species ## 

dir.create("Dr_ENM/M_variables")

#### manually done


## occurrences out of rasters
MlayerDm <- raster("Dm_ENM/M_variables/set1/pc_p1.asc")
MlayerDr <- raster("Dr_ENM/M_variables/set1/pc_p1.asc")

plot(MlayerDm)
points(Dm[, 2:3])

plot(MlayerDr)
points(Dr[, 2:3])

Dmcheck <- data.frame(Dm, value = extract(MlayerDm, Dm[, 2:3]))
Drcheck <- data.frame(Dr, value = extract(MlayerDr, Dr[, 2:3]))

Dmout <- Dmcheck[is.na(Dmcheck$value), ]
Drout <- Drcheck[is.na(Drcheck$value), ]

plot(MlayerDm)
points(Dmout[, 2:3])

Dmcheck <- Dmcheck[!is.na(Dmcheck$value), ]

Drout
plot(MlayerDr, xlim = c(-5, -4), ylim = c(51, 52))
points(Drout[, 2:3])
points(x = Drout[, 2], y = 51.1, col = "blue")

Drcheck[is.na(Drcheck$value), 3] <- 51.1

points(Drcheck[, 2:3], col = "red")



### occrence data

Dmall <- Dmcheck[, -4]
Dmall$code <- paste0(Dmall$species, Dmall$log, Dmall$lat) 
Dmtrain <- Dmall[Dmall$code %in% sample(Dmall$code, length(Dmall$code)/2), ]
Dmtest <- Dmall[!Dmall$code %in% Dmtrain$code, ]

Dmall$code <- NULL
Dmtrain$code <- NULL
Dmtest$code <- NULL

write.csv(Dmall, "Dm_ENM/dm_joint.csv", row.names = F)
write.csv(Dmtrain, "Dm_ENM/dm_train.csv", row.names = F)
write.csv(Dmtest, "Dm_ENM/dm_test.csv", row.names = F)




### second species ###

Drall <- Drcheck[, -4]
Drall$code <- paste0(Drall$species, Drall$log, Drall$lat) 
Drtrain <- Drall[Drall$code %in% sample(Drall$code, length(Drall$code)/2), ]
Drtest <- Drall[!Drall$code %in% Drtrain$code, ]

Drall$code <- NULL
Drtrain$code <- NULL
Drtest$code <- NULL

write.csv(Drall, "Dr_ENM/dr_joint.csv", row.names = F)
write.csv(Drtrain, "Dr_ENM/dr_train.csv", row.names = F)
write.csv(Drtest, "Dr_ENM/dr_test.csv", row.names = F)


## Model calibration

# Installing and loading packages

if(!require(devtools)){
  install.packages("devtools")
}

if(!require(kuenm)){
  devtools::install_github("marlonecobos/kuenm")
}

library(kuenm)

### Candidate models

spfolders <- c("Dm_ENM/", "Dr_ENM/")

naminfol <- c("dm", "dr")

occ_joint <- paste0(spfolders, naminfol, "_joint.csv")
occ_tra <-  paste0(spfolders, naminfol, "_train.csv")
M_var_dir <-  paste0(spfolders, "M_variables") 
batch_cal <- paste0(spfolders, "Candidate_models") 
out_dir <- paste0(spfolders, "Candidate_Models")
reg_mult <- c(seq(0.1, 1, 0.1), seq(2, 6, 1))
f_clas <- "no.t"
background <- 10000
maxent_path <- "C:/Maxent"
wait <- FALSE
run <- TRUE

occ_test <- paste0(spfolders, naminfol, "_test.csv")
out_eval <- paste0(spfolders, "Calibration_results")
threshold <- 5
rand_percent <- 50
iterations <- 500
kept <- FALSE
selection <- "OR_AICc"
paral_proc <- FALSE

for (i in 1:length(occ_joint)) {
  kuenm_cal(occ.joint = occ_joint[i], occ.tra = occ_tra[i], M.var.dir = M_var_dir[i], batch = batch_cal[i],
            out.dir = out_dir[i], reg.mult = reg_mult, f.clas = f_clas, background = background,
            maxent.path = maxent_path, wait = wait, run = run)
  
  ### Candidate model evaluation and selection
  
  kuenm_ceval(path = out_dir[i], occ.joint = occ_joint[i], occ.tra = occ_tra[i], occ.test = occ_test[i], 
              batch = batch_cal[i], out.eval = out_eval[i], threshold = threshold, rand.percent = rand_percent, 
              iterations = iterations, kept = kept, selection = selection, parallel.proc = paral_proc)
  
}


## Final models and projections

batch_fin <- paste0(spfolders, "Final_models")
mod_dir <- paste0(spfolders, "Final_Models")
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- TRUE
out_format <- "logistic"
project <- TRUE
G_var_dir <- paste0(spfolders, "G_variables")
ext_type <- "all"
write_mess <- FALSE
write_clamp <- FALSE
wait1 <- FALSE
run1 <- TRUE
args <- NULL 

for (i in 1:length(occ_joint)) {
  kuenm_mod(occ.joint = occ_joint[i], M.var.dir = M_var_dir[i], out.eval = out_eval[i], batch = batch_fin[i], rep.n = rep_n,
            rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir[i], out.format = out_format, project = project,
            G.var.dir = G_var_dir[i], ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
            maxent.path = maxent_path, args = args, wait = wait1, run = run1)
  
}


## Changes in suitable areas and suitability, Uncertainty

format <- "asc"
stats <- c("med", "range")
rep <- TRUE
scenarios <- list(c("present", "inm_cm4_rcp4_5", "inm_cm4_rcp8_5_", "ipsl_cm5a_mr_rcp4_5", "ipsl_cm5a_mr_rcp8_5", 
                    "miroc_miroc5_rcp4_5", "miroc_miroc5_rcp8_5", "mohc_hadgem2_es_rcp4_5", "mohc_hadgem2_es_rcp8_5", 
                    "ncar_ccsm4_rcp4_5", "ncar_ccsm4_rcp8_5"),
                  c("present", "inm_cm4_rcp4_5", "inm_cm4_rcp8_5", "ipsl_cm5a_mr_rcp4_5", "ipsl_cm5a_mr_rcp8_5", 
                    "miroc_miroc5_rcp4_5", "miroc_miroc5_rcp8_5", "mohc_hadgem2_es_rcp4_5", "mohc_hadgem2_es_rcp8_5", 
                    "ncar_ccsm4_rcp4_5", "ncar_ccsm4_rcp8_5"))
ext_type <- c("E", "EC", "NE") # the type of extrapolation can be selected according to user requirements 
out_dir1 <- paste0(spfolders, "Final_Model_Stats")

fmod_stats <- paste0(spfolders, "Final_Model_Stats")
thres <- 5
curr <- "present"
emi_scenarios <- c("rcp4_5", "rcp8_5")
c_mods <- c("inm_cm4", "ipsl_cm5a_mr", "miroc_miroc5", "mohc_hadgem2_es", "ncar_ccsm4")
ext_type <- c("E", "EC", "NE")
out_dir2 <- paste0(spfolders, "Projection_Changes") 

split <- 100
out_dir3 <- paste0(spfolders, "Variation_from_sources")

for (i in 1:length(occ_joint)) {
  sp_name <- as.character(read.csv(occ_joint[i])[1, 1])
  
  # model statistic calculation
  kuenm_modstats(sp.name = sp_name, fmod.dir = mod_dir[i], format = format, project = project, 
                 statistics = stats, replicated = rep, proj.scenarios = scenarios[[i]], 
                 ext.type = ext_type, out.dir = out_dir1[i])
  
  # identificaton of changes in suitable areas and suitability
  kuenm_projchanges(occ = occ_joint[i], fmod.stats = fmod_stats[i], threshold = thres, current = curr, 
                    emi.scenarios = emi_scenarios, clim.models = c_mods, ext.type = ext_type, 
                    out.dir = out_dir2[i])
  
  # measuring and maping model variability
  kuenm_modvar(sp.name = sp_name, fmod.dir = mod_dir[i], replicated = rep, format = format,  
               project = project, current = curr, emi.scenarios = emi_scenarios, 
               clim.models = c_mods, ext.type = ext_type, split.length = split, out.dir = out_dir3[i])
  
  cat("#########========= species", i, "of 2 finished\n")
}


## Strict extrapolation

sets_var <- list("set4","set3") # a vector of various sets can be used
out_mop <- paste0(spfolders, "MOP_results")
percent <- 5
paral <- TRUE # make this true to perform MOP calculations in parallel, recommended
# only if a powerfull computer is used (see function's help)
# Two of the variables used here as arguments were already created for previous functions


for (i in 1:length(occ_joint)) {
  kuenm_mmop(G.var.dir = G_var_dir[i], M.var.dir = M_var_dir[i], sets.var = sets_var[[i]], out.mop = out_mop[i],
             percent = percent, parallel = paral, comp.each = 3000)
  
  cat("#########========= species", i, "of 2 finished\n")
}

## agreement of extrapolative areas
source("kuenm_mopagree.R")

# Arguments
inout_format <- "GTiff"
out_magree <- paste0(spfolders, "MOP_agremment")

for (i in 1:length(occ_joint)) {
  kuenm_mopagree(mop.dir = out_mop[i], in.format = inout_format, out.format = inout_format,
                 current = curr, emi.scenarios = emi_scenarios, out.dir = out_magree[i])
  
  cat("#########========= species", i, "of 2 finished\n")
}


# Niche overlap
## Similarity test
if(!require(ENMTools)){
  install_github("danlwarren/ENMTools")
  library(ENMTools)
}

## data 
### Occurrences
dm_occ <- read.csv("Dm_ENM/dm_joint.csv")[, 2:3]
colnames(dm_occ) <- c("Longitude", "Latitude")
dr_occ <- read.csv("Dr_ENM/dr_joint.csv")[, 2:3]
colnames(dr_occ) <- c("Longitude", "Latitude")

### PCs
#### read Ms and create a new extent
dm_m <- raster("Dm_pc_p1.asc")
dr_m <- raster("Dr_pc_p1.asc")

dm_e <- extent(dm_m)
dr_e <- extent(dr_m)

n_ext <- extent(min(dm_e[1], dr_e[1]), max(dm_e[2], dr_e[2]), min(dm_e[3], dr_e[3]), max(dm_e[4], dr_e[4]))

#### cropping variables to new extent for comparisons only
scenarios_names <- dir("Dm_ENM/Temp_f")
scenarios_t <- dir("Dm_ENM/Temp_f", full.names = T)
scenarios_p <- dir("Dm_ENM/Prec_f", full.names = T)

current_Tdir <- "similarity_test/Current_T"
dir.create(current_Tdir)
current_Pdir <- "similarity_test/Current_P"
dir.create(current_Pdir)

fut_Tdir <- "similarity_test/Fut_secnarios_T"
dir.create(fut_Tdir)
fut_Pdir <- "similarity_test/Fut_secnarios_P"
dir.create(fut_Pdir)

for (i in 1:length(scenarios_names)) {
  vars_t <- stack(list.files(path = scenarios_t[i], pattern = ".asc$", full.names = T))
  vars_p <- stack(list.files(path = scenarios_p[i], pattern = ".asc$", full.names = T))
  
  crp_t <- crop(vars_t, n_ext)
  crp_p <- crop(vars_p, n_ext)
  
  numt <- gsub("bio_", "", names(crp_t))
  nump <- gsub("bio_", "", names(crp_p))
  
  if (i < length(scenarios_names)) {
    infoldert <- paste0(fut_Tdir, "/", scenarios_names[i])
    dir.create(infoldert)
    writeRaster(crp_t, filename = paste0(infoldert, "/bio.tif"), 
                format = "GTiff", bylayer = TRUE, suffix = numt)
    
    infolderp <- paste0(fut_Pdir, "/", scenarios_names[i])
    dir.create(infolderp)
    writeRaster(crp_p, filename = paste0(infolderp, "/bio.tif"), 
                format = "GTiff", bylayer = TRUE, suffix = nump)
  } else {
    writeRaster(crp_t, filename = paste0(current_Tdir, "/bio.tif"), 
                format = "GTiff", bylayer = TRUE, suffix = numt)
    
    writeRaster(crp_p, filename = paste0(current_Pdir, "/bio.tif"), 
                format = "GTiff", bylayer = TRUE, suffix = nump)
  }
  
}


#### pcs of variables
source("https://raw.githubusercontent.com/marlonecobos/ENM_manuals/master/Variables_processing/kuenm_rpca.R")

##### temperature
out_folder <- "similarity_test/PCA_temp" # name of folder that will contain the sets 
in_format <- "GTiff" # other options available are "GTiff" and "EHdr" = bil 
out_format <- "GTiff" # other options available are "GTiff" and "EHdr" = bil
n_pcs <- 3 # number of pcs you want as rasters, if not defined all pcs are returned as rasters

pct <- kuenm_rpca(vars.folder = current_Tdir, in.format = in_format, out.format = out_format, project = TRUE, 
                  proj.vars = fut_Tdir, n.pcs = n_pcs, out.dir = out_folder, return.in = TRUE)


##### precipitation
out_folder <- "similarity_test/PCA_prec"  # name of folder that will contain the sets 

pcp <- kuenm_rpca(vars.folder = current_Pdir, in.format = in_format, out.format = out_format, project = TRUE, 
                  proj.vars = fut_Pdir, n.pcs = n_pcs, out.dir = out_folder, return.in = TRUE)

## preprosessing
### projection
WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

### pcs
all_pcs <- stack(stack(list.files(path = "PCA_temp/Initial/", pattern = ".tif$", full.names = T)),
                 stack(list.files(path = "PCA_prec/Initial/", pattern = ".tif$", full.names = T)))
crs(all_pcs) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

two_pcs <- stack(raster("PCA_temp/Initial/pc_1.tif"), raster("PCA_prec/Initial/pc_1.tif"))
crs(two_pcs) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


### Create background layer (M) and background poinst (points only in M)
dm_bg <- (dm_m * 0) + 1
dr_bg <- (dr_m * 0) + 1

crs(dm_bg) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(dr_bg) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

dm_pt <- rasterToPoints(dm_bg)[, 1:2]
dr_pt <- rasterToPoints(dr_bg)[, 1:2]

set.seed(1)
dm_pt <- dm_pt[sample(1:nrow(dm_pt), 10000), ]
dr_pt <- dr_pt[sample(1:nrow(dr_pt), 10000), ]

### Creating enmtool species objects
dm <- enmtools.species()
dm$species.name <- "D_marginatus"
dm$presence.points <- dm_occ
dm$range <- dm_bg
dm$background.points <- dm_pt

dr <- enmtools.species()
dr$species.name <- "D_reticulatus"
dr$presence.points <- dr_occ
dr$range <- dr_bg
dr$background.points <- dr_pt


### ENMTools
####Background or similarity test using the asymmetric approach
bg.sim.asym <- background.test(species.1 = dm, species.2 = dr, env = all_pcs, type = "mx", 
                                nreps = 100, test.type = "asymmetric")

write.csv(t(as.data.frame(bg.sim.asym$p.values)), "Bg_similarity_test_p-values.csv")
write.csv(bg.sim.asym$reps.overlap, "Bg_similarity_test_replicates.csv", row.names = T)

jpeg(filename = "Bg_similarity_test_fig.jpg", width = 300, height = 370, units = "mm", res = 450)
plot(bg.sim.asym)
dev.off()

####Background or similarity test using the asymmetric approach reverse
bg.sim.asym1 <- background.test(species.1 = dr, species.2 = dm, env = all_pcs, type = "mx", 
                                nreps = 100, test.type = "asymmetric")

write.csv(t(as.data.frame(bg.sim.asym1$p.values)), "Bg_similarity_test_p-values1.csv")
write.csv(bg.sim.asym1$reps.overlap, "Bg_similarity_test_replicates1.csv", row.names = T)

jpeg(filename = "Bg_similarity_test_fig1.jpg", width = 300, height = 370, units = "mm", res = 450)
plot(bg.sim.asym1)
dev.off()


### ecospat
esp.bg <- enmtools.ecospat.bg(species.1 = dm, species.2 = dr, env = two_pcs, nreps = 1000)

write.csv(t(as.data.frame(esp.bg$p.values)), "Similarity_test_p-values.csv")
obrep <- as.data.frame(rbind(rbind(unlist(esp.bg$test.results$obs)), esp.bg$test.results$sim))
write.csv(obrep, "Similarity_test_replicates.csv")

jpeg(filename = "Similarity_test_fig.jpg", width = 300, height = 370, units = "mm", res = 450)
par(cex = 0.3)
plot(esp.bg)
dev.off()

jpeg(filename = "Similarity_test_D_sign.jpg", width = 80, height = 50, units = "mm", res = 600)
par(cex = 0.8, mar = c(4.5, 4, 0.5, 0.5) + 0.1)
hist(esp.bg$test.results$sim$D, main = "", breaks = 100, xlab = "D values")
abline(v=esp.bg$test.results$obs$D, col = "red")
dev.off()


## Overlap in E space using ellipsoids

library(devtools)
source("similarity_test/scripts/ellipsoid_overlap_funcs.R")
# devtools::install_github("luismurao/ntbox")
# library(ntbox)
# devtools::install_github("luismurao/hsi")
library(hsi)
library(raster)
dm <- read.csv("similarity_test/dm_joint.csv")
dr <- read.csv("similarity_test/dr_joint.csv")

### Temperature

pca_temp <- stack(list.files("similarity_test/PCA_temp/Initial/",
                             full.names = T,
                             pattern = ".tif$"))
dm_temp <- raster::extract(pca_temp,dm[,2:3])

dm_cov_cent <- cov_center(data =dm_temp,mve = T,
                          vars = c("pc_1","pc_2","pc_3") ,level=0.95)

dm_model_p <- ellipsoidfit(data = pca_temp,
                           centroid = dm_cov_cent$centroid,
                           covar = dm_cov_cent$covariance,
                           level = 0.99999,size = 3)

pca_temp <- stack(list.files("similarity_test/PCA_temp/Initial/",
                             full.names = T,
                             pattern = ".tif$"))
dr_temp <- raster::extract(pca_temp,dr[,2:3])

dr_cov_cent <- cov_center(data =dr_temp,mve = T,
                          vars = c("pc_1","pc_2","pc_3") ,level=0.95)

dr_model_p <- ellipsoidfit(data = pca_temp,
                           centroid = dr_cov_cent$centroid,
                           covar = dr_cov_cent$covariance,
                           level = 0.99999,size = 3)
plot(dr_model_p$suitRaster)
plot(dm_model_p$suitRaster)


sps_env <- list(dm=dm_temp,dr=dr_temp)

overlap_ms <- ellipsoid_overlap(envdata_list = sps_env,
                                   level = 0.975,
                                   rand_points_size = 5000000,
                                   bg_vars=pca_temp)
overlap_ms$overlap_results
overlap_ms$ellipsoid_metadata$dm
overlap_ms$ellipsoid_metadata$dr
library("oompaBase")
library(rgl)
overlapCoords_1_2 <-overlap_ms$overlap_coordinates$Background_Context_Overlap$Niche_1_vs_2[,c(1:3,6)]

# Primero obtenemos los metadatos de los elipsoides
ellipsoid_metadata <- overlap_ms$ellipsoid_metadata
# Graficamos
plot_ellipsoids3d(ellipsoid_metadata,
                  interaction_points = overlapCoords_1_2,
                  colrmp=1,n_prop=1)
rgl::rgl.postscript("dm_vs_dr_Temp_overlap.eps",fmt = "eps")

### backgroun comparison ###

bg_present_temp <- data.frame(na.omit(pca_temp[]))
bg_present_temp$bg_col <- 1
to_samp <- sample(nrow(bg_present_temp), 3000)
plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = bg_present_temp[to_samp,],
                  colrmp = 4,n_prop=0.4)
rgl::rgl.postscript("dm_vs_dr_temp_niches_bg_present.pdf",fmt = "pdf")



## precipitation

pca_prec <- stack(list.files("similarity_test/PCA_prec/Initial/",
                             full.names = T,
                             pattern = ".tif$"))
dm_prec <- raster::extract(pca_prec,dm[,2:3])

dm_cov_cent_prec <- cov_center(data =dm_prec,mve = T,
                          vars = c("pc_1","pc_2","pc_3") ,level=0.95)

dm_model_prec <- ellipsoidfit(data = pca_prec,
                           centroid = dm_cov_cent$centroid,
                           covar = dm_cov_cent$covariance,
                           level = 0.99999,size = 3)


dr_prec <- raster::extract(pca_prec,dr[,2:3])

dr_cov_cent_prec <- cov_center(data =dr_prec,mve = T,
                          vars = c("pc_1","pc_2","pc_3") ,level=0.95)

dr_model_prec <- ellipsoidfit(data = pca_prec,
                           centroid = dr_cov_cent$centroid,
                           covar = dr_cov_cent$covariance,
                           level = 0.99999,size = 3)

plot(dr_model_p$suitRaster)
plot(dm_model_p$suitRaster)


sps_env_prec <- list(dm=dm_prec,dr=dr_prec)

overlap_ms_prec <- ellipsoid_overlap(envdata_list = sps_env_prec,
                                level = 0.975,
                                rand_points_size = 5000000,
                                bg_vars=pca_prec)
overlap_ms_prec$overlap_results
overlap_ms_prec$ellipsoid_metadata$dm
overlap_ms_prec$ellipsoid_metadata$dr

library("oompaBase")
library(rgl)
overlapCoords_1_2_prec <-overlap_ms_prec$overlap_coordinates$Background_Context_Overlap$Niche_1_vs_2[,c(1:3,6)]

# Primero obtenemos los metadatos de los elipsoides
ellipsoid_metadata_prec <- overlap_ms_prec$ellipsoid_metadata
# Graficamos
plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = overlapCoords_1_2_prec,
                  colrmp=1,n_prop=1)
rgl::rgl.postscript("dm_vs_dr_Prec_overlap.pdf",fmt = "pdf")

### background comparison ###
bg_present_prec <- data.frame(na.omit(pca_prec[]))
bg_present_prec$bg_col <- 1
to_samp <- sample(nrow(bg_present_prec), 3000)
plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = bg_present_prec[to_samp,],
                  colrmp = 4,n_prop=0.4)
rgl::rgl.postscript("dm_vs_dr_Prec_niches_bg_present.pdf",fmt = "pdf")


# Comparing temperature and precipitation joint

temp_prec_st <- stack(pca_temp,pca_temp)

dr_temp_prec <- raster::extract(temp_prec_st,dr[,2:3])

#dr_cov_cent_temp_prec <- cov_center(data = dr_temp_prec, mve = T,
#                               vars = c("pc_1.1","pc_2.2","pc_3.2") ,level=0.95)

dm_temp_prec <- raster::extract(temp_prec_st,dm[,2:3])

sps_env_temp_prec <- list(dm=dm_temp_prec[,c("pc_1.1","pc_2.2","pc_3.2")],dr=dr_temp_prec[,c("pc_1.1","pc_2.2","pc_3.2")])

overlap_ms_temp_prec <- ellipsoid_overlap(envdata_list = sps_env_temp_prec ,
                                     level = 0.975,
                                     rand_points_size = 5000000,
                                     bg_vars=temp_prec_st[[c("pc_1.1","pc_2.2","pc_3.2")]])
###### Precipetation

#### RCP4.5

overlap_ms_temp_prec$overlap_results
overlap_ms_temp_prec$ellipsoid_metadata$dm
overlap_ms_temp_prec$ellipsoid_metadata$dr

pca_miroc5_rcp45_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/miroc_miroc5_rcp4_5"
pca_inm_cm4_rcp45_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/inm_cm4_rcp4_5/"
pca_ipsl_cm5a_rcp45_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/ipsl_cm5a_mr_rcp4_5/"
pca_mohc_hadgem2_es_rcp45_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/mohc_hadgem2_es_rcp4_5/"
pca_ncar_ccsm4_rcp45_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/ncar_ccsm4_rcp4_5/"

pca_miroc5_rcp45_prec_st <- stack(list.files(pca_miroc5_rcp45_prec,pattern = ".tif$",full.names = T))
pca_inm_cm4_rcp45_prec_st <- stack(list.files(pca_inm_cm4_rcp45_prec,pattern = ".tif$",full.names = T))
pca_ipsl_cm5a_rcp45_prec_st <- stack(list.files(pca_ipsl_cm5a_rcp45_prec,pattern = ".tif$",full.names = T))
pca_mohc_hadgem2_es_rcp45_prec_st <- stack(list.files(pca_mohc_hadgem2_es_rcp45_prec,pattern = ".tif$",full.names = T))
pca_ncar_ccsm4_rcp45_prec_st <- stack(list.files(pca_ncar_ccsm4_rcp45_prec,pattern = ".tif$",full.names = T))

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_miroc5_rcp45_prec_st[],4),
                  colrmp=4,n_prop=1)
#rgl::rgl.postscript("dm_vs_dr_Prec_overlap.pdf",fmt = "pdf")

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_inm_cm4_rcp45_prec_st[],4),
                  colrmp=4,n_prop=1)
#### RCP 8.5

overlap_ms_temp_prec$overlap_results
overlap_ms_temp_prec$ellipsoid_metadata$dm
overlap_ms_temp_prec$ellipsoid_metadata$dr

pca_miroc5_rcp85_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/miroc_miroc5_rcp8_5"
pca_inm_cm4_rcp85_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/inm_cm4_rcp8_5_/"
pca_ipsl_cm5a_rcp85_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/ipsl_cm5a_mr_rcp8_5/"
pca_mohc_hadgem2_es_rcp85_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/mohc_hadgem2_es_rcp8_5/"
pca_ncar_ccsm4_rcp85_prec <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_prec/ncar_ccsm4_rcp8_5/"

pca_miroc5_rcp85_prec_st <- stack(list.files(pca_miroc5_rcp85_prec,pattern = ".tif$",full.names = T))
pca_inm_cm4_rcp85_prec_st <- stack(list.files(pca_inm_cm4_rcp85_prec,pattern = ".tif$",full.names = T))
pca_ipsl_cm5a_rcp85_prec_st <- stack(list.files(pca_ipsl_cm5a_rcp85_prec,pattern = ".tif$",full.names = T))
pca_mohc_hadgem2_es_rcp85_prec_st <- stack(list.files(pca_mohc_hadgem2_es_rcp85_prec,pattern = ".tif$",full.names = T))
pca_ncar_ccsm4_rcp85_prec_st <- stack(list.files(pca_ncar_ccsm4_rcp85_prec,pattern = ".tif$",full.names = T))

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_miroc5_rcp85_prec_st[],4),
                  colrmp=4,n_prop=1)
#rgl::rgl.postscript("dm_vs_dr_Prec_overlap.pdf",fmt = "pdf")

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_inm_cm4_rcp85_prec_st[],4),
                  colrmp=4,n_prop=1)


####### Temperature

### RCP 4.5

overlap_ms_temp_prec$overlap_results
overlap_ms_temp_prec$ellipsoid_metadata$dm
overlap_ms_temp_prec$ellipsoid_metadata$dr

pca_miroc5_rcp45_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp//miroc_miroc5_rcp4_5"
pca_inm_cm4_rcp45_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/inm_cm4_rcp4_5/"
pca_ipsl_cm5a_rcp45_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/ipsl_cm5a_mr_rcp4_5/"
pca_mohc_hadgem2_es_rcp45_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/mohc_hadgem2_es_rcp4_5/"
pca_ncar_ccsm4_rcp45_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/ncar_ccsm4_rcp4_5/"

pca_miroc5_rcp45_temp_st <- stack(list.files(pca_miroc5_rcp45_prec,pattern = ".tif$",full.names = T))
pca_inm_cm4_rcp45_temp_st <- stack(list.files(pca_inm_cm4_rcp45_prec,pattern = ".tif$",full.names = T))
pca_ipsl_cm5a_rcp45_temp_st <- stack(list.files(pca_ipsl_cm5a_rcp45_prec,pattern = ".tif$",full.names = T))
pca_mohc_hadgem2_es_rcp45_temp_st <- stack(list.files(pca_mohc_hadgem2_es_rcp45_prec,pattern = ".tif$",full.names = T))
pca_ncar_ccsm4_rcp45_temp_st <- stack(list.files(pca_ncar_ccsm4_rcp45_prec,pattern = ".tif$",full.names = T))

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_miroc5_rcp45_temp_st[],4),
                  colrmp=4,n_prop=1)
#rgl::rgl.postscript("dm_vs_dr_Prec_overlap.pdf",fmt = "pdf")

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_inm_cm4_rcp45_temp_st[],4),
                  colrmp=4,n_prop=1)

#### RCP 8.5

overlap_ms_temp_prec$overlap_results
overlap_ms_temp_prec$ellipsoid_metadata$dm
overlap_ms_temp_prec$ellipsoid_metadata$dr

pca_miroc5_rcp85_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp//miroc_miroc5_rcp8_5"
pca_inm_cm4_rcp85_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/inm_cm4_rcp8_5_/"
pca_ipsl_cm5a_rcp85_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/ipsl_cm5a_mr_rcp8_5/"
pca_mohc_hadgem2_es_rcp85_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/mohc_hadgem2_es_rcp8_5/"
pca_ncar_ccsm4_rcp85_temp <- "C:/D_marginatus_D_reticulatus/similarity_test/PCA_temp/ncar_ccsm4_rcp8_5/"

pca_miroc5_rcp85_temp_st <- stack(list.files(pca_miroc5_rcp45_prec,pattern = ".tif$",full.names = T))
pca_inm_cm4_rcp85_temp_st <- stack(list.files(pca_inm_cm4_rcp45_prec,pattern = ".tif$",full.names = T))
pca_ipsl_cm5a_rcp85_temp_st <- stack(list.files(pca_ipsl_cm5a_rcp45_prec,pattern = ".tif$",full.names = T))
pca_mohc_hadgem2_es_rcp85_temp_st <- stack(list.files(pca_mohc_hadgem2_es_rcp45_prec,pattern = ".tif$",full.names = T))
pca_ncar_ccsm4_rcp85_temp_st <- stack(list.files(pca_ncar_ccsm4_rcp45_prec,pattern = ".tif$",full.names = T))

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_miroc5_rcp85_temp_st[],4),
                  colrmp=4,n_prop=1)
#rgl::rgl.postscript("dm_vs_dr_Prec_overlap.pdf",fmt = "pdf")

plot_ellipsoids3d(ellipsoid_metadata_prec,
                  interaction_points = cbind(pca_inm_cm4_rcp85_temp_st[],4),
                  colrmp=4,n_prop=1)





