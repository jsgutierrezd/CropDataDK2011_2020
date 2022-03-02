#===============================================================
# Proc01 Rasterize shapefiles
#===============================================================
rm(list = ls())

# 1) Working directory ----------------------------------------------------
setwd("O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020")

# 2) Libraries ------------------------------------------------------------
pckg <- c('terra',    
          'raster'

)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)


# 3) Load field data 2011_2020 shapefiles ---------------------------------

files <- list.files(path = paste0("O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/"),
                    pattern = "\\.shp$",
                    full.names = T
)

all <- lapply(files,function(x){
  terra::vect(x)
})


# 4) Load raster object with parameters that rasterized shapefiles should be resampled to ----------------
layer30 <- rast("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/3_TempDatabase/SOC_SpatioTemporal/STATIC_COVARIATES/StatCov.tif")
layer30 <- layer[[1]]

layer10 <- rast("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/3_TempDatabase/SOC_SpatioTemporal/STATIC_COVARIATES/StatCov.tif")
layer10 <- 

# 5) Rasterize shapefiles -------------------------------------------------

names(all[[1]])[9] <- "AfgKode"
names(all[[2]])[4] <- "AfgKode"
names(all[[3]])[4] <- "AfgKode"
names(all[[4]])[4] <- "AfgKode"
names(all[[5]])[6] <- "AfgKode"
names(all[[6]])[5] <- "AfgKode"
names(all[[7]])[4] <- "AfgKode"
names(all[[8]])[7] <- "AfgKode"
names(all[[9]])[6] <- "AfgKode"
names(all[[10]])[5] <- "AfgKode"

r <- rast()
for (i in 1:length(all)) {
  rtmp <- terra::rasterize(all[[i]], layer30, "AfgKode")
  rtmp <- as.factor(rtmp)
  r <- c(r,rtmp)
}
r

levels <- lapply(r,function(x){
  levels(x)
})
levels


