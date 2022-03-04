#===============================================================
# Proc01 Rasterize shapefiles
#===============================================================
rm(list = ls())

# 1) Working directory ----------------------------------------------------
setwd("O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020")

# 2) Libraries ------------------------------------------------------------
pckg <- c('terra',    
          'raster',
          'magrittr'

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
layer30 <- layer30[[1]]

layer10 <- rast("DEM10m.tif")
layer10

# 5) Rasterize shapefiles -------------------------------------------------

# Harmonize the name of the attribute to be rasterized in each vector layer
{
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
}

# Rasterize the vector layers, convert them into categorical raster layers,
# and save them in a raster stack using the terra package

r <- rast()
for (i in 1:length(all)) {
  rtmp <- terra::rasterize(all[[i]], layer10, "AfgKode") %>% 
    as.factor()
  r <- c(r,rtmp)
}
r

#Check the levels of each raster layer within the raster stack
levels <- lapply(r,function(x){
  levels(x)
})
levels


# 6) Save raster layers ---------------------------------------------------
names(r) <- paste0("CropData_",2011:2020)
saveRDS(names(r),"NamesCropData2011_2020.rds") #Save the names of each raster layer in one RDS file
terra::writeRaster(r,"CropData2011_2020_10m.tif",
                   datatype="INT4S",
                   names=names(r),
                   overwrite=T)

#===============================================================
# END
#===============================================================
