#===============================================================
# Proc03 Crop drainage requirements Grassland
#===============================================================

rm(list = ls())
Sys.setenv(language="EN")
# 1) Working directory ----------------------------------------------------
setwd("O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020")

# 2) Libraries ------------------------------------------------------------
pckg <- c('raster',
          'parallel',
          'terra',
          'Hmisc'
          
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)



# 3) Load IMK layers ------------------------------------------------------

IMK <- rast("CropData2011_2020_10m.tif")
names(IMK) <- readRDS("NamesCropData2011_2020.rds")

# 4) Load reclassification table ------------------------------------------

requirements <- read.table(file = 'crop_drain_req.csv',
                           sep = ';',
                           header = TRUE
)

reclasser1 <- as.matrix(requirements[, c(1, 5)])

# Check if the levels of each raster layer within the raster stack 
# has a corresponding code in the reclassification table
levels <- lapply(IMK,function(x){
  levels(x)
})

check <- list()
for (i in 1:10) {
  check[[i]] <- levels[[i]][[1]][levels[[i]][[1]]%nin%reclasser1[,1]]
}
check

# Find the levels in the raster layers that are not present in the reclassification table
check1 <- c()
for (i in 1:10) {
  tmp <- unlist(check[i])
  check1 <- unique(c(check1,tmp))
}

check1
check1 <- data.frame(Kode=check1,klasse=NA)

# Updating reclassification table
reclasser1 <- as.matrix(rbind(reclasser1,as.matrix(check1)))

# 5) Reclassify 1-----------------------------------------------------------
# Reclassify the IMK raster layers to generate a mask for every year. The mask contains values of 0 (for other land uses),
# and 1 (for grasslands)
start <- Sys.time()
beginCluster(detectCores()-2)
masks <- clusterR(stack(IMK), reclassify,
         args = list(rcl = reclasser1),
         filename = 'O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020/Grassland/Grassland_mask.tif',
         datatype = 'INT2U',
         overwrite = TRUE
)
endCluster()
Sys.time()-start
plot(masks[[1]])

# 6) Number of years with grasslands at a pixel level ---------------------

start <- Sys.time()
beginCluster(detectCores()-2)
masks_sum <- clusterR(masks, calc,
                  args = list(sum, na.rm=T),
                  filename = 'O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020/Grassland/Grassland_mask_sum.tif',
                  datatype = 'INT2U',
                  overwrite = TRUE
)
endCluster()
Sys.time()-start
plot(masks_sum)


# 7) Masking the final layer by the DK boundary ---------------------------

lim <- vect("Limit/LIMIT.shp")
masks_sum_clipped <- terra::crop(rast(masks_sum),
                    lim,
                    mask=T,
                    filename="O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020/Grassland/Grassland_mask_sum_clipped.tif",
                    datatype="INT2S",
                    overwrite=T)
plot(masks_sum_clipped)
#===============================================================
# END
#===============================================================