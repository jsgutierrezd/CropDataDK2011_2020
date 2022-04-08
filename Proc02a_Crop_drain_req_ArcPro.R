#===============================================================
# Proc02a Crop drainage requirements_rasterizedArcPro
#===============================================================

rm(list = ls())

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

files <- list.files(path = paste0("O:/Tech_AGRO/Jord/Sebastian/Fields_2011-2020/CropDataDK2011_2020/ArcPro/"),
                    pattern = "\\.tif$",
                    full.names = T
)

all <- lapply(files,function(x){
  terra::rast(x)
})


# 4) Clip raster layers ---------------------------------------------------
lim <- vect("Limit/LIMIT.shp")
layer10 <- rast("DEM10m.tif")

all <- lapply(all,function(x){
  terra::crop(x,lim,mask=T)
})

all <- lapply(all,function(x){
  terra::resample(x,layer10)
})

all <- rast(all)
IMK <- all


# 4) Load reclassification table ------------------------------------------

requirements <- read.table(file = 'crop_drain_req.csv',
                           sep = ';',
                           header = TRUE
)

reclasser <- as.matrix(requirements[, c(1, 3)])

# Check if the levels of each raster layer within the raster stack 
# has a corresponding code in the reclassification table
levels <- lapply(as.factor(IMK),function(x){
  levels(x)
})



check <- list()
for (i in 1:10) {
  check[[i]] <- levels[[i]][[1]][levels[[i]][[1]]%nin%reclasser[,1]]
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
reclasser <- as.matrix(rbind(reclasser,check1))


# 5) Reclassify -----------------------------------------------------------
start <- Sys.time()
beginCluster(detectCores()-2)
IMK_reclass <- clusterR(stack(IMK), reclassify,
                        args = list(rcl = reclasser),
                        filename = 'IMK_reclass.tif',
                        datatype = 'INT2U',
                        overwrite = TRUE
)
endCluster()
Sys.time()-start

# 6) Sums for each class --------------------------------------------------


names <- c('yes', 'maybe', 'no')

IMK_reclass <- brick('IMK_reclass.tif')

start <- Sys.time()
rs <- list()
for(i in 1:3)
{
  beginCluster(detectCores()-2)
  
  rs[[i]] <- clusterR(IMK_reclass
                      , calc
                      , args = list(fun = function(x)
                      {
                        out <- sum(x == i, na.rm = TRUE)
                        return(out)
                        # if (is.na(sum(x))) {
                        #   out <- 0
                        # } else {
                        #   out <- sum(x == i, na.rm = TRUE)
                        #   return(out)
                        # }
                      })
                      , filename = paste0('IMK_drain_'
                                          , names[i]
                                          , '.tif')
                      , overwrite = TRUE
                      , datatype  = 'INT2U'
                      , export = c('names', 'i')
  )
  
  endCluster()
}

rs <- stack(rs)
plot(rs)
Sys.time()-start

# 7) Total sum ------------------------------------------------------------
start <- Sys.time()
beginCluster(detectCores()-2)
rsum <- clusterR(rs
                 , calc
                 , args = list(fun = function(x)
                 {
                   out <- sum(x)
                   return(out)
                 })
                 , filename = 'IMK_sum.tif'
                 , overwrite = TRUE
                 , datatype  = 'INT2S'
                 , export = c('names', 'i')
)

endCluster()

plot(rsum)
Sys.time()-start

# 8) Export the final layer clipped by the boundary polygon-------------------------------------------------
#Cut out the final raster layers with the 
#spatial extent of the Denmark boundaries
lim <- vect("Limit/LIMIT.shp")
rsum <- terra::crop(rast(rsum),
                    lim,
                    mask=T,
                    filename="IMK_sum_clipped.tif",
                    datatype="INT2S",
                    overwrite=T)

#===============================================================
# END
#===============================================================