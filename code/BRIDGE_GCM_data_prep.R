library(sp)
library(raster)
library(ncdf4)
library(maptools)
library(abind)
library(sf)

# Model ID codes ----------------------------------------------------------

# model age increment is 1ky for 0-24 ka, then 4 ky up to 800,000 ka
age <- c(0:23, seq(24, 800, by=4))

# model IDs start with 'tei', plus one of [n, N, o] in every combination w a suffix
dgt123 <- 'tei'
dgt4 <- c('t','T','u','U')
dgt5 <- c(letters, LETTERS, 0:9)
mods <- vector()
for (dgt4i in dgt4){
  for (dgt5i in dgt5){
    iMod <- paste0(dgt123, dgt4i, dgt5i)
    mods <- c(mods, iMod)
  }
}
mods <- head(mods, length(age))
modMeta <- data.frame(id=mods, age_1000ka=age)
write.csv(modMeta, 'data/gcm_model_codes.csv', row.names=FALSE)

# bin GCM data every 8-ky
ageSteps <- seq(4, 800, by = 8)
## subset model ids
modSbset <- modMeta$age_1000ka %in% ageSteps
idSbset <- as.character(modMeta$id[modSbset])

# Download nc files -------------------------------------------------------

# direct download each simulation .nc file from BRIDGE website
for (i in 1:length(idSbset)){
  age <- sprintf("%03d", ageSteps[i])
  id <- idSbset[i]
  rt <- 'https://www.paleo.bristol.ac.uk/ummodel/data/'
  adrs <- paste0(rt, id, '/climate/', id, 'o.pgclann.nc') 
  dest <- paste0('Data/gcm_annual_mean/', id, '_', age, '_o.pgclann.nc')
  # Windows needs 'wb' argument to know that a binary transfer is necessary:
  temp <- download.file(adrs, dest, quiet=TRUE, mode='wb') 
}

# Extract data and convert to bricks --------------------------------------

# 'ym'= yearly mean
vars <- c('temp_ym_dpth' # potential temperature
#          'mixLyrDpth_ym_uo' # 1-D mixed layer depth
          )

# set up template raster, 1.25 degree increments (orignal scale)
rEmpt <- raster(ncols=288, nrows= 144, xmn=0, xmx=360, ymn=-90, ymx=90)
llPrj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
lat <- seq(-89.375, 89.375, length=144)
lon <- seq(0, 358.75, length=288)
llGrid <- expand.grid(lon, lat)
colnames(llGrid) <- c('lon','lat')

getRast <- function(mat, coords, rEmpt, prj){ 
  df <- data.frame('var'=as.vector(mat))
  pts <- SpatialPointsDataFrame(coords=coords, data=df)
  r <- rasterize(pts, rEmpt, 'var', fun=mean)
  r <- rotate(r)
  projection(r) <- prj
  return(r)
}

# 67 minutes for 200 models, 3 variables
pt1 <- proc.time()
for (i in 1:length(idSbset)){
  age <- sprintf("%03d", ageSteps[i])
  id <- idSbset[i]
  ann <- nc_open(paste0('Data/gcm_annual_mean/', id, '_', age,'_o.pgclann.nc'))

  dpths <- ann$dim$depth$vals
  nDpths <- length(dpths)

  # Extract at successive depths (except for mixed layer depth, 1D) and export as brick
  # Include age in file name or models that differ only in captalization will overwrite
  for (v in vars) {
    vAnn <- ncvar_get(ann, varid=v)
    expNm <- paste0('data/gcm_annual_mean/', id, '_', age, '_ann_', v, '.tif')
    if (v=='mixLyrDpth_ym_uo'){
      r <- getRast(mat=vAnn, coords=llGrid, rEmpt=rEmpt, prj=llPrj)
      writeRaster(r, nl=1, filename=expNm, format="GTiff", bylayer=FALSE, overwrite=TRUE)
    } else {
      rastL <- lapply(1:nDpths, function(x){
        getRast(mat=vAnn[,,x], coords=llGrid, rEmpt=rEmpt, prj=llPrj)
      } )
      vBrk <- brick(rastL)
      writeRaster(vBrk, nl=nDps, filename=expNm, format="GTiff", bylayer=FALSE, overwrite=TRUE)
    }
  }

} # loop through models 
pt2 <- proc.time()
(pt2-pt1)/60

# Temperature at grid points ----------------------------------------------

# Goal: univariate time series of global temperatures. Problem:
# Because of fluctuations in sea ice volume and hence polar surface area,
# it's important to sample global temperature at standardised sites.
# Start with a 10-degree lat-long grid, but remove any points that
# always or occasionally fall on land. As long as the grid is time-constant,
# any bias from the closer spacing at the poles doesn't matter, 
# only the relative differences in global mean does.

# x <- seq(-170, 180, by = 10)
# y <- seq(-80,  90,  by = 10)
# xy <- expand.grid(x = x, y = y)
# xy <- SpatialPoints(xy, proj4string = crs('+init=epsg:4326'))


source('code/raster_brick_import_fcn.R')

## longitude
x <- seq(-170, 180, by = 10)

## global lat
y <- seq(-80, 90, by = 10)
## low lat
y_ll <- seq(-30, 30, by=10)
## high lat
y_hl <- c(seq(-80, -50, by=10), 
          seq(50, 90, by=10))
## mid lat
y_ml <-  c(seq(-50, -30, by=10), 
           seq(30, 50, by=10))

## construct bbox
xy <- expand.grid(x = x, y = y) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

xyll <- expand.grid(x = x, yll = y_ll) %>%
  st_as_sf(coords = c("x", "yll"), crs = 4326)

xyml <- expand.grid(x = x, yml = y_ml) %>%
  st_as_sf(coords = c("x", "yml"), crs = 4326)

xyhl <- expand.grid(x = x, yhl = y_hl) %>%
  st_as_sf(coords = c("x", "yhl"), crs = 4326)

# Ocean depth layers are determined in 'foram occ data prep' script
dpths <- c(1, 4, 6, 8)

# save MAT from standard global globe as rows=cells, columns=time bins
gridVals <- function(b, d, grid_box){
  temp <- getBrik(b, envNm='ann_temp_ym_dpth', mods=modMeta)
  print(temp)
  surf <- temp[[1]][[d]]
  extract(surf, grid_box, method='bilinear') # buffer=150*1000, fun=mean 
}

gridMeans <- function(d, grid_box){
  glob <- sapply(ageSteps, gridVals, d = d, grid_box=grid_box)
  land <- apply(glob, 1, function(x) any(is.na(x)))
  glob <- glob[!land,]
  colMeans(glob)
}

# spp data binned at 8-ky resolution, so use same for GCMs
ageSteps <- seq(4, 800, by = 8)

dpthMeans <- sapply(dpths, gridMeans, grid_box=xy)
dpthMeans_ll <- sapply(dpths, gridMeans, grid_box=xyll)
dpthMeans_ml <- sapply(dpths, gridMeans, grid_box=xyml)
dpthMeans_hl <- sapply(dpths, gridMeans, grid_box=xyhl)

## save to csv, write a function
export_temp <- function(data, filename){
  ## add age info
  out <- data.frame(cbind(ageSteps, data))
  ## change name
  colnames(out) <- c('bin', paste0('temp_ym_',c('0m','surf','surfsub','sub')))
  ## save
  write.csv(out, filename, row.names = FALSE)
  print("Export sucessful!")
}

export_temp(dpthMeans, "Data/global-MAT_10-deg-grid_8ka.csv")
export_temp(dpthMeans_ml, "Data/global-MAT_10-deg-grid_8ka_ml.csv")
export_temp(dpthMeans_ll, "Data/global-MAT_10-deg-grid_8ka_ll.csv")
export_temp(dpthMeans_hl, "Data/global-MAT_10-deg-grid_8ka_hl.csv")

# note the vertical and horizontal variance in temperature
# (use 4 ka as an example time bin)
temp <- getBrik(4, envNm='ann_temp_ym_dpth', mods=modMeta)
surf <- temp[[1]][[1]]
deep <- temp[[1]][[19]]
dif <- surf - deep
# vertical range in MAT
summary(dif)
range(values(dif), na.rm = TRUE)
mean(values(dif), na.rm = TRUE)
# horizontal range in MAT
summary(surf)
range(values(surf), na.rm = TRUE)
mean(values(surf), na.rm = TRUE)
