#load raster, sp and rgdal packages
library(raster)
library(sp)
library(rgdal)

#set working directory
setwd("E:/Selected Landsat Images/Transect 47/47-16/4716 LEDAPS R")

#load 1984 raster and check attributes
NDVI1984 <- raster("1984/NDVI_1984_LC0.tif")
NDVI1984

#load 1989 raster and check attributes
NDVI1989 <- raster("1989/NDVI_1989_LC0.tif")
NDVI1989

#load 1990 raster and check attributes
NDVI1990 <- raster("1990/NDVI_1990_LC0.tif")
NDVI1990

#load 1992 raster and check attributes
NDVI1992 <- raster("1992/NDVI_1992_LC0.tif")
NDVI1992

#load 1997 raster and check attributes
NDVI1997 <- raster("1997/NDVI_1997_LC0.tif")
NDVI1997

#load 1998 raster and check attributes
NDVI1998 <- raster("1998/NDVI_1998_LC0.tif")
NDVI1998

#load 2001 raster and check attributes
NDVI2001 <- raster("2001/NDVI_2001_LC0.tif")
NDVI2001

#load 2004 raster and check attributes
NDVI2004 <- raster("2004/NDVI_2004_LC0.tif")
NDVI2004

#load 2011 raster and check attributes
NDVI2011 <- raster("2011/NDVI_2011_LC0.tif")
NDVI2011

#load 2013 raster and check attributes
NDVI2013 <- raster("2013/NDVI_2013_LC0.tif")
NDVI2013

#plot NDVI rasters
plot(NDVI1984, main = "1984 NDVI")
plot(NDVI1989, main = "1989 NDVI")
plot(NDVI1990, main = "1990 NDVI")
plot(NDVI1992, main = "1992 NDVI")
plot(NDVI1997, main = "1997 NDVI")
plot(NDVI1998, main = "1998 NDVI")
plot(NDVI2001, main = "2001 NDVI")
plot(NDVI2004, main = "2004 NDVI")
plot(NDVI2011, main = "2011 NDVI")
plot(NDVI2013, main = "2013 NDVI")

#create raster stack
NDVI4716 <- stack("1984/NDVI_1984_LC0.tif", "1989/NDVI_1989_LC0.tif", "1990/NDVI_1990_LC0.tif", 
                  "1992/NDVI_1992_LC0.tif", "1997/NDVI_1997_LC0.tif",
                  "1998/NDVI_1998_LC0.tif", "2001/NDVI_2001_LC0.tif", "2004/NDVI_2004_LC0.tif",
                  "2011/NDVI_2011_LC0.tif", "2013/NDVI_2013_LC0.tif")

NDVI4716
plot(NDVI4716)

#change 0 value to NA
NDVI4716_NA <- NDVI4716
NAvalue(NDVI4716_NA) <- 0
plot(NDVI4716_NA)

#write this raster stack to working directory
writeRaster(NDVI4716, "NDVI4716.tif", "GTiff")
writeRaster(NDVI4716_NA, "NDVI4716_NA.tif", "GTiff")

#years variable
years <- c(1984, 1989, 1990, 1992, 1997, 1998, 2001, 2004, 2011, 2013)

#slope function
fun_slope <- function(y) { 
  if(sum(is.na(y)) > 4) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[2] 
  }
}

#p-value function
fun_pvalue <- function(y) { 
  if(sum(is.na(y)) > 4) {
    NA
  } else {
    m = lm(y ~ years); summary(m)$coefficients[8] 
  }
}

slope <- calc(NDVI4716_NA, fun_slope)
pvalue <- calc(NDVI4716_NA,fun_pvalue)

#plot slope
plot(slope, main = "NDVI Slope Test")

#Slope raster to working directory 
writeRaster(slope, "NDVI4716_NA_Slope.tif", "GTiff", overwrite =TRUE)

#plot p-value
plot(pvalue, main = "NDVI P-Value Test")

#P-value raster to working directory
writeRaster(pvalue, "NDVI4716_NA_PValue.tif", "GTiff", overwrite = TRUE)