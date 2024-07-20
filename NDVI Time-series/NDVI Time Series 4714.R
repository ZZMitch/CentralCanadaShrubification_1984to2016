#load raster, sp and rgdal packages
library(raster)
library(sp)
library(rgdal)

#set working directory
setwd("E:/Selected Landsat Images/Transect 47/47-14/4714 LEDAPS R")

#load 1984 raster and check attributes
NDVI1984 <- raster("1984/NDVI_1984_LC0.tif")
NDVI1984

#load 1986 raster and check attributes
NDVI1986 <- raster("1986/NDVI_1986_LC0.tif")
NDVI1986

#load 1989 raster and check attributes
NDVI1989 <- raster("1989/NDVI_1989_LC0.tif")
NDVI1989

#load 1991 raster and check attributes
NDVI1991 <- raster("1991/NDVI_1991_LC0.tif")
NDVI1991

#load 1996 raster and check attributes
NDVI1996 <- raster("1996/NDVI_1996_LC0.tif")
NDVI1996

#load 2002 raster and check attributes
NDVI2002 <- raster("2002/NDVI_2002_LC0.tif")
NDVI2002

#load 2006 raster and check attributes
NDVI2006 <- raster("2006/NDVI_2006_LC0.tif")
NDVI2006

#load 2011 raster and check attributes
NDVI2011 <- raster("2011/NDVI_2011_LC0.tif")
NDVI2011

#load 2013 raster and check attributes
NDVI2013 <- raster("2013/NDVI_2013_LC0.tif")
NDVI2013

#load 2014 raster and check attributes
NDVI2014 <- raster("2014/NDVI_2014_LC0.tif")
NDVI2014

#plot NDVI rasters
plot(NDVI1984, main = "1984 NDVI")
plot(NDVI1986, main = "1986 NDVI")
plot(NDVI1989, main = "1989 NDVI")
plot(NDVI1991, main = "1991 NDVI")
plot(NDVI1996, main = "1996 NDVI")
plot(NDVI2002, main = "2002 NDVI")
plot(NDVI2006, main = "2006 NDVI")
plot(NDVI2011, main = "2011 NDVI")
plot(NDVI2013, main = "2013 NDVI")
plot(NDVI2014, main = "2014 NDVI")

#create raster stack
NDVI4714 <- stack("1984/NDVI_1984_LC0.tif", "1986/NDVI_1986_LC0.tif", "1989/NDVI_1989_LC0.tif", 
                  "1991/NDVI_1991_LC0.tif", "1996/NDVI_1996_LC0.tif",
                  "2002/NDVI_2002_LC0.tif", "2006/NDVI_2006_LC0.tif", "2011/NDVI_2011_LC0.tif",
                  "2013/NDVI_2013_LC0.tif", "2014/NDVI_2014_LC0.tif")

NDVI4714
plot(NDVI4714)

#change 0 value to NA
NDVI4714_NA <- NDVI4714
NAvalue(NDVI4714_NA) <- 0
plot(NDVI4714_NA)

#write this raster stack to working directory
writeRaster(NDVI4714, "NDVI4714.tif", "GTiff")
writeRaster(NDVI4714_NA, "NDVI4714_NA.tif", "GTiff")

#years variable
years <- c(1984, 1986, 1989, 1991, 1996, 2002, 2006, 2011, 2013, 2014)

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

slope <- calc(NDVI4714_NA, fun_slope)
pvalue <- calc(NDVI4714_NA,fun_pvalue)

#plot slope
plot(slope, main = "NDVI Slope Test")

#Slope raster to working directory 
writeRaster(slope, "NDVI4714_NA_Slope.tif", "GTiff", overwrite =TRUE)

#plot p-value
plot(pvalue, main = "NDVI P-Value Test")

#P-value raster to working directory
writeRaster(pvalue, "NDVI4714_NA_PValue.tif", "GTiff", overwrite = TRUE)