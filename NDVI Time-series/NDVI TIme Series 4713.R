#load raster, sp and rgdal packages
library(raster)
library(sp)
library(rgdal)

#set working directory
setwd("F:/Selected Landsat Images/CORRECTED LANDSAT/4713 Corrected")

#load 1986 raster and check attributes
NDVI1986 <- raster("1986/NDVI_1986_LC0.tif")
NDVI1986

#load 1988 raster and check attributes
NDVI1988 <- raster("1988/NDVI_1988_LC0.tif")
NDVI1988

#load 1991 raster and check attributes
NDVI1991 <- raster("1991/NDVI_1991_LC0.tif")
NDVI1991

#load 1995 raster and check attributes
NDVI1995 <- raster("1995/NDVI_1995_LC0.tif")
NDVI1995

#load 1998 raster and check attributes
NDVI1998 <- raster("1998/NDVI_1998_LC0.tif")
NDVI1998

#load 2000 raster and check attributes
NDVI2000 <- raster("2000 Corrected/C_NDVI_2000.tif")
NDVI2000

#load 2002 raster and check attributes
NDVI2002 <- raster("2002 Corrected/C_NDVI_2002.tif")
NDVI2002

#load 2005 raster and check attributes
NDVI2005 <- raster("2005/NDVI_2005_LC0.tif")
NDVI2005

#load 2011 raster and check attributes
NDVI2011 <- raster("2011/NDVI_2011_LC0.tif")
NDVI2011

#load 2014 raster and check attributes
NDVI2014 <- raster("2014 Corrected/C_NDVI_2014_1.tif")
NDVI2014

#plot NDVI rasters
plot(NDVI1986, main = "1986 NDVI")
plot(NDVI1988, main = "1988 NDVI")
plot(NDVI1991, main = "1991 NDVI")
plot(NDVI1995, main = "1995 NDVI")
plot(NDVI1998, main = "1998 NDVI")
plot(NDVI2000, main = "2000 NDVI")
plot(NDVI2002, main = "2002 NDVI")
plot(NDVI2005, main = "2005 NDVI")
plot(NDVI2011, main = "2011 NDVI")
plot(NDVI2014, main = "2014 NDVI")

#create raster stack
NDVI4713_C <- stack("1986/NDVI_1986_LC0.tif", "1988/NDVI_1988_LC0.tif", "1991/NDVI_1991_LC0.tif", 
                  "1995/NDVI_1995_LC0.tif", "1998/NDVI_1998_LC0.tif",
                  "2000 Corrected/C_NDVI_2000.tif", "2002 Corrected/C_NDVI_2002.tif", "2005/NDVI_2005_LC0.tif",
                  "2011/NDVI_2011_LC0.tif", "2014 Corrected/C_NDVI_2014_1.tif")

NDVI4713_C
plot(NDVI4713_C)

#change 0 value to NA
NDVI4713_NA <- NDVI4713_C
NAvalue(NDVI4713_NA) <- 0
plot(NDVI4713_NA)

#write this raster stack to working directory
writeRaster(NDVI4713_C, "NDVI4713.tif", "GTiff")
writeRaster(NDVI4713_NA, "NDVI4713_NA.tif", "GTiff")

#years variable
years <- c(1986, 1988, 1991, 1995, 1998, 2000, 2002, 2005, 2011, 2014)

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

slope <- calc(NDVI4713_NA, fun_slope)
pvalue <- calc(NDVI4713_NA,fun_pvalue)

#plot slope
plot(slope, main = "NDVI Slope Test")

#Slope raster to working directory 
writeRaster(slope, "NDVI4713_NA_Slope.tif", "GTiff", overwrite =TRUE)

#plot p-value
plot(pvalue, main = "NDVI P-Value Test")

#P-value raster to working directory
writeRaster(pvalue, "NDVI4713_NA_PValue.tif", "GTiff", overwrite = TRUE)