#load raster, sp and rgdal packages
library(raster)
library(sp)
library(rgdal)

#set working directory
setwd("F:/Selected Landsat Images/CORRECTED LANDSAT/4715 Corrected")

#load 1984 raster and check attributes
NDVI1984 <- raster("1984/NDVI_1984_LC0.tif")
NDVI1984

#load 1990 raster and check attributes
NDVI1990 <- raster("1990/NDVI_1990_LC0.tif")
NDVI1990

#load 1991 raster and check attributes
NDVI1991 <- raster("1991/NDVI_1991_LC0.tif")
NDVI1991

#load 1992 raster and check attributes
NDVI1992 <- raster("1992/NDVI_1992_LC0.tif")
NDVI1992

#load 1996 raster and check attributes
NDVI1996 <- raster("1996/NDVI_1996_LC0.tif")
NDVI1996

#load 2002 raster and check attributes
NDVI2002 <- raster("2002 Corrected/C_NDVI_2002.tif")
NDVI2002

#load 2006 raster and check attributes
NDVI2006 <- raster("2006/NDVI_2006_LC0.tif")
NDVI2006

#load 2011 raster and check attributes
NDVI2011 <- raster("2011/NDVI_2011_LC0.tif")
NDVI2011

#load 2013 raster and check attributes
NDVI2013 <- raster("2013 Corrected/C_NDVI_2013_1.tif")
NDVI2013

#load 2016 raster and check attributes
NDVI2016 <- raster("2016 Corrected/C_NDVI_2016_1.tif")
NDVI2016

#change 0 value to NA
NAvalue(NDVI1984) <- 0
NAvalue(NDVI1990) <- 0
NAvalue(NDVI1991) <- 0
NAvalue(NDVI1992) <- 0
NAvalue(NDVI1996) <- 0
NAvalue(NDVI2002) <- 0
NAvalue(NDVI2006) <- 0
NAvalue(NDVI2011) <- 0
NAvalue(NDVI2013) <- 0
NAvalue(NDVI2016) <- 0

#plot NDVI rasters
plot(NDVI1984, main = "1984")
plot(NDVI1990, main = "1990")
plot(NDVI1991, main = "1991")
plot(NDVI1992, main = "1992")
plot(NDVI1996, main = "1996")
plot(NDVI2002, main = "2002")
plot(NDVI2006, main = "2006")
plot(NDVI2011, main = "2011")
plot(NDVI2013, main = "2013")
plot(NDVI2016, main = "2016")

#create raster stack
NDVI4715 <- stack("1984/NDVI_1984_LC0.tif", "1990/NDVI_1990_LC0.tif", "1991/NDVI_1991_LC0.tif", 
                  "1992/NDVI_1992_LC0.tif", "1996/NDVI_1996_LC0.tif",
                  "2002 Corrected/C_NDVI_2002.tif", "2006/NDVI_2006_LC0.tif", "2011/NDVI_2011_LC0.tif",
                  "2013 Corrected/C_NDVI_2013_1.tif", "2016 Corrected/C_NDVI_2016_1.tif")

NDVI4715
plot(NDVI4715)

#change 0 value to NA
NDVI4715_NA <- NDVI4715
NAvalue(NDVI4715_NA) <- 0
plot(NDVI4715_NA)

#write this raster stack to working directory
writeRaster(NDVI4715, "NDVI4715.tif", "GTiff")
writeRaster(NDVI4715_NA, "NDVI4715_NA.tif", "GTiff")

#years variable
years <- c(1984, 1990, 1991, 1992, 1996, 2002, 2006, 2011, 2013, 2016)

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

slope <- calc(NDVI4715_NA, fun_slope)
pvalue <- calc(NDVI4715_NA,fun_pvalue)

#plot slope
plot(slope, main = "NDVI Slope Test")

#Slope raster to working directory 
writeRaster(slope, "NDVI4715_NA_Slope.tif", "GTiff", overwrite =TRUE)

#plot p-value
plot(pvalue, main = "NDVI P-Value Test")

#P-value raster to working directory
writeRaster(pvalue, "NDVI4715_NA_PValue.tif", "GTiff", overwrite = TRUE)