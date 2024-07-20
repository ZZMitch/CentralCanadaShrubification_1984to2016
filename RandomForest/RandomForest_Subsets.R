##########RANDOM FOREST##########

#####load packages#####
library(randomForest)
library(ggplot2)
library(ggRandomForests)
library(randomForestSRC)
library(party)
library(reprtree)
library(corrplot)
library(Hmisc)
library(pdp)

#####define workspace#####
setwd("E:/NDVI Change Analysis/FINAL TABLE/Raster2Point")

#sample_61 <- read.delim("sample_61.txt")
#sample_61

#sample_609 <- read.delim("sample_609.txt")
#sample_609

#sample_6k <- read.delim("sample_6k.txt")
#sample_6k

#sample_60k <- read.delim("sample_60k.txt")
#sample_60k

#sample_600k_1 <- read.delim("sample_600k_1.txt")
#sample_600k_1

sample_600k_100 <- read.delim("sample_600k_100.txt")
sample_600k_100

sample_600k_100_F <- read.delim("sample_600k_100_F.txt")
sample_600k_100_F

sample_600k_100_E <- read.delim("sample_600k_100_E.txt")
sample_600k_100_E

sample_600k_100_T <- read.delim("sample_600k_100_T.txt")
sample_600k_100_T

#sample_600k_100_1 <- read.delim("sample_600k_100_1.txt")
#sample_600k_100_1

###TPI Tests####
#TPI_60k <- read.delim("TPI_60k_test_1.txt")
#TPI_60k
#corTPI <- cor(TPI_60k)
#round(corTPI, 2)
#TPIvars = NDVI_C ~ TPI + TPI_15R + TPI_31R + TPI_61R
#rfTPI_60k <- randomForest(TPIvars, data = TPI_60k, ntree = 500, nodesize = 60, mtry = 1, importance = TRUE)
#print(rfTPI_60k)
#round(importance(rfTPI_60k), 2)

#plot(rfTPI_60k)
#varImpPlot(rfTPI_60k)

#partialPlot(rfTPI_60k, TPI_60k, TPI)
#partialPlot(rfTPI_60k, TPI_60k, TPI_15R)
#partialPlot(rfTPI_60k, TPI_60k, TPI_31R)
#partialPlot(rfTPI_60k, TPI_60k, TPI_61R)

#sample_600k_100$NDVI_PIF <- as.numeric(sample_600k_100$NDVI_PIF)

#####correlation matrix#####
#cor61 <- cor(sample_61)
#round(cor61, 2)

#cor609 <- cor(sample_609)
#round(cor609, 2)

#cor6k <- cor(sample_6k)
#round(cor6k, 2)

#cor60k <- cor(sample_60k)
#round(cor60k, 2)

#cor600k_1 <- cor(sample_600k_1)
#round(cor600k_1, 2)

cor600k_100 <- cor(sample_600k_100)
round(cor600k_100, 2)

#corrplot#
cor600k_100 <- cor(sample_600k_100)
round(cor600k_100, 2)

cor600k_NDVI <- cor(sample_600k_100[ , c(3:5)])
round(cor600k_NDVI, 2)
corrplot.mixed(cor600k_NDVI)

cor600k_Vars <- cor(sample_600k_100[ , -c(1,4:9, 12, 14, 15, 21, 22)])
round(cor600k_Vars, 2)
corrplot.mixed(cor600k_Vars)
corrplot(cor600k_Vars, method = "circle")

corrplot(cor600k_100, method = "number")
corrplot.mixed(cor600k_100)

#with pvalues#
cor600k_Vars2 <- rcorr(as.matrix(sample_600k_100[ , -c(1,4:9,14,15, 21, 22)]))
cor600k_Vars2

cor600k_Vars2$r
cor600k_Vars2$P

corrplot(cor600k_Vars, type = "upper", tl.col = "black", tl.srt = 45)

colnames(cor600k_Vars2$r) <- c("Northings", "NDVI yr-1", "Elevation", "Slope", "Aspect", "TPI", "TWI", "Lakes", "Rivers", "Major Lakes", "Drainages", "Precip Change", "Temp Change")
rownames(cor600k_Vars2$r) <- c("Northings", "NDVI yr-1", "Elevation", "Slope", "Aspect", "TPI", "TWI", "Lakes", "Rivers", "Major Lakes", "Drainages", "Precip Change", "Temp Change")

par(xpd = TRUE)
corrplot(cor600k_Vars2$r, type = "upper", method = "square", p.mat = cor600k_Vars2$P, sig.level = 0.01, insig = "pch", tl.col = "black", tl.srt = 45, mar = c(1, 0, 4, 0), diag = TRUE)

###

cor600k_100_F <- cor(sample_600k_100_F)
round(cor600k_100_F, 2)

cor600k_100_E <- cor(sample_600k_100_E)
round(cor600k_100_E, 2)

cor600k_100_T <- cor(sample_600k_100_T)
round(cor600k_100_T, 2)

#####set variables to categories#####
#sample_61$CLOUDFREE <- as.factor(sample_61$CLOUDFREE)
#sample_61$FTE <- as.factor(sample_61$FTE)
#sample_61$LANDCOVER <- as.factor(sample_61$LANDCOVER)
#sample_61$ASPECT <- as.factor(sample_61$ASPECT)
#sample_61$TPI_SP <- as.factor(sample_61$TPI_SP)
#sample_61$TPI_LC <- as.factor(sample_61$TPI_LC)

#sample_609$CLOUDFREE <- as.factor(sample_609$CLOUDFREE)
#sample_609$FTE <- as.factor(sample_609$FTE)
#sample_609$LANDCOVER <- as.factor(sample_609$LANDCOVER)
#sample_609$ASPECT <- as.factor(sample_609$ASPECT)
#sample_609$TPI_SP <- as.factor(sample_609$TPI_SP)
#sample_609$TPI_LC <- as.factor(sample_609$TPI_LC)

#sample_6k$CLOUDFREE <- as.factor(sample_6k$CLOUDFREE)
#sample_6k$FTE <- as.factor(sample_6k$FTE)
#sample_6k$LANDCOVER <- as.factor(sample_6k$LANDCOVER)
#sample_6k$ASPECT <- as.factor(sample_6k$ASPECT)
#sample_6k$TPI_SP <- as.factor(sample_6k$TPI_SP)
#sample_6k$TPI_LC <- as.factor(sample_6k$TPI_LC)

#sample_60k$CLOUDFREE <- as.factor(sample_60k$CLOUDFREE)
#sample_60k$FTE <- as.factor(sample_60k$FTE)
#sample_60k$LANDCOVER <- as.factor(sample_60k$LANDCOVER)
#sample_60k$ASPECT <- as.factor(sample_60k$ASPECT)
#sample_60k$TPI_SP <- as.factor(sample_60k$TPI_SP)
#sample_60k$TPI_LC <- as.factor(sample_60k$TPI_LC)

#sample_600k_1$CLOUDFREE <- as.factor(sample_600k_1$CLOUDFREE)
#sample_600k_1$FTE <- as.factor(sample_600k_1$FTE)
#sample_600k_1$LANDCOVER <- as.factor(sample_600k_1$LANDCOVER)
#sample_600k_1$ASPECT <- as.factor(sample_600k_1$ASPECT)
#sample_600k_1$TPI_SP <- as.factor(sample_600k_1$TPI_SP)
#sample_600k_1$TPI_LC <- as.factor(sample_600k_1$TPI_LC)

sample_600k_100$CLOUDFREE <- as.factor(sample_600k_100$CLOUDFREE)
sample_600k_100$FTE <- as.factor(sample_600k_100$FTE)
sample_600k_100$LANDCOVER <- as.factor(sample_600k_100$LANDCOVER)
sample_600k_100$ASPECT <- as.factor(sample_600k_100$ASPECT)
sample_600k_100$TPI_SP <- as.factor(sample_600k_100$TPI_SP)
sample_600k_100$TPI_LC <- as.factor(sample_600k_100$TPI_LC)

sample_600k_100_F$CLOUDFREE <- as.factor(sample_600k_100_F$CLOUDFREE)
sample_600k_100_F$FTE <- as.factor(sample_600k_100_F$FTE)
sample_600k_100_F$LANDCOVER <- as.factor(sample_600k_100_F$LANDCOVER)
sample_600k_100_F$ASPECT <- as.factor(sample_600k_100_F$ASPECT)
sample_600k_100_F$TPI_SP <- as.factor(sample_600k_100_F$TPI_SP)
sample_600k_100_F$TPI_LC <- as.factor(sample_600k_100_F$TPI_LC)

sample_600k_100_E$CLOUDFREE <- as.factor(sample_600k_100_E$CLOUDFREE)
sample_600k_100_E$FTE <- as.factor(sample_600k_100_E$FTE)
sample_600k_100_E$LANDCOVER <- as.factor(sample_600k_100_E$LANDCOVER)
sample_600k_100_E$ASPECT <- as.factor(sample_600k_100_E$ASPECT)
sample_600k_100_E$TPI_SP <- as.factor(sample_600k_100_E$TPI_SP)
sample_600k_100_E$TPI_LC <- as.factor(sample_600k_100_E$TPI_LC)

sample_600k_100_T$CLOUDFREE <- as.factor(sample_600k_100_T$CLOUDFREE)
sample_600k_100_T$FTE <- as.factor(sample_600k_100_T$FTE)
sample_600k_100_T$LANDCOVER <- as.factor(sample_600k_100_T$LANDCOVER)
sample_600k_100_T$ASPECT <- as.factor(sample_600k_100_T$ASPECT)
sample_600k_100_T$TPI_SP <- as.factor(sample_600k_100_T$TPI_SP)
sample_600k_100_T$TPI_LC <- as.factor(sample_600k_100_T$TPI_LC)

#sample_600k_100_1$CLOUDFREE <- as.factor(sample_600k_100_1$CLOUDFREE)
#sample_600k_100_1$FTE <- as.factor(sample_600k_100_1$FTE)
#sample_600k_100_1$LANDCOVER <- as.factor(sample_600k_100_1$LANDCOVER)
#sample_600k_100_1$ASPECT <- as.factor(sample_600k_100_1$ASPECT)
#sample_600k_100_1$TPI_SP <- as.factor(sample_600k_100_1$TPI_SP)
#sample_600k_100_1$TPI_LC <- as.factor(sample_600k_100_1$TPI_LC)

#####randomForest#####
#variablesrf = NDVI_C ~ FTE + LANDCOVER + ELEVATION + SLOPE + ASPECT + TPI + TWI + LAKE_50 + RIVER_250 + MAJLAKE + MAJRIVER + PRECIP + TEMP
variablesrf_2 = NDVI_C ~ LANDCOVER + ELEVATION + SLOPE + ASPECT + TPI + TWI + LAKE_50 + RIVER_250 + MAJLAKE + MAJRIVER + PRECIP1 + TEMP1
variablesrf_3 = NDVI_C ~ LANDCOVER + ELEVATION + SLOPE + ASPECT + TPI + TWI + LAKE_50 + RIVER_250 + MAJLAKE + MAJRIVER + TEMP1
###No NORTHINGS, TPI_SP, TPI_LC###

#rf61 <- randomForest(variables, data = sample_61, ntree = 500, importance = TRUE, proximity = TRUE)
#print(rf61)
#round(importance(rf61), 2)

#rf609 <- randomForest(variablesrf, data = sample_609, ntree = 500, importance = TRUE, proximity = TRUE)
#print(rf609)
#round(importance(rf609), 2)

#rf6k <- randomForest(variablesrf, data = sample_6k, ntree = 500, importance = TRUE, proximity = TRUE)
#print(rf6k)
#round(importance(rf6k), 2)

#rf60k <- randomForest(variablesrf, data = sample_60k, ntree = 500, mtry = 14, importance = TRUE)
#print(rf60k)
#round(importance(rf60k), 2)

#rf600k_1 <- randomForest(variablesrf, data = sample_600k_1, ntree = 500, nodesize = 500, mtry = 15, importance = TRUE)
#print(rf600k_1)
#round(importance(rf600k_1), 2)

#rf600k_100 <- randomForest(variablesrf, data = sample_600k_100, ntree = 500, nodesize = 500, mtry = 15, importance = TRUE)
#print(rf600k_100)
#round(importance(rf600k_100), 2)

#rf600k_100_F <- randomForest(variablesrf_f, data = sample_600k_100_F, ntree = 500, nodesize = 138, mtry = 4, importance = TRUE)
#print(rf600k_100_F)
#round(importance(rf600k_100_F), 2)

#rf600k_100_E <- randomForest(variablesrf_2, data = sample_600k_100_E, ntree = 500, nodesize = 177, mtry = 4, importance = TRUE)
#print(rf600k_100_E)
#round(importance(rf600k_100_E), 2)

#rf600k_100_T <- randomForest(variablesrf_2, data = sample_600k_100_T, ntree = 500, nodesize = 292, mtry = 4, importance = TRUE)
#print(rf600k_100_T)
#round(importance(rf600k_100_T), 2)

rf600k_100_0_500_4_100_3 <- randomForest(variablesrf_3, data = sample_600k_100, ntree = 500, nodesize = 100, mtry = 4, importance = TRUE)
print(rf600k_100_0_500_4_100_3)
round(importance(rf600k_100_0_500_4_100_3), 3)

rf600k_100_F_0_500_4_100_2 <- randomForest(variablesrf_2, data = sample_600k_100_F, ntree = 500, nodesize = 100, mtry = 4, importance = TRUE)
print(rf600k_100_F_0_500_4_100_2)
round(importance(rf600k_100_F_0_500_4_100_2), 3)

rf600k_100_E_0_500_4_100_2 <- randomForest(variablesrf_2, data = sample_600k_100_E, ntree = 500, nodesize = 100, mtry = 4, importance = TRUE)
print(rf600k_100_E_0_500_4_100_2)
round(importance(rf600k_100_E_0_500_4_100_2), 3)

rf600k_100_T_0_500_4_100_2 <- randomForest(variablesrf_2, data = sample_600k_100_T, ntree = 500, nodesize = 100, mtry = 4, importance = TRUE)
print(rf600k_100_T_0_500_4_100_2)
round(importance(rf600k_100_T_0_500_4_100_2), 3)

#rf600k_100_1_500_4_100 <- randomForest(variablesrf, data = sample_600k_100_1, ntree = 500, nodesize = 100, mtry = 4, importance = TRUE)
#print(rf600k_100_1_500_4_100)
#round(importance(rf600k_100_1_500_4_100), 3)

#round(importance(rf600k_100_T_0_500_4_100), 3)

######FULL TRASECT RF WITH MTRY = 4 NEEDED???#####

###Save/Load randomForests###
#save(rf600k_100, file = "rf_600k_100.RData")
#load("rf_600k_100.RData")
#print(rf600k_100)

#save(rf600k_100_F, file = "rf_600k_100-F.RData")
#load("rf_600k_100-F.RData")
#print(rf600k_100_F)

#save(rf600k_100_E, file = "rf_600k_100_E.RData")
#load("rf_600k_100_E.RData")
#print(rf600k_100_E)

#save(rf600k_100_T, file = "rf600k_100_T.RData")
#load("rf600k_100_T.RData")
#print(rf600k_100_T)

#save(rf600k_100_0_500_4_100, file = "rf_600k_100_0_500_4_100.RData")
#load("rf_600k_100_0_500_4_100.RData")
#print(rf600k_100_0_500_4_100)

save(rf600k_100_0_500_4_100_2, file = "rf_600k_100_0_500_4_100_2.RData")
load("rf_600k_100_0_500_4_100_2.RData")
print(rf600k_100_0_500_4_100_2)

save(rf600k_100_F_0_500_4_100_2, file = "rf_600k_100_F_0_500_4_100_2.RData")
load("rf_600k_100_F_0_500_4_100_2.RData")
print(rf600k_100_F_0_500_4_100_2)

save(rf600k_100_E_0_500_4_100_2, file = "rf_600k_100_E_0_500_4_100_2.RData")
load("rf_600k_100_E_0_500_4_100_2.RData")
print(rf600k_100_E_0_500_4_100_2)

save(rf600k_100_T_0_500_4_100_2, file = "rf_600k_100_T_0_500_4_100_2.RData")
load("rf_600k_100_T_0_500_4_100_2.RData")
print(rf600k_100_T_0_500_4_100_2)

#mse <- rf600k_100_0_500_4_100$mse
#mse

#predicted <- rf600k_100_0_500_4_100$predicted
#predicted

#rsq <- rf600k_100_0_500_4_100$rsq
#rsq

#save(rf600k_100_F_0_500_4_100, file = "rf_600k_100_F_0_500_4_100.RData")
#load("rf_600k_100_F_0_500_4_100.RData")
#print(rf600k_100_F_0_500_4_100)

#save(rf600k_100_E_0_500_4_100, file = "rf_600k_100_E_0_500_4_100.RData")
#load("rf_600k_100_E_0_500_4_100.RData")
#print(rf600k_100_E_0_500_4_100)

#save(rf600k_100_T_0_500_4_100, file = "rf_600k_100_T_0_500_4_100.RData")
#load("rf_600k_100_T_0_500_4_100.RData")
#print(rf600k_100_T_0_500_4_100)

###Plot randomForest outputs###
par(xpd = FALSE)

#plot(rf61)
#varImpPlot(rf61)

#plot(rf609)
#varImpPlot(rf609)

#plot(rf6k)
#varImpPlot(rf6k)

#plot(rf60k)
#varImpPlot(rf60k)

#plot(rf600k_1)
#varImpPlot(rf600k_1)

#plot(rf600k_100)
#varImpPlot(rf600k_100)

#plot(rf600k_100_F)
#varImpPlot(rf600k_100_F)

#plot(rf600k_100_E)
#varImpPlot(rf600k_100_E)

#plot(rf600k_100_T)
#varImpPlot(rf600k_100_T)

#plot(rf600k_100_0_500_4_100)
#varImpPlot(rf600k_100_0_500_4_100)

#plot(rf600k_100_F_0_500_4_100)
#varImpPlot(rf600k_100_F_0_500_4_100)

#plot(rf600k_100_E_0_500_4_100)
#varImpPlot(rf600k_100_E_0_500_4_100)

#plot(rf600k_100_T_0_500_4_100)
#varImpPlot(rf600k_100_T_0_500_4_100)

#plot(rf600k_100_1_500_4_100)
#varImpPlot(rf600k_100_1_500_4_100)

plot(rf600k_100_0_500_4_100_2)
varImpPlot(rf600k_100_0_500_4_100_2)

plot(rf600k_100_F_0_500_4_100_2)
varImpPlot(rf600k_100_F_0_500_4_100_2)

plot(rf600k_100_E_0_500_4_100_2)
varImpPlot(rf600k_100_E_0_500_4_100_2)

plot(rf600k_100_T_0_500_4_100_2)
varImpPlot(rf600k_100_T_0_500_4_100_2)

###Raw PDs###
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, LANDCOVER)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, TEMP1)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, MAJRIVER)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, MAJLAKE)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, ELEVATION)
#partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, FTE)
#partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, PRECIP1)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, LAKE_50)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, RIVER_250)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, SLOPE)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, ASPECT)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, TWI)
partialPlot(rf600k_100_0_500_4_100_3, sample_600k_100, TPI)

partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, LANDCOVER)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, TEMP1)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, MAJRIVER)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, MAJLAKE)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, ELEVATION)
#partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, FTE)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, PRECIP1)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, LAKE_50)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, RIVER_250)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, SLOPE)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, ASPECT)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, TWI)
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, TPI)

partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, LANDCOVER)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, TEMP1)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, MAJRIVER)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, MAJLAKE)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, ELEVATION)
#partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, FTE)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, PRECIP1)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, LAKE_50)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, RIVER_250)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, SLOPE)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, ASPECT)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, TWI)
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, TPI)

partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, LANDCOVER)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, TEMP1)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, MAJRIVER)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, MAJLAKE)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, ELEVATION)
#partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100_T, FTE)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, PRECIP1)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, LAKE_50)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, RIVER_250)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, SLOPE)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, ASPECT)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, TWI)
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, TPI)

###Full Transect PD###
par(mar=c(1, 8, 4, 1) + 0.1, mgp = c(6, .5, .5))
partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, LANDCOVER, xpd = FALSE, xlab = "", rug = FALSE, main = "", ylim = c(0.0016,0.0028), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'orange', lwd = 3)

#title(main = "Landcover", cex.main = 2.5, ylab = "Full Transect", cex.lab = 2)
#axis(2, seq(0.0015,0.0025, by = 0.0005), cex.axis = 1.75, las = 1)

partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, TEMP1, xlab = "", rug = FALSE, main = "", xlim = c(1.3,1.8), ylim = c(0.0016,0.0028), yaxt = 'n', xaxt = 'n')
#title(main = "Temperature Change (°C)", cex.main = 2.5)
box(lty = 1, col = 'red', lwd = 3)

partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, MAJRIVER, xlab = "", rug = FALSE, main = "", xlim = c(0,30000), ylim = c(0.0016,0.0028), yaxt = 'n', xaxt = 'n')
#title(main = "Major Drainages (m)", cex.main = 2.5)
box(lty = 1, col = 'yellow', lwd = 3)

partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, MAJLAKE, xlab = "", rug = FALSE, main = "", xlim = c(0,30000), ylim = c(0.0016,0.0028), yaxt = 'n', xaxt = 'n')
#title(main = "Major Lakes (m)", cex.main = 2.5)
box(lty = 1, col = 'gray', lwd = 3)

partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, ELEVATION, xlab = "", rug = FALSE, main = "", xlim = c(0,600), ylim = c(0.0016,0.0028), yaxt = 'n', xaxt = 'n')
#title(main = "Elevation (m)", cex.main = 2.5)
box(lty = 1, col = 'blue', lwd = 3)

partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100, PRECIP1, xlab = "", rug = FALSE, main = "", xlim = c(-1,7), ylim = c(0.0016,0.0028), yaxt= 'n', xaxt = 'n')
box(lty = 1, col = "green", lwd = 3)

###Forest PD###
#par(mar=c(1, 8, 4, 1) + 0.1, mgp = c(6, .5, .5))
partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, LANDCOVER, xpd = FALSE, xlab = "", rug = FALSE, main = "", ylim = c(0.0005,0.0019), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'red', lwd = 3)
#title(main = "", cex.main = 2.5, ylab = "Forest Zone", cex.lab = 2)
#axis(2, seq(0.0005,0.0019, by = 0.0007), cex.axis = 1.75, las = 1)

partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, TEMP1, xlab = "", rug = FALSE, main = "", xlim = c(1.3,1.8), ylim = c(0.0005,0.0019), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'gray', lwd = 3)

partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, MAJRIVER, xlab = "", rug = FALSE, main = "", xlim = c(0,30000), ylim = c(0.0005,0.0019), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'orange', lwd = 3)

partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, MAJLAKE, xlab = "", rug = FALSE, main = "", xlim = c(0,30000), ylim = c(0.0005,0.0019), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'blue', lwd = 3)

partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, ELEVATION, xlab = "", rug = FALSE, main = "", xlim = c(0,600), ylim = c(0.0005,0.0019), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'green', lwd = 3)

partialPlot(rf600k_100_F_0_500_4_100_2, sample_600k_100_F, PRECIP1, xlab = "", rug = FALSE, main = "", xlim = c(-1,7), ylim = c(0.0005,0.0019), yaxt= 'n', xaxt = 'n')
box(lty = 1, col = "yellow", lwd = 3)

###Ecotone PD###
#par(mar=c(1, 8, 4, 1) + 0.1, mgp = c(6, .5, .5))
partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, LANDCOVER, rug = FALSE, main = "", xlab = "", ylim = c(0.0009,0.0023), xpd = FALSE, yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'orange', lwd = 3)
#title(main = "", cex.main = 2.5, ylab = "Ecotone Zone", cex.lab = 2)
#axis(2, seq(0.0011,0.0021, by = 0.0005), cex.axis = 1.75, las = 1)

partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, TEMP1, rug = FALSE, xlab = "", main = "", xlim = c(1.3,1.8), ylim = c(0.0009,0.0023), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'red', lwd = 3)

partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, MAJRIVER, rug = FALSE, xlab = "", main = "", xlim = c(0,30000), ylim = c(0.0009,0.0023), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'yellow', lwd = 3)

partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, MAJLAKE, rug = FALSE, xlab = "", main = "", xlim = c(0,30000), ylim = c(0.0009,0.0023), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'gray', lwd = 3)

partialPlot(rf600k_100_E_0_500_4_100_2, sample_600k_100_E, ELEVATION, rug = FALSE, xlab = "", main = "", xlim = c(0,600), ylim = c(0.0009,0.0023), yaxt = 'n', xaxt = 'n')
box(lty = 1, col = 'green', lwd = 3)

partialPlot(rf600k_100_0_500_4_100_2, sample_600k_100_E, PRECIP1, xlab = "", rug = FALSE, main = "", xlim = c(-1,7), ylim = c(0.0009,0.0023), yaxt= 'n', xaxt = 'n')
box(lty = 1, col = "blue", lwd = 3)

###Tundra PD###
#par(mar=c(4, 8, 4, 1) + 0.1, mgp = c(6, .5, .5))
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, LANDCOVER, rug = FALSE, xlab = "", main = "", ylim = c(0.0015,0.0031), xpd = FALSE, yaxt = 'n', xaxt=)
#title(main = "", cex.main = 2.5, ylab = "Tundra Zone", cex.lab = 2)
#axis(2, seq(0.0018,0.0032, by = 0.0007), cex.axis = 1.75, las = 1)
box(lty = 1, col = 'orange', lwd = 3)

#par(mar=c(4, 8, 4, 1) + 0.1, mgp = c(6, 1, .5))
partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, TEMP1, rug = FALSE, xlab = "", main = "", xlim = c(1.3,1.8), ylim = c(0.0015,0.0031), yaxt = 'n', xaxt = 'n')
#axis(1, seq(1.5,2.1, by = 0.2), cex.axis = 1.75)
box(lty = 1, col = 'red', lwd = 3)

partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, MAJRIVER, rug = FALSE, xlab = "", main = "", xlim = c(0,30000), ylim = c(0.0015,0.0031), yaxt = 'n', xaxt = 'n')
#axis(1, seq(0,30000, by = 10000), cex.axis = 1.75)
box(lty = 1, col = 'blue', lwd = 3)

partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, MAJLAKE, rug = FALSE, xlab = "", main = "", xlim = c(0,30000), ylim = c(0.0015,0.0031), yaxt = 'n', xaxt = 'n')
#axis(1, seq(0,30000, by = 10000), cex.axis = 1.75)
box(lty = 1, col = 'gray', lwd = 3)

partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, ELEVATION, rug = FALSE, xlab = "", main = "", xlim = c(0,600), ylim = c(0.0015,0.0031), yaxt = 'n', xaxt = 'n')
#axis(1, seq(0,600, by = 200), cex.axis = 1.75)
box(lty = 1, col = 'green', lwd = 3)

partialPlot(rf600k_100_T_0_500_4_100_2, sample_600k_100_T, PRECIP1, xlab = "", rug = FALSE, main = "", xlim = c(-1,7), ylim = c(0.0015,0.0031), yaxt= 'n', xaxt = 'n')
box(lty = 1, col = "yellow", lwd = 3)

#reprtree:::plot.getTree(rf600k_1)

#####cforest#####
#variablescf = NDVI_C ~ factor(LANDCOVER) + ELEVATION + SLOPE + ASPECT + TPI + factor(TPI_SP) + factor(TPI_LC) + TWI + LAKE_50 + RIVER_250 + MAJLAKE + MAJRIVER + PRECIP + TEMP

#cf61 <- cforest(variablescf, data = sample_61, controls = cforest_unbiased())
#print(cf61)
#varimp(cf61)
#cf609 <- cforest(variablescf, data = sample_609, controls = cforest_unbiased())
#print(cf609)
#varimp(cf609)
#cf6k <- cforest(variablescf, data = sample_6k, controls = cforest_unbiased())
#print(cf6k)
#varimp(cf6k)
#cf60k <- cforest(variablescf, data = sample_60k, controls = cforest_unbiased())
#print(cf60k)
#varimp(cf60k)
#cf600k_1 <- cforest(variablescf, data = sample_600k_1, controls = cforest_unbiased())
#print(cf600k_1)
#varimp(cf600k_1)

#cforestImpPlot <- function(x) {
#  cforest_importance <<- v <- varimp(x)
#  dotchart(v[order(v)])
#}

#cforestImpPlot(cf61)
#cforestImpPlot(cf609)
#cforestImpPlot(cf6k)
#cforestImpPlot(cf60k)
#cforestImpPlot(cf600k_1)

#pt <- prettytree(cf61@ensemble[[1]], names(cf61@data@get("input")))
#nt <- new("BinaryTree")
#nt@tree <- pt
#nt@data <- cf61@data
#nt@responses <- cf61@responses
#pt <- prettytree(cf609@ensemble[[1]], names(cf609@data@get("input")))
#nt <- new("BinaryTree")
#nt@tree <- pt
#nt@data <- cf609@data
#nt@responses <- cf6k@responses
#pt <- prettytree(cf6k@ensemble[[1]], names(cf6k@data@get("input")))
#nt <- new("BinaryTree")
#nt@tree <- pt
#nt@data <- cf6k@data
#nt@responses <- cf6k@responses
#pt <- prettytree(cf60k@ensemble[[1]], names(cf60k@data@get("input")))
#nt <- new("BinaryTree")
#nt@tree <- pt
#nt@data <- cf60k@data
#nt@responses <- cf60k@responses
#nt@responses <- cf600k_1@responses
#pt <- prettytree(cf600k_1@ensemble[[1]], names(cf600k_1@data@get("input")))
#nt <- new("BinaryTree")
#nt@tree <- pt
#nt@data <- cf600k_1@data
#nt@responses <- cf600k_1@responses


###Does not work with categorical variables###
#plot(nt, type = "simple") #y = NDVI Change 