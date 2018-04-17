#install.packages("googlesheets")
options(java.parameters = "-Xmx8000m")
library(googlesheets)
library(dplyr)
library(ggplot2)
library(plotly)
library(akima)
suppressMessages(library(dplyr))
source("functions.R")
library(xlsx)
library(stats)
library(varhandle)
library(webshot)

source("functions.R")
retrieveModelData()
retrieveRunData()
master_lookup()
model$By <- model$By * -1
model_sampled$By <- model_sampled$By * -1
model_center$By <- model_center$By * -1
model_1$By <- model_1$By * -1
model_30$By <- model_30$By * -1
#getReflectedChiSQ(subset(rundata, Run = 277), 80, function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz)

rundata <- merge(rundata, mind, by=c("Hole", "Sector", "Run"), all.x = TRUE)
rundata <- normalizeRundata(rundata)
rundata <- subset(rundata, Bz != 0)

#rd <- normalizeRundata(subset(rundata, Run == 277))
mind$FittedCenterZ <- apply(mind[c('Run', 'r', 'CenterZ', 'RunLength')], 1, function(m_item){
  return(fitted_center(m_item, "Bz - r10", function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz, multiplier = 1, s=seq(from=0, to=200, by = .01)))
})
mind$CenterDelta <- abs(mind$CenterZ - mind$FittedCenterZ)

mind[abs(mind$CenterDelta) > 5 & !is.nan(mind$CenterDelta),]$FittedCenterZ <- apply(subset(mind, abs(CenterDelta) > 5 & !is.nan(CenterDelta))[c('Run', 'r', 'CenterZ', 'RunLength')], 1, function(m_item){
  return(fitted_center(m_item, "Bz - r11", function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz, multiplier = 3, s=seq(from=1, to=200, by = .01), standard=F))
})
mind$CenterDelta <- abs(mind$CenterZ - mind$FittedCenterZ)


#abs(mind$CenterDelta) >= 1 & !is.nan(mind$CenterDelta) & mind$Run == 301
#abs(CenterDelta) >= 1 & !is.nan(CenterDelta) & Run == 301

mind[mind$Run == 276,]$FittedCenterZ <- apply(subset(mind, Run == 276)[c('Run', 'r', 'CenterZ', 'RunLength')], 1, function(m_item){
  return(fitted_center(m_item, "Bz - r60", function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz, multiplier = 4, s=seq(from=30, to=120, by = .01)))
})

mind$CenterDelta <- abs(mind$CenterZ - mind$FittedCenterZ)

rundata <- merge(rundata, mind[c("Run", "FittedCenterZ", "Good")], by=c("Run"), all.x = TRUE)
rundata$FittedCenterZ <- rundata$FittedCenterZ.y
rundata$FittedCenterZ.x <- NULL
rundata$FittedCenterZ.y <- NULL
rundata$Good <- rundata$Good.y
rundata$Good.x <- NULL
rundata$Good.y <- NULL

#mind$FittedDelta <- mind$FittedCenterZ - mind$FittedCenterZBy

rundata$BxOverBz <- rundata$NormalizedBx / rundata$Bz
rundata$ByOverBz <- rundata$NormalizedBy / rundata$Bz

rundata$Run <- as.factor(rundata$Run)
rundata$Hole <- as.factor(rundata$Hole)
rundata$Sector <- as.factor(rundata$Sector)
rundata$Symbol <- as.factor(rundata$Symbol)
rundata$r <- as.numeric(rundata$r)

model_center <- subset(model_sampled, r == 0)

mean_l <- mean(subset(mind, RunLength == 'L' & Good == 'Y')$FittedCenterZ)
mean_s <- mean(subset(mind, RunLength == 'S' & Good == 'Y')$FittedCenterZ)

rundata$NormalizedZ <- apply(rundata, 1, function(x){
  if(x['RunLength'] == 'S'){
    return((as.numeric(x['z']) - as.numeric(x['FittedCenterZ'])) / 10 )
  }
  return(as.numeric(x['z']) - as.numeric(x['FittedCenterZ']))
})

rundata$NormalizedZBy <- apply(rundata, 1, function(x){
  if(x['SensorMeasuringBr'] == 'Y'){
    return((as.numeric(x['NormalizedZ']) - 5))
  }
  return((as.numeric(x['NormalizedZ']) - 10))
})

rundata$NormalizedZBx <-apply(rundata, 1, function(x){
  if(x['SensorMeasuringBr'] == 'X'){
    return((as.numeric(x['NormalizedZ']) - 5))
  }
  return((as.numeric(x['NormalizedZ']) - 10))
})

writeMasterDataFile(mind, rundata)


r0 <- subset(rundata, r == 0 & Good == "Y")
r1 <- subset(rundata, r == 1.25 & Good == "Y")
r30 <- subset(rundata, r == 30 & Good == "Y")

model_center <- fixModel(subset(model_center, z %in% seq(from=-110, to=110, by=1)))
model_1 <- fixModel(subset(model_sampled, r == 1.25 & z %in% seq(from=-110, to=110, by=1)))
model_30 <- fixModel(subset(model_sampled, r == 30 & z %in% seq(from=-110, to=110, by=1)))

scale <- max(model_center$Bz) / max(r0$Bz)

scalefactor_0_Bz <- max(model_center$Bz) / max(r0$Bz)
scalefactor_0_Bx <- max(model_center$Bx) / max(r0$NormalizedBx)
scalefactor_0_By <- max(model_center$By) / max(r0$NormalizedBy)
r0_scaled <- r0
r0_scaled$ScaledBz = r0_scaled$Bz * scale
r0_scaled$ScaledBy = r0_scaled$NormalizedBy * scale
r0_scaled$ScaledBx = r0_scaled$NormalizedBx * 1
r0_scaled$RunLength <- 'AS'

model_1max <- max(model_1$Bz)
r1max <- max(r1$Bz)
scalefactor_1_Bz <- max(model_1$Bz) / max(r1$Bz)
scalefactor_1_Bx <- max(model_1$Bx) / max(r1$NormalizedBx)
scalefactor_1_By <- max(model_1$By) / max(r1$NormalizedBy)
r1_scaled <- r1
r1_scaled$ScaledBz = r1_scaled$Bz * scale
r1_scaled$ScaledBy = r1_scaled$NormalizedBy * scale
r1_scaled$ScaledBx = r1_scaled$NormalizedBx * 1
r1_scaled$RunLength <- 'AS'

model_30max <- max(model_30$Bz)
r30max <- max(r30$Bz)
scalefactor_30_Bz <- max(model_30$Bz) / max(r30$Bz)
scalefactor_30_Bx <- max(model_30$Bx) / max(r30$NormalizedBx)
scalefactor_30_By <- max(model_30$By) / max(r30$NormalizedBy)
r30_scaled <- r30
r30_scaled$ScaledBz = r30_scaled$Bz * scalefactor_30_Bz
r30_scaled$ScaledBy = r30_scaled$NormalizedBy * scalefactor_30_By
r30_scaled$ScaledBx = r30_scaled$NormalizedBx * 1
r30_scaled$RunLength <- 'AS'


ra0BxBzVz <- makeBxBzPlot(r0, 'Bx/Bz v z at 0 radius')
ra0BxBzVz
ra0ByBzVz <- makeByBzPlot(r0, 'By/Bz v z at 0 radius')
ra0ByBzVz

ra1BxBzVz <- makeBxBzPlot(r1, 'Bx/Bz v z at 1.25 radius')
ra1BxBzVz
ra1ByBzVz <- makeByBzPlot(r1, 'By/Bz v z at 1.25 radius')
ra1ByBzVz

ra30BxBzVz <- makeBxBzPlot(r30, 'Bx/Bz v z at 30 radius')
ra30BxBzVz
ra30ByBzVz <- makeByBzPlot(r30, 'By/Bz v z at 30 radius')
ra30ByBzVz

rap30ByBzVz <- makeByBzPlotByPhi(r30, 'By/Bz v z at r = 30 cm by Phi')
rap1ByBzVz <- makeByBzPlotByPhi(r1, 'By/Bz v z at r = 1.35 cm by Phi')

export(rap1ByBzVz, file = "ByBz-r1.25-phi.png")
export(rap30ByBzVz, file = "ByBz-r20-phi.png")

export(ra0BxBzVz, file = "BxBz-r0.png")
export(ra0ByBzVz, file = "ByBz-r0.png")
export(ra1BxBzVz, file = "BxBz-r1.25.png")
export(ra1ByBzVz, file = "ByBz-r1.25.png")
export(ra30BxBzVz, file = "BxBz-r30.png")
export(ra30ByBzVz, file = "ByBz-r30.png")
export(ra30ByBzVz, file = "ByBz-r30.png")
#@TODO: Place Title
#@TODO: Bx Over Bz, By Over Bz
#@TODO: 
#export(p = ptBzVx, file = "output/center-bz-v-z-with-model.png")
#Center
#source("functions.R")
r0BzVz <- makeBzPlot(r0_scaled, model_center, T, 'Bz v z at 0 radius')
r30BzVz <- makeBzPlot(r30_scaled, model_30, T, 'Bz v z at 30cm radius')
r1BzVz <- makeBzPlot(r1_scaled, model_1, T, 'Bz v z at 1.25cm radius')

r0ByVz <- makeByPlot(r0, model_center, F, 'By v z at 0 radius')
r1ByVz <- makeByPlot(r1_scaled, model_1, F, 'By v z at 1.25cm radius')
r30ByVz <- makeByPlot(r30_scaled, model_30, T, 'By v z at 30cm radius')

m1ByVz <- makeByPlot(subset(r1_scaled, NormalizedBy > 10000), model_1, F, 'Model By v z at 1.25cm radius')

r0BxVz <- makeBxPlot(r0, model_center, F, 'Bx v z at 0 radius')
r1BxVz <- makeBxPlot(r1, model_1, F, 'Bx v z at 1.25cm radius')
r30BxVz <- makeBxPlot(r30, model_30, F, 'Bx v z at 30cm radius')


r0BzVz
export(r0BzVz, file = "Bz-r0-scaled-with-model.png")
r0ByVz
export(r0ByVz, file = "By-r0-with-model.png")
r0BxVz
export(r0BxVz, file = "Bx-r0-with-model.png")

r1BzVz
export(r1BzVz, file = "Bz-r1.25-scaled-with-model.png")
r1ByVz
export(r1ByVz, file = "By-r1.25-with-model.png")
r1BxVz
export(r1BxVz, file = "Bx-r1.25-with-model.png")

r30BzVz
export(r30BzVz, file = "Bz-r30-scaled-with-model.png")
r30ByVz
export(r30ByVz, file = "By-r30-scaled-with-model.png")
r30BxVz
export(r30BxVz, file = "Bx-r30-with-model.png")

rlt1.25 <- subset(rundata, r <= 1.25 & Good == 'Y')
rlt1.25$r <- as.factor(rlt1.25$r)
rlt1.25.p <- makeBzRPlot(rlt1.25, "Bz v z by R")
export(rlt1.25.p, file = "Bz-r0-and-r1.25.png")

max(model_1$Bz) - max(model_center$Bz)
max(r1$Bz) - max(r0$Bz)


r0_scaled$NormalizedZRounded <- round(r0_scaled$NormalizedZ)
r_m_data <- merge(r0_scaled, model_center[c("NormalizedZ", "Bz")], by.x=c("NormalizedZRounded"), by.y = c("NormalizedZ"), all.x = TRUE)
r_m_data$Difference <- r_m_data$Bz.y - r_m_data$ScaledBz
r_m_data$Varience <- r_m_data$Difference / r_m_data$Bz.y
r_m_data$Run <- as.numeric(r_m_data$Run)
r_m_data <- na.omit(r_m_data)
p <- makeBzVariencePlot(r_m_data, "Varience v z")
export(p, file = "Vaience v z at R0.png")

r1_scaled$NormalizedZRounded <- round(r1_scaled$NormalizedZ)
r_m_data <- merge(r1_scaled, model_center[c("NormalizedZ", "Bz")], by.x=c("NormalizedZRounded"), by.y = c("NormalizedZ"), all.x = TRUE)
r_m_data$Difference <- r_m_data$Bz.y - r_m_data$ScaledBz
r_m_data$Varience <- r_m_data$Difference / r_m_data$Bz.y
r_m_data$Run <- as.numeric(r_m_data$Run)
r_m_data <- na.omit(r_m_data)
p2 <- makeBzVariencePlot(r_m_data, "Varience v z")
export(p, file = "Vaience v z at R1.25.png")

r30_scaled$NormalizedZRounded <- round(r30_scaled$NormalizedZ)
r_m_data <- merge(r30_scaled, model_center[c("NormalizedZ", "Bz")], by.x=c("NormalizedZRounded"), by.y = c("NormalizedZ"), all.x = TRUE)
r_m_data$Difference <- r_m_data$Bz.y - r_m_data$ScaledBz
r_m_data$Varience <- r_m_data$Difference / r_m_data$Bz.y
r_m_data$Run <- as.numeric(r_m_data$Run)
r_m_data <- na.omit(r_m_data)
p3 <- makeBzVariencePlot(r_m_data, "Varience v z")
export(p, file = "Vaience v z at R30.png")



r264 <- subset(rundata, Run == 264)
r264r <- subset(rundata, Run == 264)

q <- data.frame(85)
colnames(q) <- c("z")
r264r <- reflectHoriz(r264, r264r, q)
r264r$Run <- "264 Reflected"
  
rc <- rbind(r264, r264r)

makeReflectPlot(rc, "Bz vs z at r 0 radius with reflection about z=85cm")

max(modelr2_30$Bz) / max(r30_scaled$ScaledBz)

max(subset(mind, RunLength == 'L' & Good == 'Y')$FittedCenterZ) - min(subset(mind, RunLength == 'L' & Good == 'Y')$FittedCenterZ)
max(subset(mind, RunLength == 'S' & Good == 'Y')$FittedCenterZ) - min(subset(mind, RunLength == 'S' & Good == 'Y')$FittedCenterZ) 

mean(subset(mind, RunLength == 'L' & Good == 'Y')$FittedCenterZ)
mean(subset(mind, RunLength == 'S' & Good == 'Y')$FittedCenterZ)
#write.xlsx(r30_scaled, "Plot Data File.xlsx", "R30 Scaled", append = TRUE, row.names = FALSE)