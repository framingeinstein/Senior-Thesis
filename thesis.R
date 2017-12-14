#install.packages("googlesheets")

library(googlesheets)
library(dplyr)
library(ggplot2)
library(plotly)
library(akima)
suppressMessages(library(dplyr))
source("functions.R")
library(xlsx)
library(stats)

# Import model data
model <- read.table("~/Desktop/ThesisData/ReferenceData/srr-solenoid.dat.txt")
colnames(model) <- c("transverse", "longitudinal", "Bx", "Bz")
#set z and r in cm instead of meters
model$r <- model$transverse * 100
model$z <- model$longitudinal * 100
#convert units
model$Bz <- model$Bz * 10000

model %>% count(r, Bz) %>% filter(Bz == max(Bz))
model %>% count(z, Bz) %>% filter(Bz == max(Bz))

#get 10% of dats
model_sampled <- model[sample(nrow(model), nrow(model) / 100), ]


file.list <- list.files("Measurement Raw Data", pattern='*.xlsx')
if(exists("rundata")){ rm(rundata) }
for (name in file.list) {

    tmp <- gsub("[.]xlsx", "", gsub("Run", "", name))
    tmp <- gsub(" ", "", tmp)
    tmp <- gsub("Sector ?", "", tmp)
    tmp <- gsub("Hole ?", "",tmp)
    
    parts <- strsplit(tmp, "_")[[1]]
    
    run <- read.xlsx(paste("Measurement Raw Data", name, sep="/"), sheetName = "Run Data")
    colnames(run) <- c("z", "Bz", "By", "Bx")
    
    run$Hole <- parts[5]
    run$Sector <- parts[4]
    run$Run <- parts[1]
    run$Name <- name
    
    if(exists("rundata")){
      rundata <- rbind(rundata, run)
    } else {
      rundata <- run
    }
}

rundata$Run <- as.factor(rundata$Run)
rm(mind)
for (ch in levels(rundata$Run)) {
  print(ch)
  center <- getCenterReflectedChiSQ(subset(rundata, Run == ch))
  
  if(!exists("mind")){
    mind <-  center
  } else {
    mind <-  rbind(mind, center)
  }
}

center <- getCenterReflectedChiSQ(subset(rundata, Run == "307"))

R307 <- subset(rundata, Run == "307")

rundata$BxBz <- rundata$Bx / rundata$Bz
rundata$ByBz <- rundata$By / rundata$Bz

mind$RunLength <- apply(mind, 1, function(x){
  if(x['z'] > 60) return('L')
  return('S')
})

mind$r <- apply(mind, 1, function(x){
  if(x[Run] %in% c(264,270,271) ){
    return(0)
  }
  if(x[Run] %in% c(264,270,271) ){
    return(0)
  }
  if(x[Run] %in% c(264,270,271) ){
    return(0)
  }
})

colnames(mind) <- c("CenterZ", "CenterBz", "CenterBy", "CenterBx", "Hole", "Sector", "Run", "File", "CHI2", "RunLength")



rundata <- merge(rundata, mind, by=c("Hole", "Sector", "Run"), all.x = TRUE)
rundata$Run <- as.factor(rundata$Run)
rundata$Hole <- as.factor(rundata$Hole)
rundata$Sector <- as.factor(rundata$Sector)
model_center <- subset(model, transverse == 0)
model_lcenter <- subset(model, longitudinal == 0)

#model_sampled <- model[sample(nrow(model), nrow(model) / 100), ]

short <- subset(rundata, RunLength == 'S')

long <- subset(rundata, RunLength == 'L')

sector0 <- subset(rundata, Sector == 0)

sector0_short <- subset(sector0, RunLength == 'S')
sector0_long <- subset(sector0, RunLength == 'L')



#sector0_short$z <- sector0_short$z

sp <- plot_ly(data = short, x = ~z, y = ~Bz, color = ~Sector)
lp <- plot_ly(data = long, x = ~z, y = ~Bz, color = ~Sector)
ap <- plot_ly(data = rundata, x = ~z, y = ~Bz, color = ~Sector)
mp <- plot_ly(data = model_center, x = ~z, y = ~Bz)
mlp <- plot_ly(data = model_lcenter, x = ~transverse, y = ~Bz)
s0 <- plot_ly(data = sector0, x = ~z, y = ~Bz)

mean_l <- mean(subset(mind, RunLength == 'L')$CenterZ)
mean_s <- mean(subset(mind, RunLength == 'S')$CenterZ)

#center data sets on 0

short$z <- short$z - mean_s
long$z <- long$z - mean_l

#convert shorts to cm from mm
short$z <- short$z / 10

s0s <- plot_ly(data = sector0_short, x = ~z, y = ~Bz, type = 'scatter', mode="markers") %>%
  layout(shapes=list(type='line', x0= mean_s, x1= mean_s, y0=min(sector0_short$Bz), y1=max(sector0_short$Bz)),
         title = 'This is the Title',
         xaxis = list(title = "X-Axis", showgrid = TRUE),
         yaxis = list(title = "Y-Axis", showgrid = TRUE))

s0l <- plot_ly(data = sector0_long, x = ~z, y = ~Bz) %>%
  layout(shapes=list(type='line', x0= mean_l, x1= mean_l, y0=min(sector0_long$Bz), y1=max(sector0_long$Bz)),
       title = 'This is the Title',
       xaxis = list(title = "Z (cm)", showgrid = TRUE),
       yaxis = list(title = "Bx", showgrid = TRUE))

sector0_recomb <- rbind(sector0_short, sector0_long)

recomb <- rbind(short, long)

model_center$RunLength <- 'M'
model_center$Sector <- '0'
model_center$Hole <- 'M'
model_center$Run <- 0
model_center$CenterZ <- 0
model_center$CenterBz <- 0
model_center$CenterBy<- 0
model_center$CenterBx <- 0
model_center$CHI2 <- 0
model_center$By <- 0
model_center$transverse <- NULL
model_center$longitudinal <- NULL
model_center$r <- NULL

model_center$RunLength <- as.factor(model_center$RunLength)
model_center$Hole <- as.factor(model_center$Hole)
model_center$Sector <- as.factor(model_center$Sector)
model_center$Run <- as.factor(model_center$Run)

sector0_recomb_with_model <- rbind(sector0_recomb, model_center)

#write.xlsx(report.wide, file="Summaries.xlsx", sheetName=sheet, 
#           col.names=TRUE, row.names=FALSE, append=TRUE)

modelmax <- max(model_center$Bz)
sector0max <- max(sector0_recomb$Bz)
scalefactor <- modelmax / sector0max
s0r <- plot_ly(data = sector0_recomb, x = ~z, y = ~Bz, color = ~RunLength)
s0rwm <- plot_ly(data = sector0_recomb_with_model, x = ~z, y = ~Bz, color = ~RunLength)
sector0_recomb_with_model[sector0_recomb_with_model$RunLength=="M"]

sector0_scaled <- sector0_recomb
sector0_scaled$Bz = sector0_scaled$Bz * scalefactor
sector0_scaled$RunLength <- 'AS'
sector0_recomb_scaled_with_model <- rbind(sector0_recomb_with_model, sector0_scaled)
s0rwm_scaled <- plot_ly(data = sector0_recomb_scaled_with_model, x = ~z, y = ~Bz, color = ~RunLength)


s0rwm_scaled <- plot_ly(data = sector0_recomb_scaled_with_model, x = ~z, y = ~Bz, color = ~RunLength)

s0rpx <- plot_ly(data = center_sector0_recomb, x = ~Bz, y = ~Bx, color = ~Run)
s0rpy <- plot_ly(data = center_sector0_recomb, x = ~Bz, y = ~By, color = ~Run)
s0rpyz <- plot_ly(data = center_sector0_recomb, x = ~z, y = ~Bz, color = ~Run)
s0rpxz <- plot_ly(data = center_sector0_recomb, x = ~z, y = ~Bx, color = ~Run)
test <- convolve(sector0_recomb$z, sector0_recomb$Bx, conj = TRUE, type = c("circular", "open", "filter"))



plt <- plot_ly(data = s0rwm_scaled, x = ~z, y = ~By, color = ~Run)
pltz <- plot_ly(data = sector0_scaled, x = ~z, y = ~By, color = ~Run)

#plot By / Bz only need to split because they are all 
#appears to be correlated in Bx  
#Bz 

#write.xlsx(sector0_recomb_scaled_with_model, file="Analysis.xlsx", sheetName="sector0_recomb_scaled_with_model", 
#           col.names=TRUE, row.names=FALSE, append=TRUE)

splineData <- data.frame(
  with(sector0_recomb, 
       spline(z, Bx, xout = seq(-90, 90, by = 4))
  ),
  method = "spline()"
)

s0rpxs <- plot_ly(data = splineData, x = ~x, y = ~y)

#Null 
center_sector0_recomb <- subset(sector0_recomb, Run %in% c(286))
fitBx <- lm(Bx ~ Bz, center_sector0_recomb)
fitBy <- lm(By ~ Bz, center_sector0_recomb)

pltsBx <- RegressionPlots(fitBx)
pltsBy <- RegressionPlots(fitBy)

center_bz_bx %>%
  plot_ly(data = center_sector0_recomb, x = ~z, color = ~Run) %>% 
  add_markers(y = ~Bz) %>% 
  add_lines(x = ~Bz, y = fitted(fitBx))

center_bz_by <- plot_ly(data = sector0_recomb, x = ~z, y = ~Bx, color = ~Run)

summary(fitBx)
summary(fitBy)
gfit <- glm(formula = Bx ~ Bz, family = binomial, data = sector0_recomb)
prd <- predict(fit)
# Scale Factor
# Find the component of Bz bleeding into Bx and By
# A * Bx -> 0 
# B * By -> 0
# Then we can fit.
# Find the center point by flipping
# Bz  fit to the model
#By / Bz vs z
#Bx / Bz vs z

