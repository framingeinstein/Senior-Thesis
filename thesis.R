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
model <- read.table("ReferenceData/srr-solenoid.dat.txt")
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
if(exists("runinfo")){ rm(runinfo) }
for (name in file.list) {

    tmp <- gsub("[.]xlsx", "", gsub("Run", "", name))
    tmp <- gsub(" ", "", tmp)
    tmp <- gsub("Sector ?", "", tmp)
    tmp <- gsub("Hole ?", "",tmp)
    
    parts <- strsplit(tmp, "_")[[1]]
    
    run <- read.xlsx(paste("Measurement Raw Data", name, sep="/"), sheetName = "Run Data")
    info <- read.xlsx(paste("Measurement Raw Data", name, sep="/"), sheetName = "Run Info", header=FALSE)
    colnames(run) <- c("z", "Bz", "By", "Bx")
    colnames(info) <- c("Key", "Value")
    
    #print(info[6,]$Value)
    
    r <- as.character(parts[1])
    current <- as.character(info[6,]$Value)
    if(is.na(info[11,]$Value)){
      comments <- ""
    } else {
      comments <- as.character(info[11,]$Value)
    }
    
    #print(info[11,]$Value)
    
    info <- data.frame(Run = character(1), Current = character(1), Comments = character(1))
    #info <- data.frame(r, current, comments)
    info$Run <- r
    info$Current <- current
    info$Comments <- comments
    #colnames(info) = c("Run", "Current", "Comments")
    #print(info)
    #print(current)
    
    #print(info)
    run$Hole <- parts[5]
    run$Sector <- parts[4]
    run$Run <- parts[1]
    run$Name <- name
    
    
    if(exists("rundata")){
      rundata <- rbind(rundata, run)
    } else {
      rundata <- run
    }
    
    if(exists("runinfo")){
      runinfo <- rbind(runinfo, info)
    } else {
      runinfo <- info
    }
}

source("functions.R")
# This performs a chi^2 minimization based on the reflection point 
rundata$Run <- as.factor(rundata$Run)
# Drop Bz = 0, 0 is not a valid value for Bz and indicates probe saturation
rundata <- subset(rundata, Bz != 0)
rundata <- subset(rundata, z <= 200)
rm(mind)
for (ch in levels(rundata$Run)) {
  print(ch)
  center <- getCenterReflectedChiSQ(subset(rundata, Run == ch))
  if(nrow(center) == 0){
    firstdr <- subset(rundata, Run == ch)[1,]
    colnames(center) <- c("z","Bz","By","Bx","Hole","Sector","Run","Name","chi2")
    center[nrow(center)+1,] <- NA
    center$Run <- c(ch)
    center$Name <- firstdr$Name
    center$Sector <- firstdr$Sector
    center$Hole <- firstdr$Hole

    #center$
  }
  
  info <- subset(runinfo, Run == ch)
  center$Current <- info$Current
  center$Comments <- info$Comments
  
  #print(head(center))
  
  #print(nrow(center))
  
  if(!exists("mind")){
    mind <-  center
  } else {
    mind <-  rbind(mind, center)
  }
}

rundata$BxOverBz <- rundata$Bx / rundata$Bz
rundata$ByOverBz <- rundata$By / rundata$Bz
mind$z <- as.numeric(mind$z)

colnames(mind) <- c("CenterZ", "CenterBz", "CenterBy", "CenterBx", "Hole", "Sector", "Run", "File", "CHI2", "Current", "Comments")

mind$RunLength <- apply(mind, 1, function(x){
  print(as.numeric(x['CenterZ']))
  if(as.numeric(x['z']) > 60) return('L')
  return('S')
})


mind$Good <- apply(mind, 1, function(x){
  #print(as.numeric(x['CenterZ']))
  if(as.numeric(x['Current']) <= 1000) return('N')
  if(as.numeric(x['Run']) %in% c(296,298)) return ('N')
  return('Y')
})

mind$R <- apply(mind, 1, function(x){
  
  if(as.numeric(x['Run']) %in% c(296,298,299,301,303,307)) return(30)
  if(as.numeric(x['Run']) %in% c(270,271,308,309,310,311)) return(0)
  return(1.25)
})

#mind$ProbeRotation <-  apply(mind, 1, function(x){
#  if(x['Run']) %in% c()) return(90)
#if(x['Run']) %in% c()) return(90)
#
#})


colnames(mind) <- c("CenterZ", "CenterBz", "CenterBy", "CenterBx", "Hole", "Sector", "Run", "File", "CHI2", "Current", "Comments", "RunLength", "Good", "R")

write.xlsx(mind, "Master Data File.xlsx", "Meta", row.names = FALSE, append = FALSE)

for (ch in levels(rundata$Run)) {
  write.xlsx(subset(rundata, Run == ch), "Master Data File.xlsx", ch, append = TRUE, row.names = FALSE)
}



rundata <- merge(rundata, mind, by=c("Hole", "Sector", "Run"), all.x = TRUE)
rundata$Run <- as.factor(rundata$Run)
rundata$Hole <- as.factor(rundata$Hole)
rundata$Sector <- as.factor(rundata$Sector)
model_center <- subset(model, transverse == 0)
model_lcenter <- subset(model, longitudinal == 0)

#model_sampled <- model[sample(nrow(model), nrow(model) / 100), ]

short <- subset(rundata, RunLength == 'S' && Good == 'Y')
long <- subset(rundata, RunLength == 'L' && Good == 'Y')
rundata$R <- as.numeric(rundata$R)

mean_l <- mean(subset(mind, RunLength == 'L' & Good == 'Y')$CenterZ)
mean_s <- mean(subset(mind, RunLength == 'S' & Good == 'Y')$CenterZ)

rundata$NormalizedZ <- apply(rundata, 1, function(x){
  if(x['RunLength'] == 'S'){
    return((as.numeric(x['z']) - mean_s) / 10 )
  }
  
  return(as.numeric(x['z']) - mean_l)
  
})

rundata$NormalizedBy <- apply(rundata, 1, function(x){
  if(x['RunLength'] == 'S'){
    return((as.numeric(x['z']) - mean_s) / 10 )
  }
  
  return(as.numeric(x['z']) - mean_l)
  
})

rundata$NormalizedBx <- apply(rundata, 1, function(x){
  if(x['RunLength'] == 'S'){
    return((as.numeric(x['z']) - mean_s) / 10 )
  }
  
  return(as.numeric(x['z']) - mean_l)
  
})


r0 <- subset(rundata, R == 0)

r0_short <- subset(r0,  RunLength == 'S')
r0_long <- subset(r0,  RunLength == 'L')

#sector0_short$z <- sector0_short$z


ap <- plot_ly(data = subset(rundata, Good == 'Y'), x = ~NormalizedZ, y = ~Bz, color = ~Run)

mp <- plot_ly(data = model_center, x = ~z, y = ~Bz)

mlp <- plot_ly(data = model_lcenter, x = ~transverse, y = ~Bz)
s0 <- plot_ly(data = sector0, x = ~z, y = ~Bz)



#center data sets on 0

r0_recomb <- rbind(r0_short, r0_long)

recomb <- rbind(short, long)

model_center$RunLength <- 'M'
model_center$Sector <- '0'
model_center$Hole <- 'M'
model_center$Run <- 1
model_center$CenterZ <- 0
model_center$CenterBz <- 0
model_center$CenterBy<- 0
model_center$CenterBx <- 0
model_center$CHI2 <- 0
model_center$By <- 0
model_center$Current <- 0
model_center$Comments <- ""
model_center$Name <- "Model"
model_center$File <- "Model"
model_center$Good <- "Y"
model_center$NormalizedZ <- model_center$z 
model_center$transverse <- NULL
model_center$longitudinal <- NULL
model_center$R <- 0

model_center$RunLength <- as.factor(model_center$RunLength)
model_center$Hole <- as.factor(model_center$Hole)
model_center$Sector <- as.factor(model_center$Sector)
model_center$Run <- as.factor(model_center$Run)

r0_with_model <- rbind(r0, model_center)

#write.xlsx(report.wide, file="Summaries.xlsx", sheetName=sheet, 
#           col.names=TRUE, row.names=FALSE, append=TRUE)

modelmax <- max(model_center$Bz)
r0max <- max(r0$Bz)
scalefactor <- modelmax / r0max
s0r <- plot_ly(data = r0, x = ~NormalizedZ, y = ~Bz, color = ~RunLength)
s0rwm <- plot_ly(data = sector0_recomb_with_model, x = ~z, y = ~Bz, color = ~RunLength)

r0_scaled <- r0
r0_scaled$Bz = r0_scaled$Bz * scalefactor
r0_scaled$RunLength <- 'AS'
r0_with_model <- rbind(r0_with_model, r0_scaled)
r0_with_model <- subset(r0_with_model, NormalizedZ > -100 &  NormalizedZ < 100)
r0_with_model_scaled <- subset(r0_with_model, RunLength %in% c('AS', 'M'))

r0p <- plot_ly(data = r0_with_model_scaled, x = ~NormalizedZ, y = ~Bz, color = ~RunLength)

splineData <- data.frame(
  with(r0, 
       spline(NormalizedZ, Bx, xout = seq(-90, 90, by = 4))
  ),
  method = "spline()"
)

spp <- plot_ly(data = splineData, x = ~x, y = ~y)

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

