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



#model_sampled <- subset(model_sampled, z >= -100 & z <= 100)
#model_sampled <- model_sampled[sample(nrow(model_sampled), nrow(model_sampled) / 100), ]

#
source("functions.R")
retrieveModelData()
retrieveRunData()
master_lookup()

rundata <- merge(rundata, mind, by=c("Hole", "Sector", "Run"), all.x = TRUE)
rundata <- normalizeRundata(rundata)


mind$FittedCenterZ <- apply(mind[c('Run', 'r', 'CenterZ', 'RunLength')], 1, function(m_item){
  #print(head(subset(rundata, Run == as.numeric(m_item['Run']))))
 
  return(fitted_center(m_item, "Bz", function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz))
  
})

mind$FittedCenterZBy <- apply(mind[c('Run', 'r', 'CenterZ', 'RunLength')], 1, function(m_item){
  #print(head(subset(rundata, Run == as.numeric(m_item['Run']))))
  
  return(fitted_center(m_item, "By", function(d){return(data.frame(d$NormalizedBy.x, d$NormalizedBy.y))}, reflectHorizVert))
  
})

mind$FittedCenterZByNonNormalized <- apply(mind[c('Run', 'r', 'CenterZ', 'RunLength')], 1, function(m_item){
  #print(head(subset(rundata, Run == as.numeric(m_item['Run']))))
  
  return(fitted_center(m_item, "ByNonNormalized", function(d){return(data.frame(d$By.x, d$By.y))}, reflectHorizVert))
  
})


#262, 264, 271, 275, 276, 
#modeling the scale factor
#
# anomalies between runs..  why are they different.
#  
#

mind$CenterDelta <- mind$CenterZ - mind$FittedCenterZ
mind$FittedDelta <- mind$FittedCenterZ - mind$FittedCenterZBy




rundata$BxOverBz <- rundata$Bx / rundata$Bz
rundata$ByOverBz <- rundata$By / rundata$Bz
#rundata$BxOverBz <- NULL
#rundata$ByOverBz <- NULL

rundata$Run <- as.factor(rundata$Run)
rundata$Hole <- as.factor(rundata$Hole)
rundata$Sector <- as.factor(rundata$Sector)
rundata$Symbol <- as.factor(rundata$Symbol)
rundata$r <- as.numeric(rundata$r)

model_center <- subset(model_sampled, r == 0)

#model_lcenter <- subset(model, longitudinal == 0)

mean_l <- mean(subset(mind, RunLength == 'L' & Good == 'Y')$CenterZ)
mean_s <- mean(subset(mind, RunLength == 'S' & Good == 'Y')$CenterZ)

rundata$NormalizedZ <- apply(rundata, 1, function(x){
  if(x['RunLength'] == 'S'){
    return((as.numeric(x['z']) - as.numeric(x['FittedCenterZ'])) / 10 )
  }
  
  return(as.numeric(x['z']) - as.numeric(x['FittedCenterZ']))
  
})



write.xlsx(mind, "Master Data File.xlsx", "Meta", row.names = FALSE, append = FALSE)

for (ch in levels(rundata$Run)) {
  gc()
  jgc()
  write.xlsx(subset(rundata, Run == ch), "Master Data File.xlsx", ch, append = TRUE, row.names = FALSE)
}

r0 <- subset(rundata, r == 0 & Good == "Y")
r1 <- subset(rundata, r == 1.25 & Good == "Y")
r30 <- subset(rundata, r == 30 & Good == "Y")

model_center <- fixModel(model_center)
model_1 <- fixModel(subset(model_sampled, r == 1.25))
model_30 <- fixModel(subset(model_sampled, r == 30))


r0_with_model <- rbind(r0, model_center)


scalefactor_0_Bz <- max(model_center$Bz) / max(r0$Bz)
scalefactor_0_Bx <- max(model_center$Bx) / max(r0$NormalizedBx)
scalefactor_0_By <- max(model_center$By) / max(r0$NormalizedBy)
#s0r <- plot_ly(data = r0, x = ~NormalizedZ, y = ~Bz, color = ~Run, symbol = ~Symbol)

r0_scaled <- r0
r0_scaled$ScaledBz = r0_scaled$Bz * scalefactor_0_Bz
r0_scaled$ScaledBy = r0_scaled$NormalizedBy * scalefactor_0_By
r0_scaled$ScaledBx = r0_scaled$NormalizedBx * 1
r0_scaled$RunLength <- 'AS'

model_1max <- max(model_1$Bz)
r1max <- max(r1$Bz)

scalefactor_1_Bz <- max(model_1$Bz) / max(r1$Bz)
scalefactor_1_Bx <- max(model_1$Bx) / max(r1$NormalizedBx)
scalefactor_1_By <- max(model_1$By) / max(r1$NormalizedBy)
r1_scaled <- r1
r1_scaled$Bz = r1_scaled$Bz * scalefactor_1_Bz
r1_scaled$ScaledBy = r1_scaled$NormalizedBy * scalefactor_1_By
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

#@TODO: Place Title
#@TODO: Bx Over Bz, By Over Bz
#@TODO: 
#Center
r0BzVx <- plot_ly(data = r0_scaled, x =~NormalizedZ, 
                 y = ~ScaledBz, type="scatter", mode="markers", 
                 name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_center$NormalizedZ, y = ~model_center$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'Bz vs Z at 0 Radius')


#export(p = ptBzVx, file = "output/center-bz-v-z-with-model.png")

r0BxVz <- plot_ly(data = r0_scaled, x =~NormalizedZ, 
                  y = ~ScaledBx, type="scatter", mode="markers", 
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_center$NormalizedZ, y = 0, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'Bx vs Z at 0 Radius')


#export(p = ptBxVz, file = "output/center-bx-v-z-with-model.png")


ptByVz <- plot_ly(data = r0_scaled, x =~NormalizedZ, 
                  y = ~ScaledBy, type="scatter", mode="markers", 
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_center$NormalizedZ, y = ~model_center$By, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'By vs Z at 0 Radius')

#write.xlsx(r0_scaled, "Plot Data File.xlsx", "R0 Scaled", append = TRUE, row.names = FALSE)
#export(p = ptByVz, file = "output/center-by-v-z-with-model.png")


#1.25
r1BzVz <- plot_ly(data = r1_scaled, x =~NormalizedZ, 
                  y = ~ScaledBz, type="scatter", mode="markers", 
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_1$NormalizedZ, y = ~model_1$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
layout(title = 'Bz vs Z at 1.25cm Radius')

#export(p = ptBzVz, file = "output/1.25-bz-v-z-with-model.png")
#write.xlsx(r1_scaled, "Plot Data File.xlsx", "R1 Scaled", append = TRUE, row.names = FALSE)
r1BxVz <- plot_ly(data = r1_scaled, x =~NormalizedZ, 
                  y = ~ScaledBx, type="scatter", mode="markers", 
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_1$NormalizedZ, y = ~model_1$By, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
layout(title = 'Bx vs z at 1.25cm Radius')


r1ByVz <- plot_ly(data = r1_scaled, x =~NormalizedZ, 
                  y = ~ScaledBy, type="scatter", mode="markers", 
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_1$NormalizedZ, y = 0, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'By vs z at 1.25cm Radius')


r1BxBzVz <- plot_ly(data = r1_scaled, x =~NormalizedZ, 
                  y = ~BxOverBz, type="scatter", mode="markers", 
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  #add_trace(x =~model_1$NormalizedZ, y = 0, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'Bx/Bz vs z at 1.25cm Radius')


#export(p = ptBxVz, file = "output/1.25-bx-v-z-with-model.png")


r1ByVz <- plot_ly(data = r1_scaled, x =~NormalizedZ, 
                  y = ~ScaledBy, type="scatter", mode="markers",
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_1$NormalizedZ, y = ~model_1$Bx, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'By vs Z at 1.25cm Radius')


#export(p = ptByVz, file = "output/1.25-by-v-z-with-model.png")


#30
r30BzVz <- plot_ly(data = r30_scaled, x =~NormalizedZ, 
                  y = ~ScaledBz, type="scatter", mode="markers",
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_30$NormalizedZ, y = ~model_30$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'Bz vs Z at 30cm Radius')


#export(p = r0pxbz, file = "output/30-bz-v-z.png")

r30BxVz <- plot_ly(data = r30_scaled, x =~NormalizedZ, 
                  y = ~ScaledBx, type="scatter", mode="markers",
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_30$NormalizedZ, y = ~model_30$Bx, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'Bx vs Z at 30cm Radius')


#export(p = r0pxbx, file = "output/30-bx-v-bz.png")

r30ByVz <- plot_ly(data = r30_scaled, x =~NormalizedZ, 
                  y = ~ScaledBy, type="scatter", mode="markers",
                  name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
  add_trace(x =~model_30$NormalizedZ, y = ~model_30$By, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
  layout(title = 'By vs Z at 30cm Radius')

#write.xlsx(r30_scaled, "Plot Data File.xlsx", "R30 Scaled", append = TRUE, row.names = FALSE)
#export(p = r0pxby, file = "output/30-by-v-bz.png")





#p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
#  add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
#  add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
