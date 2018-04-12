#change for By centering
reflectHorizontal <- function(R, z){
  return (-1 * as.numeric(z) + 2 * as.numeric(R))
}


jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}    

chisq <- function(data){
  #R2 <- 0;
  apply_chifunction <- function(f, e){
    c2 <- ((as.numeric(f) - as.numeric(e)) ^ 2) / abs(as.numeric(e))
    if(is.na(c2) || is.infinite(c2)){
      #print('na | inf --------')
      c2 <- 100000000 - 1
    }
    #R2 = R2 + c2
    #print(c2)
    return(c2)
  }
  data$chi <- apply(data, 1, function(x){apply_chifunction(x['f'], x['e'])})
  
  return(data)
}

#getScalesFactor <- function(data, ){
#  
#}

reflectHoriz <- function(data, data_reflected, x){
  data_reflected$z <- apply(data, 1, function(y){
    reflectHorizontal(x["z"], y["z"])
  })
  return(data_reflected)
}

reflectHorizVert <- function(data, data_reflected, x){
  data_reflected$z <- apply(data, 1, function(y){
    reflectHorizontal(x["z"], y["z"])
  })
  
  data_reflected$NormalizedBy <- data_reflected$NormalizedBy * -1
  
  return(data_reflected)
}

reflectedCHISq <- function(x, data, data_reflected, comparer, reflector){
  data_reflected <- reflector(data, data_reflected, x)
  #data_reflected$z <- apply(data, 1, function(y){
  #  reflectHorizontal(x["z"], y["z"])
  #})
  data <- data[order(data$z),]
  data_reflected <- data_reflected[order(data_reflected$z),] 
  d <- merge(data, data_reflected, by=c("z", "Sector", "Run", "Hole"), all = TRUE)
  
  # omit over lap
  
  #print(d)
  d <- na.omit(d)
  #d[is.na(d)] <- 0
  
  
  #d[is.na(d)] <- -10000000
  
  #print(head(d))
  d <- subset(d, z != x["z"])
  #print(d)
  if(nrow(d) < nrow(data) / 4){
    #print(nrow(d))
    #print(nrow(data))
    return(99999999)
  }
  
  d <- comparer(d)#data.frame(d$Bz.x, d$Bz.y)
  colnames(d) <- c("f", "e")
  
  #d[is.na(d)] <- -100000
  #print('===========================')
  
  #d <- na.omit(d)
  d <- chisq(d)
  #print(head(d))
  #print('---------------------------')
  #print(subset(d, !is.na(chi)))
  #print(sum(d$chi2))
  #d <- na.omit(d)
  #ct <- chisq.test(d$f, d$e)
  return(sum(d$chi))
  #return(ct$p.value)
}


getReflectedChiSQ <- function(data, point, comparer, reflector){
  x = seq(from=40, to=60, by=1)
  if(max(data$z) > 120){
    x = seq(from=70, to=130, by=1)
  }
  data <- subset(data, z %in% x)
  data <- data[order(data$z),] 
  data_reflected <- data
  data_reflected <- data_reflected[order(data_reflected$z),] 
  
  data$chi2 <-apply(data, 1, function(x){
    return(reflectedCHISq(x, data, data_reflected, comparer, reflector))
  })
  
  
  
  #get the offset with the min chi2
  #print(head(data))
  mindata <- subset(data, z == point)
  return(mindata)
}


getCenterReflectedChiSQ <- function(data){
  data <- data[order(data$z),] 
  data_reflected <- data
  data_reflected <- data_reflected[order(data_reflected$z),] 
  
  #data$chi2 <-apply(data, 1, function(x){
  #  return(reflectedCHISq(x, data, data_reflected, function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz))
  #})
  data$chi2Bz <-apply(data, 1, function(x){
    return(reflectedCHISq(x, data, data_reflected, function(d){return(data.frame(d$Bz.x, d$Bz.y))}, reflectHoriz))
  })
  data$chi2Bx <- 0
  data$chi2By <- 0
  #data$chi2Bx <-apply(data, 1, function(x){
  #  return(reflectedCHISq(x, data, data_reflected, function(d){return(data.frame(d$Bx.x, d$Bx.y))}, reflectHoriz))
  #})
  
  #data$chi2By <-apply(data, 1, function(x){
  #  return(reflectedCHISq(x, data, data_reflected, function(d){return(data.frame(d$By.x, d$By.y))}, reflectHorizVert))
  #})
  
  
  
  #get the offset with the min chi2
  #print(head(data))
  #print(min(data$chi2))
  #mindata <- subset(data, chi2 == min(data$chi2))
  #data <- na.omit(data)
  minz <- subset(data, chi2Bz == min(data$chi2Bz))
  #minx <- subset(data, chi2Bx == min(data$chi2Bx))
  #miny <- subset(data, chi2By == min(data$chi2By))
  
  #if(nrow(minx)) minz$CenterZBx <- minx$z
  #if(nrow(miny)) minz$CenterZBy <- miny$z
  return(minz)
}

RegressionPlots <- function(fit){
  
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  
  # Extract residuals from lm() object
  Residuals <-  resid(fit)
  
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)  
  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  
  # Create data frame 
  # Will be used as input to plot_ly
  
  regMat <- data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage)
  
  # Plot using Plotly
  
  # Fitted vs Residuals
  # For scatter plot smoother
  LOESS1 <- loess.smooth(Fitted.Values, Residuals)
  
  plt1 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # QQ Pot
  plt2 <- regMat %>% 
    plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
              line = list(width = 2)) %>% 
    
    layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6")
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  
  plt3 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Root.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)
  
  plt4 <- regMat %>% 
    plot_ly(x = Leverage, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
  
  plt = list(plt1, plt2, plt3, plt4)
  return(plt)
}

retrieveRunData <- function(){
  file.list <- list.files("Measurement Raw Data", pattern='*.xlsx')
  for (name in file.list) {
    
    tmp <- gsub("[.]xlsx", "", gsub("Run", "", name))
    tmp <- gsub(" ", "", tmp)
    tmp <- gsub("Sector ?", "", tmp)
    tmp <- gsub("Hole ?", "",tmp)
    
    parts <- strsplit(tmp, "_")[[1]]
    
    run <- read.xlsx(paste("Measurement Raw Data", name, sep="/"), 
                     sheetName = "Run Data",
                     stringsAsFactors=FALSE,
                     colClasses = c('numeric', 'numeric', 'numeric', 'numeric'))
    
    
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
    run$Run <- as.numeric(parts[1])
    run$Name <- name
    
    run$z <- as.numeric(run$z)
    if(exists("rd")){
      rd <- rbind(rd, run)
    } else {
      rd <- run
    }
    
    if(exists("ri")){
      ri <- rbind(ri, info)
    } else {
      ri <- info
    }
  }
  assign('rundata',rd,envir=parent.frame())
  assign('runinfo',ri,envir=parent.frame())
  
}

retrieveModelData <- function(){
  # Import model data
  model <- read.table("ReferenceData/srr-solenoid.dat.txt")
  colnames(model) <- c("transverse", "longitudinal", "By", "Bz")
  #set z and r in cm instead of meters
  model$r <- model$transverse * 100
  model$z <- model$longitudinal * 100
  
  #convert units
  model$Bz <- model$Bz * 10000
  model$By <- model$By * -10000
  model$Bx <- 0
  
  model %>% count(r, Bz) %>% filter(Bz == max(Bz))
  model %>% count(z, Bz) %>% filter(Bz == max(Bz))
  
  m1 <- subset(model, r == 1.0)
  m1.5 <- subset(model, r == 1.5)
  
  m1.25 <- merge(m1, m1.5, by=c("z"))
  
  m1.25$Bx <- (m1.25$Bx.x + m1.25$Bx.y) /2
  m1.25$Bz <- (m1.25$Bz.x + m1.25$Bz.y) /2
  m1.25$By <- (m1.25$By.x + m1.25$By.y) /2
  m1.25$transverse <- 0.0125
  m1.25$longitudinal <- m1.25$longitudinal.x
  m1.25$r <- 1.25
  m1.25$r.x <- NULL
  m1.25$r.y <- NULL
  m1.25$transverse.x <- NULL
  m1.25$transverse.y <- NULL
  m1.25$longitudinal.x <- NULL
  m1.25$longitudinal.y <- NULL
  m1.25$Bx.x <- NULL
  m1.25$Bx.y <- NULL
  m1.25$Bz.x <- NULL
  m1.25$Bz.y <- NULL
  m1.25$By.x <- NULL
  m1.25$By.y <- NULL
  #get 10% of dats
  mt <- subset(model, r >= 1 &  r <= 2)
  model_sampled <- subset(model, r == 0 | r == 1.25 | r == 30)
  model_sampled <- rbind(model_sampled, m1.25)
  
  assign('model',model,envir=parent.frame())
  assign('model_sampled',model_sampled,envir=parent.frame())
}

master_lookup <- function(){
  # This performs a chi^2 minimization based on the reflection point 
  rundata$Run <- as.factor(rundata$Run)
  # Drop Bz = 0, 0 is not a valid value for Bz and indicates probe saturation
  rundata <- subset(rundata, Bz != 0)
  #rundata <- subset(rundata, z <= 210)
  
  for (ch in levels(rundata$Run)) {
    #print(ch)
    center <- getCenterReflectedChiSQ(subset(rundata, Run == ch))
    if(nrow(center) == 0){
      firstdr <- subset(rundata, Run == ch)[1,]
      print(colnames(center))
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
    folder <- paste("output", ch, sep = "/")
    if(!dir.exists(folder)){
      dir.create(folder)
    }
  }
  
  colnames(mind) <- c("CenterZ", "CenterBz", "CenterBy", "CenterBx", "Hole", "Sector", "Run", "File", "CHI2Bz", "CHI2Bx", "CHI2By", "Current", "Comments")
  rp <- read.csv("run-properties.csv")
  
  mind <- merge(mind, rp, by=c("Run"))
  
  
  mind$SensorMeasuringBr <- apply(mind[c("pr")], 1, function(r_item){
    if(r_item['pr'] == 90){
      #print('updated for 90')
      return("X")
    }
    if(r_item['pr'] == 180){
      #print('updated for 180')
      return("Y")
    }
    if(r_item['pr'] == 270){
      #print('updated for 270')
      return("X")
    }
    return("Y")
  })
  
  
  mind$RunLength <- apply(mind, 1, function(x){
    #print(as.numeric(x['CenterZ']))
    if(as.numeric(x['CenterZ']) > 60) return('L')
    return('S')
  })
  
  mind$Good <- apply(mind, 1, function(x){
    #print(as.numeric(x['CenterZ']))
    if(as.numeric(x['Current']) <= 1000) return('N')
    if(as.numeric(x['Run']) %in% c(296,298)) return ('N')
    return('Y')
  })
  
  rownames(mind) <- 1:nrow(mind)
  mind$rownumber <- as.numeric(rownames(mind))
  #mind$Symbol <- a
  
  syms = c("cross", "square", "triangle-down", "triangle-up")
  
  mind$Symbol <-apply(mind, 1, function(x){
    mod <- as.numeric(x['rownumber']) %% 4
    return(syms[mod + 1])
  })
  
  mind$rownumber <- NULL
  mind$FittedCenterZ <- 0
  
  assign('mind',mind,envir=parent.frame())
}


normalizeRundata <- function(rundata){
  
  #y = r, x = phi, z = z
  #if 90 then  y records x,  x records -y
  #  x = y,  y = -x
  #if 180 then y records -y, x records -x
  #  x = -x, y = -y
  #if 270 then y records -x, x records y
  #  x = -y, y = x
  
  rundata$NormalizedBx <- apply(rundata[c("pr", "Bx", "By")], 1, function(r_item){
    if(r_item['pr'] == 90){
      return(as.numeric(r_item["By"]))
    }
    if(r_item['pr'] == 180){
      return(-1 * as.numeric(r_item["Bx"]))
    }
    if(r_item['pr'] == 270){
      return(-1 * as.numeric(r_item["By"]))
    }
    return(as.numeric(r_item["Bx"]))
  })
  
  
  rundata$NormalizedBy <- apply(rundata[c("pr", "Bx", "By")], 1, function(r_item){
    if(r_item['pr'] == 90){
      #print('updated for 90')
      return(-1 * as.numeric(r_item["Bx"]))
    }
    if(r_item['pr'] == 180){
      #print('updated for 180')
      return(-1 * as.numeric(r_item["By"]))
    }
    if(r_item['pr'] == 270){
      #print('updated for 270')
      return(as.numeric(r_item["Bx"]))
    }
    return(as.numeric(r_item["By"]))
  })
  
  return(rundata)
}

fitted_center <- function(m_item, clabel, comparer, reflector, multiplier = 1, offset = 0, s=seq(from=-100, to=100, by=.1)){
  x <- s
  if(exists("mod")){ rm(mod) }
  #print(m_item['Run'])
  # shift off center
  
  s = seq(from=as.numeric(m_item['CenterZ']) - (2 * multiplier) , to=as.numeric(m_item['CenterZ']) + (2 * multiplier), by=2)
  if(as.numeric(m_item['Run']) == 311){
    s = seq(from=as.numeric(m_item['CenterZ']) - (5 * multiplier), to=as.numeric(m_item['CenterZ']) +  (5 * multiplier), by=5)
  }
  
  for(i in s){
    yyyy <- getReflectedChiSQ( subset(rundata, Run == as.numeric(m_item['Run'])), i, comparer, reflector)
    #print(head(yyyy))
    #colnames(yyyy) <- c("Hole","Sector","Run", "z", "Bz", "By", "Bx", "Name", "CenterZ", "CenterBz", "CenterBy", "CenterBx", "File", "CHI2", "CHI2Bz", "CHI2Bx", "CHI2By", "Current", "Comments", "r", "theta", "full", "pr", "SensorMeasuringBr", "RunLength", "Good", "Symbol", "FittedCenterZ", "NormalizedBx", "NormalizedBy", "K2")
    
    abbreviated <- yyyy[c("z", "chi2")]
    colnames(abbreviated) <- c("CenterZ", "CHI2")
    if(exists("mod")){
      mod <- rbind(mod, abbreviated)
    } else {
      mod <- abbreviated
    }
  }
  
  
  
  #print(head(mod))
  
  #threshold <- ifelse(m_item['r']==30, 530000, 100000)
  #pts <- subset(mod, CHI2 < threshold)
  #if(nrow(pts) < 3){
  #  pts <- subset(mod, CHI2 < threshold * 10)
  #  if(nrow(pts) < 3){
  #    pts <- subset(mod, CHI2 < threshold * 100)
  #  }
  #}
  
  #mod <- pts
  
  if(!is.numeric(mod$CenterZ)){
    return(NaN)
  }
  
  model <- tryCatch(
    lm(mod$CHI2 ~ mod$CenterZ + I(mod$CenterZ ^ 2) ), error = function(e){
      return(NaN)
    }
  )
  
  if(is.na(model)){
    return(NaN)
  }
  
  df <- data.frame(x)
  df$y <- apply(df, 1, function(p_item){
    result <- (model$coefficients[3] * (model$coefficients[3] / abs(model$coefficients[3]))) * p_item['x'] * p_item['x'] + model$coefficients[2] * p_item['x'] + model$coefficients[1] 
    return (result)
  })
  df$coefficients3 <- model$coefficients[3]
  df$coefficients2 <- model$coefficients[2]
  df$coefficients1 <- model$coefficients[1]
  df$distance <- abs(df$y)
  gc()
  jgc()
  file.name <- paste("Fit Data File", clabel, sep = " - ")
  write.xlsx(df, paste(file.name, "xlsx", sep = "."), paste(clabel, m_item['Run'], sep = "-"), append = TRUE, row.names = FALSE)
  
  mini = subset(df, y == min(df$y))
  #print(head(mini))
  
  return(min(mini$x))
  
}


fixModel <- function(model_center){
  model_center$RunLength <- 'M'
  model_center$Sector <- '0'
  model_center$Hole <- 'M'
  model_center$Run <- 1
  model_center$CenterZ <- 0
  model_center$CenterBz <- 0
  model_center$CenterBy<- 0
  model_center$CenterBx <- 0
  model_center$CHI2 <- 0
  model_center$CHI2Bz <- 0
  model_center$CHI2Bx <- 0
  model_center$CHI2By <- 0
  model_center$Current <- 0
  model_center$Comments <- ""
  model_center$Name <- "Model"
  model_center$File <- "Model"
  model_center$Good <- "Y"
  model_center$NormalizedZ <- model_center$z 
  model_center$NormalizedBx <- model_center$Bx 
  model_center$NormalizedBy <- model_center$r
  model_center$By <- model_center$r
  model_center$transverse <- NULL
  model_center$longitudinal <- NULL
  model_center$Symbol <- "square"
  model_center$RunLength <- as.factor(model_center$RunLength)
  model_center$Hole <- as.factor(model_center$Hole)
  model_center$Sector <- as.factor(model_center$Sector)
  model_center$Run <- as.factor(model_center$Run)
  model_center$theta <- 0
  model_center$full <- 1
  model_center$pr <- 0
  
  model_center$BxOverBz <- model_center$Bx / model_center$Bz
  model_center$ByOverBz <- model_center$By / model_center$Bz
  
  model_center$FittedCenterZ <- 0
  model_center$FittedCenterZBx <- 0
  model_center$FittedCenterZBy <- 0
  model_center$SensorMeasuringBr <- "Y"
  return(model_center)
}


writeMasterDataDile <- function(m, r){
  write.xlsx(m, "Master Data File.xlsx", "Meta", row.names = FALSE, append = FALSE)
  
  for (ch in levels(rundata$Run)) {
    gc()
    jgc()
    write.xlsx(subset(r, Run == ch), "Master Data File.xlsx", ch, append = TRUE, row.names = FALSE)
  }
}

makeBzplot <- function(data, model, scaled = F){
  if(scaled){
    p <- plot_ly(data = data, x =~NormalizedZ, 
                 y = ~ScaledBz, type="scatter", mode="markers", 
                 name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
      add_trace(x =~model_center$NormalizedZ, y = ~model_center$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
      layout(title = 'Bz vs Z at 0 Radius')
    
    return(p)    
  } else {
    p <- plot_ly(data = data, x =~NormalizedZ, 
                 y = ~Bz, type="scatter", mode="markers", 
                 name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
      add_trace(x =~model_center$NormalizedZ, y = ~model_center$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
      layout(title = 'Bz vs Z at 0 Radius')
    
    return(p)    
  }
}

makeByplot <- function(data, model, scaled = F){
  if(scaled){
    p <- plot_ly(data = data, x =~NormalizedZ, 
                 y = ~ScaledBy, type="scatter", mode="markers", 
                 name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
      add_trace(x =~model_center$NormalizedZ, y = ~model_center$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
      layout(title = 'Bz vs Z at 0 Radius')
    
    return(p)    
  } else {
    p <- plot_ly(data = data, x =~NormalizedZ, 
                 y = ~NormalizedBy, type="scatter", mode="markers", 
                 name=~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" "), symbol = ~paste("Run# =", Run, "r = ", r, "Phi = ", theta, "pr = ", pr,sep=" ")) %>%
      add_trace(x =~model_center$NormalizedZ, y = ~model_center$Bz, name = 'Model', type="scatter", mode = 'lines', inherit=FALSE) %>%
      layout(title = 'Bz vs Z at 0 Radius')
    
    return(p)    
  }
}