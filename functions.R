reflectHorizontal <- function(R, z){
  return (-1 * as.numeric(z) + 2 * as.numeric(R))
}

chisq <- function(data){
  R2 <- 0;
  apply_chifunction <- function(f, e){
    #print(f) 
    #print(e)
    #print(((as.numeric(f) - as.numeric(e)) ^ 2) / as.numeric(e))
    R2 = R2 + ((as.numeric(f) - as.numeric(e)) ^ 2) / as.numeric(e)
    return(((as.numeric(f) - as.numeric(e)) ^ 2) / as.numeric(e))
  }
  data$chi <- apply(data, 1, function(x){apply_chifunction(data$f, data$e)})
  return(data)
}

getCenterReflectedChiSQ <- function(data){
  data <- data[order(data$z),] 
  data_reflected <- data
  data_reflected <- data_reflected[order(data_reflected$z),] 
  
  data$chi2 <-apply(data, 1, function(x){
    data_reflected$z <- apply(data, 1, function(y){
      reflectHorizontal(x["z"], y["z"])
    })
    data <- data[order(data$z),]
    data_reflected <- data_reflected[order(data_reflected$z),] 
    d <- merge(data, data_reflected, by=c("z", "Sector", "Run", "Hole"), all = TRUE)
    
    # omit over lap
    #print(head(d))
    #print(d)
    d <- na.omit(d)
    #print(head(d))
    #d <- subset(d, z != x["z"])
    #print(d)
    if(nrow(d) < nrow(data) / 4){
      #print(nrow(d))
      #print(nrow(data))
      return(99999999)
    }
    
    d <- data.frame(d$Bz.x, d$Bz.y)
    colnames(d) <- c("f", "e")
    d <- chisq(d)
    return(sum(d$chi))
  })
  
  
  
  #get the offset with the min chi2
  #print(head(data))
  mindata <- subset(data, chi2 == min(data$chi2))
  return(mindata)
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
