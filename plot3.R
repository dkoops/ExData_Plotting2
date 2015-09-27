###############################
# Question 3.	Of the four types of sources indicated by the 
# type (point, nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 
# plotting system to make a plot answer this question.
#

# loadDataframe function
loadDataframe <- function(filename){
  if (!file.exists(filename)){
    stop(paste("The file", filename, "does not exist in the local directory."))
  }
  print( paste("Loading", filename, "data file."))
  inDataframe <- readRDS(filename)
}

# Load NEI PM2.5 data if not already loaded
if (!exists("PM25")){
  PM25 <- loadDataframe("summarySCC_PM25.rds")
}

# 
library(ggplot2)
library(reshape2)
PM25_Baltimore <- PM25[PM25$fips == "24510",]
PM25_Baltimore_Melt <- melt(PM25_Baltimore, id=c("fips","SCC","Pollutant","type","year"), measure.vars = c("Emissions") )
PM25BaltimoreTypeByYear <- dcast(PM25_Baltimore_Melt, type + year ~ variable, sum)
rm(PM25_Baltimore_Melt)

#png( file = "plot3.png", width = 480, height = 480)
# Line chart
g <- ggplot(PM25BaltimoreTypeByYear, aes( x = factor(year), y = Emissions, group = type, colour = type ))
g + geom_line(size = 1) +  
  geom_point(size = 3) +  
  xlab("type") +  
  ylab("PM2.5 Emission (tons)") +  
  ggtitle("Baltimore PM2.5 Emissions by Type")
dev.copy( png, file = "plot3.png", width = 480, height = 480)
dev.off()

# Cleanup
rm(PM25BaltimoreTypeByYear)
rm(PM25_Baltimore)