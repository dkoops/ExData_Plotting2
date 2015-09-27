###############################
# Question 6.	Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time in 
# motor vehicle emissions?
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

# Load NEI SCC data if not already loaded
if (!exists("SCC")){
  SCC <- loadDataframe("Source_Classification_Code.rds")
}

PM25_BaltimoreLA <- PM25[PM25$fips == "24510" | PM25$fips == "06037" ,]
PM25_BaltimoreLA$fipsdesc <- ifelse(PM25_BaltimoreLA$fips== "24510", "Balitimore City", "Los Angeles County")

PM25_BaltLA_SCC <- merge(PM25_BaltimoreLA, SCC, by.x = "SCC", by.y = "SCC", all.x = TRUE)
PM25_BaltLA_SCC_Vehicle <- PM25_BaltLA_SCC[grep("*Vehicles*",PM25_BaltLA_SCC$EI.Sector),]

PM25_BaltLA_SCC_Vehicle$EI.Sector.Title <- sub( "Mobile - On-Road ", "", PM25_BaltLA_SCC_Vehicle$EI.Sector)

# Boxplot by fips by year
g <- ggplot(PM25_BaltLA_SCC_Vehicle, aes( factor(year), log10(Emissions)))
g + geom_boxplot() + 
  facet_grid( . ~ fipsdesc) + 
  xlab("year") + 
  ylab("log10 Emission") + 
  ggtitle("Total PM2.5 Vehicle Emissions\nby year")

dev.copy( png, file = "plot6.png", width = 480, height = 480)
dev.off()

# Cleanup
rm(PM25_BaltLA_SCC)
rm(PM25_BaltimoreLA)
rm(PM25_BaltLA_SCC_Vehicle)
rm(SCC)
