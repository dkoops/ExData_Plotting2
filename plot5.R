###############################
# Question 5.	How have emissions from motor vehicle sources changed from 
# 1999-2008 in Baltimore City?
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

PM25_Baltimore <- PM25[PM25$fips == "24510",]
PM25_Balt_SCC <- merge(PM25_Baltimore, SCC, by.x = "SCC", by.y = "SCC", all.x = TRUE)
PM25_Balt_SCC_Vehicle <- PM25_Balt_SCC[grep("*Vehicles*",PM25_Balt_SCC$EI.Sector),]

PM25_Balt_SCC_Vehicle$EI.Sector.Title <- sub( "Mobile - On-Road ", "", PM25_Balt_SCC_Vehicle$EI.Sector)

# Side by side bar chart
PM25_Vehicle <- aggregate(Emissions ~ year + EI.Sector.Title, data = PM25_Balt_SCC_Vehicle, sum)
g <- ggplot(PM25_Vehicle, aes( x = factor(year), y = Emissions ))
g + geom_bar(stat = "identity") + 
  scale_y_sqrt() +
  facet_grid(.~EI.Sector.Title) +
  xlab("Sector") + 
  ylab("PM2.5 Emission (tons)") + 
  ggtitle("Total Baltimore PM2.5 Vehicle Emissions\nby Sector")

dev.copy( png, file = "plot5.png", width = 600, height = 480)
dev.off()

# Cleanup
rm(PM25_Balt_SCC_Vehicle)
rm(PM25_Balt_SCC)
rm(PM25_Baltimore)
rm(PM25_Vehicle)
