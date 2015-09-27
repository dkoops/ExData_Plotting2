###############################
# Question 4.	Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999-2008?
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

SCC_Coal <- SCC[grep("^Fuel Comb.*Coal*",SCC$EI.Sector),]
PM25_SCC <- merge(PM25, SCC_Coal, by.x = "SCC", by.y = "SCC", all.y = TRUE)
PM25_SCC$EI.Sector.Title <- sub( " - Coal", "", sub( "Fuel Comb - ", "", PM25_SCC$EI.Sector))

# Bar chart
PM25_Coal <- aggregate(Emissions ~ year + EI.Sector.Title, data = PM25_SCC, sum)
g <- ggplot(PM25_Coal, aes( x = factor(year), y = Emissions/1000 )) #+ scale_y_sqrt()
g + geom_bar(stat = "identity") + 
  scale_y_sqrt() +
  facet_grid(.~EI.Sector.Title) +
  xlab("year") + 
  ylab("PM2.5 Emission (K tons)") + 
  ggtitle("Total Combustible Coal PM2.5 Emissions\nby Sector") 
dev.copy( png, file = "plot4.png", width = 480, height = 480)
dev.off()

# Cleanup
rm(SCC_Coal)
rm(PM25_SCC)
rm(PM25_Coal)
rm(SCC)

