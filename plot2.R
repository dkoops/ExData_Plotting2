###############################
# Question 2.	Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to 
# make a plot answering this question.
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

PM25_Baltimore <- PM25[PM25$fips == "24510",]

# Sum Total emmisions for Baltimore City (fips == 24510) by year
library(reshape2)
PM25_Baltimore_Melt <- melt(PM25_Baltimore, id=c("fips","SCC","Pollutant","type","year"), measure.vars = c("Emissions") )
PM25BaltimoreTotalByYear <- dcast(PM25_Baltimore_Melt, year ~ variable, sum)
rm(PM25_Baltimore_Melt)

# Create Barplot
png( file = "plot2.png", width = 480, height = 480)
ylim <- max(PM25BaltimoreTotalByYear$Emissions)+1000
xcoord <- barplot( PM25BaltimoreTotalByYear$Emissions, 
                   names.arg = PM25BaltimoreTotalByYear$year,
                   xlab = "Year",
                   ylab = "Emmisions (tons)",
                   main = "PM2.5 Baltimore City Emissions",
                   ylim = c(0, ylim)
                   )
text(x = xcoord, y = PM25BaltimoreTotalByYear$Emissions, label = round(PM25BaltimoreTotalByYear$Emissions, 4), pos = 3, cex = 0.8, col = "black")

dev.off()

# Cleanup
rm(PM25BaltimoreTotalByYear)
rm(PM25_Baltimore)
rm(ylim)

