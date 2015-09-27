###############################
# Question 1.	Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
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

# Sum all PM2.5 data by year
library(reshape2)
PM25Melt <- melt(PM25, id=c("fips","SCC","Pollutant","type","year"), measure.vars = c("Emissions") )
PM25TotalByYear <- dcast(PM25Melt, year ~ variable, sum)
rm(PM25Melt)

# Create Barplot
png( file = "plot1.png", width = 480, height = 480)
ylim <- ceiling(max(PM25TotalByYear$Emissions/1000000))
xcoord <- barplot( PM25TotalByYear$Emissions/1000000, 
         names.arg = PM25TotalByYear$year,
         xlab = "Year",
         ylab = "Emmisions (Million tons)",
         main = "Total PM2.5 Emissions by Year",
         ylim = c(0, ylim))
text(x = xcoord, y = PM25TotalByYear$Emissions/1000000, label = round(PM25TotalByYear$Emissions/1000000, 4), pos = 3, cex = 0.8, col = "black")

dev.off()

# Cleanup
rm(PM25TotalByYear)
rm(ylim)
