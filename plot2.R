library(dplyr)
library(ggplot2)
library(scales)

## Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


Emission_Maryland <- NEI %>%
  mutate(year = as.factor(year)) %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(Sum_Emission = sum(Emissions)) %>%
  data.frame()


# Plot the result
png("plot2.png",width = 620, height = 620)

par(mar = c(4, 4, 3, 2))
plot(Emission_Maryland$year, 
     Emission_Maryland$Sum_Emission,xlab = "Year",ylab = "Emission",
     main = "Total Emissions From PM2.5 Fluctuated in the Baltimore City, Maryland",las = 1,
     boxwex = 0.01,ylim = c(1600,4000),cex.axis = 1,cex.main = 1)

points(Emission_Maryland$year, Emission_Maryland$Sum_Emission, pch = 19)
lines(Emission_Maryland$year, Emission_Maryland$Sum_Emission)
text(Emission_Maryland$year, (Emission_Maryland$Sum_Emission) + 300, 
     labels = c("3274.180", "2453.916", "3091.354", "1862.282"), col = 4)

dev.off()