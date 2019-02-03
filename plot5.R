library(dplyr)
library(ggplot2)
library(scales)

## Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


Mobile <- SCC %>%
  mutate(Match = ifelse(grepl("Mobile", EI.Sector, ignore.case = T),"Yes","NO")) %>%
  filter(Match == "Yes") %>%
  data.frame() %>%
  mutate(On_Road_type = ifelse(grepl("Diesel Heavy Duty Vehicles", EI.Sector, ignore.case = T),"1.Diesel_Heavy Duty",
                               ifelse(grepl("Diesel Light Duty Vehicles", EI.Sector, ignore.case = T),"2.Diesel_Light Duty",
                                      ifelse(grepl("Gasoline Heavy Duty Vehicles", EI.Sector, ignore.case = T),"3.Gasoline_Heavy Duty",
                                             ifelse(grepl("Gasoline Light Duty Vehicles", EI.Sector, ignore.case = T),"4.Gasoline_Light Duty",NA)))),
         SCC = as.character(SCC)) %>%
  filter(!is.na(On_Road_type)) %>%
  select(SCC, On_Road_type)


Motor_Related <- NEI[NEI$SCC %in% Mobile$SCC,]

Motor_Related <- Motor_Related %>%
  left_join(Mobile, by = "SCC")


# Calculate total emissions from PM2.5 between 1999 and 2008 in Baltimore City.
Emission <- Motor_Related %>%
  filter(fips == "24510") %>%
  mutate(year = as.factor(year)) %>%
  group_by(year, On_Road_type) %>%
  summarise(Sum_Emission = round(sum(Emissions))) %>%
  data.frame()

# Calculate total emissions from PM2.5 for each year of 1999, 2002, 2005 and 2008

Year_Emission <- Emission %>%
  group_by(year) %>%
  summarise(Sum_Emission = round(sum(Sum_Emission))) %>%
  data.frame()



png("plot5.png",width = 620, height = 620)

ggplot(Emission, aes(x = year, y = Sum_Emission, fill = On_Road_type, ymax = max(300))) +
  geom_bar(position="dodge", stat = "identity", width = 0.9) +
  scale_fill_brewer() +
  theme_bw() +
  geom_text(aes(label = Sum_Emission), vjust = -1, position = position_dodge(width = 0.9)) +
  ylab("Total Emissions from PM2.5") +
  xlab("Year") +
  ggtitle("Emissions from Motor Vehicle Sources in Baltimore City") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14)) +
  theme(axis.title.y = element_text(face = "bold", size=12)) +
  theme(axis.title.x = element_text(face = "bold", size=12)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(legend.position = "top") +
  annotate("text", x = "1999", y = 250, label="346",color = "darkblue") +
  annotate("text", x = "1999", y = 280, label="Total Emissions",color = "darkblue") +
  annotate("text", x = "2002", y = 250, label="134",color = "darkblue") +
  annotate("text", x = "2005", y = 250, label="130",color = "darkblue") +
  annotate("text", x = "2008", y = 250, label="89",color = "darkblue")

dev.off()
