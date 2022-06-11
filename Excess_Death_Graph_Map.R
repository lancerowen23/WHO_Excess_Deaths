#EXCESS DEATH ANALYSIS BASED ON FINANCIAL TIMES DATASET
#Outputs: Horizontal Bar Graph and Map
#Author: Lance R. Owen
#Date: June 6, 2020 (Revised June 11, 2022)

library(pacman)
pacman::p_load(tidyverse, sf, viridis, patchwork, passport, geojsonio)

font <- "serif"

#import WHO data and concatenate/format date field for time-series plotting
URL.who <- "https://raw.githubusercontent.com/lancerowen23/WHO_Excess_Deaths/main/Excess_Deaths_WHO_MonthYear_3_25_22.csv"
df.who.1 <- read.csv(URL.who, fileEncoding="UTF-8-BOM", header = TRUE, stringsAsFactors = FALSE)
df.who.1$date <- as.Date(paste0(df.who.1$month, "-01-", df.who.1$year), format = "%m-%d-%Y")

#use passport package to parse iso3codes to avoid some of the cumbersome versions used by WHO
df.who.1$country <- as_country_name(df.who.1$iso3,
                                    to = "en",
                                    from = "iso3c",
                                    short = TRUE,
                                    variant = TRUE)

#check for NAs to verify all country names were converted
table(is.na(df.who.1$country))

#calculate the excess deaths as percentage of expected deaths (using mean values)
df.who.1 <- df.who.1 %>% 
  mutate(excess_deaths_percent = round(100*excess.mean/expected.mean, 2))
View(df.who.1)

#filter to highest excess_death_percent from duration of pandemic and order
df.who.overall <- df.who.1 %>%
  filter(date > "2020-01-01") %>% 
  group_by(country) %>% 
  filter(excess_deaths_percent == max(excess_deaths_percent)) %>% 
  arrange(desc(excess_deaths_percent))
View(df.who.overall)

#view breakdown of months with highest excess death percentages
table(df.who.overall$date)

#create bar graph
top25 <- ggplot(head(df.who.overall, n=25)) +
  geom_bar(aes(x=excess_deaths_percent, 
               y = reorder(country, excess_deaths_percent)), 
           stat='identity',
           col="#FFFFFF",
           fill="#6CA79F", 
           alpha=0.5, 
           width=.75, 
           size=0.10) +
  geom_text(aes(x=excess_deaths_percent, 
                y = reorder(country, excess_deaths_percent), 
                label = paste0(round(excess_deaths_percent, 0),"% (", format(date, format = "%b %Y"), ")"),
                hjust = -.25),
            family = font,
            color = "grey30") +
  theme(plot.title = element_text(family = font, face = "bold", size = 16, hjust = 0, color = "grey35"),
        plot.subtitle = element_text(family = font, face = "italic", size = 12, hjust = 0, color = "grey35"),
        panel.grid.major.x = element_line(color="grey85"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.caption = element_text(family = font, size = 12, color = "grey40"),
        axis.line.x = element_line(color="grey65"),
        axis.line.y = element_line(color="grey65"),
        axis.title.y = element_text(family = font, size = 16, color = "grey40", face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(family = font, size = 16, color = "grey40", face="bold",margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(family = font, size = 14, color = "grey40", face="bold"),
        axis.text.y = element_text(family = font, size = 14, color = "grey40", face="plain"),
        axis.ticks = element_line(color = "grey65")
  ) +
  scale_y_discrete() +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 375)) +
  labs(title = "GLOBAL COVID-19 PANDEMIC | Monthly Excess Deaths as Percent of Monthly Expected Deaths",
       subtitle = "By Country at Month of Highest Percentage of Excess Deaths During Pandemic", 
       y = "",
       x = "",
       caption = "Source: WHO | Data as of 10 June 2022")

top25

#create map
world <- topojson_read('https://raw.githubusercontent.com/lancerowen23/WHO_Excess_Deaths/main/ne_110m_admin_0_countries.json') 

#set crs and reproject into Robinson
world <-  st_set_crs(world, "EPSG:4326")
world.robinson <-st_transform(world, crs = 'ESRI:54030') %>% 
  filter(!ADM0_A3 == "ATA" & !ADM0_A3 == 'FJI') %>% 
  dplyr::select(ADMIN, ADM0_A3, geometry)

#correct iso3 codes in df.who.overall df to ensure correct join for South Sudan and Western Sahara
df.who.overall[df.who.overall$country=="South Sudan", "iso3"] <- "SDS"
df.who.overall[df.who.overall$country=="Western Sahara", "iso3"] <- "SAH"

#join data to sf variable
map.data <- merge(world.robinson, df.who.overall, by.x = "ADM0_A3", by.y = "iso3")

#set breaks for legend
cuts <- c(-50, 0, 50, 100, 150, 200, 250, 300)

#create map
highest.percent.map <- ggplot(map.data) + 
  geom_sf(aes(geometry = geometry, 
              fill = excess_deaths_percent), 
          size = 0.05, 
          color = "grey80") + 
  scale_fill_continuous(breaks = cuts,
                        guide = guide_colourbar(reverse = FALSE),
                        type = "viridis",
                        na.value = "grey40") +
  theme(plot.title = element_text(family = font, face = "bold", size = 16, hjust = 0, color = "grey35"),
        plot.subtitle = element_text(family = font, face = "italic", size = 12, hjust = 0, color = "grey35"),
        plot.caption = element_text(family = font, size = 12, color = "grey40"), 
        plot.caption.position =  "plot",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.text = element_text(size=10, family = font, color = "grey30"),
        legend.title = element_text(size=12, family = font, color = "grey30"),
        legend.position = c(0.02, 0.00),  
        legend.justification = c("left", "bottom"),  
        legend.box.just = "left",  
        legend.margin = margin(3,3,3,3), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "GLOBAL COVID-19 PANDEMIC | Monthly Excess Deaths as Percent of Monthly Expected Deaths",
       subtitle = "By Country at Month of Highest Percentage of Excess Deaths During Pandemic Since March 2020",
       caption = "Source: WHO | Data as of 10 June 2022",
       fill = "Excess Deaths as \nPercentage of \nExpected Deaths \nat Highest Monthly Rate")

highest.percent.map 