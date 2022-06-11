#EXCESS DEATHS TIME SERIES GRAPHING FUNCTION USING WHO DATASET
#Author: Lance R. Owen
#Date: June 6, 2020

library(pacman)
pacman::p_load(tidyverse, sf, viridis, patchwork, passport, geojsonio)

font <- "serif"

#excess death time series function
excess_death_time_series <- function(start_date, 
                                     end_date, 
                                     countries, 
                                     region_name) {
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
  
  #calculate the excess deaths as percentage of expected deaths (using mean values)
  df.who.1 <- df.who.1 %>% 
    mutate(excess_deaths_percent = round(100*excess.mean/expected.mean, 2))
  
  df.countries <- df.who.1 %>% 
    filter(country %in% countries) %>% 
    group_by(country) %>% 
    filter(date > start_date & date < end_date)
  
  #get max and min values of y variable to set expanded graph limits based on data
  lim_min <- 1.1*min(df.countries$excess_deaths_percent)
  lim_max <- 1.1*max(df.countries$excess_deaths_percent)
  
  plot <- ggplot(df.countries, aes(date, excess_deaths_percent, color = country)) +
    geom_line(size = 1.25, alpha = .8) +
    scale_color_viridis(discrete = TRUE, option = "D") +
    theme(rect = element_rect(color = "white"),
          plot.subtitle = element_text(family = font, face = "bold", size = 20, hjust = 0.5, color = "grey35"),
          panel.grid = element_line(color="grey85"),
          panel.background = element_rect(fill = "white"),
          legend.text = element_text(family = font, size = 14, color = "grey40", face="plain"),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "white"),
          legend.position = "right",
          axis.line.x = element_line(color="grey65"),
          axis.line.y = element_line(color="grey65"),
          axis.title.y = element_text(family = font, size = 14, color = "grey40", face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.text.x = element_text(family = font, size = 12, color = "grey40", face="bold"),
          axis.text.y = element_text(family = font, size = 14, color = "grey40", face="bold"),
          axis.ticks = element_line(color = "grey65"),
          plot.margin = margin(t = 10, r = 40, b = 10, l = 10)
    ) +
    scale_x_date(date_labels = "%b %Y", 
                 limits = as.Date(c(start_date, end_date)), 
                 expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(lim_min, 
                                  lim_max), 
                       labels = function(x) paste0(x, "%")) +
    xlab("") +
    ggtitle("",
            subtitle = region_name) +
    labs(x = "", y = "Excess Monthly Deaths\nas Percentage of Expected Monthly Deaths", caption = "") +
    geom_hline(yintercept=0, 
               linetype="dashed", 
               color = "grey45",
               size = 1)
  
  return(plot)
}
### FUNCTION STRUCTURE ###
### excess_death_time_series(start_date, end_date, countries, region_name) ###

#start and end dates must be in %Y-%m-%d format; so February 1, 2020 would be entered as "2020-02-01"
#countries should be entered as a list
#region invites the user to enter a name for the group of countries that will be the title for the plot

#example of function run with three groups of countries and output as horizontal and vertical triptychs using patchwork package

#horizontal triptych
sa <- excess_death_time_series("2020-02-01", "2021-04-28", c("Ecuador", "Chile", "Peru", "Brazil"), "SOUTH AMERICA")
eur <- excess_death_time_series("2020-02-01", "2021-04-28", c("Italy", "Spain", "France", "Germany", "Belgium"), "CONTINENTAL EUROPE")
scan <- excess_death_time_series("2020-02-01", "2021-04-28", c("Norway", "Sweden", "Denmark", "Iceland"), "SCANDINAVIA")

sa + eur + scan

#vertical triptych
seasia <- excess_death_time_series("2021-02-01", "2021-12-28", c("Malaysia", "Laos", "Thailand", "Vietnam"), "SOUTHEAST ASIA")
mideast <- excess_death_time_series("2021-02-01", "2021-12-28", c("Kuwait", "Jordan", "Saudi Arabia", "Iraq", "Israel"), "MIDDLE EAST")
northam <- excess_death_time_series("2021-02-01", "2021-12-28", c("US", "Canada", "Mexico", "Jamaica"), "NORTH AMERICA")

seasia / mideast / northam


