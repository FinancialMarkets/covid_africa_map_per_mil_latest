library(highcharter)
library(tidyverse)
library(htmlwidgets)

mapdata <- get_data_from_map(download_map_data("custom/africa"))

options(browser = "/usr/bin/firefox")

data <- read_csv("../african_latest_data.csv")
data$dateRep <- as.Date(data$dateRep, format="%d/%m/%Y")
data$popData2018 <- data$popData2018 / 1000000
data$cases_per_million <- data$cumulative_cases / data$popData2018

data_graphic <- data[, c("countryterritoryCode", "geoId", "dateRep", "countriesAndTerritories", "cases_per_million", "cumulative_cases", "cases", "deaths", "cumulative_deaths")]

## remove france

data_graphic <- subset(data_graphic, countriesAndTerritories != "France")
data_graphic <- subset(data_graphic, countriesAndTerritories != "Seychelles")
# data_graphic <- subset(data_graphic, countriesAndTerritories != "Mauritius")

## rename
data_graphic$`iso-a3` <- data_graphic$countryterritoryCode

## get latest day
latest_day <- max(unique(data_graphic$dateRep))

data_graphic_latest <- subset(data_graphic, dateRep == latest_day) 
## merge in WB data

wb_data <- read_csv("../WB_data/wb_data.csv")
wb_data$geoId <- wb_data$`hc-key`

data_graphic_latest <- merge(data_graphic_latest, wb_data, by = "geoId", all.x = TRUE)

wb_data$countriesAndTerritories <- wb_data$country_

## now merge Namibia
data_graphic_latest <- merge(data_graphic_latest, wb_data, by = "countriesAndTerritories", all.x = TRUE)


data_graphic_latest$per_pop_65 <-  data_graphic_latest$SP.POP.65UP.TO.ZS.x
data_graphic_latest$pop_urban <- data_graphic_latest$SP.URB.TOTL.IN.ZS.x
data_graphic_latest$health_exp_per_cap <- data_graphic_latest$SH.XPD.CHEX.PC.CD.x
data_graphic_latest$int_ext_debt <- data_graphic_latest$DT.INT.DECT.EX.ZS.x

## graphic

x <- c("Country", "New Cases Latest Day", "Total Number of Cases", "Number Cases Per Million", "Number of Deaths Latest Day", "Total Deaths")#, "Percent Pop. Over 65", "Percent Pop Urban", "Health Expenditure Per. Cap.", "Int Payment on External Debt (% exports)")
y <- c( "{point.countriesAndTerritories}" , "{point.cases}", "{point.cumulative_cases}", sprintf("{point.%s:.2f}", c("cases_per_million")), "{point.deaths}", "{point.cumulative_deaths}")#))#, "per_pop_65", "pop_urban", "health_exp_per_cap", "int_ext_debt")))

tltip <- tooltip_table(x, y)

carmine <- "#960018"
dark_midnight_blue <- "#003366"
white <- "#FFFFFF"
milken <- "#0066CC"

## map cases per of pop
map_cases_per_mil <- hcmap("custom/africa", data = data_graphic_latest, value = "cases_per_million",
      joinBy = c("iso-a3"), name = "Confirmed Cases per Million",
      #dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1) %>%
     # tooltip = list(valueDecimals = 2, valuePrefix = "", valueSuffix = "")) %>%
      hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) %>%
    hc_legend(align = "center", layout = "horizontal", verticalAlign = "middle", x = -160, y= 120, valueDecimals = 0) %>%
    hc_colorAxis(minColor = "#FFFFFF", maxColor = milken,
             type = "logarithmic")
  #    hc_legend(align = 'left') %>%
#      hc_add_theme(hc_theme_google())
map_cases_per_mil



## Save vis
saveWidget(map_cases_per_mil, file="map_cases_per_mil.html")



## map to create: per capita health expenditure per case (in hundred millions): the higher it is the better the country is able to care for the cases---assuming more spending means more readiness
