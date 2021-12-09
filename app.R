library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(rapportools)
library(countrycode)
library(ggplot2)
library(highcharter)
library("viridisLite")
library(plotly)
library(data.table)
library('xml2')
library(shinyWidgets)
library(shinythemes)

#Link to scrap
link = "https://www.worldometers.info/coronavirus/"

#Read the link HTML
page = read_html(link)

#Scrap the different variables
country = page %>% html_nodes("td:nth-child(2)") %>% html_text()
total = page %>% html_nodes("td:nth-child(3)") %>% html_text()
new_cases = page %>% html_nodes("td:nth-child(4)") %>% html_text()
total_deaths = page %>% html_nodes("td:nth-child(5)") %>% html_text()
new_deaths = page %>% html_nodes("td:nth-child(6)") %>% html_text()
total_recovered = page %>% html_nodes("td:nth-child(7)") %>% html_text()
new_recovered = page %>%html_nodes("td:nth-child(8)") %>% html_text()
active_cases = page %>% html_nodes("td:nth-child(9)") %>% html_text()
critical_cases = page %>% html_nodes("td:nth-child(10)") %>% html_text()
population = page %>% html_nodes("td:nth-child(15)") %>% html_text()

#Create a data frame
df = data.frame(country, total, new_cases, total_deaths, new_deaths, total_recovered, new_recovered,
                active_cases, critical_cases, population)

#Filter out countries and store in a different data frame
df_country = df[-1: -7, ]
df_country = df_country[1: 225, ]

#Formatting
df_country[1] = sapply(df_country[1], function (x) {
  gsub("[\r\n]", "", x)
})

formatCases <- function (x) {
  x = gsub("[^0-9.-]", "", x)
  x = gsub("^\\s*$", 0, x)
  x = as.integer(x)
  return (x)
}

df_country[-1] %>% sapply(FUN = function (x) {
  sapply(x, FUN = function (x) {
    formatCases(x)
  })
}) -> df_country[-1]

df_country <-df_country[!(df_country$country == "CAR" | df_country$country == "World" | df_country$country == "Channel Islands" |
                             df_country$country == "Diamond Princess" | df_country$country == "Micronesia" | df_country$country == "MS Zaandam" |
                             df_country$country == "Saint Martin" | df_country$country == "St. Barth"), ]

df_country$iso3 = countrycode(df_country$country, origin = 'country.name', destination = 'iso3c')

ui <- fluidPage(theme = shinytheme("united"), tags$head(HTML("<title>Covid 19 Stats by Zahiriddin</title>")),
                
                titlePanel(
                  h2(style = "text-align: center;", strong("Covid-19 Stats as of ", format(Sys.Date(), format = "%d %B %Y")))
                ),
                
                  mainPanel(
                    uiOutput("casesMap"),
                    width = 12
                  ),
                
                absolutePanel(top = 69, right = 35,
                              pickerInput(inputId = "category",
                                          choices = c("Total Cases" = 1, "New Cases" = 2, "Total Deaths" = 3,
                                                      "New Deaths" = 4, "Total Recovered" = 5, "New Recovered" = 6,
                                                      "Active Cases" = 7, "Critical Cases" = 8, "Population" = 9)
                              ))
)

server <- function (input, output) {
  
  data(worldgeojson, package = "highcharter")
  dshmstops <- data.frame(q = c(0, exp(1: 5) / exp(5)),
                          c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>% list_parse2()
  
  output$casesMap <-renderUI({
    
    selectedCat = input$category
    if (selectedCat == 1) {
      title = "Total Cases"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(total), .groups = "drop")
    } else if (selectedCat == 2) {
      title = "New Cases"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(new_cases), .groups = "drop")
    } else if (selectedCat == 3) {
      title = "Total Deaths"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(total_deaths), .groups = "drop")
    } else if (selectedCat == 4) {
      title = "New Deaths"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(new_deaths), .groups = "drop")
    } else if (selectedCat == 5) {
      title = "Total Recovered"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(total_recovered), .groups = "drop")
    } else if (selectedCat == 6) {
      title = "New Recovered"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(new_recovered), .groups = "drop")
    } else if (selectedCat == 7) {
      title = "Active Cases"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(active_cases), .groups = "drop")
    } else if (selectedCat == 8) {
      title = "Critical Cases"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(critical_cases), .groups = "drop")
    } else {
      title = "Population"
      countries_or <-df_country %>% group_by(country, iso3) %>% summarise(Total = sum(population), .groups = "drop")
    }
    
    highchart() %>% #from highchart package
      hc_add_series_map(worldgeojson, df = countries_or,
                        value = "Total", joinBy = "iso3") %>%
      hc_colorAxis(stops = dshmstops) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_theme(hc_theme_db()) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_title(text = title, style = list(fontSize = "25px")) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE,
                 text = "Source: worldometers.info",
                 style = list(fontSize = "10px"))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)