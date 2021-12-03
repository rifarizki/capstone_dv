library(shinydashboard)
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(glue)
library(DT) # to read data tables
library(rsconnect)
library(dashboardthemes)
library(readr)

# read data

suicide <- read_csv("Suicide Rates Overview 1985 to 2016.csv")

suicide <- suicide %>% 
  mutate_at(.vars = c("country","sex", "age", "generation"), .funs=as.factor) %>% 
  mutate(`gdp_per_capita ($)` = as.numeric(`gdp_per_capita ($)`),
         year = as.character(year),
         year = as.Date(year, "%Y"),
         year = year(year))

suicide <- subset(suicide, select = -c(`HDI for year`))

decade <- suicide %>% 
  filter(year >= 1995,
         suicide$year <= 2015)

mapping <- read.csv("country code update.csv")

decade2 <- decade %>% 
  inner_join(mapping, decade, by = "country") %>% #inner_join(data1, data2, by = "ID")
  select(country, year, suicides_no, population, Code)

single_country <- decade %>% 
  filter(country == "Lithuania") %>% 
  group_by(year) %>% 
  summarise(Total.suicides = sum(suicides_no),
            Population = sum(population),
            Ratio = (Total.suicides / Population)*100)

country_rat <- decade %>% 
  group_by(country) %>% 
  summarise(total_suicides = sum(suicides_no),
            pop = sum(population)) %>% 
  mutate(ratio = (total_suicides / pop)*100) %>% 
  arrange(desc(ratio)) %>% 
  top_n(20)