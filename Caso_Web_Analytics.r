# descargar librarias necesarias
library(readxl)
library(tidyverse)

# Cargar datos del excel
setwd("/Users/maru/Desktop/Javeriana/4/Analitica de los negocios/Caso-Web-Analytics")
data <- read_excel('Data_Web_Analytics.xls')

# Creacion de datasets
weekly_visits <- read_excel('Data_Web_Analytics.xls', sheet = 'Weekly Visits', skip = 4)
financials <- read_excel('Data_Web_Analytics.xls', sheet = 'Financials', skip = 4)

# Combinar datasets
combined_data <- left_join(weekly_visits, financials, by = "Week (2008-2009)")

#Revisar los datos combinados
head(combined_data)

# Visualizar la estructura de los datos
str(combined_data)
