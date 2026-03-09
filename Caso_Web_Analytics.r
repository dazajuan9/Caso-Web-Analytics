# descargar librarias necesarias
library(readxl)
library(tidyverse)

# Cargar datos del excel
setwd("/Users/JuanEstebanDaza/Desktop/GitHub/Caso-Web-Analytics")
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

#PUNTO 1: DATA ANALISIS
# Grafico de visitas semanales 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Unique Visits`, group = 1)) +
  geom_col(fill = "steelblue", linewidth = 1) + labs (title = "Visitas Semanales a lo largo del tiempo", x = "Semana", y = "Número de Visitas") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################################
# Gráfico de Ingresos semanales 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Revenue`, group = 1)) +
  geom_col(fill = "purple", linewidth = 1) + labs (title = "Ingresos Semanales a lo largo del tiempo", x = "Semana", y = "Ingresos") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
####################################################################
# Gráfico de Beneficios semanales 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Profit`, group = 1)) +
  geom_col(fill = "green", linewidth = 1) + labs (title = "Beneficios Semanales a lo largo del tiempo", x = "Semana", y = "Beneficios") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
####################################################################
# Gráfico de Libras gastadas en publicidad semanalmente 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Lbs. Sold`, group = 1)) +
  geom_col(fill = "orange", linewidth = 1) +
  labs(title = "Libras vendidas a lo largo del tiempo", x = "Semana", y = "Libras vendidas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PUNTO 2: ESTADÍSTICAS DESCRIPTIVAS POR PERÍODO

tabla_estadisticas_descriptivas <- function(datos) {
  data.frame(
    row.names     = c("mean", "median", "std. dev.", "minimum", "maximum"),
    Visits        = c(round(mean(datos$Visits), 2),          round(median(datos$Visits), 2),
                      round(sd(datos$Visits), 2),            round(min(datos$Visits), 2),
                      round(max(datos$Visits), 2)),
    Unique.Visits = c(round(mean(datos$`Unique Visits`), 2), round(median(datos$`Unique Visits`), 2),
                      round(sd(datos$`Unique Visits`), 2),   round(min(datos$`Unique Visits`), 2),
                      round(max(datos$`Unique Visits`), 2)),
    Revenue       = c(round(mean(datos$Revenue), 2),         round(median(datos$Revenue), 2),
                      round(sd(datos$Revenue), 2),           round(min(datos$Revenue), 2),
                      round(max(datos$Revenue), 2)),
    Profit        = c(round(mean(datos$Profit), 2),          round(median(datos$Profit), 2),
                      round(sd(datos$Profit), 2),            round(min(datos$Profit), 2),
                      round(max(datos$Profit), 2)),
    Lbs.Sold      = c(round(mean(datos$`Lbs. Sold`), 2),     round(median(datos$`Lbs. Sold`), 2),
                      round(sd(datos$`Lbs. Sold`), 2),       round(min(datos$`Lbs. Sold`), 2),
                      round(max(datos$`Lbs. Sold`), 2))
  )
}

write.csv(tabla_estadisticas_descriptivas(initial),   "summary_initial.csv")
write.csv(tabla_estadisticas_descriptivas(prepromo),  "summary_prepromo.csv")
write.csv(tabla_estadisticas_descriptivas(promo),     "summary_promo.csv")
write.csv(tabla_estadisticas_descriptivas(postpromo), "summary_postpromo.csv")
