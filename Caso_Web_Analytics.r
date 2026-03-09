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
combined_data$Period <- NA
combined_data$Period[1:14]  <- "Initial"
combined_data$Period[15:35] <- "Pre-Promotion"
combined_data$Period[36:52] <- "Promotion"
combined_data$Period[53:66] <- "Post-Promotion"

initial   <- combined_data[combined_data$Period == "Initial", ]
prepromo  <- combined_data[combined_data$Period == "Pre-Promotion", ]
promo     <- combined_data[combined_data$Period == "Promotion", ]
postpromo <- combined_data[combined_data$Period == "Post-Promotion", ]

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
  )}

write.csv(tabla_estadisticas_descriptivas(initial),   "summary_initial.csv")
write.csv(tabla_estadisticas_descriptivas(prepromo),  "summary_prepromo.csv")
write.csv(tabla_estadisticas_descriptivas(promo),     "summary_promo.csv")
write.csv(tabla_estadisticas_descriptivas(postpromo), "summary_postpromo.csv")

#PUNTO 3: MEDIAS POR PERIODO

means_table <- data.frame(
  Period = c("Initial", "Pre-Promo", "Promotion", "Post-Promo"),
  Visits = c(mean(initial$Visits), mean(prepromo$Visits), mean(promo$Visits), mean(postpromo$Visits)),
  Unique_Visits = c(mean(initial$`Unique Visits`), mean(prepromo$`Unique Visits`),
                    mean(promo$`Unique Visits`), mean(postpromo$`Unique Visits`)),
  Revenue = c(mean(initial$Revenue), mean(prepromo$Revenue),
              mean(promo$Revenue), mean(postpromo$Revenue)),
  Profit = c(mean(initial$Profit), mean(prepromo$Profit),
             mean(promo$Profit), mean(postpromo$Profit)),
  Lbs_Sold = c(mean(initial$`Lbs. Sold`), mean(prepromo$`Lbs. Sold`),
               mean(promo$`Lbs. Sold`), mean(postpromo$`Lbs. Sold`))
)

print(means_table)

#Guardar la tabla de medias por periodo en un archivo CSV
write.csv(means_table, "summary_means_by_period.csv", row.names = FALSE)

#Grafica means visits by period
ggplot(means_table, aes(x = Period, y = Visits)) +
  geom_col(fill = "steelblue") +
  labs(title = "Mean Visits by Period",
       x = "Period",
       y = "Mean Visits") +
  theme_minimal()

#Grafica mean unique visits by period
ggplot(means_table, aes(x = Period, y = Unique_Visits)) +
  geom_col(fill = "steelblue") +
  labs(title = "Mean Visits by Period",
       x = "Period",
       y = "Mean Visits") +
  theme_minimal()

#Grafica mean revenue by period
ggplot(means_table, aes(x = Period, y = Revenue)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Mean Revenue by Period",
       x = "Period",
       y = "Mean Revenue") +
  theme_minimal()

#Grafica mean profit by period
ggplot(means_table, aes(x = Period, y = Profit)) +
  geom_col(fill = "purple") +
  labs(title = "Mean Profit by Period",
       x = "Period",
       y = "Mean Profit") +
  theme_minimal()

#Grafica mean lbs sold by period
ggplot(means_table, aes(x = Period, y = Lbs_Sold)) +
  geom_col(fill = "orange") +
  labs(title = "Mean Lbs Sold by Period", x = "Period", y = "Mean Lbs Sold") +
  theme_minimal()

#Pregunta 5 

#Sactter Plot Revenue vs Lbs Sold
library(ggplot2)

ggplot(combined_data, aes(x = `Lbs. Sold`, y = Revenue)) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Revenue vs Pounds Sold",
    x = "Pounds Sold",
    y = "Revenue"
  ) +
  theme_minimal()

#Correlacion
cor(combined_data$Revenue, combined_data$`Lbs. Sold`, use = "complete.obs")


