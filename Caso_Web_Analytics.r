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
  theme_minimal() + theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )
###################################################################
# Gráfico de Ingresos semanales 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Revenue`, group = 1)) +
  geom_col(fill = "purple", linewidth = 1) + labs (title = "Ingresos Semanales a lo largo del tiempo", x = "Semana", y = "Ingresos") +
  theme_minimal() + theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )
####################################################################
# Gráfico de Beneficios semanales 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Profit`, group = 1)) +
  geom_col(fill = "green", linewidth = 1) + labs (title = "Beneficios Semanales a lo largo del tiempo", x = "Semana", y = "Beneficios") +
  theme_minimal() + theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )
####################################################################
# Gráfico de Libras gastadas en publicidad semanalmente 
ggplot(combined_data, aes(x = `Week (2008-2009)`, y = `Lbs. Sold`, group = 1)) +
  geom_col(fill = "orange", linewidth = 1) +
  labs(title = "Libras vendidas a lo largo del tiempo", x = "Semana", y = "Libras vendidas") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

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
  geom_col(fill = "red") +
  labs(title = "Mean Unique Visits by Period",
       x = "Period",
       y = "Mean Unique Visits") +
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

#Punto 5

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

#Punto 6 

#Sactter Plot Revenue vs Visits 
ggplot(combined_data, aes(x = Visits, y = Revenue)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Revenue vs Visits",
    x = "Visits",
    y = "Revenue"
  ) +
  theme_minimal()

#Correlacion
cor(combined_data$Revenue, combined_data$Visits, use = "complete.obs")

#PUNTO 8A: Estadísticas resumen de Lbs. Sold
lbs_summary <- data.frame(
  Statistic = c("Mean", "Median", "Std Dev", "Minimum", "Maximum"),
  Value = c(
    mean(combined_data$`Lbs. Sold`),
    median(combined_data$`Lbs. Sold`),
    sd(combined_data$`Lbs. Sold`),
    min(combined_data$`Lbs. Sold`),
    max(combined_data$`Lbs. Sold`)
  )
)

print(lbs_summary)

write.csv(lbs_summary, "summary_lbs_sold.csv", row.names = FALSE)

#Punto 8B Histograma de Lbs. Sold
ggplot(combined_data, aes(x = `Lbs. Sold`)) +
  geom_histogram(
    bins = 10,
    fill = "purple",
    color = "black"
  ) +
  labs(
    title = "Histogram of Pounds of Material Sold",
    x = "Pounds Sold",
    y = "Frequency"
  ) +
  theme_minimal()

#Punto 8D: Analisis de la regla empirica 
# Calcular media, desviación estándar y número de observaciones
mean_lbs <- mean(combined_data$`Lbs. Sold`)
sd_lbs <- sd(combined_data$`Lbs. Sold`)
n <- nrow(combined_data)

# Calcular z-scores
combined_data$z_score <- (combined_data$`Lbs. Sold` - mean_lbs) / sd_lbs

# Observaciones reales dentro de cada intervalo
obs_1sd <- sum(abs(combined_data$z_score) <= 1)
obs_2sd <- sum(abs(combined_data$z_score) <= 2)
obs_3sd <- sum(abs(combined_data$z_score) <= 3)

# Observaciones teóricas según la regla empírica
theo_1sd <- 0.68 * n
theo_2sd <- 0.95 * n
theo_3sd <- 0.99 * n

# Crear tabla final
empirical_rule_table <- data.frame(
  Interval = c("mean ± 1 sd", "mean ± 2 sd", "mean ± 3 sd"),
  Theoretical_Percentage = c("68%", "95%", "99%"),
  Theoretical_Observations = c(theo_1sd, theo_2sd, theo_3sd),
  Actual_Observations = c(obs_1sd, obs_2sd, obs_3sd)
)

print(empirical_rule_table)

# Guardar resultados
write.csv(empirical_rule_table, "empirical_rule_results.csv", row.names = FALSE)

#Punto 8E: 

# Porcentajes teóricos por intervalo
theoretical_percent <- c(0.34, 0.34, 0.135, 0.135, 0.0235, 0.0235)

# Observaciones teóricas
theoretical_obs <- theoretical_percent * n

# Observaciones reales usando z-scores
actual_obs <- c(
  sum(combined_data$z_score > 0 & combined_data$z_score <= 1),
  sum(combined_data$z_score < 0 & combined_data$z_score >= -1),
  sum(combined_data$z_score > 1 & combined_data$z_score <= 2),
  sum(combined_data$z_score < -1 & combined_data$z_score >= -2),
  sum(combined_data$z_score > 2 & combined_data$z_score <= 3),
  sum(combined_data$z_score < -2 & combined_data$z_score >= -3)
)

# Crear tabla final
refined_empirical_table <- data.frame(
  Interval = c(
    "mean to +1 std. dev.",
    "mean to -1 std. dev.",
    "+1 std. dev. to +2 std. dev.",
    "-1 std. dev. to -2 std. dev.",
    "+2 std. dev. to +3 std. dev.",
    "-2 std. dev. to -3 std. dev."
  ),
  Theoretical_Percentage = theoretical_percent * 100,
  Theoretical_No_Obs = round(theoretical_obs,2),
  Actual_No_Obs = actual_obs
)

# Mostrar tabla
print(refined_empirical_table)

# Guardar CSV
write.csv(refined_empirical_table, "refined_empirical_rule_table.csv", row.names = FALSE)

#Punto 8G: 

# Vector de datos
x <- combined_data$`Lbs. Sold`

# Número de observaciones
n <- length(x)

# Media y desviación estándar
mean_x <- mean(x)
sd_x <- sd(x)

# Skewness
skewness_lbs <- sum((x - mean_x)^3) / ((n - 1) * sd_x^3)

# Kurtosis
kurtosis_lbs <- sum((x - mean_x)^4) / ((n - 1) * sd_x^4)

# Crear tabla
shape_statistics <- data.frame(
  Statistic = c("Skewness", "Kurtosis"),
  Value = c(skewness_lbs, kurtosis_lbs)
)

print(shape_statistics)

# Guardar CSV
write.csv(shape_statistics, "skewness_kurtosis_lbs_sold.csv", row.names = FALSE)

