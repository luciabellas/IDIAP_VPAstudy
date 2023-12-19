# Cargar biblioteca necesarias
install.packages("MASS")  # Instala el paquete
library(MASS)  # Carga el paquete
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library (purrr)
library(ggplot2)

#añadimos columna tendencia post
prevVPA <- VPA %>%
  mutate(
    tendencia = row_number(),
    medida = ifelse(prevalence_start_date >= as.Date("2014-10-01"), 1, 0),
    tendencia_post = ifelse(prevalence_start_date >= as.Date("2014-10-01"), tendencia - 34, 0)
  )
#Ajustamos el modelo de regresión lineal
linear <- lm(n_cases ~ tendencia + medida + tendencia_post + 
                     offset(log(n_population)), 
                   data = prevVPA)

#1: Check the normality assumption.
qqnorm(residuals(linear),ylab="Residuals",main="")
qqline(residuals(linear))
hist(residuals(linear),xlab="Residuals",main="")
shapiro.test(residuals(linear))

#Ajustamos el modelo de regresión binomial negativa 
nb_model <- glm.nb(n_cases ~ tendencia + medida + tendencia_post + 
                  offset(log(n_population)), 
                   data = prevVPA)

# Resumen del modelo
summary(nb_model)

#MODELO DE POISSON 
poisson_model <- glm(n_cases ~ tendencia + medida + tendencia_post + 
                       offset(log(n_population)), 
                     family = poisson(link = "log"),
                     data = prevVPA)

summary(poisson_model)
# 
# 
# Calcular AIC (Criterio de informacion de Akaike)
AIC_nb <- AIC(nb_model)
# Calcular BIC
BIC_nb <- BIC(nb_model)
# Mostrar AIC y BIC
print(AIC_nb)
print(BIC_nb)

#Regresión con modelo de Poisson 
# Regresión binomial negativa ajustado llamado nb_model
# Calcular AIC (Criterio de informacion de Akaike)
AIC_p <- AIC(poisson_model)
# Calcular BIC
BIC_p <- BIC(poisson_model)
# Mostrar AIC y BIC
print(AIC_p)
print(BIC_p)

#Calculamos el pre_trend: 
# En este cálculo, se está calculando la prevalencia esperada antes de la medida (
# pre_trend). Se utiliza el modelo de regresión de Poisson (nb_model) para 
# calcular la expresión matemática de la prevalencia esperada. Se usa la fórmula de la regresión de Poisson:
# Prevalencia Esperada=exp( Intercepto+Tendencia×Tendencia_Pre)

#Pre_trend_exp: prevalencia esperada antes de la medida, 
# pero en este caso se considera el escenario post-medida
#ara extrapolar la tendencia y calcular la prevalencia esperada.

prevVPA <- prevVPA %>%
  mutate(
    pre_trend = exp(coef(poisson_model)["(Intercept)"] + tendencia * coef(poisson_model)["tendencia"]) * (medida == 0),
    pre_trend_extrap = exp(coef(poisson_model)["(Intercept)"] + tendencia * coef(poisson_model)["tendencia"]) * (medida == 1))

#Creamos columna de contrfactual 
prevVPA$contrafactual <- NA

# Rellena la columna "contrafactual" con los valores de "Prevalencia" cuando "medida" es igual a 0
prevVPA$contrafactual[prevVPA$medida == 0] <- prevVPA$prevalence[prevVPA$medida == 0]

# Rellena la columna "contrafactual" con los valores de "Pretrend" cuando "medida" es igual a 0
prevVPA$contrafactual[prevVPA$medida == 1] <- prevVPA$pre_trend_extrap[prevVPA$medida == 1]

# Preparamos dataframe para el gráfico
# Copiamos el dataframe original + seleccion y management de columnas
prevVPA <- prevVPA %>%
  select(prevalence_start_date, prevalence, medida, contrafactual, tendencia)
prevVPA$contrafactual <- as.numeric(prevVPA$contrafactual)
prevVPA$prevalence <- as.numeric(prevVPA$prevalence)
prevVPA$prevalence_start_date <- as.Date(prevVPA$prevalence_start_date)
##GGPLOT
ggplot(prevVPA, aes(x = prevalence_start_date)) +
  geom_vline(data = subset(prevVPA, medida == 1), aes(xintercept = prevalence_start_date),
             linetype = "dashed", color = "gray") +
  geom_point(aes(y = contrafactual, color = "Expected Prevalence"), size = 3) +
  geom_point(aes(y = prevalence, color = "Prevalence"), size = 3) +
  labs(title = "Interrupted Time Series Analysis",
       x = "Time points (months)",
       y = "Prevalence   | Expected Prevalence") +
  scale_color_manual(values = c("Expected Prevalence" = "#CD6600", "Prevalence" = "#5F9EA0")) +
  scale_y_continuous(limits = c(0, 0.01)) +
  theme_minimal()+
  theme(legend.position = "top",
        legend.text = element_text(size = 13, color = "black", face = "bold"),
        legend.title = element_text(size = 13, color = "#5F9EA0", face = "bold"),
        plot.title = element_text(size = 18, color = "#5F9EA0", face = "bold"),
        axis.title = element_text(size = 13, color = "#5F9EA0", face = "bold"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "#5F9EA0"),
        axis.text.y = element_text(color = "#5F9EA0", size = 10, hjust = 1)) +
  scale_x_date(labels = scales::date_format("%Y %B"),
               breaks = scales::breaks_pretty(n = 10)) # Adjust n to control the number of breaks


