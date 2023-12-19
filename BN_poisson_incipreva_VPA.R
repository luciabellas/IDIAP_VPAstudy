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

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -6.323870   0.084550 -74.794  < 2e-16 ***
# tendencia       0.007091   0.004277   1.658   0.0973 .  
# medida          0.080694   0.106217   0.760   0.4474    
# tendencia_post -0.027349   0.004833  -5.659 1.52e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Negative Binomial(446208.8) family taken to be 1)
# 
# Null deviance: 157.838  on 95  degrees of freedom
# Residual deviance:  42.004  on 92  degrees of freedom
# AIC: 465.82
# 
# Number of Fisher Scoring iterations: 1
# 
# 
# Theta:  446209 
# Std. Err.:  5723159 
# Warning while fitting theta: iteration limit reached 
# 
# 2 x log-likelihood:  -455.817 

#MODELO DE POISSON 
poisson_model <- glm(n_cases ~ tendencia + medida + tendencia_post + 
                       offset(log(n_population)), 
                     family = poisson(link = "log"),
                     data = prevVPA)

summary(poisson_model)
# 
# Call:
#   glm(formula = n_cases ~ tendencia + medida + tendencia_post + 
#         offset(log(n_population)), family = poisson(link = "log"), 
#       data = prevVPA)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -6.323870   0.084548 -74.796  < 2e-16 ***
#   tendencia       0.007091   0.004277   1.658   0.0973 .  
# medida          0.080694   0.106215   0.760   0.4474    
# tendencia_post -0.027349   0.004832  -5.659 1.52e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 157.842  on 95  degrees of freedom
# Residual deviance:  42.005  on 92  degrees of freedom
# AIC: 463.81
# 
# Number of Fisher Scoring iterations: 4

# Regresión binomial negativa ajustado llamado nb_model
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


