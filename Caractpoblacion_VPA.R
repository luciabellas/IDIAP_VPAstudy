install.packages ('lubridate')
install.packages ('dplyr')
install.packages('devtools')
install.packages('Desc')
install.packages("epitools")
install.packages("exact2x2")
install.packages(binom)
library(exact2x2)
library (dplyr)
library(stringr)
library (lubridate)
library(stats)
library(epitools)
library(binom)
#se añaden columnas que calculan fechas 6 meses antes, 1 mes antes 
#columna 1 mes despues + 6 meses después 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(id_emb = row_number())

# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb, dinici, dfi)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many") 


# Seleccionar las columnas de interés
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb, id_emb, dinici, dfi, durada)

# Calcular el denominador de prevalencia para embarazadas
denominadorEMB <- prescrip_drugs %>%
  mutate(deno_embarazo = if_else(between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
                                   between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0))

# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
#episodio de embarazo 
intervalo_emb <- interval(prescrip_drugs$dinici, prescrip_drugs$dfi)
#intervalo de prescripcion va desde inincio dat inici a dbaixa 
intervalo_prescripcion <- interval(denominadorEMB$dat, denominadorEMB$dbaixa)
#códigos de embarazo a los que se han prescrito farmacos del grupo N 

# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradoremb  <- denominadorEMB %>%
  mutate(
    expo_EMB = ifelse(int_overlaps(intervalo_emb, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_EMB = expo_EMB * deno_embarazo
  )

numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 7)) %>%
  filter(grupo_N %in% paste0("N03AG01")) %>%
  filter(expo_EMB == 1)%>%  
  distinct(id_emb, .keep_all = T)%>%
  select(idp, id_emb, cod, ctanca, durada, dinici, dfi)

n <- nrow(numeradoremb)

#Codi tancament 
table(numeradoremb$ctanca)
ctanca.tabla <- table(numeradoremb$ctanca)
prop_coditancament <- prop.table(ctanca.tabla)
prop_coditancament


#Codi durada 
summary(numeradoremb$durada)


#Intervalos de confianza 
# Datos de las proporciones
prop_part <- 0.54304636
prop_cesaria <- 0.16225166
prop_prematuritat <-0.00
prop_avortament <- 0.25165563
prop_ive  <- 0.04304636
prop_mf  <- 0.00
prop_emb_ect  <- 0.000
prop_mh  <- 0.000

# Desviación estándar para cada proporción
se_part <- sqrt(prop_part * (1 - prop_part) / n)
se_cesaria <- sqrt(prop_cesaria * (1 - prop_cesaria) / n)
se_prematuritat <- sqrt(prop_prematuritat * (1 - prop_prematuritat) / n)
se_avortament <- sqrt(prop_avortament * (1 - prop_avortament) / n)
se_ive <- sqrt(prop_ive * (1 - prop_ive) / n)
se_mf <- se_ive <- sqrt(prop_mf* (1 - prop_mf) / n)
se_emb_ect <- sqrt(prop_emb_ect * (1 - prop_emb_ect) / n)
se_mh <- se_mh <- sqrt(prop_mh * (1 - prop_mh) / n)

# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_part <- c(prop_part - z * se_part, prop_part + z * se_part)*100
ci_cesaria <- c(prop_cesaria - z * se_cesaria, prop_cesaria + z * se_cesaria)*100
ci_prematuritat <- c(prop_prematuritat - z * se_prematuritat, prop_prematuritat + z * se_prematuritat)*100
ci_avortament <- c(prop_avortament - z * se_avortament, prop_avortament + z * se_avortament)*100
ci_ive <- c(prop_ive - z * se_ive, prop_ive + z * se_ive)*100
ci_mf <- c(prop_mf - z * se_mf, prop_mf+ z * se_mf)*100
ci_emb_ect <- c(prop_emb_ect - z * se_emb_ect, prop_emb_ect + z * se_emb_ect)*100
ci_mh <-c(prop_mh - z * se_mh, prop_mh + z * se_mh)*100

# Datos de la variable "durada"
durada <- numeradoremb$durada

# Media y mediana de la variable "durada"
media_durada <- mean(durada)
mediana_durada <- median(durada)

# Desviación estándar de la variable "durada"
desviacion_durada <- sd(durada)

# Calcular el intervalo de confianza para la media de "durada"
se_media_durada <- desviacion_durada / sqrt(n)
ci_media_durada <- c(media_durada - z * se_media_durada, media_durada + z * se_media_durada)

# Resumen del intervalo de confianza para la media de "durada"
media_durada_ci <- data.frame(
  "Media Durada" = media_durada,
  "Límite Inferior" = ci_media_durada[1],
  "Límite Superior" = ci_media_durada[2]
)

print(media_durada_ci)

#TABAQUISMO
tabaquismo <- numeradoremb %>%
  left_join(`tabaquisme_LuciaVH_N_2023-06-09`, relationship = "many-to-many") %>%
  mutate(tab = if_else(dat >= (dinici - years(1)), val, NA))%>%  
  group_by(idp, id_emb) %>%
  summarise(across(c("tab"), max)) 

#Proporcion e intervalos de confianza
tabla.tabaco <- table(tabaquismo$tab)
prop_0  <- 29/n
prop_1  <- 20/n
prop_2  <- 8/n

# Desviación estándar para cada proporción
se_0 <- sqrt(prop_0 * (1 - prop_0) / n)
se_1 <- sqrt(prop_1 * (1 - prop_1) / n)
se_2 <- sqrt(prop_2 * (1 - prop_2) / n)

# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_0 <- c(prop_0 - z * se_0, prop_0 + z * se_0)*100
ci_1 <- c(prop_1 - z * se_1, prop_1 + z * se_1)*100
ci_2 <- c(prop_2 - z * se_2, prop_2 + z * se_2)*100

#COMORBILIDADES 
num_comorb <- numeradoremb %>% 
  mutate (drug=cod)%>%
  select(idp, id_emb, drug, dinici, dfi)

CIE <- `diagnostics_LuciaVH_N_2023-06-09` %>% 
  mutate(dx=cod)%>% 
  select(idp, dat, dbaixa, agr, dx)

CIE <- num_comorb %>%
  left_join(CIE, relationship = "many-to-many") %>%
  mutate(Diagnost = if_else(dat >= (dinici - years(1)), dx, NA))

#N, proporciones
tabla.comorb <- table(CIE$dx)
tabla.comorb_df <- as.data.frame(tabla.comorb)
tabla.comorb_df <- tabla.comorb_df %>% 
  mutate(frecuencia_relativa=(Freq/n)*100)

#intervalos de confianza
prop_0  <- 79.13 /100
prop_1  <- 68.87/100
prop_2  <- 37.08/100
prop_3  <- 23.50/100
prop_4  <- 23.17/100
prop_5  <-  19.20/100
prop_6  <-  14.90/100
prop_7  <-  14.90/100

# Desviación estándar para cada proporción
se_0 <- sqrt(prop_0 * (1 - prop_0) / n)
se_1 <- sqrt(prop_1 * (1 - prop_1) / n)
se_2 <- sqrt(prop_2 * (1 - prop_2) / n)
se_3 <- sqrt(prop_3 * (1 - prop_3) / n)
se_4 <- sqrt(prop_4 * (1 - prop_4) / n)
se_5 <- sqrt(prop_5 * (1 - prop_5) / n)
se_6 <- sqrt(prop_6 * (1 - prop_6) / n)
se_7 <- sqrt(prop_7 * (1 - prop_7) / n)


# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_0 <- c(prop_0 - z * se_0, prop_0 + z * se_0)*100
ci_1 <- c(prop_1 - z * se_1, prop_1 + z * se_1)*100
ci_2 <- c(prop_2 - z * se_2, prop_2 + z * se_2)*100
ci_3 <- c(prop_3 - z * se_3, prop_3 + z * se_3)*100
ci_4 <- c(prop_4 - z * se_4, prop_4 + z * se_4)*100
ci_5 <- c(prop_5 - z * se_5, prop_5 + z * se_5)*100
ci_6 <- c(prop_6 - z * se_6, prop_6 + z * se_6)*100
ci_7 <- c(prop_7 - z * se_7, prop_7 + z * se_7)*100

#QMEDEA/RURALITAT 
qmedea <- numeradoremb %>%
  left_join(`variables_socioeconomiques_LuciaVH_N_2023-06-09`, relationship = "many-to-many")

#Proporcion e intervalos de confianza
tabla.qmedea <- table(qmedea$qmedea)
proporciones <- prop.table(tabla.qmedea)
proporciones
tabla.ruralidad <- table(qmedea$ruralitat)
tabla.ruralidad_df <- as.data.frame(tabla.ruralidad)
prop_rurarit <- prop.table(tabla.ruralidad)
prop_rurarit

#qemedea
prop_u1  <- 0.08278146
prop_u2  <- 0.10596026
prop_u3 <- 0.16887417 
prop_u4 <- 0.15231788 
prop_u5 <- 0.15231788 
#rural
prop_U <- 0.7682119
prop_R <- 0.2317881

# Tamaño de muestra
n <- nrow(numeradoremb)

# Desviación estándar para cada proporción
se_u1 <- sqrt(prop_u1 * (1 - prop_u1) / n)
se_u2 <- sqrt(prop_u2 * (1 - prop_u2) / n)
se_u3 <- sqrt(prop_u3 * (1 - prop_u3) / n)
se_u4 <- sqrt(prop_u4 * (1 - prop_u4) / n)
se_u5 <- sqrt(prop_u5 * (1 - prop_u5) / n)

se_R <- sqrt(prop_R * (1 - prop_R) / n)
se_U <- sqrt(prop_U * (1 - prop_U) / n)



# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_u1 <- c(prop_u1 - z * se_u1, prop_u1 + z * se_u1)*100
ci_u2 <- c(prop_u2 - z * se_u2, prop_u2 + z * se_u2)*100
ci_u3 <- c(prop_u3 - z * se_u3, prop_u3 + z * se_u3)*100
ci_u4 <- c(prop_u4 - z * se_u4, prop_u4 + z * se_u4)*100
ci_u5 <- c(prop_u5 - z * se_u5, prop_u5 + z * se_u5)*100
ci_U <- c(prop_U - z * se_U, prop_U + z * se_U)*100
ci_R <- c(prop_R - z * se_R, prop_R + z * se_R)*100

#Media y mediana edad madres en el episodio de embarazo 

edad <- numeradoremb %>%
  left_join(`poblacio_LuciaVH_N_2023-06-09`, relationship = "many-to-many") %>%
  mutate(edad = as.numeric(dinici-dnaix)/365)

# Media y mediana de la variable "durada"
media_edad <- mean(edad$edad)
media_edad
mediana_durada <- median(edad$edad)
mediana_durada
desviacion_edad <- sd(edad$edad) 

# Nivel de confianza (por ejemplo, 95%)
confianza <- 0.95

# Tamaño de la muestra (supongamos 23346)
n <- nrow(edad)

# Valor crítico de la distribución normal estándar para el nivel de confianza
z <- qnorm((1 + confianza) / 2)

# Error estándar de la media
se_media_edad <- desviacion_edad / sqrt(n)

# Calcular el intervalo de confianza para la media de "edad"
ci_media_edad <- c(media_edad - z * se_media_edad, media_edad + z * se_media_edad)
ci_media_edad
