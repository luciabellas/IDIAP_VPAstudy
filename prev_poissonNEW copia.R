install.packages ('lubridate')
install.packages ('dplyr')
install.packages('devtools')
install.packages('Desc')
install.packages("epitools")
install.packages("exact2x2")
library(exact2x2)
library (dplyr)
library(stringr)
library (lubridate)
library(stats)
library(epitools)
+#se añaden columnas que calculan fechas 6 meses antes, 1 mes antes 
#columna 1 mes despues + 6 meses después 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(six_monthB= dinici- 180,
                                                                three_monthB = dinici - 90,  # 3 meses antes
                                                                one_monthB= dinici - 30,
                                                                firstT = dinici + 90, 
                                                                secondT = dinici + 180, 
                                                                one_monthA = dfi+ 30,
                                                                three_monthA = dfi + 90,  # 3 meses después
                                                                six_monthA = dfi +180,
                                                                id_emb = row_number())
# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb, six_monthB, three_monthB, one_monthB, dinici, dfi,              
                                       firstT, secondT, dfi, one_monthA,three_monthB, six_monthA)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")

##Seleccionamos columnas que nos interesen 
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb,id_emb, six_monthB, three_monthB, one_monthB, dinici, firstT, secondT, dfi, 
         one_monthA, three_monthA, six_monthA)

##denominador prevalencia: embarazadas por periodo de tiempo
#todos aquellos periodos de observacion completos (-6 m, -1m, 1t,2t,3t,1m+,6+)entraran como denominador 
#si no tienen denominador: no entraran. 
#creamos nuevas columnas en forma de 0 y 1 que nos servirán para calcular posteriormente el denominador 
denominadorTrimestres <- prescrip_drugs %>%
  ##si la fecha 6mB esta entre inicio de datos disponibles y fecha de inicio: entra denominador
  ##sino no entra
  mutate(deno_6mB = if_else(between(six_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha 3mB está entre inicio de periodo de observacion y dinici: entra en el denominador
         #sino no entra: O 
         deno_3mB = if_else(between(three_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha 1mB está entre inicio de periodo de observacion y dinici: entra en el denominador
         #sino no entra: O 
         deno_1mB = if_else(between(one_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha fin esta en primer, segundo o tercer trimestre, entra en denominador 1T
         #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
         deno_firstT = if_else((dfi>= dinici) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")) & between(dinici, as.Date("2010-12-31"), as.Date("2020-06-01")), 1, 0),
         ##si la fecha fin esta entre segundo y tercer trimestre: entra en 2T. Si ffin en primer t: no entra
         #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
         deno_secondT = if_else((between(dfi, firstT, secondT) | between(dfi, secondT, dfi)) & !between(dfi, dinici, firstT) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
         ##Si la fecha fin es posterior a segundo trimestre y se encuentra entre 01/2011 y junio 2020
         #entra como denominador de 3er trimestre 
         deno_thirdT = if_else(dfi >= secondT & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 1mB  
         deno_1mA = if_else(one_monthA < as.Date("2020-06-01"), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 3mB  
         deno_3mA = if_else(three_monthA < as.Date("2020-06-01"), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 6mB
         deno_6mA = if_else(six_monthA < as.Date("2020-06-01"), 1, 0))

#nueva base de datos denom
#calculamos el denominador total 
denom_nocolapsado <- denominadorTrimestres %>%  
  distinct(id_emb, .keep_all = T) %>%
  #seleccionamos las columnas que nos interesan
  select("idp","id_emb","deno_6mB","deno_3mB", "deno_1mB","deno_firstT",
         "deno_secondT","deno_thirdT","deno_1mA","deno_3mA","deno_6mA") %>%
  #Filtra las filas duplicadas basadas en todas las columnas seleccionadas. 
  #Esto asegura que solo se consideren combinaciones únicas de "idp" e "id_emb".
  distinct() 

#sumamos los 0 y 1 de las columnas 
denom_colapsado <- denom_nocolapsado%>% 
  #sumamos el total de 1 en cada columna para calcular el total de denominadores 
  summarise(across(c("deno_6mB","deno_3mB", "deno_1mB",
                     "deno_firstT","deno_secondT","deno_thirdT","deno_1mA","deno_3mA","deno_6mA"),sum))


#Numerador de prevalencia
# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
#6 meses antes : desde fecha six_monthB a dinici
intervalo_6mB <- interval(denominadorTrimestres$six_monthB, denominadorTrimestres$dinici)
#3 meses antes 
intervalo_3mB <- interval(denominadorTrimestres$three_monthB, denominadorTrimestres$dinici)
#1 meses antes : desde fecha one_monthB a dinici
intervalo_1mB <- interval(denominadorTrimestres$one_monthB, denominadorTrimestres$dinici)
#primer trimestre: dinici-firstT 
intervalo_1T <- interval(denominadorTrimestres$dinici, denominadorTrimestres$firstT)
#segundo trimestre: firstT - secondt
intervalo_2T <- interval(denominadorTrimestres$firstT, denominadorTrimestres$secondT)
#tercer trimestre: secondt - dfi 
intervalo_3T <- interval(denominadorTrimestres$secondT, denominadorTrimestres$dfi)
#1 mes despues_ dfi: onemonthA
intervalo_1mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$one_monthA)
#3 meses despues 
intervalo_3mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$three_monthA)
#6 meses despues dfi- sixmonthA 
intervalo_6mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$six_monthA)
#intervalo de prescripcion va desde inincio dat inici a dbaixa 
intervalo_prescripcion <- interval(denominadorTrimestres$dat, denominadorTrimestres$dbaixa)

# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradorTrimestres <- denominadorTrimestres %>%
mutate(
expo_6mB = ifelse(int_overlaps(intervalo_6mB, intervalo_prescripcion), 1, 0),
expo_3mB = ifelse(int_overlaps(intervalo_3mB, intervalo_prescripcion), 1, 0),
expo_1mB = ifelse(int_overlaps(intervalo_1mB, intervalo_prescripcion), 1, 0), 
expo_1T = ifelse(int_overlaps(intervalo_1T, intervalo_prescripcion), 1, 0),
expo_2T = ifelse(int_overlaps(intervalo_2T, intervalo_prescripcion), 1, 0),
expo_3T = ifelse(int_overlaps(intervalo_3T, intervalo_prescripcion), 1, 0),
expo_1mA = ifelse(int_overlaps(intervalo_1mA, intervalo_prescripcion), 1, 0),
expo_3mA = ifelse(int_overlaps(intervalo_3mA, intervalo_prescripcion), 1, 0),
expo_6mA = ifelse(int_overlaps(intervalo_6mA, intervalo_prescripcion), 1, 0),
## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
expo_6mB = expo_6mB * deno_6mB,
expo_3mB = expo_3mB * deno_3mB,
expo_1mB = expo_1mB * deno_1mB,
expo_1T = expo_1T * deno_firstT,
expo_2T = expo_2T * deno_secondT,
expo_3T = expo_3T * deno_thirdT,
expo_1mA = expo_1mA * deno_1mA,
expo_3mA = expo_3mA * deno_3mA,
expo_6mA = expo_6mA * deno_6mA
)


#creamos bd numerador
num <- numeradorTrimestres %>% 
  #agrupamos por id de embarazo y codigo del farmaco 
  group_by(id_emb, cod) %>%
  #sumamos cada una de las filas 
  summarise(across(c("expo_6mB","expo_3mB","expo_1mB","expo_1T","expo_2T","expo_3T",
                     "expo_1mA","expo_3mA","expo_6mA"), max)) 

#Calculo del numerador para el grupo N (excepto N01 Y N02)
#filtramos por grupo N desde la tabla num

grupo_N_num_descolapsado <- num %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 3)) %>%
  filter(grupo_N %in% paste0("N0", 3:9)) %>%
  group_by(id_emb, grupo_N) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"), max)) %>%
  group_by(id_emb) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"), max))
  
#colapsamos Para obtener el N de episodios de embarazo en cada etapa del embarazo 
grupo_N_numcolapsado <- grupo_N_num_descolapsado%>%
#sumamos el total de 1 en cada una de las filas de exposicion 
  summarise(across(c("expo_6mB","expo_3mB", "expo_1mB","expo_1T","expo_2T","expo_3T","expo_1mA",
                     "expo_3mA","expo_6mA"),sum))

#calculo de prevalencia total para todo el grupo N 
prevalencia_total_grupoN <- grupo_N_numcolapsado/denom_colapsado*100      

# #Intervalos de confianza para cada uno de los periodos 
# los eventos (en este caso, los eventos "1" de los vectores) 
# siguen una distribución de Poisson con una tasa lambda desconocida y 
# después estimamos esa tasa lambda a partir de los datos disponibles 
#Vectores que contienen los valores 0 y 1 por cada periodo de tiempo
#Numeradores de todo el grupo N (excepto N01 y N02)
vector_NUM6mb <- grupo_N_num_descolapsado$expo_6mB
vector_NUM3mb <- grupo_N_num_descolapsado$expo_3mB
vector_NUM1mb <- grupo_N_num_descolapsado$expo_1mB
vector_NUM1T <- grupo_N_num_descolapsado$expo_1T
vector_NUM2T <- grupo_N_num_descolapsado$expo_2T
vector_NUM3T <- grupo_N_num_descolapsado$expo_3T
vector_NUM1mA <- grupo_N_num_descolapsado$expo_1mA
vector_NUM3mA <- grupo_N_num_descolapsado$expo_3mA
vector_NUM6mA <- grupo_N_num_descolapsado$expo_6mA
#Denominadores
vector_DENOM6mb <- denom_nocolapsado$deno_6mB
vector_DENOM3mb <- denom_nocolapsado$deno_3mB
vector_DENOM1mb <- denom_nocolapsado$deno_1mB
vector_DENOM1T <- denom_nocolapsado$deno_firstT
vector_DENOM2T <- denom_nocolapsado$deno_secondT
vector_DENOM3T <- denom_nocolapsado$deno_thirdT
vector_DENOM1mA <- denom_nocolapsado$deno_1mA
vector_DENOM3mA <- denom_nocolapsado$deno_3mA
vector_DENOM6mA <- denom_nocolapsado$deno_6mA

# Calcular el intervalo de confianza exacto de Poisson
conf_int_exact_6mB <- poisson.exact(sum(vector_NUM6mb), sum(vector_DENOM6mb), conf.level = 0.95)
print(conf_int_exact_6mB)
conf_int_exact_3mB <- poisson.exact(sum(vector_NUM3mb), sum(vector_DENOM3mb), conf.level = 0.95)
print(conf_int_exact_3mB)
conf_int_exact_1mB <- poisson.exact(sum(vector_NUM1mb), sum(vector_DENOM1mb), conf.level = 0.95)
print(conf_int_exact_1mB)
conf_int_exact_1T<- poisson.exact(sum(vector_NUM1T), sum(vector_DENOM1T), conf.level = 0.95)
print(conf_int_exact_1T)
conf_int_exact_2T<- poisson.exact(sum(vector_NUM2T), sum(vector_DENOM2T), conf.level = 0.95)
print(conf_int_exact_2T)
conf_int_exact_3T <- poisson.exact(sum(vector_NUM3T), sum(vector_DENOM3T), conf.level = 0.95)
print(conf_int_exact_3T)
conf_int_exact_1mA <- poisson.exact(sum(vector_NUM1mA), sum(vector_DENOM1mA), conf.level = 0.95)
print(conf_int_exact_1mA)
conf_int_exact_3mA <- poisson.exact(sum(vector_NUM3mA), sum(vector_DENOM3mA), conf.level = 0.95)
print(conf_int_exact_3mA)
conf_int_exact_6mA <- poisson.exact(sum(vector_NUM6mA), sum(vector_DENOM6mA), conf.level = 0.95)
print(conf_int_exact_6mA)

# Antidepresivos: tots
antidepresivos_num_nocolapsado <- num %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 3)) %>%
  filter(grupo_N %in% c("N06")) %>%
  group_by(id_emb, grupo_N) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"), max)) %>%
  group_by(id_emb) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"), max))

#colapsamos Para obtener el N de episodios de embarazo en cada etapa del embarazo 
AD_numcolapsado <- antidepresivos_num_nocolapsado%>%
  #sumamos el total de 1 en cada una de las filas de exposicion 
  summarise(across(c("expo_6mB","expo_3mB", "expo_1mB","expo_1T","expo_2T","expo_3T","expo_1mA",
                     "expo_3mA","expo_6mA"),sum))

#calculo de prevalencia total para todo el grupo N 
prevalencia_total_AD <- AD_numcolapsado/denom_colapsado*100      

# #Intervalos de confianza para cada uno de los periodos 
# los eventos (en este caso, los eventos "1" de los vectores) 
# siguen una distribución de Poisson con una tasa lambda desconocida y 
# después estimamos esa tasa lambda a partir de los datos disponibles 
#Vectores que contienen los valores 0 y 1 por cada periodo de tiempo
#Numeradores de AD
vector_AD6mb <- antidepresivos_num_nocolapsado$expo_6mB
vector_AD3mb <- antidepresivos_num_nocolapsado$expo_3mB
vector_AD1mb <- antidepresivos_num_nocolapsado$expo_1mB
vector_AD1T <- antidepresivos_num_nocolapsado$expo_1T
vector_AD2T <- antidepresivos_num_nocolapsado$expo_2T
vector_AD3T <- antidepresivos_num_nocolapsado$expo_3T
vector_AD1mA <- antidepresivos_num_nocolapsado$expo_1mA
vector_AD3mA <- antidepresivos_num_nocolapsado$expo_3mA
vector_AD6mA <- antidepresivos_num_nocolapsado$expo_6mA
#Denominadores
#mismos vectores que previamente
# Calcular el intervalo de confianza exacto de Poisson
conf_int_exact_6mB <- poisson.exact(sum(vector_AD6mb), sum(vector_DENOM6mb), conf.level = 0.95)
print(conf_int_exact_6mB)
conf_int_exact_3mB <- poisson.exact(sum(vector_AD3mb), sum(vector_DENOM3mb), conf.level = 0.95)
print(conf_int_exact_3mB)
conf_int_exact_1mB <- poisson.exact(sum(vector_AD1mb), sum(vector_DENOM1mb), conf.level = 0.95)
print(conf_int_exact_1mB)
conf_int_exact_1T<- poisson.exact(sum(vector_AD1T), sum(vector_DENOM1T), conf.level = 0.95)
print(conf_int_exact_1T)
conf_int_exact_2T<- poisson.exact(sum(vector_AD2T), sum(vector_DENOM2T), conf.level = 0.95)
print(conf_int_exact_2T)
conf_int_exact_3T <- poisson.exact(sum(vector_AD3T), sum(vector_DENOM3T), conf.level = 0.95)
print(conf_int_exact_3T)
conf_int_exact_1mA <- poisson.exact(sum(vector_AD1mA), sum(vector_DENOM1mA), conf.level = 0.95)
print(conf_int_exact_1mA)
conf_int_exact_3mA <- poisson.exact(sum(vector_AD3mA), sum(vector_DENOM3mA), conf.level = 0.95)
print(conf_int_exact_3mA)
conf_int_exact_6mA <- poisson.exact(sum(vector_AD6mA), sum(vector_DENOM6mA), conf.level = 0.95)
print(conf_int_exact_6mA)

#ACIDO VALPROICO
valproico_nocolapsado <- num %>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 7))%>%
  filter(grupo_N %in% paste0("N03AG01"))%>%
  group_by(id_emb, cod) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"))) %>%
 ungroup()


#colapsamos Para obtener el N de episodios de embarazo en cada etapa del embarazo 
VPA_numcolapsado <- valproico_nocolapsado%>%
  #sumamos el total de 1 en cada una de las filas de exposicion 
  #sumamos el total de 1 en cada una de las filas de exposicion 
  summarise(across(c("expo_6mB","expo_3mB", "expo_1mB","expo_1T","expo_2T","expo_3T","expo_1mA",
                     "expo_3mA","expo_6mA"),sum))


#calculo de prevalencia total para todo el grupo N 
prevalencia_total_vpa <- VPA_numcolapsado/denom_colapsado*100      

# #Intervalos de confianza para cada uno de los periodos 
# los eventos (en este caso, los eventos "1" de los vectores) 
# siguen una distribución de Poisson con una tasa lambda desconocida y 
# después estimamos esa tasa lambda a partir de los datos disponibles 
#Vectores que contienen los valores 0 y 1 por cada periodo de tiempo
#Numeradores de AD
vector_VPA6mb <- valproico_nocolapsado$expo_6mB
vector_VPA3mb <- valproico_nocolapsado$expo_3mB
vector_VPA1mb <- valproico_nocolapsado$expo_1mB
vector_VPA1T <- valproico_nocolapsado$expo_1T
vector_VPA2T <- valproico_nocolapsado$expo_2T
vector_VPA3T <- valproico_nocolapsado$expo_3T
vector_VPA1mA <- valproico_nocolapsado$expo_1mA
vector_VPA3mA <- valproico_nocolapsado$expo_3mA
vector_VPA6mA <- valproico_nocolapsado$expo_6mA
#Denominadores
#mismos vectores que previamente
# Calcular el intervalo de confianza exacto de Poisson
conf_int_exact_6mB <- poisson.exact(sum(vector_VPA6mb), sum(vector_DENOM6mb), conf.level = 0.95)
print(conf_int_exact_6mB)
conf_int_exact_3mB <- poisson.exact(sum(vector_VPA3mb), sum(vector_DENOM3mb), conf.level = 0.95)
print(conf_int_exact_3mB)
conf_int_exact_1mB <- poisson.exact(sum(vector_VPA1mb), sum(vector_DENOM1mb), conf.level = 0.95)
print(conf_int_exact_1mB)
conf_int_exact_1T<- poisson.exact(sum(vector_VPA1T), sum(vector_DENOM1T), conf.level = 0.95)
print(conf_int_exact_1T)
conf_int_exact_2T<- poisson.exact(sum(vector_VPA2T), sum(vector_DENOM2T), conf.level = 0.95)
print(conf_int_exact_2T)
conf_int_exact_3T <- poisson.exact(sum(vector_VPA3T), sum(vector_DENOM3T), conf.level = 0.95)
print(conf_int_exact_3T)
conf_int_exact_1mA <- poisson.exact(sum(vector_VPA1mA), sum(vector_DENOM1mA), conf.level = 0.95)
print(conf_int_exact_1mA)
conf_int_exact_3mA <- poisson.exact(sum(vector_VPA3mA), sum(vector_DENOM3mA), conf.level = 0.95)
print(conf_int_exact_3mA)
conf_int_exact_6mA <- poisson.exact(sum(vector_VPA6mA), sum(vector_DENOM6mA), conf.level = 0.95)
print(conf_int_exact_6mA)


#por grupitos 
grupitos_N_prevalencia <-num%>% 
  #filtramos por grupo codigo del grupo N
  filter(grepl("^N0[3-9]", cod))%>% 
  #agrupamos pro cada uno de los subgrupo
  group_by(cod) %>%
  #sumamos las tablas de cada uno de los subgrupos 
  summarise(across(c("expo_6mB","expo_1mB","expo_1T","expo_2T","expo_3T","expo_1mA","expo_6mA"),sum)) %>%
  left_join(`cataleg_LuciaVH_N_2023-06-09`%>%filter(domini=="farmacs_prescrits")) %>%
  #calculamos la prevalencia para cada uno de los subgrupos 
  mutate(prev_6mB = expo_6mB/denom$deno_6mB*100,
         prev_1mB = expo_1mB/denom$deno_6mB*100, 
         prev_1T = expo_1T/denom$deno_firstT*100, 
         prev_2T = expo_2T/denom$deno_secondT*100, 
         prev_3T = expo_3T/denom$deno_thirdT*100, 
         prev_1mA = expo_1mA/denom$deno_1mA*100, 
         prev_6mA = expo_6mA/denom$deno_6mA*100)
