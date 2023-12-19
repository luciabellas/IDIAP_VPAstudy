#Creamos BD 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(three_monthB = dinici - 90,                                                                three_monthB = dinici - 90,  # 3 meses antes
                                                                firstT = dinici + 90, 
                                                                secondT = dinici + 180, 
                                                                three_monthA = dfi + 90,  # 3 meses después
                                                                six_monthA = dfi +180,
                                                                id_emb = row_number())

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")

##Seleccionamos columnas que nos interesen 
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb,id_emb, three_monthB, dinici, 
         firstT, secondT, dfi, three_monthA, six_monthA)

#Creamos los intervalos para definir la incidencia 
#Definimos intervalo de prescripción 
intervalo_prescrip <- interval (prescrip_drugs$dat, prescrip_drugs$dbaixa)
#DEFINIMOS INTERVALOS DE OSBERVACION
#3MESES ANTES  three monthB- dinici:   
intervalo_i3mB <- interval(prescrip_drugs$three_monthB, prescrip_drugs$dinici) 
#primer trimestre: dinici-firstT 
intervalo_i1T <- interval(prescrip_drugs$dinici, prescrip_drugs$firstT)
#segundo trimestre: firstT - secondt
intervalo_i2T <- interval(prescrip_drugs$firstT, prescrip_drugs$secondT)
#tercer trimestre: secondt - dfi 
intervalo_i3T <- interval(prescrip_drugs$secondT, prescrip_drugs$dfi)
#1 mes despues_ dfi: onemonthA
intervalo_i1mA <- interval(prescrip_drugs$dfi, prescrip_drugs$one_monthA)
#3 meses despues dfi: three monthA 
intervalo_i3mA <- interval(prescrip_drugs$dfi, prescrip_drugs$three_monthA)
#6 meses despues dfi- sixmonthA 
intervalo_i6mA <- interval(prescrip_drugs$dfi, prescrip_drugs$six_monthA)
#INTERVALOS DE WASHOUT 
#Hay uno para cada intervalo porque tiene que incluir el lo antes + trimestre anterior 
#3 meses antes
intervalo_w3mB <- interval(prescrip_drugs$dinici -years(1), prescrip_drugs$three_monthB) 
#primer trimestre: 1 año antes embarazo - dinici
intervalo_w1T <- interval(prescrip_drugs$dinici- years (1), prescrip_drugs$dinici)
#segundo trimestre: 1 año antes de embarazo hsata fecha de fin de primer trimestrs
intervalo_w2T <- interval(prescrip_drugs$dinici - years(1), prescrip_drugs$firstT)
#tercer trimestre: 1 año antes del embarazo hasta fecha fin de segundo trimestre
intervalo_w3T <- interval(prescrip_drugs$dinici-years(1), prescrip_drugs$secondT)
# despues_ dfi:  1 año antes del embarazo - despues del embarazo 
intervalo_wA <- interval(prescrip_drugs$dinici-years(1), prescrip_drugs$dfi)

#Calculamos denominadores 
#Si hay solapamiento entre el periodo de em 
denom_ITS_Inci <- prescrip_drugs %>%
  mutate(##si la fecha 3mB está entre inicio de periodo de observacion y dinici: entra en el denominador
    #sino no entra: O 
    deno_3mB = if_else(between(three_monthB, as.Date("2011-01-01"), dinici), 1, 0),
    ##si la fecha fin esta en primer, segundo o tercer trimestre, entra en denominador 1T
    #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
    deno_firstT = if_else((dfi>= dinici) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")) & between(dinici, as.Date("2010-12-31"), as.Date("2020-06-01")), 1, 0),
    ##si la fecha fin esta entre segundo y tercer trimestre: entra en 2T. Si ffin en primer t: no entra
    #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
    deno_secondT = if_else((between(dfi, firstT, secondT) | between(dfi, secondT, dfi)) & !between(dfi, dinici, firstT) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
    ##Si la fecha fin es posterior a segundo trimestre y se encuentra entre 01/2011 y junio 2020
    #entra como denominador de 3er trimestre 
    deno_thirdT = if_else(dfi >= secondT & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
    ##Si el periodo de observacion entra en periodo de estudio: entra en 3mB  
    deno_3mA = if_else(three_monthA < as.Date("2020-06-01"), 1, 0),
    ##Si el periodo de observacion entra en periodo de estudio: entra en 6mB
    deno_6mA = if_else(six_monthA < as.Date("2020-06-01"), 1, 0),
    EmB_washout = ifelse(!int_overlaps(intervalo_w3mB, intervalo_prescrip), 1,0), 
    E1T_washout = ifelse(!int_overlaps(intervalo_w1T, intervalo_prescrip), 1,0), 
    E2T_washout = ifelse(!int_overlaps(intervalo_w2T, intervalo_prescrip), 1,0), 
    E3T_washout = ifelse(!int_overlaps(intervalo_w3T, intervalo_prescrip), 1,0), 
    EmA_washout = ifelse(!int_overlaps(intervalo_wA, intervalo_prescrip), 1,0))

#denominador por trimestres para VPA 
deno_grupoN <- denom_ITS_Inci %>%
  mutate(
    deno_3mB = if_else(str_detect(cod,"N03AG01"), deno_3mB * EmB_washout, deno_3mB),
    deno_firstT = if_else(str_detect(cod,"N03AG01"), deno_firstT * E1T_washout, deno_firstT),
    deno_secondT = if_else(str_detect(cod,"N03AG01"), deno_secondT * E2T_washout, deno_secondT),
    deno_thirdT = if_else(str_detect(cod,"N03AG01"), deno_thirdT * E3T_washout, deno_thirdT),
    deno_3mA = if_else(str_detect(cod,"N03AG01"), deno_3mA * EmA_washout, deno_3mA),
    deno_6mA = if_else(str_detect(cod,"N03AG01"), deno_6mA * EmA_washout, deno_6mA))

#nos quedamos con el denominador máximo
#Al agurpar por id_emb y cod
deno_grupoNmax <- deno_grupoN %>%  
  select("idp", "id_emb", "cod", "deno_3mB", "deno_firstT", "deno_secondT",
         "deno_thirdT","deno_3mA","deno_6mA") %>% 
  group_by(id_emb, cod) %>%
  #nos quedamos con el valor máximo de cada uno de los códigos 
  summarise(across(c("deno_3mB", "deno_firstT", "deno_secondT","deno_thirdT","deno_3mA","deno_6mA"), max))

#al agruparlo por id_emb, nos quedamos con el valor mínimo 
#así nos quitaremos del denominador
#los N que hayan sido prescritos el año anterior. 
deno_grupoNmin <- deno_grupoNmax%>%
  group_by(id_emb)%>%  
  summarise(across(c("deno_3mB", "deno_firstT", "deno_secondT","deno_thirdT","deno_3mA","deno_6mA"), min))%>%
  select("deno_3mB", "deno_firstT", "deno_secondT","deno_thirdT","deno_3mA","deno_6mA")

#colapsamos las columnas 
VPA_denocolapsado <- deno_grupoNmin %>%
  summarise(across(c("deno_3mB", "deno_firstT", "deno_secondT", "deno_thirdT", "deno_3mA", "deno_6mA"), 
                   ~ sum(., na.rm = TRUE)))

#NUMERADOR PARA EL VPA  
#Calculamos si el episodio de solapamiento coincide 
numeradorTrimestres_Inci <- deno_grupoN %>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    E3mB_Inci = ifelse(int_overlaps(intervalo_i3mB, intervalo_prescrip), 1,0),
    E1T_Inci = ifelse(int_overlaps(intervalo_i1T, intervalo_prescrip), 1,0),
    E2T_Inci = ifelse(int_overlaps(intervalo_i2T, intervalo_prescrip), 1, 0),
    E3T_Inci = ifelse(int_overlaps(intervalo_i3T, intervalo_prescrip), 1, 0),
    E3mA_Inci = ifelse(int_overlaps(intervalo_i3mA, intervalo_prescrip), 1, 0),
    E6mA_Inci= ifelse(int_overlaps(intervalo_i6mA, intervalo_prescrip), 1, 0))%>%
  #multiplicamos por los 
  mutate(
    E3mB_Inci=E3mB_Inci*deno_3mB*EmB_washout,
    E1T_Inci=E1T_Inci*deno_firstT*E1T_washout, 
    E2T_Inci= E2T_Inci*deno_secondT*E2T_washout,
    E3T_Inci=E3T_Inci*deno_thirdT*E3T_washout, 
    E3mA_Inci= E3mA_Inci*deno_3mA*EmA_washout, 
    E6mA_Inci=E6mA_Inci*deno_6mA*EmA_washout)%>%
  select("id_emb", "cod", "idp", "E3mB_Inci","E1T_Inci","E2T_Inci","E3T_Inci","E3mA_Inci", "E6mA_Inci")

# Al agrupar por id_emb y cod y obtener el valor máximo para ciertas columnas
num_grupoNmax <- numeradorTrimestres_Inci %>%
  group_by(id_emb, cod) %>%
  summarise(
    across(c("E3mB_Inci", "E1T_Inci", "E2T_Inci", "E3T_Inci", "E3mA_Inci", "E6mA_Inci"), max))

#al agruparlo por id_emb, nos quedamos con el valor mínimo 
#así nos quitaremos del denominador
#los N que hayan sido prescritos el año anterior. 
NUM_grupoN<- num_grupoNmax%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 7)) %>%
  filter(grupo_N %in% paste0("N03AG01"))%>%
  group_by(id_emb)%>%  
  summarise(
    across(c("E3mB_Inci", "E1T_Inci", "E2T_Inci", "E3T_Inci", "E3mA_Inci", "E6mA_Inci"), max))

#sumamos a través de las columnas para calcular Ns 
N_colapsado <- NUM_grupoN %>%
  #sumamos todos los denominadores
  summarise(across(c("E3mB_Inci", "E1T_Inci","E2T_Inci","E3T_Inci","E3mA_Inci","E6mA_Inci")
                   ,~sum(., na.rm = T)))

#INCIDENCIA ACUMULADA 
incidenciagrupoN <- N_colapsado/VPA_denocolapsado*100 

#INTERVALOS DE CONFIANZA VALPROICO 
# Vectores de numeradores
numerator_vectors <- list(
  N_colapsado$E3mB_Inci,
  N_colapsado$E1T_Inci,
  N_colapsado$E2T_Inci,
  N_colapsado$E3T_Inci,
  N_colapsado$E3mA_Inci,
  N_colapsado$E6mA_Inci
)

# Vectores de denominadores
denominator_vectors <- list(
  VPA_denocolapsado$deno_3mB,
  VPA_denocolapsado$deno_firstT,
  VPA_denocolapsado$deno_secondT,
  VPA_denocolapsado$deno_thirdT,
  VPA_denocolapsado$deno_3mA,
  VPA_denocolapsado$deno_6mA
)

# Reemplazar NA con 0 en todos los vectores
#Por que me salen NA aqui??? 
numerator_vectors <- lapply(numerator_vectors, function(x) { x[is.na(x)] <- 0; return(x) })
denominator_vectors <- lapply(denominator_vectors, function(x) { x[is.na(x)] <- 0; return(x) })

# Calcular los intervalos de confianza exactos de Poisson para todos los vectores
conf_int_exact <- lapply(1:length(numerator_vectors), function(i) {
  poisson.exact(sum(numerator_vectors[[i]]), sum(denominator_vectors[[i]]), conf.level = 0.95)
})

# Imprimir los intervalos de confianza
for (i in 1:length(conf_int_exact)) {
  print(conf_int_exact[[i]])
  cat("\n")
}
