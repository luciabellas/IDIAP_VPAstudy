library("DBI")
library("duckdb")
library ("ggplot2")
library(CDMConnector)
library(dplyr)
library(tidyr)
library(lubridate)
library(IncidencePrevalence)

####### Load data
embarazos <- `repositori_LuciaVH_N_2023-06-09`  %>% 
  select(idp, cod_emb,id_emb, dinici, dfi) %>%
  mutate(dinici = as.Date(dinici),
         dfi = as.Date(dfi))
rm(`repositori_LuciaVH_N_2023-06-09`)

farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  left_join(embarazos, relationship = "many-to-many") %>%
  select(idp, cod_emb, dinici, dfi, cod, dat, dbaixa, cod_emb,id_emb)%>%
  mutate(dinici = as.Date(dinici),
         dfi = as.Date(dfi),
         id_presc = row_number())
rm(`farmacs_prescrits_LuciaVH_N_2023-06-09`)

#Create the cohorts 
#cohort_definition_id subject_id cohort_start_date cohort_end_date
#<int>                   <int64>     <date>            <date>    


embarazos <- embarazos %>% select( id_emb, dinici, dfi) %>%
  rename( subject_id=id_emb, cohort_start_date=dinici,  cohort_end_date= dfi ) %>%
  mutate(cohort_definition_id=1)

levetiracetam <- farmacos %>% 
  filter(grepl("N03AX14", cod)) %>% 
  select( id_emb, dat, dbaixa) %>%
  rename( subject_id=id_emb, cohort_start_date=dat,  cohort_end_date= dbaixa ) %>%
  mutate(cohort_definition_id=2) %>%
  filter(cohort_start_date > as.Date("2011-01-01") &
           cohort_start_date < as.Date("2020-06-30") &
           cohort_end_date > as.Date("2011-01-01") &
           cohort_end_date < as.Date("2020-06-30"))%>%
  as_tibble()


# to start an in-memory database
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

DBI::dbWriteTable(con, "embarazadas", embarazos, overwrite = TRUE)
DBI::dbWriteTable(con, "levetiracetam", levetiracetam, overwrite = TRUE)
DBI::dbWriteTable(con, "observation_period", dplyr::tibble(observation_period_id = NA, person_id = NA, observation_start_date = NA, observation_end_date = NA), overwrite = TRUE)
DBI::dbWriteTable(con, "person", dplyr::tibble(person_id   = 1, gender_concept_id  = NA, year_of_birth = NA, month_of_birth = NA), overwrite = TRUE)

cdm <- cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main", cdmName = "my_cdm", cohortTables = c("embarazadas", "levetiracetam"))


# Ruta completa al escritorio en macOS
ruta_escritorio <- file.path("/Users/luciabellas_/Desktop", "LVT.csv")

prevLVT<- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "embarazadas",
  outcomeTable = "levetiracetam",
  interval = "months",
  minCellCount = 0 )


# Guardar el dataset en formato CSV en el escritorio
write.csv(prevLVT, file = ruta_escritorio)

#grafico 
ggplot(data = prevLVT,
       mapping = aes(           # map aesthetics to columns
         x = prevalence_start_date ,
         y = prevalence*100)
)+ 
  geom_point(                   # add points for each row of data
    size = 1,
    alpha = 1)

##GPLOT BONITO 

# Convertir prevLMT$prevalence_start_date a tipo de dato Date 
prevLVT$prevalence_start_date <- as.Date(prevLVT$prevalence_start_date)

# Crear un objeto que contenga las fechas deseadas para el eje X
fechas_deseadas <- seq(as.Date("2011-01-01"), as.Date("2020-06-01"), by = "1 month")

ggplot(prevLVT, aes(x = prevalence_start_date, y = prevalence * 100)) +
  geom_point(size = 3, color = "#5F9EA0") +
  geom_line(aes(group = 1), color = "#CD6600") +
  labs(x = "Time-points (months)", y = "Prevalence %",
       title = "Prevalence levetiracetam monthly") +
  theme(plot.title = element_text(size = 18, color = "#5F9EA0", face = "bold"),
        axis.title = element_text(size = 15, color = "#5F9EA0", face = "bold"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "#5F9EA0"),
        axis.text.y = element_text(color = "#5F9EA0", size = 10, hjust = 1)) +
  scale_x_date(breaks = fechas_deseadas[seq_along(fechas_deseadas) %% 4 == 1], 
               labels = format(fechas_deseadas[seq_along(fechas_deseadas) %% 4 == 1], "%b %Y"))