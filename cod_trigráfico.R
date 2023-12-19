install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(data.table)


# Transformación de datos
VPA <- VPA %>% select(prevalence_start_date, prevalence)
VPA$prevalence <- as.numeric(VPA$prevalence)
VPA$prevalence_start_date <- as.Date(VPA$prevalence_start_date)
colnames(VPA) <- c("Fecha", "PREV_VPA")

LMT <- LMT %>% select(prevalence_start_date, prevalence)
LMT$prevalence <- as.numeric(LMT$prevalence)
LMT$prevalence_start_date <- as.Date(LMT$prevalence_start_date)
colnames(LMT) <- c("Fecha", "PREV_LMT")


LVT <- LVT %>% select(prevalence_start_date, prevalence)
LVT$prevalence <- as.numeric(LVT$prevalence)
LVT$prevalence_start_date <- as.Date(LVT$prevalence_start_date)
colnames(LVT) <- c("Fecha", "PREV_LVT")

#Datos combinados 
#Juntamos todas las columnas en un único dataset 
trigraph <- VPA %>%mutate( 
  PREV_LVT=LVT$PREV_LVT, 
  PREV_LMT=LMT$PREV_LMT) %>% mutate(
  medida = ifelse(Fecha >= 
                    as.Date("2014-10-01"), 1, 0))


# Cargar el paquete ggplot2 si aún no está cargado

ggplot(trigraph, aes(x = Fecha)) +
  geom_vline(data = subset(trigraph, medida == 1), aes(xintercept = Fecha),
             linetype = "dashed", color = "gray") +
  geom_line(aes(y = PREV_VPA, color = "VPA"), linetype = "solid") +
  geom_line(aes(y = PREV_LMT, color = "LTG"), linetype = "solid") +
  geom_line(aes(y = PREV_LVT, color = "LEV"), linetype = "solid") +
  scale_color_manual(values = c("VPA" = "#53868B", "LTG" = "#FF7F24", "LEV"="#CAFF70"))+
  labs(title = "Prevalence of VPA, LEV and LGT over time ",
       x = "Time points (months)",
       y = "Prevalence")+
  scale_y_continuous(limits = c(0, 0.005)) +
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
               breaks = scales::breaks_pretty(n = 15)) 
