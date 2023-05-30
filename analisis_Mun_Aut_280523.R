# Carga de paquetes y datos ----
library(ggplot2)
library(lubridate)

colClas <- c("character", "character", "integer")
x <- read.csv("voto_horario_Ulea.csv", header = T, sep = ";",
              colClasses = colClas)

# Formateo de datos ----

x$Hora_ini <- as.POSIXct(x$Hora_ini, format = "%H:%M:%S")
x$Hora_fin <- as.POSIXct(x$Hora_fin, format = "%H:%M:%S")
x$temp <- x$Hora_ini+(x$Hora_fin-x$Hora_ini)/2
x$temp <- seq(as.POSIXct("09:30:00", format = "%H:%M:%S"),
              as.POSIXct("19:30:00", format = "%H:%M:%S"),
              "1 hour")

# Gráficos ----

## Histograma por horas ----
ggplot(x, aes(x = temp, y = N_Mun_Aut_280523))+
  geom_col(fill = "#6ce76a")+
  geom_text(aes(label = N_Mun_Aut_280523), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25)+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M")+
  scale_y_continuous(breaks = seq(0, 120, 10))+
  labs(x = "Hora", y = "Número de votantes")+
  theme_classic()

ggsave("Histograma_por_horas.png")
  
## Histograma acumulado por horas ----

ggplot(x, aes(x = temp, y = cumsum(N_Mun_Aut_280523)))+
  geom_col(fill = "#6ce76a")+
  geom_text(aes(label = cumsum(N_Mun_Aut_280523)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25)+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M")+
  scale_y_continuous(breaks = seq(0, 700, 50))+
  labs(x = "Hora", y = "Número de votantes")+
  theme_classic()
ggsave("Histograma_acumulado_por_horas.png")

