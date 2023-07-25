# Carga de paquetes y datos ----
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggrepel)


colClas <- c("character", "character", "integer", "integer")
x <- read.csv("voto_horario_Ulea.csv", header = T, sep = ";",
              colClasses = colClas)


# Formateo de datos ----

x$Hora_ini <- as.POSIXct(x$Hora_ini, format = "%H:%M:%S")
x$Hora_fin <- as.POSIXct(x$Hora_fin, format = "%H:%M:%S")
x$Hora_int <- paste(format(x$Hora_ini, "%H:%M"), 
                    format(x$Hora_fin, "%H:%M"), 
                    sep = "-")
#x$temp <- x$Hora_ini+(x$Hora_fin-x$Hora_ini)/2
#x$temp <- seq(as.POSIXct("09:30:00", format = "%H:%M:%S"),
              as.POSIXct("19:30:00", format = "%H:%M:%S"),
              "1 hour")
x%>%
  gather("Elecciones", "Asistentes", -c(Hora_ini, Hora_fin, Hora_int)) -> temp

# Gráficos ----

## Gráfico horario ----
ggplot(temp, aes(x = Hora_int, y = Asistentes, 
                 color = Elecciones))+
  geom_line(aes(group = Elecciones))+
  geom_point()+
  scale_x_discrete(breaks=unique(temp$Hora_int))+
  scale_y_continuous(breaks = seq(0, 120, 10))+
  scale_color_discrete(labels = c("Generales-23/7/23", "Mun&Auto-28/5/23"))+
  labs(title = "Asistencia de votantes por horas", color = "", x = "Hora", y = "Número de votantes")+
  geom_text_repel(aes(label = Asistentes), 
                  position = position_dodge(width = .1),
                  vjust = .2, size = 3)+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(size = 7, angle = 35,
                                   vjust = .9, hjust = .9))

ggsave("Votos_por_franjas_horarias.png")
  
## Acumulado por horas ----
temp%>%
  group_by(Elecciones)%>%
  summarise(Acumulado = cumsum(Asistentes))%>%
  cbind(temp[1:2])%>%
  ggplot(aes(x = Hora_fin, y = Acumulado, color = Elecciones))+
  geom_line(aes(group = Elecciones))+
  geom_point()+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M")+
  scale_y_continuous(breaks = seq(0, 650, 50))+
  scale_color_discrete(labels = c("Generales-23/7/23", "Mun&Auto-28/5/23"))+
  labs(title = "Asistencia acumulada de votantes por horas", color = "", x = "Hora", y = "Número de votantes acumulado")+
  geom_text_repel(aes(label = Acumulado), 
            position = position_dodge(width = .5), 
            vjust = .2, size = 3)+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(size = 7, angle = 35,
                                   vjust = .9, hjust = .9))

ggsave("Acumulado_por_horas.png")

