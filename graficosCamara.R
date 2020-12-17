rm(list = ls())
dev.off()
cat("\f")



library(readr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(extrafont)
library(cowplot)
library(patchwork)
library(grid)
library(dplyr)
library(gridExtra)
library(ggrepel)
library(tidyr)
library(reshape2)
library(ggforce)
library(ggridges)
library(shiny)
library(purrr)
library(scales)
library(plotrix)
library(ggalt)
library(treemapify)
library(tibble)


#Colores
verde = c("#2ACA5F")
naranja = c("#F79E0B")
gris = c("#AFAFB0")
gris_letra= c("#808080")


#Movimiento áero
  #Principales destino
DestinosPrincipales <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "Principales_destinos")

DestinosPrincipales= pivot_longer(DestinosPrincipales,cols = c(3,4,5,6),values_to = ("Pasajeros"), names_to = "Año")

str(DestinosPrincipales)

DestinosPrincipales$Año = as.character(DestinosPrincipales$Año)

Destino = DestinosPrincipales %>% group_by(Destino,Año) %>% summarise(Pasajeros = sum(Pasajeros))
Destino$Año = as.numeric(Destino$Año)

Destino$Pasajeros = format(Destino$Pasajeros, big.mark=",")

ggplot(Destino, aes(Año)) +
  geom_line(lwd=3,aes(group= Destino,y = Pasajeros, col = Destino))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(naranja,verde))+
  labs(title = "Salida de pasajeros desde Aeropuerto Alfonso Bonilla Aragón a destinos nacionales e internacionales (2016-2019)",
       x = "Año",
       y = "Pasajeros",
       caption = "Fuente:  Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil. NO se incluyen pasajeros en tránsito ni pasajeros en conexión.")+
  geom_text_repel(aes(x = Año, y=Pasajeros,label=Pasajeros),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")


##Llegada de pasajeros

LlegadaPasajeros <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "LlegadaPasajeros")

LlegadaPasajeros = pivot_longer(LlegadaPasajeros, cols = c(2,3), names_to = "Destino",values_to = "Pasajeros")

LlegadaPasajeros$Pasajeros = format(LlegadaPasajeros$Pasajeros, big.mark=",")

ggplot(LlegadaPasajeros, aes(Año)) +
  geom_line(lwd=3,aes(group= Destino,y = Pasajeros, col = Destino))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(naranja,verde))+
  labs(title = "Llegada de pasajeros desde orígenes nacionales e internacionales con destino a 
  Aeropuerto Alfonso Bonilla Aragón (2016-2019)",
       x = "Año",
       y = "Pasajeros",
       caption = "Fuente:  Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil. NO se incluyen pasajeros en tránsito ni pasajeros en conexión.")+
  geom_text_repel(aes(x = Año, y=Pasajeros,label= Pasajeros),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")



###2.8.2.5 Operaciones aéreas de salida desde Aeropuerto Alfonso Bonilla Aragón a destinos nacionales e internacionales (2016-2019)

OpeAreaSalida <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "OperacionesAreasSalida")


OpeAreaSalida$Vuelos = format(OpeAreaSalida$Vuelos, big.mark=",")

ggplot(OpeAreaSalida, aes(Año)) +
  theme_minimal()+
  geom_line(lwd=3,aes(group= Destino,y = Vuelos, col = Destino))+ 
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(naranja,verde))+
  labs(title = "Operaciones aéreas de salida desde Aeropuerto Alfonso Bonilla Aragón
       a destinos nacionales e internacionales (2016-2019)",
       x = "Año",
       y = "Vuelos",
       caption = "Fuente:  Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil.")+
  geom_text_repel(aes(x = Año, y=Vuelos,label= Vuelos),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")


OpeAreaLlegada <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "OperacionesAreasLlegada")


OpeAreaLlegada$Vuelos = format(OpeAreaLlegada$Vuelos, big.mark=",")


ggplot(OpeAreaLlegada, aes(Año)) +
  geom_line(lwd=3,aes(group= Destino,y = Vuelos, col = Destino))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(naranja,verde))+
  labs(title = "Operaciones aéreas de llegada desde orígenes nacionales e internacionales con destino
       a Aeropuerto Alfonso Bonilla Aragón (2016-2019)",
       x = "Año",
       y = "Vuelos",
       caption = "Fuente:  Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil.")+
  geom_text_repel(aes(x = Año, y=Vuelos,label= Vuelos),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")


### 2.8.2.7 Carga-Correo de salida (ton) transportada en avión desde Aeropuerto Alfonso Bonilla Aragón a destinos nacionales e internacionales (2016-2019)

SalidaCarga<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "SalidaCarga")

SalidaCarga$Año = as.factor(SalidaCarga$Año)
  SalidaCarga$Toneladas = format(SalidaCarga$Toneladas, big.mark=",")

ggplot(SalidaCarga, aes(Año)) +
  geom_line(lwd=3,aes(group= Destino,y = Toneladas, col = Destino))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(naranja,verde))+
  labs(title = "Carga-Correo de salida (ton) transportada en avión desde Aeropuerto Alfonso Bonilla Aragón
       a destinos nacionales e internacionales (2016-2019)",
       x = "Año",
       y = "Toneladas",
       caption = "Fuente:  Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil.")+
  geom_text_repel(aes(x = Año, y=Toneladas,label= Toneladas),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")

#  scale_y_continuous(limits = c(min(SalidaCarga$Toneladas),max(SalidaCarga$Toneladas)),breaks = NULL)



### 2.8.2. Carga-Correo de llegada (ton) transportada en avión desde Aeropuerto Alfonso Bonilla Aragón a destinos nacionales e internacionales (2016-2019)

LlegadaCarga<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "LlegadaCarga")


LlegadaCarga$Toneladas = format(LlegadaCarga$Toneladas, big.mark=",")

ggplot(LlegadaCarga, aes(Año)) +
  geom_line(lwd=3,aes(group= Destino,y = Toneladas, col = Destino))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(naranja,verde))+
  labs(title = "Carga-correo de llegada (ton) transportada en avión desde orígenes nacionales e internacionales con destino
  a Aeropuerto Alfonso Bonilla Aragón (2016-2019)",
       x = "Año",
       y = "Toneladas",
       caption = "Fuente:  Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil.")+
  geom_text_repel(aes(x = Año, y=Toneladas,label= Toneladas),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")




###2.8.2.9 Movimiento anual total de pasajeros por aeropuerto en 2019

AereopuertosPasajeros<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "AereopuertosPasajeros")

str(AereopuertosPasajeros)
AereopuertosPasajeros$Pasajeros = format(AereopuertosPasajeros$Pasajeros, big.mark=",")

ggplot(data=AereopuertosPasajeros, aes(x= Pasajeros, y= Aereopuertos, fill=Operación)) + 
  geom_bar(position = "dodge",stat='identity') +
  geom_text(aes(label=number((Pasajeros))), size = 5, color = "Black", position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c(verde,naranja))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(0.5)),
        legend.text = element_text(face="bold", colour=gris_letra))+
  labs(title = "Movimiento anual total de pasajeros por aeropuerto en 2019",
       x = "Pasajeros",
       y = "Aéreopuertos Nacionales",
       caption = "Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil. Se incluye el tráfico total por aeropuerto incluyendo pasajeros en tránsito y conexión.")
  

###2.8.2.10 Movimiento anual total de CARGA por aeropuerto en 2019

AereopuertosCarga<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "AereopuertosCarga")


ggplot(data=AereopuertosCarga, aes(x= Toneladas, y= Aereopuertos, fill=Operación)) + 
  geom_bar(position = "dodge",stat='identity') +
  geom_text(aes(label=number((Toneladas))), size = 5, color = "Black", position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c(verde,naranja))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(0.5)),
        legend.text = element_text(face="bold", colour=gris_letra))+
  labs(title = "Movimiento anual total de carga y correo (ton) por aeropuerto en 2019",
       x = "Toneladas",
       y = "Aéreopuertos Nacionales",
       caption = "Elaborado por Cámara de Comercio de Palmira con datos de Aerocivil. Se incluye el tráfico total de carga-correo transportada por aeropuerto.")



###Comercio exterior
  #Balanza comerciial

Balanza<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "Balanza")

Balanza = pivot_longer(Balanza, cols = c(2,3,4,5), names_to = "Año", values_to = "Millones")

Balanza$Año = as.character(Balanza$Año)
Balanza$Millones = as.numeric(Balanza$Millones)
Balanza$Millones = format(as.numeric(Balanza$Millones), big.mark=",")

ggplot(Balanza,aes(Año, Millones),show.legend = T) + 
  geom_bar(stat = "identity",position = position_dodge(),width = 0.5, aes(fill=Tipo)) + 
  scale_fill_manual(values = c(verde,naranja,gris)) +
  scale_y_continuous(breaks = c(-600000,800000,100000))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+
  geom_text(aes(label=number(Millones)), size = 5, color = "Black",position = position_dodge(0.9))+
  labs(title = "Balanza Comercial Palmira 2016 - 2019, Valores FOB Miles USD Precios Corrientes",
       x = "Año",
       y = "USD miles",
       caption = "Fuente: Elaborado por Departamento Planeación y Desarrollo Cámara de Comercio de Palmira con datos de: DANE")
  




##Productos de exportaciones


ProductosExp<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "ProductosExportaciones")

ProductosExp = pivot_longer(ProductosExp, cols = c(2,3,4,5), names_to = "Año", values_to = "Millones")


ggplot(ProductosExp,aes(Año, Millones),show.legend = T) + 
  geom_bar(stat = "identity",position = position_dodge(),width = 0.5, aes(fill=Productos)) + 
  scale_fill_manual(values = c(verde,naranja,gris,"steelblue")) +
  scale_y_continuous(breaks = c(0,23000000,1000000),limits = c(0,23000000))+
  theme(panel.background = element_rect(fill = "transparent"),
  plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Cenparent",color="white"),ter subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())+
        labs(title = "Comportamiento de las exportaciones de productos tradicionales de Palmira
        a Estados Unidos (2016-2019), millones de pesos.",
       x = "Año",
       y = "Millones",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: DANE")

##Exportaciones no convencionales 


ProductosNoConvencionalesExp<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "ProductosNoExportaciones")

ProductosNoConvencionalesExp = pivot_longer(ProductosNoConvencionalesExp, cols = c(2,3,4,5), names_to = "Año", values_to = "Millones")
ProductosNoConvencionalesExp$Año = as.character(ProductosNoConvencionalesExp$Año)

ggplot(ProductosNoConvencionalesExp,aes(Año,Millones,fill= Productos),show.legend = T) + 
  geom_bar(stat = "identity",position = position_dodge(),width = 0.5) + 
  scale_fill_manual(values = c(verde,naranja,gris,"steelblue","salmon","yellow","red","purple")) +
  scale_y_continuous(breaks = c(0,3000000,100000),limits = c(0,3000000))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())+
  labs(title = "Comportamiento de las exportaciones de productos no tradicionales de Palmira
        a Estados Unidos (2016-2019), millones de pesos.",
       x = "Año",
       y = "Millones",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: DANE")

##3.6.1.7 Comportamiento de las exportaciones según socios comerciales (2016-2019, millones de pesos)

SociosComerciales<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "3.6.1.7")

SociosComerciales = pivot_longer(SociosComerciales, cols = c(2,3,4,5), names_to = "Año", values_to = "Millones")

ggplot(SociosComerciales, aes(Año,reorder(País,Millones))) +
  geom_tile(aes(fill = Millones), color = "white",show.legend = F) +
  scale_fill_gradient2(low = "yellow", high = "Red",mid=naranja, midpoint = mean(SociosComerciales$Millones))+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        axis.text.y = element_text(angle = 0, vjust = 1,face="bold",size=15,color= "#AFAFB0"),panel.background = element_rect(fill = "transparent",color="white"),
        axis.text.x =  element_text(face="bold",size=15,color= "#AFAFB0"),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(title = "Comportamiento de las exportaciones según socios comerciales (2016-2019, millones de pesos)",
       x = "Año",
       y = "Socio comercial",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: DANE")+
  geom_text(aes(label= paste("$",number(Millones))),fontface = "bold")

##3.6.1.8 Participación (%) de las exportaciones por socios comerciales (2016-2019, millones de pesos)

SociosComerciales_2<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx",sheet = "3.6.1.8")

SociosComerciales_2 = pivot_longer(SociosComerciales_2, cols = c(2,3,4,5), names_to = "Año", values_to = "Porcentaje")

SociosComerciales_2$Año = as.character(SociosComerciales_2$Año)

ggplot(SociosComerciales_2,aes(Año,Porcentaje,fill= reorder(País,Porcentaje)),show.legend = T) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("steelblue","salmon","yellow","red","purple","darkblue","pink","darkgreen",gris,verde,naranja)) +
  scale_y_continuous(breaks = c(0,100,1),limits = c(0,100))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())+
  labs(title = "Participación (%) de las exportaciones por socios comerciales (2016-2019)",
       x = "Año",
       y = "Porcentaje",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: DANE",
        fill = "País")
      
     

###CAPITULO SECTOR EMPRESARIA

## 3.5.1 Empresas por tamaño (2016-2019)

TEmpresa<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.1TAmañoEmpresa")



ggplot(TEmpresa,aes(x=2, y=Número, fill=Tamaño))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(round(Número/sum(Número),3))),
            color="black",size=4.5, vjust=1, hjust = 1,position = position_dodge(width = 0.9),fontface ="bold")+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("orange","gray",verde,"purple"))+
  theme_void()+
  labs(title="Tamaño de las empresas 2019",
       x = NULL,
       y = NULL,
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Base de datos Registro Mercantil Cámara de Comercio de Palmira.")+
  xlim(0.5,2.5)+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"))


### 3.5.3  Empresas según ente jurídico en 2019

Tjuridico<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.3.juridico")

Tjuridico = pivot_longer(Tjuridico,cols = c(2,3,4,5),names_to = "Tamaño", values_to = "Número")


ggplot(Tjuridico, aes(x = Tamaño, y = reorder(Ente,Número))) +
  geom_tile(data = Tjuridico, aes(fill = Número), color = "white", show.legend = F) +
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y =  element_text(face="bold", colour= gris_letra, size=10))+# move caption to the left
  scale_fill_gradient(low= "#D0F5C9" , high  = "#6CE557")+ 
  labs(title = " Empresas según ente jurídico en 2019",
       x = "Tamaño de las empresas",
       y = "Ente jurídico",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: 
                  Base de datos Registro Mercantil Cámara de Comercio de Palmira.",
       legend = F)+
  geom_text(aes(label= number((Número))),fontface = "bold", size=3.5, col="black",hjust=0)


##CAncelacion de empresas

Cancelacion<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.8Cancelacion")


ggplot(Cancelacion, aes(Año, Empresas, fill = Acuerdo)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5)))+# move caption to the left
  
  scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
  labs(title = "Número de empresas canceladas por Acuerdo Final y Ley 1727 en el municipio (2017-2019)",
       y = "Número de empresas",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Base de datos Registro Mercantil Cámara de Comercio de Palmira.")+
  geom_text(aes(label= number(round(Empresas,0))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")


##CAPITULO SERVICIOS PUBLICOS

 #2.5.1.Suscriptores

SuscriptoresEnergia<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "2.5.1.Suscriptores")


ggplot(SuscriptoresEnergia, aes(reorder(Sector,Suscriptores), Suscriptores,fill=Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5)))+# move caption to the left
    scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue"))+ 
  labs(title = "Suscriptores del servicio de energía por sectores, zona urbana 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: EPSA",
       x = "Sector", 
       y = "Suscriptores")+
  geom_text(aes(label= number(round(Suscriptores,0))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")

#3.5.1.SuscriptoresEstrato

SuscriptoresEnergiaEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.1.SuscriptoresEstrato")

SuscriptoresEnergiaEstrato = subset(SuscriptoresEnergiaEstrato,Sector == "Residencial")

ggplot(SuscriptoresEnergiaEstrato, aes(Estrato, Suscriptores,fill=Zona)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5)),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(naranja,verde))+ 
  labs(title = "Suscriptores del servicio de energía del sector residencial año 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: EPSA",
       x = "Sector", 
       y = "Suscriptores")+
  geom_text(aes(label= number(round(Suscriptores,0))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")

##Consumo de energia por sector


ConsumoEnergia<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.1. ConsumoKw")

ggplot(ConsumoEnergia, aes(Sector,Kw,fill=Zona)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(naranja,verde))+ 
  labs(title = "Consumo de energía en Kw/Hr Acumulado por zona y sector año 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: EPSA",
       x = "Sector", 
       y = "Kw/Hr")+
  geom_text(aes(label= number(round(Kw,0))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")
##Suscriptores del servicio de acueducto

SuscriptoresAcueducto<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.2 SuscriptoresAgua")

ggplot(SuscriptoresAcueducto, aes(Sector,Suscriptores,fill = Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue"))+ 
  labs(title = "Suscriptores del servicios de acueducto 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Sector", 
       y = "Suscriptores")+
  geom_text(aes(label= number(round(Suscriptores))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")

## volumen agua facturado

ggplot(SuscriptoresAcueducto, aes(Volumen, reorder(Sector,Volumen),fill = Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(0),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue"))+ 
  labs(title = "Volumen de agua facturado básico año 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Volumen", 
       y = "Sector")+
  geom_text(aes(label= number(round(Volumen))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")


## Suscritores de acueducto por estrato

SuscriptoresAcueductoEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.2.SuscriptoresEstrato")

ggplot(SuscriptoresAcueductoEstrato, aes(Estrato,Suscriptores)) +
  geom_bar(stat="identity", position=position_dodge(),color = naranja,fill=naranja)+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue","Salmon"))+ 
  labs(title = "Suscriptores del servicio de acueducto del sector residencial por estrato",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Estrato", 
       y = "Suscriptores",
       fill = FALSE)+
  geom_text(aes(label= number(round(Suscriptores))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")

### Tarifa acueducto 2016 - 2019

TarifaAcueducto<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.3.TarifaAcueducto")

TarifaAcueducto = pivot_longer(TarifaAcueducto,cols = c(2,3,4,5,6),names_to = "Año",values_to = "Tarifa")
  

ggplot(TarifaAcueducto, aes(Año,Tarifa, fill = Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,"purple", verde))+ 
  labs(title = "Tarifa básica de acueducto 2015 - 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Año", 
       y = "Tarifa",
       fill = "Sector")+
  geom_text(aes(label= paste0("$",number(round(Tarifa)))),position=position_dodge(width=0.9), size = 3.7, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")

##Tarifa  por estrato


TarifaAcueductoEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.3.TarifaPorEstrato")

TarifaAcueductoEstrato = pivot_longer(TarifaAcueductoEstrato,cols = c(2,3,4,5,6),names_to = "Año",values_to = "Tarifa")
view(TarifaAcueductoEstrato)


ggplot(TarifaAcueductoEstrato, aes(Año,Tarifa, fill = Estrato)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,"purple","steelblue", verde))+ 
  labs(title = "Tarifa básica de acueducto por estrato, sector residencial 2015 - 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Año", 
       y = "Tarifa",
       fill = "Estrato")+
  geom_text(aes(label= paste0("$",format(round(as.numeric(Tarifa),0), nsmall = 0, big.mark = ","))),position=position_dodge(width=0.9), size = 3.0, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold",angle = 15 )
##Tarifa alcantarillado


TarifaAlcantarillado<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.3TarifaAlcantarilladoNoRes")

TarifaAlcantarillado = pivot_longer(TarifaAlcantarillado,cols = c(2,3,4,5,6),names_to = "Año",values_to = "Tarifa")

view(TarifaAlcantarillado)


ggplot(TarifaAlcantarillado, aes(Año,Tarifa, fill = Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,"purple","steelblue", verde))+ 
  labs(title = "Tarifa básica de alcantarillado, no residencial 2015 - 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Año", 
       y = "Tarifa",
       fill = "Sector")+
  geom_text(aes(label= paste0("$",format(round(as.numeric(Tarifa),0), nsmall = 0, big.mark = ","))),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold",angle = 0 )
##Tarifa alcantarillado residencial


TarifaAlcantarilladoEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "3.5.3.TarifaAlcantarilladoEstra")

TarifaAlcantarilladoEstrato = pivot_longer(TarifaAlcantarilladoEstrato,cols = c(2,3,4,5,6),names_to = "Año",values_to = "Tarifa")

view(TarifaAlcantarilladoEstrato)


ggplot(TarifaAlcantarilladoEstrato, aes(Año,Tarifa, fill = Estrato)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,"purple","steelblue", verde))+ 
  labs(title = "Tarifa básica de alcantarillado por estrato, sector residencial 2015 - 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Año", 
       y = "Tarifa",
       fill = "Estrato")+
  geom_text(aes(label= paste0("$",format(round(as.numeric(Tarifa),0), nsmall = 0, big.mark = ","))),position=position_dodge(width=0.9), size = 3, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold",angle = 15 )


##Suscriptores de alcantarillado
 

SuscritoresAlcantarillado<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "2.5.3SuscritoresAlcantarillado")


ggplot(SuscritoresAlcantarillado, aes(Suscriptores, reorder(Sector,Suscriptores), fill = Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue"))+ 
  labs(title = "Suscriptores del servicio de alcantarillado por sector año 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Suscriptores", 
       y = "Sector",
       fill = "Sector")+
  geom_text(aes(label= format(round(as.numeric(Suscriptores),0), nsmall = 0, big.mark = ",")),position=position_dodge(width=0.9), size = 3, color = "Black",vjust= -0.25,hjust= 0,
            fontface = "bold",angle = 0)


##Suscriptores alcalntarillado por estrato



SuscritoresAlcantarilladoEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "AlcantarilladoEstrato")


ggplot(SuscritoresAlcantarilladoEstrato, aes(Estrato,Suscriptores)) +
  geom_bar(stat="identity", position=position_dodge(),fill = verde, color =verde)+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue"))+ 
  labs(title = "Suscriptores del servicio de alcantarillado por estrato sector residencial año 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Estrato", 
       y = "Suscriptores",
       fill = "Estratp")+
  geom_text(aes(label= format(round(as.numeric(Suscriptores),0), nsmall = 0, big.mark = ",")),position=position_dodge(width=0.9), size = 3, color = "Black",vjust= -0.25,hjust= 0,
            fontface = "bold",angle = 0)


##Volumen vertimento



VolumenVertimento<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Graficos_Cámara.xlsx", sheet = "VolumenVertimento")


ggplot(VolumenVertimento, aes(reorder(Sector,Volumen),Volumen, fill = Sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde,"purple","steel blue"))+ 
  labs(title = "Volumen vertimento básico facturado año 2019",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: AquaOccidente",
       x = "Sector", 
       y = "Volumen",
       fill = "Sector")+
  geom_text(aes(label= format(round(as.numeric(Volumen),0), nsmall = 0, big.mark = ",")),position=position_dodge(width=0.9), size = 3, color = "Black",vjust= -0.25,hjust= 0,
            fontface = "bold",angle = 0)



#sector financiero

cartera<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "cartera")

cartera$Año= as.character(cartera$Año)

ggplot(cartera, aes(Año, Cartera,fill = Año)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Cartera neta por establecimientos bancarios en Palmira (2018-2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos")+
  geom_text(aes(label= number(Cartera)), size = 5, color = "Black",vjust= -0.25,hjust= 0,
            fontface = "bold",angle = 0)

## cartera compañias de financiamiento

cartera_com_finan<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "Cartera_com_fin")

cartera_com_finan$Año= as.character(cartera_com_finan$Año)
cartera_com_finan$Cartera = format(as.numeric(cartera_com_finan$Cartera), big.mark = ",")
ggplot(cartera_com_finan, aes(Año, Cartera,fill = Año)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Cartera neta por compañías de financiamiento en Palmira (2018-2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos")+
  geom_text(aes(label= Cartera), size = 5, color = "Black",vjust= -0.25,hjust= 0,
            fontface = "bold",angle = 0)

##cartera instituciones especiales

cartera_ins_esp<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "cartera_ins_esp")

cartera_ins_esp$Año= as.character(cartera_ins_esp$Año)
cartera_ins_esp$Cartera = format(as.numeric(cartera_ins_esp$Cartera), big.mark = ",")

ggplot(cartera_ins_esp, aes(Año, Cartera,fill = Año)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Cartera neta por instituciones oficiales especiales en Palmira (2018-2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos")+
  geom_text(aes(label= Cartera), size = 5, color = "Black",vjust= -0.25,hjust= 0,
            fontface = "bold",angle = 0)


####
cartera_tipo_est<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "cartera_tipo_est")


ggplot(cartera_tipo_est, aes(Cartera, reorder(Tipo,Cartera))) +
  geom_bar(stat="identity", fill = "steelblue")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Total cartera neta en Palmira por tipo de entidad (2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos",
       x = "Millones",
       y = "Tipo")+
  geom_text(aes(label= number(Cartera)), size = 4, color = "Black",vjust= -0.25,hjust= 0.6,
            fontface = "bold",angle = 0)

##Capataciones

captaciones<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "captaciones")

captaciones$Año = as.character(captaciones$Año)

ggplot(captaciones, aes(Año, Captaciones,fill =Año)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Captaciones por establecimientos bancarios en Palmira (2018-2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos",
       x = "",
       y = "Millones")+
  geom_text(aes(label= number(Captaciones)), size = 4, color = "Black",vjust= -0.25,hjust= 0.6,
            fontface = "bold",angle = 0)


###
captaciones_com_fin<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "captacionesComFin")

captaciones_com_fin$Año = as.character(captaciones_com_fin$Año)

ggplot(captaciones_com_fin, aes(Año, Captaciones,fill =Año)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Captaciones por compañías de financiamiento en Palmira (2018-2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos",
       x = "",
       y = "Millones")+
  geom_text(aes(label= number(Captaciones)), size = 4, color = "Black",vjust= -0.25,hjust= 0.6,
            fontface = "bold",angle = 0)


###

captaciones_tipo<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/Sector financiero/Financiero_R.xlsx", sheet = "Captaciones_tipo")

ggplot(captaciones_tipo, aes(Captaciones, reorder(Tipo,Captaciones))) +
  geom_bar(stat="identity", fill = "steelblue")+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Total captaciones en Palmira por tipo de entidad (2019, millones de pesos)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: con datos de Superintendencia Financiera de Colombia
*Datos en millones de pesos",
       x = "Millones",
       y = "Tipo")+
  geom_text(aes(label= number(Captaciones)), size = 4, color = "Black",vjust= -0.25,hjust= 0.6,
            fontface = "bold",angle = 0)


####vivienda 

VendidaEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "VendidasEstrato")

VendidaEstrato = pivot_longer(VendidaEstrato, cols = c(2,3,4,5,6), values_to = "Vivienda", names_to = "Estrato")



ggplot(VendidaEstrato, aes(x =Año, y = Vivienda, fill =Estrato)) +
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde,"steelblue","red","purple"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Unidades de vivienda vendidas según estrato socioeconómico (2016-2019)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle",
       x = "Año",
       y = "Viviendas vendidas")+
  geom_text(aes(label= number(Vivienda)), size = 4, color = "Black", position = position_dodge(width = 0.9),
            fontface = "bold",angle = 0)


## viviendas vendidas segun tipo

VendidasTipo<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "SegunTipo")

VendidasTipo = pivot_longer(VendidasTipo, cols = c(2,3,4), values_to = "Vivienda", names_to = "Tipo")



ggplot(VendidasTipo, aes(x =Año, y = Vivienda, fill =Tipo)) +
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde,"steelblue"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+ 
  labs(title = "Unidades de vivienda vendidas en Palmira según caracterización (2016-2019)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle",
       x = "Año",
       y = "Viviendas vendidas")+
  geom_text(aes(label= number(round(Vivienda,0))), size = 4, color = "Black", position = position_dodge(width = 0.9),
            fontface = "bold",angle = 0)

# ## viviendas ofertadas por estrato
# 
 OfertasEstrato<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "OfertadasEstrato")

OfertasEstrato = pivot_longer(OfertasEstrato, cols = c(2,3,4,5), values_to = "Vivienda", names_to = "Estrato")

OfertasEstrato$Vivienda = round(OfertasEstrato$Vivienda,0)

ggplot(OfertasEstrato, aes(x =Año, y = Vivienda, fill =Estrato)) +
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde,"steelblue","purple"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+
  labs(title = "Unidades de vivienda ofertadas en Palmira según estrato socioeconómico (2016-2019)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle",
       x = "Año",
       y = "Viviendas vendidas")+
  geom_text(aes(label= number(round(Vivienda,0))), size = 4, color = "Black", position = position_dodge(width = 0.9),
            fontface = "bold",angle = 0)
  

  ##VIVIENDA OFERTADA POR TIPO

OfertasTipo<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "OfertadasTipo")

OfertasTipo = pivot_longer(OfertasTipo, cols = c(2,3,4), values_to = "Vivienda", names_to = "Tipo")

OfertasTipo$Vivienda = round(OfertasTipo$Vivienda,0)

ggplot(OfertasTipo, aes(x =Año, y = Vivienda, fill =Tipo)) +
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde,"steelblue","purple"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+
  labs(title = "Unidades de vivienda ofertadas en Palmira según caracterización (2016-2019)",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle",
       x = "Año",
       y = "Viviendas vendidas")+
  geom_text(aes(label= number(round(Vivienda,0))), size = 4, color = "Black", position = position_dodge(width = 0.9),
            fontface = "bold",angle = 0)


###vivienda estado 

ViviendaEstado<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "VivendaEstado")

ViviendaEstado = pivot_longer(ViviendaEstado, cols = c(2,3,4), values_to = "Vivienda", names_to = "Estado")

OfertasTipo$Vivienda = round(OfertasTipo$Vivienda,0)

ggplot(ViviendaEstado, aes(x =Año, y = Vivienda, fill =Estado)) +
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde,"steelblue","purple"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+
  labs(title = "Número de viviendas culminadas, paralizadas y en proceso (2016p-2019p)*",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle",
       x = "Año",
       y = "Viviendas vendidas")+
  geom_text(aes(label= number(round(Vivienda,0))), size = 4, color = "Black", position = position_dodge(width = 0.9),
            fontface = "bold",angle = 0)




###vivienda estado y tipo 

ViviendaTipoEstado<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "ViviendaTipoEstado")



Gv= ggplot(ViviendaTipoEstado, aes(x =Vivienda, y = Tipo, fill =Estado)) +
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde,"steelblue","purple"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5),angle = 0),
        legend.text = element_text(face = "bold",colour = gris_letra, size =10))+# move caption to the left+
  labs(title = "Número de viviendas culminadas, paralizadas y en proceso (2016p-2019p)*",
       caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle",
       x = "Año",
       y = "Viviendas vendidas")+
  geom_text(aes(label= number(round(Vivienda,0))), size = 4, color = "Black", position = position_dodge(width = 0.9),
            fontface = "bold",angle = 0)
  
  Gv + facet_grid(Año~.)
  
  
  ##vievienda ofertada
  
  Viviendaofertada<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/Graficos Cámara comercio/VIVIENDA/vivienda_R.xlsx", sheet = "Viviendas ofertadas")
  
  
  Viviendaofertada$Año = as.factor(Viviendaofertada$Año)
  
  ggplot(Viviendaofertada, aes(Año,Vivienda,group =1)) +
    geom_line(aes(y=Vivienda),lwd=2, color = naranja) +
    theme_minimal() +
        labs(x = "Año",
         y = "Viviendas ofertadas",
         title = "Oferta de vivienda 2016 - 2019",
         caption = "Fuente: Elaborado por Cámara de Comercio de Palmira con datos de: Camacol del Valle")+
    theme(panel.background = element_rect(fill = "transparent",color="white"),
          plot.title = element_text(hjust = 0, size = 14, face = "bold"),    # Center title position and size
          plot.caption = element_text(hjust = 0, face = "italic"),
          axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1.5)),
          legend.text = element_text(face = "bold",colour = gris_letra, size = rel(1.5)),)+
    geom_text_repel(aes(x = Año, y=Vivienda,label= number(Vivienda)),size  =  4,
                    nudge_x = 0,nudge_y = 0,vjust="top",hjust="center",color = "black",
                    segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White")
