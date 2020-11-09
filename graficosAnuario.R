
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
gris_letra= c("#606565")

##Gráficos capitulo I Datos generales

# Gráfico presipitaciones

Precipitaciones <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/1. Datos Generales/Presepitaciones_temperatura.xlsx",sheet = "Precipitaciones")
Temperatura <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/1. Datos Generales/Presepitaciones_temperatura.xlsx",sheet = "Temperatura")

Precipitaciones = Precipitaciones %>% pivot_longer(cols = c(3,4),names_to ="Año",values_to = "Precipitaciones")
Precipitaciones$Año = as.character(Precipitaciones$Año)
Precipitaciones <- Precipitaciones[with(Precipitaciones,order(Año)),]


#Precipitaciones

ggplot(Precipitaciones, aes(as.numeric(No_mes), Precipitaciones, fill = Año)) +
  geom_point(aes(),show.legend=T,color = gris) +
  geom_line(lwd=3,aes(color = Año))+
  theme_minimal()+
  scale_color_manual(values=c(verde,naranja))+
  scale_x_discrete(limits = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
                              "Agosto","Septiembre","Octubre","Noviembre","Diciembre"))+
    labs(x = "Mes", y = "Precipitaciones (mm)", title = "Precipitaciones,  2018 - 2019",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal con datos de: Sistema de Información para la gestión de datos Hidrológicos y Meteorológicos (DHIME) del Instituto de Hidrología,
                  Meteorología y Estudios Ambientales (IDEAM)")+
  theme(plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_y_continuous(limits = c(0,max(Precipitaciones$Precipitaciones,50), breaks = seq(0,max(Precipitaciones$Precipitaciones,50))))+
  geom_text_repel(aes(x = Mes, y=Precipitaciones,label=Precipitaciones),size  = 3,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="center",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White")


#Temperatura

Temperatura = Temperatura %>% pivot_longer(cols = c(3,4),names_to ="Año",values_to = "Temperatura")
Temperatura$Año = as.character(Temperatura$Año)
Temperatura <- Temperatura[with(Temperatura,order(Año)),]


ggplot(Temperatura, aes(as.numeric(No_mes), Temperatura,fill = Año)) +
  geom_point(aes(), color = gris,show.legend=T) +
  geom_line(lwd=2,aes(color = Año))+
  scale_color_manual(values=c(verde,naranja))+
  scale_fill_manual(values=c(verde,naranja))+
  scale_x_discrete(limits = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))+
  labs(x = "Mes", y = "Temperatura (mm)", title = "Temperatura,  2018 - 2019",
       caption = "Fuente:Elaborado por la Secretaría de Planeación Municipal con datos de Sistema de Información para la gestión de datos Hidrológicos
                  y Meteorológicos (DHIME) del Instituto de Hidrología,Meteorología y Estudios Ambientales (IDEAM)")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"))+
  geom_text_repel(aes(x = Mes, y=Temperatura,label=Temperatura),size  = 3,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="center",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White")



##Gráficos captitulo II Demográfia


    #Cargar tabla datos de proyección demográfica
Pob_proyeccion <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/R_Población.xlsx")
Pob_proyeccion = Pob_proyeccion %>% mutate(Total = Mujeres + Hombres)
pob = pivot_longer(Pob_proyeccion, cols = c(Hombres,Mujeres,Total), names_to = ("Sexo"), values_to = "Poblacion")

pob = pob %>% subset(Censo>=2018)

pob$Censo= as.factor(pob$Censo)
pob$Sexo=as.character(pob$Sexo)
pob$Poblacion = as.integer(pob$Poblacion)

pob$Poblacion = round(pob$Poblacion/1000,4)
pob = select(pob,1,3,4)
pob = as.data.frame(pob)

str(pob)

p <- ggplot(pob) + scale_y_continuous(limits = c(0, 392200), breaks = NULL)
p1 <- p + geom_bar(aes(Censo, Poblacion), stat = "identity", fill = "#E9EBED", )
  
p1
p2 <- p1 + geom_bar(aes(Censo, Poblacion, fill = Sexo),stat = "identity", position = "dodge") 

p2+ theme(panel.background = element_rect(fill = "transparent",color="white"),
       plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
       plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
       plot.caption = element_text(hjust = 0, face = "italic"),
       axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1)),
       legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c("#5D771B","#F9CB6A", "#E9EBED"))+
     labs(title = "Proyecciones de la población del Municipio de Palmira,  por sexo 2018 - 2035",
       x = "Año",
       y = "Población",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Proyecciones del  Censo Nacional de Población y Vivienda 2018. DANE.")+
  geom_text(aes(x= Censo,y=Poblacion,label = number(Poblacion)),vjust = 1, angle = 0,size=3, position = position_dodge(width = 0.9),
              fontface = "bold", colour = "black")


#Pirámide Poblacional 2019

tabla_pir <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/R_piramide2019.xlsx",sheet = "Hoja1")










tabla_pir$Mujeres = format(round(as.numeric(tabla_pir$Mujeres), 0), nsmall=0, big.mark=".")

piramide = par(mar=pyramid.plot(tabla_pir$Hombres,tabla_pir$Mujeres,labels=tabla_pir$Edad,
                     main="Pirámide poblacional del Municipio de Palmira 2019",
                                       lxcol=c("#2ACA5F"),rxcol=c("#F79E0B"),
                     gap=800,space=1.8,top.labels=c("Hombres","Rango de edad","Mujeres"),show.values=TRUE,labelcex=0.7,
                     ndig=0))

piramide 
  




## Tasa de natalidad


Natalidad_Mortalidad<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/R_Natalidad_mortalidad.xlsx",sheet ="Hoja1")

Natalidad_Mortalidad$`Tasa Bruta de Natalidad` = round(Natalidad_Mortalidad$`Tasa Bruta de Natalidad`,2)
Natalidad_Mortalidad$`Tasa Bruta de Mortalidad` = round(Natalidad_Mortalidad$`Tasa Bruta de Mortalidad`,2)

Natalidad_Mortalidad$Año = as.character(Natalidad_Mortalidad$Año)

Natalidad_Mortalidad = Natalidad_Mortalidad %>% dplyr::rename(TN = "Tasa Bruta de Natalidad")
Natalidad_Mortalidad = Natalidad_Mortalidad %>% dplyr::rename(TM = "Tasa Bruta de Mortalidad")
#Grafico de Tasa de Natalidad


Natalidad_Mortalidad$aAño = as.factor(Natalidad_Mortalidad$Año)
  
 ggplot(Natalidad_Mortalidad, aes(Año,TN,group =1)) +
 geom_line(aes(y=TN),lwd=2, color = naranja) +
   theme_minimal() +
  labs(x = "Año",
       y = "Tasa",
       title = "Tasa Bruta de Natalidad Municipio de Palmira 2005 - 2019",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal con datos de: Estadísticas vitales de nacimientos y defunciones, Con corte a diciembre del 2019, DANE.")+
   theme(panel.background = element_rect(fill = "transparent",color="white"),
         plot.title = element_text(hjust = 0, size = 14, face = "bold"),    # Center title position and size
         plot.caption = element_text(hjust = 0, face = "italic"),
         axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1.5)))+
    geom_text_repel(aes(x = Año, y=TN,label=TN),size  =  4,
                    nudge_x = 0,nudge_y = 0,vjust="center",hjust="center",
                    segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White")+
   scale_y_continuous(limits = c(9,max(Natalidad_Mortalidad$TN)),
                      breaks = seq(9,max(Natalidad_Mortalidad$TN),1))
   
 
 
 
 #Grafico de Tasa de Mortalidad
 
 ggplot(Natalidad_Mortalidad, aes(Año,TM,group =1)) +
   geom_line(aes(y=TM),lwd=2, color = naranja) +
   theme_minimal() +
   labs(x = "Año",
        y = "Tasa",
        title = "Tasa Bruta de Mortalidad Municipio de Palmira 2005 - 2019",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal con datos de: Estadísticas vitales de nacimientos y defunciones, Con corte a diciembre del 2019, DANE.")+
   theme(panel.background = element_rect(fill = "transparent",color="white"),
         plot.title = element_text(hjust = 0, size = 14, face = "bold"),    # Center title position and size
         plot.caption = element_text(hjust = 0, face = "italic"),
         axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1.5)))+
   geom_text_repel(aes(x = Año, y=TM,label=TM),size  =  4,
                   nudge_x = 0,nudge_y = 0,vjust="center",hjust="center",
                   segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White")+
   scale_y_continuous(limits = c(4.5,max(Natalidad_Mortalidad$TM)),
                      breaks = seq(4.5,max(Natalidad_Mortalidad$TM),1))
 
 
 ##apendice del censo
 
 Tipo_vivienda <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/Otros.xlsx", sheet = "TipoVivienda")
 
 ggplot(Tipo_vivienda,aes(y=reorder(Tipo,Porcentaje),x=Porcentaje))+
   geom_bar(stat = "identity",color="white",fill = naranja)+
   geom_text(aes(label=paste(Porcentaje,"%")),color="black",size=4, vjust = -0.25, fontface = "bold")+
   labs(title="Distribución de la vivienda según tipo",
        x = "Nivel educativo",
        y = "Estudiantes matriculados",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, 
                  con datos de:Censo Nacional de Población y Vivienda 2018.DANE")+
   theme(panel.background = element_rect(fill = "transparent",color="white"),
     legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         plot.title = element_text(size=14,face="bold", hjust = 0),
         axis.title=element_text(size=10,face="bold",color = "white"),
         plot.caption = element_text(hjust = 0, face = "italic"),
         legend.position = "none",
         axis.text.y = element_text(size=10,face="bold",color = gris_letra,hjust = 1),
         axis.text.x  = element_text(size=10,face="bold",color = gris_letra),
         strip.text.y  = element_text(angle = 0, hjust = 0),
        axis.ticks =  element_blank(),
         axis.title.y = element_text(hjust = 0))+
   scale_x_continuous(limits = c(-5,100,10),breaks = seq(-5,100,10))
  
 
 
 ## Hogares por vivienda
 
 HogaresVivienda <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/Otros.xlsx", sheet = "HogaresVivienda")
 
HogaresVivienda$Año = as.factor(HogaresVivienda$Año)
 
 ggplot(HogaresVivienda, aes(Numero, Porcentaje, fill = (Año))) + 
   geom_bar(stat = "identity", position = position_dodge()) +
   scale_fill_manual(values = c(verde, naranja))+
   theme(panel.background = element_rect(fill = "transparent",color="white"),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         plot.title = element_text(size=14,face="bold", hjust = 0),
         axis.title=element_text(size=10,face="bold",color = "white"),
         plot.caption = element_text(hjust = 0, face = "italic"),
         axis.text.y = element_text(size=10,face="bold",color = gris_letra,hjust = 1),
         axis.text.x  = element_text(size=10,face="bold",color = gris_letra),
         strip.text.y  = element_text(angle = 0, hjust = 0),
        axis.ticks =  element_blank(),
         axis.title.y = element_text(hjust = 0))+
   scale_y_continuous(limits = c(0,100,10),breaks = seq(0,100,10))+
   geom_text(aes(label = paste0(Porcentaje,"%")),position = position_dodge(width = 0.9),vjust =0.25, fontface="bold")+
   labs(title = "Número de hogares por vivienda, censos 2005 -  2018",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de:Censo Nacional de Población y Vivienda 2018.DANE")
 
 ### Personas por hogar
  
 library(scales)
 
 
 PersonasHogar<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/Otros.xlsx", sheet = "PersonasHogar")
 
 PersonasHogar$Personas = as.factor(PersonasHogar$Personas)
 
 p1 <- ggplot(PersonasHogar, aes(factor(Año), Porcentaje, fill = Personas)) + 
   geom_bar(stat = "identity", width = 1, size = 1, color = "white") +
   coord_polar("y") + 
   theme_void() +
   theme(legend.position = "none") +
   scale_fill_manual(values = c(verde, naranja,"red","purple","salmon"))+
   labs(title = "Distribución del Número de personas por hogar Censos 2005 - 2018",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de:Censo Nacional de Población y Vivienda 2018.DANE")+
   theme(legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         plot.title = element_text(size=14,face="bold"),
         axis.title=element_text(size=10,face="bold",color = "white"),
         plot.caption = element_text(hjust = 0, face = "italic"),
         legend.position = "none")
 
 p1
 
 # se crea una base de datos solo para los años 
 foo <- data.frame(cumsum(table(PersonasHogar$Año)) + 1:length(unique(PersonasHogar$Año)))
 foo$Año <- rownames(foo)
 colnames(foo)[1] <- "row"
 
 foo
 
 PersonasHogar$row <- which(do.call(rbind, by(PersonasHogar, PersonasHogar$Año, rbind, ""))$Año != "")
 
 p2 <- ggplot(PersonasHogar, aes(y = row)) + 
   geom_point(aes(0, color = Personas), size = 8) +
   geom_text(data = foo, aes(0, label = rev(Año)), size = 4, color = "grey50") +
   geom_text(aes(0.7, label = paste0(Personas, ":", Porcentaje, "%")), fontface = "bold", color=gris_letra) +
   theme_void() +
   theme(legend.position = "none") +
   scale_x_discrete() +
   scale_color_manual(values = c(verde, naranja,"red","purple","salmon"))
 p2
 
 library(egg)
 ggarrange(p1, p2, nrow = 1, widths = c(2, 1))
 
 
 # Migración
 
 Migracion<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/Otros.xlsx", sheet = "Migracion")
 
 ggplot(Migracion, aes(x=Porcentaje, y=reorder(Lugar,Porcentaje))) +
   geom_bar(stat = "identity",color="white",fill = verde)+
   geom_text(aes(label=paste(Porcentaje,"%")),color="black",size=4, vjust = -0.25, fontface = "bold")+
   labs(title="Lugar de procedencia",
        x = "",
        y = "",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal,con datos de:Censo Nacional de Población y Vivienda 2018.DANE")+
   theme(panel.background = element_rect(fill = "transparent",color="white"),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         plot.title = element_text(size=14,face="bold", hjust = 0),
         axis.title=element_text(size=10,face="bold",color = "white"),
         plot.caption = element_text(hjust = 0, face = "italic"),
         legend.position = "none",
         axis.text.y = element_text(size=10,face="bold",color = gris_letra,hjust = 1),
         axis.text.x  = element_text(size=10,face="bold",color = gris_letra),
         strip.text.y  = element_text(angle = 0, hjust = 0),
         axis.ticks =  element_blank(),
         axis.title.y = element_text(hjust = 0))+
   scale_x_continuous(limits = c(-5,100,10),breaks = seq(-5,100,10))
  
 
   
 
 
 ##Etnia
 
 
 etnia<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/2. Demográfia/Otros.xlsx", sheet = "Etnia")
 etnia = subset(etnia, Zona == "Total" )
 
 ggplot(etnia,aes(y=reorder(Etnia,Porcentaje),x=Porcentaje))+
   geom_bar(stat = "identity",color="white",fill = naranja)+
   geom_text(aes(label=paste(Porcentaje,"%")),color="black",size=4, vjust = -0.25, fontface = "bold")+
   scale_fill_manual(values=c("salmon","steelblue","orange","gray",verde,"purple","red"))+
      labs(title="Distibución porcentual del autoreconocimiento  étnico de la población",
        x = "",
        y = "",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de:Censo Nacional de Población y Vivienda 2018.DANE")+
   theme(panel.background = element_rect(fill = "transparent",color="white"),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         plot.title = element_text(size=14,face="bold", hjust = 0),
         axis.title=element_text(size=10,face="bold",color = "white"),
         plot.caption = element_text(hjust = 0, face = "italic"),
         legend.position = "none",
         axis.text.y = element_text(size=10,face="bold",color = gris_letra,hjust = 1),
         axis.text.x  = element_text(size=10,face="bold",color = gris_letra),
         strip.text.y  = element_text(angle = 0, hjust = 0),
         axis.ticks =  element_blank(),
         axis.title.y = element_text(hjust = 0))+
   scale_x_continuous(limits = c(-5,100,10),breaks = seq(-5,100,10))
 
 
   

 ##CAPITULO 3 EDUCACIÓN
 
 #Docentes colegios oficiales por zona y sexo
 
Docentes_oficiales <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/4. Educación/Educacion_R.xlsx",sheet = "Hoja2")
 
 
names(Docentes_oficiales)
 


ggplot(Docentes_oficiales, aes(Zona, Docentes, fill = Sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
     theme(panel.background = element_rect(fill = "transparent",color="white"),
         plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
         plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
         plot.caption = element_text(hjust = 0, face = "italic"),
       axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5)))+# move caption to the left
          
   scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
   labs(title = "Docentes de Instituciones Educativas Oficiales por zona y sexo 2019",
        x = "Zona",
        y = "Número de docentes",
        caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Educación Municipal")+
   geom_text(aes(label= Docentes),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")
  
 

#Instituciones educativas por  zona y calendario

Instituciones <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/4. Educación/Educacion_R.xlsx",sheet = "Hoja3")
 
names(Instituciones)
Instituciones$Año = as.factor(Instituciones$Año)

Instituciones = Instituciones %>% group_by(Zona,Año) %>% summarise(Instituciones = sum(Instituciones))

ggplot(Instituciones, aes(Zona, Instituciones, fill = Año)) +
  geom_bar( stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1.5)))+# move caption to the left
  
  scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
  labs(title = "Instituciones Educativas por zona 2018 - 2019",
       x = "Zona",
       y = "Número de IE",
       caption = "Fuente:Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Educación Municipal")+
  geom_text(aes(label= Instituciones),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")


## Matricula

Matricula <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/4. Educación/Educacion_R.xlsx",sheet = "Hoja1")
str(Matricula)
names(Matricula)

Matricula = pivot_longer(Matricula, cols = c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), names_to = ("Grado"), values_to = "Alumnos")

Matricula = Matricula %>% mutate(Sexo=substr(Grado ,start = 1,stop = 1))
Matricula = Matricula %>% mutate(Nivel=substr(Grado ,start = 2,stop = length(Grado)))
Matricula = Matricula %>% mutate(Sexo=ifelse(Sexo=="H","Hombres",ifelse(Sexo=="M","Mujeres","")))
Matricula = Matricula %>% mutate(Nivel=ifelse(Nivel=="Preesocolar","1.Preescolar",ifelse(Nivel=="Prejardín","2.Prejardín",
                                              ifelse(Nivel=="Jardín","3.Jardín",ifelse(Nivel=="Primaria","4.Primaria",
                                              ifelse(Nivel == "Secundaria","5.Secundaria",ifelse(Nivel=="Media","6.Media",
                                              ifelse(Nivel == "Ciclos","7.Ciclos",ifelse(Nivel=="Discapacitados","8.Pob. con discapacidad","")))))))))

                                              
names(Matricula)
Matricula = as.data.frame(Matricula)

Matricula_1 = Matricula %>% group_by(Nivel,Sexo) %>% summarise(Alumnos = sum(Alumnos))

Matricula_1$Alumnos = format(Matricula_1$Alumnos,big.mark = ",")
Matricula_1$Alumnos = as.numeric(Matricula_1$Alumnos)

Matricula_sexo = group_by(Matricula_1,Sexo) %>% summarise(Alumnos = sum(Alumnos))
#Matricula por nivel

ggplot(Matricula_1, aes(Nivel,Alumnos, fill = Sexo))+
  geom_bar(stat="identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1), angle=0))+# move caption to the left
    scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
  labs(title = "Total de estudiantes matriculados por nivel en el municipio 2019",
       x = "Nivel educativo",
       y = "Estudiantes matriculados",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Educación Municipal")+
  geom_text(aes(label= Alumnos),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")

##Alumnos por Nivel y tipo de IE público y privado

Matricula_2 <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/4. Educación/Matricula_3.xlsx")
Matricula_2$Porcentaje = Matricula_2$Porcentaje*100


ggplot(Matricula_2, aes(Naturaleza,Nivel)) +
  geom_tile(aes(fill = Porcentaje), color = "white",show.legend = F) +
  scale_fill_gradient(low = "white", high = naranja)+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        axis.text.y = element_text(angle = 0, vjust = 1,face="bold",size=15,color= "#AFAFB0"),panel.background = element_rect(fill = "transparent",color="white"),
        axis.text.x =  element_text(face="bold",size=15,color= "#AFAFB0"),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(title = " Porcentaje de alumnos matriculados en IE Ofciales y Privadas 2019",
       x = "Nivel educativo",
       y = "Estudiantes matriculados",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Educación Municipal")+
  geom_text(aes(label= paste(Porcentaje,"%")),fontface = "bold")

  

##Nivel educativo docentes

Docentes_niveleducativo <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/4. Educación/Docentes_niveleducativo.xlsx")


ggplot(Docentes_niveleducativo,aes(x=2,y=Porcentaje, fill=Nivel_Educativo))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Porcentaje/100)),
            position=position_stack(vjust=0.5),color="black",size=3)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray",verde,"purple"))+
  theme_void()+
  labs(title="Distribución porcentual nivel académico docentes IE Oficiales 2019",
       x = "Nivel educativo",
       y = "Estudiantes matriculados",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Educación Municipal")+
  xlim(0.5,2.5)+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
               plot.caption = element_text(hjust = 0, face = "italic"))


##Torta para IE privadas
Docentes_niveleducativo <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/4. Educación/Docentes_niveleducativo.xlsx",sheet="Hoja2")

ggplot(Docentes_niveleducativo,aes(x=2,y=Porcentaje, fill=Nivel_Educativo))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Porcentaje/100)),
            position= position_jitter(h=1,w=0.5),color="black",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("orange","gray",verde,"purple"))+
  theme_void()+
  labs(title="Distribución porcentual nivel académico docentes IE Privadas 2019",
       x = "Nivel educativo",
       y = "Estudiantes matriculados",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Educación Municipal")+
  xlim(0.5,2.5)+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"))

### GRAFICOS EDUCACIÓN - PARTE CAMARA

## Nivel de icfes de colegios publicos

  ggplot(Clasificacion_col, aes(Clasificación,Número, fill = años))+
  geom_bar(stat="identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(3), angle=0))+# move caption to the left
  scale_fill_manual(values = c("salmon","steelblue","purple",verde,naranja))+ 
  labs(title = "Clasificación ICFES de Instituciones Educativas Oficiales, 2015 - 2019",
       x = "Clasificación ICFES",
       y = "Número de IE",
       caption = "Fuente: Elaborado por la Cámara de Comercio de Palmira, con datos de: ICFES")+
  geom_text(aes(label= Número),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")




## Gráficos capitulo IV Salud

Base_atenciones <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Base_atenciones.xlsx",sheet = "Hoja2")

Base_atenciones = Base_atenciones %>% subset(Año == 2019)
Base_atenciones = Base_atenciones %>% subset(Rango != "R")
Base_atenciones = Base_atenciones %>% subset(Rango != "D")
Base_atenciones = Base_atenciones %>% subset(Descripicion != "NO DEFINIDO-NO DEFINIDO")
Base_atenciones = Base_atenciones %>% subset(Descripicion != "C22-CODIGOS PARA PROPOSITOS ESPECIALES")
Base_atenciones = Base_atenciones %>% subset(Sexo !="D")
Base_atenciones = Base_atenciones %>% subset(Sexo !="R")

Base_atenciones = Base_atenciones %>% group_by(Descripcion,Rango) %>% summarise(Consultas=sum(Consultas))

write.csv(Base_atenciones,"D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Consultas_rango.csv")

Atenciones_2019 <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Atenciones_2019.xlsx")

names(Atenciones_2019)
# Grafico de Atenciones por sexo

ggplot(Atenciones_2019, aes(Porcentaje,reorder(Descripcion,Consultas), fill = Sexo))+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(0.3),angle = 45))+# move caption to the left
  scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
  labs(title = "Distribución porcentual por sexo de las veinte causas de consulta, año 2019",
       x = "Porcentaje de consulta",
       y = "Enfermedades  consultadas",
       caption = "Fuente: Elaborado por Secretaría de Planeación Municipal con información de: Sivigila, Secretaría de Salud Municipal,
       datos preliminares con corte a diciembre de 2019.")+
   geom_text(aes(label= paste(Porcentaje,"%")),fontface = "bold", position = position_stack(), size=4, vjust=0.5, hjust=2.5 ,col="black")

##Grafico consultas por rango de edad

Consultas_rango <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Consultas_rango.xlsx")

ggplot(Consultas_rango, aes(Rango_edad,reorder(Descripcion,Consultas))) +
  geom_tile(aes(fill = Porcentaje), color = "white",show.legend = F) +
  scale_fill_gradient(low = "White", high = naranja,guide = "colourbar",
                      aesthetics = "fill")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        axis.text.y = element_text(angle = 0, vjust = 1,face="bold",size=8,color= gris_letra),panel.background = element_rect(fill = "transparent",color="white"),
        axis.text.x =  element_text(face="bold",size=10,color= gris_letra),
        plot.caption=element_text(hjust = 0,face = "italic"))+
  labs(title = "Distribución porcentual de las causas de consultas por etapa de vida, año 2019",
       x = "Etapas ciclo de vida",
       y = "Enfermedades  consultadas",
       caption = "Fuente: Elaborado por Secretaría de Planeación Municipal con información de: Sivigila, Secretaría de Salud Municipal,
                datos preliminares con corte a diciembre de 2019.")+
  geom_text(aes(label= paste(Porcentaje,"%")),fontface = "bold")



#Eventos de salud pública

Eventos <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Eventos_salud_publica.xlsx")

Eventos = pivot_longer(Eventos,cols = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),names_to = "Edad",values_to = "No_eventos")

Eventos = Eventos %>% group_by(EVENTOS) %>% summarise(No_eventos = sum(No_eventos))


ggplot(Eventos, aes(No_eventos,reorder(EVENTOS,No_eventos)),color= verde)+
  geom_bar(stat = "identity",fill = verde)+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(0.3),angle = 45))+# move caption to the left
  scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
  labs(title = "Enfermedades de notificación obligatoria, año 2019",
       x = "Número de eventos",
       y = "Enfermedades",
       caption = "Fuente: Elaborado por Secretaría de Planeación Municipal con información de: Sivigila, Secretaría de Salud Municipal,
                datos preliminares con corte a diciembre de 2019.")+
  geom_text(aes(label= (No_eventos)),fontface = "bold", size=3.5, col="black",hjust=0)


##Eventos por edad

write.csv(Eventos,"D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Eventos.csv")

Eventos <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/5. Salud/Eventos.xlsx")

Eventos = Eventos %>% group_by(EVENTOS,Etapa) %>% summarise(Total = sum(No_eventos))
Eventos = as.data.frame(Eventos)
Eventos = subset(Eventos,Eventos$Total>=1)


ggplot(Eventos, aes(x = Etapa, y = reorder(EVENTOS,Total))) +
  geom_tile(data = Eventos, aes(fill = Total)) +
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10))+# move caption to the left
  scale_fill_gradient(low= "#D9DEDB" , high  = naranja)+ 
  labs(title = "Enfermedades de notificación obligatoria, año 2019",
       x = "Número de eventos",
       y = "Enfermedades",
       caption = "Fuente: Elaborado por Secretaría de Planeación Municipal con información de: Sivigila, Secretaría de Salud Municipal,
                datos preliminares con corte a diciembre de 2019.")+
  geom_text(aes(label= (Total)),fontface = "bold", size=3.5, col="black",hjust=0)
  


## Cobertura sisben

Sisben = Sisben %>% group_by(Nivel) %>% summarise(Total=sum(Total))
ggplot(data = Sisben, aes(x = Nivel, y = Total)) +
  geom_col(aes(fill = factor(Nivel))) +
  labs(x = "", y = "Median Ozone Level") +
  coord_polar("y") +
  scale_fill_brewer(palette = "Set1", name = "Season:") +
  theme(axis.ticks = element_blank())


#####  RECREACION Y ZONAS VERDES

Eventos<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Recreación y zonas verdes/R_cultura.xlsx",sheet="Hoja2")

Eventos= pivot_longer(Eventos, cols = c(2,3), names_to = "Rol", values_to = "Número")

Eventos = subset(Eventos,Tipo_de_evento != "Total")

ggplot(Eventos, aes(Número,reorder(Tipo_de_evento,Número), fill = Rol))+
  geom_bar(stat="identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(1), angle=0),
        axis.text.y =  element_text(face="bold", colour=gris_letra, size=rel(1.5), angle=0),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c( "#2ACA5F","#F79E0B","#AFAFB0"))+ 
  labs(title = "Eventos culturales 2019",
       x = "Número de eventos",
       y = "Tipo de evento",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Cultura")+
  geom_text(aes(label= Número),position=position_dodge(width=0.9), size = 4, color = "Black",vjust= -0.25,hjust="center",
            fontface = "bold")


####CAPITULO SEGURIDAD

Homicidios <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Hoja2")

Homicidios = pivot_longer(Homicidios, cols = c(2,4,6), values_to = "No.Homicidios", names_to = "Casos")
Homicidios$Año = as.character(Homicidios$Año)
Homicidios = Homicidios %>%  arrange((desc(Año)))

ggplot(Homicidios, aes(x = Año, y = No.Homicidios,fill = Casos))+
  geom_bar(aes(fill = fct_rev(Casos)), stat = "identity", position = position_stack(reverse = F))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde))+
  labs(title = "Casos de homicidios por sexo 2014 - 2019",
       x = "Año",
       y = "No de casos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís
              Delictivo-CIADPAL con información de Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal")+
  scale_y_discrete("No.Homicidios",breaks = c(" No.Homicidios"))+
  geom_text(aes(label= No.Homicidios), size = 4, color = "Black", position = position_stack(reverse =T),vjust = 0.8)

#Tasas 

Tasas <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Hoja3")

Tasas = pivot_longer(Tasas, cols = c(2,3,4), values_to = "Tasa", names_to = "Tasas")

legend_title <- "Tasas"
ggplot(Tasas, aes(Año,Tasa,fill = Tasas ,color = Tasas),show.legend = T) +
  geom_line(lwd=3)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(gris,naranja,verde))+
labs(title = "Tasa de homicidios por 100.000 habitantes, 2014 - 2019",
     x = "Año",
     y = "Tasa de homicidios por 100 mil Hab.",
     caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
             Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
 geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")


# Homicidios por rango de edad  

HomicidiosEdad = read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Hoja5")
HomicidiosEdad = pivot_longer(HomicidiosEdad, cols = c(2,3), values_to = "Homicidios", names_to = "Año")
HomicidiosEdad = subset(HomicidiosEdad, Homicidios !=0)


HomicidiosEdad$Edad = factor(HomicidiosEdad$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                           "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                            levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                     "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))




ggplot(HomicidiosEdad, aes(Edad,Homicidios, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris,"steel blue"))+ 
  labs(title = "Homicidios por rango de edad, 2018 - 2019",
       x = "Rango de Edad",
       y = "Homicidios",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
             Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.")+
  geom_text(aes(label= (Homicidios)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))

##Homicidios por comuna

library(viridis)
if(!require(pacman)) install.packages("pacman")
library(ggbump)
pacman::p_load(tidyverse, cowplot, wesanderson)

Homicidioscomuna = read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Homicidios_por comuna")


Homicidioscomuna$Comuna= as.character(Homicidioscomuna$Comuna)

Urbanas = subset(Homicidioscomuna,Zona == "Urbano")
Rurales = subset(Homicidioscomuna,Zona == "Rural")


p1 = ggplot(Urbanas, aes(Año, Ranking, color = Comuna,fill=Comuna)) +
    geom_bump(size = 1, smooth = 8 )+
  geom_point(size = 4) +
  geom_text(data = Urbanas %>% filter(Año == min(Año)),
            aes(x = Año - 0.2, label = Tasa), size = 5, hjust = 0) +
  geom_text(data = Urbanas %>% filter(Año == max(Año)),
            aes(x = Año + 0.1, label = Tasa), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(2017.8, 2019.5),
                     breaks = seq(2018, 2019, 1)) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_text(face="bold", colour= gris_letra, size=14),                         
        legend.text = element_text(face="bold", colour=gris_letra)) +
        labs(y = "RANK",
       x = "Año",
       title = "Comparativo tasa de homicidios por 100.000 habitantes, 2018 - 2019",
       subtitle = "Zona Urbana",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: La Secretaría de Seguridad Municipal.
            Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal. 
            Tasa por 100.000 habitantes, Censo 2018, DANE.") +
       scale_y_reverse(breaks = seq(1,7,1))+
     scale_color_manual(values = c("red","green","gray","#F11385","steelblue",naranja,"purple"))



p2 = ggplot(Rurales, aes(Año, Ranking, color = Comuna,fill=Comuna)) +
  geom_bump(size = 1, smooth = 8 )+
  geom_point(size = 4) +
  geom_text(data = Rurales %>% filter(Año == min(Año)),
            aes(x = Año - 0.2, label = Tasa), size = 5, hjust = 0) +
  geom_text(data = Rurales %>% filter(Año == max(Año)),
            aes(x = Año + 0.1, label = Tasa), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(2017.8, 2019.5),
                     breaks = seq(2018, 2019, 1)) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_text(face="bold", colour= gris_letra, size=14),                         
        legend.text = element_text(face="bold", colour=gris_letra)) +
  labs(y = "RANK",
       x = "Año",
       title = "Comparativo tasa de homicidios por 100.000 habitantes, 2018 - 2019",
       subtitle = "Zona Rural",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: La Secretaría de Seguridad Municipal.
            Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal. 
            Tasa por 100.000 habitantes, Censo 2018, DANE.") +
  scale_y_reverse(breaks = seq(1,9,1))+
  scale_color_manual(values = c("red","green","gray","#F11385","steelblue",naranja,"purple","#3F4A80","#19DDEA"))

p1 

p2




# Motivos y arma empleada

Motivos <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Hoja4")

Motivos = pivot_longer(Motivos, cols = c(3,4,5,6), values_to = "Homicidios", names_to = "Arma")

Motivos[is.na(Motivos)] = 0
Motivos = subset(Motivos,Homicidios !=0)

ggplot(Motivos, aes(Homicidios,reorder(Motivo,Homicidios), fill = Arma))+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(0.3),angle = 45))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris,"steel blue"))+ 
  labs(title = "Móviles y armas empleadas homicidios 2019",
       x = "Homicidios",
       y = "Motivos homicidios",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
             Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.")+
  geom_text(aes(label= (Homicidios)),fontface = "bold",  size=4, vjust=0.5, hjust=1 ,col="black",position = position_stack())

#suicidios
SuicidiosEdad <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "SuicidiosEdad")

SuicidiosEdad = pivot_longer(SuicidiosEdad, cols = c(2,3), values_to = "Suicidios", names_to = "Año")

SuicidiosEdad = subset(SuicidiosEdad, Suicidios !=0)

SuicidiosEdad$Edad = factor(SuicidiosEdad$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                            "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                             levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                    "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))


ggplot(SuicidiosEdad, aes(Edad,Suicidios, fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris,"steel blue"))+ 
  labs(title = "Suicidios por rango de edad, 2018 - 2019",
       x = "Rango de Edad",
       y = "Suicidios",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Medicina Legal.")+
  geom_text(aes(label= (Suicidios)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))



## suicidios y tasa de suicidios

SuicidiosTasa<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "suicidiosTas")

SuicidiosTasa = SuicidiosTasa %>%  arrange((desc(Sexo)))
SuicidiosTasa$Año =as.factor(SuicidiosTasa$Año)

ggplot(SuicidiosTasa, aes(x = Año, y = Suicidios,fill = Sexo))+
  geom_bar(aes(fill = fct_rev(Sexo)), stat = "identity", position = position_stack(reverse = F))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde))+
  labs(title = "Casos de suicidios por sexo 2014 - 2019",
       x = "Año",
       y = "No de casos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Medicina Legal.")+
  scale_y_discrete("Suicidios",breaks = c("Suicidios"))+
  geom_text(aes(label= Suicidios), size = 4, color = "Black", position = position_stack(reverse =T),vjust = 0.8)



legend_title <- "Tasas"

ggplot(SuicidiosTasa, aes(Año,Tasa,fill = Sexo ,color = Sexo),show.legend = T) +
  geom_line(lwd=3,aes(group = Sexo))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde,naranja,gris))+
  labs(title = "Tasa de suicidios por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de suicidios por 100 mil Hab.",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Medicina Legal.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")





#Hurtos

#Caso de hurtos

Hurtos <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Hurtos")
Hurtos$Año = as.factor(Hurtos$Año)
Hurtos = Hurtos %>%  arrange((desc(Año)))

ggplot(Hurtos, aes(x = Año, y = Hurtos,fill = Casos))+
  geom_bar(aes(fill = fct_rev(Casos)), stat = "identity", position = position_stack(reverse = F))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde))+
  labs(title = "Casos de Hurtos por sexo 2014 - 2019",
       x = "Año",
       y = "No de casos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís
              Delictivo-CIADPAL con información de Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal")+
  scale_y_discrete("No.Hurtos",breaks = c(" No.Hurtos"))+
  geom_text(aes(label= Hurtos), size = 4, color = "Black", position = position_stack(reverse =T),vjust = 0.8)


##tasa hurtos

legend_title <- "Tasas"

ggplot(Hurtos, aes(Año,Tasa,color = Sexo),show.legend = T) +
  geom_line(lwd=3, aes(group = Sexo))+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde,naranja,gris))+
  labs(title = "Tasa de hurtos por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de hurtos por 100 mil Hab.",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
             Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.",
       legend_title= "Tasa")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")






##Tipo de hurto y arma

TipoHurtos <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "TipoHurtos")

TipoHurtos = pivot_longer(TipoHurtos, cols = c(2,3,4,5,6,7,8,9), values_to = "Hurtos", names_to = "Arma")

Motivos[is.na(Motivos)] = 0
TipoHurtos = subset(TipoHurtos,Hurtos !=0)

ggplot(TipoHurtos, aes(Tipo, Hurtos, fill = Arma))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris,"steel blue","salmon","yellow"))+ 
  labs(title = "Hurtos por tipo y arma empleada 2019",
       x = "Tipo de hurtos",
       y = "Hurtos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
             Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.")+
  geom_text(aes(label= (Hurtos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))

#Variacion tipo de hurto 2018 - 2019

HurtoTipo <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "HurtoTipo")
HurtoTipo = pivot_longer(HurtoTipo, cols = c(2,3), values_to = "Hurtos", names_to = "Año")


ggplot(HurtoTipo, aes(Tipo, Hurtos, fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Hurtos por tipo 2018-2019",
       x = "Tipo de hurtos",
       y = "Hurtos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Medicina Legal.")+
  geom_text(aes(label= (Hurtos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))


##Hurtos Edad

HurtosEdad <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "HurtosEdad")
HurtosEdad = pivot_longer(HurtosEdad, cols = c(2,3), values_to = "Hurtos", names_to = "Año")
HurtosEdad
HurtosEdad$Edad = factor(HurtosEdad$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                           "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                    "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))

ggplot(HurtosEdad, aes(Edad, Hurtos, fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Hurtos por rango de edad, 2018-2019",
       x = "Rango de edad",
       y = "Hurtos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
       Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.")+
  geom_text(aes(label= (Hurtos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))


##Lesiones personales

Lesiones <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/6.Seguridad/Seguridad_R.xlsx",sheet= "Lesiones")

Lesiones$Edad = factor(Lesiones$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                     "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                         levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                  "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))

ggplot(Lesiones, aes(Edad, Lesiones))+
  geom_bar(stat = "identity", fill = verde)+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  labs(title = "Lesiones personales rango de edad, 2019",
       x = "Rango de edad",
       y = "Lesiones",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de:  Datos Procesados por Centro de Analisís,  Delictivo-CIADPAL con información de
       Policía Nacional, Sijin, Fiscalía, CTI, Medicina Legal.")+
  geom_text(aes(label= (Lesiones)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))



###

VIF_TASA <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VIF_TASA")

 ggplot(VIF_TASA, aes(Año,Tasa, fill = Sexo , color = Sexo),show.legend = T) +
  geom_line(lwd=3)+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde,naranja,gris))+
  labs(title = "Tasa de violencia intrafamiliar por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de VIF por 100 mil Hab.",
       caption = "Fuente: Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")



VIF_AGRESOR <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VIF_AGRESOR")

VIF_AGRESOR$Año= as.factor(VIF_AGRESOR$Año)
ggplot(VIF_AGRESOR, aes(Año, Casos, fill = Sexo))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris))+ 
  labs(title = "Violencia Intrafamiliar por sexo del agresor",
       x = "Año",
       y = "Casos VIF",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Casos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))



## agresor 2019


ggplot(subset(VIF_AGRESOR,VIF_AGRESOR$Año==2019&VIF_AGRESOR$Casos!=0),aes(x=2,y=Casos, fill=Sexo))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Casos/sum(Casos))),
            position=position_stack(vjust=0.5),color="black",size=5)+
  coord_polar(theta = "y",start = 0)+
  scale_fill_manual(values=c(verde,"purple"))+
  theme_void()+
  labs(title="Distribución porcentual sexo de
       l agresor violencia intrafamiliar 2019",
       x = "Sexo agresor",
       y = "Casos",
       caption = "Fuente: Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal
              y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  xlim(0.5,2.5)+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"))





##VIF POR EDAD

VIF_EDAD <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VIF_EDAD")

VIF_EDAD$Edad = factor(VIF_EDAD$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                 "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                       levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))

VIF_EDAD$Año = as.character(VIF_EDAD$Año)
ggplot(VIF_EDAD, aes(Edad, Casos , fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Casos de Violencia Intrafamiliar por rango de edad, 2018-2019",
       x = "Rango de edad",
       y = "Casos VIF",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Casos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))

###Tasas por comuna
Tasa_comuna_VIF<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VIF_COMUNA")

Tasa_comuna_VIF$Año = as.character(Tasa_comuna_VIF$Año)


ggplot(subset(Tasa_comuna_VIF, Zona == "Urbana"), aes(Tasa,Comuna,fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Tasa de Violencia Intrafamiliar por comuna, 2018-2019",
       subtitle = "Zona Urbana",
       y = "Comuna",
       x = "Tasa",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Tasa)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))


ggplot(subset(Tasa_comuna_VIF, Zona == "Rural"), aes(Tasa,Comuna,fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  scale_x_continuous(limits = c(0,7050,100), breaks = c(0,7050,100))+
  labs(title = "Tasa de Violencia Intrafamiliar por comuna, 2018-2019",
       subtitle = "Zona Rural",
       y = "Comuna",
       x = "Tasa",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Tasa)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))
















## Tasa Matrato infantil

MALTRATOINFANTIL <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "MALTRATOINFANTIL")
 

MALTRATOINFANTIL$Año = as.character(MALTRATOINFANTIL$Año)

ggplot(MALTRATOINFANTIL, aes(Año,Tasa,fill=Año),show.legend = T) +
  geom_line(lwd=3,color=verde,aes(group = 1))+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde))+
  labs(title = "Tasa de maltrato infantil por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de maltrato infantil por 100 mil Hab.",
       caption = "Fuente: Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")+
  scale_y_continuous(limits = c(140,max(MALTRATOINFANTIL$Tasa),50))


##Maltrato infantil edad

MIedad <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "MaltratoInfantilEdad")


MIedad$Edad = factor(MIedad$Edad, labels = c("0-4","5-9","10-14","15-18"),
                       levels=c("0-4","5-9","10-14","15-18"))
MIedad$Año= as.character(MIedad$Año)

ggplot(data=MIedad, aes(Año,Edad), show_legend =T)+
geom_tile( aes(fill = Casos),colour = "White") +
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_text(face="bold", colour= gris_letra, size=10),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_gradient(low = "yellow",high = "red")+ 
  labs(title = "Maltrato Infantil por rango de edad, 2018 - 2019",
       x = "Años",
       y = "Edad",
       caption = "Fuente: Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses")+
  geom_text(aes(label= (Casos)),fontface = "bold", size=5, col="black",hjust=0)


## Parentesco agresor maltrato infantil 

MI_parentesco <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "MaltratoInfantilParentesco")

MI_parentesco$Año = as.character(MI_parentesco$Año)

ggplot(MI_parentesco, aes(Casos,reorder(Parentesco,Casos) , fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Casos de maltrato infantil por parentesco del agresor, 2018-2019",
       x = "Casos VIF",
       y = "Parentesco con el agresor",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Casos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))

#TAsa de maltrato infatil por comuna

Tasa_comuna_MI<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "MaltratoInfantilComuna")

Tasa_comuna_MI$Año = as.character(Tasa_comuna_MI$Año)


ggplot(subset(Tasa_comuna_MI, Zona == "Urbana"), aes(Comuna,Tasa,fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Tasa de maltrato infantil por comuna, 2018-2019",
       subtitle = "Zona Urbana",
       x = "Comuna",
       y = "Tasa",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Tasa)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))



ggplot(subset(Tasa_comuna_MI, Zona == "Rural"), aes(Comuna,Tasa,fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Tasa de maltrato infantil por comuna, 2018-2019",
       subtitle = "Zona rural",
       x = "Comuna",
       y = "Tasa",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Tasa)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))





##Tasa de violencia sexual

Tasa_vs <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "TasaVSexual")

Tasa_vs = subset(Tasa_vs,Sexo == "Total")

Tasa_vs$Año = as.numeric(Tasa_vs$Año)
Tasa_vs$Tasa = as.numeric(Tasa_vs$Tasa)

ggplot(Tasa_vs, aes(Año,Tasa,fill=Año),show.legend = T) +
  geom_line(lwd=3,color=verde)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde))+
  labs(title = "Tasa de violencia sexual por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de violencia sexual por 100 mil Hab.",
       caption = "Fuente: Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")

## Violencia sexual edad


VsEdad <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VsexualEdad")


VsEdad$Edad = factor(VsEdad$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                 "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                       levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))

VsEdad$Año = as.character(VsEdad$Año)

ggplot(VsEdad, aes(Edad, Casos , fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Casos de violencia sexual por rango de edad, 2018-2019",
       x = "Rango de edad",
       y = "Casos violencia sexual",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Casos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))


##Grafico parentesco agresosr violencia sexual

VsParentesco <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VsexualParentesco")

VsParentesco = pivot_longer(VsParentesco, cols = c(2,3),values_to = "Casos",names_to = "Año")


VsParentesco = subset(VsParentesco,Año ==2019)
VsParentesco = tibble(VsParentesco)

treeMapCoordinates <- treemapify(VsParentesco, area = "Casos",
                                 fill = "Tipo",  label = "Parantesco",
                                 group = "Tipo")


    ggplot(VsParentesco, aes(Casos, reorder(Parentesco,Casos), fill = Año))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Casos de violencia sexual por Parentesco con el agresor, 2018-2019",
       x = "Casos de violencia sexual",
       y = "Parentesco con el agresor",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal  y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Casos)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))

#Violencia sexual tipo de hecho

VsTipo <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/7. VIF Y y Violencia Sexual/VIF_R.xlsx",sheet= "VSexualTipo")

VsTipo$Año = as.character(VsTipo$Año)
ggplot(VsTipo, aes(x = Año, y = reorder(Tipo,Casos)))+
  geom_tile(data = VsTipo, aes(fill = Casos),colour = "White") +
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_text(face="bold", colour= gris_letra, size=8),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_gradient(low = "Yellow" , high = "Red")+ 
  labs(title = "Hechos involucrados en casos de violencia sexual, 2018 - 2019",
       subtitle = "",
       x = "Años",
       y = "Hechos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Gobierno Municipal
              y el Instituto Nacional de Medicina Legal y Ciencias Forenses.")+
  geom_text(aes(label= (Casos)),fontface = "bold", size=3.5, col="black",hjust=0)


### CAPITULO MOVILIDAD

## victimas fatales


AccidentesTasa<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Tasa_Victimas_fatales")

AccidentesTasa$Año =as.factor(AccidentesTasa$Año)
AccdientesTasa = arrange(AccidentesTasa,desc(Sexo))


ggplot(AccidentesTasa, aes(x = Año, y = Víctimas ,fill = Sexo))+
  geom_bar(aes(fill = fct_rev(Sexo)), stat = "identity", position = position_stack(reverse = F))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c(gris,naranja,verde))+
  labs(title = "Víctimas fatales accidentes de tránsito 2014 - 2019",
       x = "Año",
       y = "No de casos",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal")+
  scale_y_discrete("Víctimas",breaks = c("Víctimas"))+
  geom_text(aes(label= Víctimas), size = 4, color = "Black", position = position_stack(reverse =T),vjust = 0.8)



legend_title <- "Tasas"

ggplot(AccidentesTasa, aes(Año,Tasa, fill = Sexo , color = Sexo),show.legend = T) +
  geom_line(lwd=3,aes(group = Sexo))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde,naranja,gris))+
  labs(title = "Tasa de víctimas fatales en accidentes de tránsito,  por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de víctimas por 100 mil Hab.",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")


### víctimas segun edad


VictimasEdad<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Víctimas_edad")
VictimasEdad = pivot_longer(VictimasEdad,cols = c(2,3), values_to = "Víctimas", names_to = "Año")

VictimasEdad$Edad = factor(VictimasEdad$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                             levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                      "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))


ggplot(VictimasEdad, aes(Edad,Víctimas, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris,"steel blue"))+ 
  labs(title = "Víctimas fatales por rango de edad, 2018 - 2019",
       x = "Rango de Edad",
       y = "Víctimas",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal.")+
  geom_text(aes(label= (Víctimas)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))


##Clase de accidentes
ClaseAccidente<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Clase_de_accidentes")

ClaseAccidente = subset(ClaseAccidente, Año ==2019)
ClaseAccidente = ClaseAccidente %>%mutate(Porcentaje = round(Número/sum(Número)*100),2)


ggplot(ClaseAccidente,aes(x=2,y=Porcentaje, fill=Clase))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Porcentaje/100)),
            position=position_stack(vjust=0.5),color="black",size=5)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray",verde,"purple"))+
  theme_void()+
  labs(title="Víctimas fatales en accidentes de tránsito según clase de accidente 2019",
       x = "",
       y = "",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, con datos de: Secretaría de Movilidad Municipal.")+
  xlim(0.5,2.5)+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"))

## Clase de vehículos

ClaseVehículos<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Clase_vehículos")

ClaseVehículos = subset(ClaseVehículos, Víctimas !=0)
ClaseVehículos$Año = as.character(ClaseVehículos$Año)

ggplot(ClaseVehículos, aes(x = Víctimas, y = reorder(Vehículo,Víctimas),fill = Año))+
  geom_bar( stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        axis.text.y = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+
  labs(title = "Víctimas fatales en accidente de tránsito, según tipo de vehículo 2018 - 2019",
       x = "Víctimas",
       y = "Tipo de vehículo",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal")+
    geom_text(aes(label= Víctimas), size = 6, color = "Black", position = position_dodge(width = 0.9))

##Clase de rol

Rol<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Rol")

ggplot(AccdientesTasa, aes(Año,Tasa, fill = Sexo , color = Sexo),show.legend = T) +
  geom_line(lwd=3,aes(group = Sexo))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde,naranja,gris))+
  labs(title = "Tasa de víctimas fatales en accidentes de tránsito,  por 100.000 habitantes, 2014 - 2019",
       x = "Año",
       y = "Tasa de víctimas por 100 mil Hab.",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")


p1 =   ggplot(filter(Rol,Consecuencia == "Lesionados"), aes(Consecuencia,reorder(Rol,Casos))) +
  geom_tile(aes(fill = Casos), color = "white",show.legend = F) +
  scale_fill_gradient(low = "yellow", high = "red")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=9,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        axis.text.y = element_text(angle = 0, vjust = 1,face="bold",size=12,color= gris_letra),panel.background = element_rect(fill = "transparent",color="white"),
        axis.text.x =  element_text(face="bold",size=15,color= gris_letra),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(title = "Lesiones y muertes, según rol de la víctima",
       x = "",
       y = "Rol de la víctima",
       caption = "Fuente: Elaborado por la Secretaría de Planeación Municipal, 
       con datos de: Secretaría de Movilidad Municipal.")+
  geom_text(aes(label= paste(Casos)),fontface = "bold")

p2 =   ggplot(filter(Rol,Consecuencia == "Muertes"), aes(Consecuencia,reorder(Rol,Casos))) +
  geom_tile(aes(fill = Casos), color = "white",show.legend = F) +
  scale_fill_gradient(low = "yellow", high = "red")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        axis.text.y = element_blank(), #Elimina las etiquetas del eje
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent",color="white"),
        axis.text.x =  element_text(face="bold",size=15,color= gris_letra),
        plot.caption = element_text(hjust = 0, face = "italic"))+
   geom_text(aes(label= paste(Casos)),fontface = "bold")


p1+p2

##Tasa de lesionados


Tasa_lesionados<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Tasa_lesionados")

Tasa_lesionados$Año = as.character(Tasa_lesionados$Año)
Tasa_lesionados = arrange(Tasa_lesionados,desc(Sexo))


ggplot(Tasa_lesionados, aes(Año,Tasa, fill = Sexo , color = Sexo),show.legend = T) +
  geom_line(lwd=3,aes(group = Sexo))+ 
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour=gris_letra, size=rel(2)),
        legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
  scale_color_manual(values = c(verde,naranja,gris))+
  labs(title = "Tasa de lesiones personales en accidentes de tránsito,  por 100.000 habitantes, 2017 - 2019",
       x = "Año",
       y = "Tasa de víctimas por 100 mil Hab.",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal.
             Tasa por 100.000 habitantes calculadas a partir de las proyecciones del Censo 2005, DANE, para los años 2014 a 2017. Censo 2018, DANE para los años  2018 a 2019.")+
  geom_text_repel(aes(x = Año, y=Tasa,label=Tasa),size  = 5,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="right",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White",color ="Black")



#lesionados por edad


LesionadosEdad<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/8.Movilidad/Movilidad_R.xlsx",sheet= "Lesionados_Edad")
LesionadosEdad = pivot_longer(LesionadosEdad,cols = c(2,3), values_to = "Víctimas", names_to = "Año")

LesionadosEdad$Edad = factor(LesionadosEdad$Edad, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"),
                             levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                      "35-39","40-44","45-49","50-54","55-59","60-64","65-69","Mayor a 70","Sd"))

LesionadosEdad$Año= as.character(LesionadosEdad$Año)
ggplot(LesionadosEdad, aes(Edad,Lesionados, fill = Año ))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1)))+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,gris,"steel blue"))+ 
  labs(title = "Lesionados por rango de edad, 2018 - 2019",
       x = "Rango de Edad",
       y = "Lesionados",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Movilidad Municipal.")+
  geom_text(aes(label= (Lesionados)),fontface = "bold",  size=4, col="black",position=position_dodge(width=0.9))


##  HACIENDA



Predios<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "Predios")


##predios

Predios_municipio = Predios %>% select(1,2,5,7)
names(Predios_municipio)
Predios_municipio = pivot_longer(Predios_municipio, cols = c(3,4), names_to = "Tipo", values_to = "Predios")

Predios_municipio$Año = as.factor(Predios_municipio$Año)
view(Predios_municipio)
Predios_municipio$Predios = format(Predios_municipio$Predios, big.mark = ",")

g1 = ggplot(Predios_municipio, aes(Sector,Predios, fill = Tipo))+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1.5)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Predios del municipio, 2014 - 2019",
       x = "Zona",
       y = "Predios",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
  geom_text(aes(label= (Predios)),fontface = "bold",  size=4, col="black", position = position_stack(vjust = 0.5))

g1 + facet_grid(.~ Año)


#avaluo
avaluo = Predios %>% select(1,2,6,8)

avaluo = pivot_longer(avaluo, cols = c(3,4), names_to = "Tipo", values_to = "avaluo")

avaluo$Año = as.factor(avaluo$Año)

avaluo = format(avaluo$avaluo, big.mark = ",")

g2 = ggplot(Predios_municipio, aes(Sector,avaluo, fill = Tipo))+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1.5)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank() )+# move caption to the left
  scale_fill_manual(values = c(verde,naranja))+ 
  labs(title = "Avalúo castratal predios del municipio (millones de pesos), 2014 - 2019",
       x = "Zona",
       y = "Avalúo",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
  geom_text(aes(label= paste("$",avaluo)),fontface = "bold",  size=3.0, col="black", position = position_stack(vjust = 0.5))

g2 + facet_grid(.~ Año)


### ingresos


Ingresos<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "Ingresos")

ggplot(Ingresos, aes(area = Ingresos, fill = Porcentaje, label = Descripción,
                subgroup = Tipo_de_ingreso)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                               "white", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "Black", place = "topleft", reflow = T)+
    labs(title = "Distribución del ingreso 2019",
         caption = "Fuente:Elaborado por la Secretaría de Planeación Municpal con datos de: Secretaría de Hacienda Municipal.")+
scale_fill_gradient2(high = "Blue",mid = "orange", low = "yellow", midpoint =6)


###Ingresos tributarios 2014 - 2019


Ingresos_trib<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "Ingresos_porAño")

Ingresos_trib = pivot_longer(Ingresos_trib, cols = c(2,3,4,5,6),names_to = "Tipo", values_to = "Ingresos")
Ingresos_trib = subset(Ingresos_trib, Tipo != "Total ingresos")
str(Ingresos_trib)
Ingresos_trib$Ingresos = as.numeric(Ingresos_trib$Ingresos)

g2 = ggplot(Ingresos_trib, aes(Ingresos,reorder(Tipo,Ingresos),fill = Tipo),show.legend = F)+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.text = element_text(face="bold", colour=gris_letra),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+# move caption to the left
  scale_fill_manual(values = c(verde,naranja,"steelblue","red"))+ 
  labs(title = "Descomposición del Ingreso Tributario del municipio, 2014 - 2019",
       x = "",
       y = "Tipo de ingreso",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
  geom_text(aes(label= paste("$",Ingresos)),fontface = "bold",  size=rel(4), col="black",vjust =0.5,hjust=0.5, angle = 90)


  g2 + facet_grid(.~ Año)

  ##Gasto publico 
  
  
  GastoPublico<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "GastoPublico")
  
  GastoPublico = GastoPublico %>% group_by(Tipo) %>% summarise(Gasto = sum(Gasto))
  GastoPublico$Gasto = round(GastoPublico$Gasto,1)
  
  g4 = ggplot(GastoPublico, aes(Gasto,reorder(Tipo,Gasto),fill=Tipo),show.legend = F)+
    geom_bar(stat = "identity")+
    theme(panel.background = element_rect(fill = "transparent",color="white"),
          plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
          plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
          plot.caption = element_text(hjust = 0, face = "italic"),
          legend.text = element_text(face="bold", colour=gris_letra),
          axis.text.y =  element_text(face="bold", colour="#AFAFB0", size=rel(1.5)),
          axis.text.x =  element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())+# move caption to the left
    scale_fill_manual(values = c(verde,naranja,"steelblue","red"))+ 
    labs(title = "Descomposición de los Egresos municipales 2019",
         x = "",
         y = "Tipo de Gasto",
         caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
    geom_text(aes(label= paste("$",Gasto)),fontface = "bold",  size=rel(4), col="black",vjust =-0.1,position = position_jitterdodge())
  
  
  g4 
  
  
  ##Gasto funcionamiento
  
  GastoFuncionamiento<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "GastoFuncionamiento")
  
  
  g5 = ggplot(GastoFuncionamiento, aes(Organo,Millones,fill=Rubro),show.legend = F)+
    geom_bar(stat = "identity")+
    theme(panel.background = element_rect(fill = "transparent",color="white"),
          plot.title = element_text(hjust = 0, size = 14,face ="bold"),    # Center title position and size
          plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
          plot.caption = element_text(hjust = 0, face = "italic"),
          legend.text = element_text(face="bold", colour=gris_letra),
          axis.text.y =  element_blank(),
          axis.text.x =  element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())+# move caption to the left
    scale_fill_manual(values = c(verde,naranja,"steelblue","red","purple",gris,"pink", "Salmon"))+ 
    labs(title = "Descomposición de los Egresos municipales 2019",
         x = "",
         y = "Tipo de Gasto",
         caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
    geom_text(aes(label= paste("$",Millones)),fontface = "bold",  size=rel(4), col="black",vjust =-0.1,position = position_jitterdodge(), angle = 90)
  
  
  g5 + facet_grid(.~ Organo)
  
  
  ggplot(GastoFuncionamiento, aes(area = Millones, fill = Millones, label = Rubro,
                       subgroup = Rubro)) +
    geom_treemap() +
    geom_treemap_subgroup_border() +
    geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                                 "white", fontface = "italic", min.size = 0) +
    geom_treemap_text(colour = "Black", place = "topleft", reflow = T)+
    labs(title = "Distribución de los gastos de funcionamiento, 2019",
         caption = "Fuente:Elaborado por la Secretaría de Planeación Municpal con datos de: Secretaría de Hacienda Municipal.")+
    scale_fill_gradient2(high = "Blue",mid = "orange", low = "yellow", midpoint =6)
  
#Inversiones municipal
  
  InversionMunicipal<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "InversionMunicipal")

  InversionMunicipal = pivot_longer(InversionMunicipal, cols = c(2,3), values_to = "Millones", names_to = "Año")
  InversionMunicipal$Millones = as.numeric(InversionMunicipal$Millones)
  InversionMunicipal$Millones = format(as.numeric(InversionMunicipal$Millones), big.mark = ",")
  InversionMunicipal$Año = as.character(InversionMunicipal$Año)
  
  GInversion1= ggplot(filter(InversionMunicipal,Año=="2019"),aes(Millones,reorder(Dependencia,Millones)))+
    geom_bar(stat = "identity",color = verde,fill = verde) +
    theme(panel.background = element_rect(fill = "transparent",color="white"),
          plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
          plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
          plot.caption = element_text(hjust = 0, face = "italic"),
          axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
          axis.text.y = element_text(face="bold", colour= gris_letra, size=10),
          legend.text = element_text(face="bold", colour=gris_letra))+# move caption to the left
       labs(title = "Gasto de inversión por dependencia (Millones de pesos), 2019",
         x = "Millones",
         y = "Dependencia",
         caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
   geom_text(aes(label= paste("$",number(Millones))),fontface = "bold", size=3, col="black",position = position_stack(),hjust = 0.3)
  
  GInversion1

  
  
  ## Presupuesto de ingresos y gastos
  
Presupuesto<- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/9. Hacienda Pública/Hacienda_R.xlsx",sheet= "PptoIngesosGastos")


PptoIngreso = ggplot(filter(Presupuesto, Tipo == "Ingresos"), aes(Rubro,Millones,fill = Rubro)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c(naranja,verde))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_blank(),
        legend.text = element_text(face="bold", colour=gris_letra),
        legend.position = "none")+# move caption to the left
  labs(title = "Presupuesto de Inversión y Gastos, 2019",
       subtitle = "Ingresos",
       x = "",
       y = "Millones",
       caption = "Fuente: Elaborado por la Secretaría de Planeación con datos de: Secretaría de Hacienda Municipal.")+
  geom_text(aes(label= paste("$",number(Millones))),fontface = "bold", size=4, col="black",position = position_stack(), hjust = 0,vjust=0)
    
PptoGastos = ggplot(filter(Presupuesto, Tipo == "Gasto"), aes(Rubro,Millones,fill = Rubro)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c("steelblue","red","salmon"))+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(face="bold", colour=gris_letra),
        legend.position = "none")+# move caption to the left
  labs(title = "",
       subtitle = "Gastos",
       x = "",
       y = "",
       caption = "")+
  geom_text(aes(label= paste("$",number(Millones))),fontface = "bold", size=4, col="black",position = position_stack(),hjust = 0,vjust=0)


PptoIngreso + PptoGastos


# Captitulo de sector agropecuario

# Frutas
 
Frutas <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/11. SEctor Agropecuario/Agropecuario_r.xlsx",sheet= "Frutas")

Frutas$Producción = format(round(Frutas$Producción, 1), nsmall=0, big.mark=",")


gfrutas = ggplot(Frutas, aes(x=2, Producción, fill =Frutales)) + 
  geom_bar(stat = "identity", width = 1, size = 1, color = "white") +
  coord_polar(theta = "y") + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values =c(verde,naranja,"pink",gris,"red","purple","blue","yellow","salmon","brown","#00FFFF"))+
  labs(title = "Producción  en toneladas de cultivos frutales 2019",
       caption = "Fuente: URPA - Valle, con base en información suministrada por las UMATA, Gremios del Sector, Evaluación Agropecuaria - 
              EVA de la Gobernación del Valle del Cauca, Secretaría Municipal de Agricultura y Desarrollo Económico.")+
  theme(legend.title = element_text(size = 12),
        legend.text =element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.position = "none")


gfrutas

etiquetasfrutas <- ggplot(Frutas, aes(y = Frutales)) + 
  geom_point(aes(0, color = Frutales), size = 8) +
  geom_text(data = Frutas, aes(0, label = paste(Frutales,number(Producción))), size = 4, color = "black",
                  fontface = "bold", hjust = -0.05) +
  
scale_x_continuous(limits = c(0,5,1))+
   theme_void() +
  theme(legend.position = "none")+
  scale_color_manual(values = c(verde,naranja,"pink",gris,"red","purple","blue","yellow","salmon","brown","#00FFFF"))

etiquetasfrutas
library(egg)
ggarrange(gfrutas, etiquetasfrutas, nrow = 1, widths = c(2, 1))

## cultivos permanentes

hortalizas <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/11. SEctor Agropecuario/Agropecuario_r.xlsx",sheet= "hortalizas")

ghortalizas = ggplot(hortalizas, aes(x=2, Toneladas, fill =Cultivo)) + 
  geom_bar(stat = "identity", width = 1, size = 1, color = "white") +
  coord_polar(theta = "y") + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values =c(verde,naranja,"pink",gris,"red","purple","blue","yellow","salmon","brown","#00FFFF"))+
  labs(title = "Producción  en toneladas de cultivos de hortalizas 2019",
       caption = "Fuente: URPA - Valle, con base en información suministrada por las UMATA, Gremios del Sector, Evaluación Agropecuaria - 
              EVA de la Gobernación del Valle del Cauca, Secretaría Municipal de Agricultura y Desarrollo Económico.")+
  theme(legend.title = element_text(size = 12),
        legend.text =element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.position = "none")

ghortalizas

etiquetashortalizas <- ggplot(hortalizas, aes(y = Cultivo)) + 
  geom_point(aes(0, color = Cultivo), size = 8) +
  geom_text(data = hortalizas, aes(0, label = paste(Cultivo,number(Toneladas))), size = 4, color = "black",
            fontface = "bold", hjust = -0.05) +
  
  scale_x_continuous(limits = c(0,5,1))+
  theme_void() +
  theme(legend.position = "none")+
  scale_color_manual(values = c(verde,naranja,"pink",gris,"red","purple","blue","yellow","salmon","brown","#00FFFF"))

etiquetashortalizas
library(egg)
ggarrange(ghortalizas, etiquetashortalizas, nrow = 1, widths = c(2, 1))

## Producción avicola



avicola <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/11. SEctor Agropecuario/Agropecuario_r.xlsx",sheet= "avicola")

 ggplot(avicola, aes(x=2, Porcentaje, fill =Tipo)) + 
  geom_bar(stat = "identity", width = 1, size = 1, color = "white") +
  coord_polar(theta = "y") + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values =c(verde,"purple"))+
  labs(title = "Producción ávicola 2019",
       caption = "Fuente: URPA - Valle, con base en información suministrada por las UMATA, Gremios del Sector, Evaluación Agropecuaria - 
              EVA de la Gobernación del Valle del Cauca, Secretaría Municipal de Agricultura y Desarrollo Económico.")+
  theme(legend.title = element_text(size = 12),
        legend.text =element_text(size = 10),
        plot.title = element_text(size=14,face="bold"),
        axis.title=element_text(size=10,face="bold",color = "white"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.position = "right")+
   geom_text(aes(label =paste(Porcentaje,"%")),fontface = "bold",size = 5)

 

#Otras especies pecuarias
 
Pecuarias <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/11. SEctor Agropecuario/Agropecuario_r.xlsx",sheet= "Pecuarias")

ggplot(Pecuarias, aes(x=reorder(Item,Producción),Producción,fill = Item)) + 
  geom_bar(stat = "identity") +
  theme(legend.position = "right") +
  scale_fill_manual(values =c(verde,"purple","steelblue","salmon","red","yellow",naranja))+
  labs(title = "Producción de otras especies pecuarias 2019",
       caption = "Fuente: URPA - Valle, con base en información suministrada por las UMATA, Gremios del Sector, Evaluación Agropecuaria - 
              EVA de la Gobernación del Valle del Cauca, Secretaría Municipal de Agricultura y Desarrollo Económico.")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour= gris_letra, size=10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(face="bold", colour=gris_letra))+
  geom_text(aes(label = Producción),fontface = "bold",size = 5)

##Bovino


Bovino <- read_excel("D:/PLANEACIÓN/ANUARIO/Tablas_graficos/11. SEctor Agropecuario/Agropecuario_r.xlsx",sheet= "Bovino")

Bovino$Año = as.factor(Bovino$Año)

ggplot(Bovino, aes(Año,Cantidad,fill = Sexo,color = Sexo))+
  geom_line(lwd=2, aes(group = Sexo)) +
  scale_color_manual(values = c(naranja, verde))+
  labs(x = "Año",
       y = "Número de bovinos",
       title = "Producción bovinos 2015 - 2019",
       caption = "Fuente: URPA - Valle, con base en información suministrada por las UMATA, Gremios del Sector, Evaluación Agropecuaria - 
              EVA de la Gobernación del Valle del Cauca, Secretaría Municipal de Agricultura y Desarrollo Económico.")+
  theme(panel.background = element_rect(fill = "transparent",color="white"),
        plot.title = element_text(hjust = 0, size = 14, face = "bold"),    # Center title position and size
        plot.caption = element_text(hjust = 0, face = "italic"),
        axis.text.x = element_text(face="bold", colour="#AFAFB0", size=rel(1.5)),
        legend.text = element_text(face = "bold",colour = gris_letra, size = rel(1.5)),)+
  geom_text_repel(aes(x = Año, y=Cantidad,label= number(Cantidad)),size  =  4,
                  nudge_x = 0,nudge_y = 0,vjust="top",hjust="center",color = "black",
                  segment.alpha=0, box.padding = 0.5, fontface = "bold", segment.color ="White")
 
