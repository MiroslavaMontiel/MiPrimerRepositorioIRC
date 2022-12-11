library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(gapminder)
library(xlsx)
library(data.table)
library(readxl)
library(readr)
library(trelliscopejs)
library(forecast)
library(plotly)


####Importar base de datos csv
file.choose()#buscar ruta del archivo csv
ruta_xlsx <-"C:\\Users\\Miroslava\\Documents\\IRC\\Proyecto Prototípico\\PP\\TB_SEC_XII.xlsx"
TB_SEC_XII<-read_excel(ruta_xlsx)




#Base de datos INEGI Número 12
TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO", P12_6 != "3") %>%
  select(NOM_MUN, P12_4) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  summarise(count = n() )  %>%
  arrange(count)  %>%
  mutate(parents="Casos totales de violencia en la niñez por")  %>%
  ungroup()

cdmx<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO")

TotEntrevistadas<-count(cdmx, "NOM_ENT")
TotEntrevistadas



#Personas que han sufritdo violencia en su niñez por alcaldía
#totales
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO", P12_6 != "3") %>%
  select(NOM_MUN, P12_4) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  summarise(count = n() )  %>%
  arrange(count)  %>%
  mutate(parents="Casos totales donde la entrevistada sufrió golpes en su casa durante su niñez")  %>%
  ungroup()
V_n

#mapa de arbol totales 
plot_ly(data =V_n,
        type="treemap",
        values = ~count,
        labels= ~ NOM_MUN,
        parents= ~parents,
        domain= list(colum=0),
        name="Casos totales de violencia en la niñez por alcaldía",
        textinfo= "label+value+ parent"
)

#Personas que han sufritdo violencia en su niñez por alcaldía
#Porcentaje respecto al total de entrevistadas que son 3267 personas
V_n1<-V_n %>%
  select(NOM_MUN, count) %>% 
  mutate(Porcentaje = count*100/3267) %>% 
  arrange(Porcentaje)  
V_n1

# 
# P12_12 Numérico A1, A2, Cuando usted se enoja  o enojaba con sus hijas e hijos, ¿los insulta o
# insultaba…
# 1 - de vez en cuando?  2 - seguido 3 - No les insulta # 4 - No tiene hijos(as)
# P12_7 Numérico  ¿Recuerda si las  personas con las que  vivía la insultaban o la  ofendían a usted…
# 1 - de vez en cuando?  2 - seguido? 3 - No la insultaban ni la ofendían
#Mujeres que si insultan a sus hijos, tienen antecedentes de insultos hacía su persona en la niñez? no, 53, si 47
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",P12_12!= "3", P12_12!= "4" ) %>%
  group_by(NOM_ENT) %>% 
  mutate(P12_7=case_when(P12_7=="1"~"si",P12_7=="2"~"si", P12_7=="3"~"no")) %>%
  count(P12_7) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  
V_n

# GGPLOT
g1<-ggplot(V_n,aes(x="", y=porcentaje, fill=P12_7))+
  geom_col(color = "black") +
  geom_label(aes(label = porcentaje), color =  "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") + 
  theme_void()

g1

# *****************************************Plotly 1 INICIO

p1 <- plot_ly(data = V_n, labels = ~P12_7, values = ~n, type = 'pie',textinfo = "label+percent",
              insidetextorientation = "radial")  %>% layout(title = 'Mujeres que insultan a sus hijos, ¿sufrieron insultos en su niñez?')

p1


# *****************************************Plotly 2 FIN


#Mujeres que no insultan a sus hijos, tienen antecedentes de insultos hacía su persona en la niñez? no, 84.8 si 15.2
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_12!= "1", P12_12!= "2" ) %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_7) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_7), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n



V_n2<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",P12_12!= "1", P12_12!= "2" ) %>%
  group_by(NOM_ENT) %>% 
  mutate(P12_7=case_when(P12_7=="1"~"si",P12_7=="2"~"si", P12_7=="3"~"no")) %>%
  count(P12_7) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  
V_n2

#GGPLOT
g2<-ggplot(V_n2,aes(x="", y=porcentaje, fill=P12_7))+
  geom_col(color = "black") +
  geom_label(aes(label = porcentaje), color =  "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") + 
  theme_void()

g2

# *****************************************Plotly 2 INICIO

p2 <- plot_ly(data = V_n2, labels = ~P12_7, values = ~n, type = 'pie',textinfo = "label+percent",
              insidetextorientation = "radial")  %>% layout(title = 'Mujeres que NO INSULTAN a sus hijos, ¿sufrieron insultos en su niñez?')

p2


# *****************************************Plotly 2 FIN




# P12_13  Cuando usted se enoja o enojaba con sus hijas e hijos, ¿les pega o pegaba…
# # 1 - de vez en cuando?  2 - seguido?  3 - No les pega 4 - No tiene hijos(as)
# P12_6 Numérico A1, A2, ¿Las personas con las que vivía le pegaban a usted…
# 1 - de vez en cuando? 2 - seguido? 3 - No le pegaban


#Mujeres que golpean a sus hijos, tienen antecedentes de golpes hacía su persona en la niñez? no, 43.4 si 56.7
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_13!= "3",P12_13!= "4") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_6) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_6), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

V_n3<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",P12_13!= "3",P12_13!= "4") %>%
  group_by(NOM_ENT) %>% 
  mutate(P12_6=case_when(P12_6=="1"~"si",P12_6=="2"~"si", P12_6=="3"~"no")) %>%
  count(P12_6) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  
V_n3

#GGPLOT 3
g3<-ggplot(V_n3,aes(x="", y=porcentaje, fill=P12_6))+
  geom_col(color = "black") +
  geom_label(aes(label = porcentaje), color =  "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") + 
  theme_void()

g3

# *****************************************Plotly 3 INICIO

p3 <- plot_ly(data = V_n3, labels = ~P12_6, values = ~n, type = 'pie',textinfo = "label+percent",
              insidetextorientation = "radial")  %>% layout(title = 'Mujeres que SI GOLPEAN a sus hijos, ¿sufrieron insultos en su niñez?')

p3


# *****************************************Plotly 3 FIN





#Mujeres no que golpean a sus hijos, tienen antecedentes de golpes hacía su persona en la niñez? no, 74 si 26
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_13!= "1",P12_13!= "2",P12_13!= "4") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_6) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_6), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

V_n4<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",P12_13!= "1",P12_13!= "2",P12_13!= "4") %>%
  group_by(NOM_ENT) %>% 
  mutate(P12_6=case_when(P12_6=="1"~"si",P12_6=="2"~"si", P12_6=="3"~"no")) %>%
  count(P12_6) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  
V_n4

#GGPLOT 3
g4<-ggplot(V_n4,aes(x="", y=porcentaje, fill=P12_6))+
  geom_col(color = "black") +
  geom_label(aes(label = porcentaje), color =  "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") + 
  theme_void()

g4

# *****************************************Plotly 4 INICIO

p4 <- plot_ly(data = V_n4, labels = ~P12_6, values = ~n, type = 'pie',textinfo = "label+percent",
              insidetextorientation = "radial")  %>% layout(title = 'Mujeres que SI GOLPEAN a sus hijos, ¿sufrieron insultos en su niñez?')

p4
# *****************************************Plotly 4 FIN






# P12_8 Cuando su pareja o esposo era niño (hasta antes de cumplir 15 años), ¿le pegaban o
# insultaban en su casa… 1 - de vez en cuando? 2 - seguido? 3 - No le pegaban ni lo insultaban 8 - No sabe b - blanco
# P12_10Cuando su esposo o pareja se enoja o enojaba con sus hijas e hijos, ¿los
# insulta o insultaba… 1 - de vez en cuando? 2 - seguido? 3 - No los insulta 4 - No tiene hijos(as) b - blanco




#Parejas de mujeres que golpean o insultan a sus hijos, tienen antecedentes de golpes o insultos hacía su persona en la niñez? no, 42.9 si 57.1
V_n<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_8!= "8",P12_8!= "b",P12_10!= "3",P12_10!= "4", P12_10!= "b") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_8) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_8), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

V_n5<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",P12_8!= "8",P12_8!= "b",P12_10!= "3",P12_10!= "4", P12_10!= "b") %>%
  group_by(NOM_ENT) %>% 
  mutate(P12_8=case_when(P12_8=="1"~"si",P12_8=="2"~"si", P12_8=="3"~"no")) %>%
  count(P12_8) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  
V_n5

#GGPLOT 3
g5<-ggplot(V_n5,aes(x="", y=porcentaje, fill=P12_8))+
  geom_col(color = "black") +
  geom_label(aes(label = porcentaje), color =  "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") + 
  theme_void()

g5

# *****************************************Plotly 4 INICIO

p5 <- plot_ly(data = V_n5, labels = ~P12_8, values = ~n, type = 'pie',textinfo = "label+percent",
              insidetextorientation = "radial")  %>% layout(title = 'Parejas de Mujeres que SI GOLPEAN a sus hijos, ¿sufrieron insultos en su niñez?')

p5






#Parejas de mujeres que NO golpean o insultan a sus hijos, tienen antecedentes de golpes o insultos hacía su persona en la niñez? no, 73 si 28
V_n<-TB_SEC_XII %>%
  
  filter(NOM_ENT == "CIUDAD DE MÉXICO",NOM_MUN=="GUSTAVO A. MADERO",P12_8!= "8",P12_8!= "b",P12_10!= "1",P12_10!= "2",P12_10!= "4", P12_10!= "b") %>%
  group_by(NOM_MUN) %>% #Ordena de manera ascendente, si quisiéramos desc tenemos que añadir desc
  count(P12_8) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  %>%
  ggplot(aes(x=NOM_MUN, y=n, fill=P12_8), position="dodge")+
  geom_bar(stat="identity")+
  geom_text(aes(label=porcentaje), color="white", vjust=0.001, position=position_dodge (2))
V_n

V_n6<-TB_SEC_XII %>%
  filter(NOM_ENT == "CIUDAD DE MÉXICO",P12_8!= "8",P12_8!= "b",P12_10!= "1",P12_10!= "2",P12_10!= "4", P12_10!= "b") %>%
  group_by(NOM_ENT) %>% 
  mutate(P12_8=case_when(P12_8=="1"~"si",P12_8=="2"~"si", P12_8=="3"~"no")) %>%
  count(P12_8) %>% 
  mutate(porcentaje=scales::percent(n/sum(n)))  
V_n6

#GGPLOT 3
g6<-ggplot(V_n6,aes(x="", y=porcentaje, fill=P12_8))+
  geom_col(color = "black") +
  geom_label(aes(label = porcentaje), color =  "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") + 
  theme_void()

g6

# *****************************************Plotly 4 INICIO

p6 <- plot_ly(data = V_n6, labels = ~P12_8, values = ~n, type = 'pie',textinfo = "label+percent",
              insidetextorientation = "radial")  %>% layout(title = 'Parejas de Mujeres que NO GOLPEAN a sus hijos, ¿sufrieron insultos en su niñez?')

p6

#GIFT
#Base de datos línea Mujeres
head(servicios_integrales_2019_2021)


servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>% 
  ggplot(aes(x=fecha_alta,
             y= m,
             color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")+
  transition_reveal(fecha_alta)



servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>% 
  ggplot(aes(x=fecha_alta,
             y= m,
             color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")+
  transition_reveal(fecha_alta)

servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(fecha_alta, m,))+
  geom_point(size = 0.3) +
  geom_smooth(se=FALSE, span=1) +
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")+
  facet_trelliscope(
    ~municipio_usuaria,
    ncol = 2,
    nrow = 2,
    
    as_plotly = TRUE)

#Gráfica de lineas de la GAM
servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", municipio_usuaria=="GUSTAVO A. MADERO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  summarise(m=n)%>%
  ggplot(aes(x=fecha_alta,
             y= m,
             color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")

#Serie de Tiempo "
llamadas_dia_gam<-servicios_integrales_2019_2021 %>% 
  filter(estado_usuaria=="CIUDAD DE MÉXICO", municipio_usuaria=="GUSTAVO A. MADERO") %>% 
  group_by(fecha_alta, municipio_usuaria) %>% 
  count(municipio_usuaria, sort = TRUE) %>%
  mutate(m=n)
ggplot(llamadas_dia_gam, aes(x=fecha_alta,
           y= m,
           color= municipio_usuaria))+
  geom_line()+
  labs(title="No. de llamadas realizadas a Línea Mujeres agrupadas por Alcaldía \r\n 2019-2021
       ",
       x="\r\nFecha de llamada", 
       y= "\r\nNúmero de llamdas recibidas por día")
 
llamadas_dia_gam

#c(2021,01,01)
llamdg<-ts(llamadas_dia_gam$m, start=c(2019), frequency = 365)


llamdg
  
ajuste<-auto.arima((y=llamdg))
summary(ajuste)
autoplot(llamdg)



predicciones<-forecast(ajuste)
min(predicciones[["lower"]])
max(predicciones[["upper"]])
p_predict<-autoplot(predicciones)
p_predict

