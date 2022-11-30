#Prototípico Cálculo Integral
#Aproximación por mínimos cuadrados
año<-c(2006,2011,2016,2021)
unidas_casadas<-c(53.3,50.8, 47.3,44.6)
tipoderelacióncdmx<-cbind(año, unidas_casadas)
tipoderelacióncdmx<-as.data.frame(tipoderelacióncdmx)

tipoderelacióncdmx
library(ggplot2)


# Basic scatter plot
ggplot(tipoderelacióncdmx, aes(x = año, y = unidas_casadas)) +
  geom_point() + 
  scale_x_continuous("Año") + 
  scale_y_continuous("No. de Mujeres unidas o casadas")+ 
  ggtitle("Distribución porcentual de mujeres de 15 años o más unidas o casadas en CDMX por año")
  
#Ajuste de regresión lineal simple
lm(unidas_casadas~año, data = tipoderelacióncdmx)

# Rehaciendo la gráfica con la recta de regresión
ggplot(tipoderelacióncdmx, aes(x = año, y = unidas_casadas)) +
  geom_point() + 
  scale_x_continuous("Año") + 
  scale_y_continuous("Distribución porcentual de Mujeres unidas o casadas")+ 
  ggtitle("Distribución porcentual de mujeres de 15 años o más unidas o casadas en CDMX por año")+
  geom_abline(intercept = 1240.992,
              slope = -0.592,
              col="red")+
  geom_text(aes(x = 2040, y = 48,
                label = "y=-0.592*x+1240.992"),
            stat = "unique")

#Recta y=-0.592*x+1240.992

#El año cuando el porcentaje se reduzca a cero
#y=0, por lo tanto tenemos por despeje
#x=1240.992/0.592
x=1240.992/0.592
x


#De acuerdo a la recta estimada, para el año 2050 cuál será la distribución porcentual de Mujeres unidas o casadas en la CDMX?
x=2050
y=-0.592*x+1240.992
y



# Rehaciendo la gráfica con la recta de regresión y el intercepto del año 2050
ggplot(tipoderelacióncdmx, aes(x = año, y = unidas_casadas)) +
  theme_grey()+
  geom_point() + 
  scale_x_continuous("Año",limits = c(2000,2115)) + 
  scale_y_continuous("Distribución porcentual de Mujeres unidas o casadas", limits = c(0,60))+ 
  ggtitle("Regresión lineal de la Distribución porcentual de mujeres de 15 años o más unidas o casadas en CDMX vs año")+
  geom_abline(intercept = 1240.992,
              slope = -0.592,
              col="red")+
  geom_vline(xintercept = 2050,
             col="blue")+
  geom_vline(xintercept = 2096,
             col="orange")+
  geom_text(aes(x = 2015, y = 40,
                label = "y=-0.592*x+1240.992"),
            stat = "unique")+
  geom_text(aes(x = 2055, y = 30,
                label = "Intercepto y=27.392"),
            stat = "unique")+
  geom_text(aes(x = 2090, y = 0,
                label = "Intercepto y=2096.27"),
            stat = "unique")

