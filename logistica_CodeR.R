### EQUIPO: HUNAB KU ###

##CAMBIO DE DIRECTORIO
setwd("C:/Users/saman/Documents/Sami/DataCup/")

## CARGAR LOS DATOS
ubicaciones <- read.csv("ubicaciones.csv", header=T)
str(ubicaciones) 
summary(ubicaciones)
colnames(ubicaciones)<-c("Id_Cliente", "id_Agencia", "Frecuencia","Vol_Entrega","Latitud","Longitud")
#Misma agencia
#3625 clientes
#Frecuencia máxima 3
#Volumen de entrega min 0 máx 175

library(plyr)
library(dplyr)
library(ggplot2)

## Por frecuencia
ggplot(ubicaciones)+aes(x=Longitud,y=Latitud)+
  geom_point(aes(colour=as.factor(Frecuencia)))+
  labs(x='Longitud',y='Latitud')+
  scale_color_manual("Frecuencia", values = c("3" = "orangered4", "2" = "navy", "1" = "darkgoldenrod2"))+
  ggtitle("Dispersión de los datos por frecuencia")+
  theme(plot.title=element_text(color='gray27',hjust=0.5))


## CREAMOS 6 ZONAS CON LOS CLIENTES QUE TIENEN FRECUENCIA 1
library(geosphere)
subbases<- split (ubicaciones, ubicaciones$Frecuencia) 
u_Freq1<-subbases$`1`

distancias <- u_Freq1 %>%
  select(Longitud, Latitud) %>% # extraemos las columnas de longitud y latitud
  distm(fun = distGeo) %>% # Calculamos las distancias de acuerdo a la curvatura de la Tierra
  as.dist() # convertimos en una matriz de distancias (el tipo de objeto que DBSCAN espera)
set.seed(444)
clusters <- kmeans(distancias, centers = 6)
zonasFreq1 <- transform(u_Freq1, Zona = clusters$cluster)

sb_zonas <- split (zonasFreq1, zonasFreq1$Zona) 
NumClientes <-NULL
Vol_Entregas <- NULL
Centroides <- data.frame(Zonas=seq(1,6),Longitud=double(6),Latitud=double(6))

## Objetivo de tamaños balanceados considerando un error del 5%
size <- 1.05*sum(ubicaciones$Frecuencia)/6 
volumen <- 1.05*sum(ubicaciones$Frecuencia*ubicaciones$Vol_Entrega)/6

# ZONAS
#####
vec <- c("Id_Cliente","Vol_Entrega","Latitud","Longitud")
Zona1 <- subset( sb_zonas$`1`, select = vec )
Centroides[1,2] <- centroid(Zona1[4:3])[1]
Centroides[1,3] <- centroid(Zona1[4:3])[2]
NumClientes[1] <- nrow(Zona1)
Vol_Entregas[1] <- sum(Zona1$Vol_Entrega)

Zona2 <- subset( sb_zonas$`2`, select = vec )
Centroides[2,2] <- centroid(Zona2[4:3])[1]
Centroides[2,3] <- centroid(Zona2[4:3])[2]
NumClientes[2] <- nrow(Zona2)
Vol_Entregas[2] <- sum(Zona2$Vol_Entrega)

Zona3 <- subset( sb_zonas$`3`, select = vec )
Centroides[3,2] <- centroid(Zona3[4:3])[1]
Centroides[3,3] <- centroid(Zona3[4:3])[2]
NumClientes[3] <- nrow(Zona3)
Vol_Entregas[3] <- sum(Zona3$Vol_Entrega)

Zona4 <- subset( sb_zonas$`4`, select = vec )
Centroides[4,2] <- centroid(Zona4[4:3])[1]
Centroides[4,3] <- centroid(Zona4[4:3])[2]
NumClientes[4] <- nrow(Zona4)
Vol_Entregas[4] <- sum(Zona4$Vol_Entrega)

Zona5 <- subset( sb_zonas$`5`, select = vec )
Centroides[5,2] <- centroid(Zona5[4:3])[1]
Centroides[5,3] <- centroid(Zona5[4:3])[2]
NumClientes[5] <- nrow(Zona5)
Vol_Entregas[5] <- sum(Zona5$Vol_Entrega)

Zona6 <- subset( sb_zonas$`6`, select =vec )
Centroides[6,2] <- centroid(Zona6[4:3])[1]
Centroides[6,3] <- centroid(Zona6[4:3])[2]
NumClientes[6] <- nrow(Zona6)
Vol_Entregas[6] <- sum(Zona6$Vol_Entrega)


# AÑADIR FREQ>1
#####
u_Freq2a3 <- rbind(subbases$`3`, subbases$`2`)
library(Rfast)
for (i in 1:nrow(u_Freq2a3)) {
  ind <- distm(rbind(u_Freq2a3[1,6:5],Centroides[,2:3]), fun = distGeo)[1,2:7]
  check <- 0
  for (j in 1:u_Freq2a3[i,3]) {
    Z=1
    if (j==1){k=j} else{k=k+1}
    while (Z!=0) {
      if (match(nth(ind, k),ind)==1 & NumClientes[1]<size & Vol_Entregas[1]<volumen){
        Zona1 = rbind( Zona1, subset(u_Freq2a3[i,], select = vec) )
        NumClientes[1] = NumClientes[1]+1
        Vol_Entregas[1] = Vol_Entregas[1]+u_Freq2a3[i,4]
        Z=0
      }
      else if (match(nth(ind, k),ind)==2 & NumClientes[2]<size & Vol_Entregas[2]<volumen){
        Zona2 = rbind( Zona2, subset(u_Freq2a3[i,], select = vec) )
        NumClientes[2] = NumClientes[2]+1
        Vol_Entregas[2] = Vol_Entregas[2]+u_Freq2a3[i,4]
        Z=0
      }
      else if (match(nth(ind, k),ind)==3 & NumClientes[3]<size & Vol_Entregas[3]<volumen){
        Zona3 = rbind( Zona3, subset(u_Freq2a3[i,], select = vec) )
        NumClientes[3] = NumClientes[3]+1
        Vol_Entregas[3] = Vol_Entregas[3]+u_Freq2a3[i,4]
        Z=0
      }
      else if (match(nth(ind, k),ind)==4 & NumClientes[4]<size & Vol_Entregas[4]<volumen){
        Zona4 = rbind( Zona4, subset(u_Freq2a3[i,], select = vec) )
        NumClientes[4] = NumClientes[4]+1
        Vol_Entregas[4] = Vol_Entregas[4]+u_Freq2a3[i,4]
        Z=0
      }
      else if (match(nth(ind, k),ind)==5 & NumClientes[5]<size & Vol_Entregas[5]<volumen){
        Zona5 = rbind( Zona5, subset(u_Freq2a3[i,], select = vec) )
        NumClientes[5] = NumClientes[5]+1
        Vol_Entregas[5] = Vol_Entregas[5]+u_Freq2a3[i,4]
        Z=0
      }
      else if (match(nth(ind, k),ind)==6 & NumClientes[6]<size & Vol_Entregas[6]<volumen){
        Zona6 = rbind( Zona6, subset(u_Freq2a3[i,], select = vec) )
        NumClientes[6] = NumClientes[6]+1
        Vol_Entregas[6] = Vol_Entregas[6]+u_Freq2a3[i,4]
        Z=0
      }
      else {k=k+1}
    }
  }
}



#####
codigozonas<- rep(1:6,times=NumClientes)
Zonas<-rbind(Zona1,Zona2,Zona3,Zona4,Zona5,Zona6)
Zonas<-transform(Zonas, Zona=codigozonas)

nopasa<- rep(0,times=nrow(Zonas))
Output <- data.frame(Id_Cliente=Zonas$Id_Cliente, D1=nopasa,D2=nopasa,D3=nopasa,D4=nopasa,D5=nopasa,D6=nopasa)
for (i in 1:nrow(Zonas)){
  for (j in 1:6){if (Zonas$Zona[i]==j){
    Output[i,j+1]=1
    break
  }
  }
}
Output<-arrange(Output,Id_Cliente)
Output<-Output %>% 
  group_by(Id_Cliente) %>% 
  summarise_all(funs(trimws(sum(.))))
write.csv(Output,"Output.csv",row.names = FALSE)


## Visualizar las Zonas
#####
ggplot(Zonas)+aes(x=Longitud,y=Latitud)+
  geom_point(aes(colour=as.factor(Zona)))+
  labs(x='Longitud',y='Latitud')+
  scale_color_manual("Zona", values = c("6"="skyblue3","5"="darkred","4"="maroon4","3" = "salmon1", "2" = "navy", "1" = "darkgoldenrod2"))+
  ggtitle("Zonas")+
  theme(plot.title=element_text(color='gray27',hjust=0.5))

## DISTANCIAS ENTRE LOS PUNTOS DE CADA ZONA
#####
DistanciaRutas<-NULL
library(TSP)
ruta1 <- solve_TSP(ATSP(distm(Zona1[4:3], fun = distGeo)/1000,Zona1$Id_Cliente), method = "nn")
DistanciaRutas[1] <- tour_length(ruta1)

ruta2 <- solve_TSP(ATSP(distm(Zona2[4:3], fun = distGeo)/1000,Zona2$Id_Cliente), method = "nn")
DistanciaRutas[2] <- tour_length(ruta2)

ruta3 <- solve_TSP(ATSP(distm(Zona3[4:3], fun = distGeo)/1000,Zona3$Id_Cliente), method = "nn")
DistanciaRutas[3] <- tour_length(ruta3)

ruta4 <- solve_TSP(ATSP(distm(Zona4[4:3], fun = distGeo)/1000,Zona4$Id_Cliente), method = "nn")
DistanciaRutas[4] <- tour_length(ruta4)

ruta5 <- solve_TSP(ATSP(distm(Zona5[4:3], fun = distGeo)/1000,Zona5$Id_Cliente), method = "nn")
DistanciaRutas[5] <- tour_length(ruta5)

ruta6 <- solve_TSP(ATSP(distm(Zona6[4:3], fun = distGeo)/1000,Zona6$Id_Cliente), method = "nn")
DistanciaRutas[6] <- tour_length(ruta6)

ResumenRutas <- data.frame(Zonas=seq(1,6), Clientes=NumClientes, Volumen=Vol_Entregas, DistanciaKm=DistanciaRutas)
