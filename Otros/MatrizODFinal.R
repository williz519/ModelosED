

library(ggplot2)
library(ggmap)
library(leaflet)
library(readxl)
library(gmapsdistance)
library(xlsx)


register_google(key = "AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4")

# Se carga la MatrizODFinal con los solo los 12 puntos escogidos de la ciudad

MatrizODFull <- read_excel("/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MatrizOD16Ptos.xlsx", 
                       sheet = "ODFull")

MatrizOD12ptos <- read_excel("/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MatrizOD16Ptos.xlsx", 
                             sheet = "ODMatriz")

Puentes <- read_excel("/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MatrizOD16Ptos.xlsx", 
                      sheet = "Puentes")


#Se grafica los puntos

#med <- get_map(location = 'Medellin, Colombia', zoom = 13, maptype = "roadmap")

map<- get_googlemap(center = c(lon = -75.576451, lat = 6.241869),zoom = 13, 
                    size=c(640,640), scale =2, format = "png8", maptype = "roadmap") 

ggmap(map)

MaOD <- ggmap(map) + geom_point(data=MatrizODFull, aes(x=Longitud, y=Latitud, col="red"),
                                alpha = 0.6, show.legend = FALSE, size=2)

MaOD

#Grafico de los 16 puntos
map1<- get_googlemap(center = c(lon = -75.576451, lat = 6.241869),zoom = 13, 
                    size=c(640,640), scale =2, format = "png8", maptype = "roadmap") 

MaOD16 <- ggmap(map1) + geom_point(data=MatrizOD12ptos, aes(x=Longitud, y=Latitud, col="red"),
                                alpha = 0.6, show.legend = FALSE, size=2)

MaOD16


#Grafico de puentes
map2<- get_googlemap(center = c(lon = -75.576451, lat = 6.241869),zoom = 13, 
                     size=c(640,640), scale =2, format = "png8", maptype = "roadmap") 

MaPUENTES <- ggmap(map2) + geom_point(data=Puentes, aes(x=Longitud, y=Latitud, col="blue"),
                                   alpha = 0.6, show.legend = FALSE, size=2)

MaPUENTES



#Origenes Sector Occidental y Destinos Sector Oriental

occidente <- c("6.286315+-75.574404", "6.273829+-75.592982", "6.27907+-75.571295", 
     "6.241308+-75.58755", "6.232039+-75.604523", "6.25696+-75.59199", "6.207895+-75.5909",
     "6.21883+-75.58655")

oriente <- c("6.282256+-75.561384", "6.266524+-75.563666", "6.261663+-75.565777", "6.253639+-75.564185",
      "6.252321+-75.567432", "6.235712+-75.56956", "6.209042+-75.56778", "6.223611+-75.574438")

puentes <-c("6.286183+-75.565989", "6.275893+-75.56978", "6.265202+-75.572182", "6.260895+-75.573703",
            "6.254212+-75.577288", "6.24796+-75.580281", "6.239645+-75.577397", "6.236897+-75.576467",
            "6.231378+-75.57567", "6.213594+-75.578073", "6.204031+-75.579593", "6.195293+-75.581542")



#Se Calcula la Matriz de distancias y tiempos entre origenes (occiental) y destinos (oriental)

orientepuentes = gmapsdistance(oriente, puentes,
                           mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                           combinations = "all")

orientepuentes

occidentepuentes = gmapsdistance(occidente, puentes,
                                 mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                                 combinations = "all")
occidentepuentes

#Se guardan en arcivos CVS y Excel
write.csv(orientepuentes, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistOriente_Puentes.txt", row.names = FALSE)
write.csv(occidentepuentes, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistOccidente_Puentes.txt", row.names = FALSE)


write.xlsx(orientepuentes, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistOriente_Puentes.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)

write.xlsx(occidentepuentes, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistOccidente_Puentes.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)


#Distancias puentes a nodos

puentesoriente = gmapsdistance(puentes, oriente,
                               mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                               combinations = "all")

puentesoriente

puentesoccidente = gmapsdistance(puentes, occidente,
                                 mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                                 combinations = "all")
puentesoccidente

#Se guardan en arcivos CVS y Excel
write.csv(puentesoriente, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistPuentes_Oriente.txt", row.names = FALSE)
write.csv(puentesoccidente, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistPuentes_Occidente.txt", row.names = FALSE)


write.xlsx(puentesoriente, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistPuentes_Oriente.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)

write.xlsx(puentesoccidente, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistPuentes_Occidente.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)

#Distancia entre puentes

puentes_puentes = gmapsdistance(puentes, puentes,
                               mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                               combinations = "all")
puentes_puentes

write.csv(puentes_puentes, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistPuentes_Puentes.txt", row.names = FALSE)


write.xlsx(puentes_puentes, file = "/Users/williz/Desktop/ModelosED/MatrizODMedellin Modelos/MxDistPuentes_Puentes.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)


#Origenes Sector Oriental y Destinos Sector  Occidental

des1=c("6.286315+-75.574404", "6.273829+-75.592982", "6.255575+-75.601588", 
     "6.24448+-75.589435", "6.232039+-75.604523", "6.21883+-75.58655")

or1=c("6.209042+-75.56778", "6.229264+-75.570485", "6.244864+-75.57721", "6.253639+-75.564185",
      "6.266524+-75.563666", "6.282256+-75.561384")


#Se Calcula la Matriz de distancias y tiempos entre origenes (oriental) y destinos (occidental)

resul_Ori_Occ = gmapsdistance(or1, des1,
                           mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                           combinations = "all")

resul_Ori_Occ

#Se guardan en arcivos CVS y Excel
write.csv(resul_Ori_Occ, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizD_Ori_Occ.txt", row.names = FALSE)

write.xlsx(resul_Ori_Occ, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizD_Ori_Occ.txt.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)




#Origenes Todos y Destinos Todos

or2=c("6.209042+-75.56778", "6.229264+-75.570485", "6.244864+-75.57721",
      "6.253639+-75.564185", "6.266524+-75.563666", "6.282256+-75.561384",
      "6.286315+-75.574404", "6.273829+-75.592982", "6.255575+-75.601588", 
      "6.24448+-75.589435", "6.232039+-75.604523", "6.21883+-75.58655")

des2=c("6.209042+-75.56778", "6.229264+-75.570485", "6.244864+-75.57721", 
       "6.253639+-75.564185", "6.266524+-75.563666", "6.282256+-75.561384",
       "6.286315+-75.574404", "6.273829+-75.592982", "6.255575+-75.601588", 
       "6.24448+-75.589435", "6.232039+-75.604523", "6.21883+-75.58655")




#Se Calcula la Matriz de distancias y tiempos entre origenes (oriental) y destinos (occidental)

resul_all = gmapsdistance(or2, des2,
                           mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                           combinations = "all")

resul_all

#Se guardan en arcivos CVS y Excel
write.csv(resul_all, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDT_All.txt", row.names = FALSE)

write.xlsx(resul_all, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDT_All.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)

