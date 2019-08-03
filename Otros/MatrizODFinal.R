

library(ggplot2)
library(ggmap)
library(leaflet)
library(readxl)
library(gmapsdistance)
library(xlsx)


register_google(key = "AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4")

# Se carga la MatrizODFinal con los solo los 12 puntos escogidos de la ciudad

MatrizOD <- read_excel("C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/MatrizOD.xlsx", 
                       sheet = "ODMatriz")

datos=MatrizOD

#Se grafica los puntos

#med <- get_map(location = 'Medellin, Colombia', zoom = 13, maptype = "roadmap")

map<- get_googlemap(center = c(lon = -75.576451, lat = 6.241869),zoom = 13, 
                    size=c(640,640), scale =2, format = "png8", maptype = "roadmap") 

MaOD <- ggmap(map) + geom_point(data=datos, aes(x=Longitud, y=Latitud, col="red"),
                                alpha = 0.6, show.legend = FALSE, size=2)

MaOD


#Origenes Sector Occidental y Destinos Sector Oriental

or=c("6.286315+-75.574404", "6.273829+-75.592982", "6.255575+-75.601588", 
     "6.24448+-75.589435", "6.232039+-75.604523", "6.21883+-75.58655")

des=c("6.209042+-75.56778", "6.229264+-75.570485", "6.244864+-75.57721", "6.253639+-75.564185",
      "6.266524+-75.563666", "6.282256+-75.561384")




#Se Calcula la Matriz de distancias y tiempos entre origenes (occiental) y destinos (oriental)

resultados = gmapsdistance(or, des,
                           mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                           combinations = "all")

resultados

#Se guardan en arcivos CVS y Excel
write.csv(resultados, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDistanciasFinal.txt", row.names = FALSE)

write.xlsx(resultados, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDistanciasFinal.xlsx",
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

