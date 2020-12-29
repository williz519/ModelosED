# Github installation
install.packages("devtools")
devtools::install_github("rodazuero/gmapsdistance")

library("gmapsdistance")

origin=Oriental$Coordenadas
fix(origin)

destination=Occidental$Coordenadas
fix(destination)




or=c("6.198954+-75.57458", "6.229264+-75.570485", "6.235712+-75.56956", 
     "6.253639+-75.564185", "6.266524+-75.563666", "6.206794+-75.57106", 
     "6.242804+-75.55743", "6.261663+-75.565777", "6.213903+-75.558228", 
     "6.252321+-75.567432", "6.269552+-75.554713", "6.265994+-75.562178", 
     "6.223611+-75.574438", "6.248156+-75.55767", "6.244864+-75.57721", 
     "6.269477+-75.565793", "6.209042+-75.56778", "6.271251+-75.55765", 
     "6.282256+-75.561384", "6.26785+-75.568856")

des=c("6.221713+-75.59181", "6.21883+-75.58655", "6.214981+-75.59758", 
      "6.232039+-75.604523", "6.241308+-75.58755", "6.207895+-75.5909", 
      "6.262951+-75.604237", "6.25696+-75.59199", "6.273829+-75.592982", 
      "6.286315+-75.574404", "6.276734+-75.58055", "6.255575+-75.601588", 
      "6.275722+-75.59619", "6.293733+-75.57159", "6.245128+-75.596843", 
      "6.27907+-75.571295", "6.234699+-75.58744", "6.230405+-75.609889", 
      "6.24448+-75.589435", "6.241586+-75.5765")

Puentes = c("6.286183+-75.56599", "6.275893+-75.56978", "6.265202+-75.57218", "6.260895+-75.57370",
            "6.254212+-75.57729", "6.247960+-75.58028", "6.239645+-75.57740", "6.236897+-75.57647", 
            "6.231378+-75.57567", "6.213594+-75.57807", "6.204031+-75.57959", "6.195293+-75.58154")



resultados1 = gmapsdistance(or, Puentes,
                           mode = "driving", shape= "long", key ="AIzaSyDP4YQ9BZH0U0oicJ7nFkP7groRV9eGkEQ", 
                           combinations = "all")

resultados2 = gmapsdistance(Puentes, des,
                            mode = "driving", shape= "long", key ="AIzaSyDP4YQ9BZH0U0oicJ7nFkP7groRV9eGkEQ" , 
                            combinations = "all")

resultados1

library(xlsx)

write.table(resultados1, file = "/Users/williz/Desktop/ModelosED/Mapas/MatrizDistancias1.txt", row.names = FALSE)
write.table(resultados2, file = "/Users/williz/Desktop/ModelosED/Mapas/MatrizDistancias2.txt", row.names = FALSE)

write.csv(resultados1, file = "/Users/williz/Desktop/ModelosED/Mapas/MatrizDistancias1.txt", row.names = FALSE)
write.csv(resultados2, file = "/Users/williz/Desktop/ModelosED/Mapas/MatrizDistancias2.txt", row.names = FALSE)

write.xlsx(resultados1, file = "/Users/williz/Desktop/ModelosED/Mapas/MatrizDistancias1.xlsx",
            sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)
write.xlsx(resultados2, file = "/Users/williz/Desktop/ModelosED/Mapas/MatrizDistancias2.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)


