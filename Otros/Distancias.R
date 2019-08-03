
library("gmapdistance")



Dist = gmapsdistance(or="6.2458218+-75.574752", des="6.2448442+-75.5756626",
                           mode = "driving", key ="AIzaSyDTpTbw19_FAOtRd0dLHIllsla3jLDq0h4", 
                           combinations = "all")

Dist

library(xlsx)

write.table(resultados, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDistancias.txt", row.names = FALSE)

write.csv(resultados, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDistancias.txt", row.names = FALSE)

write.xlsx(resultados, file = "C:/Users/sin definir/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/MatrizDistancias.xlsx",
           sheetName = "Distancias", col.names=TRUE, row.names=TRUE, append=FALSE)

