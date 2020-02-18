library(ggplot2)
library(ggmap)
library(leaflet)
library(readxl)
library(gmapsdistance)
library(xlsx)

dataset <- read.table('/Users/williz/Desktop/ModelosED/ACO/Med.txt', header =TRUE, sep =' ' )



View(dataset)

write.table(dataset, 
            file="/Users/williz/Desktop/ModelosED/ACO/NodosMed.csv", sep="\t", dec=".")
