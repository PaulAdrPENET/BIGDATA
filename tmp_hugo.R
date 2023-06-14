data <- read.csv2("stat_hugo.csv", sep=";")

#affiche les différents paramètres d'une colonne 
cat=data$num_veh
print(table(cat))

#colonne descr_grav
data$descr_grav  <- factor(data$descr_grav, level=c("Indemne","Tué","Blessé léger","Blessé hospitalisé"))
data$descr_grav <- as.numeric(data$descr_grav)

#Indemne = 1
#Tué = 2
#Blessé léger = 3
#Blessé hospitalisé = 4

#colonne descr_type_col
data$descr_type_col <- factor(data$descr_type_col,level=c("Autre collision ","Deux véhicules - Frontale","Deux véhicules – Par l’arrière ","Deux véhicules – Par le coté","Sans collision","Trois véhicules et plus – Collisions multiples","Trois véhicules et plus – En chaîne"))
data$descr_type_col<- as.numeric(data$descr_type_col)

#Autre collision = 1
#Deux véhicules - Frontale = 2
#Deux véhicules – Par l’arrière = 3
#Deux véhicules – Par le coté = 4
#Sans collision = 5
#Trois véhicules et plus – Collisions multiples = 6
#Trois véhicules et plus – En chaîne = 7

#colonne description_intersection
data$description_intersection <- factor(data$description_intersection)
data$description_intersection <- as.numeric(data$description_intersection)

#Autre intersection = 1
#Giratoire = 2
#Hors intersection = 3
#Intersection à plus de 4 branches = 4
#Intersection en T = 5
#Intersection en X = 6
#Intersection en Y = 7
#Passage à niveau = 8
#Place = 9

#colonne descr_etat_surf
data$descr_etat_surf <- factor(data$descr_etat_surf)
data$descr_etat_surf <- as.numeric(data$descr_etat_surf)

#Autre = 1
#Boue = 2
#Corps gras – huile = 3
#Enneigée = 4
#Flaques = 5
#Inondée = 6
#Mouillée = 7
#Normale = 8
#Verglacée = 9

#colonne descr_lum
data$descr_lum <- factor(data$descr_lum)
data$descr_lum <- as.numeric(data$descr_lum)

#Crépuscule ou aube = 1
#Nuit avec éclairage public allumé = 2
#Nuit avec éclairage public non allumé = 3
#Nuit sans éclairage public = 4
#Plein jour = 5

#colonne descr_agglo
data$descr_agglo <- factor(data$descr_agglo)
data$descr_agglo <- as.numeric(data$descr_agglo)

#En agglomération = 1
#Hors agglomération = 2

#colonne descr_cat_veh
data$descr_cat_veh <- factor(data$descr_cat_veh)
data$descr_cat_veh <- as.numeric(data$descr_cat_veh)

#Autobus = 1
#Autocar = 2
#Autre véhicule = 3
#Bicyclette = 4
#Cyclomoteur <50cm3 =5
#Engin spécial = 6
#Motocyclette > 125 cm3 = 7
#Motocyclette > 50 cm3 et <= 125 cm3 = 8
#PL > 3,5T + remorque = 9
#PL seul > 7,5T = 10
#PL seul 3,5T <PTCA <= 7,5T = 11
# Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé) = 12
#Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé) = 13
#Scooter < 50 cm3 = 14
#Scooter > 125 cm3 = 15
#Scooter > 50 cm3 et <= 125 cm3 = 16
#Tracteur agricole = 17
#Tracteur routier + semi-remorque = 18
#Tracteur routier seul = 19
#Train = 20
#Tramway = 21
#VL seul = 22
#Voiturette (Quadricycle à moteur carrossé) (anciennement "voiturette ou tricycle à moteur") = 23
#VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque = 24

# Modification de la colonne descr_athmo 
data$descr_athmo <- factor(data$descr_athmo)
data$descr_athmo <- as.numeric(data$descr_athmo)

# 1 = Autre
# 2 = Brouillard – fumée 
# 3 = Neige – grêle 
# 4 = Normale 
# 5 = Pluie forte 
# 6 = Pluie légère
# 7 = Temps couvert
# 8 = Temps éblouissant 
# 9 = Vent fort – tempête +


# Visualisation

# Nombre d’accidents en fonction des conditions athmosphériques
datagraph<-data
conditions_athmospheriques <- subset(datagraph,select =descr_athmo)#récupère la colonne avec les informations
conditions_athmospheriques <- table(conditions_athmospheriques)#met sous la forme de tableau avec le nombre de récurrence de chaque paramètre
par(cex.axis = 0.7) #permet de réduir la police d'écriture des axes
barplot(conditions_athmospheriques,xlab="conditions atmosphériques",ylab="nombre d'accidents",main="Nombre d’accidents en fonction des conditions athmosphériques",names.arg = c("Autre", "Brouillard – fumée","Neige – grêle", "Normale", "Pluie forte", "Pluie légère", "Temps couvert", "Temps éblouissant", "Vent fort– tempête"),
        las = 1.5,cey.names = 0.5)

# Nombre d’accidents en fonction de la description de la surface
description_surface <- subset(data,select = descr_etat_surf)
description_surface <- table(description_surface)
barplot(description_surface,xlab="description de la surface",ylab="nombre d'accidents",main="Nombre d’accidents en fonction de la description de la surface",
        names.arg = c("Autre",'Boue',"Corps gras – huile","Enneigée","Flaques","Inondée","Mouillée","Normale","Verglacée"),las=1.5 )

# Nombre d’accidents selon la gravité 
description_grav <- subset(data,select = descr_grav)
description_grav <- table(description_grav)
barplot(description_grav,xlab="description de la gravité",ylab="nombre d'accidents",main="Nombre d’accidents selon la gravité",
        names.arg=c("Indemne","Tué","Blessé léger","Blessé hospitalisé"),las=1.5)



#carte :
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")

library(sf)
library(ggplot2)
library(dplyr)
liste_departements_normandie =c(50,14,67,27,76)
accidents_regions_normandie <-liste_departements_normandie %in% data$
url_geojson <-"https://france-geojson.gregoiredavid.fr/repo/regions/normandie/region-normandie.geojson"
regions_normandie_geojson <- st_read(url_geojson)
ggplot() +
  geom_sf(data = regions_geojson) +
  geom_point(data = accidents_regions_normandie, aes(x = longitude, y = latitude)) +
  theme_void()


#leaflet
install.packages("leaflet")
install.packages("geojsonio")

library(leaflet)
library(geojsonio)

#liste des départements par région :
departements_normandie =c("50","14","67","27","76")
departements_hauts_de_france =c("62","59","80","02","60")
departements_ile_de_france =c("93","75","94","92","95","78","91","77")
departements_grand_est =c("08","51","10","52","55","54","57","88","67","68")
departements_bourgogne_franche_compte =c("89","58","21","71","70","39","25")
departements_bretagne =c("29","22","56","35")
departements_pays_de_la_loire =c("53","72","49","44","85")
departements_centre_val_de_loire =c("28","41","45","37","36","18")
departements_nouvelle_aquitaine =c("79","86","17","16","87","23","33","24","19","40","47","64")
departements_occitanie =c("46","12","48","82","32","31","65","09","11","66","81","34","30")
departements_auvergne_rhone_alpes =c("03","63","42","69","01","74","73","38","43","07","26","15")
departements_provence_alpes_cote_azur =c("05","04","84","13","82","06")
departements_corse =c("2B","2A")



colonne_2prem_insee <- substr(data$id_code_insee, 1,2)#récupère les 2 premiers caractère de la colonne id_code_insee

#vérifie si les codes postaux correpondent à la région demandée avec %in% et les compte avec table()
accidents_normandie <- table(colonne_2prem_insee %in% departements_normandie)
accidents_hauts_de_france <- table(colonne_2prem_insee %in% departements_hauts_de_france)
accidents_ile_de_france <- table(colonne_2prem_insee %in% departements_ile_de_france)
accidents_grand_est <- table(colonne_2prem_insee %in% departements_grand_est)
accidents_bourgogne_franche_compte <- table(colonne_2prem_insee %in% departements_bourgogne_franche_compte)
accidents_bretagne <- table(colonne_2prem_insee %in% departements_bretagne)
accidents_pays_de_la_loire <- table(colonne_2prem_insee %in% departements_pays_de_la_loire)
accidents_centre_val_de_loire <- table(colonne_2prem_insee %in% departements_centre_val_de_loire)
accidents_nouvelle_aquitaine <- table(colonne_2prem_insee %in% departements_nouvelle_aquitaine)
accidents_occitanie <- table(colonne_2prem_insee %in% departements_occitanie)
accidents_auvergne_rhone_alpes <- table(colonne_2prem_insee %in% departements_auvergne_rhone_alpes)
accidents_provence_alpes_cote_azur <- table(colonne_2prem_insee %in% departements_provence_alpes_cote_azur)
accidents_corse <- table(colonne_2prem_insee %in% departements_corse)


regions_geojson <- geojsonio::geojson_read("regions.geojson", what = "sp") 


carte_regions <- leaflet() 
carte_regions <-  carte_regions%>%addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = regions_geojson,weight = 1,fillOpacity = 0.6,fillColor = "lightblue", color = "white")%>%
  setView(lng = 1, lat = 46, zoom = 5) #permet de centrer la carte et de la mettre à la bonne dimension

#ajout des marqueurs sur la carte, région par région :

carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 49,lng = 0.4,color = "red",fillOpacity = 0.5,label = accidents_normandie[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 50.4,lng = 2.7,color = "red",fillOpacity = 0.5,label = accidents_hauts_de_france[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 48.9,lng = 2.5,color = "red",fillOpacity = 0.5,label = accidents_ile_de_france[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 48.5,lng = 6,color = "red",fillOpacity = 0.5,label = accidents_grand_est[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 47,lng = 5,color = "red",fillOpacity = 0.5,label = accidents_bourgogne_franche_compte[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 48.2,lng = -2.9,color = "red",fillOpacity = 0.5,label = accidents_bretagne[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 47.1,lng = -1.6,color = "red",fillOpacity = 0.5,label = accidents_pays_de_la_loire[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 47.6,lng = 1.8,color = "red",fillOpacity = 0.5,label = accidents_centre_val_de_loire[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 45.3,lng = -0.2,color = "red",fillOpacity = 0.5,label = accidents_nouvelle_aquitaine[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 43.6,lng = 1.9,color = "red",fillOpacity = 0.5,label = accidents_occitanie[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 45.4,lng = 4.37,color = "red",fillOpacity = 0.5,label = accidents_auvergne_rhone_alpes[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 43.7,lng = 6.18,color = "red",fillOpacity = 0.5,label = accidents_provence_alpes_cote_azur[2])
carte_regions <- carte_regions %>%addCircleMarkers(data = regions_geojson,lat = 42.1,lng = 9,color = "red",fillOpacity = 0.5,label = accidents_corse[2])


#carte_regions <- carte_regions %>% clearMarkers() #permet de supprimer les points en cas d'erreur
  
  
carte_regions #affiche la carte des régions de France

#carte_departements
departements_geojson <- geojsonio::geojson_read("departements.geojson", what = "sp") 


carte_departements <- leaflet() 
carte_departements <-  carte_departements%>%addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = departements_geojson,weight = 1,fillOpacity = 0.6,fillColor = "lightblue", color = "white")%>%
  setView(lng = 1, lat = 46, zoom = 5) #permet de centrer la carte et de la mettre à la bonne dimension


# Création d'un Dataset contenant le nombre d'accident, et les coordonnées géographiques de chaque département 

data$departement <- substr(data$id_code_insee, 1,2)
data_departement <- aggregate(Num_Acc ~ departement, data, FUN = length)
data_coord <- read.csv2("coord_depart.csv", sep =",")
data_coord$Departement <- as.numeric(data_coord$Departement)
data_coord$Latitude.la.plus.au.nord <- as.numeric(data_coord$Latitude.la.plus.au.nord)
data_coord$Latitude.la.plus.au.sud <- as.numeric(data_coord$Latitude.la.plus.au.sud)
data_coord$Longitude.la.plus.à.l.est <- as.numeric(data_coord$Longitude.la.plus.à.l.est)
data_coord$Longitude.la.plus.à.l.ouest <- as.numeric(data_coord$Longitude.la.plus.à.l.ouest)
latitude_moy <- aggregate(cbind(Latitude = rowMeans(cbind(Latitude.la.plus.au.nord, Latitude.la.plus.au.sud))) ~ Departement, data_coord, FUN = mean)
longitude_moy <- aggregate(cbind(Longitude = rowMeans(cbind(Longitude.la.plus.à.l.est, Longitude.la.plus.à.l.ouest))) ~ Departement, data_coord, FUN = mean)
data_coord_moy <- merge(latitude_moy, longitude_moy, by = "Departement")
summary(data_coord_moy)
summary(data_departement)
data_departement$departement <- as.character(data_departement$departement)
data_final_departement <- merge(data_departement, data_coord_moy, by.x = "departement", by.y = "Departement", all.x = TRUE)
#On remarque que certaines valeurs sont manquantes (par exemple la corse)
data_final_departement <- data_final_departement[complete.cases(data_final_departement$Latitude, data_final_departement$Longitude), ]
#On supprime les valeurs non complètes (NA)

#On fait un nouveau dataset avec cette fois-çi uniquement les accidents graves (tués)

data_departement_mort <- aggregate(descr_grav ~ departement, data=subset(data, descr_grav == 2), FUN = length)
data_departement_hospitalises <- aggregate(descr_grav ~ departement, data=subset(data, descr_grav == 4), FUN = length)
colnames(data_departement_mort)[2]<-"tués"
colnames(data_departement_hospitalises)[2]<-"hospitalises"
data_final_departement <- merge(data_final_departement, data_departement_mort, by.x = "departement", by.y = "departement", all.x = TRUE)
data_final_departement <- merge(data_final_departement, data_departement_hospitalises, by.x = "departement", by.y = "departement", all.x = TRUE)




#boucle qui permet d'ajouter chaque marqueur sur la carte des départements
for (i in 1:nrow(data_final_departement)) {
  latitude <- data_final_departement[i, "Latitude"]
  longitude <- data_final_departement[i, "Longitude"]
  nombre_accidents <- data_final_departement[i, "Num_Acc"]
  
  carte_departements <- carte_departements %>% addCircleMarkers(data = data_final_departement,
                              lat = latitude,
                              lng = longitude,
                              radius = 2,
                              color = "red",
                              fill = TRUE,
                              fillOpacity = 0.5,
                              label = nombre_accidents)
}

carte_departements#affiche la carte des départements




#carte des régions comportant le nombre de tués et de Blessés hospitalisés
for (i in 1:nrow(accidents_reg)) {
  latitude <- accidents_reg[i, "Latitude"]
  longitude <- accidents_reg[i, "Longitude"]
  nombre_tues <- accidents_reg[i, "nombre_tue"]
  nombre_blesse_hospitalise <- accidents_reg[i, "nombre_blesse_hospitalise"]
  
  carte_departements_gravite <- carte_departements_gravite %>% 
    addCircleMarkers(data = accidents_region,
                     lat = latitude,
                     lng = longitude,
                     color = "red",
                     fill = TRUE,
                     fillOpacity = 0.5,
                     label = paste("nombre_tue : ", nombre_tues, "<br>", "nombre_blesse_hospitalise : ", nombre_blesse_hospitalise))
}


#carte des départements comportant le nombre de tués et de Blessés hospitalisés
carte_departements_gravite <- leaflet() 
carte_departements_gravite <-  carte_departements_gravite%>%addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = departements_geojson,weight = 1,fillOpacity = 0.6,fillColor = "lightblue", color = "white")%>%
  setView(lng = 1, lat = 46, zoom = 5) #permet de centrer la carte et de la mettre à la bonne dimension



for (i in 1:nrow(data_final_departement)) {
  latitude <- data_final_departement[i, "Latitude"]
  longitude <- data_final_departement[i, "Longitude"]
  nombre_tues <- data_final_departement[i, "tués"]
  nombre_blesse_hospitalise <- data_final_departement[i, "hospitalises"]
  
  carte_departements_gravite <- carte_departements_gravite %>% 
    addCircleMarkers(data = data_final_departement,
                              lat = latitude,
                              lng = longitude,
                              radius = 2,
                              color = "red",
                              fill = TRUE,
                              fillOpacity = 0.5,
                              label = paste("nombre tués : ", nombre_tues, ",", "nombre blessés hospitalisés : ", nombre_blesse_hospitalise))
}
carte_departements_gravite





#Analyse des données


