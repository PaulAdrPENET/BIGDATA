# Visualisation : 

# Nombre d’accidents en fonction des conditions athmosphériques


# Visualisation Hugo Monnier

# Nombre d’accidents en fonction de la description de la surface
description_surface <- subset(data,select =descr_etat_surf)
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



# Visualisation Paul-Adrien PENET :


data$hours <- format(data$date, "%H") 
data_hourly <- aggregate(Num_Acc ~ hours, data, FUN = length)


#Nombre d'accidents en fonction de la luminosité :
data_luminosite <- aggregate(Num_Acc ~ descr_lum, data, FUN = length)

barplot(data_luminosite$Num_Acc,
        main = "Nombre d'accident en fonction de la luminosite",
        xlab = "Type de luminosite",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = data_luminosite$descr_lum,
        las = 1.5
)
#Nombre d'accidents en fonction du type de véhicule :
data_vehicule <- aggregate(Num_Acc ~ descr_cat_veh, data, FUN = length)

barplot(data_vehicule$Num_Acc,
        main = "Nombre d'accidents selon le type de véhicule",
        xlab = "Type du véhicule",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = data_vehicule$descr_cat_veh,
        las = 1.5
)
#Nombre d'accidents en du type d'intersection :
data_intersection <- aggregate(Num_Acc ~ description_intersection, data, FUN = length)

barplot(data_intersection$Num_Acc,
        main = "Nombre d'accidents selon le type d'intersection",
        xlab = "Type de véhicule",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = data_intersection$description_intersection,
        las = 1.5
)
#Nombre d'accidents en agglomération ou hors agglomération :
data_agglomeration <- aggregate(Num_Acc ~ descr_agglo, data, FUN = length)

barplot(data_agglomeration$Num_Acc,
        main = "Nombre d'accidents en agglo ou hors agglo",
        xlab = "Type",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = data_agglomeration$descr_agglo,
        las = 1.5
)

#Nombre d'accidents par tranches d'heures.

barplot(data_hourly$Num_Acc,
        main = "Nombre d'accidents par tranches d'heures",
        xlab = "Heures de la journée",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = data_hourly$hours,
        las = 1.5
)

# Moyenne mensuelle des accidents. 
summary(data_monthly)
data_monthly$month <- as.numeric(data_monthly$month)
summary(data_monthly)
barplot(data_monthly$Num_Acc,
        main = "Moyenne mensuelle des accidents",
        xlab = "Mois",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = c("Jan", "Fev", "Mar", "Avr", "Mai", "Juin", "Juil", "Aou", "Sep", "Oct", "Nov", "Dec"),
        las = 1,  # Rotation des étiquettes de l'axe x à 0 degré (horizontal)
        border = "gray"  # Couleur des bordures des barres
)
#vecteur <- rep(data_monthly$month -1, times = data_monthly$Num_Acc)
#print(vecteur)
# Création de l'histogramme
#hist(vecteur, 
#     breaks = 12,  # Nombre de barres pour les 12 mois de l'année
#     main = "Nombre d'accidents par mois",
#     xlab = "Mois",
#     ylab = "Fréquence du nombre d'accidents",
#     col = "lightblue",
#     border = "gray",
#     axes = TRUE,
#     ylim = c(0, max(table(vecteur)))  #
#)
#mois_abreges <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", "Juil", "Aoû", "Sep", "Oct", "Nov", "Déc")
#axis(side = 1, at = 1:12, labels = mois_abreges, las = 1.5, tck=0)

#Nombre d'accidents par ville :
data_cities <- aggregate(Num_Acc ~ ville, data, FUN = length)
data_cities_sorted <- data_cities[order(data_cities$Num_Acc, decreasing = TRUE), ]
top_20_villes <- head(data_cities_sorted, 20)
#print(top_20_villes)
top_20_villes_noms <- top_20_villes$ville
#print(top_20_villes_noms)
barplot(top_20_villes$Num_Acc,
        main = "Nombre d'accidents par ville",
        xlab = "Villes",
        ylab = "Nombre d'accidents",
        col = "lightblue",
        names.arg = top_20_villes_noms,
        las = 2,
        cex.names = 0.45,
)


#Nombre d'accidents en fonction des tranches d'âge. 

data_age <- aggregate(Num_Acc ~ age, data, FUN = length)
data_age$age <- (data_age$age - 14)
vecteur_age <- rep(data_age$age -1, times = data_age$Num_Acc)
hist(vecteur_age, 
     breaks = 109,  # Nombre de barres pour les 12 mois de l'année
     main = "Nombre d'accidents par tranche d'âge",
     xlab = "âge",
     ylab = "Nombre d'accidents",
     col = "lightblue",
     border = "gray",
     axes = TRUE,
     ylim = c(0,2500),
     xlim = c(0,110)
)

#On ajoute la moyenne.
moy_age <- mean(vecteur_age)
abline(v = moy_age, col = "blue", lwd = 2)


#affichage des cartes (HUGO MONNIER et PAUL-ADRIEN PENET)

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

#on telecharge la carte des régions de france au format geojson
regions_geojson <- geojsonio::geojson_read("regions.geojson", what = "sp") 

#affichage de la carte des régions :
carte_regions <- leaflet() 
carte_regions <-  carte_regions%>%addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = regions_geojson,weight = 1,fillOpacity = 0.6,fillColor = "lightblue", color = "white")%>%
  setView(lng = 1, lat = 46, zoom = 5) #permet de centrer la carte et de la mettre à la bonne dimension

#ajout des marqueurs sur la carte du nombre d'accidents, région par région :

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



#carte departements avec le nombre d'accidents

#on telecharge la carte des départements de france au format geojson :
departements_geojson <- geojsonio::geojson_read("departements.geojson", what = "sp") 

#affichage de la carte vierge
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

#On fait un nouveau dataset avec cette fois-çi uniquement les accidents graves (tués et blessés hospitalisés)

data_departement_mort <- aggregate(descr_grav ~ departement, data=subset(data, descr_grav == 2), FUN = length)
data_departement_hospitalises <- aggregate(descr_grav ~ departement, data=subset(data, descr_grav == 4), FUN = length)
colnames(data_departement_mort)[2]<-"tués"
colnames(data_departement_hospitalises)[2]<-"hospitalises"
data_final_departement <- merge(data_final_departement, data_departement_mort, by.x = "departement", by.y = "departement", all.x = TRUE)
data_final_departement <- merge(data_final_departement, data_departement_hospitalises, by.x = "departement", by.y = "departement", all.x = TRUE)




#boucle qui permet d'ajouter chaque marqueur sur la carte des départements à partir du dataset ci-dessus
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
#carte_departements <- carte_departements %>% clearMarkers()
carte_departements#affiche la carte des départements


#carte des départements comportant le nombre de tués et de Blessés hospitalisés

#affichage de la carte avec les départements
carte_departements_gravite <- leaflet() 
carte_departements_gravite <-  carte_departements_gravite%>%addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = departements_geojson,weight = 1,fillOpacity = 0.6,fillColor = "lightblue", color = "white")%>%
  setView(lng = 1, lat = 46, zoom = 5) #permet de centrer la carte et de la mettre à la bonne dimension


#boucle qui ajoute les marqueurs à partir du dataset créé au dessus
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
carte_departements_gravite #affiche la carte des départements avec le nombre de tués et de blessés hospitalisés




#affichage de la carte des régions de France avec le nombre de tués et de blessés hospitalisés

#collecte des données dans data_final_departement :
donnees_normandie <- subset(data_final_departement,departement %in% departements_normandie)
donnees_hauts_de_france <- subset(data_final_departement,departement %in% departements_hauts_de_france)
donnees_ile_de_france <- subset(data_final_departement,departement %in% departements_ile_de_france)
donnees_grand_est <- subset(data_final_departement,departement %in% departements_grand_est)
donnees_bourgogne_franche_compte <- subset(data_final_departement,departement %in% departements_bourgogne_franche_compte)
donnees_bretagne <- subset(data_final_departement,departement %in% departements_bretagne)
donnees_pays_de_la_loire <- subset(data_final_departement,departement %in% departements_pays_de_la_loire)
donnees_centre_val_de_loire <- subset(data_final_departement,departement %in% departements_centre_val_de_loire)
donnees_nouvelle_aquitaine <- subset(data_final_departement,departement %in% departements_nouvelle_aquitaine)
donnees_occitanie <- subset(data_final_departement,departement %in% departements_occitanie)
donnees_auvergne_rhone_alpes <- subset(data_final_departement,departement %in% departements_auvergne_rhone_alpes)
donnees_provence_alpes_cote_azur <- subset(data_final_departement,departement %in% departements_provence_alpes_cote_azur)


#affichage de la carte des régions :
carte_regions_gravite <- leaflet() 
carte_regions_gravite <-  carte_regions_gravite%>%addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = regions_geojson,weight = 1,fillOpacity = 0.6,fillColor = "lightblue", color = "white")%>%
  setView(lng = 1, lat = 46, zoom = 5) #permet de centrer la carte et de la mettre à la bonne dimension


#carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 49,lng = 0.4,color = "red",fillOpacity = 0.5,label = accidents_normandie[2])

carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 49,lng = 0.4,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_normandie$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_normandie$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 50.4,lng = 2.7,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_hauts_de_france$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_hauts_de_france$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 48.9,lng = 2.5,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_ile_de_france$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_ile_de_france$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 48.5,lng = 6,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_grand_est$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_grand_est$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 47,lng = 5,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_bourgogne_franche_compte$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_bourgogne_franche_compte$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 48.2,lng = -2.9,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_bretagne$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_bretagne$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 47.1,lng = -1.6,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_pays_de_la_loire$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_pays_de_la_loire$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 47.6,lng = 1.8,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_centre_val_de_loire$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_centre_val_de_loire$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 45.3,lng = -0.2,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_nouvelle_aquitaine$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_nouvelle_aquitaine$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 43.6,lng = 1.9,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_occitanie$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_occitanie$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 45.4,lng = 4.37,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_auvergne_rhone_alpes$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_auvergne_rhone_alpes$hospitalises)))
carte_regions_gravite <- carte_regions_gravite %>%addCircleMarkers(data = regions_geojson,lat = 43.7,lng = 6.18,color = "red",fillOpacity = 0.5,label = paste("nombre tués : ", sum(donnees_provence_alpes_cote_azur$tués), ",", "nombre blessés hospitalisés : ", sum(donnees_provence_alpes_cote_azur$hospitalises)))



carte_regions_gravite #affichage de la carte des régions de france avec le nombre de tués et de blessés hospitalisés
