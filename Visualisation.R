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


