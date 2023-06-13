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




# Visualisation

# Nombre d’accidents en fonction des conditions athmosphériques

conditions_athmospheriques <- subset(data,select =descr_athmo)
conditions_athmospheriques <- table(conditions_athmospheriques)
plot(conditions_athmospheriques,xlab="conditions atmosphériques",ylab="nombre d'accidents")

# Nombre d’accidents en fonction de la description de la surface
description_surface <- subset(data,select =descr_etat_surf)