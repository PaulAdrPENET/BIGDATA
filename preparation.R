#Preparation PAUL-ADRIEN

data <- read.csv2("stat_acc_V3.csv", sep =";")
summary(data)
#Modification du type des colonnes chaîne de caractère vers numérique.

data$Num_Acc <- as.numeric(data$Num_Acc)
options(scipen = 999)
summary(data)
data$id_usa <- as.numeric(data$id_usa)
data$an_nais <- as.numeric(data$an_nais)
data$age <- as.numeric(data$age)
data$place <- as.numeric(data$place)
summary(data)

data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
summary(data)
ville = c("PARIS 01", "PARIS 02", "PARIS 03", "PARIS 04", "PARIS 05", 
          "PARIS 06", "PARIS 07", "PARIS 08", "PARIS 09", "PARIS 10", 
          "PARIS 11", "PARIS 12", "PARIS 13", "PARIS 14", "PARIS 15", 
          "PARIS 16", "PARIS 17", "PARIS 18", "PARIS 19", "PARIS 20",
          "MARSEILLE 01", "MARSEILLE 02", "MARSEILLE 03", "MARSEILLE 04", 
          "MARSEILLE 05", "MARSEILLE 06", "MARSEILLE 07", "MARSEILLE 08", 
          "MARSEILLE 09", "MARSEILLE 10", "MARSEILLE 11", "MARSEILLE 12", 
          "MARSEILLE 13", "MARSEILLE 14", "MARSEILLE 15", "MARSEILLE 16",
          "LYON 01", "LYON 02", "LYON 03", "LYON 04", "LYON 05", 
          "LYON 06", "LYON 07", "LYON 08", "LYON 09")
latitude = c(48.8592, 48.8655, 48.8637, 48.8541, 48.8449, 48.8484, 48.8562, 
             48.8727, 48.8760, 48.8761, 48.8584, 48.8352, 48.8286, 48.8296, 
             48.8408, 48.8604, 48.8872, 48.8925, 48.8822, 48.8626,
             43.2964, 43.3017, 43.3023, 43.3023, 43.2978, 43.2903, 43.2856, 
             43.2679, 43.2502, 43.3071, 43.2903, 43.3066, 43.3363, 43.3649, 
             43.3836, 43.3699,
             45.7640, 45.7577, 45.7586, 45.7640, 45.7621, 45.7696, 
             45.7486, 45.7292, 45.7705)
longitude = c(2.3417, 2.3430, 2.3615, 2.3549, 2.3486, 2.3324, 2.3122, 2.3180, 
              2.3394, 2.3488, 2.3787, 2.4219, 2.3627, 2.3273, 2.2936, 2.2621, 
              2.3075, 2.3280, 2.3488, 2.3995,
              5.3690, 5.3712, 5.3662, 5.3690, 5.3746, 5.3734, 5.3491, 5.3729, 
              5.3841, 5.3882, 5.3984, 5.4061, 5.4291, 5.4009, 5.3580, 5.3422,
              4.8357, 4.8322, 4.8320, 4.8278, 4.8294, 4.8353, 4.8514, 4.8574, 4.8058)

for (i in 1:nrow(data)) {
  if (data[i, "ville"] %in% ville) {
    index <- match(data[i, "ville"], ville)
    data[i, "latitude"] <- latitude[index]
    data[i, "longitude"] <- longitude[index]
  } else {
    data[i, "latitude"] <- data[i, "latitude"]
    data[i, "longitude"] <- data[i, "longitude"]
    # On garde les autres valeurs
  }
}

print(data)
summary(data)
print(length(data$latitude))
print(length(data$longitude))
#Toutes les valeurs sont présentes.


#Conversion des dates sous formes de chaîne de caractère en date
data$date <- as.Date(data$date, format = "%Y-%m-%d %H:%M:%S")
summary(data)
#Vérification que toutes les dates sont biens comprises entre le 1er janvier 2009 et le 31 decembre 2009
date_min <- min(data$date)
date_max <- max(data$date)
print(date_min)
print(date_max)

na_rows <- data[!complete.cases(data$place), ]
na_rows_subset <- head(na_rows, 20)
print(na_rows_subset) 
#Nous remarquons qu'il y a de nombreuse valeurs manquante NA notamment dans le nombre de place environ 3500 valeurs manquantes, on remplace par un 1 car il y a au moins une personne dans la voiture.
data$place[is.na(data$place)] <- 1
summary(data)
#Il y a trois lignes ou l'année de naissance et l'age son manquant, on décide de supprimer ces lignes.
missing_rows <- data[is.na(data$an_nais) | is.na(data$age), ]
print(missing_rows)
data <- data[!(is.na(data$an_nais) & is.na(data$age)), ]
summary(data)


#Construction des série chronologiques sur l'évolution du nombre d'accidents par mois et par semaines.
# Aggrégation par mois :
# Créer une nouvelle colonne pour les mois
data$month <- format(data$date, "%m") 
data_monthly <- aggregate(Num_Acc ~ month, data, FUN = length)

# Aggrégation par semaine : 
# Créer une nouvelle colonne pour les semaines
data$week <- format(data$date, "%U") 
data_weekly <- aggregate(Num_Acc ~ week, data, FUN = length)

#Il y a trois lignes ou l'année de naissance et l'age son manquant, on décide de supprimer ces lignes. Même chose dans data_brut pour une meilleur compatibilités entre data et data_brut
data_brut <- read.csv2("stat_acc_V3.csv", sep =";")
summary(data_brut)
data_brut$an_nais <- as.numeric(data_brut$an_nais)
data_brut$age <- as.numeric(data_brut$age)
data_brut <- data_brut[!(is.na(data_brut$an_nais) & is.na(data_brut$age)), ]
#data$date <- as.Date(data_brut$date, format = "%Y-%m-%d %H:%M:%S")
data$date <- as.POSIXct(data_brut$date, format = "%Y-%m-%d %H:%M:%S")
summary(data)

# Préparation Gabriel 

pop <- read.csv("data_pop.csv", sep=";")

# Modification de la colonne descr_motif_traj
data$descr_motif_traj <- factor(data$descr_motif_traj)
data$descr_motif_traj <- as.numeric(data$descr_motif_traj)
print(table(data$descr_motif_traj))
# 1 = Autres
# 2 = Courses - achats
# 3 = Domicile - école
# 4 = Domicile - travail
# 5 = Promenade - loisirs
# 6 = Utilisation professionnelle

# Modification de la colonne descr_dispo_secu 
# print(table(data$descr_dispo_secu))
data$descr_dispo_secu <- factor(data$descr_dispo_secu)
data$descr_dispo_secu <- as.numeric(data$descr_dispo_secu)
print(table(data$descr_dispo_secu))
# 1 = Autre - Non déterminable  
# 2 = Autre - Non utilisé 
# 3 = Autre - Utilisé 
# 4 = Présence d un casque - Utilisation non déterminable
# 5 = Présence d un casque non utilisé
# 6 = Présence d un dispositif enfant non utilisé
# 7 = Présence d un équipement réfléchissant non utilisé
# 8 = Présence d une ceinture de sécurité - Utilisation non déterminable
# 9 = Présence de ceinture de sécurité non utilisée
# 10 = Présence dispositif enfant - Utilisation non déterminable
# 11 = Présence équipement réfléchissant - Utilisation non déterminable
# 12 = Utilisation d un casque
# 13 = Utilisation d un dispositif enfant
# 14 = Utilisation d un équipement réfléchissant
# 15 = Utilisation d une ceinture de sécurité

# Modification de la colonne descr_dispo_secu 
# print(table(data$descr_athmo))
data$descr_athmo <- factor(data$descr_athmo)
data$descr_athmo <- as.numeric(data$descr_athmo)
print(table(data$descr_athmo))
# 1 = Autre
# 2 = Brouillard – fumée 
# 3 = Neige – grêle 
# 4 = Normale 
# 5 = Pluie forte 
# 6 = Pluie légère
# 7 = Temps couvert
# 8 = Temps éblouissant 
# 9 = Vent fort – tempête 

# Construcion du jeu de données
#print(table(data$ville))

# Préparation Hugo : 


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


# .