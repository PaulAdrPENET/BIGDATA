# Paul-Adrien 
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

summary(data)

#Il y a trois lignes ou l'année de naissance et l'age son manquant, on décide de supprimer ces lignes. Même chose dans data_brut pour une meilleur compatibilités entre data et data_brut
data_brut <- read.csv2("stat_acc_V3.csv", sep =";")
summary(data_brut)
data_brut$an_nais <- as.numeric(data_brut$an_nais)
data_brut$age <- as.numeric(data_brut$age)
data_brut <- data_brut[!(is.na(data_brut$an_nais) & is.na(data_brut$age)), ]
#data$date <- as.Date(data_brut$date, format = "%Y-%m-%d %H:%M:%S")
data$date <- as.POSIXct(data_brut$date, format = "%Y-%m-%d %H:%M:%S")
summary(data)



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

