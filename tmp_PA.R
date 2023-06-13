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