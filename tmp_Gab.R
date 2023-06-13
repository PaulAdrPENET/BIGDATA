# Gabriel

# Import des fichiers csv
data <- read.csv("MyFile.csv", sep=";")
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


