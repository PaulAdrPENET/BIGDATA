# ANALYSE 

# GABRIEL
couleurs <- c("#ADD8E6", "#FFFFE0", "#98FB98", "#FFC0CB")

# Liste des variables qualitatives
list_var_quali <- list(data$descr_cat_veh, data$descr_agglo,
                       data$descr_athmo, data$descr_lum,
                       data$descr_etat_surf, data$description_intersection,
                       data$descr_dispo_secu, data$descr_grav,
                       data$descr_motif_traj, data$descr_type_col)
# Liste des noms des variables
name_tbl <- list("cat_veh", "descr_agglo", "descr_athmo",
                 "descr_lum", "etat_surf", "descr_intersection",
                 "dispo_secu", "descr_grav", "motif_traj",
                 "type_col")
# Réalisation des tableaux croisées, des tests de chi2 et des affichages
# Utilisation d'une double boucle pour couvir l'ensemble des combinaisons
for (idx1 in 1:(length(list_var_quali)-1)) {
  for (idx2 in (idx1+1):(length(list_var_quali))) {
    # Création du tableau croisé
    tmp_tbl <- table(list_var_quali[[idx1]], list_var_quali[[idx2]])
    # Affichage du résultat du test chi2
    print(chisq.test(tmp_tbl))
    # Affichage du tableau
    mosaicplot(tmp_tbl, 
               main = paste(name_tbl[[idx1]], "&", name_tbl[[idx2]]),
               color = couleurs,
               xlab = name_tbl[[idx1]],
               ylab = name_tbl[[idx2]])
  }
}
# PAUL-ADRIEN

#Analyse en régression linéaire : 
# Régression par mois :
data_monthly$month <- as.numeric(data_monthly$month)
regression_mois <- lm(Num_Acc~ month, data = data_monthly)
summary(regression_mois)



plot(data_monthly$month,
     data_monthly$Num_Acc,
     xlim = c(1, 12),
     ylim = c(3526, 8000),
     xlab='mois',
     ylab="Nombre d'accidents",)

abline(regression_mois, col = 'blue')
title("Droite de régression par mois")

# Régression par semaine :
data_weekly$week <- as.numeric(data_weekly$week)
regression_semaine <- lm(Num_Acc ~ week, data = data_weekly)
summary(regression_semaine)

plot(data_weekly$week,
     data_weekly$Num_Acc,
     xlim = c(1, 53),
     ylim = c(200, 2000),
     xlab = "Semaine",
     ylab="Nombre d'accidents")
abline(regression_semaine, col = 'blue')
title("Droite de régression par semaine")
