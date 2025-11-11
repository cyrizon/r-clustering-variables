# Étape 1 : Installer le package (décommentez et exécutez si nécessaire)
# install.packages("readxl")

# Étape 2 : Charger le package
library(readxl)

# La fonction file.choose() ouvre une fenêtre pour sélectionner le fichier
jobrate <- read_excel(file.choose())

# Ensuite, vous pouvez afficher les premières lignes
head(jobrate)


# --- Définition des Variables ---
active_vars <- c("Communication_Skills", "Problem_Solving", "Learning_Ability",
                 "Judgement_under_Pressure", "Observational_Skills",
                 "Willingness_to_Confront_Problems", "Interest_in_People",
                 "Interpersonal_Sensitivity", "Desire_for_Self_Improvement",
                 "Appearance", "Dependability", "Physical_Ability", "Integrity")

illustrative_vars <- "Overall_Rating"


# --- 1. Préparation des données ---
# Variables Actives : utilisées pour former les clusters
X_active <- jobrate[, active_vars]

# Variables Illustratives : utilisées uniquement pour le rattachement (predict)
# L'argument drop=FALSE maintient le résultat sous forme de data.frame
X_illustrative <- jobrate[, illustrative_vars, drop = FALSE]


# --- 2. Initialisation et Ajustement (Fit) ---
# Nous choisissons ici k=4 comme nombre cible de clusters pour l'exemple.
message("\n--- Démarrage de la CAH (HAC) sur les variables actives (k=4) ---")

cah_model <- HACVariablesR6$new(
  k = 4,
  distance_method = "correlation",
  linkage_method = "ward.D2"
)

# Ajustement du modèle
cah_model$fit(X_active)


# --- 3. Interprétation du Modèle (summary & plot) ---
message("\n--- Résumé Détaillé du Modèle Ajusté ---")
# Montre la composition de chaque cluster
cah_model$summary()

message("\n--- Visualisation du Dendrogramme (Coupé à k=4) ---")
# Génère le graphique du dendrogramme
# (La fenêtre graphique R s'ouvrira ou le plot sera affiché dans le viewer)
cah_model$plot()


# --- 4. Prédiction (Rattachement de la variable illustrative) ---
message("\n--- Prédiction : Rattachement de la variable 'Overall_Rating' ---")

# La fonction predict va calculer la corrélation moyenne entre
# 'Overall_Rating' et les variables de chaque cluster
prediction_results <- cah_model$predict(X_illustrative)

print(prediction_results)
