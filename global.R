############
#### GLOBAL
# MULLER Arno
############

### Contexte ----

# Cette application est en cours de developpement dans le cadre du projet
# Apprentis Chercheurs.
# Elle vise à faciliter l'exploration des données pour des utilisateurs.rices
# de niveau collège et lycée.


### Fonctionnalité ----

# - Import des données
# - Tris à plats
# - Tris croisées
# - Pondérations
# - Graphiques


### Structure du travail ----

# Le projet est composé de 3 scripts principaux

# - global.R : Explication de l'application  + Appel des packages + Choix de certaines options
# - ui.R     : Création de l'interface utilisateur
# - server.R : Calcules et création des tables et graphiques. 
# Pour plus de clarté, le serveur a été divisé en plusieurs scripts.

# Il est possible de regrouper ces 3 scripts dans un même programme app.R
# Shiny permet différentes organisations du travail.



### Lancer l'application ----

# Si vous êtes dans un projet contenant les différents programmes mentionnés dans la partie
# précédente alors : CLIQUER SUR LE BOUTON "Run App" EN-HAUT A DROITE.

# A l'avenir l'application sera mise soit sur le serveur de l'Ined.
# Problème : un seul utilisateur à la fois.
# Ou compilée sous forme de package R 


### Packages utilisées ----


# Shiny :
library(shiny)         # Pour Shiny
library(shinyWidgets)  # Ajout d'options notamment SelectPicker
library(shinythemes)   # Changer le theme du Shiny
library(fresh)         # Personnaliser l'interface CSS

# Import de données
#library(xlsx)          # Pour importer et écrire des données dans d'autres formats (Excel)
library(openxlsx)
# Import de CSV par la suite

# Manipulation de données :
library(dplyr)         # Fonctions de manipulation de données
library(tibble)        # Dataframe du tidyverse, on pourra l'enlever quand on fera une refonte des tables sans rownames_to_column
library(sortable)      # Pour réordonner les modalités

# Tables
library(DT)            # Afficher des tables au format HTML
library(questionr)     # Création de tables pondérées

# Graphiques
library(ggplot2)       # Graphiques du tidyverse
library(colourpicker)  # Shiny : sélection manuelle des couleurs
library(RColorBrewer)  # Création de palette de couleurs



### Options ----

# Problème d'accents
options(encoding = 'UTF-8')

# Pour importer des données plus importantes (ici 80Mo)
options(shiny.maxRequestSize = 80*1024^2)

# Pour enlever les messages de Dplyr
options(dplyr.summarise.inform = FALSE)


### Limite : ----

# on ne peut recoder ou réordonner que les variables dans la base en entrée.



### Idées de développement ----

# - Graphique interactif (library GGiraph)
# - Sauvegarde avec rapport automatisés
# - 





