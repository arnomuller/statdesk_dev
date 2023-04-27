

# Installation en local ----

# Liste des dépendances
.packages = c("shiny", "shinyWidgets", "shinythemes", "fresh", "openxlsx", "dplyr", "sortable",
              "tidyr", "DT", "ggplot2", "colourpicker", "RColorBrewer", "plotly")
# Installation des manquantes
.new.packages <- .packages[!(.packages %in% installed.packages()[,"Package"])]
if(length(.new.packages)) install.packages(.new.packages)

# Installer statdesk
install.packages("./statdesk_0.1.0.tar.gz", repos = NULL, type = "source")
# Lancer statdesk
library(statdesk)
statdesk()


# Installation depuis Github ----

install.packages("devtools")
library("devtools")
install_github("arnomuller/statdesk")







