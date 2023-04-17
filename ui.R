#########
#### UI : APPRENTIS CHERCHEURS
#########


# Ce code programme l'interface utilisateur du programme shiny pour faire des
# analyses exporatoires dans le cadres du projet pour les apprentis chercheurs
#

### Fonctionnement du code ----

# Si dans ce programme on paramètre l'interface, on pourra être amener à en programmer
# certains bouts, comme les sorties graphiques et les outils de séléction dans la partie 
# server, en même temps que les calculs
# Dans ce cas, j'utilise RenderUI pour indiquer au server qu'on code de l'interface
# et j'appellerai ces bouts de codes grâce à uiOutput() dans ce code.


### Interface Utilisateur ----

ui = shinyUI(
  fluidPage(
    
    # Choisir le theme : exemple : https://rstudio.github.io/shinythemes/
    
    # On crée un thème personnalisé à partir du package "fresh"
    use_theme(create_theme(
      theme = "default",
      # Wellpanel et sidebar panel par défaut blanc + bordure grise
      bs_vars_wells(
        bg = "#FFF",
        border =  "#E2E2E2"
        
      ),
      # Texte des tabs
      bs_vars_global(
        link_color = "#5E6FFF" #texte pas sélectionné du tabs
      ),
      # Tabulation
      bs_vars_pills(
        border_radius = "100px", # radius de l'arrondi du coin (0% = carrée)
        active_link_hover_color = "#FFF",
        active_link_hover_bg = "#5E6FFF"
      ),
      # Police des textes
      bs_vars_font(
        size_base = "11px",
        size_h4 = "15px",
        size_h5 = "14px",
        
      )
      
    )),
    
    # sidebarPanel pour Import, Sous-population et sauvegarde
    sidebarPanel(
      # Largeur du panel
      width = 3,
      # Bordure en blanc
      style = "border: white",
      
      # Import du logo
      tags$figure(
        align = "center",
        tags$img(
          src = "robot.png",
          width = "70%",
        )
      ),
      br(),
      wellPanel(
        style = "background: #E2E2E2",
        h4("Import des données :"),
        # IMPORT DES DONNEES
        # Création de bouton CSV ou Excel, pour l'import des données
        radioButtons("datatype","Format des données à importer : ",choices = c(".csv",".xlsx"), selected=".csv",inline=TRUE),
        
        conditionalPanel(condition="input.datatype=='.csv'",
                         # Comme d'une base à l'autre le séparateur du CSV peut changer, on propose un choix avec radioButtons() :
                         radioButtons("separator","Séparateur des colonnes :",choices = c(";",",",":"), selected=";",inline=TRUE)),
        
        
        fileInput('target_upload', 'Choix de la base de donnée',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    '.csv',
                    '.xlsx'),
                  buttonLabel = "Parcourir...",
                  placeholder = "Pas de base sélectionnée"),
        # DIMENSION DE LA BASE DE DONNEES
        textOutput("info_row"),
        textOutput("info_col"),
        
        # Possibilité de trier les colonnes alphabetiquement
        checkboxInput(inputId = "col_alpha", label = "Tri colonnes alphabétique", value = FALSE)
        
      ), # Fin Wellpanel
      
      br(),
      
      
      conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                       
                       # CHOIX DE LA SOUS-POPULATION
                       wellPanel(
                         style = "background: #F4F4F4",
                         h4("Choix de la sous-population"),
                         uiOutput("selection_variables"),
                         # Choix des modalités d'intérêt
                         conditionalPanel(condition="output.afficher_choix_moda == 'Oui'",
                                          h5("Filtrer les modalités d'intérêt dans les variables choisies :"),
                                          # On appelle la boucle qui permet de faire la selection des modalités
                                          # Creée dans ServeurTableau.R
                                          uiOutput("selection_modalites")),
                         # Nombre d'individus
                         textOutput("info_row_filter")
                       ),
                       
                       br(),
                       
                       # SAUVEGARDE DES DONNEES FILTREES ET RECODE
                       wellPanel(style = "background: #CBD5E8",
                                 h4("Sauvegarde"),
                                 helpText("Télécharger les données récodées et/ou filtrées"),
                                 fluidRow(column(12, align="center", id="buttons",
                                                 downloadButton('downLoadFilter',"Télécharger"))))
      ) # Fin conditional
      
      
      
      
    ), # Fin SidebarPanel
    
    
    # AIRE CENTRALE
    mainPanel(
      width = 9,
      # Un panneau composé de plusieurs onglets
      tabsetPanel(
        id = "windows",  type = "pills",
        
        # Onglet qui affiche la base de données
        tabPanel("Base de données", value = "BDD",
                 br(),
                 
                 # Option de Zoom dans la table
                 conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                  fluidRow(
                                    column(2, offset = 10,
                                           sliderInput("zoom_tab", label = NULL, min = 50, 
                                                       max = 150, value = 80, post = "%", ticks = F)    
                                    )
                                  )),
                 # Affichage de la table générale
                 fluidRow(
                   uiOutput("view_tab")
                 )
                 
                 
        ), # Fin BDD
        
        
        # Onglet qui permet de recoder ou réordonner des variables
        tabPanel("Variables", value = "Recod_Reord",
                 br(),
                 h4("Recoder ou réordonner les modalités des variables"),
                 helpText("- Recoder une variable permet de modifier les noms de modalités et de regrouper plusieurs modalités dans une même catégorie."),
                 helpText("- Réordonner une variable permet de changer l'ordre d'affichage des modalités (pour les tables ou les graphiques)"),
                 br(),
                 # Bouton pour choisir entre recoder ou réordonner
                 radioButtons("recod_or_reord","Voulez-vous :",choices = c("Recoder","Réordonner"), selected="Recoder",inline=TRUE),
                 # Voir ServeurVariable.R
                 
                 # Si on choisit recoder :
                 conditionalPanel(condition="input.recod_or_reord == 'Recoder'",
                                  # Choix de la variable à recoder
                                  uiOutput("choix_var_recod"),
                                  
                                  
                                  
                                  tags$head(
                                    tags$style(
                                      HTML("
                                      #RecodeGO {
                                      background-color: #5E6FFF;
                                      color: white;
                                      border-color: #5E6FFF;
                                      }
                                      #RecodeGO:hover {
                                      background-color: darkblue;
                                      border-color: darkblue;
                                      }
                                           ")
                                    )
                                  ),
                                  
                                  
                                  fluidRow(column(4, align = "center", 
                                  actionButton("RecodeGO", "GO !", class = "btn-success"))),
                                  
                                  br(),
                                  # Affichage d'une table de la variable
                                  fluidRow(column(12, align = "center",
                                                  tableOutput("table_recod_avant"))),
                                  
                                  br(),
                                  # Si une variable est sélectionnée, affichage de l'interface de recodage.
                                  conditionalPanel(condition = "input.var_recod != ''",
                                                   uiOutput("recodage")
                                  ),
                                  
                                  # Affichage de la table de la nouvelle variable
                                  
                                  conditionalPanel(condition = "input.var_recod == ''",
                                                   fluidRow(column(12, align = "center",
                                                                   textOutput("texte_table_avant"))),
                                                   fluidRow(column(12, align = "center",
                                                                   uiOutput("aff_table_avant2")))),
                                  
                                  # Affichage de la table de la nouvelle variable
                                  conditionalPanel(condition = "input.var_recod == ''",
                                                   fluidRow(column(12, align = "center",
                                                                   textOutput("texte_table_apres"))),
                                  fluidRow(column(12, align = "center",
                                                  uiOutput("aff_table_apres"))))),
                 
                 # Si on choisit réordonner
                 conditionalPanel(condition="input.recod_or_reord == 'Réordonner'",
                                  # Choix de la variable à réordonner
                                  uiOutput("choix_var_reorder"),
                                  br(),
                                  # Interface pour réordonner
                                  uiOutput("reorder_ui"),
                                  # Bouton pour valider la réorganisation
                                  conditionalPanel(condition="input.var_reord != ''",
                                                   fluidRow(column(12, align = "center", 
                                                                   actionButton("ReorderOK", "Valider", class = "btn-success")))),
                                  # Affichage de la table réorganisée
                                  fluidRow(column(12, align = "center",
                                                  uiOutput("aff_table_apres_reord")))
                 ) # Fin Reorder
        ), # Fin Variable
        
        
        # Onglet d'exploration des tables
        tabPanel("Tables", value = "Tables",
                 br(),
                 # h4("Tableaux croisées"),
                 helpText("Dans cet onglet vous pouvez croiser jusqu'à trois variables."),
                 # Voir ServeurTable.R
                 
                 fluidRow(
                   # Affichage choix variables
                   column(5, wellPanel(
                     uiOutput("affichage_choix_var1"),
                     uiOutput("affichage_choix_var2"),
                     uiOutput("affichage_choix_var3"),
                     uiOutput("affichage_choix_var3_moda"))),
                   # Affichage image explicative
                   column(2, tags$figure(
                     align = "center",
                     tags$img(
                       src = "little_robot.svg",
                       width = "100%",
                     )
                   )),
                   # Affichage choix tables
                   column(5, 
                          uiOutput("affichage_choix_table_type")),
                 ),
                 br(),
                 
                 # Affichage de la table
                 fluidRow(column(12, align = "center",
                                 uiOutput("affichage_table")
                 )),
                 br(),
                 br(),
                 # Sauvegarde de la table
                 fluidRow(column(12, align="center", id="buttons",
                                 downloadButton('savetable',"Télécharger la table")))
                 
        ), # Fin Tables
        
        
        # Onglet de création des graphiques
        tabPanel("Graphiques", value = "Graphiques",
                 br(),
                 helpText("Dans cet onglet vous pouvez faire des représentations graphiques"),
                 
                 fluidRow(
                   # Choix variables
                   column(5, wellPanel(
                     # Voir ServeurGraphique.R
                     uiOutput("affichage_choix_var1_plot"),
                     uiOutput("affichage_choix_var2_plot"))),
                   
                   # Dessin explication
                   column(2, tags$figure(
                     align = "center",
                     tags$img(
                       src = "little_robot_plot.svg",
                       width = "100%",
                     )
                   )),
                   
                   # Choix graphique
                   column(5, 
                          uiOutput("affichage_choix_plot_type"))
                 ),
                 
                 fluidRow(
                   # Paramètre graphique
                   column(2,
                          br(),
                          uiOutput("param_plot")),
                   
                   column(10, align = "center",
                          br(),
                          # Choix du titre
                          uiOutput("param_titre"),
                          # Affichage Graphique
                          br(),
                          plotlyOutput("reactiv_plot"),
                          br(),
                          # Sauvegarde
                          conditionalPanel(condition = "output.afficher_plot_sauvegarde == 'Oui'",
                                           
                                           textOutput('label_sauvegarde'),
                                           tags$head(tags$style("#label_sauvegarde{
                                           color: #3175a8; 
                                           font-size: 12px;
                                           font-weight: bold ;
                                                                }")),
                                           # helpText("Sauvegarder le graphique :"),
                                           fluidRow(column(12,align = "center",
                                                           radioButtons("save_format", "", 
                                                                        choices = list(".png",".svg"), 
                                                                        selected = ".png",
                                                                        inline = T))),
                                           fluidRow(column(12,align = "center",
                                                           downloadButton('plot_save', 
                                                                          label = "Sauvegarde"))))
                          
                   )
                 )
                 
        ) # Fin Graphique
      ) # Fin tabsetpanel
    ) # Fin mainpanel
  ) # Fin fluidpage
) # FIN UI











