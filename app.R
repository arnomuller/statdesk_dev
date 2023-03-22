


# Problème d'accents
options(encoding = 'UTF-8')

# Pour importer des données plus importantes (ici 80Mo)
options(shiny.maxRequestSize = 80*1024^2)

# Pour enlever les messages de Dplyr
options(dplyr.summarise.inform = FALSE)


# Shiny :
library(shiny)         # Pour Shiny
library(shinyWidgets)  # Ajout d'options notamment SelectPicker
library(shinythemes)   # Changer le theme du Shiny
library(fresh)         # Personnaliser l'interface CSS

# Import de données
library(openxlsx)     # Pour importer et écrire des données dans d'autres formats (Excel)


# Manipulation de données :
library(dplyr)         # Fonctions de manipulation de données
library(sortable)      # Pour réordonner les modalités
library(tidyr)         # NEW POUR TABLE PIVOT

# Tables
library(DT)            # Afficher des tables au format HTML

# Graphiques
library(ggplot2)       # Graphiques du tidyverse
library(colourpicker)  # Shiny : sélection manuelle des couleurs
library(RColorBrewer)  # Création de palette de couleurs


shinyApp(
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
          size_h4 = "15px"
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
                                    fluidRow(column(12, align = "center",
                                                    uiOutput("aff_table_apres")))),
                   
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
                            # Choix du titre
                            uiOutput("param_titre"),
                            # Affichage Graphique
                            br(),
                            plotOutput("reactiv_plot"),
                            br(),
                            # Sauvegarde
                            conditionalPanel(condition = "output.afficher_plot_sauvegarde == 'Oui'",
                                             helpText("Sauvegarder le graphique :"),
                                             fluidRow(column(12,align = "center",
                                                             downloadButton('pngsave', 
                                                                            label = "Sauvegarde"))))
                            
                     )
                   )
                   
          ) # Fin Graphique
        ) # Fin tabsetpanel
      ) # Fin mainpanel
    ) # Fin fluidpage
  ), # Fin UI
  
  server = shinyServer(
    function(input, output, session) {
      
      
      
      # Import des données ----
      
      data <- reactive({
        inFile <<- input$target_upload
        if (is.null(inFile)) {
          return(NULL)
        }
        
        if (input$datatype == ".xlsx"){
          
          # Si ce qu'on importe n'est pas un xlsx, il ne se passe rien
          if (substr(inFile$datapath, nchar(inFile$datapath)-4, nchar(inFile$datapath)) == ".xlsx") {
            
            df <- openxlsx::read.xlsx(inFile$datapath,1)
            # On remplace les virgules par des points pour en faire des variables numeriques
            df <- data.frame(lapply(df, function(x) {gsub(",", ".", x)}))
            nomcol_data <<- colnames(df)
            
            return(df)
          } else {
            return(NULL)
          }
        } else if (input$datatype == ".csv"){ # Même processus pour les fichiers csv.
          if (substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == ".csv") {
            
            df <- read.csv(inFile$datapath, header = TRUE, sep = input$separator)
            df <- data.frame(lapply(df, function(x) {gsub(",", ".", x)}))
            nomcol_data <<- colnames(df)
            
            return(df)
          } else {
            return(NULL)
          }
        }
      }) # Fin Import
      
      
      # Warning si pas le bon format de données
      observeEvent(input$target_upload, {
        if(is.null(data()) == T ){
          
          showModal(modalDialog(
            title = "Attention",
            "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
            easyClose = TRUE,
            footer = NULL))
          
        }
      })
      
      
      # On crée un objet "reactiveValues" qui est une sorte d'objet reactif, qui va pouvoir
      # contenir d'autres objets comme des dataframes.
      # L'avantage c'est qu'on peut le modifier dans différents observeEvent
      # au contraire du simple reactive, qui ne peut être crée et modifié que dans un
      # unique bloc de code.
      
      v <- reactiveValues(data = NULL)
      
      # 1ère modification : quand on importe les données v$data prend la valeur des
      # des données.
      
      
      # L'objet contenant les données se modifie quand : 
      # On importe les données
      observeEvent(input$target_upload, {
        v$data <<- data()
      })
      
      # On change le séparateur pour les csv
      observeEvent(input$separator, {
        validate(need(input$target_upload, 'Importer des données'))
        v$data <<- data()
      })
      
      # On change le type de données avec un message d'erreur si ce n'est pas le bon
      observeEvent(input$datatype, {
        validate(need(input$target_upload, 'Importer des données'))
        v$data <<- data()
        if(is.null(data()) == T ){
          
          showModal(modalDialog(
            title = "Attention",
            "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
            easyClose = TRUE,
            footer = NULL))
          
        }
      })
      
      
      # Changement de l'ordre des colonnes alphabetiquement
      observeEvent(input$col_alpha, {
        validate(need(input$target_upload, 'Importer des données'))
        
        if(ncol(data()) >1 ){ # if Inutile mais fait pas de mal
          if (input$col_alpha == TRUE) {
            nomcol_data <<- order(colnames(data()))
            v$data <<- data() %>%
              select(order(colnames(data())))
            
          } else{
            nomcol_data <<- colnames(data())
            v$data <<- data()
          }
        }
        
      })
      
      
      # On sauvegarde des objets réactifs qui renvoie les noms de variables.
      # Pour la base importée (= nomcol_data)
      nomcol_data_start <- reactive({
        nomcol_data
      })
      
      # Idem pour la base qui sera modifiée (utilisé dans les pickers)
      nomcol_data_reac <- reactive({
        colnames(v$data)
      })
      
      
      # Dimension de la table en entrée      
      n_col_start <- reactive({ ncol(data()) })
      n_row_start <- reactive({ nrow(data()) })
      
      # Affichage dimension table
      output$info_row <- renderPrint({
        validate(need(data(), ""))
        cat("Nombre d'individus :", isolate(n_row_start()))
      })
      output$info_col <- renderPrint({
        validate(need(data(), ""))
        cat("Nombre de variables :", isolate(n_col_start()))
      })
      
      
      
      
      # SUITE DU SERVER ----     
      
      # Pour la suite, on continue le server dans des scripts différents pour chacun des onglets.
      
      
      # 1) Variable ----  
      # Permet de modifier les variables
      ################
      #### Onglet Recodage
      ################
      
      # C'est la première étape après l'import de données.
      # On va pouvoir modifier des variables en les recodant ou en les réordonnant.
      
      
      ###########################
      #### CREATION DE L'INTERFACE        ----
      ### POUR RECODER : ----
      
      # Choix de la variable à importer
      output$choix_var_recod <- renderUI({
        
        validate(need(input$target_upload, 'Importer des données'))
        
        fluidRow(pickerInput("var_recod", "Choix de la variable à recoder :", c("",nomcol_data_start()),
                             multiple = F,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable à recoder")))
      })
      
      # Table de cette variable
      output$table_recod_avant <- renderTable({
        validate(need(input$var_recod,''))
        
        hop <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(input$var_recod), useNA = "always")))))) %>% 
          slice(2)
        colnames(hop) <- c(with(v$data, names(table(get(input$var_recod)))), "Non Réponse", "Total")
        hop
        
      })
      
      
      # Cadre avec le nom de la variable à recoder
      output$nom_var_recod_avant <- renderText(input$var_recod)
      
      
      # Pour chaque modalité de la variable a recoder, on créer un cadre de texte
      # avec le nom de la modalité
      observeEvent(input$var_recod, {
        lapply((1:length(unique(with(data(), get(var_recod_nom()))))), function(i) {
          outputId <- paste0("OUT", i)
          output[[outputId]] <- renderText(levels(with(data(), as.factor(get(var_recod_nom()))))[i])
          
        })
      })
      
      
      # Ancien nom de la variable provient d'un input
      # C'est pas super utile, on pourrait se contenter de input$var_recod, mais 
      # ça posait quelques problèmes plus loin
      var_recod_nom <- reactive({
        validate(need(input$var_recod,''))
        input$var_recod
      })
      
      
      # Nouveau nom que l'on retiendra pour nommer la nouvelle variable dans la base
      var_recod_nom_apres <- reactive({
        # Si l'utilisateur n'écrit pas un nom de variable,
        # on ajoute par defaut le suffixe _recode, sinon choix utilisateur 
        if (input$nom_var_recod_apres == "") {
          paste0(var_recod_nom(), "_recode") # Pas utile
        } else {
          input$nom_var_recod_apres
        }
      })
      
      
      ## CREATION DES CASES AVEC LES MODALITES
      
      # Quand on choisit une variable à recoder, ça ouvre l'interface suivante :
      observeEvent(input$var_recod, {
        output$recodage <- renderUI({
          validate(need(input$target_upload,''))
          validate(need(input$var_recod, 'Choisir une variable'))
          # Par ligne :
          
          # Titre des colonnes
          wellPanel(style = "background: #FBFBFB",
                    fluidRow(
                      column(4, offset = 1,
                             h4("Variable à recoder :")),
                      column(4,offset = 2,
                             h4("Nouveau nom de la variable :"))
                    ),
                    
                    # les noms de variables :
                    fluidRow(
                      column(4, align = "center",  offset = 1,
                             verbatimTextOutput("nom_var_recod_avant")),
                      column(2, align = "center",
                             icon("arrow-right", class = "fa-3x", lib = "font-awesome")),
                      column(4,
                             textInput("nom_var_recod_apres", NULL, placeholder = paste0(var_recod_nom(),"_recode")))
                    ),
                    br(),
                    br(),
                    
                    # Les titres pour les modalités
                    fluidRow(
                      column(4,  offset = 1,
                             h4("Modalité à recoder :")),
                      column(4,offset = 2,
                             h4(paste0("Nouvelles modalités de la variable :")))
                    ),
                    
                    # Les cases de recodages :
                    lapply((1:length(unique(with(data(), get(var_recod_nom()))))), function(i){
                      
                      inputId <- paste0("input_", i)
                      fluidRow(column(4,  offset = 1,
                                      verbatimTextOutput(outputId=paste0("OUT",i))),
                               column(2, align = "center",
                                      icon("arrow-right", class = "fa-3x", lib = "font-awesome")),
                               column(4,
                                      textInput(paste0("input_", i), NULL, width = 500, placeholder = levels(as.factor(with(data(), get(var_recod_nom()))))[i])))
                      #textInput(paste0("input_", i), NULL, width = 500, placeholder = "Même modalité")))
                      
                    }), # FIN cases
                    
                    # Affichage du bouton de validation
                    fluidRow(column(4, offset = 4 , align = "center",
                                    actionButton("RecodeOK", "Valider", class = "btn-success")))
          ) # FIN wellpanel 
        }) # FIN UI
      }) # FIN observeEvent
      
      
      
      
      ### POUR REORDER : ----
      
      ## Choix de la variable à réordonner
      output$choix_var_reorder<- renderUI({
        
        validate(need(input$target_upload, 'Importer des données'))
        # Pour le moment, on ne peut reordonner que les variables en entrée 
        # (techniquement possible avec des variables recodées, mais ça rend pas bien)
        fluidRow(pickerInput("var_reord", "Choix de la variable à réordonner :", c("",nomcol_data_start()),
                             multiple = F,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable à réordonner")))
      })
      
      
      ## Interface de reorder : 
      # avec library(sortable)
      output$reorder_ui <- renderUI({
        
        validate(need(input$var_reord, 'Choisir une variable'))
        
        sortable::rank_list(
          text = "Cliquer et glisser les modalités dans l'ordre voulu",
          labels = with(v$data, levels(as.factor(get(input$var_reord)))),
          input_id = "rank_list_basic"
        )
        
        
        
      })
      
      ###########################
      
      
      ###########################
      #### RECODAGE                       ----
      
      
      # Quand on appuye sur le bouton recodage
      observeEvent(input$RecodeOK, {
        
        # On crée une nouvelle variable basée sur l'input
        recod_data <<- v$data  %>% 
          mutate(newvar = as.character(get(input$var_recod)))
        
        
        # Pour chaque modalité, si l'utilisateur écrit dans l'emplacement de texte
        # newvar prendra cette valeur.
        
        # Si la variable contient des NAs :
        if (anyNA(with(recod_data, get(input$var_recod))) == T) {
          
          # Pour chaque modalité de la variable (-1 pour les NA)
          # On donne la valeur dans la case recodage si l'utilisateur à écrit dedans,
          # sinon on garde la valeur précédente.
          for (i in c(1: (length(unique(with(recod_data, as.factor(get(input$var_recod)))))-1))) {
            recod_data <- recod_data %>% 
              mutate(newvar = ifelse(is.na(get(input$var_recod)) == T, NA,
                                     ifelse(get(input$var_recod) != levels(with(data(), as.factor(get(input$var_recod))))[i], newvar,
                                            ifelse(input[[paste0("input_", i)]] != '' & input[[paste0("input_", i)]] != "NA" ,
                                                   input[[paste0("input_", i)]],
                                                   ifelse(input[[paste0("input_", i)]] == "NA", NA,
                                                          levels(with(data(), as.factor(get(input$var_recod))))[i])))))
          }
          # POUR LES NAs :
          recod_data <- recod_data %>% 
            mutate(newvar = ifelse(is.na(get(input$var_recod)) == F, newvar,
                                   ifelse(input[[paste0("input_", length(unique(with(data(), get(input$var_recod)))))]] != '',
                                          input[[paste0("input_", length(unique(with(data(), get(input$var_recod)))))]], NA)))
          
          
        } else { # Si pas de NA dans la variable : 
          
          for (i in c(1: length(unique(with(data(), get(input$var_recod)))))) {
            recod_data[ with(recod_data, get(input$var_recod)) ==
                          levels(with(recod_data, as.factor(get(input$var_recod))))[i],]$newvar <-  ifelse(input[[paste0("input_", i)]] != '' & input[[paste0("input_", i)]] != "NA" ,
                                                                                                           input[[paste0("input_", i)]],
                                                                                                           ifelse(input[[paste0("input_", i)]] == "NA", NA,
                                                                                                                  levels(with(data(), as.factor(get(input$var_recod))))[i]))
          } # Fin for
        } # Fin else
        
        
        
        # On renomme la variable newvar avec le choix du nouveau nom
        colnames(recod_data) <- c(colnames(recod_data)[1:(ncol(recod_data)-1)], var_recod_nom_apres())
        
        # Si le nom existe déjà, la variable est automatiquement renommée avec le suffixe _new
        if (sum(duplicated(colnames(recod_data))) > 0) {
          showModal(modalDialog(
            title = "ATTENTION : Nom de variable existant",
            "Ce nom de variable est déjà utilisée, la variable a été recodée avec le suffixe '_new'.",
            easyClose = TRUE,
            footer = NULL))
          colnames(recod_data)[which(duplicated(colnames(recod_data)))] <- paste0(colnames(recod_data)[which(duplicated(colnames(recod_data)))],"_new")
          
        }
        
        
        # On sauvegarde la base recodée et réécriture de l'ancienne
        v$data <- recod_data
        
        # On affiche une table de la variable recodée
        output$table_recod_apres <- renderTable({
          tab <- as.data.frame(t(as.data.frame(with(recod_data, addmargins(table(get(var_recod_nom_apres()), useNA = "always")))))) %>%
            slice(2)
          
          colnames(tab) <- c(with(recod_data, names(table(get(var_recod_nom_apres())))), "Non Réponse", "Total")
          tab
        })
        # Affichage de la table recodée
        output$aff_table_apres <- renderUI({
          tableOutput("table_recod_apres")
        })
        
        
      }) # Fin Recodage 
      
      
      #### REORDONNER                     ----
      
      # Quand on appuie sur le bouton :
      observeEvent(input$ReorderOK, {
        
        # On sauvegarde le nouvelle ordre des modalités
        # (pas forcément utile)
        new_order <<- input$rank_list_basic
        
        # Pas forcément utile
        reord_data <- v$data
        # Changement de l'ordre de la variable
        reord_data[,input$var_reord] <- with(reord_data, factor(get(input$var_reord), levels = input$rank_list_basic))
        # Sauvegarde de la nouvelle base et réécriture de l'ancienne
        v$data <- reord_data
        
        # On affiche une table de la variable réordonnée
        output$table_reord_apres <- renderTable({
          tab_reord <- as.data.frame(t(as.data.frame(with(reord_data, addmargins(table(get(input$var_reord), useNA = "always")))))) %>% 
            slice(2)
          colnames(tab_reord) <- c(with(reord_data, names(table(get(input$var_reord)))), "Non Réponse", "Total")
          tab_reord
        })
        output$aff_table_apres_reord <- renderUI({
          tableOutput("table_reord_apres")
        })
        
      }) # Fin reordonne
      
      
      
      ###########################
      
      
      
      
      # 2) Sous Population ----  
      # Permet de faire une sous-population
      ##################
      #### Onglet sous-population ----
      ##################
      
      
      
      ### Condition pour affichage des filtres ----
      
      # (dans UI)
      # Le filtre se fait sur la base recodée dans ServeurVariable.R
      output$afficher_choix_souspop <- reactive({
        if (is.null(v$data) == FALSE) {
          "Oui"
        }
      })
      outputOptions(output, "afficher_choix_souspop", suspendWhenHidden=FALSE)
      
      
      
      ### Filtre des modalités                 ----
      
      # Création de PickerInput dynamique, selon le nombre de variables de sous-population
      observeEvent(input$target_upload, {
        
        output$selection_variables <- renderUI({
          v$data
          tagList(
            fluidRow(
              column(12, selectizeInput('var_souspop',
                                        label=paste0("Sélection des variables de sous-populations"),
                                        # Choix parmis les noms de variables de data
                                        choices=c("",colnames(v$data)),
                                        # Plusieurs options : 
                                        options = list(`actions-box` = TRUE, placeholder = 'Quelles variables pour sous-population ?'), 
                                        multiple = TRUE # Si TRUE alors on peut choisir plusieurs variables.
              )))
          )})
        
        
      })
      
      
      # Condition affichage des boites de sélection des modalités (dans UI)
      output$afficher_choix_moda <- reactive({
        if (length(input$var_souspop) >= 1) { "Oui" }
      })
      outputOptions(output, "afficher_choix_moda", suspendWhenHidden=FALSE)
      
      
      
      # Je crée des boites qui renvoie une liste des modalités pour chaque variables séléctionnées,
      # l'utilisateur peut donc choisir celles qui l'intéresse
      output$selection_modalites <- renderUI({
        tagList(
          if(length(input$var_souspop) >= 1) { # si une variable sélectionné
            lapply((1:length(input$var_souspop)), function(i){
              pickerInput(inputId= input$var_souspop[i], # On donne comme signal le nom de la variable dans vect_var
                          label=paste0("Choix des filtres pour '",input$var_souspop[i], "'"),
                          # choices=levels(v$data[,which(nomcol_data == vect_var[i])]),
                          choices=levels(as.factor(as.character(v$data[,which(colnames(v$data) == input$var_souspop[i])]))),
                          options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Pas de filtre"),
                          multiple = TRUE)
            })
          }
        )}) # Fin renderUI
      
      
      
      ### Création des donnees filtrees        ----
      
      
      filter_data <- reactive({
        validate(need(input$target_upload,"Importer des données"))
        
        data_conditionnel <- v$data
        
        if(length(input$var_souspop) >= 1) {
          for (i in c(1:length(input$var_souspop))) {
            if (is.null(input[[input$var_souspop[i]]]) == FALSE) {
              data_conditionnel <- data_conditionnel[data_conditionnel[[input$var_souspop[i]]] %in% input[[input$var_souspop[i]]],]
              
            }
          }
        } 
        # S'il n'y a pas de sélection, on ne fait rien
        if (is.null(input$var_souspop)== T) { data_conditionnel <- data_conditionnel }
        
        filter_data2 <<- data_conditionnel 
        
        return(data_conditionnel)
      })
      
      
      # Creation de l'objet dynamique qui contient les noms des variables
      nomcol_data2 <- reactive({ colnames(filter_data()) })
      
      # Nombre d'individus dans les données filtrées
      n_row_filter <- reactive({ nrow(filter_data()) })
      
      output$info_row_filter <- renderPrint({
        validate(need(filter_data(), ""))
        cat("Nombre d'individus après la sélection :", isolate(n_row_filter()))
      })
      
      
      
      ### Affichage de la table filtrees       ----
      
      output$table <- renderDataTable({ 
        
        DT::datatable(filter_data(), extensions = 'Scroller', rownames = F, options = list(deferRender = F, 
                                                                                           dom = 't',
                                                                                           # columnDefs = list(list(className = 'dt-center',
                                                                                           #                        targets = 5)),
                                                                                           scrollY = 500,  #Hauteur de la table en pixel
                                                                                           scroller = TRUE, 
                                                                                           scrollX = T,
                                                                                           pageLength = 5))
        
      })
      
      
      # Affichage table avec option du ZOOM
      output$view_tab <- renderUI({
        div(dataTableOutput("table"), style = paste0("font-size: ",input$zoom_tab,"%"))
      })
      
      
      
      ### Sauvegarde de la table               ----
      
      output$downLoadFilter <- downloadHandler(
        filename = function() {
          paste('Nouvelle_table_', Sys.Date(), '.xlsx', sep = '')
        },
        content = function(file){
          openxlsx::write.xlsx(filter_data2,file)
        }
      )
      
      
      
      
      
      # 3) Table ----  
      # Permet d'observer les variables dans des tables
      ###########
      #### Onglet Tables
      ###########
      
      
      ### GESTION DE L'UI            ----
      
      
      
      # Condition d'affichage des pickers de variables à mettre dans les tables
      output$affichage_choix_var1 <- renderUI({
        
        validate(need(input$target_upload, 'Importer des données'))
        
        fluidRow(pickerInput("var_table1", "Variable 1 :", c("",nomcol_data_reac()),
                             multiple = F,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en ligne")))
      })
      
      output$affichage_choix_var2 <- renderUI({
        validate(need(input$target_upload, ''))
        fluidRow(
          conditionalPanel(condition="input.var_table1 != ''", 
                           pickerInput("var_table2", "Variable 2 :", c("",nomcol_data_reac()),
                                       multiple = F,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en colonne"))))
      })
      
      output$affichage_choix_var3 <- renderUI({
        
        validate(need(input$target_upload, ''))
        fluidRow(
          conditionalPanel(condition="input.var_table2 != ''", 
                           pickerInput("var_table3", "Variable 3 / modalités :", c("",nomcol_data_reac()),
                                       multiple = F,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Diviser les tables selon :"))))
      }) 
      
      output$affichage_choix_var3_moda <- renderUI({ 
        validate(need(input$target_upload, ''))
        fluidRow(
          # Choix de la moda de var3
          conditionalPanel(condition="input.var_table3 != ''", 
                           selectizeInput('var_table3_moda',
                                          label=NULL,
                                          # Choix parmis les noms de variables de data
                                          choices= levels(as.factor(as.character(filter_data()[,which(colnames(filter_data()) == input$var_table3)]))) ,
                                          # Plusieurs options : 
                                          options = list(`actions-box` = TRUE), 
                                          multiple = FALSE, 
                                          width = 450))
        )
      })      
      
      
      
      # Condition d'affichage du choix de variable de pondération
      output$afficher_tri_ponder <- reactive({
        validate(need(input$checkbox_ponder,""))
        if (input$checkbox_ponder != FALSE) {
          "Oui"
        }
      })
      outputOptions(output, "afficher_tri_ponder", suspendWhenHidden=FALSE)
      
      
      # Bandeau du choix de la table a afficher  
      output$affichage_choix_table_type <- renderUI({ 
        validate(need(input$target_upload, ''))
        wellPanel(
          fluidRow(
            selectInput(inputId="choix_table",
                        label="Choix du type de table : ",
                        choices= c("Effectifs univariés" = "eff_uni", 
                                   "Effectif bivariés" = "eff", 
                                   "% Ligne" = "pct_lign", 
                                   "% Colonne" = "pct_col"))),
          fluidRow(
            # Case a cocher pour pondération
            checkboxInput(inputId = "checkbox_ponder", label = "Utiliser une pondération ?", value = FALSE, width = NULL)),
          fluidRow(
            # Choix de la variable de pondération
            conditionalPanel(condition="output.afficher_tri_ponder == 'Oui'", 
                             selectizeInput('var_ponder_tri',
                                            label=NULL,
                                            # Choix parmis les noms de variables de data
                                            choices=c("",nomcol_data_reac()),
                                            # Plusieurs options : 
                                            options = list(`actions-box` = TRUE, placeholder = 'Pas de pondération'), 
                                            multiple = FALSE, 
                                            width = 450))
          ),
          fluidRow(
            checkboxInput(inputId = "checkbox_na", label = "Afficher les valeurs manquantes ?", value = TRUE, width = NULL) 
          )
        )
        
        
      })
      
      
      
      
      
      ### CREATION DES TABLES        ----
      
      table_to_save <- reactive({
        validate(need(input$target_upload, ''))
        validate(need(input$var_table1, 'Choisir une 1ère variable'))
        
        if (input$choix_table == "eff_uni"
        ){
          # SANS PONDERATION
          if (input$checkbox_ponder == FALSE) {
            # AVEC NA
            if (input$checkbox_na == TRUE) {
              
              # Sans Ponder avec NA
              as.data.frame(filter_data() %>% 
                              group_by(get(input$var_table1)) %>% 
                              summarise(Effectif = n()) %>% 
                              rename(Variable = 1) %>% 
                              mutate(Variable = as.character(Variable)) %>% 
                              mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
                              mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3)) %>% 
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))
              
            }else{ # ELSE SANS NA
              
              # Sans Ponder sans NA
              as.data.frame(filter_data() %>% 
                              group_by(get(input$var_table1)) %>% 
                              summarize(Effectif = n()) %>% 
                              rename(Variable = 1) %>% 
                              mutate(Variable = as.character(Variable)) %>% 
                              filter(is.na(Variable) == F) %>% 
                              mutate(`Pourcent (%)` =  round(100 * Effectif / sum(Effectif),3)) %>% 
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))
              
            }
            
          }else{ # ELSE AVEC PONDER
            
            validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
            
            # AVEC NA
            if (input$checkbox_na == TRUE) {
              
              
              # Avec Ponder avec NA  
              as.data.frame(filter_data() %>% 
                              group_by(get(input$var_table1)) %>% 
                              summarise(Effectif = round(sum(as.numeric(get(input$var_ponder_tri))),3)) %>% 
                              rename(Variable = 1) %>% 
                              mutate(Variable = as.character(Variable)) %>% 
                              mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
                              mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>% 
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))
              
              
            }else{ # ELSE SANS NA
              
              # Avec Ponder sans NA
              as.data.frame(filter_data() %>% 
                              group_by(get(input$var_table1)) %>% 
                              summarise(Effectif = round(sum(as.numeric(get(input$var_ponder_tri))),3)) %>% 
                              rename(Variable = 1) %>% 
                              mutate(Variable = as.character(Variable)) %>% 
                              filter(is.na(Variable) == F) %>% 
                              mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
                              mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>% 
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))
              
            }
          }
        } else if(input$choix_table == "eff"
        ){
          validate(need(input$var_table2, 'Choisir une 2ème variable'))
          
          # Si 2 variables : 
          if(input$var_table3 == ""){
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                
                # Sans Ponder avec NA
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2)) 
                
                
              }else{ # ELSE SANS NA
                
                # Sans Ponder sans NA
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2) )
                
                
              }
              
            }else{ # ELSE AVEC PONDER
              
              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
              
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Avec Ponder avec NA  
                
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                mutate(Effectif = round(ifelse(is.na(Effectif) == T,0, Effectif),2)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2)) 
                
                
              }else{ # ELSE SANS NA
                
                # Avec Ponder sans NA
                
                
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = round(ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif),2)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2)) 
                
                
                
                
              }
            }
            
          } else { # Si 3 variables
            validate(need(input$var_table3,'Choisir une 3ème variable'))
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                
                # Sans Ponder avec NA AVEC 3 VARIABLES
                
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2)) 
                
                
              }else{ # ELSE SANS NA
                
                # Sans Ponder sans NA AVEC 3 VARIABLES
                
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2) )
                
                
              }
              
            }else{ # ELSE AVEC PONDER
              
              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
              
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Avec Ponder avec NA  AVEC 3 VARIABLE
                
                
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                mutate(Effectif = round(ifelse(is.na(Effectif) == T,0, Effectif),2)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2)) 
                
                
              }else{ # ELSE SANS NA
                
                # Avec Ponder sans NA  AVEC 3 VARIABLE
                
                
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = round(ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif),2)) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2)) 
                
                
                
                
              }
            }
            
            
          }
        } else if(input$choix_table == "pct_lign"
        ){
          validate(need(input$var_table2, 'Choisir une 2ème variable'))
          
          # Si 2 variables : 
          if(input$var_table3 == ""){
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Sans Ponder avec NA
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_l) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 100*sum(Effectif, na.rm = T)/nrow(filter_data()), Pct_l),2))%>% 
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
                
                
              }else{ # ELSE SANS NA
                
                
                # Sans Ponder sans NA
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F & 
                                                                                   is.na(with(filter_data(),get(input$var_table2)))== F,]), 
                                                            Pct_l),2))%>% 
                                
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
              }
              
            }else{ # ELSE AVEC PONDER
              
              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
              
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                
                # Avec Ponder avec NA
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_l) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 100*sum(Effectif, na.rm = T)/sum(with(filter_data(), as.numeric(get(input$var_ponder_tri)))), Pct_l),2))%>% 
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
                
                
              }else{ # ELSE SANS NA
                
                # Avec Ponder sans NA
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                
                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F &
                                                                                       is.na(with(filter_data(),get(input$var_table2)))== F,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_l),2))%>%
                                
                                
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
                
                
                
                
              }
            }
            
          } else { # Si 3 variables
            
            validate(need(input$var_table3,'Choisir une 3ème variable'))
            
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                
                # Sans Ponder avec NA AVEC 3 VARIABLES
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_l) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(), get(input$var_table3) == input$var_table3_moda)), 
                                                            Pct_l),2))%>% 
                                
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
                
              }else{ # ELSE SANS NA
                
                
                # Sans Ponder sans NA AVEC 3 VARIABLE 
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(), 
                                                                          get(input$var_table3) == input$var_table3_moda &
                                                                            is.na(get(input$var_table1)) == F &
                                                                            is.na(get(input$var_table2)) == F )),
                                                            Pct_l),2))%>% 
                                
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
              }
              
            }else{ # ELSE AVEC PONDER
              
              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
              
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Avec Ponder avec NA AVEC 3 VARIABLES
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_l) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) == 
                                                                                       input$var_table3_moda,], 
                                                                       as.numeric(get(input$var_ponder_tri)))), 
                                                            Pct_l),2))%>% 
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
                
                
              }else{ # ELSE SANS NA
                
                
                
                # Avec Ponder sans NA AVEC 3 VARIABLE 
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variables) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_l = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variables) %>% 
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_l = round(ifelse(Variables == "Total", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) == 
                                                                                       input$var_table3_moda  &
                                                                                       with(filter_data(),is.na(get(input$var_table1))) == F &
                                                                                       with(filter_data(),is.na(get(input$var_table2))) == F  ,], 
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_l),2))%>% 
                                
                                select(Variables, Variable2, Pct_l) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>% 
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>% 
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))
                
                
              }
            }
            
            
          }
        } else if(input$choix_table == "pct_col"
        ){
          validate(need(input$var_table2, 'Choisir une 2ème variable'))
          
          # Si 2 variables : 
          if(input$var_table3 == ""){
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Sans Ponder avec NA
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_c) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 100*sum(Effectif, na.rm = T)/nrow(filter_data()), Pct_c),2))%>% 
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
                
              }else{ # ELSE SANS NA
                
                # Sans Ponder sans NA
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F & 
                                                                                   is.na(with(filter_data(),get(input$var_table2)))== F,]), 
                                                            Pct_c),2))%>% 
                                
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
              }
              
            }else{ # ELSE AVEC PONDER
              
              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
              
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Avec Ponder avec NA
                
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_c) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 100*sum(Effectif, na.rm = T)/sum(with(filter_data(), as.numeric(get(input$var_ponder_tri)))), Pct_c),2))%>% 
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
              }else{ # ELSE SANS NA
                
                # Avec Ponder sans NA
                as.data.frame(filter_data() %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F &
                                                                                       is.na(with(filter_data(),get(input$var_table2)))== F,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_c),2))%>%
                                
                                
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
                
                
              }
            }
            
          } else { # Si 3 variables
            
            validate(need(input$var_table3,'Choisir une 3ème variable'))
            
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                
                # Sans Ponder avec NA AVEC 3 VARIABLES
                
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_c) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(), get(input$var_table3) == input$var_table3_moda)), 
                                                            Pct_c),2))%>% 
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
              }else{ # ELSE SANS NA
                
                # Sans Ponder sans NA AVEC 3 VARIABLE 
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = n()) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(), 
                                                                          get(input$var_table3) == input$var_table3_moda &
                                                                            is.na(get(input$var_table1)) == F &
                                                                            is.na(get(input$var_table2)) == F )),
                                                            Pct_c),2))%>% 
                                
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
              }
              
            }else{ # ELSE AVEC PONDER
              
              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
              
              # AVEC NA
              if (input$checkbox_na == TRUE) {
                
                # Avec Ponder avec NA AVEC 3 VARIABLES
                
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = round(sum(as.numeric(get(input$var_ponder_tri))),5)) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>% 
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                #select(Variables, Variable2, Pct_c) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) == 
                                                                                       input$var_table3_moda,], 
                                                                       as.numeric(get(input$var_ponder_tri)))), 
                                                            Pct_c),2))%>% 
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
              }else{ # ELSE SANS NA
                
                
                # Avec Ponder sans NA AVEC 3 VARIABLE 
                as.data.frame(filter_data() %>% 
                                filter(get(input$var_table3) == input$var_table3_moda) %>% 
                                group_by(get(input$var_table1), get(input$var_table2)) %>% 
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>% 
                                rename(Variables = 1) %>% 
                                rename(Variable2 = 2) %>% 
                                filter(is.na(Variables) == F) %>% 
                                filter(is.na(Variable2) == F) %>% 
                                mutate(Variables = as.character(Variables)) %>% 
                                mutate(Variable2 = as.character(Variable2)) %>% 
                                ungroup() %>% 
                                complete(Variables,Variable2) %>% 
                                group_by(Variable2) %>% 
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                                mutate(Total = sum(Effectif, na.rm = T)) %>% 
                                mutate(Pct_c = 100*Effectif/Total) %>% 
                                ungroup() %>% 
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>% 
                                complete(Variables,Variable2)%>% 
                                group_by(Variable2) %>% 
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>% 
                                group_by(Variables) %>% 
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) == 
                                                                                       input$var_table3_moda  &
                                                                                       with(filter_data(),is.na(get(input$var_table1))) == F &
                                                                                       with(filter_data(),is.na(get(input$var_table2))) == F  ,], 
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_c),2))%>% 
                                
                                select(Variables, Variable2, Pct_c) %>% 
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>% 
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>% 
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))
                
                
                
              }
            }
            
            
          }
        }
        
        
      })
      
      
      
      #### Affichage de la table     -----
      
      
      # Possibilité de l'afficher en data.frame, on préfère une simple table
      
      output$tri_affiche <- renderTable({ 
        table_to_save()
        
      })
      
      observeEvent(input$var_table1, { 
        output$affichage_table <- renderUI({
          tableOutput("tri_affiche")
        })
      })
      
      
      
      
      #### Warning trop de modalités ----
      
      # Pour la variable 1, si plus de 9 modalités
      observeEvent(input$var_table1, {
        validate(need(input$var_table1,""))
        if(dim(with(filter_data(), table(get(input$var_table1))))  >= 9 ){
          
          # showNotification("This is a notification.")
          showModal(modalDialog(
            title = "Nombre de modalités trop important",
            "La variable que vous avez sélectionné a trop de modalité pour être observée correctement dans une table. \n Vous pouvez la recoder dans l'onglet 'Variables'",
            easyClose = TRUE,
            footer = NULL))
          
        }
      })
      
      # Pour la variable 2, si plus de 9 modalités
      observeEvent(input$var_table2, {
        validate(need(input$var_table2,""))
        if(dim(with(filter_data(), table(get(input$var_table2))))  >= 9 ){
          
          #showNotification("This is a notification. 22222")
          showModal(modalDialog(
            title = "Nombre de modalités trop important",
            "La variable que vous avez sélectionné a trop de modalité pour être observée correctement dans une table. \n Vous pouvez la recoder dans l'onglet 'Variables'",
            easyClose = TRUE,
            footer = NULL))
          
        }
        
      })
      
      # Pour la variable 3, si plus de 5 modalités
      observeEvent(input$var_table3, {
        validate(need(input$var_table3,""))
        if(dim(with(filter_data(), table(get(input$var_table3))))  >= 5 ){
          
          #showNotification("This is a notification. 22222")
          showModal(modalDialog(
            title = "Nombre de modalités trop important",
            "La variable que vous avez sélectionné a trop de modalité pour être observée correctement dans une table. \n Vous pouvez la recoder dans l'onglet 'Variables'",
            easyClose = TRUE,
            footer = NULL))
          
        }
        
      })
      
      
      #### Sauvegarde de la table    ----
      
      
      output$savetable <- downloadHandler(
        filename = function() {
          paste('statdesc-', Sys.Date(), '.xlsx', sep = '')
        },
        content = function(file){
          openxlsx::write.xlsx(table_to_save(),file)
        }
      )
      
      
      
      # 4) Graphique ----  
      # Permet d'observer les variables dans des graphiques
      ###################
      #### Serveur Graphique
      ###################
      
      
      #### GESTION DE L'UI       ----  
      
      ## Choix Variables         ----
      
      # Condition d'affichage des variables a représenter
      
      output$affichage_choix_var1_plot <- renderUI({
        
        validate(need(input$target_upload, 'Importer des données'))
        
        fluidRow(pickerInput("var_plot1", "Variable 1 :", c("",nomcol_data_reac()),
                             multiple = F,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en ligne")))
      })
      
      
      output$affichage_choix_var2_plot <- renderUI({
        validate(need(input$target_upload, ''))
        fluidRow(
          conditionalPanel(condition="input.var_plot1 != ''", 
                           pickerInput("var_plot2", "Variable 2 :", c("",nomcol_data_reac()),
                                       multiple = F,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en colonne"))))
      })
      
      
      ## Choix Graphique         ----
      # Bandeau de sélection du type de graphique
      
      output$affichage_choix_plot_type <- renderUI({ 
        validate(need(input$target_upload, ''))
        wellPanel(
          fluidRow(
            # Choix du graphique
            selectInput(inputId="choix_plot",
                        label="Choix du type de graphique : ",
                        choices= c("Univarié : Cleveland (effectifs)" = "barplot_eff_uni", 
                                   "Univarié : Cleveland (proportions)" = "barplot_freq_uni", 
                                   "Bivarié : Bâtons (effectifs)" = "barplot_eff_bi", 
                                   "Bivarié : Bâtons (proportions)" = "barplot_freq_bi"))),
          fluidRow(
            # Case a cocher pour pondération
            checkboxInput(inputId = "checkbox_ponder_plot", label = "Utiliser une pondération ?", value = FALSE, width = NULL)),
          fluidRow(column(11,offset = 1,
                          # Choix de la variable de pondération
                          conditionalPanel(condition="output.afficher_plot_ponder == 'Oui'", 
                                           selectizeInput('var_ponder_plot',
                                                          label=NULL,
                                                          # Choix parmis les noms de variables de data
                                                          choices=c("",nomcol_data_reac()),
                                                          # Plusieurs options : 
                                                          options = list(`actions-box` = TRUE, placeholder = 'Pas de pondération'), 
                                                          multiple = FALSE, 
                                                          width = 450))
          )),
          fluidRow(
            checkboxInput(inputId = "checkbox_na_plot", label = "Afficher les valeurs manquantes ?", value = FALSE, width = NULL) 
          )
          
          
        )
        
        
      })
      
      ## Condition d'affichage   ----
      
      # Condition d'affichage de la selection de la variable de pondération
      output$afficher_plot_ponder <- reactive({
        validate(need(input$checkbox_ponder_plot,""))
        if (input$checkbox_ponder_plot != FALSE) {
          "Oui"
        }
      })
      outputOptions(output, "afficher_plot_ponder", suspendWhenHidden=FALSE)
      
      
      # Condition d'affichage du bouton de sauvegarde
      output$afficher_plot_sauvegarde <- reactive({
        validate(need(input$var_plot1,""))
        if (input$var_plot1 != FALSE) {
          "Oui"
        }
      })
      outputOptions(output, "afficher_plot_sauvegarde", suspendWhenHidden=FALSE)
      
      
      
      ## Paramètres du graphique ----
      
      # Creation du bandeau des paramètres du graphique 
      # Il dépend du choix du graphique
      
      output$param_plot <- renderUI({ 
        validate(need(input$target_upload, ''))
        validate(need(input$var_plot1, ''))
        
        if (input$choix_plot == "barplot_eff_uni" | input$choix_plot == "barplot_freq_uni") {
          
          wellPanel(
            fluidRow(
              radioButtons(inputId = "radio_ordre",
                           label = "Ordre des modalités",
                           choices = c("Normal","Croissant", "Décroissant"), selected="Normal",inline=F)
            ),
            
            fluidRow(
              sliderInput(inputId = "taille_axe",
                          label = "Taille des axes",                            
                          min = 10, max = 20, step = 1, value = 15)
            ),
            fluidRow(
              sliderInput(inputId = "n_break",
                          label = "Intervalle du quadrillage",                            
                          min = 5, max = 100, step = 10, value = 20)
            ),
            fluidRow(
              colourInput("col", "Couleur", "#076fa2",showColour = "background")
            )
            
          )
          
          
        } else if (input$choix_plot == "barplot_eff_bi" | input$choix_plot == "barplot_freq_bi") {
          
          validate(need(input$var_plot2, 'Choisir une 2ème variable'))
          
          wellPanel(
            fluidRow(
              radioButtons(inputId = "radio_ordre",
                           label = "Ordre des modalités",
                           choices = c("Normal","Croissant", "Décroissant"), selected="Normal",inline=F)
            ),
            
            fluidRow(
              sliderInput(inputId = "taille_axe",
                          label = "Taille des axes",                            
                          min = 10, max = 20, step = 1, value = 15)
            ),
            fluidRow(
              sliderInput(inputId = "taille_label",
                          label = "Taille des étiquettes",                            
                          min = 4, max = 10, step = 1, value = 7)
            ),
            fluidRow(
              sliderInput(inputId = "n_break",
                          label = "Intervalle du quadrillage",                            
                          min = 10, max = 100, step = 10, value = 20)
            ),
            fluidRow(
              sliderInput(inputId = "width_bar",
                          label = "Epaisseur des barres",                            
                          min = 0.1, max = 1, step = 0.1, value = 0.5)
            ),
            fluidRow(
              colourInput("col_label", "Couleur label", "#B2B2B2",showColour = "background")
            ),
            fluidRow(
              radioButtons("choix_color","Choix couleurs :",
                           choices = c("Palette","Manuel"),
                           selected="Palette",inline=FALSE)
            ),
            conditionalPanel(condition="input.choix_color == 'Palette'",
                             fluidRow(
                               selectizeInput('palette',
                                              label=NULL,
                                              choices=c("Pastel1","Pastel2", "Set1", "Set2", "Accent",
                                                        "YlOrRd","YlGnBu","Purples",
                                                        "Spectral", "RdYlGn", "RdBu", "PiYG"),
                                              selected = "Pastel1",
                                              multiple = FALSE)
                             )
            ),
            
            # On crée autant de sélection que de modalité dans la variable 1 
            conditionalPanel(condition="input.choix_color == 'Manuel'",
                             fluidRow(
                               
                               lapply((1:length(unique(with(filter_data(), get(input$var_plot2))))), function(i){
                                 col_hop <- brewer.pal(n = length(unique(with(filter_data(), get(input$var_plot2)))), name = "Blues")
                                 colourInput(paste0("color_",i), NULL, col_hop[i],showColour = "background")
                               })
                               
                             )
            )
            
          )
          
        }
        
        
      })
      
      
      
      ## Choix du titre          ----
      
      # Encadré pour l'écrire
      output$param_titre <- renderUI({ 
        validate(need(input$target_upload,''))
        conditionalPanel(condition="input.var_plot1 != ''",
                         if (input$choix_plot == "barplot_eff_uni" | input$choix_plot == "barplot_freq_uni") {
                           textInput("plot_titre", NULL, width = "600px",
                                     placeholder = paste0("Graphique en bâton de la variable ",
                                                          input$var_plot1))
                         }else{
                           textInput("plot_titre", NULL,  width = "600px",
                                     placeholder = paste0("Graphique en bâton des variables ",
                                                          input$var_plot1, " et ", input$var_plot2 ))
                         }
                         
        )
      })
      
      
      # Définition du titre
      plot_titre_reac <- reactive({
        
        validate(need(input$target_upload,''))
        if (input$choix_plot == "barplot_eff_uni" | input$choix_plot == "barplot_freq_uni") {
          if (input$plot_titre == "") {
            paste0("Graphique en bâton de la variable ", input$var_plot1)
          } else {
            input$plot_titre
          }
        }else{
          if (input$plot_titre == "") {
            paste0("Graphique en bâton des variables ",input$var_plot1, " et ", input$var_plot2 )
          } else {
            input$plot_titre
          }
        }
        
      })
      
      
      
      #############
      
      #############
      #### CREATION DES GRAPHIQUES ----
      
      # Dans un objet reactif on crée un graphique qui dépend des paramètre
      # défini au dessus
      # Il s'agit d'un embrachement en if else
      # Noeud 1 : choix du graphique
      # Noeud 2 : pondération
      # Noeud 3 : affichage des NA
      
      plot_to_save <- reactive({
        validate(need(input$target_upload, ''))
        validate(need(input$var_plot1, ''))
        
        if (input$choix_plot == "barplot_eff_uni"
        ){
          # Pondération
          if (input$checkbox_ponder_plot == FALSE) {
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_uni_na <<- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                mutate(Pct = Somme / sum(Somme)*100)%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", levels(with(filter_data(), as.factor(get(input$var_plot1)))))) %>% 
                mutate(Var1=factor(Var1, levels=Var1))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni_na$Var1, p_uni_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni_na$Var1, desc(p_uni_na$Somme))
              }
              
              # Graphique
              p <- ggplot(p_uni_na, aes(x=ordre_moda_graph, y=Somme)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni_na$Somme)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni_na$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Effectifs"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
              
            }else{ # ELSE SANS NA
              
              p_uni <- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                mutate(Pct = Somme / sum(Somme)*100) 
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni$Var1, p_uni$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni$Var1, desc(p_uni$Somme))
              }
              
              # Graphique
              p <- ggplot(p_uni, aes(x=ordre_moda_graph, y=Somme)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni$Somme)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Effectifs"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
            }
            
          }else{ # ELSE AVEC PONDER
            
            validate(need(input$var_ponder_plot, 'Choisir une variable de pondération'))
            
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_uni_ponder_na <- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
                rename(Var1 = 1) %>% 
                mutate(Pct = Somme / sum(Somme)*100)%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", levels(with(filter_data(), as.factor(get(input$var_plot1))))))%>% 
                mutate(Var1=factor(Var1, levels=Var1))
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni_ponder_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni_ponder_na$Var1, p_uni_ponder_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni_ponder_na$Var1, desc(p_uni_ponder_na$Somme))
              }
              
              
              # Graphique
              p <- ggplot(p_uni_ponder_na, aes(x=ordre_moda_graph, y=Somme)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni_ponder_na$Somme)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni_ponder_na$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Effectifs pondérés"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
            }else{ # ELSE SANS NA
              
              
              
              p_uni_ponder <- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder
                rename(Var1 = 1) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                mutate(Pct = Somme / sum(Somme)*100)
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni_ponder$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni_ponder$Var1, p_uni_ponder$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni_ponder$Var1, desc(p_uni_ponder$Somme))
              }
              
              
              # Graphique
              p <- ggplot(p_uni_ponder, aes(x=ordre_moda_graph, y=Somme)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni_ponder$Somme)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni_ponder$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Effectifs pondérés"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
            }
          }
        } else if(input$choix_plot == "barplot_freq_uni"
        ){
          # Pondération
          if (input$checkbox_ponder_plot == FALSE) {
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_uni_na <<- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                mutate(Pct = Somme / sum(Somme)*100)%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", levels(with(filter_data(), as.factor(get(input$var_plot1)))))) %>% 
                mutate(Var1=factor(Var1, levels=Var1))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni_na$Var1, p_uni_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni_na$Var1, desc(p_uni_na$Somme))
              }
              
              # Graphique
              p <- ggplot(p_uni_na, aes(x=ordre_moda_graph, y=Pct)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni_na$Pct)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni_na$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Pourcentages"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
              
            }else{ # ELSE SANS NA
              
              p_uni <- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                mutate(Pct = Somme / sum(Somme)*100)
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni$Var1, p_uni$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni$Var1, desc(p_uni$Somme))
              }
              
              # Graphique
              
              # Graphique
              p <- ggplot(p_uni, aes(x=ordre_moda_graph, y=Pct)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni$Pct)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Pourcentages"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
              
            }
            
          }else{ # ELSE AVEC PONDER
            
            validate(need(input$var_ponder_plot, 'Choisir une variable de pondération'))
            
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_uni_ponder_na <- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
                rename(Var1 = 1) %>% 
                mutate(Pct = Somme / sum(Somme)*100)%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", levels(with(filter_data(), as.factor(get(input$var_plot1)))))) %>% 
                mutate(Var1=factor(Var1, levels=Var1))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni_ponder_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni_ponder_na$Var1, p_uni_ponder_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni_ponder_na$Var1, desc(p_uni_ponder_na$Somme))
              }
              
              # Graphique
              p <- ggplot(p_uni_ponder_na, aes(x=ordre_moda_graph, y=Pct)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni_ponder_na$Pct)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni_ponder_na$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Pourcentages pondérés"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
            }else{ # ELSE SANS NA
              
              
              
              p_uni_ponder <- filter_data() %>% 
                group_by(get(input$var_plot1)) %>% #input$var_plot1
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder
                rename(Var1 = 1) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                mutate(Pct = Somme / sum(Somme)*100)
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_uni_ponder$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_uni_ponder$Var1, p_uni_ponder$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_uni_ponder$Var1, desc(p_uni_ponder$Somme))
              }
              
              # Graphique
              p <- ggplot(p_uni_ponder, aes(x=ordre_moda_graph, y=Pct)) +
                geom_segment( aes(xend=Var1, yend=0), color=input$col) +
                geom_point( size=4, color=input$col) +
                coord_flip()+
                theme_bw() +
                xlab("") + # input$col input$width_bar
                
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_uni_ponder$Pct)/input$n_break) * input$n_break),
                  breaks = seq(0, ceiling(max(p_uni_ponder$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Bouger echelle
                ) +
                
                scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "white", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "#202020"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_text( size = input$taille_axe),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                )  +
                
                labs(
                  title = plot_titre_reac(), # input$plot_titre
                  subtitle = "Pourcentages pondérés"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                )
              p
              
              
            }
          }
        } else if(input$choix_plot == "barplot_eff_bi"
        ){
          validate(need(input$var_plot2, 'Choisir une 2ème variable'))
          # Pondération
          if (input$checkbox_ponder_plot == FALSE) {
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_bi_na <<- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme))%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1),
                       Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2))
              
              p_bi_na$Var2 <- factor(p_bi_na$Var2, levels = rev(levels(p_bi_na$Var2)))
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi_na$Var1, p_bi_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi_na$Var1, desc(p_bi_na$Somme))
              }
              
              
              
              p <- ggplot(p_bi_na, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="stack", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_bi_na$Total)/input$n_break) * input$n_break), # input$n_break
                  breaks = seq(0, ceiling(max(p_bi_na$Total)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top after we pivot
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = subset(p_bi_na, Total < max(p_bi_na$Total)/10),
                  aes(Total, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label, # input$col_label
                  size = input$taille_label # input$taille_label
                ) + 
                geom_text(
                  data = subset(p_bi_na, Total >= max(p_bi_na$Total)/10),
                  aes(1, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Effectifs"
                ) + 
                theme(
                  plot.title = element_text(
                    #  family = "Econ Sans Cnd", 
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    #  family = "Econ Sans Cnd",
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
              
            }else{ # ELSE SANS NA
              
              p_bi <- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                filter(is.na(Var2) == FALSE) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme),
                       Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2))
              
              p_bi$Var2 <- factor(p_bi$Var2, levels = rev(levels(p_bi$Var2)))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi$Var1, p_bi$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi$Var1, desc(p_bi$Somme))
              }
              
              p <- ggplot(p_bi, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="stack", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_bi$Total)/input$n_break) * input$n_break), # input$n_break
                  breaks = seq(0, ceiling(max(p_bi$Total)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top after we pivot
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = subset(p_bi, Total < max(p_bi$Total)/10),
                  aes(Total, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label, # input$col_label
                  size = input$taille_label # input$taille_label
                ) + 
                geom_text(
                  data = subset(p_bi, Total >= max(p_bi$Total)/10),
                  aes(1, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Effectifs"
                ) + 
                theme(
                  plot.title = element_text(
                    #  family = "Econ Sans Cnd", 
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    #  family = "Econ Sans Cnd",
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
            }
            
          }else{ # ELSE AVEC PONDER
            
            validate(need(input$var_ponder_plot, 'Choisir une variable de pondération'))
            
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_bi_ponder_na <- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme))%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1),
                       Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2)) 
              
              p_bi_ponder_na$Var2 <- factor(p_bi_ponder_na$Var2, levels = rev(levels(p_bi_ponder_na$Var2)))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi_ponder_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi_ponder_na$Var1, p_bi_ponder_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi_ponder_na$Var1, desc(p_bi_ponder_na$Somme))
              }
              
              p <- ggplot(p_bi_ponder_na, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="stack", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_bi_ponder_na$Total)/input$n_break) * input$n_break), # input$n_break
                  breaks = seq(0, ceiling(max(p_bi_ponder_na$Total)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top after we pivot
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = subset(p_bi_ponder_na, Total < max(p_bi_ponder_na$Total)/10),
                  aes(Total, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label, # input$col_label
                  size = input$taille_label # input$taille_label
                ) + 
                geom_text(
                  data = subset(p_bi_ponder_na, Total >= max(p_bi_ponder_na$Total)/10),
                  aes(1, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Effectifs pondérés"
                ) + 
                theme(
                  plot.title = element_text(
                    #  family = "Econ Sans Cnd", 
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    #  family = "Econ Sans Cnd",
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
            }else{ # ELSE SANS NA
              
              
              
              p_bi_ponder <- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                filter(is.na(Var2) == FALSE) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme)) %>% 
                mutate(Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2))
              
              
              p_bi_ponder$Var2 <- factor(p_bi_ponder$Var2, levels = rev(levels(p_bi_ponder$Var2)))
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi_ponder$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi_ponder$Var1, p_bi_ponder$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi_ponder$Var1, desc(p_bi_ponder$Somme))
              }
              
              
              p <- ggplot(p_bi_ponder, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="stack", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  limits = c(0,ceiling(max(p_bi_ponder$Total)/input$n_break) * input$n_break), # input$n_break
                  breaks = seq(0, ceiling(max(p_bi_ponder$Total)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top after we pivot
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = subset(p_bi_ponder, Total < max(p_bi_ponder$Total)/10),
                  aes(Total, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label, # input$col_label
                  size = input$taille_label # input$taille_label
                ) + 
                geom_text(
                  data = subset(p_bi_ponder, Total >= max(p_bi_ponder$Total)/10),
                  aes(1, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = -0.2,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Effectifs pondérés"
                ) + 
                theme(
                  plot.title = element_text(
                    #  family = "Econ Sans Cnd", 
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    #  family = "Econ Sans Cnd",
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
            }
          }
        } else if(input$choix_plot == "barplot_freq_bi"
        ){
          
          validate(need(input$var_plot2, 'Choisir une 2ème variable'))
          
          # Pondération
          if (input$checkbox_ponder_plot == FALSE) {
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_bi_na <<- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme))%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1),
                       Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2))
              
              p_bi_na$Var2 <- factor(p_bi_na$Var2, levels = rev(levels(p_bi_na$Var2)))
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi_na$Var1, p_bi_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi_na$Var1, desc(p_bi_na$Somme))
              }
              
              p <- ggplot(p_bi_na, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="fill", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  breaks = seq(0,1,input$n_break/100),
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = p_bi_na,
                  aes(0.01, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = 0,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Proportions"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
              
            }else{ # ELSE SANS NA
              
              p_bi <- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = n()) %>% 
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                filter(is.na(Var2) == FALSE) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme),
                       Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2))
              
              p_bi$Var2 <- factor(p_bi$Var2, levels = rev(levels(p_bi$Var2)))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi$Var1, p_bi$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi$Var1, desc(p_bi$Somme))
              }
              
              p <- ggplot(p_bi, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="fill", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  breaks = seq(0,1,input$n_break/100),
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),            
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = p_bi,
                  aes(0.01, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = 0,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Proportions"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
            }
            
          }else{ # ELSE AVEC PONDER
            
            validate(need(input$var_ponder_plot, 'Choisir une variable de pondération'))
            
            # AVEC NA
            if (input$checkbox_na_plot == TRUE) {
              
              p_bi_ponder_na <- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme))%>% 
                # Prise en compte des NAs
                mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1),
                       Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2)) 
              
              p_bi_ponder_na$Var2 <- factor(p_bi_ponder_na$Var2, levels = rev(levels(p_bi_ponder_na$Var2)))
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi_ponder_na$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi_ponder_na$Var1, p_bi_ponder_na$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi_ponder_na$Var1, desc(p_bi_ponder_na$Somme))
              }
              
              p <- ggplot(p_bi_ponder_na, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="fill", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  breaks = seq(0,1,input$n_break/100),
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),             
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = p_bi_ponder_na,
                  aes(0.01, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = 0,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Proportions"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
            }else{ # ELSE SANS NA
              
              
              
              p_bi_ponder <- filter_data() %>% 
                group_by(get(input$var_plot1), get(input$var_plot2)) %>% #input$var_plot1 input$var_plot2
                summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
                rename(Var1 = 1) %>% 
                rename(Var2 = 2) %>% 
                filter(is.na(Var1) == FALSE) %>% 
                filter(is.na(Var2) == FALSE) %>% 
                group_by(Var1) %>%
                mutate(Pct = Somme / sum(Somme)*100,
                       Total = sum(Somme)) %>% 
                mutate(Var1 = as.factor(Var1),
                       Var2 = as.factor(Var2))
              
              
              p_bi_ponder$Var2 <- factor(p_bi_ponder$Var2, levels = rev(levels(p_bi_ponder$Var2)))
              
              
              # Ordre
              ordre_moda_graph <- vector()
              if (input$radio_ordre == "Normal") {
                ordre_moda_graph <- p_bi_ponder$Var1
              }else if (input$radio_ordre == "Croissant") {
                ordre_moda_graph <- reorder(p_bi_ponder$Var1, p_bi_ponder$Somme)
              }else if (input$radio_ordre == "Décroissant") {
                ordre_moda_graph <- reorder(p_bi_ponder$Var1, desc(p_bi_ponder$Somme))
              }
              
              p <- ggplot(p_bi_ponder, aes(fill=Var2, y=Somme, x=ordre_moda_graph)) + 
                geom_bar(position="fill", stat="identity", width = input$width_bar) +
                scale_fill_manual(values=palette_plot())+
                scale_y_continuous(
                  breaks = seq(0,1,input$n_break/100),
                  expand = c(0, 0), # The horizontal axis does not extend to either side
                  position = "right"  # Labels are located on the top
                )  +
                # scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
                #coord_flip()+
                theme(
                  # Set background color to white
                  panel.background = element_rect(fill = "white"),    
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  # Set the color and the width of the grid lines for the horizontal axis
                  panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
                  # Remove tick marks by setting their length to 0
                  axis.ticks.length = unit(0, "mm"),
                  # Remove the title for both axes
                  axis.title = element_blank(),
                  # Only left line of the vertical axis is painted in black
                  axis.line.y.left = element_line(color = "black"),
                  # Remove labels from the vertical axis
                  axis.text.y = element_blank(),
                  # But customize labels for the horizontal axis
                  axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
                ) + 
                geom_text(
                  data = p_bi_ponder,
                  aes(0.01, x = as.factor(Var1), label = as.factor(Var1)),
                  hjust = 0,
                  nudge_x = 0,
                  colour = input$col_label,  # input$col_label
                  #   bg.colour = "black",
                  size = input$taille_label # input$taille_label
                ) +
                coord_flip()+
                labs(
                  title = plot_titre_reac(), # plot_titre_reac()
                  subtitle = "Proportions"
                ) + 
                theme(
                  plot.title = element_text(
                    face = "bold",
                    size = 22
                  ),
                  plot.subtitle = element_text(
                    face = "italic",
                    size = 16
                  )
                ) + 
                guides(fill = guide_legend(title = input$var_plot2)) # input$var_plot2
              
              p
              
              
            }
          }
        }
        
        
      })
      
      
      
      output$reactiv_plot <- renderPlot({
        plot_to_save()
      })
      
      
      ## CREATION PALETTE COULEUR  ----
      
      
      
      palette_plot <- reactive({
        validate(need(input$target_upload, ''))
        validate(need(input$var_plot1, ''))
        validate(need(input$var_plot2, ''))
        
        if (input$choix_color == "Palette") {
          
          brewer.pal(n = length(unique(with(filter_data(), get(input$var_plot2)))), 
                     name = input$palette)
          
        } else {
          
          hop <- input$color_1
          for (i in c(2:length(unique(with(filter_data(), get(input$var_plot2)))))) {
            hop <- c(hop, input[[paste0("color_",i)]])
          }
          return(hop)
        }
        
        
        
      })
      
      
      
      
      ## CREATION LECTURE ----
      
      # Pour plus tard :
      # Texte explicatif
      
      # output$reactiv_plot_lecture <- renderPlot({
      #   plot_lecture()
      # })
      # 
      # plot_lecture <- reactive({
      #   validate(need(input$target_upload, ''))
      #   validate(need(input$var_plot1, ''))
      #   
      #   
      #   # SANS Pondération
      #   if (input$checkbox_ponder_plot == FALSE) {
      #     # AVEC NA
      #     if (input$checkbox_na_plot == TRUE) {
      #       
      #       p_uni_na <<- filter_data() %>% 
      #         group_by(get(input$var_plot1)) %>% #input$var_plot1
      #         summarise(Somme = n()) %>% 
      #         rename(Var1 = 1) %>% 
      #         mutate(Pct = Somme / sum(Somme)*100)%>% 
      #         # Prise en compte des NAs
      #         mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1)) %>% 
      #         arrange(desc(Somme))
      #       
      #       grid.text(
      #         paste("Lecture :",round(p_uni_na[1,2],2),"individus ont répondu", p_uni_na[1,1], "à la variable", input$var_plot1), #input$var_plot1
      #         x = 0.005,
      #         y = 0.06,
      #         just = c("left", "bottom"),
      #         gp = gpar(
      #           col = "grey50",
      #           fontsize = 12,
      #           fontface = "italic"
      #         )
      #       )
      #       
      #       
      #       
      #     }else{ # ELSE SANS NA
      #       
      #       p_uni <- filter_data() %>% 
      #         group_by(get(input$var_plot1)) %>% #input$var_plot1
      #         summarise(Somme = n()) %>% 
      #         rename(Var1 = 1) %>% 
      #         filter(is.na(Var1) == FALSE) %>% 
      #         mutate(Pct = Somme / sum(Somme)*100)%>% 
      #         # Prise en compte des NAs
      #         arrange(desc(Somme))
      #       
      #       
      #       grid.text(
      #         paste("Lecture :",round(p_uni[1,2],2),"individus ont répondu", p_uni[1,1], "à la variable", input$var_plot1), #input$var_plot1
      #         x = 0.005, 
      #         y = 0.06, 
      #         just = c("left", "bottom"),
      #         gp = gpar(
      #           col = "grey50",
      #           fontsize = 12,
      #           fontface = "italic"
      #         )
      #       )
      #       
      #       
      #     }
      #     
      #   }else{ # ELSE AVEC PONDER
      #     
      #     validate(need(input$var_ponder_plot, 'Choisir une variable de pondération'))
      #     
      #     # AVEC NA
      #     if (input$checkbox_na_plot == TRUE) {
      #       
      #       p_uni_ponder_na <- filter_data() %>% 
      #         group_by(get(input$var_plot1)) %>% #input$var_plot1
      #         summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder_plot
      #         rename(Var1 = 1) %>% 
      #         mutate(Pct = Somme / sum(Somme)*100)%>% 
      #         # Prise en compte des NAs
      #         mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1)) %>% 
      #         arrange(desc(Somme))
      #       
      #       
      #       grid.text(
      #         paste("Lecture :",round(p_uni_ponder_na[1,2],2),"individus ont répondu", p_uni_ponder_na[1,1], "à la variable", input$var_plot1), #input$var_plot1
      #         x = 0.005, 
      #         y = 0.06, 
      #         just = c("left", "bottom"),
      #         gp = gpar(
      #           col = "grey50",
      #           fontsize = 10,
      #           fontface = "italic"
      #         )
      #       )
      #       
      #       
      #     }else{ # ELSE SANS NA
      #       
      #       
      #       
      #       p_uni_ponder <- filter_data() %>% 
      #         group_by(get(input$var_plot1)) %>% #input$var_plot1
      #         summarise(Somme = sum(as.numeric(get(input$var_ponder_plot)))) %>% #input$var_ponder
      #         rename(Var1 = 1) %>% 
      #         filter(is.na(Var1) == FALSE) %>% 
      #         mutate(Pct = Somme / sum(Somme)*100)%>% 
      #         # Prise en compte des NAs
      #         arrange(desc(Somme))
      #       
      #       
      #       grid.text(
      #         paste("Lecture :",round(p_uni_ponder[1,2],2),"individus ont répondu", p_uni_ponder[1,1], "à la variable", input$var_plot1), #input$var_plot1
      #         x = 0.005, 
      #         y = 0.06, 
      #         just = c("left", "bottom"),
      #         gp = gpar(
      #           col = "grey50",
      #           fontsize = 10,
      #           fontface = "italic"
      #         )
      #       )
      #       
      #       
      #     }
      #   }
      #   
      #   
      #   
      # })
      # 
      
      
      
      
      #############
      
      #############
      #### SAUVEGARDE ----
      
      
      output$pngsave <- downloadHandler(
        filename = function() { paste("Graphique-", Sys.Date(), '.png', sep='') },
        content = function(file) {
          png(file, width = 800)
          print(plot_to_save())
          dev.off()
        })
      
      
      
    }) # Fin ShinyServeur
  
  
) # Fin ShinyApp















