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
  
  pickerInput("var_recod", "Choix de la variable à recoder :", c("",nomreac$nomcol),
                       multiple = F,
                       options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable à recoder"))
  
  
})

# Table de cette variable
output$table_recod_avant <- renderTable({
  validate(need(input$var_recod,''))
  
  hop <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(input$var_recod), useNA = "always")))))) %>% 
    slice(2)
  colnames(hop) <- c(with(v$data, names(table(get(input$var_recod)))), "Non Réponse", "Total")
  hop
  
})


# Création d'un reactiveValues qui permet de contrôler le fait d'appuyer sur les boutons
# permet l'affichage et la disparition de table
button_state <- reactiveValues(bttn = 0, valid = FALSE)

# Quand on clique prend valeur >= 1
observeEvent(input$RecodeGO, {
  button_state$bttn <- button_state$bttn +1
})
# Quand on change de variable se reset
observeEvent(input$var_recod, {
  button_state$bttn <- 0
})
# Si 0 table s'efface, si 1 table s'affiche

# Idem pour RecodeOK
observeEvent(input$RecodeOK, {
  button_state$valid <- button_state$valid +1
})


# Cadre avec le nom de la variable à recoder
output$nom_var_recod_avant <- renderText({
  validate(need(input$var_recod,''))
  
  if (button_state$bttn > 0) {
  input$var_recod
  }
})



# Pour chaque modalité de la variable a recoder, on créer un cadre de texte
# avec le nom de la modalité
observeEvent(input$RecodeGO, {
  validate(need(input$var_recod,''))
  lapply((1:length(unique(with(v$data, get(input$var_recod))))), function(i) {
    outputId <- paste0("OUT", i)
    output[[outputId]] <- renderText(levels(with(v$data, as.factor(get(input$var_recod))))[i])
    
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
  validate(need(input$var_recod,''))
  # Si l'utilisateur n'écrit pas un nom de variable,
  # on ajoute par defaut le suffixe _recode, sinon choix utilisateur 
  if (input$nom_var_recod_apres == "") {
    paste0(input$var_recod, "_recode") # Pas utile
  } else {
    input$nom_var_recod_apres
  }
})


## CREATION DES CASES AVEC LES MODALITES

# Quand on choisit une variable à recoder, ça ouvre l'interface suivante :
observeEvent(input$RecodeGO, {
  output$recodage <- renderUI({
    
    if (button_state$bttn > 0) {
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
                       textInput("nom_var_recod_apres", NULL, placeholder = paste0(input$var_recod,"_recode"))) ### changement
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
              lapply((1:length(unique(with(v$data, get(input$var_recod))))), function(i){ # Changement
                
                inputId <- paste0("input_", i)
                fluidRow(column(4,  offset = 1,
                                verbatimTextOutput(outputId=paste0("OUT",i))),
                         column(2, align = "center",
                                icon("arrow-right", class = "fa-3x", lib = "font-awesome")),
                         column(4,
                                textInput(paste0("input_", i), NULL, width = 500, placeholder = levels(as.factor(with(v$data, get(input$var_recod))))[i]))) # Changement
                #textInput(paste0("input_", i), NULL, width = 500, placeholder = "Même modalité")))
                
              }), # FIN cases
              
              # Affichage du bouton de validation
              fluidRow(column(4, offset = 4 , align = "center",
                              actionButton("RecodeOK", "Valider", class = "btn-success")))
    ) # FIN wellpanel 
  
    } # FIN if
    }) # FIN UI
}) # FIN observeEvent




### POUR REORDER : ----

## Choix de la variable à réordonner
output$choix_var_reorder<- renderUI({
  
  validate(need(input$target_upload, 'Importer des données'))
  # Pour le moment, on ne peut reordonner que les variables en entrée 
  # (techniquement possible avec des variables recodées, mais ça rend pas bien)
  fluidRow(pickerInput("var_reord", "Choix de la variable à réordonner :", c("",nomreac$nomcol),
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


# Quand on appuye sur le bouton recodage GO
observeEvent(input$RecodeGO, {
  validate(need(input$var_recod,''))
  
  # On sauvegarde le nom de la variable choisi pour l'affichage de table même
  # quand var_recod est reset
  var_recod <<- input$var_recod

  # On crée une autre base et une nouvelle variable basée sur l'input
  recod_data <<- v$data  %>% 
    mutate(newvar = as.character(get(var_recod)))
  
}) # Fin var_recod 



# Quand on valide le recodage
observeEvent(input$RecodeOK, {
  
  # On procède au recodage
  
  # Recodage si NA : 
  if (anyNA(with(recod_data, get(var_recod))) == T) {
    # Pour chaque modalité de la variable (-1 pour les NA)
    # On donne la valeur dans la case recodage si l'utilisateur à écrit dedans,
    # sinon on garde la valeur précédente.
    for (i in c(1: (length(unique(with(recod_data, as.factor(get(var_recod)))))-1))) {
      recod_data <- recod_data %>%
        mutate(newvar = ifelse(is.na(get(var_recod)) == T, NA,
                               ifelse(get(var_recod) != levels(with(recod_data, as.factor(get(var_recod))))[i], newvar,
                                      ifelse(input[[paste0("input_", i)]] != '' & input[[paste0("input_", i)]] != "NA" ,
                                             input[[paste0("input_", i)]],
                                             ifelse(input[[paste0("input_", i)]] == "NA", NA,
                                                    levels(with(recod_data, as.factor(get(var_recod))))[i])))))
    }
    # POUR LES NAs :
    recod_data <- recod_data %>%
      mutate(newvar = ifelse(is.na(get(var_recod)) == F, newvar,
                             ifelse(input[[paste0("input_", length(unique(with(recod_data, get(var_recod)))))]] != '',
                                    input[[paste0("input_", length(unique(with(recod_data, get(var_recod)))))]], NA)))
    
    
  } else { # Si pas de NA dans la variable :
    
    for (i in c(1: length(unique(with(recod_data, get(var_recod)))))) {
      recod_data[ with(recod_data, get(var_recod)) ==
                    levels(with(recod_data, as.factor(get(var_recod))))[i],]$newvar <-  ifelse(input[[paste0("input_", i)]] != '' & input[[paste0("input_", i)]] != "NA" ,
                                                                                                     input[[paste0("input_", i)]],
                                                                                                     ifelse(input[[paste0("input_", i)]] == "NA", NA,
                                                                                                            levels(with(recod_data, as.factor(get(var_recod))))[i]))
    } # Fin for
  } # Fin else
  
  
  ## GESTION DES NOMS DE VARIABLES
  old_name <- "newvar"
  new_name <<- var_recod_nom_apres()
  
  # Si le nom existe déjà, la variable est automatiquement renommée avec le suffixe _new
  if (new_name %in% names(df) && new_name != old_name) {
    showModal(modalDialog(
      title = "ATTENTION : Nom de variable existant",
      "Ce nom de variable est déjà utilisée, la nouvelle variable a été recodée avec un suffixe numérique.",
      easyClose = TRUE,
      footer = NULL))
  }
  
  # Vérification si le nouveau nom est déjà dans le dataframe
  i <- 1
  while (new_name %in% names(recod_data) && new_name != old_name) {
    # Si le nom est déjà présent on ajoute un numéro à la fin du nom
    i <- i + 1
    new_name <- paste0(new_name, i)
  }
  # Renommer les variables avec le nouveau nom
  names(recod_data)[names(recod_data) == old_name] <- new_name
  # Sauvegarde de la base pour être sur
  recod_data <<- recod_data
  
  # Modification du fichier en entrée
  v$data <- recod_data
  nomreac$nomcol <- colnames(v$data)
  
  
  
  
  # Table de la variable selectionnee
  # Info
  output$texte_table_avant <- renderPrint({
    cat("Variable à recoder: ", var_recod)
  })
  # Table
  output$table_recod_avant2 <- renderTable({
    hop <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(var_recod), useNA = "always")))))) %>% 
      slice(2)
    colnames(hop) <- c(with(v$data, names(table(get(var_recod)))), "Non Réponse", "Total")
    hop
  })
  # Affichage de la table avant recod
  output$aff_table_avant2 <- renderUI({
      tableOutput("table_recod_avant2")
  })
  
  
  
  # Table de la variable nouvelle
  # Info 
  output$texte_table_apres <- renderPrint({
    cat("Nouvelle variable: ", new_name)
  })
  # Table
  output$table_recod_apres <- renderTable({
    tab <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(new_name), useNA = "always")))))) %>%
      slice(2)
    
    colnames(tab) <- c(with(v$data, names(table(get(new_name)))), "Non Réponse", "Total")
    tab
  })
  # Affichage de la table recodée
  output$aff_table_apres <- renderUI({
    tableOutput("table_recod_apres")
  })
  
})








#### REORDONNER                     ----

# Quand on appuie sur le bouton :
observeEvent(input$ReorderOK, {
  
  validate(need(input$var_reord, ''))
  
  # On sauvegarde le nouvelle ordre des modalités (pas forcément utile)
  new_order <<- input$rank_list_basic
  
  # On change l'ordre directement dans le fichier en entrée
  v$data[,input$var_reord] <- with(v$data, factor(get(input$var_reord), levels = input$rank_list_basic))
  
  
  # On affiche une table de la variable réordonnée
  output$table_reord_apres <- renderTable({
    tab_reord <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(input$var_reord), useNA = "always")))))) %>% 
      slice(2)
    colnames(tab_reord) <- c(with(v$data, names(table(get(input$var_reord)))), "Non Réponse", "Total")
    tab_reord
  })
  output$aff_table_apres_reord <- renderUI({
    tableOutput("table_reord_apres")
  })
  
}) # Fin reordonne



###########################


