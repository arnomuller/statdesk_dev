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


