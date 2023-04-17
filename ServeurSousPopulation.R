
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
  validate(need(input$target_upload,""))
  
  tryCatch({
  
  DT::datatable(filter_data(), extensions = 'Scroller', rownames = F, options = list(deferRender = F, 
                                                                                     dom = 't',
                                                                                     # columnDefs = list(list(className = 'dt-center',
                                                                                     #                        targets = 5)),
                                                                                     scrollY = 500,  #Hauteur de la table en pixel
                                                                                     scroller = TRUE, 
                                                                                     scrollX = T,
                                                                                     pageLength = 5))
  
  }, error = function(e) {
   
    datatable(
      data.frame(Erreur = c(paste("Error: ", e$message), 
                            "Vous pouvez changer le séparateur ou le type de fichier, dans l'import du fichier",
                            "Ne pas changer d'onglet, sinon risque de 'crash'")),
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      ), rownames = F
    )
    
  })
    
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


