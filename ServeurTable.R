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
                  choices= c("Effectifs univariés" = "eff_uni_ACM", 
                             "Effectif bivariés" = "eff_ACM", 
                             "% Ligne" = "pct_lign_ACM", 
                             "% Colonne" = "pct_col_ACM"))),
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
    )
  )
  
  
})





### CREATION DES TABLES        ----




## Effectifs univariés         ----
output$eff_uni_ACM <- renderTable({
  validate(need(input$var_table1,'Choisir une 1ère variable'))
  
  # Si pas de pondération
  if(input$checkbox_ponder == FALSE){
    export_tab <<- as.data.frame.matrix(with(filter_data(), questionr::freq(get(input$var_table1))))
    export_tab
    # Si pondération  
  }else{
    
    validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
    
    export_tab <<- as.data.frame.matrix(
      as.data.frame(
        with(filter_data(), addmargins(questionr::wtd.table(get(input$var_table1), 
                                                            weights = as.numeric(as.character(get(input$var_ponder_tri))), 
                                                            na.rm = FALSE)))) %>% 
        mutate(Pourcent = round(100*with(filter_data(), 
                                         addmargins(
                                           prop.table(
                                             questionr::wtd.table(get(input$var_table1), na.rm = FALSE, 
                                                                  weights = as.numeric(as.character(get(input$var_ponder_tri))))))),2),
               Var1 = ifelse(is.na(Var1), "Valeurs Manquantes", as.character(Var1))) %>% 
        tibble::column_to_rownames(var = "Var1"))
    export_tab
    
  }
  
}, include.rownames = T)



## Effectif bivariées          ----

output$eff_ACM <- renderTable({
  validate(need(input$var_table1,'Choisir une 1ère variable'),
           need(input$var_table2,'Choisir une 2ème variable'))
  
  if(input$var_table3 == ""){
    
    if(input$checkbox_ponder == FALSE){
      
      export_tab <<- as.data.frame.matrix(with(filter_data(), addmargins(table(get(input$var_table1), get(input$var_table2), useNA = "always"))))
      export_tab 
      
    }else{
      
      validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
      export_tab <<- as.data.frame.matrix(
        with(filter_data(), 
             addmargins(
               questionr::wtd.table(get(input$var_table1), get(input$var_table2), 
                                    na.rm = FALSE, 
                                    weights = as.numeric(as.character(get(input$var_ponder_tri))))
             )))
      export_tab 
      
    }
  } else{
    validate(need(input$var_table3,'Choisir une 3ème variable'))
    
    if(input$checkbox_ponder == FALSE){
      
      export_tab <<- as.data.frame.matrix(with(filter_data()[which(filter_data()[input$var_table3] == input$var_table3_moda),],
                                               addmargins(table(get(input$var_table1), get(input$var_table2), useNA = "always"))))
      export_tab
      
    } else {
      
      validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
      export_tab <<- as.data.frame.matrix(
        with(filter_data()[which(filter_data()[input$var_table3] == input$var_table3_moda),], 
             addmargins(
               questionr::wtd.table(get(input$var_table1), get(input$var_table2), 
                                    na.rm = FALSE, 
                                    weights = as.numeric(as.character(get(input$var_ponder_tri))))
             )))
      export_tab
      
    }
    
    
  }
  
  
}, include.rownames = T)




## Pourcentage Ligne           ----


output$pct_lign_ACM <- renderTable({
  validate(need(input$var_table1,'Choisir une 1ère variable'),
           need(input$var_table2,'Choisir une 2ème variable'))
  
  if(input$var_table3 == ""){
    
    if(input$checkbox_ponder == FALSE){
      
      export_tab <<- as.data.frame.matrix(with(filter_data(), questionr::lprop(table(get(input$var_table1), get(input$var_table2)))))
      export_tab
      
    }else{
      
      validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
      export_tab <<- as.data.frame.matrix(round(
        with(filter_data(), 
             addmargins(
               prop.table(
                 addmargins(
                   questionr::wtd.table(get(input$var_table1), get(input$var_table2), 
                                        na.rm = FALSE, 
                                        weights = as.numeric(as.character(get(input$var_ponder_tri)))),
                   1), # addmargins ligne 
                 1), # Proptable ligne
               2)) # addmargins colonnes
        *100, # Pourcent
        2)) # Round
      export_tab
      
    }
  } else{
    validate(need(input$var_table3,'Choisir une 3ème variable'))
    
    if(input$checkbox_ponder == FALSE){
      
      export_tab <<- as.data.frame.matrix(with(filter_data()[which(filter_data()[input$var_table3] == input$var_table3_moda),],
                                               questionr::lprop(table(get(input$var_table1), get(input$var_table2)))))
      export_tab
      
    } else {
      
      validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
      
      
      export_tab <<- as.data.frame.matrix(round(
        with(filter_data()[which(filter_data()[input$var_table3] == input$var_table3_moda),], 
             addmargins(
               prop.table(
                 addmargins(
                   questionr::wtd.table(get(input$var_table1), get(input$var_table2), 
                                        na.rm = FALSE, 
                                        weights = as.numeric(as.character(get(input$var_ponder_tri)))),
                   1), # addmargins ligne 
                 1), # Proptable ligne
               2)) # addmargins colonnes
        *100, # Pourcent
        2)) # Round
      
      export_tab
    }
    
    
  }
  
  
}, include.rownames = T)






## Pourcentage Colonne         ----
output$pct_col_ACM <- renderTable({
  validate(need(input$var_table1,'Choisir une 1ère variable'),
           need(input$var_table2,'Choisir une 2ème variable'))
  
  if(input$var_table3 == ""){
    
    if(input$checkbox_ponder == FALSE){
      
      export_tab <<- as.data.frame.matrix(with(filter_data(), questionr::cprop(table(get(input$var_table1), get(input$var_table2)))))
      export_tab 
      
    }else{
      
      validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
      export_tab <<- as.data.frame.matrix(round(
        with(filter_data(), 
             addmargins(
               prop.table(
                 addmargins(
                   questionr::wtd.table(get(input$var_table1), get(input$var_table2), 
                                        na.rm = FALSE, 
                                        weights = as.numeric(as.character(get(input$var_ponder_tri)))),
                   2), # addmargins colonnes
                 2), # Proptable colonnes
               1)) # addmargins ligne 
        *100, # Pourcent
        2))  # Round
      export_tab
      
    }
  } else{
    validate(need(input$var_table3,'Choisir une 3ème variable'))
    
    if(input$checkbox_ponder == FALSE){
      
      export_tab <<- as.data.frame.matrix(with(filter_data()[which(filter_data()[input$var_table3] == input$var_table3_moda),],
                                               questionr::cprop(table(get(input$var_table1), get(input$var_table2)))))
      export_tab 
      
    } else {
      
      validate(need(input$var_ponder_tri,'Choisir une variable de pondération'))
      
      
      export_tab <<- as.data.frame.matrix(round(
        with(filter_data()[which(filter_data()[input$var_table3] == input$var_table3_moda),], 
             addmargins(
               prop.table(
                 addmargins(
                   questionr::wtd.table(get(input$var_table1), get(input$var_table2), 
                             na.rm = FALSE, 
                             weights = as.numeric(as.character(get(input$var_ponder_tri)))),
                   2), # addmargins colonnes
                 2), # Proptable colonnes
               1)) # addmargins ligne 
        *100, # Pourcent
        2))  # Round
      export_tab 
      
    }
    
    
  }
  
  
}, include.rownames = T)





#### Affichage de la table     -----

observeEvent(input$var_table1, { 
  output$affichage_table <- renderUI({
    tableOutput(input$choix_table)
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
    openxlsx::write.xlsx(export_tab,file)
  }
)
