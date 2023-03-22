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
