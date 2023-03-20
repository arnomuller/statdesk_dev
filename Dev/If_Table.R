

# Possible de reorder les modalités des graphiques avec :
# reorder(Var1, Pct)

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
                        summarise(Effectif = round(sum(get(input$var_ponder_tri)),3)) %>% 
                        rename(Variable = 1) %>% 
                        mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
                        mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>% 
                        rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
                        mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                               `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))
        
        
      }else{ # ELSE SANS NA
        
        # Avec Ponder sans NA
        as.data.frame(filter_data() %>% 
                        group_by(get(input$var_table1)) %>% 
                        summarise(Effectif = round(sum(get(input$var_ponder_tri)),3)) %>% 
                        rename(Variable = 1) %>% 
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
          
          as.data.frame(filter_data2 %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                          pivot_wider(values_from = Effectif,
                                      names_from = Variable2)) 
          
          
        }else{ # ELSE SANS NA
          
          # Sans Ponder sans NA
          
          as.data.frame(filter_data2 %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Total"))) %>% 
                          mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                          pivot_wider(values_from = Effectif,
                                      names_from = Variable2) )
          
          
        }
        
      }else{ # ELSE AVEC PONDER
        
        validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
        
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          # Avec Ponder avec NA  
          
          
          as.data.frame(filter_data2 %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
                          summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),1)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                          pivot_wider(values_from = Effectif,
                                      names_from = Variable2)) 
          
          
        }else{ # ELSE SANS NA
          
          # Avec Ponder sans NA
          
          
          
          as.data.frame(filter_data2 %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
                          summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),1)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Total"))) %>% 
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
          
          as.data.frame(filter_data2 %>% 
                          filter(get(input_var_table3) == input_var_table3_moda) %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                          pivot_wider(values_from = Effectif,
                                      names_from = Variable2)) 
          
          
        }else{ # ELSE SANS NA
          
          # Sans Ponder sans NA AVEC 3 VARIABLES
          
          as.data.frame(filter_data2 %>% 
                          filter(get(input_var_table3) == input_var_table3_moda) %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Total"))) %>% 
                          mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>% 
                          pivot_wider(values_from = Effectif,
                                      names_from = Variable2) )
          
          
        }
        
      }else{ # ELSE AVEC PONDER
        
        validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
        
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          # Avec Ponder avec NA  AVEC 3 VARIABLE
          
          
          as.data.frame(filter_data2 %>% 
                          filter(get(input_var_table3) == input_var_table3_moda) %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
                          summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),1)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                          pivot_wider(values_from = Effectif,
                                      names_from = Variable2)) 
          
          
        }else{ # ELSE SANS NA
          
          # Avec Ponder sans NA  AVEC 3 VARIABLE
          
          
          as.data.frame(filter_data2 %>% 
                          filter(get(input_var_table3) == input_var_table3_moda) %>% 
                          group_by(get(input_var_table1), get(input_var_table2)) %>% 
                          summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),1)) %>% 
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
                          arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Total")),
                                  match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Total"))) %>% 
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
          
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
        }
        
      }else{ # ELSE AVEC PONDER
        
        validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
        
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
          
          
        }
      }
      
    } else { # Si 3 variables
      
      validate(need(input$var_table3,'Choisir une 3ème variable'))
      
      # SANS PONDERATION
      if (input$checkbox_ponder == FALSE) {
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
        }
        
      }else{ # ELSE AVEC PONDER
        
        validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
        
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
          
          
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
          
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
        }
        
      }else{ # ELSE AVEC PONDER
        
        validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
        
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
          
          
        }
      }
      
    } else { # Si 3 variables
      
      validate(need(input$var_table3,'Choisir une 3ème variable'))
      
      # SANS PONDERATION
      if (input$checkbox_ponder == FALSE) {
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
        }
        
      }else{ # ELSE AVEC PONDER
        
        validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))
        
        # AVEC NA
        if (input$checkbox_na == TRUE) {
          
          
          
          
        }else{ # ELSE SANS NA
          
          
          
          
          
          
        }
      }
      
      
    }
  }
  
  
})






