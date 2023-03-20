### BROUILLON

input_var_table2 <- "ETAB"
input_var_table1 <- "FRERE"
input_var_table3 <- "GENRE"
input_var_ponder_tri <- "POIDS_norm"
input_var_table3_moda <- "Une fille"

as.data.frame.matrix(with(filter_data2, questionr::freq(get(input$var_table1))))

addmargins(wtd.table(filter_data2$ETAB, weights = filter_data2$POIDS, useNA = "always"))

table(filter_data2$GENRE)



with(filter_data2, questionr::freq(get(input_var_table1)))

table(filter_data2$FRERE, useNA = "always")


# Sans Ponder avec NA

as.data.frame(filter_data2 %>% 
  group_by(get(input_var_table1), get(input_var_table2)) %>% 
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
  mutate(Pct_l = round(ifelse(Variables == "Total", 100*sum(Effectif, na.rm = T)/nrow(filter_data2), Pct_l),2))%>% 
  #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
  ### Et pour ponder il faut la somme des pondérations
  select(Variables, Variable2, Pct_l) %>% 
  arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
          match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
  pivot_wider(values_from = Pct_l,
              names_from = Variable2)%>% 
  mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
         Total = ifelse(Variables == "Ensemble", 100, Total)))




#############
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
                                nrow(filter(filter_data2, get(input_var_table3) == input_var_table3_moda)), 
                              Pct_l),2))%>% 
  
  select(Variables, Variable2, Pct_l) %>% 
  arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
          match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
  pivot_wider(values_from = Pct_l,
              names_from = Variable2)%>% 
  mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
         Total = ifelse(Variables == "Ensemble", 100, Total)))

#################





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
                                nrow(filter_data2[is.na(with(filter_data2,get(input_var_table1))) == F & 
                                                    is.na(with(filter_data2,get(input_var_table2)))== F,]), 
                              Pct_l),2))%>% 
  
  select(Variables, Variable2, Pct_l) %>% 
  arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
          match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
  pivot_wider(values_from = Pct_l,
              names_from = Variable2) %>% 
  mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
         Total = ifelse(Variables == "Ensemble", 100, Total)))


#######
# Sans Ponder sans NA AVEC 3 VARIABLE 
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
                              nrow(filter(filter_data2, 
                                          get(input_var_table3) == input_var_table3_moda &
                                            is.na(get(input_var_table1)) == F &
                                            is.na(get(input_var_table2)) == F )),
                              Pct_l),2))%>% 
  
  select(Variables, Variable2, Pct_l) %>% 
  arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
          match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
  pivot_wider(values_from = Pct_l,
              names_from = Variable2) %>% 
  mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
         Total = ifelse(Variables == "Ensemble", 100, Total)))



##############









#############
## AVEC PONDERATION


# Sans Ponder avec NA

as.data.frame(filter_data2 %>% 
                group_by(get(input_var_table1), get(input_var_table2)) %>% 
                summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),1)) %>% 
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
                mutate(Pct_l = round(ifelse(Variables == "Total", 100*sum(Effectif, na.rm = T)/sum(with(filter_data2, as.numeric(get(input_var_ponder_tri)))), Pct_l),2))%>% 
                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                ### Et pour ponder il faut la somme des pondérations
                select(Variables, Variable2, Pct_l) %>% 
                arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                        match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                pivot_wider(values_from = Pct_l,
                            names_from = Variable2)%>% 
                mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
                       Total = ifelse(Variables == "Ensemble", 100, Total)))




#############
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
                                              nrow(filter(filter_data2, get(input_var_table3) == input_var_table3_moda)), 
                                            Pct_l),2))%>% 
                
                select(Variables, Variable2, Pct_l) %>% 
                arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                        match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                pivot_wider(values_from = Pct_l,
                            names_from = Variable2)%>% 
                mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
                       Total = ifelse(Variables == "Ensemble", 100, Total)))

#################





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
                                              nrow(filter_data2[is.na(with(filter_data2,get(input_var_table1))) == F & 
                                                                  is.na(with(filter_data2,get(input_var_table2)))== F,]), 
                                            Pct_l),2))%>% 
                
                select(Variables, Variable2, Pct_l) %>% 
                arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                        match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                pivot_wider(values_from = Pct_l,
                            names_from = Variable2) %>% 
                mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
                       Total = ifelse(Variables == "Ensemble", 100, Total)))


#######
# Sans Ponder sans NA AVEC 3 VARIABLE 
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
                                              nrow(filter(filter_data2, 
                                                          get(input_var_table3) == input_var_table3_moda &
                                                            is.na(get(input_var_table1)) == F &
                                                            is.na(get(input_var_table2)) == F )),
                                            Pct_l),2))%>% 
                
                select(Variables, Variable2, Pct_l) %>% 
                arrange(match(Variables, c(levels(as.factor(with(filter_data2,get(input_var_table1)))), "Val.Manq.", "Total")),
                        match(Variable2, c(levels(as.factor(with(filter_data2,get(input_var_table2)))), "Val.Manq.", "Total"))) %>% 
                pivot_wider(values_from = Pct_l,
                            names_from = Variable2) %>% 
                mutate(Variables = ifelse(Variables == "Total", "Ensemble", Variables),
                       Total = ifelse(Variables == "Ensemble", 100, Total)))












#OLD
  ###################



# PONDéRATION

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

