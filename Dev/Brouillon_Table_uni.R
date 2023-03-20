### BROUILLON

input_var_table1 <- "ETAB"
input_var_ponder_tri <- "POIDS"

as.data.frame.matrix(with(filter_data2, questionr::freq(get(input$var_table1))))

addmargins(wtd.table(filter_data2$ETAB, weights = filter_data2$POIDS, useNA = "always"))

class(filter_data2$POIDS)



with(filter_data2, questionr::freq(get(input_var_table1)))





# Sans Ponder avec NA

as.data.frame(filter_data2 %>% 
  group_by(get(input_var_table1)) %>% 
  summarise(Effectif = n()) %>% 
  rename(Variable = 1) %>% 
  mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
  mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3)) %>% 
  rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
  mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
         `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))

?rows_insert()



# Sans Ponder sans NA
as.data.frame(filter_data2 %>% 
  group_by(get(input_var_table1)) %>% 
  summarize(Effectif = n()) %>% 
  rename(Variable = 1) %>% 
  filter(is.na(Variable) == F) %>% 
  mutate(`Pourcent (%)` =  round(100 * Effectif / sum(Effectif),3)) %>% 
  rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
  mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
         `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))

  



# PONDéRATION

# Avec Ponder avec NA  
as.data.frame(filter_data2 %>% 
  group_by(get(input_var_table1)) %>% 
  summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),3)) %>% 
  rename(Variable = 1) %>% 
  mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
  mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>% 
  rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
  mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
         `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))



# Avec Ponder sans NA
as.data.frame(filter_data2 %>% 
                group_by(get(input_var_table1)) %>% 
                summarise(Effectif = round(sum(as.numeric(get(input_var_ponder_tri))),3)) %>% 
                rename(Variable = 1) %>% 
                filter(is.na(Variable) == F) %>% 
                mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>% 
                mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>% 
                rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>% 
                mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                       `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))









