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
library(tidyr)

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


###################

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

