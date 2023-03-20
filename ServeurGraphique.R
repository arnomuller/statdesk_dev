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
                  choices= c("Univarié : Bâtons (effectifs)" = "barplot_eff_uni", 
                             "Univarié : Bâtons (proportions)" = "barplot_freq_uni", 
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
        colourInput("col", "Couleur barres", "#076fa2",showColour = "background")
      ),
      fluidRow(
        colourInput("col_label", "Couleur label", "white",showColour = "background")
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
      
      conditionalPanel(condition="input.choix_color == 'Manuel'",
                       fluidRow(
                         # colourInput("col", NULL, "blue")
                         
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
          mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1)) %>% 
          arrange(desc(Somme))
        
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
        p <- ggplot(p_uni_na) +
          geom_col(aes(Somme, ordre_moda_graph), fill = input$col, width = input$width_bar) + # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni_na$Somme)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni_na$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          )+ 
          geom_text( 
            data = subset(p_uni_na, Somme < max(p_uni_na$Somme)/10),
            aes(Somme, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni_na, Somme >= max(p_uni_na$Somme)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
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
          mutate(Pct = Somme / sum(Somme)*100)%>% 
          arrange(desc(Somme))
        
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
        
        p <- ggplot(p_uni) +
          geom_col(aes(Somme, ordre_moda_graph), fill = input$col, width = input$width_bar) + # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni$Somme)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          ) + 
          geom_text(
            data = subset(p_uni, Somme < max(p_uni$Somme)/10),
            aes(Somme, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni, Somme >= max(p_uni$Somme)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
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
          mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1)) %>% 
          arrange(desc(Somme))
        
        
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
        p <- ggplot(p_uni_ponder_na) +
          geom_col(aes(Somme, ordre_moda_graph), fill = input$col, width = input$width_bar) + # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni_ponder_na$Somme)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni_ponder_na$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          )+ 
          geom_text(
            data = subset(p_uni_ponder_na, Somme < max(p_uni_ponder_na$Somme)/10),
            aes(Somme, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni_ponder_na, Somme >= max(p_uni_ponder_na$Somme)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
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
          mutate(Pct = Somme / sum(Somme)*100)%>% 
          # Prise en compte des NAs
          arrange(desc(Somme))
        
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
        p <- ggplot(p_uni_ponder) +
          geom_col(aes(Somme, ordre_moda_graph), fill = input$col, width = input$width_bar)+ # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni_ponder$Somme)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni_ponder$Somme)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          ) + 
          geom_text(
            data = subset(p_uni_ponder, Somme < max(p_uni_ponder$Somme)/10),
            aes(Somme, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni_ponder, Somme >= max(p_uni_ponder$Somme)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
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
          mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1)) %>% 
          arrange(desc(Somme))
        
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
        p <- ggplot(p_uni_na) +
          geom_col(aes(Pct, ordre_moda_graph), fill = input$col, width = input$width_bar) + # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni_na$Pct)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni_na$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          )+ 
          geom_text(
            data = subset(p_uni_na, Pct < max(p_uni_na$Pct)/10),
            aes(Pct, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni_na, Pct >= max(p_uni_na$Pct)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
          labs(
            title = plot_titre_reac(), # input$plot_titre
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
          )
        
        p
        
        
        
      }else{ # ELSE SANS NA
        
        p_uni <- filter_data() %>% 
          group_by(get(input$var_plot1)) %>% #input$var_plot1
          summarise(Somme = n()) %>% 
          rename(Var1 = 1) %>% 
          filter(is.na(Var1) == FALSE) %>% 
          mutate(Pct = Somme / sum(Somme)*100)%>% 
          arrange(desc(Somme))
        
        
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
        
        p <- ggplot(p_uni) +
          geom_col(aes(Pct, ordre_moda_graph), fill = input$col, width = input$width_bar) + # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni$Pct)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          ) + 
          geom_text(
            data = subset(p_uni, Pct < max(p_uni$Pct)/10),
            aes(Pct, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni, Pct >= max(p_uni$Pct)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
          labs(
            title = plot_titre_reac(), # input$plot_titre
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
          mutate(Var1 = ifelse(is.na(Var1), "Val.Manq", Var1)) %>% 
          arrange(desc(Somme))
        
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
        p <- ggplot(p_uni_ponder_na) +
          geom_col(aes(Pct, ordre_moda_graph), fill = input$col, width = input$width_bar) + # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni_ponder_na$Pct)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni_ponder_na$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          )+ 
          geom_text(
            data = subset(p_uni_ponder_na, Pct < max(p_uni_ponder_na$Pct)/10),
            aes(Pct, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni_ponder_na, Pct >= max(p_uni_ponder_na$Pct)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
          labs(
            title = plot_titre_reac(), # input$plot_titre
            subtitle = "Proportions pondérées"
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
          mutate(Pct = Somme / sum(Somme)*100)%>% 
          # Prise en compte des NAs
          arrange(desc(Somme))
        
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
        p <- ggplot(p_uni_ponder) +
          geom_col(aes(Pct, ordre_moda_graph), fill = input$col, width = input$width_bar)+ # input$col input$width_bar
          
          scale_x_continuous(
            limits = c(0,ceiling(max(p_uni_ponder$Pct)/input$n_break) * input$n_break),
            breaks = seq(0, ceiling(max(p_uni_ponder$Pct)/input$n_break) * input$n_break, by = input$n_break), # input$n_break
            expand = c(0, 0), # The horizontal axis does not extend to either side
            position = "top"  # Bouger echelle
          ) +
          
          scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
          theme(
            # Set background color to white
            panel.background = element_rect(fill = "white"),
            # Set the color and the width of the grid lines for the horizontal axis
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            # Remove tick marks by setting their length to 0
            axis.ticks.length = unit(0, "mm"),
            # Remove the title for both axes
            axis.title = element_blank(),
            # Only left line of the vertical axis is painted in black
            axis.line.y.left = element_line(color = "#202020"),
            # Remove labels from the vertical axis
            axis.text.y = element_blank(),
            # But customize labels for the horizontal axis
            axis.text.x = element_text( size = input$taille_axe) # input$taille_axe
          ) + 
          geom_text(
            data = subset(p_uni_ponder, Pct < max(p_uni_ponder$Pct)/10),
            aes(Pct, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col, # input$col
            size = input$taille_label # input$taille_label
          ) + 
          geom_text(
            data = subset(p_uni_ponder, Pct >= max(p_uni_ponder$Pct)/10),
            aes(0, y = get("Var1"), label = get("Var1")),
            hjust = 0,
            nudge_x = 0.3,
            colour = input$col_label, # input$col_label
            size = input$taille_label # input$taille_label
          ) +
          labs(
            title = plot_titre_reac(), # input$plot_titre
            subtitle = "Proportions pondérées"
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