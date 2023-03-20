#############
#### Server : Apprentis chercheurs
#############
# Ce code programme le server du programme shiny pour faire des 
# analyses exporatoires dans le cadres du projet pour les apprentis chercheurs




### Server ----

server = shinyServer(
  function(input, output, session) {
    
    
    
    # Import des données ----
    
    data <- reactive({
      inFile <<- input$target_upload
      if (is.null(inFile)) {
        return(NULL)
      }
      
      if (input$datatype == ".xlsx"){
        
        # Si ce qu'on importe n'est pas un xlsx, il ne se passe rien
        if (substr(inFile$datapath, nchar(inFile$datapath)-4, nchar(inFile$datapath)) == ".xlsx") {
          df <- openxlsx::read.xlsx(inFile$datapath,1)
          
          # Si une variable s'appelle ID, on garde la première qu'on laisse devant
          # et on classe les autres par ordre alphabétique
          # On pourrait aussi mettre un bouton qui permet de classer ou non.
          
          to_match <- c("ID", "IDENT", "INDIV","id", "ident","indiv")
          match_id <- unique(grep(paste(to_match ,collapse="|"), 
                                  colnames(df)))[1]

          df <- df %>% 
            select(all_of(match_id), order(colnames(df)))
          
          # On sauvegarde les noms de colonnes dans un objet
          nomcol_data <<- colnames(df)
          # data() prend la valeur df
          return(df)
        } else {
          return(NULL)
        }
      }
      
      # Même processus pour les fichiers csv.
      if (input$datatype == ".csv"){
        if (substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == ".csv") {
        df <- read.csv2(inFile$datapath, header = TRUE, sep = input$separator)
        
        to_match <- c("ID", "IDENT", "INDIV","id", "ident","indiv")
        match_id <- unique(grep(paste(to_match ,collapse="|"), 
                                colnames(df)))[1]
        
        df <- df %>% 
          select(all_of(match_id), order(colnames(df)))
        
        nomcol_data <<- colnames(df)
        return(df)
        } else {
          return(NULL)
        }
      }
    }) # Fin Import
    
    
    # Warning si pas le bon format de données
    observeEvent(input$target_upload, {
      if(is.null(data()) == T ){
        
        showModal(modalDialog(
          title = "Attention",
          "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
          easyClose = TRUE,
          footer = NULL))
        
      }
    })
    
    
    # On crée un objet "reactiveValues" qui est une sorte d'objet reactif, qui va pouvoir
    # contenir d'autres objets comme des dataframes.
    # L'avantage c'est qu'on peut le modifier dans différents observeEvent
    # au contraire du simple reactive, qui ne peut être crée et modifié que dans un
    # unique bloc de code.
    
    v <- reactiveValues(data = NULL)
    
    # 1ère modification : quand on importe les données v$data prend la valeur des
    # des données.
    
    observeEvent(input$target_upload, {
      v$data <<- data()
    })
    
    
    # On sauvegarde des objets réactifs qui renvoie les noms de variables.
    # Pour la base importée (= nomcol_data)
    nomcol_data_start <- reactive({
      colnames(data())
    })
    
    # Idem pour la base qui sera modifiée (utilisé dans les pickers)
    nomcol_data_reac <- reactive({
      colnames(v$data)
    })
    
    
    # Dimension de la table en entrée      
    n_col_start <- reactive({ ncol(data()) })
    n_row_start <- reactive({ nrow(data()) })
    
    # Affichage dimension table
    output$info_row <- renderPrint({
      validate(need(data(), ""))
      cat("Nombre d'individus :", isolate(n_row_start()))
    })
    output$info_col <- renderPrint({
      validate(need(data(), ""))
      cat("Nombre de variables :", isolate(n_col_start()))
    })
    
    

    
    # SUITE DU SERVER ----     
    
    # Pour la suite, on continue le server dans des scripts différents pour chacun des onglets.
    
    
    # 1) Variable ----  
    # Permet de modifier les variables
    source('ServeurVariable.R', local = TRUE)
    
    
    # 2) Sous Population ----  
    # Permet de faire une sous-population
    source('ServeurSousPopulation.R', local = TRUE)
    
    
    # 3) Table ----  
    # Permet d'observer les variables dans des tables
    source('ServeurTable.R', local = TRUE)
    
    
    # 4) Graphique ----  
    # Permet d'observer les variables dans des graphiques
    source('ServeurGraphique.R', local = TRUE)
    

    
  })
