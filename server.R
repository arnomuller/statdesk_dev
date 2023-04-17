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
          # On remplace les virgules par des points pour en faire des variables numeriques
          df <- data.frame(lapply(df, function(x) {gsub(",", ".", x)}))
          nomcol_data <<- colnames(df)
          
          return(df)
        } else {
          return(NULL)
        }
      } else if (input$datatype == ".csv"){ # Même processus pour les fichiers csv.
        if (substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == ".csv") {
          
          tryCatch({
          
          df <- read.csv(inFile$datapath, header = TRUE, sep = input$separator)
          df <- data.frame(lapply(df, function(x) {gsub(",", ".", x)}))
          nomcol_data <<- colnames(df)
          
          }, error = function(e) {
            return(NULL)
          })
          
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
    
    nomreac <- reactiveValues(nomcol = NULL)
    
    
    # 1ère modification : quand on importe les données v$data prend la valeur des
    # des données.
    
    
    # L'objet contenant les données se modifie quand : 
    # On importe les données
    observeEvent(input$target_upload, {
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
    })
    
    # On change le séparateur pour les csv
    observeEvent(input$separator, {
      validate(need(input$target_upload, 'Importer des données'))
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
    })
    
    # On change le type de données avec un message d'erreur si ce n'est pas le bon
    observeEvent(input$datatype, {
      validate(need(input$target_upload, 'Importer des données'))
      v$data <<- data()
      nomreac$nomcol <<- colnames(data())
      
      if(is.null(data()) == T ){
        
        showModal(modalDialog(
          title = "Attention",
          "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
          easyClose = TRUE,
          footer = NULL))
        
      }
    })
    
    
    # Changement de l'ordre des colonnes alphabetiquement
    observeEvent(input$col_alpha, {
      validate(need(input$target_upload, 'Importer des données'))
      

        if (input$col_alpha == TRUE) {
          #nomcol_data <<- order(colnames(data()))
          v$data <<- data() %>%
            select(order(colnames(data())))
          
          nomreac$nomcol <<- colnames(data() %>%
                                        select(order(colnames(data()))))
          
        } else{
          #nomcol_data <<- colnames(data())
          v$data <<- data()
          nomreac$nomcol <<- colnames(data())
        }

      
    })
    
    
    
   
    
    
    
    
    
    # On sauvegarde des objets réactifs qui renvoie les noms de variables.
    # Pour la base importée (= nomcol_data)
    nomcol_data_start <- reactive({
      nomcol_data
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
