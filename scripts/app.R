library(shiny)
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)
library(timevis)
library(lubridate)  
library(here)
library(shinyWidgets)
library(webshot)
library(webshot2)



# Ne pas toucher
ui <- fluidPage(
  titlePanel("Frise chronologique par acteurs"),
  
  # Onglets principaux
  tabsetPanel(
    tabPanel("Visualisation",
             fileInput("file", "Importer un fichier Excel", accept = ".xlsx"),
             uiOutput("acteur_ui"),
             timevisOutput("timeline"),
             # Ajout de la zone de texte pour afficher les détails de l'élément cliqué
             br(),
             h4("Détails de l'élément sélectionné :"),
             uiOutput("clicked_content"),
             div(style = "position: fixed; bottom: 10px; width: 100%; text-align: center; z-index: 1000;",
                 downloadButton("export_png", "Exporter Timeline (PNG)", class = "btn-success")
             )
    ),
    tabPanel("Documentation",
             tags$div(
               style = "padding: 15px;",
               tags$h3("Explication du code"),
               tags$p("Cette application permet de :"),
               tags$ul(
                 tags$li("Importer un fichier Excel avec des périodes exprimées en texte:"),
                 tags$ul(
                   tags$li("La première colonne est celle des dates : juin-juillet 1990, du 12 au 29 mai 2010, années 1990..."),
                   tags$li("Les autres colonnes sont celles des acteurs clés : Simon, Leïla, Désiré Doué..."),
                   tags$li("Le contenu des cellules est tronqué pour l'affichage en frise, mais disponible en cliquant sur les entrées")
                 ),
                 tags$li("Parser ces périodes en dates de début et de fin avec la fonction parser_periode."),
                 tags$li("Afficher une frise chronologique interactive par acteurs en utilisant le package", 
                         tags$a(href = "https://example.com/guide", 
                                target = "_blank",  # pour ouvrir dans un nouvel onglet
                                "timevis"),
                         "de Dean Attali"),
                 tags$li("Filtrer les acteurs grâce à un selectInput dynamique."),
                 tags$li("Afficher les données dans une timeline avec timevis.")
               ),
               tags$p("Librairies utilisées : ",
                      tags$a(href = "https://cran.r-project.org/package=readxl", "readxl"), ", ",
                      tags$a(href = "https://cran.r-project.org/package=lubridate", "lubridate"), ", ",
                      tags$a(href = "https://cran.r-project.org/package=dplyr", "dplyr"), ", ",
                      tags$a(href = "https://cran.r-project.org/package=tidyr", "tidyr"), ", ",
                      tags$a(href = "https://cran.r-project.org/package=timevis", "timevis"), ", ",
                      tags$a(href = "https://cran.r-project.org/package=shinyWidgets", "shinyWidgets"), "."
               ),
               tags$p(
                 "Cette application est mise à disposition sous la licence ",
                 tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/", target = "_blank", "Creative Commons BY-NC 4.0"),
                 ". Vous êtes libre de la réutiliser, de la modifier et de la partager à condition :"
               ),
               tags$ul(
                 tags$li("de mentionner l’auteur original ;"),
                 tags$li("de ne pas en faire un usage commercial ;"),
                 tags$li("de partager toute adaptation sous les mêmes conditions.")
               ),
               tags$p("Pour toute question ou collaboration, n'hésitez pas à me contacter ou à visiter mon", 
                      tags$a(href = "https://github.com/sim-jean", target = "_blank", "GitHub")
               ),
               tags$p("Work in progress : "),
               tags$ul(
                 tags$li("Changer le formatage du texte pour avoir uniquement un titre affiché sur la frise"),
                 tags$li("Changer le formatage du texte pour avoir la légende dans l'espace de texte dédié"),
                 tags$li("Mettre les plages de dates d'une autre couleur")
               )
             )
    ),
    
    # Signature en bas, toujours visible
    tags$footer(
      style = "
      position: fixed;
      bottom: 5px;
      right: 10px;
      font-size: 0.8em;
      color: #666;",
      "Codé par Simzer — ",
      tags$a(href = "https://github.com/sim-jean.github.io", target = "_blank", "GitHub")
    )
  )
)

  
  
  
  
  
  # Sert à déclencher la partie interactive
  server4 <- function(input, output, session) {
    
    # Reactive expression qui charge les données à partir du fichier uploadé
    donnees <- reactive({
      # On s'assure qu'un fichier a bien été uploadé avant de continuer
      req(input$file)
      
      # Lecture du fichier Excel uploadé dans un dataframe
      df <- readxl::read_excel(input$file$datapath)
      
      # Fonction interne qui convertit des textes variés représentant des périodes
      # en deux dates au format Date (start et end)
      parser_periode <- function(texte) {
        
        # Si le texte est trop long ou contient des caractères suspects, on renvoie NA pour les dates
        if (stringr::str_length(texte) > 30 || stringr::str_detect(texte, "[\\?\\!\\;\\:\\…]") || is.na(texte)) {
          return(tibble::tibble(start = NA_Date_, end = NA_Date_))
        }
        
        # Normalisation du texte : suppression des espaces inutiles, passage en minuscules
        texte <- stringr::str_trim(stringr::str_to_lower(texte))
        
        # Table de correspondance mois français -> numéro du mois
        mois_fr <- c("janvier"=1, "février"=2, "mars"=3, "avril"=4, "mai"=5, "juin"=6,
                     "juillet"=7, "août"=8, "septembre"=9, "octobre"=10, "novembre"=11, "décembre"=12)
        
        # Cas 1 : plage d'années format "1934-1945"
        if (stringr::str_detect(texte, "^\\d{4}\\s*-\\s*\\d{4}$")) {
          an <- as.numeric(stringr::str_extract_all(texte, "\\d{4}")[[1]])
          start <- lubridate::ymd(sprintf("%d-01-01", an[1]))
          end <- lubridate::ymd(sprintf("%d-12-31", an[2]))
          return(tibble::tibble(start = start, end = end))
        }
        
        # Cas 2 : expression "années 1930" ou "annees 1930"
        if (stringr::str_detect(texte, "années|annees")){
          year <- as.numeric(stringr::str_extract(texte, "\\d{4}"))
          return(tibble::tibble(start = lubridate::ymd(sprintf("%d-01-01", year)),
                                end = lubridate::ymd(sprintf("%d-12-31", year+9))))
        }
        
        # Cas 3 : plage avec jours précis du même mois "du 17 juillet au 23 juillet 1934"
        if (stringr::str_detect(texte, "^du \\d{1,2} au \\d{1,2} [[:alpha:]]+ \\d{4}$")) {
          jour1 <- stringr::str_extract(texte, "(?<=du )\\d{1,2}")
          jour2 <- stringr::str_extract(texte, "(?<=au )\\d{1,2}")
          mois  <- stringr::str_extract(texte, paste(names(mois_fr), collapse="|"))
          annee <- stringr::str_extract(texte, "\\d{4}")
          mois_num <- mois_fr[mois]
          start <- lubridate::ymd(sprintf("%s-%02d-%02d", annee, mois_num, as.integer(jour1)))
          end   <- lubridate::ymd(sprintf("%s-%02d-%02d", annee, mois_num, as.integer(jour2)))
          return(tibble::tibble(start = start, end = end))
        }
        
        # Cas 4 : deux jours dans le même mois "17 juillet et 23 juillet 1934"
        if (stringr::str_detect(texte, "^\\d{1,2} et \\d{1,2} [[:alpha:]]+ \\d{4}$")) {
          jours <- as.integer(stringr::str_extract_all(texte, "\\d{1,2}")[[1]])
          mois  <- stringr::str_extract(texte, paste(names(mois_fr), collapse="|"))
          annee <- stringr::str_extract(texte, "\\d{4}")
          mois_num <- mois_fr[mois]
          start <- lubridate::ymd(sprintf("%s-%02d-%02d", annee, mois_num, jours[1]))
          end   <- lubridate::ymd(sprintf("%s-%02d-%02d", annee, mois_num, jours[2]))
          return(tibble::tibble(start = start, end = end))
        }
        
        # Cas 5 : plage de mois dans une même année "janvier - mars 1934"
        if (stringr::str_detect(texte, paste0("(", paste(names(mois_fr), collapse="|"), ")\\s*-\\s*(",
                                              paste(names(mois_fr), collapse="|"), ")\\s+\\d{4}$"))) {
          mois1 <- stringr::str_match(texte, paste0("^(", paste(names(mois_fr), collapse="|"), ")"))[,2]
          mois2 <- stringr::str_match(texte, paste0("-\\s*(", paste(names(mois_fr), collapse="|"), ")"))[,2]
          annee <- as.integer(stringr::str_extract(texte, "\\d{4}"))
          mois1_num <- mois_fr[mois1]
          mois2_num <- mois_fr[mois2]
          start <- lubridate::ymd(sprintf("%d-%02d-01", annee, mois1_num))
          # Fin du mois 2 (premier jour mois 2 + 1 mois - 1 jour)
          end <- lubridate::ymd(sprintf("%d-%02d-01", annee, mois2_num)) + months(1) - days(1)
          return(tibble::tibble(start = start, end = end))
        }
        
        # Cas 6 : année seule "1934"
        if (stringr::str_detect(texte, "^\\d{4}$")) {
          year <- as.numeric(texte)
          return(tibble::tibble(start = lubridate::ymd(sprintf("%d-01-01", year)),
                                end = lubridate::ymd(sprintf("%d-12-31", year))))
        }
        
        # Cas 7 : mois et année "janvier 1934"
        if (stringr::str_detect(texte, paste0("^(", paste(names(mois_fr), collapse="|"), ")\\s+\\d{4}$"))) {
          mois <- stringr::str_extract(texte, paste(names(mois_fr), collapse="|"))
          annee <- as.numeric(stringr::str_extract(texte, "\\d{4}"))
          mois_num <- mois_fr[[mois]]
          start <- lubridate::ymd(sprintf("%d-%02d-01", annee, mois_num))
          end <- start + months(1) - days(1)
          return(tibble::tibble(start = start, end = end))
        }
        
        # Cas 8 : date précise "17 juillet 1934" ou "1er juillet 1934"
        if (stringr::str_detect(texte, "\\d{1,2}\\s+[[:alpha:]]+\\s+\\d{4}")) {
          # On remplace "1er" par "1" pour faciliter le parsing
          texte_clean <- stringr::str_replace(texte, "^1er", "1")
          # Parse la date avec lubridate (format français)
          date_parsed <- lubridate::parse_date_time(texte_clean, orders = c("d b Y", "d B Y"), locale = "fr_FR")
          if (!is.na(date_parsed)) {
            return(tibble::tibble(start = as.Date(date_parsed), end = NA_Date_))
          }
        }
        
        # Par défaut, si aucune condition ne correspond, on renvoie NA pour les dates
        return(tibble::tibble(start = NA_Date_, end = NA_Date_))
      }
      
      
      # Ajout de colonnes start et end initialisées à NA_Date_
      donnees <- df %>%
        dplyr::mutate(start = as.Date(NA), end = as.Date(NA))
      
      # Pour chaque ligne, on applique parser_periode sur la première colonne (celle contenant la période)
      for (i in seq_len(nrow(donnees))) {
        dates <- parser_periode(donnees[i, 1][[1]])
        donnees[i, "start"] <- dates$start
        donnees[i, "end"] <- dates$end
      }
      
      # Réorganisation des données en format long pour faciliter la manipulation
      # On conserve start et end, on transforme les autres colonnes en paires (group, content)
      donnees_long <- donnees[,2:length(donnees)] %>%
        tidyr::pivot_longer(-c(start, end), 
                            names_to = "group", 
                            values_to = "content")
      
      # Fonction interne pour formater (tronquer) le contenu textuel long
      txt_formater = function(content){
        sous_chaine <- substr(content, 0, 40)
        # Trouver la position du premier espace dans cette sous-chaîne
        all_spaces = gregexpr(" ", sous_chaine)[[1]]
        pos_espace <- all_spaces[floor(length(all_spaces)/2)]
        if (pos_espace == -1) {
          # Pas d'espace trouvé, couper arbitrairement à 20 puis 40
          pos_coupure <- 20
          pos_espace_fin = 40
        } else {
          # Coupure au milieu de la chaîne pour éviter de couper un mot
          pos_coupure <- pos_espace
          pos_espace_fin = all_spaces[which(all_spaces > 25)[1]]
        }
        
        # Construction du texte tronqué avec saut de ligne HTML <br> et indication [...]
        y = paste0(
          substr(content, 1, pos_coupure),
          "\n",
          substr(content, pos_coupure + 1, pos_espace_fin),
          "[...]"
        )
        return(y)
      }
      
      # Fonction interne pour reformater le contenu long à garder, mieux imprimable
      
      txt_formater2 = function(content){
        # Diviser le texte en chunks de 150 caractères
        chunks <- c()
        texte <- content
        
        while(nchar(texte) > 100) {
          # Trouver la position du dernier espace avant 150 caractères
          sous_chaine <- substr(texte, 1, 100)
          derniers_espaces <- gregexpr(" ", sous_chaine)[[1]]
          
          if(length(derniers_espaces) > 0 && derniers_espaces[1] != -1) {
            # Couper au dernier espace pour éviter de couper un mot
            pos_coupure <- derniers_espaces[length(derniers_espaces)]
          } else {
            # Pas d'espace trouvé, couper à 150
            pos_coupure <- 100
          }
          
          # Ajouter le chunk
          chunks <- c(chunks, substr(texte, 1, pos_coupure))
          
          # Supprimer la partie déjà traitée
          texte <- substr(texte, pos_coupure + 1, nchar(texte))
        }
        
        # Ajouter le reste du texte
        if(nchar(texte) > 0) {
          chunks <- c(chunks, texte)
        }
        
        # Joindre avec des retours à la ligne
        return(paste(chunks, collapse = "\n"))
      }
      # Application du formatage sur toutes les lignes, uniquement si le texte dépasse 20 caractères
      donnees_long = donnees_long %>%
        mutate(content_long = content)
      
      for(i in 1:nrow(donnees_long)){
        if(!is.na(donnees_long[i, "content"])){
          if(nchar(donnees_long[i, "content"]) > 20){
            donnees_long[i, "content"] = txt_formater(donnees_long[i, "content"])
          }
        }
      }
      
      
      for(i in 1:nrow(donnees_long)){
        if(!is.na(donnees_long[i, "content_long"])){
          if(nchar(donnees_long[i, "content_long"]) > 20){
            donnees_long[i, "content_long"] = txt_formater2(donnees_long[i, "content_long"])
          }
        }
      }
      
      # Comptage avant filtrage
      n_avant <- nrow(donnees_long)
      # Filtrage des lignes avec une date de début valide (non NA)
      donnees_filtrees <- donnees_long %>% dplyr::filter(!is.na(start))
      # Comptage après filtrage
      n_apres <- nrow(donnees_filtrees)
      
      # On retourne uniquement les données filtrées prêtes à être utilisées ailleurs
      donnees_filtrees
      
    })
    
    # UI dynamique : création du selectInput des acteurs  
    
    output$acteur_ui <- renderUI({
      req(donnees()) # s'assure que les données sont bien chargés
      acteurs <- unique(donnees()$group) # récupère la liste unique des noms
      pickerInput(
        inputId = "acteurs_sel",
        label = "Acteurs à afficher :",
        choices = acteurs,
        selected = acteurs[1], # Sélectionne un acteur par défaut, pour éviter le bug
        multiple = TRUE, # Permet de sélectionner plusieurs acteurs
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
      )
    })
    
    # Reactive expression qui filtre les données selon les acteurs sélectionnés
    # Et abandonne les colonnes vides pour une visualisation plus fluide
    filtered_data <- reactive({
      req(donnees())
      
      req(input$acteurs_sel)
      
      donnees() %>%
        filter(group %in% input$acteurs_sel) %>%
        filter(!is.na(content))%>% # Filtrage du contenu ici
        filter(!is.na(start))%>%
        mutate(id = row_number()) %>% # Initiation de l'identifiant unique pour display dans le package
        # timeviz
        select(id, content, content_long, start, end, group)
    })
    
    # Sortie de la timeline
    output$timeline <- renderTimevis({
      
      req(filtered_data())
      req(nrow(filtered_data())>0)
      
      groupes <- data.frame(id = unique(filtered_data()$group),
                            content = unique(filtered_data()$group),
                            stringsAsFactors = FALSE)
      
      timevis(data = filtered_data(), 
              groups = groupes, 
              options = list(
                stack = TRUE,
                # Options pour améliorer la visibilité des éléments sélectionnés
                selectable = TRUE,
                multiselect = FALSE
              )
      )
    })
    
    # Affichage des informations de l'élément cliqué
    output$clicked_content <- renderUI({
      if (is.null(input$timeline_selected)) {
        return(div(class = "text-muted", "Cliquez sur un élément de la timeline pour voir ses détails"))
      }
      
      # Récupérer l'élément sélectionné
      selected_id <- input$timeline_selected
      item <- filtered_data()[filtered_data()$id == selected_id, ]
      
      if (nrow(item) > 0) {
        div(
          class = "content-display",
          div(class = "field mb-2", 
              span(class = "fw-bold", "Acteur: "), 
              span(item$group)
          ),
          div(class = "field mb-2", 
              span(class = "fw-bold", "Début: "), 
              span(item$start)
          ),
          div(class = "field mb-2", 
              span(class = "fw-bold", "Fin: "), 
              span(ifelse(is.na(item$end), "Non définie", format(as.Date(item$end), "%Y-%m-%d")))
          ),
          div(class = "content-field",
              div(class = "fw-bold mb-1", "Contenu:"),
              div(class = "content-text", 
                  style = "white-space: pre-wrap; line-height: 1.5;",
                  item$content_long
              )
          )
        )
      } else {
        div(class = "text-danger", "Élément introuvable")
      }
    })
    
    # Sauvegarde de l'output graphique
    output$export_png <- downloadHandler(
      filename = function() {
        paste0("timeline_", Sys.Date(), ".png")
      },
      content = function(file) {
        # Fichier temporaire HTML du timeline
        temp_html <- tempfile(fileext = ".html")
        
        groupes <- data.frame(
          id = unique(filtered_data()$group),
          content = unique(filtered_data()$group),
          stringsAsFactors = FALSE
        )
        
        timeline_widget <- timevis(
          filtered_data(),
          groups = groupes,
          options = list(width = "1200px", height = "500px")
        )
        
        htmlwidgets::saveWidget(timeline_widget, temp_html, selfcontained = TRUE)
        
        # Chemin exact où on veut sauvegarder le PNG
        save_path <- file.path("outputs", paste0("timeline_", Sys.Date(), ".png"))
        
        # Génération PNG avec webshot, qui va créer le fichier à save_path
        webshot2::webshot(temp_html, save_path, vwidth = 1200, vheight = 500, delay = 3)
        
        # Copier ce fichier dans 'file' pour le téléchargement
        file.copy(save_path, file)
      }
    )
    
  }
  
  shinyApp(ui, server4)
  
  