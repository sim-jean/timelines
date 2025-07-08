### Troubleshooting

library(shiny)
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)
library(here)
library(timevis)
library(lubridate)


data = read_excel(paste0(here("data"),"/", list.files(here("data"))[[1]]))


parser_periode <- function(texte) {
  
  if (str_length(texte) > 30 || str_detect(texte, "[\\?\\!\\;\\:\\…]") || is.na(texte)) {
    return(tibble(start = NA, end = NA))
  }
  
  texte <- str_trim(str_to_lower(texte))
  
  mois_fr <- c("janvier"=1, "février"=2, "mars"=3, "avril"=4, "mai"=5, "juin"=6,
               "juillet"=7, "août"=8, "septembre"=9, "octobre"=10, "novembre"=11, "décembre"=12)
  
  # Cas 1. Plage d’années : "1950-1955"
  if (str_detect(texte, "^\\d{4}\\s*-\\s*\\d{4}$")) {
    an <- as.numeric(str_extract_all(texte, "\\d{4}")[[1]])
    start <- ymd(sprintf("%d-01-01", an[1]))
    end <- ymd(sprintf("%d-12-31", an[2]))
    return(tibble(start = start, end = end))
  }
  
  if (str_detect(texte, c("années"))){
    year = strsplit(texte, "années")[[1]][2]
    year = as.numeric(year)
    return(tibble(start = ymd(sprintf("%d-01-01", year)),
                  end = ymd(sprintf("%d-12-31", year+9))))
  }
  
  if (str_detect(texte, c("annees"))){
    year = strsplit(texte, c("annees"))[[1]][2]
    year = as.numeric(year)
    return(tibble(start = ymd(sprintf("%d-01-01", year)),
                  end = ymd(sprintf("%d-12-31", year+9))))
  }
  
  
  # Cas 2b. Plages de date "du 17 au 30 ???"
  
  if (str_detect(texte, "^du \\d{1,2} au \\d{1,2} [[:alpha:]]+ \\d{4}$")) {
    jour1 <- str_extract(texte, "(?<=du )\\d{1,2}")
    jour2 <- str_extract(texte, "(?<=au )\\d{1,2}")
    mois  <- str_extract(texte, paste(names(mois_fr), collapse="|"))
    annee <- str_extract(texte, "\\d{4}")
    mois_num <- mois_fr[mois]
    start <- ymd(sprintf("%s-%02d-%02d", annee, mois_num, as.integer(jour1)))
    end   <- ymd(sprintf("%s-%02d-%02d", annee, mois_num, as.integer(jour2)))
    return(tibble(start = start, end = end))
  }
  
  if (str_detect(texte, "^\\d{1,2} et \\d{1,2} [[:alpha:]]+ \\d{4}$")) {
    jours <- as.integer(str_extract_all(texte, "\\d{1,2}")[[1]])
    mois  <- str_extract(texte, paste(names(mois_fr), collapse="|"))
    annee <- str_extract(texte, "\\d{4}")
    mois_num <- mois_fr[mois]
    start <- ymd(sprintf("%s-%02d-%02d", annee, mois_num, jours[1]))
    end   <- ymd(sprintf("%s-%02d-%02d", annee, mois_num, jours[2]))
    return(tibble(start = start, end = end))
  }
  
  # Cas : janvier-mars 1990
  
  if (str_detect(texte, paste0("(", paste(names(mois_fr), collapse="|"), ")\\s*-\\s*(",
                               paste(names(mois_fr), collapse="|"), ")\\s+\\d{4}$"))) {
    mois1 <- str_match(texte, paste0("^(", paste(names(mois_fr), collapse="|"), ")"))[,2]
    mois2 <- str_match(texte, paste0("-\\s*(", paste(names(mois_fr), collapse="|"), ")"))[,2]
    annee <- as.integer(str_extract(texte, "\\d{4}"))
    mois1_num <- mois_fr[mois1]
    mois2_num <- mois_fr[mois2]
    start <- ymd(sprintf("%d-%02d-01", annee, mois1_num))
    end <- ymd(sprintf("%d-%02d-01", annee, mois2_num)) + months(1) - days(1)
    return(tibble(start = start, end = end))
  }
  
  # Cas 2. Année seule : "1990"
  if (str_detect(texte, "^\\d{4}$")) {
    year <- as.numeric(texte)
    return(tibble(start = ymd(sprintf("%d-01-01", year)),
                  end = ymd(sprintf("%d-12-31", year))))
  }
  
  
  # Cas 3. Mois et année : "janvier 1990"
  if (str_detect(texte, paste0("^(", paste(names(mois_fr), collapse="|"), ")\\s+\\d{4}$"))) {
    mois <- str_extract(texte, paste(names(mois_fr), collapse="|"))
    annee <- as.numeric(str_extract(texte, "\\d{4}"))
    mois_num <- mois_fr[[mois]]
    start <- ymd(sprintf("%d-%02d-01", annee, mois_num))
    end <- start + months(1) - days(1)
    return(tibble(start = start, end = end))
  }
  
  # Cas 4. Date précise : "18 juin 1955", "1er février 1991"
  if (str_detect(texte, "\\d{1,2}\\s+[[:alpha:]]+\\s+\\d{4}")) {
    texte_clean <- str_replace(texte, "^1er", "1") # gère les "1er"
    date_parsed <- parse_date_time(texte_clean, orders = c("d b Y", "d B Y"), locale = "fr_FR")
    if (!is.na(date_parsed)) {
      return(tibble(start = as_date(date_parsed), end = NA))
    }
  }
  
  # Cas 5. Défaut : NA
  return(tibble(start = NA_Date_, end = NA_Date_))
}


data = data %>%
  mutate(start = NA, 
         end = NA)

for (i in 1:nrow(data)){
  data[i, "start"] = parser_periode(data[i,1])[1]
  data[i, "end"] = parser_periode(data[i,1])[2]
}

# Sert à déclencher la partie interactive
server <- function(input, output, session) {
  
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
    
    # Application du formatage sur toutes les lignes, uniquement si le texte dépasse 20 caractères
    for(i in 1:nrow(donnees_long)){
      if(!is.na(donnees_long[i, "content"])){
        if(nchar(donnees_long[i, "content"]) > 20){
          donnees_long[i, "content"] = txt_formater(donnees_long[i, "content"])
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
      select(id, content, start, end, group)
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
              stack = TRUE
            )
    )
  })
}


server2 <- function(input, output, session) {
  
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
    
    # Application du formatage sur toutes les lignes, uniquement si le texte dépasse 20 caractères
    for(i in 1:nrow(donnees_long)){
      if(!is.na(donnees_long[i, "content"])){
        if(nchar(donnees_long[i, "content"]) > 20){
          donnees_long[i, "content"] = txt_formater(donnees_long[i, "content"])
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
      select(id, content, start, end, group)
  })
  
  # Reactive value pour stocker l'ID de l'élément sélectionné
  selected_item <- reactiveVal(NULL)
  
  # Observer pour capturer les clics sur la timeline
  observe({
    if (!is.null(input$timeline_selected)) {
      selected_item(input$timeline_selected)
    }
  })
  
  # Sortie de la timeline avec mise en évidence des éléments sélectionnés
  output$timeline <- renderTimevis({
    
    req(filtered_data())
    req(nrow(filtered_data())>0)
    
    # Création des données avec styles conditionnels
    timeline_data <- filtered_data()
    
    # Appliquer un style différent à l'élément sélectionné
    if (!is.null(selected_item())) {
      timeline_data <- timeline_data %>%
        mutate(
          style = ifelse(
            id == selected_item(), 
            "background-color: #ff6b6b; border: 3px solid #ff4757; color: white; font-weight: bold; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
            "background-color: #74b9ff; border: 1px solid #0984e3; color: white;"
          )
        )
    } else {
      # Style par défaut pour tous les éléments
      timeline_data <- timeline_data %>%
        mutate(style = "background-color: #74b9ff; border: 1px solid #0984e3; color: white;")
    }
    
    groupes <- data.frame(id = unique(filtered_data()$group),
                          content = unique(filtered_data()$group),
                          stringsAsFactors = FALSE)
    
    timevis(data = timeline_data, 
            groups = groupes, 
            options = list(
              stack = TRUE,
              selectable = TRUE,
              multiselect = FALSE,
              # Configuration pour améliorer la visibilité de la sélection
              configure = list(
                interaction = list(
                  selectConnectedEdges = FALSE
                )
              )
            )
    )
  })
  
  # Affichage du contenu complet de l'élément cliqué
  output$clicked_content <- renderText({
    req(selected_item())
    req(filtered_data())
    
    # Récupérer les données originales (non formatées) pour l'affichage complet
    original_data <- donnees() %>%
      filter(group %in% input$acteurs_sel) %>%
      filter(!is.na(content)) %>%
      filter(!is.na(start)) %>%
      mutate(id = row_number())
    
    clicked_item <- original_data[original_data$id == selected_item(), ]
    
    if (nrow(clicked_item) > 0) {
      paste0(
        "Élément sélectionné (ID: ", selected_item(), ")\n",
        "Acteur: ", clicked_item$group, "\n",
        "Date de début: ", clicked_item$start, "\n",
        "Date de fin: ", ifelse(is.na(clicked_item$end), "Non définie", clicked_item$end), "\n",
        "Contenu complet:\n", 
        # Récupérer le contenu original non tronqué
        stringr::str_replace_all(clicked_item$content, "\n", " ")
      )
    }
  })
}

# Sert à déclencher la partie interactive
server3 <- function(input, output, session) {
  
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
    
    donnees_long = donnees_long %>%
      mutate(content2 = NA)
    # Application du formatage sur toutes les lignes, uniquement si le texte dépasse 20 caractères
    for(i in 1:nrow(donnees_long)){
      if(!is.na(donnees_long[i, "content"])){
        if(nchar(donnees_long[i, "content"]) > 20){
          donnees_long[i, "content2"] = txt_formater(donnees_long[i, "content2"])
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
      select(id, content, start, end, group)
  })
  
  filtered_data2 <- reactive({
    req(donnees())
    
    req(input$acteurs_sel)
    
    donnees() %>%
      filter(group %in% input$acteurs_sel) %>%
      filter(!is.na(content))%>% # Filtrage du contenu ici
      filter(!is.na(start))%>%
      mutate(id = row_number()) %>% # Initiation de l'identifiant unique pour display dans le package
      # timeviz
      select(id, content2, start, end, group)
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
  output$clicked_content <- renderText({
    if (is.null(input$timeline_selected)) {
      return("Cliquez sur un élément de la timeline pour voir ses détails")
    }
    
    # Récupérer l'élément sélectionné
    selected_id <- input$timeline_selected
    item <- filtered_data()[filtered_data()$id == selected_id, ]
    
    if (nrow(item) > 0) {
      paste(
        paste("Acteur:", item$group),
        paste("Début:", item$start),
        paste("Fin:", ifelse(is.na(item$end), "Non définie", item$end)),
        paste("Contenu:", gsub("\n", " ", item$content2)),
        sep = "\n"
      )
    } else {
      "Élément introuvable"
    }
  })
}
# Besoin de traiter les /
# Besoin de traiter les ?
# Besoin de traiter les janvier - décembre 1999