#------------------------------------------- LES LIBRAIRIES -----------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)   
library(tidyr)   
library(ggplot2)
library(plotly)
library(shinyjs)
library(openxlsx)
library(writexl)
library(readr)  
library(stringr)  
library('reticulate') # Importation de la bibliothèque reticulate pour excuter les script python
library(tibble)
library(DT)
library(gtools)
library("clustcurv")
#------------------------------------------ Fonction utilisé pour le scraping  -----------------------------------------------------------------------------------
check_and_install_python_pkgs <- function(pkgs = c("pandas", "selenium", "numpy")) {
  for (pkg in pkgs) {
    if (!reticulate::py_module_available(pkg)) {
      reticulate::py_install(pkg, quiet = TRUE)
    }
  }
}

check_and_install_python_pkgs()

source_python("Recupération_Code_Source.py")

scraping <- function(fichier)
{
  texte <- read_file(fichier)  # Chargement du contenu du code source dans la variable texte
  v <- str_split(texte, 'Points')[[1]]  # Séparation du texte en fonction de 'Points:' et sélection du contenu d'intérêt
  interet <- v[[length(v)]]  # Extraction du contenu d'intérêt
  w <- str_split(interet, 'app-mass-start-result-table')[[1]]  # Séparation supplémentaire du contenu
  interet2 <- w[[1]]  # Sélection du contenu final
  
  infos_brut <- v[[1]]
  titre <- str_extract_all(infos_brut, "(?<=\\<h3 _ngcontent-vog-c47=).*?(?=\\</h3>)")
  Date_localisation <- str_extract_all(infos_brut, "(?<=\\</i>).*?(?=\\</span>)")
  
  #-------------------------------Extraction du contenu entre les balises <td>----------------------------
  
  contenus <- str_extract_all(interet2, "(?<=\\>).*?(?=\\<)")  # Extraction du contenu entre les balises <td>
  contenus <- unlist(contenus)  # Transformation de la liste en vecteur
  contenus <- contenus[which(contenus!="")]  # Suppression des éléments vides 
  #-------------------------------traiteement du contenu-----------------------------------------------
  k <- 0  # Initialize counter for elements in LISTE
  LISTE <- list()  # Initialize list to store athletes' data
  
  for (i in 1:length(contenus)) {
    if (contenus[i] == "#" && str_detect(contenus[i-1],'Age:')) {
      debut <- i - 10  # Start of athlete's data
      fin<-i
      break
    }}
  
  for (j in (fin+1):length(contenus)) {
    if (contenus[j] == "#" && str_detect(contenus[j-1],'Age:')) {
      fin <- j - 10  # End of athlete's data
      k <- k + 1  # Increment counter for elements in LISTE
      vec <- contenus[debut:fin]  # Extract athlete's data
      vec <- trimws(vec, 'both')  # Clean whitespace
      if(grepl("[0-9]", vec[1])){
        if(as.numeric(vec[1]==k))
        {
          if(vec[[5]]=="PB")
          {
            vec <- contenus[(debut-1):fin]  # Extract athlete's data
            vec <- trimws(vec, 'both')  # Clean whitespace
            LISTE[[k]] <- vec  # Store athlete's data in LISTE
            debut<-(fin)
          } 
          else{
            LISTE[[k]] <- vec  # Store athlete's data in LISTE
            debut<-(fin)
          }
        }else if(vec[5]=="PB")
        {
          vec <- contenus[(debut-1):fin]  # Extract athlete's data
          vec <- trimws(vec, 'both')  # Clean whitespace
          LISTE[[k]] <- vec  # Store athlete's data in LISTE
          debut<-(fin)
        } else{
          vec <- contenus[(debut+1):fin]  # Extract athlete's data
          vec <- trimws(vec, 'both')  # Clean whitespace
          LISTE[[k]] <- vec  # Store athlete's data in LISTE
          debut<-(fin)
        }
      }else if(vec[[5]]=="PB")
      {
        vec <- contenus[(debut-1):fin]  # Extract athlete's data
        vec <- trimws(vec, 'both')  # Clean whitespace
        LISTE[[k]] <- vec  # Store athlete's data in LISTE
        debut<-(fin)
      } else if(vec[7]=="TR" || vec[7]=="PB")
      {
        vec <- contenus[(debut+1):fin]  # Extract athlete's data
        vec <- trimws(vec, 'both')  # Clean whitespace
        LISTE[[k]] <- vec  # Store athlete's data in LISTE
        debut<-(fin)
      } else{
        vec <- contenus[(debut+2):fin]  # Extract athlete's data
        vec <- trimws(vec, 'both')  # Clean whitespace
        LISTE[[k]] <- vec  # Store athlete's data in LISTE
        debut<-(fin)
      }
      
    } else if (j == length(contenus)) {
      fin <- j
      k <- k + 1
      vec <- contenus[debut:fin]
      vec <- trimws(vec, 'both')
      if(as.numeric(vec[1]==k))
      {
        LISTE[[k]] <- vec  # Store athlete's data in LISTE
        debut<-(fin)
      }else
      {
        vec <- contenus[(debut+1):fin]  # Extract athlete's data
        vec <- trimws(vec, 'both')  # Clean whitespace
        LISTE[[k]] <- vec  # Store athlete's data in LISTE
        debut<-(fin)
      }
      break
    }
  }
  
  # Clean up empty lists from LISTE
  LISTE <- LISTE[lengths(LISTE) > 0]
  
  T_<-character()
  R_<-character()
  Data_player<-list()
  for(i in 1:(length(LISTE)))
  {
    ess <- LISTE[[i]]  # Sélection des données de l'athlète
    ess <- ess[-c(1:5)]  # Suppression des premières colonnes qui ont déjà été traitées
    ess <- ess[which(str_detect(ess, " "))]  # Sélection des éléments contenant des espaces
    ess_clean <- ess[!grepl("[a-zA-Z]", ess)]  # Suppression des éléments contenant des lettres
    ess_clean <- unique(ess_clean)  # Suppression des doublons
    
    if (length(ess_clean>0))
    {Data_player[[i]]<- ess_clean}
    else
    {
      LISTE[[i]][1]<-i
      ess_clean<-rep('0',length(Data_player[i-1]))
      Data_player[[i]]<- ess_clean
    }
  }
  #--------------------------------------Remplissage des contenues dans la table de données-------------------------------
  l<-max(sapply(Data_player, length))
  g<-2*l+5
  DATA_MASS_ <- data.frame(matrix(nrow = length(LISTE), ncol = g))  # Création d'un dataframe vide
  names(DATA_MASS_)[1:5] <- c('Rnk', 'NO', 'Name', 'COUNTRY', 'Total Time')  # Attribution des noms de colonnes
  DATA_MASS <- data.frame(matrix(nrow = length(LISTE), ncol = g+l))  # Création d'un dataframe vide
  vv <- c()  # Initialisation du vecteur pour stocker les noms des colonnes du vecteur du temps intermédiaire cumulé (le temps de chaque partie
  for(n in 1:l)
  {
    vv[2*n-1] <- paste('AT_tour', n)    
    vv[2*n] <- paste('Rank_tour', n)
    
  }
  names(DATA_MASS_)[6:g] <- vv  # Attribution des noms de colonnes du vecteur du TIC
  for(i in 1:(length(LISTE)))
  {
    for (k in 1:l) 
    {
      T_[k] <-(strsplit(Data_player[[i]][k], " \\(")[[1]])[1]  # Extraire les temps en enlevant les rangs
      R_[k]<-gsub("\\)", "", strsplit(Data_player[[i]][k], " \\(")[[1]][2])  # Extraire les rangs et convertir en numérique  
    }
    DATA_MASS[i, 1:5] <- LISTE[[i]][1:5]
    DATA_MASS_[i, 1:5] <- LISTE[[i]][1:5]  
    for (j in seq(from=6,to=g,by=2) ) 
    {
      index <- (j-4)/2
      DATA_MASS_[i, j ] <-T_[index]
      DATA_MASS_[i, j+1 ] <-R_[index]
    }
  }
  convert_to_numeric <- function(df) 
  {
    ifelse(grepl(":", df), as.numeric(unlist(strsplit(df, ":")))[1] * 60 + as.numeric(unlist(strsplit(df, ":")))[2], as.numeric(df))
  }
  for (j in 5:g) {
    DATA_MASS_[,j]<-sapply(DATA_MASS_[, j], convert_to_numeric)
  }
  DATA_MASS_[is.na(DATA_MASS_)] <- 0
  Q<-c()
  AT_ <- DATA_MASS_[,8]-DATA_MASS_[,6]
  DATA_MASS <- cbind(DATA_MASS_[, 1:9], AT_, DATA_MASS_[, 10:ncol(DATA_MASS_)])
  colnames(DATA_MASS)[10] <- paste('Time_tour',2)  # Renomme la nouvelle colonne
  AT_ <- DATA_MASS_[,8]
  h= g+l -1
  for (j in seq(from = 13, to = h, by = 3))
  {
    if(j== h)
    {
      AT_ <- DATA_MASS[,j-2]-AT_
      AT <- paste('Time_tour',(j-2)%/%3)
      DATA_MASS <- cbind(DATA_MASS, AT_)
      colnames(DATA_MASS)[j] <- AT  # Renomme la nouvelle colonne
    }
    else
    {
      AT_ <- DATA_MASS[,j-2]-AT_
      AT <- paste('Time_tour',(j-2)%/%3)
      DATA_MASS <- cbind(DATA_MASS[, 1:j-1], AT_, DATA_MASS[, (j):ncol(DATA_MASS)])
      colnames(DATA_MASS)[j] <- AT  # Renomme la nouvelle colonne
      AT_ <- DATA_MASS[,j-2]
    }
    
  }
  #-----------------------Table de données final + les attribus des infos de compétition----------------------------------------------------
  DATA_MASS[, 5:ncol(DATA_MASS)] <- replace(DATA_MASS[, 5:ncol(DATA_MASS)], DATA_MASS[, 5:ncol(DATA_MASS)] <= 0, NA)
  
  infos_brut <- v[[1]]
  titre <- str_extract_all(infos_brut, "(?<=<h3).*?(?=</h3>)")
  Date_localisation <- str_extract_all(infos_brut, "(?<=</i>).*?(?=</span>)")
  
  competition_info <- str_split(infos_brut, 'row competition-info')  #competition-info__self
  infos <- str_extract_all(competition_info[[1]][2], "(?<=>).*?(?=<)")[[1]]
  infos=unlist(infos)
  infos=infos[which(infos!="")][1:8]
  
  Date <- Date_localisation[[1]][1]
  Localisation <- Date_localisation[[1]][2]
  titre_table <- gsub("_ngcontent-deu-c47=\"\">","", titre[[1]][1])
  titre_comp <- infos[1]
  Referee <-c(infos[3],infos[4])
  Starter <-c(infos[6],infos[7])
  time_comp <-infos[8]
  
  attr(DATA_MASS, "title") <- titre_table
  attr(DATA_MASS, "subtitle") <- list("competition" = titre_comp,
                                      "Localisation" = Localisation,
                                      "Date de compétition" = Date,
                                      "time competition" = time_comp,
                                      "Referee" = Referee[1],
                                      "Referee_NAT" = Referee[2],
                                      "Starter" = Starter[1],
                                      "Starter_NAT" = Starter[2])
  competition <- str_split(attributes(DATA_MASS)$title, '">')[[1]][2]
  Game <-attributes(DATA_MASS)$subtitle$competition
  JNS <- regmatches(competition, regexpr("(Junior|Neo Senior)", competition))
  JNS_ <- regmatches(Game,regexpr("(Junior|Neo-seniors)", Game))
  
  categorie <- ifelse(length(JNS_) > 0, JNS_[1], ifelse(length(JNS) > 0, JNS[1], "Senior"))
  
  Tour<-ifelse(grepl("SF | Semi-Final", Game),'SF', 'FINAL')
  Sex <-ifelse(grepl("Women", Game),'Women', 'Men')
  Lieu<-attributes(DATA_MASS)$subtitle$Localisation
  Date<-attributes(DATA_MASS)$subtitle$`Date de compétition`
  J_H<-attributes(DATA_MASS)$subtitle$`time competition`
  Referee<-attributes(DATA_MASS)$subtitle$Referee
  Referee_NAT<-attributes(DATA_MASS)$subtitle$Referee_NAT
  Starter<-attributes(DATA_MASS)$subtitle$Starter
  Starter_NAT<-attributes(DATA_MASS)$subtitle$Starter_NAT
  Division<-ifelse(grepl("Division B", Game),'Division B', 'Division A')
  infos_data <- tibble (
    Compétition = competition,`Titre de table` = Game,Catégorie = categorie,Division = Division,Tour = Tour,Sex = Sex,Lieu = Lieu,Date = Date,`Jour et Heure competition` = J_H,
    Referee = Referee,Ref_Nationalité = Referee_NAT,Starter = Starter,Sta_Nationalité = Starter_NAT)
  
  DATA_MASS <- cbind(DATA_MASS, infos_data)
  
  return(DATA_MASS)
}

Add_DB <- function(data) {
  nom_fichier <- "Base de données 5000m.xlsx"
  
  tryCatch({
    if (file.exists(nom_fichier)) {
      existing_data <- read_xlsx(nom_fichier)
      
      if (ncol(existing_data) != ncol(data)) {
        showNotification("Le nombre de colonnes ne correspond pas à celui de la BD.", type = "error")
        return(FALSE)
      } else if (!identical(colnames(existing_data), colnames(data))) {
        showNotification("Les noms de colonnes ne correspondent pas à ceux de la BD.", type = "error")
        return(FALSE)
      } else {
        wb <- loadWorkbook(nom_fichier)
        writeData(wb, sheet = 1, x = data, startRow = nrow(existing_data) + 1, colNames = FALSE, rowNames = FALSE)
        saveWorkbook(wb, nom_fichier, overwrite = TRUE)
        showNotification("Les données ont été soumises avec succès !", type = "message")
        return(TRUE)
      }
    } else {
      write_xlsx(data, nom_fichier)
      showNotification("Une nouvelle base de données a été créée avec succès !", type = "message")
      return(TRUE)
    }
  }, error = function(e) {
    showNotification(paste("Erreur lors de l'ajout des données :", e$message), type = "error")
    return(FALSE)
  })
}

#------------------------------------------ Importation des Données -----------------------------------------------------------------------------------
# 
Data_5000m <- read_xlsx("Base de données 5000m.xlsx")
indices <- which(Data_5000m$Rnk == 1)
names_competition <- unique(Data_5000m$Compétition)

# Initialiser une liste pour stocker les tables par compétition
names_tables <- list()
for (n in 1:length(names_competition)) {
  names_tables[[n]] <- unique(Data_5000m$`Titre de table`
                              [Data_5000m$Compétition == names_competition[n]])
}

noms_tables <- unlist(names_tables)

#---------------- Calcule des ecart des Temps entre le patineur vinqeur et les autres patineurs compétiteurs -----------------------------------------------------------------------------------

ecart_temps_tours_total <- list()

for (i in 1:length(indices)) {

  if(i < length(indices)) {
    Data <- Data_5000m[indices[i]:(indices[i+1]-1),]
  } else {
    Data <- Data_5000m[indices[i]:nrow(Data_5000m),]
  }

  ecart_temps_tour <- list()

  # Parcourir les colonnes correspondant à chaque tour
  for (k in c(6, seq(10, 43, by = 3))) {
    ecart_tour <- c()
    # Calculer l'écart de temps pour chaque tour
    if(k == 6) {
      Data <- Data %>% arrange(`AT_tour 1`)
      for (j in 2:(nrow(Data))) {
        if(!is.na(Data[j, 6]) && (Data[j, 6] - Data[1, 6] > 0)) {
          ecart_tour <- c(ecart_tour, Data[j, 6] - Data[1, 6])
        }
      }
      if(length(ecart_tour > 0)) {
        ecart_temps_tour[[paste0("tour", 1)]] <- unlist(unname(ecart_tour))
        ecart_temps_tours_total[[paste(unique(Data$`Titre de table`),
                                       unique(Data$Date), sep = ", ")]] <- ecart_temps_tour
      }
    } else {
      Data <- Data %>% arrange(!!sym(paste0("Time_tour ", (k-4) %/% 3)))
      for (j in 2:(nrow(Data))) {
        if(!is.na(Data[j, k]) && (Data[j, k] - Data[1, k] > 0)) {
          ecart_tour <- c(ecart_tour, Data[j, k] - Data[1, k])
        }
      }
      if(length(ecart_tour > 0)) {
        ecart_temps_tour[[paste0("tour", (k-4) %/% 3)]] <- unlist(unname(ecart_tour))
        ecart_temps_tours_total[[paste(unique(Data$`Titre de table`),
                                       unique(Data$Date), sep = ", ")]] <- ecart_temps_tour
      }
    }
  }
}
DATA_5000m<-read_xlsx("Base de données 5000m.xlsx")
DATA_5000m<-data.frame(DATA_5000m)%>%
  distinct()
#-----> Data Men :
DATA_5000m_Men_WC<- DATA_5000m[DATA_5000m$Sex == 'Men' & grepl("ISU World Cup",DATA_5000m$Compétition)== TRUE, ]
identifiant_Men_WC <-unique(DATA_5000m_Men_WC[, c("Titre.de.table", "Jour.et.Heure.competition"),])
#-----> Data Women :
DATA_5000m_Women_WC<- DATA_5000m[DATA_5000m$Sex == 'Women' & grepl("ISU World Cup",DATA_5000m$Compétition)== TRUE, ]
identifiant_Women_WC <-unique(DATA_5000m_Women_WC[, c("Titre.de.table", "Jour.et.Heure.competition"),])
#----------------------------------- Calcule des Ecarts ---------------------------------------------------------------------------------------------------
#-----> Ecart Men :
Ecart_TT_Men_WC <- list()
for (i in 1:nrow(identifiant_Men_WC))
{
  Data <- DATA_5000m_Men_WC[DATA_5000m_Men_WC$Titre.de.table == identifiant_Men_WC[i,"Titre.de.table"] &
                              DATA_5000m_Men_WC$Jour.et.Heure.competition == identifiant_Men_WC[i,"Jour.et.Heure.competition"],]
  
  ecart_temps_tour <- list()
  
  for (k in c(6, seq(10, 43, by = 3)) )
  {
    ecart_tour <- c()
    if(k==6)
    {
      Data <- Data %>% arrange(AT_tour.1)
      for (j in 2:(nrow(Data)))
      {
        if(!is.na(Data[j,6])&(Data[j,6]-Data[1,6]>0)){ecart_tour<-c(ecart_tour,Data[j,6]-Data[1,6])}
      }
      if(length(ecart_tour>0))
      {
        ecart_temps_tour[[paste0("R",1,".T",i)]]<-ecart_tour
        Ecart_TT_Men_WC[[paste0("table",i)]]<-ecart_temps_tour
      }
    }
    else
    {
      Data <-Data%>%
        arrange(!!sym(paste0("Time_tour.", (k-4)%/%3)))
      for (j in 2:(nrow(Data)))
      {
        if(!is.na(Data[j,k])&(Data[j,k]-Data[1,k]>0)){ecart_tour<-c(ecart_tour,Data[j,k]-Data[1,k])}
      }
      if(length(ecart_tour>0))
      {
        ecart_temps_tour[[paste0("R",(k-4)%/%3,".T",i)]]<-ecart_tour
        Ecart_TT_Men_WC[[paste0("table",i)]]<-ecart_temps_tour
      }
    }
  }
}

#-----> Ecart Women :

# Ecart_TT_Women_WC <- list()
# for (i in 1:nrow(identifiant_Women_WC))
# {
#   Data <- DATA_5000m_Women_WC[DATA_5000m_Women_WC$Titre.de.table == identifiant_Women_WC[i,"Titre.de.table"] &
#                                    DATA_5000m_Women_WC$Jour.et.Heure.competition == identifiant_Women_WC[i,"Jour.et.Heure.competition"],]
# 
#   ecart_temps_tour <- list()
# 
#   for (k in c(6, seq(10, 44, by = 3)) )
#   {
#     ecart_tour <- c()
#     if(k==6)
#     {
#       Data <- Data %>% arrange(AT_tour.1)
#       for (j in 2:(nrow(Data)))
#       {
#         if(!is.na(Data[j,6])&(Data[j,6]-Data[1,6]>0)){ecart_tour<-c(ecart_tour,Data[j,6]-Data[1,6])}
#       }
#       if(length(ecart_tour>0))
#       {
#         ecart_temps_tour[[paste0("R",1,".T",i)]]<-ecart_tour
#         Ecart_TT_Women_WC[[paste0("table",i)]]<-ecart_temps_tour
#       }
#     }
#     else
#     {
#       Data <-Data%>%
#         arrange(!!sym(paste0("Time_tour.", (k-4)%/%3)))
#       for (j in 2:(nrow(Data)))
#       {
#         if(!is.na(Data[j,k])&(Data[j,k]-Data[1,k]>0)){ecart_tour<-c(ecart_tour,Data[j,k]-Data[1,k])}
#       }
#       if(length(ecart_tour>0))
#       {
#         ecart_temps_tour[[paste0("R",(k-4)%/%3,".T",i)]]<-ecart_tour
#         Ecart_TT_Women_WC[[paste0("table",i)]]<-ecart_temps_tour
#       }
#     }
#   }
# }

#------------------------------------------- Implémentation de l'appli Shiny -----------------------------------------------------------------------------------

# Définir le CSS pour le mode sombre
dark_mode_css <- "
.custom-datatable th, .custom-datatable td {
  background-color: white;
  color: black;
}
"

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('FFRS.jpg'); /* Chemin vers votre image */
        background-size: cover; /* Ajustement de la taille de l'image */
        background-size: 100% 100%;
        background-repeat: no-repeat; /* Empêche la répétition de l'image */
        background-attachment: fixed; /* Fixe l'image de fond */
      }
      
    ")),
    tags$style(HTML(dark_mode_css))
  ),
  
  navbarPage(
    "Ice Speed Data",
    
    tabPanel("5000m",
             
    ),
    tabPanel("Dataupload",
             sidebarLayout(
               sidebarPanel(
                 textInput("urlInput", "Entrez l'URL à scraper :"),
                 fluidRow(
                   column(3, actionButton("scrapeButton", "Scraper", class = "btn btn-success")),
                   column(2, actionButton("Soum", "Soumettre", class = "btn btn-primary"), offset = 3)
                 )
               ),
               mainPanel(
                 DTOutput("dataTable")
               ))
    ),
    navbarMenu("Analysis",
               tabPanel("Rangs et Temps",
                        fluidRow(
                          column(2, 
                                 tags$div(style = "color: black;", 
                                          checkboxGroupInput("selected_sex", "Sélectionnez le sexe :", 
                                                             choices = c("Men" = "Men", "Women" = "Women"), inline = TRUE)
                                 )
                          ),
                          column(3, 
                                 uiOutput("table_select"),
                                 #checkboxInput("All_tab", label = tags$span("All", style = "color: black;"), value = FALSE)
                          ),
                          column(3,
                                 uiOutput("select_date"),
                                 #checkboxInput("All_dat", label = tags$span("All", style = "color: black;"), value = FALSE)
                          ),
                          column(3,
                                 uiOutput("select_nat"),
                                 # checkboxInput("All_nat", label = tags$span("All", style = "color: black;"), value = FALSE)
                          )
                        ),
                        fluidRow(
                          column(3,
                                 uiOutput("athlete"),
                                 #checkboxInput("All_pla", label = tags$span("All", style = "color: black;"), value = FALSE)
                          ),
                          column(3,
                                 uiOutput("Rang_time")
                          )),
                        fluidRow(
                          column(2,       
                                 actionButton("show", "Afficher", class = "btn-primary"),
                                 offset = 9
                          )
                        ),
                        conditionalPanel(
                          condition = "input.Rang_time.length > 1",
                          plotlyOutput("evolution_rang"),
                          plotlyOutput("evolution_temps")
                        ),
                        conditionalPanel(
                          condition = "input.Rang_time.length == 1",
                          plotlyOutput("evolution_single", width = "100%")
                        ),
                        conditionalPanel(
                          condition = "input.Rang_time.length > 0",
                          verbatimTextOutput("rang_time_tours")
                        )
               ),
               tabPanel("Fonctions des densités & des répartitions",
                        fluidRow(
                          column(4,
                                 selectInput("competition",label = tags$span("Choisir la compétition :", style = "color: black;"), choices = names_competition, multiple = TRUE)     
                          ),
                          column(4,
                                 uiOutput("select_table")),
                          column(3,
                                 uiOutput("select_tours")),
                          column(3,       
                                 actionButton("afficher", "Afficher"))
                        ),
                        fluidRow(
                          # Graphique de densité
                          plotlyOutput("graphique_densite")),
                        # Graphique de fonction de répartition
                        fluidRow(
                          plotlyOutput("graphique_repartition")),
                        fluidRow(
                          # Zone pour afficher les vecteurs d'ecart du temps
                          verbatimTextOutput("ecart_selected"))
               )),
    tabPanel("Patineurs FFRS",
             div(style = "text-align: center; color: blue;font-style: italic; font-weight: bold;", 
                 p("Cette Onglet est réservée pour visualiser les résultats des patineurs de la FFRS. ")),
             fluidRow(
               column(2,
                      selectInput("athletefr",label = tags$span("Choix du Patineur", style = "color: black;"), choices = unique(trimws(Data_5000m$Name[grepl("fr", trimws(Data_5000m$COUNTRY), ignore.case = TRUE)])), multiple = TRUE)
               ),
               column(2,
                      selectInput("RangTimeFR", label = tags$span("les Rangs/Temps :", style = "color: black;"),
                                  choices = c("RANGS", "TIMES"),
                                  multiple = TRUE)
               )),
             fluidRow(
               column(2,       
                      actionButton("sh", "Afficher"),
                      offset = 9
               )),
             fluidRow(
               plotlyOutput("evolution_rangfr"),
               plotlyOutput("evolution_tempsfr"),
             ),
             fluidRow(
               verbatimTextOutput("rang_time_toursfr")
             )
    ),
    tabPanel("Analyse Avancée",
             tags$div(style = "color: black;", 
                      checkboxGroupInput("selec_sex", "Sélectionnez le sexe :", 
                                         choices = c("Men" = "Men", "Women" = "Women"), inline = TRUE)
             ),
             fluidRow(
               plotlyOutput("Clustersplot"),
               plotOutput("Clustersgrille"),
             )
    ),
    tabPanel("Site Officiel",
             tags$a(href = "https://app.isuresults.eu/events", "Visitez le site officiel de l'ISU", target="_blank")
    )),
  actionButton("quit", "Quitter", class = "quit-button"),
  tags$head(tags$style(HTML("
    .quit-button {
      position: fixed;
      bottom: 20px;
      left: 50%;
      transform: translateX(-50%);
    }
  ")))
)
server <- function(input, output, session) {
  
  DATA <- reactiveVal(NULL)
  # Observer pour le bouton de scraping
  observeEvent(input$scrapeButton, {
    url <- input$urlInput
    DATA(NULL)
    # Appel de la fonction de scraping
    source_code <- fetch_page_source(url)
    scraped_data <- scraping(source_code)
    DATA(scraped_data)
    
    # Affichage des données dans le tableau
    output$dataTable <- renderDataTable({
      data<-DATA()
      if(!is.null(data))
      {
        data$`Total Time` <- sapply(data$`Total Time`, function(sec) {
          minutes <- floor(sec / 60)
          seconds <- sec %% 60
          sprintf("%d:%02.02f", minutes, seconds)
        })
        datatable(data[,c("Rnk","Name","COUNTRY","Total Time")], options = list(searching = FALSE ,paging = FALSE  ), class = "display custom-datatable", caption = paste("Epreuve :", unique(data$`Titre de table`), ", Date : ", unique(data$Date),
                                                                                                                                                                          ", Lieu : ", unique(data$Lieu)) )
      }else
      {datatable(data.frame(), options = list(searching = FALSE ,paging = FALSE  ))}
    })
  })
  
  # Téléchargement des données en fichier Excel
  observeEvent(input$Soum, {
    #Add_DB(DATA())
    if (Add_DB(DATA())) {
      session$reload()
    }
  })
  
  observeEvent(input$selected_sex, {
    #if (is.null(input$selected_sex)) return(NULL)
    selected_sex <-input$selected_sex
    choices <- unique(Data_5000m$`Titre de table`[Data_5000m$Sex %in% selected_sex])
    output$table_select <- renderUI({
      tagList(
        selectInput("table", label = tags$span("Choix des compétitions :", style = "color: black;"), choices = choices, multiple = TRUE),
        checkboxInput("All_tab", label = tags$span("All", style = "color: black;"), value = FALSE)
      ) 
    })
  })
  
  observeEvent(input$All_tab, {
    selected_sex <-input$selected_sex
    choices <- unique(Data_5000m$`Titre de table`[Data_5000m$Sex %in% selected_sex])
    if (input$All_tab) {
      updateSelectInput(session, "table", selected = choices)
    }
  })
  
  observeEvent(input$table, {
    output$select_date <- renderUI({
      date_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table)$Date)
      tags$div(style = "color: black;",
               selectInput("date", label = tags$span("Date :", style = "color: black;"), choices = date_choices, multiple = TRUE),
               checkboxInput("All_dat", label = tags$span("All", style = "color: black;"), value = FALSE)
      )
    })
  })
  
  observeEvent(input$All_dat, {
    date_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table)$Date)
    if (input$All_dat) {
      updateSelectInput(session, "date", selected = date_choices)
    }
  })
  
  
  # observeEvent(input$All_tab, {
  #   selected_sex <- input$selected_sex
  #   choices <- unique(Data_5000m$`Titre de table`[Data_5000m$Sex %in% selected_sex])
  #   if (input$All_tab) {
  #     updateSelectInput(session, "table", selected = choices)
  #   } else {
  #     updateSelectInput(session, "table", selected = character(0))
  #   }
  # })
  
  observeEvent(input$date, {
    output$select_nat <- renderUI({
      Nat_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table & Date %in% input$date)$COUNTRY)
      tags$div(style = "color: black;",
               selectInput("Nat", label = tags$span("Choix des nationalités :", style = "color: black;"), choices = Nat_choices, multiple = TRUE),
               checkboxInput("All_nat", label = tags$span("All", style = "color: black;"), value = FALSE)
      )
    })
  })
  
  observeEvent(input$All_nat, {
    Nat_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table & Date %in% input$date)$COUNTRY)
    if (input$All_nat) {
      updateSelectInput(session, "Nat", selected = Nat_choices)
    }
  })
  
  
  # observeEvent(input$All_dat, {
  #   date_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table)$Date)
  #   if (input$All_dat) {
  #     updateSelectInput(session, "date", selected = date_choices)
  #   } else {
  #     updateSelectInput(session, "date", selected = character(0))
  #   }
  # })
  
  observeEvent(input$Nat, {
    output$athlete <- renderUI({
      athlete_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table &
                                         Date %in% input$date &
                                         COUNTRY %in% input$Nat)$Name)
      tags$div(style = "color: black;",
               selectizeInput("athlete", "Choix des Patineurs(ses) :", choices = athlete_choices, multiple = TRUE),
               checkboxInput("All_pla", label = tags$span("All", style = "color: black;"), value = FALSE)
      )
    })
    
    # if (input$All_nat) {
    #   athlete_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table &
    #                                     Date %in% input$date &
    #                                     COUNTRY %in% input$Nat)$Name)
    #   updateSelectInput(session, "athlete", selected = athlete_choices)
    # }
  })
  
  observeEvent(input$All_pla, {
    athlete_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table &
                                       Date %in% input$date &
                                       COUNTRY %in% input$Nat)$Name)
    if (input$All_pla) {
      updateSelectInput(session, "athlete", selected = athlete_choices)
    } else {
      updateSelectInput(session, "athlete", selected = character(0))
    }
  })
  
  observeEvent(input$All_nat, {
    Nat_choices <- unique(subset(Data_5000m, `Titre de table` %in% input$table & Date %in% input$date)$COUNTRY)
    if (input$All_nat) {
      updateSelectInput(session, "Nat", selected = Nat_choices)
    } else {
      updateSelectInput(session, "Nat", selected = character(0))
    }
  })
  
  observeEvent(input$athlete, {
    output$Rang_time <- renderUI({
      if (is.null(input$Nat) || is.null(input$athlete)) return(NULL)
      selectizeInput("Rang_time", label = tags$span("les Rangs/Temps :", style = "color: black;"), choices = c("RANGS", "TIMES"), multiple = TRUE)
    })
  })
  
  observeEvent(input$show,
               {
                 req(input$table,input$date, input$Nat, input$athlete, input$Rang_time)
                 selected_data <- subset(Data_5000m, `Titre de table` %in% input$table & COUNTRY %in% input$Nat & Name %in% input$athlete & Date %in% input$date)
                 selected_Rang <- select(selected_data, 3, 51, 7, 9, seq(12, 42, by = 3))
                 SR <- gather(selected_Rang, key = "Tour", value = "Rang", -Name, -Date) %>%
                   mutate(Tour = as.numeric(gsub("Rank_tour", "", Tour)),
                          athlete = paste(Name, Date, sep = "_"))
                 
                 q <- ggplot(SR, aes(x = Tour, y = Rang, color = athlete, group = athlete, text = athlete)) +
                   geom_line() +
                   labs(title = paste("Évolution du rang de(s) Patineur(se) : \n",
                                      paste(input$athlete, collapse =", ")),
                        color = "Patineur(se)s") +
                   scale_x_continuous(breaks = 1:13, labels = paste0("tour", 1:13)) +
                   scale_y_continuous(breaks = 1:35) + 
                   theme_minimal() +
                   theme(legend.title = element_text("Patineur(se)s"), legend.position = "right")
                 
                 selected_Time <- select(selected_data, 3, 51, 6, seq(10, 45, by = 3))
                 ST <- gather(selected_Time, key = "Tour", value = "Time", -Name, -Date) %>%
                   mutate(Tour = as.numeric(gsub("Time_tour |AT_tour ", "", Tour)),
                          athlete = paste(Name, Date, sep = "_"))
                 
                 p <- ggplot(ST, aes(x = Tour, y = Time, color = athlete, group = athlete, text = athlete)) +
                   geom_line() + labs(title = paste("Évolution du Temps de(s) Patineur(se) : \n",
                                                    paste(input$athlete, collapse =", ")),
                                      color = "Patineur(se)s") +
                   scale_x_continuous(breaks = 1:13, labels = paste0("tour", 1:13)) +
                   scale_y_continuous(breaks = seq(17, 38, by = 1)) +
                   theme_minimal() +
                   theme(legend.title = element_text("Patineur(se)s"), legend.position = "right")
                 
                 output$evolution_rang <- renderPlotly({
                   if ("RANGS" %in% input$Rang_time) {
                     ggplotly(q, tooltip = "text")
                   } else {
                     NULL
                   }
                 })
                 
                 output$evolution_temps <- renderPlotly({
                   if ("TIMES" %in% input$Rang_time) {
                     ggplotly(p, tooltip = "text")
                   } else {
                     NULL
                   }
                 })
                 
                 output$evolution_single <- renderPlotly({
                   if ("RANGS" %in% input$Rang_time & "TIMES" %in% input$Rang_time) {
                     NULL
                   } else {
                     if (!is.null(input$Rang_time)) {
                       if (input$Rang_time == "RANGS") {
                         q
                       } else if (input$Rang_time == "TIMES") {
                         p
                       }
                     }
                   }
                 })
                 
                 #   output$rang_time_tours <- renderText({
                 #     G <- c()
                 #     R <- c()
                 #     
                 #     for (L in 1:nrow(selected_Rang)) {
                 #       G <- c(G,paste("->",paste(unlist(selected_Rang[L,]), collapse = ", "),"\n")) 
                 #     }
                 #     for (L in 1:nrow(selected_Time)) {
                 #       R <- c(R,paste("->",paste(unlist(selected_Time[L,]), collapse = ", "),"\n")) 
                 #     }
                 #     O <- c()
                 #     j <- c()
                 #     for (p in input$athlete) {
                 #       for (d in input$date) {
                 #         j <- c(j,p)
                 #         O <- c(O,paste("du date : ",d,":\n"))
                 #       }
                 #     }
                 #     return(paste(paste("Les temps :\n",R),
                 #                  paste("Les Rang :\n",G),"\n")) #"Pour le Patineur(se/s)",j,", ",O,
                 #   })
               })
  
  observeEvent(input$competition, 
               {
                 selected_ind_com <- reactiveVal()
                 output$select_table <- renderUI({
                   if (is.null(input$competition)) return(NULL)
                   tables_choices<- c()
                   indices_comp<-which(names_competition %in% input$competition)
                   
                   for (i in indices_comp) {
                     tables_choices <- c(tables_choices,paste(Data_5000m$`Titre de table`[Data_5000m$Compétition==names_competition[[i]]], Data_5000m$Date[Data_5000m$Compétition==names_competition[[i]]], sep = ", "))  
                   }
                   selectizeInput("table",label = tags$span("Choisir une table :", style = "color: black;") , choices = tables_choices, multiple = TRUE)
                 })
                 
                 # Sélection du nombre de tours pour la table sélectionnée
                 output$select_tours <- renderUI({
                   table_selected <- input$table
                   
                   #selected_ind_com(indices_comp)
                   if (is.null(table_selected)) return(NULL)
                   selectInput("tours",label = tags$span("choisir les tours désirées :", style = "color: black;") ,  choices = 1:13, multiple = TRUE)
                 })
                 
                 observeEvent(input$afficher, {
                   if (is.null(input$competition) || is.null(input$table)) {
                     return(NULL)
                   }
                   
                   # Obtenir les tours sélectionnés
                   table_selected <- input$table
                   
                   if (length(table_selected) == 0) {
                     output$graphique <- renderPlotly(NULL)
                     return()
                   }
                   # Sélectionner les écarts de temps pour les tours sélectionnés
                   
                   ecart_selected <- list()
                   
                   for (t in input$tours) 
                   {
                     for (TB in input$table) {
                       ecart_selected[[paste0("Tour",t,"(",TB,")")]] <- round(ecart_temps_tours_total[[TB]][[paste("tour", t, sep = "")]],4)
                     } 
                   }
                   ML<-max(sapply(ecart_selected, length))
                   for (i in 1:length(ecart_selected)) {
                     ecart<-ecart_selected[[i]]
                     if(length(ecart)<ML)
                     {
                       ecart<-c(ecart, rep(NA, ML - length(ecart)))
                     }
                     ecart_selected[[i]]<-ecart
                   }
                   DF <- as.data.frame(do.call(cbind, ecart_selected))    
                   DF <- gather(DF, key = "Nom", value = "Ecart")
                   DF<-na.omit(DF)
                   for (e in ecart_selected) {
                     e <- na.omit(e)  
                   }
                   
                   
                   p_d <- ggplot(DF, aes(x = Ecart, color = Nom, group = Nom, text = Nom)) +
                     geom_density(alpha = 0.5) +
                     labs(title = "Densité des écarts", color = "Tours") +
                     theme_minimal()+ labs(title = paste("Densité d'ecart de temps des Tours :",
                                                         paste(input$tours,collapse =", "), " pour :\n",
                                                         paste(input$table,collapse ="; "), "dans",
                                                         paste(input$competition,collapse =", ")))
                   
                   # Créer un graphique de fonction de répartition pour chaque tour sélectionné
                   p_r <- ggplot(DF, aes(x = Ecart, color = Nom, group = Nom, text = Nom)) +
                     stat_ecdf(alpha = 0.5) +
                     labs(title = "Répartition des écarts", color = "Tours") +
                     theme_minimal() + labs(title = paste("Fonction de Répartition d'ecart de temps des Tours :",
                                                          paste(input$tours,collapse =", "), " pour :\n",
                                                          paste(input$table,collapse ="; "), "dans",
                                                          paste(input$competition,collapse =", ")))
                   
                   # Afficher les vecteurs d'écart de temps sélectionnés
                   
                   R <- paste("Les vecteurs d'ecart de temps pour les tours :",
                              paste(input$tours,collapse = ", "),"dans les tables : \n",
                              paste(input$table,collapse = "; "),"sont :\n",
                              paste("->",ecart_selected,collapse = "\n "))
                   
                   
                   # Afficher les vecteur des ecart des données sélectionnées
                   # output$ecart_selected <- renderText({
                   #   return(R)
                   # })
                   
                   # Afficher le graphique de densité
                   output$graphique_densite <- renderPlotly({
                     ggplotly(p_d, tooltip = "text")
                   })
                   
                   # Afficher le graphique de fonction de répartition
                   output$graphique_repartition <- renderPlotly({
                     ggplotly(p_r, tooltip = "text") 
                   })
                 })
               })
  
  
  observeEvent(input$sh,
               {
                 req(input$athletefr, input$RangTimeFR)
                 selected_data <- subset(Data_5000m, trimws(Name) %in% input$athletefr)
                 selected_Rang <- select(selected_data, 3, 51, 7, 9, seq(12, 45, by = 3))
                 
                 SR <- gather(selected_Rang, key = "Tour", value = "Rang", -Name, -`Titre de table`,-Date) %>%
                   mutate(Tour = as.numeric(gsub("Rank_tour", "", Tour)),
                          athlete = paste(Name, `Titre de table`, Date,sep = ","))
                 
                 qfr <- ggplot(SR, aes(x = Tour, y = Rang, color = athlete, group = athlete, text = athlete)) +
                   geom_line() +
                   labs(title = paste("Évolution du rang de(s) Patineur(se) par tour : \n",
                                      paste(input$athletefr, collapse =", "),
                                      paste(input$date, collapse =", ")),
                        color = "Patineur(se)s") +
                   scale_x_continuous(breaks = 1:13, labels = paste0("tour", 1:13)) +
                   scale_y_continuous(breaks = 1:35) + 
                   theme_minimal() +
                   theme(legend.title = element_text("Patineur(se)s"), legend.position = "right")
                 
                 selected_Time <- select(selected_data, 3, 51,45, 6, seq(10, 45, by = 3))
                 ST <- gather(selected_Time, key = "Tour", value = "Time", -Name, -`Titre de table`,-Date) %>%
                   mutate(Tour = as.numeric(gsub("Time_tour |AT_tour ", "", Tour)),
                          athlete = paste(Name, `Titre de table`,Date, sep = ", "))
                 pfr <- ggplot(ST, aes(x = Tour, y = Time, color = athlete, group = athlete, text = athlete)) +
                   geom_line() + labs(title = paste("Évolution du Temps de(s) Patineur(se) par tour : \n",
                                                    paste(input$athletefr, collapse =", "),
                                                    paste(input$date, collapse =", ")),
                                      color = "Patineur(se)s") +
                   scale_x_continuous(breaks = 1:13, labels = paste0("tour", 1:13)) +
                   scale_y_continuous(breaks = seq(17, 38, by = 1)) +
                   theme_minimal() +
                   theme(legend.title = element_text("Patineur(se)s"), legend.position = "right")
                 output$evolution_rangfr <- renderPlotly({
                   req("RANGS" %in% input$RangTimeFR)
                   ggplotly(qfr, tooltip = "text")
                 })
                 
                 output$evolution_tempsfr <- renderPlotly({
                   req("TIMES" %in% input$RangTimeFR)
                   ggplotly(pfr, tooltip = "text")
                 })
                 
                 # output$rang_time_toursfr <- renderText({
                 #   G <- c()
                 #   R <- c()
                 
                 # for (L in 1:nrow(selected_Rang)) {
                 #   G <- c(G,paste("->",paste(unlist(selected_Rang[L,]), collapse = ", "),"\n")) 
                 # }
                 # for (L in 1:nrow(selected_Time)) {
                 #   R <- c(R,paste("->",paste(unlist(selected_Time[L,]), collapse = ", "),"\n")) 
                 # }
                 # return(paste(paste("Les temps :\n",R),
                 #              paste("Les Rang :\n",G),"\n")) 
                 #})
                 if ("RANGS" %in% input$RangTimeFR && !"TIMES" %in% input$RangTimeFR) {
                   shinyjs::show("evolution_rangfr")
                   shinyjs::show("rang_time_toursfr")
                   shinyjs::hide("evolution_tempsfr")
                 } else if ("TIMES" %in% input$RangTimeFR && !"RANGS" %in% input$RangTimeFR) {
                   shinyjs::show("evolution_tempsfr")
                   shinyjs::hide("evolution_rangfr")
                   shinyjs::show("rang_time_toursfr")
                 } else if ("RANGS" %in% input$RangTimeFR && "TIMES" %in% input$RangTimeFR) {
                   shinyjs::hide("rang_time_toursfr")
                   shinyjs::hide("evolution_rangfr")
                   shinyjs::hide("evolution_tempsfr")
                 } else {
                   shinyjs::hide("evolution_rangfr")
                   shinyjs::hide("evolution_tempsfr")
                   shinyjs::hide("evolution_singlefr")
                 }
                 
               })
  
  observeEvent(input$selec_sex,
               {
                 if(input$selec_sex=="Men")
                 {
                   #------> Préparation des Ecarts Men au clustering : 
                   
                   data_Clust_Men_WC <- data.frame()
                   W<-c()
                   CW<-c()
                   for (i in 1:length(Ecart_TT_Men_WC)) {
                     ecart<-Ecart_TT_Men_WC[[i]]
                     DF<-as.data.frame(do.call(cbind, ecart))
                     for (j in 1:ncol(DF)) {
                       V <- numeric()
                       CV <- numeric()
                       for (k in 1:nrow(DF)) {
                         colonne_courante <- DF[, j]
                         ecart_type_total <- sd(colonne_courante[colonne_courante < 10])
                         contribution_SD_ind <- sd(colonne_courante[-k][colonne_courante[-k] < 10])
                         var_diff <- ecart_type_total - contribution_SD_ind
                         V <- c(V, round(var_diff, 4))
                         CV <- c(CV, var_diff > 0.10 * sd(colonne_courante))
                       }
                       nom_colonne <- paste("Var.R", j, sep = ".")
                       DF[[nom_colonne]] <- V
                       CW <- c(CW, CV)
                       W <- c(W, V)
                     }
                     DF.E<-gather(DF[,c(1:13)], key = "Tour.Table", value = "Ecart")
                     DF.V<-gather(DF[,c(14:26)], key = "Tour.Table", value = "Cont.ind.Var")
                     DF <- as.data.frame(cbind(DF.E,DF.V))
                     data_Clust_Men_WC<-rbind(data_Clust_Men_WC,DF)
                   }
                   
                   data_Clust_Men_WC <- data_Clust_Men_WC[,-3] %>%
                     mutate(Cond.H0.1.Cont.Var = CW)%>%
                     mutate(Cond.Lmean.Cont.Var = if_else(Cont.ind.Var <= mean(W), 1, 0))
                   data_Clust_Men_WC<-data_Clust_Men_WC[data_Clust_Men_WC$Ecart<10,]
                   
                   M.Ksurv_WC <- ksurvcurves(time = data_Clust_Men_WC$Ecart, 
                                             x = data_Clust_Men_WC$Tour.Table,
                                             status = data_Clust_Men_WC$Cond.Lmean.Cont.Var, 
                                             algorithm = "kmeans", 
                                             seed = 300424, k = 6)
    
                   #autoplot(M.Ksurv_WC, groups_by_colour = TRUE)
                   Groupes<- cbind(M.Ksurv_WC$levels,M.Ksurv_WC$cluster)
                   colnames(Groupes)<-c("Tour.Table","Groupe")
                   Groupes<-as.data.frame(Groupes)
                   
                   MenClusters= Groupes
                   
                   for(i in 1:nrow(MenClusters)){
                     
                     MenClusters$Tour[i]=str_split(MenClusters$Tour.Table,"\\.")[[i]][1]
                     MenClusters$Table[i]=str_split(MenClusters$Tour.Table,"\\.")[[i]][2]
                     
                   }
                   
                   replacement_dict <- c(
                     "T1" = "T1_Division A",
                     "T2" = "T2_Division B",
                     "T3" = "T2_Division B",
                     "T4" = "T4_Division A",
                     "T5" = "T5_Division B",
                     "T6" = "T6_Division A",
                     "T7" = "T7_Division B",
                     "T8" = "T8_Division A",
                     "T9" = "T9_Division B",
                     "T10" = "T10_Division A",
                     "T11" = "T11_Division B",
                     "T12" = "T12_Division A"
                   )
                   
                   # Remplacer les valeurs en utilisant mutate et recode
                   MenClusters <- MenClusters %>%
                     mutate(Table = recode(Table, !!!replacement_dict))
                   MenClusters$Groupe2=paste('Groupe',MenClusters$Groupe)
                   
                    M.Clusters <- MenClusters %>%
                     group_by(Table,Tour,Groupe2) %>%
                     summarise(Frequency = n()) %>%
                     ungroup()
                   
                   M.Clusters$Tour <- factor(M.Clusters$Tour, levels = mixedsort(unique(M.Clusters$Tour)))
                   M.Clusters$Table <- factor(M.Clusters$Table, levels = mixedsort(unique(M.Clusters$Table)))
                   M.Clusters$Groupe2 <- factor(M.Clusters$Groupe2, levels = mixedsort(unique(M.Clusters$Groupe2)))
                   
                    output$Clustersplot <- renderPlotly({
                     ggplotly(autoplot(M.Ksurv_WC, groups_by_colour = TRUE,centers = TRUE))
                   })
                    
                    output$Clustersgrille <- renderPlot({
                      ggplot(M.Clusters, aes(fill=Groupe2, y=0, x=factor(0))) + 
                        geom_rect(aes(fill=Groupe2, col=Groupe2, xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf))+
                        facet_grid(Tour~Table)+labs(y='',x='')+
                        labs(title='Profil Général - Men -')+
                        theme_classic()+
                        theme( axis.ticks =element_blank(), axis.text = element_blank() )    
                    })
                 }else if(input$selec_sex=="Women")
                 {
                   #------> Préparation des Ecarts Women au clustering : 
                   
                   data_Clust_Women_WC <- data.frame()
                   W<-c()
                   CW<-c()
                   for (i in 1:length(Ecart_TT_Women_WC)) {
                     ecart<-Ecart_TT_Women_WC[[i]]
                     DF<-as.data.frame(do.call(cbind, ecart))
                     for (j in 1:ncol(DF)) {
                       V <- numeric()
                       CV <- numeric()
                       for (k in 1:nrow(DF)) {
                         colonne_courante <- DF[, j]
                         ecart_type_total <- sd(colonne_courante[colonne_courante < 10])
                         contribution_SD_ind <- sd(colonne_courante[-k][colonne_courante[-k] < 10])
                         var_diff <- ecart_type_total - contribution_SD_ind
                         V <- c(V, round(var_diff, 4))
                         CV <- c(CV, var_diff > 0.10 * sd(colonne_courante))
                       }
                       nom_colonne <- paste("Var.R", j, sep = ".")
                       DF[[nom_colonne]] <- V
                       CW <- c(CW, CV)
                       W <- c(W, V)
                     }
                     DF.E<-gather(DF[,c(1:13)], key = "Tour.Table", value = "Ecart")
                     DF.V<-gather(DF[,c(14:26)], key = "Tour.Table", value = "Cont.ind.Var")
                     DF <- as.data.frame(cbind(DF.E,DF.V))
                     data_Clust_Women_WC<-rbind(data_Clust_Women_WC,DF)
                   }
                   
                   data_Clust_Women_WC <- data_Clust_Women_WC[,-3] %>%
                     mutate(Cond.H0.1.Cont.Var = CW)%>%
                     mutate(Cond.Lmean.Cont.Var = if_else(Cont.ind.Var <= mean(W), 1, 0))
                   data_Clust_Women_WC<-data_Clust_Women_WC[data_Clust_Women_WC$Ecart<10,]
                   
                   W.Ksurv_WC <- ksurvcurves(time = data_Clust_Women_WC$Ecart, 
                                             x = data_Clust_Women_WC$Tour.Table,
                                             status = data_Clust_Women_WC$Cond.Lmean.Cont.Var, 
                                             algorithm = "kmeans", 
                                             seed = 300424, k = 6)
                   
                   #autoplot(W.Ksurv_WC, groups_by_colour = TRUE)
                   Groupes<- cbind(W.Ksurv_WC$levels,W.Ksurv_WC$cluster)
                   colnames(Groupes)<-c("Tour.Table","Groupe")
                   Groupes<-as.data.frame(Groupes)
                   
                   WomenClusters= Groupes
                   
                   for(i in 1:nrow(WomenClusters)){
                     
                     WomenClusters$Tour[i]=str_split(WomenClusters$Tour.Table,"\\.")[[i]][1]
                     WomenClusters$Table[i]=str_split(WomenClusters$Tour.Table,"\\.")[[i]][2]
                     
                   }
                   
                   replacement_dict <- c(
                     "T1" = "T1_Division A",
                     "T2" = "T2_Division B",
                     "T3" = "T2_Division B",
                     "T4" = "T4_Division A",
                     "T5" = "T5_Division B",
                     "T6" = "T6_Division A",
                     "T7" = "T7_Division B",
                     "T8" = "T8_Division A",
                     "T9" = "T9_Division B",
                     "T10" = "T10_Division A",
                     "T11" = "T11_Division B",
                     "T12" = "T12_Division A"
                   )
                   
                   # Remplacer les valeurs en utilisant mutate et recode
                   WomenClusters <- WomenClusters %>%
                     mutate(Table = recode(Table, !!!replacement_dict))
                   WomenClusters$Groupe2=paste('Groupe',WomenClusters$Groupe)
                   
                   W.Clusters <- WomenClusters %>%
                     group_by(Table,Tour,Groupe2) %>%
                     summarise(Frequency = n()) %>%
                     ungroup()
                   
                   W.Clusters$Tour <- factor(W.Clusters$Tour, levels = mixedsort(unique(W.Clusters$Tour)))
                   W.Clusters$Table <- factor(W.Clusters$Table, levels = mixedsort(unique(W.Clusters$Table)))
                   W.Clusters$Groupe2 <- factor(W.Clusters$Groupe2, levels = mixedsort(unique(W.Clusters$Groupe2)))
                   
                   output$Clustersplot <- renderPlotly({
                     ggplotly(autoplot(W.Ksurv_WC, groups_by_colour = TRUE,centers = TRUE))
                   })
                   
                   output$Clustersgrille <- renderPlot({
                     ggplot(W.Clusters, aes(fill=Groupe2, y=0, x=factor(0))) + 
                       geom_rect(aes(fill=Groupe2, col=Groupe2, xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf))+
                       facet_grid(Tour~Table)+labs(y='',x='')+
                       labs(title='Profil Général - Women -')+
                       theme_classic()+
                       theme( axis.ticks =element_blank(), axis.text = element_blank() )    
                   })
                 }
                 
               })
  observeEvent(input$quit, 
               {
                 stopApp()
               })
}
shinyApp(ui = ui, server = server)
#------------------------------------------- Fin de Script -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------