# Chargement des bibliothèques nécessaires
library(dplyr)
library(lubridate)
library(hms)
library(data.table)
library(janitor)
library(sf)

# Fonction de chargement, nettoyage et transformation des données
#' load_data
#'
#' Cette fonction charge plusieurs jeux de données CSV, les nettoie et les transforme.
#' Elle inclut la conversion des formats de dates et d'heures, la gestion des valeurs manquantes.
#' Elle charge également un fichier geojson contenant les informations géographiques des régions.
#'
#' @return Une liste contenant les jeux de données nettoyés et transformés :
#'   - conso_sup36 : Données de consommation d'éléctricité supérieure à 36 KWH
#'   - conso : Données de consommation d'éléctricité inférieur à 36 KWH
#'   - df : Données de production d'éléctricité
#'   - regions_geo : Données géographiques des régions (code des régions)
#' 
#' @details Cette fonction prend les fichiers CSV suivants en entrée :
#'   - "conso-sup36-region.csv"
#'   - "conso-inf36-region.csv"
#'   - "prod-region.csv"
#' Et un fichier geojson :
#'   - "regions.geojson" pour les informations géographiques des régions.
#' 
#' Les étapes de traitement incluent :
#'   - Lecture des fichiers CSV avec `fread()` et nettoyage des noms de colonnes via `clean_names()`.
#'   - Transformation des dates et des heures avec `lubridate` et `hms`.
#'   - Filtrage des lignes avec `complete.cases()` pour supprimer les valeurs manquantes.
#'   - Sélection des colonnes pertinentes et réarrangement des données par date (`horodate_clean`).
#'   - Lecture des données géographiques avec `st_read()` et conversion du code des régions en numérique.
#'
#' @examples
#' data <- load_data()
#' str(data)

load_data <- function() {
  
  # ----------------------------
  # Chargement des données
  # ----------------------------
  conso_sup36 <- fread("data/conso-sup36-region.csv") %>% clean_names()
  conso <- fread("data/conso-inf36-region.csv") %>% clean_names()
  df <- fread("data/prod-region.csv") %>% clean_names()
  
  # ----------------------------
  # Nettoyage et transformation des données de production (df)
  # ----------------------------
  df <- df[complete.cases(df),] %>%
    mutate(
      # Transformation de la colonne horodate en datetime et ajout de 2 heures
      horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
      date = as.Date(horodate_clean),
      heure = as_hms(format(horodate_clean, format = "%H:%M:%S")),
      total_energie_injectee_wh = as.numeric(total_energie_injectee_wh),
      courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
      courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
    ) %>%
    select(c(16:18, 2:15))  # Sélection des colonnes d'intérêt
  
  # ----------------------------
  # Nettoyage et transformation des données de consommation inf à 36KWH (conso)
  # ----------------------------
  conso <- conso[complete.cases(conso),] %>%
    mutate(
      # Transformation de la colonne horodate en datetime
      horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
      date = as.Date(horodate_clean),
      heure = as_hms(format(horodate_clean, format = "%H:%M:%S")),
      total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
      courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
      courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
    ) %>%
    select(c(16:18, 2:15))  # Sélection des colonnes d'intérêt
  
  # ----------------------------
  # Nettoyage et transformation des données de consommation sup à 36 KWH (conso_sup36)
  # ----------------------------
  conso_sup36 <- conso_sup36[complete.cases(conso_sup36),] %>%
    mutate(
      # Transformation de la colonne horodate en datetime
      horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
      date = as.Date(horodate_clean),
      heure = format(horodate_clean, format = "%H:%M:%S"),
      total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
      courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
      courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
    ) %>%
    select(c(17:19, 2:16))  # Sélection des colonnes d'intérêt
  
  # ----------------------------
  # Tri des données par horodate (pour assurer l'ordre chronologique)
  # ----------------------------
  df <- df %>% arrange(horodate_clean)
  conso <- conso %>% arrange(horodate_clean)
  conso_sup36 <- conso_sup36 %>% arrange(horodate_clean)
  
  # ----------------------------
  # Chargement des données géographiques des régions
  # ----------------------------
  regions_geo <- st_read("data/regions.geojson")
  regions_geo$code <- as.numeric(regions_geo$code)  # Conversion du code des régions en numérique
  
  # ----------------------------
  # Retour des données sous forme de liste
  # ----------------------------
  return(list(conso_sup36 = conso_sup36, conso = conso, df = df, regions_geo = regions_geo))
}
