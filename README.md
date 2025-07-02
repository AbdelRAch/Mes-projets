# Mes-projets
# Description des dossiers et projets

---

## APP Final

Application Shiny pour visualiser consommation et production électrique à pas demi-horaire ou quotidien, par région.

### Fonctionnalités principales

- 3 onglets : consommation < 36 kVA, consommation ≥ 36 kVA, production  
- Sélections multiples : périodes, régions, profils, plages de puissance, secteurs (sup36), filières (production)  
- Pas temporel : demi-horaire ou quotidien  
- Visualisations interactives colorées selon profil, région, secteur ou filière  
- Carte interactive pour visualisation spatiale des données par région  
- Indicateurs clés : somme, moyenne, max, date du max  
- Export des données affichées

---

# Stage FFRS – Analyse de la Vitesse sur Glace

---

## 📊 Clustering

### `Clustering_By_Cont_Ind_Var.R`
- Analyse des rythmes de tours en compétition.
- Identification de groupes d’athlètes pour adapter les stratégies de course.

### `Visualisation_groupes.R`
- Visualisation des groupes identifiés selon les compétitions et les différents tours.

---

## 🧊 ICE Speed Analysis

### `App_Global.R`
Application interactive combinant web scraping, visualisation et analyse statistique.

**Fonctionnalités :**
- Récupération automatique des résultats de compétitions via un lien fourni par l’utilisateur.
- Intégration des résultats dans une base de données.
- Visualisation des rangs ou temps pour un ou plusieurs athlètes, selon la nationalité et la compétition.
- Comparaison des performances et des stratégies de patinage.
- Analyse de la fonction de répartition et de densité des écarts entre le premier athlète et les suivants, pour mieux comprendre le rythme des courses.

---

## 📄 Scraping de PDF

### `SCRAPING_Interactif_Plus.R`
- Extraction interactive de données depuis des fichiers PDF.
- Possibilité de modifier l'ordre, ajouter/supprimer des colonnes ou lignes, pour structurer correctement les résultats.

### `DataFFRS.Viz.R`
- Visualisation multi-échelle des données extraites.
- Filtres et critères personnalisés pour explorer les résultats selon plusieurs dimensions.
---

## Projets Machine Learning & IA

Ce dossier contient plusieurs notebooks explorant des problématiques variées :

### 1. Classification d’images (Deep Learning)

- Dataset Stanford Dogs, 5 races ciblées  
- CNN entraîné from scratch + data augmentation  
- Transfer learning (feature extraction & fine tuning)  
- Évaluation sur échantillon test (25%)

### 2. Classification d’articles de presse (NLP avancé)

- Catégorisation en Économie, Sport, Science  
- Prétraitement textuel (nettoyage, tokenisation, stopwords)  
- Approches TF-IDF, Word2Vec, BERT  
- Modèles sur titre seul, titre+description, texte complet  
- Analyse des probabilités de classification et erreurs

### 3. Classification âge coquilles d’ormeaux (ML tabulaire)

- Dataset UCI Abalone  
- Catégorisation en classes d’âge : Jeune, Adulte, Vieux  
- Modèles : Régression Logistique, k-NN, Random Forest  
- Recherche d’hyperparamètres, évaluation complète (accuracy, F1, matrices de confusion)  
- Random Forest performe le mieux (~62.7% accuracy)

### 4. Prédiction de l’âge des ormeaux (Régression)

- Dataset UCI Abalone  
- Objectif : prédire l’âge d’un ormeau à partir de caractéristiques physiques  
- Encodage de la variable catégorielle "Sex" via one-hot encoding  
- Modèles testés : Arbre de Décision (GridSearchCV), Random Forest, Bagging Regressor, Gradient Boosting  
- Comparaison finale : KNN, SVM, Decision Tree, Random Forest  
- Métriques d’évaluation : MSE, MAE, R², courbes d’apprentissage (par nombre de variables)  
- Observations clés :  
  - Random Forest offre les meilleurs résultats globaux (MSE ~3.98, R² ~0.60)  
  - SVM est le plus précis selon la MAE (~1.45), robuste aux petites erreurs  
  - Performances optimales avec 5 à 7 variables (surapprentissage léger au-delà)  
  - Variables les plus influentes : Shell_weight, Whole_weight, Height  
- Visualisations :  
  - Importance des variables  
  - Évolution MSE / R² (train vs test)  
  - Âge réel vs Âge prédit (scatter plots comparant les modèles)
