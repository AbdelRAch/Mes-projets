# Installer le package
install.packages("clustcurv_2.0.1.tar.gz", repos = NULL, type = "source")

# Charger la bibliothèque clustcurv
library("clustcurv")

# Vérifier les fonctions disponibles dans le package clustcurv
ls("package:clustcurv")

# Charger la bibliothèque condSURV
library(condSURV)


#------------------------------------------- Exemple de Documentation ------------------------------------------------

# Utiliser l'ensemble de données gbcsCS et afficher les premières lignes
data(gbcsCS)
head(gbcsCS[, c(5:10, 13, 14)])

# Effectuer une analyse de clustering des courbes de survie avec l'algorithme k-medians
fit.gbcs <- survclustcurves(time = gbcsCS$rectime, status = gbcsCS$censrec,
                            x = gbcsCS$nodes, nboot = 500, seed = 300716, algorithm = 'kmedians',
                            cluster = TRUE)

# Afficher un résumé des résultats de l'analyse
summary(fit.gbcs)

# Créer un graphique représentant les courbes de survie
autoplot(fit.gbcs, groups_by_colour = FALSE)
autoplot(fit.gbcs, groups_by_colour = TRUE)

# Effectuer une analyse de clustering des courbes de survie avec l'algorithme k-means
fit.gbcs <- survclustcurves(time = gbcsCS$rectime, status = gbcsCS$censrec,
                            x = gbcsCS$nodes, nboot = 500, seed = 300716, algorithm = 'kmeans',
                            cluster = TRUE)

# Utiliser la fonction ksurvcurves pour visualiser les courbes de survie pour chaque cluster
ksurvcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, x = gbcsCS$nodes,
            seed = 300716, algorithm = 'kmedians', k = 3)

# Charger la bibliothèque survminer
library(survminer)

# Utiliser l'ensemble de données myeloma et afficher les premières lignes
data(myeloma)
head(myeloma[,1:5])

# Effectuer une analyse de clustering des courbes de survie pour les données myeloma
fit.mye <- survclustcurves(time = myeloma$time, status = myeloma$event,
                           x = myeloma$molecular_group, nboot = 500, seed = 300716,
                           algorithm = 'kmedians', cluster = TRUE)

# Effectuer une autre analyse de clustering des courbes de survie pour les données myeloma
fit.mye2 <- survclustcurves(time = myeloma$time, status = myeloma$event,
                            x = myeloma$molecular_group, nboot = 500, seed = 300716,
                            algorithm = 'kmeans', cluster = TRUE)

# Créer un graphique représentant les courbes de survie pour les données myeloma
autoplot(fit.mye, groups_by_color = TRUE)

# Effectuer une analyse de clustering des courbes de survie pour les données barnacle5
fit.bar <- regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                          nboot = 500, seed = 300716, algorithm = 'kmeans', cluster = TRUE)

# Créer un graphique représentant les courbes de survie pour les données barnacle5
autoplot(fit.bar, groups_by_color = TRUE)

# Définir une fonction m(x, j)
m <- function(x, j) {
  y <- numeric(length(x))
  y[j <= 5] <- x[j <= 5] + 1
  y[j > 5 & j <= 10] <- x[j > 5 & j <= 10] ^ 2 + 1
  y[j > 10 & j <= 15] <- 2 * sin(2 * x[j > 10 & j <= 15]) #- 4
  y[j > 15 & j <= 20] <- 2 * sin(x[j > 15 & j <= 20])
  y[j > 20 & j <= 25] <- 2 * sin(x[j > 20 & j <= 25]) + a * exp(x[j > 20 & j <= 25])
  y[j > 25] <- 1
  return(y)
}

# Définir la graine aléatoire
seed <- 300716
set.seed(seed)

# Générer des données simulées
n <- 5000
a <- 0.0
x <- runif(n, -2, 2)
prob <- sample(c(1, 1.5, 2, 2.5, 3), 30, replace = TRUE)
prob <- prob/sum(prob)
f <- sample(1:30, n, replace = TRUE, prob = prob)
N <- length(unique(f))
error <- rnorm(n,0,1.5)
y <- m(x, f) + (0.5 + 0.05 * m(x, f)) * error
data <- data.frame(x, y, f)

# Effectuer une analyse de clustering des courbes de régression pour les données simulées
fit.sim <- regclustcurves(x = data$x, y = data$y, z = data$f, nboot = 500,
                          algorithm = 'kmedians', cluster = TRUE, seed = 300716)

# Afficher les résultats de l'analyse de clustering
fit.sim

# Créer un graphique représentant les courbes de régression pour les données simulées
autoplot(fit.sim, groups_by_colour = TRUE, centers = TRUE)

# Réinitialiser la graine aléatoire
seed <- 300716
set.seed(seed)

# Générer d'autres données simulées avec une valeur différente pour 'a'
n <- 5000
a <- 0.4
x <- runif(n, -2, 2)
prob <- sample(c(1, 1.5, 2, 2.5, 3), 30, replace = TRUE)
prob <- prob/sum(prob)
f <- sample(1:30, n, replace = TRUE, prob = prob)
N <- length(unique(f))
error <- rnorm(n,0,1.5)
y <- m(x, f) + (0.5 + 0.05 * m(x, f)) * error
data2 <- data.frame(x, y, f)

# Effectuer une analyse de clustering des courbes de régression pour les nouvelles données simulées
fit.sim2 <- regclustcurves(x = data2$x, y = data2$y, nboot = 500, seed = 300716,
                           z = data$f, algorithm = 'kmedians', cluster = TRUE)

# Afficher les résultats de l'analyse de clustering
fit.sim2

# Créer un graphique représentant les courbes de régression pour les nouvelles données simulées
autoplot(fit.sim2, groups_by_colour = TRUE, centers = TRUE)
