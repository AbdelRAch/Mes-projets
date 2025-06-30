library(readr)  
library(stringr)  
library(dplyr)
library(tidyr)
library(ggplot2)
library("clustcurv")
#----------------------------------- Préparation des Données ---------------------------------------------------------------------------------------------------
#-----> Data Men :
                  # DATA_MASS_Start_Men_WC<- DATA_MASS_Start[DATA_MASS_Start$Sex == 'Men' & grepl("ISU World Cup",DATA_MASS_Start$Compétition)== TRUE, ]
                  # identifiant_Men_WC <-unique(DATA_MASS_Start_Men_WC[, c("Titre.de.table", "Jour.et.Heure.competition"),])
#-----> Data Women :
                  # DATA_MASS_Start_Women_WC<- DATA_MASS_Start[DATA_MASS_Start$Sex == 'Women' & grepl("ISU World Cup",DATA_MASS_Start$Compétition)== TRUE, ]
                  # identifiant_Women_WC <-unique(DATA_MASS_Start_Women_WC[, c("Titre.de.table", "Jour.et.Heure.competition"),])
#----------------------------------- Calcule des Ecarts ---------------------------------------------------------------------------------------------------
#-----> Ecart Men :
                  # Ecart_TT_Men_WC <- list()
                  # for (i in 1:nrow(identifiant_Men_WC))
                  # {
                  #   Data <- DATA_MASS_Start_Men_WC[DATA_MASS_Start_Men_WC$Titre.de.table == identifiant_Men_WC[i,"Titre.de.table"] &
                  #                                    DATA_MASS_Start_Men_WC$Jour.et.Heure.competition == identifiant_Men_WC[i,"Jour.et.Heure.competition"],]
                  #   
                  #   ecart_temps_tour <- list()
                  #   
                  #   for (k in c(6, seq(10, 52, by = 3)) ) 
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
                  #         Ecart_TT_Men_WC[[paste0("table",i)]]<-ecart_temps_tour
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
                  #         Ecart_TT_Men_WC[[paste0("table",i)]]<-ecart_temps_tour
                  #       } 
                  #     }
                  #   }
                  # }

#-----> Ecart Women :

                  # Ecart_TT_Women_WC <- list()
                  # for (i in 1:nrow(identifiant_Women_WC))
                  # {
                  #   Data <- DATA_MASS_Start_Women_WC[DATA_MASS_Start_Women_WC$Titre.de.table == identifiant_Women_WC[i,"Titre.de.table"] &
                  #                                    DATA_MASS_Start_Women_WC$Jour.et.Heure.competition == identifiant_Women_WC[i,"Jour.et.Heure.competition"],]
                  #   
                  #   ecart_temps_tour <- list()
                  #   
                  #   for (k in c(6, seq(10, 52, by = 3)) ) 
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

#----------------------------------- Application du Clustering ---------------------------------------------------------------

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
                                        DF.E<-gather(DF[,c(1:16)], key = "Tour.Table", value = "Ecart")
                                        DF.V<-gather(DF[,c(17:32)], key = "Tour.Table", value = "Cont.ind.Var")
                                        DF <- as.data.frame(cbind(DF.E,DF.V))
                                        data_Clust_Men_WC<-rbind(data_Clust_Men_WC,DF)
                                      }
                                      
                                      data_Clust_Men_WC <- data_Clust_Men_WC[,-3] %>%
                                        mutate(Cond.H0.1.Cont.Var = CW)%>%
                                        mutate(Cond.Lmean.Cont.Var = if_else(Cont.ind.Var <= mean(W), 1, 0))
                                      data_Clust_Men_WC<-data_Clust_Men_WC[data_Clust_Men_WC$Ecart<10,]
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
                                        DF.E<-gather(DF[,c(1:16)], key = "Tour.Table", value = "Ecart")
                                        DF.V<-gather(DF[,c(17:32)], key = "Tour.Table", value = "Cont.ind.Var")
                                        DF <- as.data.frame(cbind(DF.E,DF.V))
                                        data_Clust_Women_WC<-rbind(data_Clust_Women_WC,DF)
                                      }
                                      
                                      data_Clust_Women_WC <- data_Clust_Women_WC[,-3] %>%
                                        mutate(Cond.H0.1.Cont.Var = CW)%>%
                                        mutate(Cond.Lmean.Cont.Var = if_else(Cont.ind.Var <= mean(W), 1, 0))
                                      data_Clust_Women_WC<-data_Clust_Women_WC[data_Clust_Women_WC$Ecart<10,]
                                      
#Application du Clustering : 

                          M.Ksurv_WC <- ksurvcurves(time = data_Clust_Men_WC$Ecart, 
                                                    x = data_Clust_Men_WC$Tour.Table,
                                                    status = data_Clust_Men_WC$Cond.Lmean.Cont.Var, 
                                                    algorithm = "kmeans", 
                                                    seed = 300424, k = 6)
                          # summary(M.Ksurv_WC)[5]
                          # print(M.Ksurv_WC)
                          autoplot(M.Ksurv_WC, groups_by_colour = TRUE)
                          autoplot(M.Ksurv_WC, groups_by_colour = TRUE,centers = TRUE)
                          
                          W.Ksurv_WC <- ksurvcurves(time = data_Clust_Women_WC$Ecart, 
                                                    x = data_Clust_Women_WC$Tour.Table,
                                                    status = data_Clust_Women_WC$Cond.Lmean.Cont.Var, 
                                                    algorithm = "kmeans", 
                                                    seed = 300424, k = 6)
#----> 10 Clusters

                  M.10surv_WC <- ksurvcurves(time = data_Clust_Men_WC$Ecart, 
                                             x = data_Clust_Men_WC$Tour.Table,
                                             status = data_Clust_Men_WC$Cond.Lmean.Cont.Var, 
                                             algorithm = "kmeans", 
                                             seed = 300424, k = 10)
                  
                  W.10surv_WC <- ksurvcurves(time = data_Clust_Women_WC$Ecart, 
                                             x = data_Clust_Women_WC$Tour.Table,
                                             status = data_Clust_Women_WC$Cond.Lmean.Cont.Var, 
                                             algorithm = "kmeans", 
                                             seed = 300424, k = 10)
                  autoplot(W.10surv_WC, groups_by_colour = TRUE)
                  autoplot(W.10surv_WC, groups_by_colour = TRUE,centers = TRUE)

#----------------------------------- Visualisation des Clusters ---------------------------------------------------------------

#-----> Clusters Men :

                  Groupes<- cbind(M.Ksurv_WC$levels,M.Ksurv_WC$cluster)
                  colnames(Groupes)<-c("Tour.Table","Groupe")
                  Groupes<-as.data.frame(Groupes)
                  
                  # Compter le nombre de catégories par groupe
                  count_data <- Groupes %>% 
                    count(Groupe, Tour.Table) 
                  
                  # Créer le graphique en barres empilées
                  ggplot(count_data, aes(x = Groupe, y = n, fill = Groupe)) +
                    geom_bar(stat = "identity") +
                    geom_text(aes(label = Tour.Table), position = position_stack(vjust = 0.5))
                  
                  # Créer le nuage de points
                  ggplot(Groupes, aes(x = Tour.Table, y = Groupe,col = Groupe)) +
                      geom_point()+
                      geom_text(aes(label = Tour.Table))
                  
                  
                  heatmap_data <- table(Groupes$Tour.Table, Groupes$Groupe)
                  
                  # Convertir la table en dataframe
                  heatmap_df <- as.data.frame.matrix(heatmap_data)
                  
                  # Tracez la heatmap en utilisant la fonction heatmap()
                  heatmap(as.matrix(heatmap_df), Rowv=NA, Colv=TRUE,scale="column", margins=c(3,4),
                          xlab = "Numéro de Groupe", ylab = "Nom de Catégorie", main = "Heatmap Men")
                  
                  # Divisez vos données en groupes plus petits
                  nb_categories_total <- nrow(heatmap_df)
                  nb_categories_par_groupe <- 48  
                  
                  # Calculez le nombre total de groupes
                  nb_groupes <- ceiling(nb_categories_total / nb_categories_par_groupe)
                  
                  # Tracez une heatmap pour chaque groupe de données
                  par(mfrow=c(1, nb_groupes))  
                  
                  for (i in 1:nb_groupes) {
                    debut <- (i - 1) * nb_categories_par_groupe + 1
                    fin <- min(i * nb_categories_par_groupe, nb_categories_total)
                    
                    heatmap(as.matrix(heatmap_df[debut:fin, ]), Rowv=NA, Colv=TRUE, col=cm.colors(256),
                            scale="column", margins=c(3,4), xlab="Numéro de Groupe", ylab="Nom de Catégorie",
                            main=paste("Heatmap Men - Groupe", i))
                  }
#-----> Clusters Women :

                    W.Groupes<- cbind(W.Ksurv_WC$levels,W.Ksurv_WC$cluster)
                    colnames(W.Groupes)<-c("Tour.Table","Groupe")
                    W.Groupes<-as.data.frame(W.Groupes)
                     
                    
                    # Compter le nombre de catégories par groupe
                    W.count_data <- W.Groupes %>% 
                      count(Groupe, Tour.Table) 
                    
                    # Créer le graphique en barres empilées
                    ggplot(W.count_data, aes(x = Groupe, y = n, fill = Groupe)) +
                      geom_bar(stat = "identity") +
                      geom_text(aes(label = Tour.Table), position = position_stack(vjust = 0.5))
                    
                    # Créer le nuage de points
                    ggplot(W.Groupes, aes(x = Tour.Table, y = Groupe,col = Groupe)) +
                      geom_point()+
                      geom_text(aes(label = Tour.Table))
                    
                    
                    W.heatmap_data <- table(W.Groupes$Tour.Table, W.Groupes$Groupe)
                    
                    # Convertir la table en dataframe
                    W.heatmap_df <- as.data.frame.matrix(W.heatmap_data)
                    
                    # Tracez la heatmap en utilisant la fonction heatmap()
                    heatmap(as.matrix(W.heatmap_df), Rowv=NA, Colv=TRUE,scale="column", margins=c(3,4),
                            xlab = "Numéro de Groupe", ylab = "Nom de Catégorie", main = "Heatmap Women")
                    
                    # Divisez vos données en groupes plus petits
                    W.nb_categories_total <- nrow(W.heatmap_df)
                    W.nb_categories_par_groupe <- 40  
                    
                    # Calculez le nombre total de groupes
                    nb_W.Groupes <- ceiling(W.nb_categories_total / W.nb_categories_par_groupe)
                    
                    # Tracez une heatmap pour chaque groupe de données
                    par(mfrow=c(1, nb_W.Groupes))  
                    
                    for (i in 1:nb_W.Groupes) {
                      debut <- (i - 1) * W.nb_categories_par_groupe + 1
                      fin <- min(i * W.nb_categories_par_groupe, W.nb_categories_total)
                      
                      heatmap(as.matrix(W.heatmap_df[debut:fin, ]), Rowv=NA, Colv=TRUE, col=cm.colors(256),
                              scale="column", margins=c(3,4), xlab="Numéro de Groupe", ylab="Nom de Catégorie",
                              main=paste("Heatmap Women - Groupe", i))
                    }

                    
                    # write.csv(Groupes, "Groupes.csv", row.names = FALSE)
                    # write.csv(Groupes10, "10Groupes.csv", row.names = FALSE)
                   
#------> 10 Clusters
                    #-----> Clusters Men :
# Groupes10<- cbind(M.10surv_WC$levels,M.10surv_WC$cluster)
# colnames(Groupes10)<-c("Tour.Table","Groupe")
# Groupes10<-as.data.frame(Groupes10)

# # Compter le nombre de catégories par groupe
# count_data <- Groupes10 %>% 
#   count(Groupe, Tour.Table) 
 
# # Créer le graphique en barres empilées
# ggplot(count_data, aes(x = Groupe, fill = Tour.Table)) +
#   geom_bar()
 
# # Créer le nuage de points
# ggplot(Groupes10, aes(x = Tour.Table, y = Groupe)) +
#   geom_point()
 
# heatmap_data10 <- table(Groupes10$Tour.Table, Groupes10$Groupe)
 
# # Convertir la table en dataframe
# heatmap_df10 <- as.data.frame.matrix(heatmap_data10)
 
# # Tracez la heatmap en utilisant la fonction heatmap()
# heatmap(as.matrix(heatmap_df10), Rowv=NA, Colv=TRUE,scale="column",col=cm.colors(192), margins=c(3,4),
#         xlab = "Numéro de Groupe", ylab = "Nom de Catégorie", main = "Heatmap")
 
# # Tracez une heatmap pour chaque groupe de données
# par(mfrow=c(1, nb_groupes)) 
 
# for (i in 1:nb_groupes) {
#   debut <- (i - 1) * nb_categories_par_groupe + 1
#   fin <- min(i * nb_categories_par_groupe, nb_categories_total)
   
#   heatmap(as.matrix(heatmap_df10[debut:fin, ]), Rowv=NA, Colv=TRUE, col=cm.colors(256),
#           scale="column", margins=c(3,4), xlab="Numéro de Groupe", ylab="Nom de Catégorie",
#           main=paste("Heatmap - Groupe", i))
# }

#------> 10 Clusters
                    #-----> Clusters Women :

# W.Groupes10<- cbind(W.10surv_WC$levels,W.10surv_WC$cluster)
# colnames(W.Groupes10)<-c("Tour.Table","Groupe")
# W.Groupes10<-as.data.frame(W.Groupes10)
# # Compter le nombre de catégories par groupe
# W.count_data <- W.Groupes10 %>% 
#   count(Groupe, Tour.Table) 
 
# # Créer le graphique en barres empilées
# ggplot(W.count_data, aes(x = Groupe, fill = Tour.Table)) +
#   geom_bar()

# # Créer le nuage de points
# ggplot(W.Groupes10, aes(x = Tour.Table, y = Groupe)) +
#   geom_point()
 
# W.heatmap_data <- table(W.Groupes10$Tour.Table, W.Groupes10$Groupe)
 
# # Convertir la table en dataframe
# W.heatmap_df <- as.data.frame.matrix(W.heatmap_data)
 
# # Tracez la heatmap en utilisant la fonction heatmap()
# heatmap(as.matrix(W.heatmap_df), Rowv=NA, Colv=TRUE,scale="column", margins=c(3,4),
#         xlab = "Numéro de Groupe", ylab = "Nom de Catégorie", main = "Heatmap Women")

# # Tracez une heatmap pour chaque groupe de données
# par(mfrow=c(1, nb_W.Groupes))  
# 
# for (i in 1:nb_W.Groupes) {
#   debut <- (i - 1) * W.nb_categories_par_groupe + 1
#   fin <- min(i * W.nb_categories_par_groupe, W.nb_categories_total)
#   
#   heatmap(as.matrix(W.heatmap_df[debut:fin, ]), Rowv=NA, Colv=TRUE, col=cm.colors(256),
#           scale="column", margins=c(3,4), xlab="Numéro de Groupe", ylab="Nom de Catégorie",
#           main=paste("Heatmap Women - Groupe", i))
# }