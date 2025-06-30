library(stringr)
library(ggplot2)
library(dplyr)
library(gtools)

#-----> Clusters Men :

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

M.Tours.Clusters <- MenClusters %>%
  group_by(Tour,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

M.Tours.Clusters$Tour <- factor(M.Tours.Clusters$Tour, levels = mixedsort(unique(M.Tours.Clusters$Tour)))
M.Tours.Clusters$Groupe2 <- factor(M.Tours.Clusters$Groupe2, levels = mixedsort(unique(M.Tours.Clusters$Groupe2)))

########

ggplot(M.Tours.Clusters, aes(fill=Groupe2, y=Frequency, x=Tour)) + 
  geom_bar(position="stack", stat="identity")+labs(title='Profil des tours - Men -')+
  theme_classic()

##################### Profil des tables

M.Tables.Clusters <- MenClusters %>%
  group_by(Table,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

M.Tables.Clusters$Table <- factor(M.Tables.Clusters$Table, levels = mixedsort(unique(M.Tables.Clusters$Table)))
M.Tables.Clusters$Groupe2 <- factor(M.Tables.Clusters$Groupe2, levels = mixedsort(unique(M.Tables.Clusters$Groupe2)))
########

ggplot(M.Tables.Clusters, aes(fill=Groupe2, y=Frequency, x=Table)) + 
  geom_bar(position="stack", stat="identity")+labs(title='Profil des tables - Men -')+
  theme_classic()

####### Profil général 

M.Clusters <- MenClusters %>%
  group_by(Table,Tour,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

M.Clusters$Tour <- factor(M.Clusters$Tour, levels = mixedsort(unique(M.Clusters$Tour)))
M.Clusters$Table <- factor(M.Clusters$Table, levels = mixedsort(unique(M.Clusters$Table)))
M.Clusters$Groupe2 <- factor(M.Clusters$Groupe2, levels = mixedsort(unique(M.Clusters$Groupe2)))

########

ggplot(M.Clusters, aes(fill=Groupe2, y=0, x=factor(0))) + 
  geom_rect(aes(fill=Groupe2, col=Groupe2, xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf))+
  facet_grid(Tour~Table)+labs(y='',x='')+
  labs(title='Profil Général - Men -')+
  theme_classic()+
  theme( axis.ticks =element_blank(), axis.text = element_blank() )

#--------------------------------------------------------- Women Clusters --------------------------------------------------------------------------
W.Groupes10<- cbind(W.10surv_WC$levels,W.10surv_WC$cluster)
colnames(W.Groupes10)<-c("Tour.Table","Groupe")
W.Groupes10<-as.data.frame(W.Groupes10)

WomenClusters= W.Groupes10

for(i in 1:nrow(WomenClusters)){
  
  WomenClusters$Tour[i]=str_split(WomenClusters$Tour.Table,"\\.")[[i]][1]
  WomenClusters$Table[i]=str_split(WomenClusters$Tour.Table,"\\.")[[i]][2]
  
}

replacement_dict <- c(
  "T1" = "T1_Division A",
  "T2" = "T2_Division A",
  "T3" = "T2_Division B",
  "T4" = "T4_Division A",
  "T5" = "T5_Division B",
  "T6" = "T6_Division A",
  "T7" = "T7_Division B",
  "T8" = "T8_Division A",
  "T9" = "T9_Division B",
  "T10" = "T10_Division A"
)

# Remplacer les valeurs en utilisant mutate et recode
WomenClusters <- WomenClusters %>%
  mutate(Table = recode(Table, !!!replacement_dict))


WomenClusters$Groupe2=paste('Groupe',WomenClusters$Groupe)

W.Tours.Clusters <- WomenClusters %>%
  group_by(Tour,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

W.Tours.Clusters$Tour <- factor(W.Tours.Clusters$Tour, levels = mixedsort(unique(W.Tours.Clusters$Tour)))
W.Tours.Clusters$Groupe2 <- factor(W.Tours.Clusters$Groupe2, levels = mixedsort(unique(W.Tours.Clusters$Groupe2)))

########

ggplot(W.Tours.Clusters, aes(fill=Groupe2, y=Frequency, x=Tour)) + 
  geom_bar(position="stack", stat="identity")+labs(title='Profil des tours -Women-')+
  theme_classic()

##################### Profil des tables

W.Tables.Clusters <- WomenClusters %>%
  group_by(Table,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

W.Tables.Clusters$Table <- factor(W.Tables.Clusters$Table, levels = mixedsort(unique(W.Tables.Clusters$Table)))
W.Tables.Clusters$Groupe2 <- factor(W.Tables.Clusters$Groupe2, levels = mixedsort(unique(W.Tables.Clusters$Groupe2)))

########

ggplot(W.Tables.Clusters, aes(fill=Groupe2, y=Frequency, x=Table)) + 
  geom_bar(position="stack", stat="identity")+labs(title='Profil des tables -Women-')+
  theme_classic()

####### Profil général 

W.Clusters <- WomenClusters %>%
  group_by(Table,Tour,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

W.Clusters$Tour <- factor(W.Clusters$Tour, levels = mixedsort(unique(W.Clusters$Tour)))
W.Clusters$Table <- factor(W.Clusters$Table, levels = mixedsort(unique(W.Clusters$Table)))
W.Clusters$Groupe2 <- factor(W.Clusters$Groupe2, levels = mixedsort(unique(W.Clusters$Groupe2)))

########

ggplot(W.Clusters, aes(fill=Groupe2, y=0, x=factor(0))) + 
  geom_rect(aes(fill=Groupe2, col=Groupe2, xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf))+
  facet_grid(Tour~Table)+labs(y='',x='')+labs(title='Profil Général -Women-')+
  theme_classic()+
  theme( axis.ticks =element_blank(), axis.text = element_blank() )