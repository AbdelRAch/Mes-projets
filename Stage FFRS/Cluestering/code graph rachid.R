library(stringr)
library(ggplot2)
library(dplyr)
library(gtools)

rachid=read.csv('10Groupes.csv',header=T)



for(i in 1:nrow(rachid)){

rachid$Tour[i]=str_split(rachid$Tour.Table,"\\.")[[i]][1]
rachid$Table[i]=str_split(rachid$Tour.Table,"\\.")[[i]][2]

}

rachid$Groupe2=paste('Groupe',rachid$Groupe)

tab1 <- rachid %>%
  group_by(Tour,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

tab1$Tour <- factor(tab1$Tour, levels = mixedsort(unique(tab1$Tour)))
tab1$Groupe2 <- factor(tab1$Groupe2, levels = mixedsort(unique(tab1$Groupe2)))

########


ggplot(tab1, aes(fill=Groupe2, y=Frequency, x=Tour)) + 
  geom_bar(position="stack", stat="identity")+labs(title='Profil des tours')+
  theme_classic()





##################### Profil des tables


tab2 <- rachid %>%
  group_by(Table,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

tab2$Table <- factor(tab2$Table, levels = mixedsort(unique(tab2$Table)))
tab2$Groupe2 <- factor(tab2$Groupe2, levels = mixedsort(unique(tab2$Groupe2)))

########


ggplot(tab2, aes(fill=Groupe2, y=Frequency, x=Table)) + 
  geom_bar(position="stack", stat="identity")+labs(title='Profil des tables')+
  theme_classic()


####### Profil général 



tab3 <- rachid %>%
  group_by(Table,Tour,Groupe2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

tab3$Tour <- factor(tab3$Tour, levels = mixedsort(unique(tab3$Tour)))
tab3$Table <- factor(tab3$Table, levels = mixedsort(unique(tab3$Table)))
tab3$Groupe2 <- factor(tab3$Groupe2, levels = mixedsort(unique(tab3$Groupe2)))

########


ggplot(tab3, aes(fill=Groupe2, y=0, x=factor(0))) + 
  geom_rect(aes(fill=Groupe2, col=Groupe2, xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf))+
  facet_grid(Tour~Table)+labs(y='',x='')+
  theme_classic()+
  theme( axis.ticks =element_blank(), axis.text = element_blank() )









View(rachid)