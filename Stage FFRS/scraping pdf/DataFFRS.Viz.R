#------------------------------------------- LES LIBRAIRIES -----------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)   
library(tidyr)   
library(grid)
library(ggplot2)
library(shinyjs)
library(shinydashboard)
library(gridExtra)
#------------------------------------------ Importation des Données -----------------------------------------------------------------------------------
data <- read_excel("Base de données resultats globale FFRS.xlsx")
data <- as.data.frame(data)%>%
  distinct()
#------------------------------------------- Implémentation de l'appli Shiny -----------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "FFRS Stats"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data par discipline", tabName = "data_discipline", icon = icon("chart-line")),
      menuItem("Data interne FFRS", tabName = "data_ffrs", icon = icon("home")),
      menuItem("Data Globale Internationale", tabName = "data_internationale", icon = icon("globe")),
      menuItem("Discipline de haut niveau",tabName = "DHN",icon = icon("trophy"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data par discipline
      tabItem(tabName = "data_discipline",
              tabBox(
                id = "tabset1", width = "100%",height = "100%",
                tabPanel("Visualisation des résultats", 
                         selectInput("discipline1", "Choisissez une discipline:", choices = unique(data[data$Nationalité %in% c("France","FRA"),]$Discipline)),
                         plotOutput("plot1")),
                tabPanel("Classement des athlètes", 
                         selectInput("discipline2", "Choisissez une discipline:", choices = unique(data[data$Nationalité %in% c("France","FRA"),]$Discipline)),
                         tableOutput("table1"))
              )
      ),
      # Data Globale FFRS
      tabItem(tabName = "data_ffrs",
              tabBox(
                id = "tabset2",width = "100%",height = "120%",
                tabPanel("Statistique FFRS", 
                         #selectInput("discipline3", "Choisissez une discipline:", choices = unique(data$Discipline)),
                         plotOutput("plot2")),
                tabPanel("Médailles FFRS", 
                         #selectInput("discipline3", "Choisissez une discipline:", choices = unique(data$Discipline)),
                         plotOutput("plot3")),
                tabPanel("Classement des athlètes", 
                         #selectInput("discipline4", "Choisissez une discipline:", choices = unique(data$Discipline)),
                         tableOutput("table2"))
              )
      ),
      # Data Globale Internationale
      tabItem(tabName = "data_internationale",
              tabBox(
                id = "tabset3", width = "100%",height = "100%",
                tabPanel("Statistiques Générales",
                         fluidRow(
                           column(4, selectInput("discipline5", "Choisissez une discipline :", choices = unique(data$Discipline))),
                           column(4, selectInput("Nat", "Choisissez une Nationalité :", choices = NULL, multiple = TRUE), offset = 1)
                         ),
                         plotOutput("plot4")),
                tabPanel("Distribution des Médailles", 
                         selectInput("discipline6", "Choisissez une discipline:", choices = unique(data$Discipline)),
                         plotOutput("plot5")),
                tabPanel("Classement des nations", 
                         selectInput("discipline7", "Choisissez une discipline:", choices = unique(data$Discipline)),
                         tableOutput("table3"))
              )
      ),
      tabItem(tabName = "DHN",
              tableOutput("tabledhn")
      )
    )
  )
)

# Server
server <- function(input, output,session) {
  output$plot1 <- renderPlot({
    # Subset data for the selected discipline and French athletes
    desciplinefr <- data[data$Discipline == input$discipline1 & data$Nationalité %in% c("France", "FRA"),]
    
    athletes_uniquefr <- desciplinefr %>%
      select(Athlète, Sexe, `catégorie d’âge`, Epreuve) %>%
      distinct() %>%
      group_by(Sexe, Epreuve) %>%
      summarise(Count = n(), .groups = 'drop')
    
    table_age_discipline <- desciplinefr %>%
      mutate(Sexe = case_when(
        Sexe == "Femme" ~ "F",
        Sexe == "Homme" ~ "H",
        TRUE ~ Sexe
      ))%>%
      distinct() %>%
      group_by(`catégorie d’âge`, Epreuve, Sexe) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    table_epreuve_med <- desciplinefr %>%
      mutate(`Type de compétition` = case_when(
        `Type de compétition` == "European Championships" ~ "EC",
        `Type de compétition` == "Pro Tour" ~ "PT",
        `Type de compétition` == "World Championships" ~ "WC",
        TRUE ~ `Type de compétition`
      )) %>%
      group_by(Classement, Epreuve, `Type de compétition`) %>%
      distinct()%>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      filter(Classement %in% c(1, 2, 3))
    
    # Plot 1: Répartition des Athlètes
    plot1 <- ggplot(athletes_uniquefr, aes(x = "", y = Count, fill = paste(Epreuve,Sexe) )) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Répartition des Athlètes", fill= "Epreuve, Sexe") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "left")+
      geom_text(aes(label = Count), 
                position = position_stack(vjust = 0.5))
    
    # Plot 2: Participation par Catégorie d'Âge
    plot2 <- ggplot(table_age_discipline, aes(x = "", y = Percentage, fill = paste(Epreuve,`catégorie d’âge`,Sexe))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Participation par Catégorie d'Âge", , fill = "Epreuve, Catégorie d’âge ,Sexe") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "left")+
      geom_text(aes(label = Count), 
                position = position_stack(vjust = 0.5))
    
    # Plot 3: Distribution des Médailles
    
    clrs <- c("1" = "#FFD700",  # Or
                           "2" = "#C0C0C0",  # Argent
                           "3" = "#CD7F32")  # Bronze
    
    # Créer le graphique
    plot3 <- ggplot(table_epreuve_med, aes(x = "", y = Count, fill = as.factor(Classement))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void(base_size = 16) +  # Augmente la taille de base du texte
      labs(title = "Distribution des Médailles", fill = "Classement") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "left") +
      geom_text(aes(label = paste(Epreuve, ": ", Count, " (", `Type de compétition`, ")")), 
                position = position_stack(vjust = 0.5),
                angle = 45) +
      scale_fill_manual(values = clrs)
    
    # Combine the three plots using grid.arrange
    gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 2)
  },height = 1000, width = 1200)
  
  output$table1 <- renderTable({
    table_epreuve_med <- data[data$Discipline == input$discipline2 & data$Nationalité %in% c("France", "FRA"),] %>%
      group_by(Athlète, Epreuve, `Type de compétition`,`Nom de la compétition`)%>%
      distinct() %>%
      summarise(
        `1er` = sum(Classement == "1"),
        `2eme` = sum(Classement == "2"),
        `3eme` = sum(Classement == "3"), .groups = 'drop'
      ) %>%
      arrange(desc(`1er`), desc(`2eme`), desc(`3eme`))
    
    table_epreuve_med[,c("Athlète","1er","2eme","3eme")]
  })
  
  output$plot2 <- renderPlot({
    datafr <- data %>% filter(Nationalité %in% c("France", "FRA"))
    
    athletes_uniquefr <- datafr %>%
      select(Athlète, Sexe, `catégorie d’âge`, Discipline) %>%
      distinct()
    
    table_sexe <- athletes_uniquefr %>%
      group_by(Sexe) %>%
      summarise(Count = n(), .groups = 'drop')
    
    table_sexe_discipline <- athletes_uniquefr %>%
      group_by(Sexe, Discipline) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    table_age_discipline <- datafr %>%
      mutate(Sexe = case_when(
        Sexe == "Femme" ~ "F",
        Sexe == "Homme" ~ "H",
        TRUE ~ Sexe
      )) %>%
      group_by(`catégorie d’âge`, Discipline, Epreuve, Sexe) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    table_comp_discipline <- datafr %>%
      group_by(`Type de compétition`, Discipline, Epreuve, Sexe,`catégorie d’âge`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    # Camembert pour la distribution des athlètes par sexe
    p1 <- ggplot(table_sexe, aes(x = "", y = Count, fill = Sexe)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void(base_size = 16) +  # Augmente la taille de base du texte
      labs(title = "Distribution des Athlètes par Sexe") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "left") +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 5) 
    
    p2 <- ggplot(table_age_discipline, aes(x = Discipline, y = Count, fill = interaction(`catégorie d’âge`, Sexe))) +
          geom_bar(stat = "identity") +
          theme_minimal(base_size = 18) +
          labs(title = paste("Participation par Catégorie d'Âge"), x = "Discipline", y = "nombre d'athlètes",fill="Catégorie d’âge, Sexe") +
          theme(plot.title = element_text(hjust = 0.5),
                legend.position = "left",
                axis.text.x = element_text(angle = 30, hjust = 1)
                ) + 
          geom_text(aes(label = paste(Epreuve,": ",Count)), position = position_stack(vjust = 0.5), size = 4)
    
    create_pie_chart_disc <- function(disci) {
      data_filtered <- table_sexe_discipline %>% filter(Discipline == disci)
      
      ggplot(data_filtered, aes(x = "", y = Percentage, fill = Sexe)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void(base_size = 16) +
        labs(title = paste("Répartition des Athlètes dans : ", disci)) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "left") +
        geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 5)
    }
    
    Disci <- unique(table_sexe_discipline$Discipline)
    pie_charts_disc <- lapply(Disci, create_pie_chart_disc)
    
    # Fonction pour créer un barplot pour un type de compétition spécifique
    create_bar_chart_comp <- function(category) {
      data_filtered <- table_comp_discipline %>% filter(`Type de compétition` == category)
      
      ggplot(data_filtered, aes(x = Discipline, y = Count, fill =paste(`catégorie d’âge`,Sexe))) +
        geom_bar(stat = "identity") +
        theme_minimal(base_size = 16) +
        labs(title = paste("Participation par Compétition:", category), x = "Discipline", y = "nombre d'athlètes", fill = "catégorie d’âge, Sexe") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "left") +
        geom_text(aes(label = paste(Epreuve,": ",Count)), position = position_stack(vjust = 0.5), size = 4)
    }
    
    competition <- unique(table_comp_discipline$`Type de compétition`)
    bar_charts_comp <- lapply(competition, create_bar_chart_comp)
    
    all_graphe <- c(list(p1), list(p2), pie_charts_disc, bar_charts_comp)#bar_charts_age
    
    # Ajuster la taille globale de la grille
    do.call(grid.arrange, c(all_graphe, ncol = 2, top = "Analyse des Athlètes"))
  }, height = 1500, width = 1420)  # Ajuste la taille de la figure globale
  
  output$plot3 <- renderPlot({
    datafr <- data %>% filter(Nationalité %in% c("France","FRA"))
  
    table_discipline_epreuve_med <- datafr %>%
      mutate(`Type de compétition` = case_when(
        `Type de compétition` == "European Championships" ~ "EC",
        `Type de compétition` == "Pro Tour" ~ "PT",
        `Type de compétition` == "World Championships" ~ "WC",
        TRUE ~ `Type de compétition`
      )) %>%
      group_by(Classement, Discipline, Epreuve, `Type de compétition`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      filter(Classement %in% c(1, 2, 3))
    
    # Fonction pour créer un camembert pour une discipline spécifique
    create_pie_chart_disc_med <- function(disc) {
      data_filtered <- table_discipline_epreuve_med %>%
        filter(Discipline == disc) %>%
        group_by(Classement, Epreuve, `Type de compétition`) %>%
        summarise(Count = sum(Count), .groups = 'drop') %>%
        mutate(Percentage = Count / sum(Count) * 100)
      
      classement_colors <- c("1" = "#FFD700",  # Or
                "2" = "#C0C0C0",  # Argent
                "3" = "#CD7F32")  # Bronze
      
      ggplot(data_filtered, aes(x = "", y = Count, fill = as.factor(Classement))) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void(base_size=16) +
        labs(title = paste("Médailles :", disc),fill= "Classement") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "left") +
        geom_text(aes(label = paste(Epreuve, ": ", Count, " (", `Type de compétition`, ")")), 
                  position = position_stack(vjust = 0.5),
                  angle = 45)+
        scale_fill_manual(values = classement_colors)
    }
    
    disc <- unique(table_discipline_epreuve_med$Discipline)
    pie_charts_disc_med <- lapply(disc, create_pie_chart_disc_med)
    
    # Combiner les graphiques dans une seule grille
    do.call(grid.arrange, c(pie_charts_disc_med, ncol = 2))
  }, height = 1500, width = 1420)
  
  output$table2 <- renderTable({
    datafr <- data %>% filter(Nationalité %in% c("France","FRA"))
    table_epreuve_med <- datafr %>%
      group_by(Athlète,Discipline, Epreuve, `Type de compétition`, `Nom de la compétition`) %>%
      distinct() %>%
      summarise(
        `1er` = sum(Classement == 1),
        `2eme` = sum(Classement == 2),
        `3eme` = sum(Classement == 3), .groups = 'drop'
      ) %>%
      arrange(desc(`1er`), desc(`2eme`), desc(`3eme`))
    
    table_epreuve_med[, c("Athlète","Discipline", "1er", "2eme", "3eme")]
  })
  
  observeEvent(input$discipline5, {
    updateSelectInput(session, "Nat", choices = unique(data[data$Discipline == input$discipline5,]$Nationalité), selected = NULL)
  })
  
  output$plot4 <- renderPlot({
    req(input$discipline5, input$Nat)
    
    athletes_unique <- data[data$Discipline == input$discipline5 & data$Nationalité %in% input$Nat,] %>%
      select(Athlète, Sexe, `catégorie d’âge`, Nationalité,Epreuve) %>%
      distinct()
    
    table_sexe <- athletes_unique %>%
      group_by(Sexe, Nationalité) %>%
      summarise(Count = n(), .groups = 'drop')
    
    table_sexe_discipline <- athletes_unique %>%
      group_by(Sexe, Nationalité,Epreuve) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    table_age_discipline <- data[data$Discipline == input$discipline5 & data$Nationalité %in% input$Nat,] %>%
      mutate(Sexe = case_when(
        Sexe == "Femme" ~ "F",
        Sexe == "Homme" ~ "H",
        TRUE ~ Sexe
      )) %>%
      group_by(`catégorie d’âge`, Epreuve, Nationalité, Sexe) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    table_comp_discipline <- data[data$Discipline == input$discipline5 & data$Nationalité %in% input$Nat,] %>%
      group_by(`Type de compétition`, Nationalité, Epreuve, Sexe, `catégorie d’âge`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    p1 <- ggplot(table_sexe, aes(x = "", y = Count, fill = Sexe)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void(base_size = 14) +
      labs(title = "Distribution des Athlètes par Sexe") +
      geom_text(aes(label = paste(Nationalité, ": ", Count)), position = position_stack(vjust = 0.5), size = 5) +
      theme(plot.title = element_text(hjust = 0.5))
    
    p2 <- ggplot(table_age_discipline, aes(x = Nationalité, y = Count, fill = `catégorie d’âge`)) +
      geom_bar(stat = "identity") +
      theme_minimal(base_size = 14) +
      labs(title = "Participation par Catégorie d'Âge", x = "Nationalité", y = "Nombre d'athlètes") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
      geom_text(aes(label = paste(Epreuve, ": ", Count, " (", Sexe, ")")), position = position_stack(vjust = 0.5), size = 4)
    
    create_pie_chart_disc <- function(nat) {
      data_filtered <- table_sexe_discipline %>% filter(Nationalité == nat)
      
      ggplot(data_filtered, aes(x = "", y = Percentage, fill = Epreuve)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void(base_size = 14) +
        labs(title = paste("Répartition des Athlètes de :", nat)) +
        geom_text(aes(label = paste(Count, " (", Sexe, ")")), position = position_stack(vjust = 0.5), size = 5) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    Nat <- unique(table_sexe_discipline$Nationalité)
    pie_charts_disc <- lapply(Nat, create_pie_chart_disc)
    
    create_bar_chart_comp <- function(category) {
      data_filtered <- table_comp_discipline %>% filter(`Type de compétition` == category)
      
      ggplot(data_filtered, aes(x = Nationalité, y = Count, fill = `catégorie d’âge`)) +
        geom_bar(stat = "identity") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Participation par Compétition:", category), x = "Nation", y = "Nombre d'athlètes") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
        geom_text(aes(label = paste(Epreuve, ": ", Count, " (", Sexe, ")")), position = position_stack(vjust = 0.5), size = 4)
    }
    
    competition <- unique(table_comp_discipline$`Type de compétition`)
    bar_charts_comp <- lapply(competition, create_bar_chart_comp)
    
    all_graphe <- c(list(p1), list(p2), pie_charts_disc, bar_charts_comp)
    
    grid.arrange(grobs = all_graphe, ncol = 2, top = textGrob("Analyse des Athlètes", gp = gpar(fontsize = 16)))
  }, height = 1500, width = 1430)
  
  
  
  output$plot5 <- renderPlot({
    req(input$discipline6)
    
    dataint <- data[data$Discipline == input$discipline6, ]
    
    table_epreuve_med <- dataint %>%
      mutate(`Type de compétition` = case_when(
        `Type de compétition` == "European Championships" ~ "EC",
        `Type de compétition` == "Pro Tour" ~ "PT",
        `Type de compétition` == "World Championships" ~ "WC",
        TRUE ~ `Type de compétition`
      )) %>%
      group_by(Classement, Nationalité, Epreuve, `Type de compétition`, Sexe) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      filter(Classement %in% c(1, 2, 3))
    
    create_stacked_bar_chart <- function(Nat) {
      data_filtered <- table_epreuve_med %>%
        filter(Nationalité == Nat) %>%
        group_by(Classement, Epreuve, `Type de compétition`, Sexe) %>%
        summarise(Count = sum(Count), .groups = 'drop') %>%
        mutate(Percentage = Count / sum(Count) * 100)
      
      classement_colors <- c("1" = "#FFD700",  # Or
                             "2" = "#C0C0C0",  # Argent
                             "3" = "#CD7F32")  # Bronze
      
      ggplot(data_filtered, aes(x = Epreuve, y = Count, fill = as.factor(Classement))) +
        geom_bar(stat = "identity", position = "stack") +
        theme_minimal(base_size = 10) +
        labs(title = paste("Médailles par Epreuve pour: ", Nat), x = "Épreuve", y = "Nombre de Médailles", fill = "Classement") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
        geom_text(aes(label = paste(Count, " (", Sexe, ", ", `Type de compétition`, ")")),
                  position = position_stack(vjust = 0.5), size = 3,
                  angle = 45) +
        scale_fill_manual(values = classement_colors)
    }
    
    Nat <- unique(table_epreuve_med$Nationalité)
    stacked_bar_charts <- lapply(Nat, create_stacked_bar_chart)
    
    do.call(grid.arrange, c(stacked_bar_charts, ncol = 2))
  }, height = 1500, width = 1400)
  
  output$table3 <- renderTable({
    req(input$discipline7)
    dataint <- data %>% filter(Discipline == input$discipline7)
    
    table_nat_med <- dataint %>%
      group_by(Athlète,Nationalité, Epreuve, `Type de compétition`, `Nom de la compétition`) %>%
      distinct() %>%
      group_by(Nationalité, Epreuve) %>%
      summarise(
        `1er` = sum(Classement == 1),
        `2eme` = sum(Classement == 2),
        `3eme` = sum(Classement == 3),
        .groups = 'drop'
      ) %>%
      arrange(desc(`1er`), desc(`2eme`), desc(`3eme`))%>%
      filter(`1er` != 0 | `2eme` != 0 | `3eme` != 0)  # Filtre les lignes avec au moins une médaille
    
    table_nat_med <- table_nat_med[, c("Nationalité", "Epreuve", "1er", "2eme", "3eme")]
    
    table_nat_med
  })
  
  output$tabledhn <- renderTable({
    
    # Regrouper et résumer les données
    DD <- data %>%
      group_by(Nationalité, Discipline) %>%
      summarise(
        `1er` = sum(Classement == 1),
        `2eme` = sum(Classement == 2),
        `3eme` = sum(Classement == 3),
        .groups = 'drop'
      ) %>%
      arrange(desc(`1er`), desc(`2eme`), desc(`3eme`)) %>%
      mutate(rank = row_number())
    
    # Calculer le Nombre de pays participants par discipline
    DDD <- DD %>%
      group_by(Discipline) %>%
      summarise(
        `Nombre de pays participants` = n_distinct(Nationalité)
      )
    DD <- rbind(DD[DD$Discipline=="Artistic_Skating",]%>%
                  arrange(desc(`1er`), desc(`2eme`), desc(`3eme`)) %>%
                  mutate(rank = row_number()),
                DD[DD$Discipline=="Inline_Speed_Skating",]%>%
                  arrange(desc(`1er`), desc(`2eme`), desc(`3eme`)) %>%
                  mutate(rank = row_number()),
                DD[DD$Discipline=="Roller_Freestyle",]%>%
                  arrange(desc(`1er`), desc(`2eme`), desc(`3eme`)) %>%
                  mutate(rank = row_number()),
                DD[DD$Discipline=="Scootering",]%>%
                  arrange(desc(`1er`), desc(`2eme`), desc(`3eme`)) %>%
                  mutate(rank = row_number()),
                DD[DD$Discipline=="Skateboarding",]%>%
                  arrange(desc(`1er`), desc(`2eme`), desc(`3eme`)) %>%
                  mutate(rank = row_number())
                )
    
    # Filtrer les données pour la France
    DDCF <- DD %>%
      filter(Nationalité %in% c("FRA", "France"))
    
    table_DHN <- DDD %>%
      left_join(DDCF %>% select(Discipline, rank) %>% rename(`Classement de la france` = rank), by = "Discipline")
    
    table_DHN <- table_DHN %>%
      mutate(
        Motif = case_when(
          `Nombre de pays participants` >= 35 & Discipline != "Skateboarding" ~ "Nombre de pays participant supérieur ou égale 35",
          `Nombre de pays participants` >= 15 & `Classement de la france` <= 3 ~ "Nombre de pays participant supérieur ou égale 15 et la France parmi les top 3",
          Discipline == "Skateboarding" ~ "Sport Olympiques",
          TRUE ~ NA_character_
        )
      )%>%filter(!is.na(Motif))
    
    table_DHN
  })
  
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
