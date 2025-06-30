library(shiny)
library(pdftools)
library(stringr)
library(dplyr)
library(readxl)
library(writexl)
library(shinyjs)
library(openxlsx)
library(fs)
library(DT)

Add_DB <- function(data) {
  nom_fichier <- "Base de données resultats globale FFRS.xlsx"
  
  if (file.exists(nom_fichier)) {
    # Charger le fichier existant pour vérifier le nombre de colonnes
    existing_data <- read_xlsx(nom_fichier)
    
    if (ncol(existing_data) != ncol(data)) {
      # Afficher un message d'erreur si le nombre de colonnes est différent
      showNotification("Le nombre de colonnes ne correspond pas à celui de la BD.", type = "error")
      return(FALSE)
    } else if (!identical(colnames(existing_data), colnames(data))) {
      # Afficher un message d'erreur si les noms de colonnes sont différents
      showNotification("Les noms de colonnes ne correspondent pas à ceux de la BD.", type = "error")
      return(FALSE)
    } else {
      # Ajouter les données au fichier Excel existant
      wb <- loadWorkbook(nom_fichier)
      writeData(wb, sheet = 1, x = data, startRow = nrow(existing_data) + 1, colNames = FALSE, rowNames = FALSE)
      saveWorkbook(wb, nom_fichier, overwrite = TRUE)
      showNotification("Les données ont été soumises avec succès !", type = "message")
      return(TRUE)
    }
  } else {
    # Créer un nouveau fichier Excel et y écrire les données
    write_xlsx(data, nom_fichier)
    showNotification("Une nouvelles base des données a été Crée avec succès !", type = "message")
    return(TRUE)
  }
}

# Chargement des données initiales
discipline.epreuves <- read_excel("disciplineFFRS.xlsx")
discipline.epreuves <- as.data.frame(discipline.epreuves)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Extraction des données des PDF"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        fileInput("file", "Choisir un fichier PDF"),
        selectInput("columns", "Sélectionner les Colonnes", choices = NULL, multiple = TRUE),
        column(2, actionButton("sep_cols", " Séparer ")),
        column(1, actionButton("fus_cols", "Fusionner"), offset = 1),
        column(2, actionButton("edit_cols", "Modifier les noms"), offset = 2)
      ),
      fluidRow(
        selectInput("discipline", "Discipline", choices = NULL, selected = NULL),
        selectInput("events", "Épreuves", choices = NULL, selected = NULL),
        selectInput("age", "catégorie d’âge",choices = NULL, selected = NULL),
        selectInput("tcom", "Type de compétition", choices = NULL, selected = NULL),
        checkboxGroupInput("sexe", "Sexe :", choices = c("Women", "Men"))
      ),
      fluidRow(
        selectizeInput("com", "Nom de la compétition", choices = NULL, selected = NULL, options = list(create = TRUE)),
        selectizeInput("season", "Saison Sportive", choices = as.character(seq(as.numeric(format(Sys.Date(), "%Y")), by = -1, length.out = 10)), selected = NULL, options = list(create = TRUE)),
        selectizeInput("debut", "Date de début", choices = NULL, selected = NULL, options = list(create = TRUE)),
        selectizeInput("fin", "Date de fin", choices = NULL, selected = NULL, options = list(create = TRUE)),
        selectizeInput("obs", "Commentaire", choices = NULL, selected = NULL, multiple = TRUE, options = list(create = TRUE))
      ),
      fluidRow(
        column(3, downloadButton("download", "Télécharger")),
        column(2, actionButton("Soum", "Soumettre"), offset = 3)
      )
    ),
    mainPanel(
      DTOutput("editable_table")
    )
  )
)
server <- function(input, output, session) {
  tryCatch({
    extracted_text <- eventReactive(input$file, {
    req(input$file$datapath)
    text <- pdf_text(input$file$datapath)
    modified_lines <- unlist(sapply(text, function(page) {
      lines <- str_split(page, "\n")[[1]]
      modified_lines <- sapply(lines, function(line) {
        line <- str_trim(line, side = "left")
        line <- str_replace_all(line, " ", "*")
        line <- str_replace_all(line, "\\*{2,}", "+")
        return(line)
      })
      return(modified_lines)
    }))
    return(modified_lines)
  })
  
  processed_data <- reactiveVal(NULL)
  Data_Brute <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(extracted_text())
    lines <- extracted_text()
    list_data <- lapply(lines, function(line) {
      elements <- str_split(line, "\\+")[[1]]
      elements <- str_replace_all(elements, "\\*", " ")
      elements <- replace(elements, elements == "", NA)
      return(elements)
    })

    column_names <- NULL
    for (i in seq_along(list_data)) {
      if ('Name' %in% list_data[[i]] || 'SKATER' %in% list_data[[i]] || 'Firstname' %in% list_data[[i]]|| 'First Name' %in% list_data[[i]]) {
        column_names <- list_data[[i]]
        if (length(column_names) >= 10) {
          column_names <- column_names[seq_len(8)]
        }
        i_with_names <- i
        break
      }
    }
    
    df_data <- list()
    
    if (is.null(column_names)) {
      for(h in 1:8) {
        column_names <- c(column_names, paste("Colonne.", h))
      }
      
      l <- sapply(list_data, length)
      M <- max(l) 
      
      
      for (i in seq_along(list_data)) {
        if (length(list_data[[i]]) >= (M-2)) {
          i_with_names <- i - 1
          break
        }
      }
      for (j in (i_with_names + 1):length(list_data)) {
        line <- list_data[[j]]
        if (length(line) >= 3) {
          df_data <- append(df_data, list(line))
        }
      }
    } else {
      for (j in (i_with_names + 1):length(list_data)) {
        line <- list_data[[j]]
        if (length(line) >= (length(column_names) - 2)) {
          df_data <- append(df_data, list(line))
        }
      }
    }
    
    DF <- data.frame()
    df <- as.data.frame(do.call(rbind, df_data), stringsAsFactors = FALSE, na.strings = "")
    colnames(df) <- column_names[seq_len(ncol(df))]
    
    updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
    updateSelectInput(session, "discipline", choices = colnames(discipline.epreuves), selected = "")
    Data_Brute(df)
    processed_data(Data_Brute())
  })
  
  observeEvent(input$columns, {
    selected_columns <- input$columns
    DF <- Data_Brute()
    if (length(selected_columns) == 0) {
      processed_data(NULL)
    } else {
      DF <- DF[, selected_columns, drop = FALSE]
      processed_data(DF)
    }
    output$editable_table <- renderDT({
      req(processed_data())
      df <- processed_data()
      df$Supprimer <- sapply(seq_len(nrow(df)), function(i) {
        sprintf('<button id="delete_%s" onclick="deleteRow(%s)" class="btn btn-danger btn-sm">Supprimer</button>', i, i)
      })
      datatable(df, escape = FALSE, editable = TRUE, options = list(searching = TRUE ,paging = FALSE  ))
    })
  })
  
  observeEvent(input$edit_cols, {
    DF <- processed_data()
    showModal(modalDialog(
      title = "Modifier les noms des colonnes",
      textInput("colnames_input", "Entrer les nouveaux noms des colonnes (séparés par des virgules). 
                Les noms des colonnes de la bd sont : 
                Classement,Athlète,Nationalité,Discipline,Épreuve,Sexe,Catégorie d’âge,Type de compétition,Nom de la compétition,Saison,Date de début,Date de fin,Commentaires",
                value = paste(c("Classement","Athlète","Nationalité"), collapse = ",")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("save_colnames", "Sauvegarder")
      )
    ))
  })
  
  observeEvent(input$save_colnames, {
    new_colnames <- unlist(str_split(input$colnames_input, ","))
    selected_columns <- input$columns
    DF <- processed_data()
    if(length(new_colnames)!= length(selected_columns))
    {
      showNotification("Le nombre de colonnes saisies ne correspond pas à celui du colonnes séléctionnées", type = "error") 
    }else
    {colnames(DF) <- new_colnames}
    updateSelectInput(session, "columns", choices = colnames(DF), selected = colnames(DF))
    #Data_Brute(DF)
    processed_data(DF)
    Data_Brute(processed_data())
    removeModal()
  })
  
  observeEvent(input$fus_cols, {
    selected_columns <- colnames(processed_data())
    showModal(modalDialog(
      title = "Fusionner des colonnes",
      checkboxGroupInput("cols_to_fuse", "Sélectionner les colonnes à fusionner", choices = selected_columns),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("do_fusion", "Fusionner")
      )
    ))
  })
  
  observeEvent(input$do_fusion, {
    selected_cols <- input$cols_to_fuse
    if (length(selected_cols) > 1) {
      new_col <- apply(processed_data()[, selected_cols], 1, paste, collapse = " ")
      new_data <- processed_data()
      new_data[[paste(selected_cols, collapse = "_")]] <- new_col
      new_data <- new_data[, !(names(new_data) %in% selected_cols)]
      updateSelectInput(session, "columns", choices = colnames(new_data), selected = colnames(new_data))
      #Data_Brute(new_data)
      processed_data(new_data)
      Data_Brute(processed_data())
      removeModal()
    } else {
      showModal(modalDialog(
        title = "Erreur",
        "Veuillez sélectionner au moins deux colonnes à fusionner.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observeEvent(input$sep_cols, {
    selected_columns <- colnames(processed_data())
    showModal(modalDialog(
      title = "Séparer des colonnes",
      checkboxGroupInput("cols_to_separate", "Sélectionner les colonnes à séparer", choices = selected_columns),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("do_separation", "Séparer")
      )
    ))
  })
  
  observeEvent(input$do_separation, {
    selected_cols <- input$cols_to_separate
    if (length(selected_cols) > 0) {
      new_data <- processed_data()
      for (col in selected_cols) {
        new_col1 <- sapply(new_data[[col]], function(x) strsplit(x, " ")[[1]][1])
        new_col2 <- sapply(new_data[[col]], function(x) paste(strsplit(x, " ")[[1]][-1], collapse = " "))
        new_data[[paste0(col, "_S")]] <- new_col1
        new_data[[paste0(col, "_Rest")]] <- new_col2
      }
      new_data <- new_data[, !(names(new_data) %in% selected_cols)]
      updateSelectInput(session, "columns", choices = colnames(new_data), selected = colnames(new_data))
      #Data_Brute(new_data)
      processed_data(new_data)
      Data_Brute(processed_data())
      removeModal()
    } else {
      showModal(modalDialog(
        title = "Erreur",
        "Veuillez sélectionner au moins une colonne à séparer.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observeEvent(input$discipline, {
    req(Data_Brute())
    if (!is.null(input$discipline)) {
      choices <- paste(na.omit(discipline.epreuves[[input$discipline]]))
      df <- processed_data()
      df[["Discipline"]] <- input$discipline
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
    updateSelectInput(session, "events", choices = choices)
  })
  
  observeEvent(input$events, {
    req(Data_Brute())
    if (!is.null(input$events)) {
      df <- processed_data()
      df[["Epreuve"]] <- input$events
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      updateSelectInput(session,"age",choices =c("Senior","Junior","Open","U19","U17","U20","Cadet","Youth","Espoir"), selected = "")
      updateSelectInput(session,"tcom", choices = c("World Championships","European Championships","World Cup","Pro Tour","Jeux Olympiques","JOJ","OQS"), selected = "")
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$age, {
    req(Data_Brute())
    if (!is.null(input$age)) {
      df <- processed_data()
      df[["catégorie d’âge"]] <- input$age
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$tcom, {
    req(Data_Brute())
    if (!is.null(input$tcom)) {
      df <- processed_data()
      df[["Type de compétition"]] <- input$tcom
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$sexe, {
    req(Data_Brute())
    if (!is.null(input$sexe)) {
      df <- processed_data()
      df[["Sexe"]] <- input$sexe
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$com, {
    req(Data_Brute())
    if (!is.null(input$com)) {
      df <- processed_data()
      df[["Nom de la compétition"]] <- input$com
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$season, {
    req(Data_Brute())
    if (!is.null(input$season)) {
      df <- processed_data()
      df[["Saison Sportive"]] <- input$season
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$debut, {
    req(Data_Brute())
    if (!is.null(input$debut)) {
      df <- processed_data()
      df[["Date de début"]] <- input$debut
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$fin, {
    req(Data_Brute())
    if (!is.null(input$fin)) {
      df <- processed_data()
      df[["Date de fin"]] <- input$fin
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  observeEvent(input$obs, {
    req(Data_Brute())
    if (!is.null(input$obs)) {
      df <- processed_data()
      if(length(input$obs) == 1){df[["Commentaire"]] <- input$obs}
      else{
        com <- c(input$obs, rep(NA, max(0, nrow(df) - length(input$obs))))
        df[["Commentaire"]] <- com}
      
      processed_data(df)
      updateSelectInput(session, "columns", choices = colnames(df), selected = colnames(df))
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
  })
  
  output$editable_table <- renderDT({
    req(processed_data())
    df <- processed_data()
    df$Supprimer <- sapply(seq_len(nrow(df)), function(i) {
      sprintf('<button id="delete_%s" onclick="deleteRow(%s)" class="btn btn-danger btn-sm">Supprimer</button>', i, i)
    })
    datatable(df, escape = FALSE, editable = TRUE,options = list(searching = FALSE,paging = TRUE  ))
  })
  
  observe({
    runjs("
      // Fonction JavaScript pour gérer la suppression avec confirmation
      window.deleteRow = function(index) {
        var confirmDelete = confirm('Êtes-vous sûr de vouloir supprimer cette ligne ?');
        if (confirmDelete) {
          Shiny.onInputChange('delete_row', index);
        }
      }
    ")
  })
  
  observeEvent(input$delete_row, {
    selected_row <- input$delete_row
    if (!is.null(selected_row)) {
      # Affichez un message d'alerte pour indiquer que la ligne a été supprimée
      showNotification("Ligne supprimée avec succès !", type = "message")
      df <- processed_data()
      df <- df[-selected_row, ]
      processed_data(df)
      #Data_Brute(df)
      Data_Brute(processed_data())
    }
    })
  
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    DF <- processed_data()
    DF[i, j] <- v
    processed_data(DF)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("PDF_modifie_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      selected_columns <- input$columns
      if (is.null(selected_columns)) {
        showNotification("Veuillez sélectionner au moins une colonne.", type = "error")
        return()
      }
      write_xlsx(processed_data(), file)
    }
  )
  
  observeEvent(input$Soum, {
    if (is.null(input$discipline)) {
      df <- processed_data()
      df[["Discipline"]]  <- NA
      processed_data(df)
    }
    
    if (input$events=="") {
      df <- processed_data()
      df[["Epreuve"]] <- NA
      processed_data(df)
    }
    
    if (is.null(input$sexe)) {
      df <- processed_data()
      df[["Sexe"]] <- NA
      processed_data(df)
    }
    
    if (input$com=="") {
      df <- processed_data()
      df[["Nom de la compétition"]]  <- NA
      processed_data(df)
    }
    
    if (input$season =="") {
      df <- processed_data()
      df[["Saison Sportive"]] <- NA
      processed_data(df)
    }
    
    if (input$debut=="") {
      df <- processed_data()
      df[["Date de début"]] <- NA
      processed_data(df)
    }
    
    if (input$fin=="") {
      df <- processed_data()
      df[["Date de fin"]] <- NA
      processed_data(df)
    }
    
    if (is.null(input$obs) || input$obs=="") {
      df <- processed_data()
      df[["Commentaire"]] <- NA
      processed_data(df)
    }

    data <- processed_data()
    if (Add_DB(data)) {
      session$reload()
      output$editable_table <- renderDT({
        df <- data.frame()
        df$Supprimer <- sapply(seq_len(nrow(df)), function(i) {
          sprintf('<button id="delete_%s" onclick="deleteRow(%s)" class="btn btn-danger btn-sm">Supprimer</button>', i, i)
        })
        datatable(df, escape = FALSE, editable = TRUE)
      })
    }
  })
  }, error = function(e) {
    showNotification("Une erreur est survenue lors de l'extraction du texte du PDF. Veuillez réessayer !", type = "error")
    NULL
  })
}
shinyApp(ui, server)
