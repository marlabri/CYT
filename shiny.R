# Add librarys
library(shiny)
library(tidyverse)
library(dplyr)
library(et.nwfva)
library(rBDAT)

# UI-Definition (angepasst)
ui <- fluidPage( 
  titlePanel("Ertragstafel- und Biomasse-Generator"), 
  
  sidebarLayout( 
    sidebarPanel( 
      selectInput( 
        "baumart", 
        "Baumart:", 
        choices = c("Buche", "Eiche", "Fichte", "Kiefer", "Douglasie") 
      ), 
      sliderInput( 
        "alter", 
        "Alter (Jahre):", 
        min = 0, 
        max = 200, 
        value = 100, 
        step = 1 
      ), 
      sliderInput( 
        "relative_bonitaet", 
        "Relative Bonität:", 
        min = -2, 
        max = 4, 
        value = 1, 
        step = 0.1 
      ), 
      checkboxGroupInput("ausgabe_werte", "Ausgabe Werte:", choices = NULL), 
      checkboxGroupInput( 
        "biom", 
        "Biomasse berechnen für:", 
        choices = c( 
          "Stehender Bestand" = "stand", 
          "Ausscheidender Bestand" = "aus", 
          "Kohlenstoff" = "C", 
          "Kohlenstoffdioxid" = "CO2" 
        ), 
        selected = NULL 
      ) 
    ), 
    
    mainPanel( 
      tableOutput("ertragstafel"), 
      verbatimTextOutput("biomasse_output") 
    ) 
  ) 
) 

# Server-Logik (angepasst)
server <- function(input, output, session) { 
  # Baumart-Zuordnung für rBDAT (unverändert) 
  baumart_spp <- c( 
    "Fichte" = 1, 
    "Kiefer" = 5, 
    "Douglasie" = 8, 
    "Buche" = 15, 
    "Eiche" = 17 
  ) 
  
  # Reaktive Funktion zur Erstellung der Ertragstafel (angepasst)
  ertragstafel_daten <- reactive({ 
    tryCatch({ 
      et_tafel(input$baumart, 
               bon = input$relative_bonitaet, 
               alter = input$alter) 
    }, error = function(e) { 
      data.frame(Fehler = paste("Fehler bei der Erstellung der Tafel:", e$message)) 
    }) 
  }) 
  
  # Dynamische Aktualisierung der Checkbox-Auswahl für die Ertragstafel (unverändert) 
  observe({ 
    tafel <- ertragstafel_daten() 
    if (!"Fehler" %in% names(tafel)) { 
      updateCheckboxGroupInput( 
        session, 
        "ausgabe_werte", 
        choices = names(tafel), 
        selected = names(tafel)[1:min(5, length(names(tafel)))] 
      ) 
    } else { 
      updateCheckboxGroupInput(session, "ausgabe_werte", choices = NULL) 
    } 
  }) 
  
  # Reaktive Funktion zur Berechnung der Biomasse (unverändert)
  biomasse_daten <- reactive({ 
    tafel <- ertragstafel_daten() 
    if (!"Fehler" %in% names(tafel) && !is.null(input$biom)) { 
      if (!("Dg" %in% names(tafel)) || 
          !("Hg" %in% names(tafel)) || !("Dg_aus" %in% names(tafel))) { 
        return("Dg, Hg oder Dg_aus nicht in den Ertragstafeldaten vorhanden") 
      } 
      tryCatch({ 
        biomasse_ergebnisse <- lapply(input$biom, function(biom_typ) { 
          if (biom_typ == "stand") { 
            bdat_df <- data.frame(spp = baumart_spp[input$baumart], 
                                  D1 = tafel$Dg, 
                                  H = tafel$Hg) 
            biomasse <- getBiomass(bdat_df) 
            paste("Biomasse stehender Bestand:", biomasse) 
          } else if (biom_typ == "aus") { 
            bdat_df <- data.frame(spp = baumart_spp[input$baumart], 
                                  D1 = tafel$Dg_aus, 
                                  H = tafel$Hg) 
            biomasse <- getBiomass(bdat_df) 
            paste("Biomasse ausscheidender Bestand:", biomasse) 
          } else if (biom_typ == "C") { 
            bdat_df <- data.frame(spp = baumart_spp[input$baumart], 
                                  D1 = tafel$Dg, 
                                  H = tafel$Hg) 
            biomasse <- getBiomass(bdat_df) 
            kohlenstoff <- biomasse * 0.5 # Annahme: 50% Kohlenstoffgehalt 
            paste("Kohlenstoff im stehenden Bestand:", kohlenstoff) 
          } 
          else if (biom_typ == "CO2") { 
            bdat_df <- data.frame(spp = baumart_spp[input$baumart], 
                                  D1 = tafel$Dg, 
                                  H = tafel$Hg) 
            biomasse <- getBiomass(bdat_df) 
            co2 <- biomasse * 1.83 # Umrechnungsfaktor von C nach CO2 (44/12) gerundet 
            paste("Kohlenstoffdioxid im stehenden Bestand:", co2) 
          } 
          else { 
            "Ungültiger Biomasse-Typ" 
          } 
        }) 
        paste(biomasse_ergebnisse, collapse = "\n") 
      }, error = function(e) { 
        paste("Fehler bei der Biomasseberechnung:", e$message) 
      }) 
    } else if ("Fehler" %in% names(tafel)) { 
      return("Keine Biomasseberechnung möglich, da Ertragstafel Fehler enthält.") 
    } 
    else { 
      return("Bitte wähle einen Biomasse Typ aus") 
    } 
  }) 
  
  # Ausgabe der Tabelle (unverändert)
  output$ertragstafel <- renderTable({ 
    tafel <- ertragstafel_daten() 
    if (!"Fehler" %in% names(tafel)) { 
      tafel %>% select(input$ausgabe_werte) 
    } else { 
      tafel 
    } 
  }) 
  
  # Ausgabe der Biomasse (unverändert)
  output$biomasse_output <- renderText({ 
    biomasse_daten() 
  }) 
} 

# Shiny App starten (unverändert)
shinyApp(ui = ui, server = server)
