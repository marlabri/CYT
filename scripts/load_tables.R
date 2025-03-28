### At first we load the data from the package et.nwfva for further usage

library(tcltk)
library(et.nwfva)
library(dplyr)
library(tidyr)

# Generally needed definitions

# For the initialisation, tree species in German are needed
baumarten <- c("Buche", "Fichte", "Kiefer", "Eiche", "Douglasie")

# List to save YTs
ergebnisse_baumarten <- list()

# Path to save et.nwfva YTs
path_original_data <- file.path(getwd(), "data", "original_data")

# Function to load from et.nwfva

for (baumart in baumarten) {
  # Leere Liste zum Speichern der Ergebnisse für die aktuelle Baumart
  ergebnisse_liste <- list()
  
  # Fortschrittsbalken
  pb <- tkProgressBar(
    title = paste("Progress for", baumart),
    min = 0,
    max = length(seq(-1, 3.0, 1)) * length(seq(5, 180, 5)),
    width = 300
  )
  zaehler <- 0
  
  # Schleife für die Bonitäten
  for (bon in seq(-1, 3.0, 1)) {
    # Schleife für die Alter
    for (alter in seq(5, 180, 5)) {
      # Ertragstafelwerte abrufen (mit Fehlerbehandlung
      ertragswerte <- tryCatch({
        et_tafel(baumart, bon = bon, alter = alter) # Baumart als Variable
        
      }, error = function(e) {
        cat(
          paste(
            "Fehler bei Baumart",
            baumart,
            ", Bonität",
            bon,
            "und Alter",
            alter,
            ": ",
            e$message,
            "\n"
          )
        )
        
        return(NULL)
      })
      if (is.null(ertragswerte))
        next
      if (!is.data.frame(ertragswerte)) {
        ertragswerte <- as.data.frame(ertragswerte)
      }
      # Ergebnisse der Liste hinzufügen (mit eindeutigem Namen)
      
      listen_name <- paste0("bon_", bon, "_alter_", alter)
      
      ergebnisse_liste[[listen_name]] <- ertragswerte
      
      zaehler <- zaehler + 1
      
      setTkProgressBar(pb, zaehler, label = paste(round(zaehler / (
        length(seq(-1, 3.0, 0.1)) * length(seq(5, 180, 5))
      ) * 100, 0), "% abgeschlossen"))
      
    }
    
  }
  
  close(pb)
  
  # Alle Datensätze zusammenführen
  Fichte <- do.call(rbind, ergebnisse_liste)
  
  # Zeilennamen entfernen
  rownames(Fichte) <- NULL
  
  # Bereinigung (Entfernen von Zeilen mit Dg/Hg == 0 oder NA) mit dplyr
  Fichte_bereinigt <- Fichte %>%
    filter(Dg != 0 & !is.na(Dg) & Hg != 0 & !is.na(Hg))
  
  # Ergebnisse für die aktuelle Baumart in der Liste speichern
  ergebnisse_baumarten[[baumart]] <- Fichte_bereinigt
  
  #Optionale Ausgabe in eine CSV-Datei für jede Baumart
  dateiname <- file.path(getwd(), "data", "original_data", paste0(baumart, "_export.csv"))
  write.csv(Fichte_bereinigt, file = dateiname, row.names = FALSE)
}

# SAve the data for later, so that you must not always use the package
data <- ergebnisse_baumarten
original_data <- file.path(path_original_data, "data.rda")
save(data, file = original_data)

# Cleaning

rm(ergebnisse_baumarten, ergebnisse_liste, Fichte, Fichte_bereinigt, pb)


