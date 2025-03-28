### 2.2 Calculation of full tree biomass ###

# Necessary packages for data manipulation and calculations
install.packages("rBDAT")
library(rBDAT)
library(tcltk)
library(tidyr)
library(dplyr)

# Here, if already saved data.rda - load it!
# load("~/your_file_path_to/data.rda) # remove hashtag if needed and replace with real filepath

# Type of displaying numbers in the output tables
options(scipen = 999)


# spp_codes derived from "rBDAT" - to get other codes use getSpeciesCode() 
spp_codes <- c(Buche = 15, Douglasie = 8, Fichte = 1, Kiefer = 5, Eiche = 17)


# caution: Single tree calculation for Yield after Thinning
# Iterate trough all dataframes to calculate CO2
for (baumart in names(data)) {
  # Extract data
  daten <- data[[baumart]]
  
  # Make sure data exists and is not empty
  if (is.null(daten) || nrow(daten) == 0) {
    cat(paste("Datensatz für", baumart, "ist leer oder NULL. Überspringe.\n"))
    next
  }
  
  # CO2 calculation with trouble shooting
  tryCatch({
    # BuildTree 
    tree <- data.frame(spp = spp_codes[baumart], D1 = daten$Dg, H = daten$Hg, row.names = NULL)
    
    #See if columns D1 and H are 0, if so - remove
    tree <- tree %>% filter(!is.na(D1),!is.na(H))
    if(nrow(tree)==0){
      cat(paste("All values of Dg or Hg of tree species", baumart, "are NA. Skip.\n"))
      next
    }
    
    # CO2 calculation of Volume in m^3 ha-^1
    daten$Single_CO2 <- (getBiomass(tree) * 3.67 * 0.5)/1000 # from kg to tonnes
    
    #Write back
    data[[baumart]] <- daten
    
  }, error = function(e) {
    cat(paste("No Co2 calculation possible for", baumart, ": ", e$message, "\n"))
    
    #additional Debugging tip, if getBiomass fails
    if(grepl("NA/NaN/Inf in foreign function call", e$message)){
      cat("getBiomass got NA/NaN/Inf values. Proof input data (Dg and Hg) a for correct numeric values without NAs.\n")
    }
    return(NULL)
  })
}

# Now for the Yield from Thinning

# Iterate through the list
for (baumart in names(data)) {
  # Extract data
  daten <- data[[baumart]]
  
  # Make sure data exists and is not empty
  if (is.null(daten) || nrow(daten) == 0) {
    cat(paste("Datensatz für", baumart, "ist leer oder NULL. Überspringe.\n"))
    next
  }
  
  # Co2 calculation
  tryCatch({
    # BuildTree
    tree <- data.frame(spp = spp_codes[baumart], D1 = daten$Dg_aus, H = daten$Hg, row.names = NULL)
    
    #Check if NAs
    tree <- tree %>% filter(!is.na(D1),!is.na(H))
    if(nrow(tree)==0){
      cat(paste("Alle Werte in Dg oder Hg von Baumart", baumart, "sind NA. Überspringe.\n"))
      next
    }
    
    # CO2 calc
    daten$single_CO2_YFT <- (getBiomass(tree) * 3.67 * 0.5)/1000 # in kg per single tree yielded
    
    #Write back
    data[[baumart]] <- daten
    
  }, error = function(e) {
    cat(paste("Fehler bei der CO2-Berechnung für", baumart, ": ", e$message, "\n"))
    
    #Debugging
    if(grepl("NA/NaN/Inf in foreign function call", e$message)){
      cat("getBiomass hat möglicherweise NA/NaN/Inf Werte erhalten. Überprüfen Sie die Eingangsdaten (Dg und Hg) auf korrekte numerische Werte ohne fehlende Werte.\n")
    }
    return(NULL)
  })
}

# Next step: Root biomass in CO2

# Koeffizienten für die Wurzelmasse
koeffizienten <- data.frame(
  Baumart = c("Fichte", "Kiefer", "Buche", "Eiche", "Douglasie"),
  b0 = c(0.003720, 0.006089, 0.018256, 0.028000, 0.018256),
  b1 = c(2.792465, 2.739073, 2.321997, 2.440000, 2.321997)
)

# Durch die Liste der Baumarten iterieren
for (baumart in names(data)) {
  # Aktuellen Datensatz extrahieren
  daten <- data[[baumart]]
  
  # Überprüfen, ob der Datensatz existiert und nicht leer ist
  if (is.null(daten) || nrow(daten) == 0) {
    cat(paste("Datensatz für", baumart, "ist leer oder NULL. Überspringe.\n"))
    next
  }
  
  # Koeffizienten für die aktuelle Baumart auswählen
  koeff <- koeffizienten %>% filter(Baumart == baumart)
  
  #Überprüfung ob es NA werte gibt in der Spalte Dg. Falls ja, werden diese Zeilen entfernt, da die Berechnung sonst Fehler wirft
  daten <- daten %>% filter(!is.na(Dg))
  if(nrow(daten)==0){
    cat(paste("Alle Werte in Dg von Baumart", baumart, "sind NA. Überspringe.\n"))
    next
  }
  
  
  # Wurzelmasse berechnen
  daten$root_co2 <- (((koeff$b0 * daten$Dg^koeff$b1)*0.5*3.67)/1000)*daten$N
  
  # Ergebnisse in die Liste zurückschreiben
  data[[baumart]] <- daten
}

# And root biomass from CYT, which remains in the stand

# Durch die Liste der Baumarten iterieren
for (baumart in names(data)) {
  # Aktuellen Datensatz extrahieren
  daten <- data[[baumart]]
  
  # Überprüfen, ob der Datensatz existiert und nicht leer ist
  if (is.null(daten) || nrow(daten) == 0) {
    cat(paste("Datensatz für", baumart, "ist leer oder NULL. Überspringe.\n"))
    next
  }
  
  # Koeffizienten für die aktuelle Baumart auswählen
  koeff <- koeffizienten %>% filter(Baumart == baumart)
  
  #Überprüfung ob es NA werte gibt in der Spalte Dg. Falls ja, werden diese Zeilen entfernt, da die Berechnung sonst Fehler wirft
  daten <- daten %>% filter(!is.na(Dg_aus))
  if(nrow(daten)==0){
    cat(paste("Alle Werte in Dg von Baumart", baumart, "sind NA. Überspringe.\n"))
    next
  }
  
  
  # Wurzelmasse berechnen
  daten$root_YFT_CO2 <- (((koeff$b0 * daten$Dg_aus^koeff$b1)*0.5*3.67)/1000)**daten$N_aus
  
  # Ergebnisse in die Liste zurückschreiben
  data[[baumart]] <- daten
}

# Now we multiply everything by the Number of Stems
# Durch die Liste der Baumarten iterieren
for (baumart in names(data)) {
  # Aktuellen Datensatz extrahieren
  daten <- data[[baumart]]
  
  # Überprüfen, ob der Datensatz existiert und nicht leer ist
  if (is.null(daten) || nrow(daten) == 0) {
    cat(paste("Datensatz für", baumart, "ist leer oder NULL. Überspringe.\n"))
    next
  }
  
  # Überprüfen, ob die notwendigen Spalten existieren
  if (!all(c("Single_CO2", "N", "single_CO2_YFT", "N_aus") %in% names(daten))) {
    warning(paste("Mindestens eine der Spalten 'Single_CO2', 'N', 'single_CO2_YFT' oder 'N_aus' fehlt im Datensatz für", baumart))
    next
  }
  
  ## Überprüfen und Ersetzen von 0 oder NA durch 1 FÜR N
  daten$N <- ifelse(daten$N == 0 | is.na(daten$N), 1, daten$N)
  
  # Überprüfen und Ersetzen von 0 oder NA durch 1 FÜR N_aus
  daten$N_aus <- ifelse(daten$N_aus == 0 | is.na(daten$N_aus), 1, daten$N_aus)
  
  # Gesamt CO2 berechnen
  daten$Gesamt_CO2 <- daten$Single_CO2 * daten$N + daten$root_co2 # plus Root
  
  # Gesamt CO2_aus berechnen
  daten$Gesamt_CO2_aus <- daten$single_CO2_YFT * daten$N_aus + daten$root_YFT_CO2 # plus Root
  
  # Ergebnisse in die Liste zurückschreiben
  data[[baumart]] <- daten
}

## 2.2 Total Volume Production (TVP), 
## Mean Annual Increment (MAI) and 
## Current Annual Increment (CAI)

berechne_gwl <- function(df) {
  # Überprüfen, ob die Spalten "Gesamt_CO2", "Ekl" und "Gesamt_CO2_aus" existieren
  if (!all(c("Gesamt_CO2", "Ekl", "Gesamt_CO2_aus") %in% names(df))) {
    warning("Spalten 'Gesamt_CO2', 'Ekl' oder 'Gesamt_CO2_aus' nicht im Dataframe gefunden.")
    return(df)
  }
  
  #Überprüfen ob der Datensatz leer ist
  if(nrow(df)==0){
    warning("Datensatz ist leer")
    return(df)
  }
  
  # GWL_CO2 innerhalb jeder Eklitätsgruppe berechnen
  df <- df %>%
    arrange(Ekl) %>% # Sortierung nach Eklität UND Alter ist wichtig!
    group_by(Ekl) %>% # Gruppieren nach Eklität
    mutate(GWL_CO2 = cumsum(Gesamt_CO2_aus) + Gesamt_CO2)
  
  #Erste GWL anpassen, da diese falsch berechnet wird. Dies muss nun innerhalb der Gruppe passieren
  df <- df %>% group_by(Ekl) %>% mutate(GWL_CO2 = ifelse(row_number()==1, Gesamt_CO2, GWL_CO2)) %>% ungroup()
  
  return(df)
}

data <- lapply(data, berechne_gwl)

# Data cleaning: If one value in a row is negative, delete whole row:

# Funktion zum Entfernen von Zeilen mit NAs
remove_na_rows <- function(df) {
  df[complete.cases(df), ]
}

# Anwenden der Funktion auf jede Unterliste
data_cleaned <- lapply(data, remove_na_rows)

all <- list()
baum_namen <- names(data_cleaned) # Extract names 

# Write each Yield Class in one Dataframe
for (i in 1:length(data_cleaned)) {
  df <- data_cleaned[[i]]
  Ekl_inhalt <- list()
  for (ekl_value in unique(df$Ekl)) {
    ekl_df <- df[df$Ekl == ekl_value, ]
    Ekl_inhalt[[paste0("Ekl_", ekl_value)]] <- ekl_df # Eindeutige Namen innerhalb der Ekl_inhalt Listen
  }
  all[[baum_namen[i]]] <- Ekl_inhalt # Hier wird der Name für all[[i]] gesetzt!
}

### Now we need to load our fitting files in here
# Load file fitting

# MAI and CAI Calculation
calculate_increments <- function(ET_all) {
  for (tree_type in names(ET_all)) {
    for (ekl_name in names(ET_all[[tree_type]])) {
      df <- ET_all[[tree_type]][[ekl_name]]
      
      # Calculate Ai (as before)
      if (tree_type %in% c("Buche", "Douglasie")) {
        if (ekl_name %in% c("Ekl_-1", "Ekl_0", "Ekl_1")) {
          if ("GWL_CO2_pred" %in% colnames(df) && "Alter" %in% colnames(df)) {
            df$Ai <- df$GWL_CO2_pred / df$Alter
          } else {
            warning(paste("Warning: 'GWL_CO2_pred' or 'Alter' column not found in", tree_type, ",", ekl_name))
          }
        } else if (ekl_name %in% c("Ekl_2", "Ekl_3")) {
          if ("GWL_CO2" %in% colnames(df) && "Alter" %in% colnames(df)) {
            df$Ai <- df$GWL_CO2 / df$Alter
          } else {
            warning(paste("Warning: 'GWL_CO2' or 'Alter' column not found in", tree_type, ",", ekl_name))
          }
        }
      } else if (tree_type %in% c("Fichte", "Eiche", "Kiefer")) {
        if ("GWL_CO2" %in% colnames(df) && "Alter" %in% colnames(df)) {
          df$Ai <- df$GWL_CO2 / df$Alter
        } else {
          warning(paste("Warning: 'GWL_CO2' or 'Alter' column not found in", tree_type, ",", ekl_name))
        }
      } else {
        warning(paste("Warning: Tree type", tree_type, "not recognized."))
      }
      
      # Calculate Ci (delta GWL / delta Alter)
      if (nrow(df) > 1) {
        if (tree_type %in% c("Buche", "Douglasie") && ekl_name %in% c("Ekl_-1", "Ekl_0", "Ekl_1")) {
          if ("GWL_CO2_pred" %in% colnames(df) && "Alter" %in% colnames(df)) {
            df$Ci <- c(NA, diff(df$GWL_CO2_pred) / diff(df$Alter))
          } else {
            warning(paste("Warning: 'GWL_CO2_pred' or 'Alter' column not found for Ci in", tree_type, ",", ekl_name))
          }
        } else if (tree_type %in% c("Buche", "Douglasie") && ekl_name %in% c("Ekl_2", "Ekl_3")) {
          if ("GWL_CO2" %in% colnames(df) && "Alter" %in% colnames(df)) {
            df$Ci <- c(NA, diff(df$GWL_CO2) / diff(df$Alter))
          } else {
            warning(paste("Warning: 'GWL_CO2' or 'Alter' column not found for Ci in", tree_type, ",", ekl_name))
          }
        } else if (tree_type %in% c("Fichte", "Eiche", "Kiefer")) {
          if ("GWL_CO2" %in% colnames(df) && "Alter" %in% colnames(df)) {
            df$Ci <- c(NA, diff(df$GWL_CO2) / diff(df$Alter))
          } else {
            warning(paste("Warning: 'GWL_CO2' or 'Alter' column not found for Ci in", tree_type, ",", ekl_name))
          }
        }
      } else {
        df$Ci <- NA
      }
      
      ET_all[[tree_type]][[ekl_name]] <- df
    }
  }
  return(ET_all)
}

ET_all <- calculate_increments(ET_all)

# Long list

ET_long <- list()

# Iterate through the lists
for (baumart in names(ET_all)) {
  # Combine to long
  ET_long[[baumart]] <- bind_rows(ET_all[[baumart]])
}


# Hier prüfen, ob alle Werte vorhanden, wenn nicht durch GWL_CO2 ersetzen
ET_long$Buche$GWL_CO2_pred <- ifelse(ET_long$Buche$GWL_CO2_pred < 0 | is.na(ET_long$Buche$GWL_CO2_pred), 
                                     ET_long$Buche$GWL_CO2, 
                                     ET_long$Buche$GWL_CO2_pred)

# Hier prüfen, ob alle Werte vorhanden, wenn nicht durch GWL_CO2 ersetzen
ET_long$Douglasie$GWL_CO2_pred <- ifelse(ET_long$Douglasie$GWL_CO2_pred < 0 | is.na(ET_long$Douglasie$GWL_CO2_pred), 
                                         ET_long$Douglasie$GWL_CO2, 
                                         ET_long$Douglasie$GWL_CO2_pred)

# Liste der Baumarten und ihre lateinischen Namen
tree_species <- list(
  "Buche" = list(english = "Beech", latin = "Fagus sylvatica", xlim = 150),
  "Fichte" = list(english = "Spruce", latin = "Picea abies", xlim = 120),
  "Douglasie" = list(english = "Douglas-fir", latin = "Pseudotsuga menziesii", xlim = 120),
  "Kiefer" = list(english = "Pine", latin = "Pinus sylvestris", xlim = 120),
  "Eiche" = list(english = "Oak", latin = "Quercus robur", xlim = 180)
)

# Farbcodes
colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
