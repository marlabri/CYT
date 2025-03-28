### Fitting data for Tree Species

# Writing a loop to get all and to not overwrite what we've got
ET_all <- lapply(all, function(baumart) {
  namen <- c("Ekl_-1", "Ekl_0", "Ekl_1", "Ekl_2", "Ekl_3")
  baumart[namen]
})

# Erstellen Sie einen ggplot ohne Filterung - this is a universal function
plot_GWL_CO2 <- function(data_list, title = "Comparison of TVP_CO2 between different Yield Classes") {
  # Erstellt einen ggplot mit dem Vergleich der GWL_CO2 Werte zwischen verschiedenen EKLs.
  #
  # Args:
  #   data_list (list): Eine Liste von Dataframes, die die GWL_CO2 Daten enthalten.
  #   title (str): Der Titel des Plots.
  #d to load
  # Returns:
  #   ggplot: Ein ggplot-Objekt.
  
  # Daten zusammenführen und EKL-Kennzeichnung hinzufügen
  plot_data <- bind_rows(data_list, .id = "EKL")
  
  # ggplot erstellen
  p <- ggplot(plot_data, aes(x = Alter, y = GWL_CO2, color = EKL)) +
    geom_point() +
    labs(title = title,
         x = "Age",
         y = "TVP_CO2",
         color = "YC") +
    theme_minimal()
  
  return(p)
}

# We check each frame:
plot_GWL_CO2(ET_all$Fichte) # Empirically proven up to 120, therefore cut off, no EP necessary.
plot_GWL_CO2(ET_all$Buche) # Extrapolate, as the stem count is so low at 110 that we cannot make any statements. 
plot_GWL_CO2(ET_all$Kiefer) # Empirically proven up to 120, therefore cut off, no EP necessary.
plot_GWL_CO2(ET_all$Eiche) # Empirically proven up to 180, therefore cut off, no EP necessary.
plot_GWL_CO2(ET_all$Douglasie) # E# Empirically proven up to 120 but N too low. Therefore EP needed. 

# Extrapolation is firstly done linearally, since we do not want the downward trend in general. 
# The plot directory is our fitting_plot wd

plot_dir <- file.path(getwd(), "results", "plots", "fitting_plots")

ekl_werte <- c("Ekl_-1", "Ekl_0", "Ekl_1") # only for the better sites the fitting is necessary initially

for (ekl in ekl_werte) {
  # 1. Änderungsrate berechnen
  delta_GWL <- diff(ET_all$Buche[[ekl]]$GWL_CO2)
  
  # 2. Negativen Wendepunkt finden
  negativer_Wendepunkt <- which(delta_GWL < 0)[1]
  
  if (!is.na(negativer_Wendepunkt)) {
    wendepunkt_Alter <- ET_all$Buche[[ekl]]$Alter[negativer_Wendepunkt + 1]
    
    # 3. Lineare Extrapolation
    steigung <- delta_GWL[negativer_Wendepunkt - 1] /
      (ET_all$Buche[[ekl]]$Alter[negativer_Wendepunkt] -
         ET_all$Buche[[ekl]]$Alter[negativer_Wendepunkt - 1])
    
    for (i in (negativer_Wendepunkt + 1):nrow(ET_all$Buche[[ekl]])) {
      ET_all$Buche[[ekl]]$GWL_CO2[i] <- ET_all$Buche[[ekl]]$GWL_CO2[i - 1] +
        steigung * (ET_all$Buche[[ekl]]$Alter[i] -
                      ET_all$Buche[[ekl]]$Alter[i - 1])
    }
  }
  
  # Visualisierung der linearen Extrapolation mit GWL_CO2 und Anpassungen
  plot_linear <- ggplot(ET_all$Buche[[ekl]], aes(x = Alter, y = GWL_CO2)) +
    geom_point(shape = 1) +
    labs(title = paste("Linear extrapolation for", ekl),
         x = "Age",
         y = expression(TP[CO[2]])) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(min(ET_all$Buche[[ekl]]$Alter), 180),
                       breaks = seq(min(ET_all$Buche[[ekl]]$Alter), 180, by = 20),
                       expand = c(0, 0))
  
  # Speichern des linearen Extrapolationsplots
  ggsave(filename = file.path(plot_dir, paste0("linear_extrapolation_", ekl, ".png")),
         plot = plot_linear, width = 10, height = 6)
  
  # Polynomische Regression (kubisch)
  modell <- lm(GWL_CO2 ~ poly(Alter, 4), data = ET_all$Buche[[ekl]])
  vorhersagen <- predict(modell, newdata = data.frame(Alter = ET_all$Buche[[ekl]]$Alter))
  ET_all$Buche[[ekl]]$GWL_CO2_pred <- vorhersagen
  
  # ggplot mit angepasster Kurve
  plot_poly <- ggplot(ET_all$Buche[[ekl]], aes(x = Alter, y = GWL_CO2)) +
    geom_point(shape = 1) +
    geom_line(aes(y = GWL_CO2_pred), color = "red") +
    labs(title = paste("Polynomial fit for", ekl),
         x = "Age",
         y = expression(TP[CO[2]])) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(min(ET_all$Buche[[ekl]]$Alter), 180),
                       breaks = seq(min(ET_all$Buche[[ekl]]$Alter), 180, by = 20),
                       expand = c(0, 0))
  
  # Speichern des Polynom-Fits
  ggsave(filename = file.path(plot_dir, paste0("polynomial_fit_", ekl, ".png")),
         plot = plot_poly, width = 10, height = 6)
  
  # Residuenanalyse
  residuals <- ET_all$Buche[[ekl]]$GWL_CO2 - ET_all$Buche[[ekl]]$GWL_CO2_pred
  
  # Speichern des Residuenplots
  png(file.path(plot_dir, paste0("residual_plot_", ekl, ".png")))
  plot(residuals, main = paste("Residual plot for", ekl))
  abline(h = 0, col = "red")
  dev.off()
  
  # Speichern des Residuendiagramms
  png(file.path(plot_dir, paste0("residual_histogram_", ekl, ".png")))
  hist(residuals, main = paste("Residual histogram for", ekl), col = "#fdc086")
  dev.off()
  
  # R-Quadrat berechnen
  model <- lm(GWL_CO2_pred ~ GWL_CO2, data = ET_all$Buche[[ekl]])
  summary_model <- summary(model)
  
  #Speichern der Summary
  capture.output(summary_model, file = file.path(plot_dir, paste0("summary_", ekl, ".txt")))
}

## Next step is fitting of Douglas Fir

for (ekl in ekl_werte) {
  # 1. Änderungsrate berechnen
  delta_GWL <- diff(ET_all$Douglasie[[ekl]]$GWL_CO2)
  
  # 2. Negativen Wendepunkt finden
  negativer_Wendepunkt <- which(delta_GWL < 0)[1]
  
  if (!is.na(negativer_Wendepunkt)) {
    wendepunkt_Alter <- ET_all$Douglasie[[ekl]]$Alter[negativer_Wendepunkt + 1]
    
    # 3. Lineare Extrapolation
    steigung <- delta_GWL[negativer_Wendepunkt - 1] /
      (ET_all$Douglasie[[ekl]]$Alter[negativer_Wendepunkt] -
         ET_all$Douglasie[[ekl]]$Alter[negativer_Wendepunkt - 1])
    
    for (i in (negativer_Wendepunkt + 1):nrow(ET_all$Douglasie[[ekl]])) {
      ET_all$Douglasie[[ekl]]$GWL_CO2[i] <- ET_all$Douglasie[[ekl]]$GWL_CO2[i - 1] +
        steigung * (ET_all$Douglasie[[ekl]]$Alter[i] -
                      ET_all$Douglasie[[ekl]]$Alter[i - 1])
    }
  }
  
  # Visualisierung der linearen Extrapolation mit GWL_CO2 und Anpassungen
  plot_linear <- ggplot(ET_all$Douglasie[[ekl]], aes(x = Alter, y = GWL_CO2)) +
    geom_point(shape = 1) +
    labs(title = paste("Linear extrapolation for", ekl),
         x = "Age",
         y = expression(TP[CO[2]])) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(min(ET_all$Douglasie[[ekl]]$Alter), 180),
                       breaks = seq(min(ET_all$Douglasie[[ekl]]$Alter), 180, by = 20),
                       expand = c(0, 0))
  
  # Speichern des linearen Extrapolationsplots
  ggsave(filename = file.path(plot_dir, paste0("linear_extrapolation_", ekl, ".png")),
         plot = plot_linear, width = 10, height = 6)
  
  # Polynomische Regression (kubisch)
  modell <- lm(GWL_CO2 ~ poly(Alter, 4), data = ET_all$Douglasie[[ekl]])
  vorhersagen <- predict(modell, newdata = data.frame(Alter = ET_all$Douglasie[[ekl]]$Alter))
  ET_all$Douglasie[[ekl]]$GWL_CO2_pred <- vorhersagen
  
  # ggplot mit angepasster Kurve
  plot_poly <- ggplot(ET_all$Douglasie[[ekl]], aes(x = Alter, y = GWL_CO2)) +
    geom_point(shape = 1) +
    geom_line(aes(y = GWL_CO2_pred), color = "red") +
    labs(title = paste("Polynomial fit for", ekl),
         x = "Age",
         y = expression(TP[CO[2]])) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(min(ET_all$Douglasie[[ekl]]$Alter), 180),
                       breaks = seq(min(ET_all$Douglasie[[ekl]]$Alter), 180, by = 20),
                       expand = c(0, 0))
  
  # Speichern des Polynom-Fits
  ggsave(filename = file.path(plot_dir, paste0("polynomial_fit_", ekl, ".png")),
         plot = plot_poly, width = 10, height = 6)
  
  # Residuenanalyse
  residuals <- ET_all$Douglasie[[ekl]]$GWL_CO2 - ET_all$Douglasie[[ekl]]$GWL_CO2_pred
  
  # Speichern des Residuenplots
  png(file.path(plot_dir, paste0("residual_plot_", ekl, ".png")))
  plot(residuals, main = paste("Residual plot for", ekl))
  abline(h = 0, col = "red")
  dev.off()
  
  # Speichern des Residuendiagramms
  png(file.path(plot_dir, paste0("residual_histogram_", ekl, ".png")))
  hist(residuals, main = paste("Residual histogram for", ekl), col = "#e78ac3")
  dev.off()
  
  # R-Quadrat berechnen
  model <- lm(GWL_CO2_pred ~ GWL_CO2, data = ET_all$Douglasie[[ekl]])
  summary_model <- summary(model)
  
  #Speichern der Summary
  capture.output(summary_model, file = file.path(plot_dir, paste0("summary_", ekl, ".txt")))
}

