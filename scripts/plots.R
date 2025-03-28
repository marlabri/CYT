### 3. Results

# Where to store the plots:
plot_dir <- "~/CYT/CYT/results/plots/result_plots/"

# Volume Plots
for (species_name in names(tree_species)) {
  species_data <- ET_long[[species_name]]
  species_info <- tree_species[[species_name]]
  
  # Anpassung der xlim-Werte für Douglasie mit Ekl = -1
  if (species_name == "Douglasie") {
    xlim_value <- species_info$xlim
    if (-1 %in% species_data$Ekl) {
      xlim_value <- 95
    }
  } else {
    xlim_value <- species_info$xlim
  }
  
  roman_ekl <- c("-I", "0", "I", "II", "III") # Define Roman numeral labels
  
  p <- ggplot(species_data, aes(x = Alter, y = Gesamt_CO2, color = factor(Ekl))) +
    geom_line() +
    geom_point() +
    labs(
      title = expression("CO"[2] ~ " Stock over age for different Yield Classes"),
      subtitle = eval(bquote(expression(paste(.(species_info$english), " (", italic(.(species_info$latin)), ")")))),
      x = "Age",
      y = expression("CO"[2] ~ "e Stock [t ha"^-1 * "]"),
      color = "Yield Class"
    ) +
    theme_minimal() +
    coord_cartesian(xlim = c(0, xlim_value)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(values = colors, labels = roman_ekl) # Apply Roman numeral labels
  
  filename <- paste0(plot_dir, species_info$english, "_CO2_stock.png")
  ggsave(filename, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  
  cat("Plot saved:", filename, "\n")
}

cat("All plots saved to", plot_dir, "\n")


# TVP Plots
for (species_name in names(tree_species)) {
  species_data <- ET_long[[species_name]]
  species_info <- tree_species[[species_name]]
  
  y_column <- if (species_name %in% c("Buche", "Douglasie")) "GWL_CO2_pred" else "GWL_CO2"
  
  # Anpassung der xlim-Werte für Douglasie mit Ekl = -1
  if (species_name == "Douglasie") {
    xlim_value <- species_info$xlim
    if (-1 %in% species_data$Ekl) {
      xlim_value <- 95
    }
  } else {
    xlim_value <- species_info$xlim
  }
  
  roman_ekl <- c("-I", "0", "I", "II", "III") # Define Roman numeral labels
  
  p <- ggplot(species_data, aes(x = Alter, y = .data[[y_column]], color = factor(Ekl))) +
    geom_line() +
    geom_point() +
    labs(
      title = expression("Total Volume Production CO"[2] ~ " over age for different Yield Classes"),
      subtitle = eval(bquote(expression(paste(.(species_info$english), " (", italic(.(species_info$latin)), ")")))),
      x = "Age",
      y = expression("TVP CO"[2] ~ "e [t ha"^-1 * "]"),
      color = "Yield Class"
    ) +
    theme_minimal() +
    coord_cartesian(xlim = c(0, xlim_value)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(values = colors, labels = roman_ekl) # Apply Roman numeral labels
  
  filename <- paste0(plot_dir, species_info$english, "_CO2_production.png")
  ggsave(filename, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  
  cat("Plot saved:", filename, "\n")
}

cat("All plots saved to", plot_dir, "\n")

# DGZ und lz
# Hier die Plots abspeichern 
for (species_name in names(tree_species)) {
  species_data <- ET_long[[species_name]]
  species_info <- tree_species[[species_name]]
  
  y_column <- if (species_name %in% c("Buche", "Douglasie")) "GWL_CO2_pred" else "GWL_CO2"
  
  # Anpassung der xlim-Werte für Douglasie mit Ekl = -1
  if (species_name == "Douglasie") {
    xlim_value <- species_info$xlim
    if (-1 %in% species_data$Ekl) {
      xlim_value <- 95
    }
  } else {
    xlim_value <- species_info$xlim
  }
  
  roman_ekl <- c("-I", "0", "I", "II", "III") # Define Roman numeral labels
  
  p <- ggplot(species_data, aes(x = Alter, y = .data[[y_column]] + root_vor, color = factor(Ekl))) +
    geom_line() +
    geom_point() +
    labs(
      title = expression("Total CO"[2] ~ " production over age for different Yield Classes"),
      subtitle = eval(bquote(expression(paste(.(species_info$english), " (", italic(.(species_info$latin)), ")")))),
      x = "Age",
      y = expression("Total CO"[2] ~ "e [t ha"^-1 * "]"),
      color = "Yield Class"
    ) +
    theme_minimal() +
    coord_cartesian(xlim = c(0, xlim_value)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(values = colors, labels = roman_ekl) # Apply Roman numeral labels
  
  filename <- paste0(plot_dir, species_info$english, "_CO2_production.png")
  ggsave(filename, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  
  cat("Plot saved:", filename, "\n")
}

# MAI and CAI plots

# 1. Data Preparation - Extract and Combine Correctly (Handles NA)
combined_data <- do.call(rbind, lapply(1:length(ET_long), function(i) {
  df <- ET_long[[i]][, c("Ekl", "Alter", "Ai", "Ci")]
  df$Species <- names(ET_long)[i]
  return(df)
}))


# Assuming your data is in a data frame called 'combined_data'

# Updated color palettes
buche_colors <- c('#d8d0c6', '#8b2323', '#8b5a2b', '#cd853f', '#ffa54f')
eiche_colors <- c('#f8f5e0', '#ffd704', '#eedc84', '#fff68f', '#ffff00')
fichte_colors <- c('#afafda', '#8181ff', '#86acf1', '#7dc6ce', '#98f5ff')
douglasie_colors <- c('#6A287E', '#9948C9', '#C237C9', '#E114D3', '#D0AEDF')
kiefer_colors <- c('#282828', '#585858', '#707070', '#888888', '#A0A0A0', '#B8B8B8')

# Create a color mapping based on Species and Ekl
combined_data$Color <- NA

for (i in 1:nrow(combined_data)) {
  species <- combined_data$Species[i]
  ekl <- combined_data$Ekl[i]
  
  if (species == "Eiche") {
    combined_data$Color[i] <- eiche_colors[ekl + 2] # Ekl ranges from -1 to 3, so adjust index
  } else if (species == "Kiefer") {
    combined_data$Color[i] <- kiefer_colors[ekl + 2]
  } else if (species == "Fichte") {
    combined_data$Color[i] <- fichte_colors[ekl + 2]
  } else if (species == "Buche") {
    combined_data$Color[i] <- buche_colors[ekl + 2]
  } else if (species == "Douglasie") {
    combined_data$Color[i] <- douglasie_colors[ekl + 2]
  }
}

# Translate Species to English
combined_data$Species_English <- factor(combined_data$Species, 
                                        levels = c("Eiche", "Kiefer", "Fichte", "Buche", "Douglasie"),
                                        labels = c("Oak", "Pine", "Spruce", "Beech", "Douglas Fir"))

# Create a combined Species_Ekl variable with English species names for faceting
combined_data$Species_Ekl_English <- paste(combined_data$Species_English, combined_data$Ekl, sep = "_")

# Create the plot with dashed gridlines and English species names
# Create the plot with dashed gridlines and English species names
ggplot(combined_data, aes(x = Alter, y = Ai, color = Color)) +
  geom_point(size = 1.5, alpha = 0.9) + 
  scale_color_identity() + 
  facet_wrap(~ Species_Ekl_English) + # Facet by Species_Ekl with English names
  labs(title = "Relationship between Annual Average Increment and Age by Species and Yield Class", 
       x = "Age", 
       y = expression(Ai[t]~CO[2]~e~ha^{-1})) +# Corrected Ai subscript 
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"))

# Ci
# Create the plot with smoothing lines (4th degree polynomial)
# Function to calculate R-squared for each facet
calculate_r_squared <- function(data) {
  model <- lm(Ci ~ poly(Alter, 4), data = data)
  r_squared <- summary(model)$r.squared
  data$r_squared <- paste("R² =", round(r_squared, 3))
  return(data)
}

# Calculate R-squared for each facet
combined_data_r_squared <- combined_data %>%
  dplyr::group_by(Species_Ekl_English) %>%
  dplyr::do(calculate_r_squared(.))

# Create the plot with R-squared labels
ggplot(combined_data_r_squared, aes(x = Alter, y = Ci, color = Color)) +
  geom_point(size = 1, alpha = 0.9) + 
  scale_color_identity() + 
  facet_wrap(~ Species_Ekl_English) + # Facet by Species_Ekl with English names
  labs(title = "Relationship between Current Annual Increment and Age by Species and Yield Class", 
       x = "Age", 
       y = expression(CAI[t]~CO[2]~e~ha^{-1})) + # Corrected Ci subscript
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE) + # Add 4th degree polynomial smoothing
  geom_text(aes(x = Inf, y = Inf, label = r_squared), 
            hjust = 1.1, vjust = 1.1, size = 3) + # Add R-squared labels
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"))

## PLot diskussion
# Colors
colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
roman_ekl <- c("-I", "0", "I", "II", "III")

# Select the "Buche" data
buche_data <- ET_long[["Buche"]]

# Determine y-axis column based on Ekl
buche_data$y_column <- ifelse(buche_data$Ekl %in% c(-1, 0, 1), buche_data$GWL_CO2_pred, buche_data$GWL_CO2)

# Create the plot

# Assuming xlim_value is also defined (as you have coord_cartesian)

compare_plot_beech <- ggplot(buche_data, aes(x = Alter, color = factor(Ekl))) +
  geom_point(aes(y = y_column)) +
  geom_point(aes(y = Pretzsch), shape = 4, size = 1.5) +
  labs(
    title = "Comparison between Pretzsch and our Results on Total Production",
    subtitle = expression(paste("Beech (", italic("Fagus sylvatica"), ")")), # Corrected subtitle
    x = "Age",
    y = expression("Total CO"[2] ~ "e [t ha"^-1 * "]"),
    color = "Yield Class"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, xlim_value)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = colors, labels = roman_ekl)



