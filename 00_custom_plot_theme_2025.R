library(scales)
library(RColorBrewer)

# custom plotting theme
color_pal = c(
  "#004d65", "#378a70", "#e25b5b", "#fc8d62", "#8da0cb", "#a6d854", "#ffd92f"
)

alliance_color_pal <- c(
  "Climb" = "#de9802",   # Dark teal
  "Snowboard" = "#A58AFF", # Coral red
  "Run" = "#00C094",     # Greenish teal
  "Water" = "#FB61D7",   # Soft blue
  "Other" = "#867c6e",   # Orange
  "Bike" = "#F8766D",   # Light green
  "Ski" = "#00B6EB"      # Yellow
)

invest_color_pal = c(
"High" = "#e25b5b",
"Medium" = "#378a70",
"Low" = "#004d65"
)

redgreen_gradient <- col_numeric(
  palette = brewer.pal(11, "PiYG"),
  #palette = c("#B22222", "#E87D50", "#D0E17D", "#8EBF65", "#228B22"),  # Darker and muted tones
  domain = c(-1, 1)  # Relative difference as a percentage
)

my_pow_theme <- theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14)
  )

##############
##############
# De-identified color scheme
deidentified_color_pal <- c(
  "Marketing" = "#de9802",   # Dark teal
  "Engineering" = "#A58AFF", # Coral red
  "Finance" = "#00C094",     # Greenish teal
#  "Sales" = "#FB61D7",   # Soft blue
  "Sales" = "#867c6e",   # Orange
  "Accounting" = "#F8766D",   # Light green
  "Data" = "#00B6EB"      # Yellow
)

de_id_invest_color_pal = c(
  "Executive" = "#e25b5b",
  "Management" = "#378a70",
  "Contributor" = "#004d65"
)

de_id_redgreen_gradient <- col_numeric(
  palette = brewer.pal(11, "PiYG"),
  domain = c(-1.3, 1.3)
)

