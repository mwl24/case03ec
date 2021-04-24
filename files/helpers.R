arrest_plotter <- function(df, year_spec) {
  year_spec <- as.numeric(year_spec)
  df_year <- df %>% 
    filter(YEAR2 == year_spec)
  
  ggplot(df_year) + 
    geom_sf(aes(fill = Arrests)) +
    scale_fill_gradient(low = "#fde0dd", high = "#c51b8a") +
    labs(title = paste("Precinct Arrest Rate in", year_spec),
         subtitle = "Proportion of Arrests by Precinct",
         fill = "Arrest Rate") +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9, face = "italic"),
          legend.title = element_text(size = 10)) 
}

force_plotter <- function(df, year_spec) {
  year_spec <- as.numeric(year_spec)
  df_year <- df %>% 
    filter(YEAR2 == year_spec)
  
  ggplot(df_year) + 
    geom_sf(aes(fill = MaxForce)) +
    scale_fill_gradient(low = "#fde0dd", high = "#c51b8a") +
    labs(title = paste("Precinct Force Rate in", year_spec),
         subtitle = "Proportion of SQF Involving Use of Force by Precinct",
         fill = "Physical Force \n Proportion") +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9, face = "italic"),
          legend.title = element_text(size = 10))
}