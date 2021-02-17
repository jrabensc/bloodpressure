

# settings


options(readr.num_columns = 0)
invisible(Sys.setlocale('LC_TIME', 'de_DE.UTF-8'))

# load libraries ----------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(cli, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

# define functions --------------------------------------------------------

prepare_data <- function(.data, ...) {
  .data %>%
    rename(
      "systolisch" = 2,
      "diastolisch" = 3,
      "timestamp" = 1
    ) %>%
    drop_na() %>%
    mutate(timestamp = ymd_hms(timestamp)) %>%
    pivot_longer(cols = c(systolisch, diastolisch), names_to = "Messung", values_to = "Werte")
}

filter_data <- function(.data, year, ...) {
  .data %>% 
    filter(year(timestamp) == year)
}

calc_mean_Werte <- function(.data, Messung) {
  .data %>% 
    filter(Messung == !!Messung) %>% 
    summarise(mean = round(mean(Werte), digits = 0))
}
   
draw_plot <- function(.data, year) {
  .data %>% 
  ggplot(aes(x = timestamp, y = Werte)) +
    geom_line(aes(color = Messung), size = 1) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    geom_text(aes(label = Werte), vjust = -.5) +
    ggtitle(paste0("Blutdruck für das Jahr ", year, ", Johannes Rabenschlag, geb. 07.01.1991")) +
    scale_x_datetime(name = "Zeit", date_breaks = "1 month", date_labels = "%B" ) +
    scale_y_continuous(name = "Wert") +
    annotate("text", x = as.POSIXct(paste0(year, "-01-01")), y = 55, hjust = .1, label = paste0("Ø diastolisch = ", .data %>% calc_mean_Werte(Messung = "diastolisch"))) +
    annotate("text", x = as.POSIXct(paste0(year, "-01-01")), y = 60, hjust = .1, label = paste0("Ø systolisch = ", .data %>% calc_mean_Werte(Messung = "systolisch"))) +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme_minimal()
}

create_blodpressure_plot <- function(.data, year = NULL) {
  .data %>% 
    prepare_data() %>% 
    filter_data(year = year) %>% 
    draw_plot(year = year)
}

# create the plots --------------------------------------------------------

for (i in 2018:year(today())) {
  read_csv("/main/01-data/data.csv") %>% 
  create_blodpressure_plot(year = i) %>% 
  ggsave(filename = paste0("/main/03-output/rabenschlag_johannes_19910107_blutdruck_", i, ".pdf"), device = "pdf", width = 297, height = 210, units = "mm")
}

cli_alert_success("Plots created!")
