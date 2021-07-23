

# settings

options(readr.num_columns = 0)

# load libraries ----------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(cli, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
devtools::install_github(repo = "https://github.com/jrabensc/rabtools")

# read environment variables ----------------------------------------------

firstname <- rabtools::env_default(env = "firstname", default = "firstname")
surname <- rabtools::env_default(env = "surname", default = "surname") 
birthday <- rabtools::env_default(env = "birthday", default = "birthday")
systolic_label <- rabtools::env_default(env = "systolic_label", default = "systolic_label")
diastolic_label <- rabtools::env_default(env = "diastolic_label", default = "diastolic_label")
measurement_label <- rabtools::env_default(env = "measurement_label", default = "measurement_label")
values_label <- rabtools::env_default(env = "values_label", default = "values_label")
plot_title_label <- rabtools::env_default(env = "plot_title_label", default = "plot_title_label")
time_axis_label <- rabtools::env_default(env = "time_axis_label", default = "time_axis_label")
value_axis_label <- rabtools::env_default(env = "value_axis_label", default = "value_axis_label")
time_locale <- rabtools::env_default(env = "time_locale", default = "time_locale")

file_name <- base::paste0(base::tolower(surname),
                          "_",
                          base::tolower(firstname),
                          "_",
                          birthday,
                          "_bloodpressure_")

cli::cli_alert_info("Starting plotting process.")

# define functions --------------------------------------------------------

prepare_data <- function(.data, systolic_label, diastolic_label, measurement_label, values_label,...) {
  .data %>%
    dplyr::rename(
      systolic_label = 2,
      diastolic_label = 3,
      "timestamp" = 1
    ) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
    tidyr::pivot_longer(cols = c(systolic_label, diastolic_label), names_to = measurement_label, values_to = values_label)
}

filter_data <- function(.data, year, ...) {
  .data %>% 
    dplyr::filter(year(timestamp) == year)
}

calc_mean_Werte <- function(.data, measurement, measurement_label, values_label, ...) {
  .data %>% 
    dplyr::filter(measurement_label == !!measurement) %>% 
    dplyr::summarise(mean = round(mean(values_label), digits = 0))
}
   
draw_plot <- function(.data, year, measurement_label, values_label, value_axis_label, plot_title_label) {
  .data %>% 
  ggplot(aes(x = timestamp, y = values_label)) +
    geom_line(aes(color = measurement_label), size = 1) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    geom_text(aes(label = values_label), vjust = -.5) +
    ggtitle(paste0(plot_title_label, ", ", year, ", ", firstname, " ", surname, ", ", birthday)) +
    scale_x_datetime(name = time_axis_label, date_breaks = "1 month", date_labels = "%B" ) +
    scale_y_continuous(name = value_axis_label) +
    annotate("text", x = as.POSIXct(paste0(year, "-01-01")), y = 55, hjust = .1, label = paste0("Ø ", diastolic_label, " = ", .data %>% calc_mean_Werte(measurement = diastolic_label))) +
    annotate("text", x = as.POSIXct(paste0(year, "-01-01")), y = 60, hjust = .1, label = paste0("Ø ", systolic_label, " = ", .data %>% calc_mean_Werte(measurement = systolic_label))) +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme_minimal()
}

create_blodpressure_plot <- function(.data, year = NULL) {
  .data %>% 
    prepare_data(systolic_label = systolic_label,
                 diastolic_label = diastolic_label,
                 measurement_label = measurement_label,
                 values_label = values_label) %>% 
    filter_data(year = year) %>% 
    draw_plot(year = year,
              measurement_label = measurement_label,
              values_label = values_label,
              value_axis_label = value_axis_label,
              plot_title_label = plot_title_label)
}

# create the plots --------------------------------------------------------

for (i in 2018:year(today())) {
  read_csv("/main/01-data/data.csv") %>% 
  create_blodpressure_plot(year = i) %>% 
  ggsave(filename = paste0("/main/03-output/", file_name, i, ".pdf"), device = "pdf", width = 297, height = 210, units = "mm")
  cli::cli_alert_info("Created plot for year {i} as {file_name}{i}.pdf")
}

cli::cli_alert_success("All plots created!")
