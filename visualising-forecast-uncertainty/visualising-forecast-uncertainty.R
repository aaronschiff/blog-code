# Visualising forecast uncertainty

library(tidyverse)
library(here)
library(glue)
library(janitor)
library(scales)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(as.charts)   # Custom library for formatting charts nicely

# Custom font setup
systemfonts::register_font(
  name = "National 2 Custom", 
  plain = systemfonts::system_fonts() |> filter(family == "National 2", style == "Regular") |> pull(path), 
  bold = systemfonts::system_fonts() |> filter(family == "National 2", style == "Extrabold") |> pull(path), 
  italic = systemfonts::system_fonts() |> filter(family == "National 2", style == "Regular Italic") |> pull(path), 
  bolditalic = systemfonts::system_fonts() |> filter(family == "National 2", style == "Extrabold Italic") |> pull(path), 
  features = systemfonts::font_feature(ligatures = c("discretionary", 
                                                     "standard", 
                                                     "contextual"), 
                                       numbers = c("lining", "proportional"))
)

# Custom geom_text with custom font
geom_text_custom <- function(family = "National 2 Custom",
                             fontface = "bold",
                             ...) {
  t <- shadowtext::geom_shadowtext(family = family,
                                   fontface = fontface,
                                   bg.colour = "white", 
                                   bg.r = 0.08, 
                                   ...)
  return(t)
}

# Function to position labels nicely on line charts
vjust_up <- -0.75
vjust_down <- 1.75
label_vjust = function(x) {
  y <- tibble(x = x) |> 
    mutate(slope_left = x - dplyr::lag(x), 
           slope_right = dplyr::lead(x) - x) |> 
    mutate(vjust = case_when(
      is.na(slope_left) & slope_right <= 0 ~ vjust_up, 
      is.na(slope_left) & slope_right > 0 ~ vjust_down, 
      slope_left <= 0 & slope_right <= 0 ~ vjust_up, 
      slope_left <= 0 & slope_right > 0 ~ vjust_down, 
      slope_left > 0 & slope_right > 0 ~ vjust_down, 
      slope_left > 0 & slope_right <= 0 ~ vjust_up, 
      slope_left <= 0 & is.na(slope_right) ~ vjust_down, 
      slope_left > 0 & is.na(slope_right) ~ vjust_up
    ))
  return(y$vjust)
}

# Load and clean NZ quarterly unemployment data
dat <- read_csv(file = here("visualising-forecast-uncertainty/unemployment.csv"), 
                col_types = "cn", 
                skip = 1) |>
  clean_names() |>  
  rename(period = x1, unemp = hlfq_s1f3s) |> 
  filter(!is.na(unemp)) |> 
  separate(col = period, into = c("year", "quarter"), sep = "Q", 
           convert = TRUE) |>
  mutate(quarter = as.integer(quarter)) |> 
  mutate(date = yearquarter(glue("{year}Q{quarter}"))) |>
  filter(year > 1989) |> 
  select(date, year, quarter, unemp) |> 
  mutate(unemp = unemp / 100) |> 
  as_tsibble(index = date, regular = TRUE)

# Forecasting model
l <- guerrero(x = dat$unemp)

model <- dat |> 
  model(
    arima = ARIMA(formula = box_cox(x = unemp, lambda = l), 
                  ic = "bic")
  ) 

# Uncertainty simulations
forecast_uncertainty_sims <- model |> 
  generate(h = 8, 
           times = 5000, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  select(-.model) 

# Forecast uncertainty intervals as percentiles of simulations
forecast_uncertainty_intervals <- full_join(
  # 90% interval lower limit
  x = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_lower = quantile(x = .sim, probs = 0.05)), 
  
  # 90% interval upper limit
  y = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_upper = quantile(x = .sim, probs = 0.95)), 
  
  by = "date"
) |> 
  # 50% interval lower limit
  full_join(
    y = forecast_uncertainty_sims |> 
      group_by(date) |>
      summarise(central_lower = quantile(x = .sim, probs = 0.25)), 
    by = "date"
  ) |> 
  # 50% interval upper limit
  full_join(
    y = forecast_uncertainty_sims |> 
      group_by(date) |>
      summarise(central_upper = quantile(x = .sim, probs = 0.75)), 
    by = "date"
  )

# Actuals to show on the chart: twice as many points as in the forecast
vis_actuals <- dat |> 
  as_tibble() |>
  slice_max(order_by = date, n = 8) |> 
  arrange(date) |> 
  mutate(vjust = label_vjust(unemp))

# Create data for for visualisation - prepend last actual value of
# unemployment rate
last_actual_unempl_rate <- dat |> 
  as_tibble() |> 
  slice_max(order_by = date, n = 1) |> 
  select(date, unemp)

vis_uncertainty <- bind_rows(
  forecast_uncertainty_sims, 
  tibble(.rep = as.character(1:5000), 
         last_actual_unempl_rate |> rename(.sim = unemp))
) |> 
  arrange(.rep, date)

vis_intervals <- bind_rows(
  forecast_uncertainty_intervals |> 
    mutate(type = "forecast"), 
  last_actual_unempl_rate |> 
    mutate(type = "actual")
) |> 
  mutate(conf_lower = ifelse(is.na(conf_lower), unemp, conf_lower), 
         conf_upper = ifelse(is.na(conf_upper), unemp, conf_upper), 
         central_lower = ifelse(is.na(central_lower), unemp, central_lower), 
         central_upper = ifelse(is.na(central_upper), unemp, central_upper)) |> 
  select(-unemp) |> 
  arrange(date) |> 
  pivot_longer(cols = c("conf_lower", "conf_upper", "central_lower", "central_upper"), 
               names_to = "limit", 
               values_to = "value") |> 
  mutate(limit_type = case_when(
    str_detect(string = limit, pattern = "conf_") ~ "conf", 
    str_detect(string = limit, pattern = "central_") ~ "central", 
    TRUE ~ NA_character_
  ))

# Date labels
vis_dates <- tibble(
  date = seq(from = min(vis_actuals$date), 
             to = max(vis_intervals$date), 
             by = 1)
) |> 
  mutate(year = as.integer(year(date)), 
         quarter = quarter(date)) |> 
  mutate(label = ifelse(quarter == 1L, 
                        glue("Q{quarter}\n{year}"), 
                        glue("Q{quarter}"))) 

# Chart colours 
colour_actuals <- "black"
colour_forecasts <- "midnightblue"
colour_intervals <- "firebrick"
colour_intervals_alt <- colorspace::lighten(col = colour_intervals, 0.5)
alpha_uncertainty <- 0.1
colour_zeroline <- grey(0.5)
halo_colour <- "white"

# Chart line sizes etc
linetype_forecast_intervals <- "solid"
linesize_forecast_intervals <- 0.25
linetype_forecast_central <- "solid"
linesize_forecast_central <- 0.25
linesize_actuals <- 0.25
linesize_zeroline <- 0.15
linesize_uncertainty <- 0.1
point_size <- 1
point_stroke <- 0.25
linesize_af <- 0.25
linetype_af <- "dotted"
label_size <- 1.8
label_size_small <- 1.5
latest_data <- "2021Q4"
forecast_label_y <- 0.065

# Create visualisation
chart_forecasts <- ggplot() +
  # Zero line
  geom_hline(yintercept = 0, 
             size = linesize_zeroline, 
             colour = colour_zeroline) + 
  
  # Actual / forecast line
  geom_vline(xintercept = as.Date(yearquarter(latest_data)), 
             size = linesize_af, 
             colour = colour_forecasts, 
             linetype = linetype_af) + 
  
  # Forecast period label
  annotate(geom = "text", 
           x = as.Date(yearquarter(latest_data)) + dmonths(1), 
           y = forecast_label_y, 
           hjust = 0, 
           label = "Forecast", 
           family = "National 2 Custom", 
           fontface = "bold", 
           size = 2.5, 
           colour = colour_forecasts) + 
  
  # Actual period label
  annotate(geom = "text", 
           x = as.Date(yearquarter(latest_data)) - dmonths(1), 
           y = forecast_label_y, 
           hjust = 1, 
           label = "Actual", 
           family = "National 2 Custom", 
           fontface = "bold", 
           size = 2.5, 
           colour = colour_actuals) + 
  
  # Uncertainty simulations
  geom_line(data = vis_uncertainty, 
            mapping = aes(x = date, 
                          y = .sim, 
                          group = .rep), 
            size = linesize_uncertainty, 
            colour = colour_forecasts, 
            alpha = alpha_uncertainty) + 
  
  # Actuals line
  geom_line(data = vis_actuals, 
            mapping = aes(x = date, 
                          y = unemp), 
            colour = colour_actuals, 
            size = linesize_actuals) + 
  
  # Actuals labels
  geom_text_custom(data = vis_actuals, 
                   mapping = aes(x = date, 
                                 y = unemp, 
                                 label = comma(x = 100 * unemp, 
                                               accuracy = 0.1), 
                                 vjust = vjust), 
                   size = label_size, 
                   colour = colour_actuals) + 
  
  # Uncertainty interval lines
  geom_line(data = vis_intervals, 
            mapping = aes(x = date, 
                          y = value, 
                          colour = limit_type, 
                          group = limit), 
            size = linesize_forecast_intervals, 
            linetype = linetype_forecast_intervals) + 
  
  # Uncertainty interval points
  geom_point(data = vis_intervals, 
             mapping = aes(x = date, 
                           y = value, 
                           fill = limit_type), 
             colour = halo_colour, 
             shape = 21, 
             size = point_size, 
             stroke = point_stroke) + 
  
  # Uncertainty interval labels
  geom_text_custom(data = vis_intervals |> 
                     filter(type == "forecast", 
                            limit %in% c("central_upper", 
                                         "conf_upper")), 
                   mapping = aes(x = date, 
                                 y = value, 
                                 colour = limit_type, 
                                 size = limit_type, 
                                 label = comma(x = 100 * value, 
                                               accuracy = 0.1), 
                                 vjust = vjust_up), 
                   show.legend = FALSE) + 
  geom_text_custom(data = vis_intervals |> 
                     filter(type == "forecast", 
                            limit %in% c("central_lower", 
                                         "conf_lower")), 
                   mapping = aes(x = date, 
                                 y = value, 
                                 colour = limit_type, 
                                 size = limit_type, 
                                 label = comma(x = 100 * value, 
                                               accuracy = 0.1), 
                                 vjust = vjust_down), 
                   show.legend = FALSE) + 
  
  # Actual points
  geom_point(data = vis_actuals, 
             mapping = aes(x = date, 
                           y = unemp), 
             fill = colour_actuals, 
             colour = halo_colour, 
             shape = 21, 
             size = point_size,
             stroke = point_stroke) + 
  
  # Scales
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.07),
                     breaks = seq(0, 0.08, 0.01)) +
  scale_x_yearquarter(breaks = vis_dates$date, 
                      labels = vis_dates$label, 
                      expand = expansion(0.05, 0)) + 
  scale_colour_manual(values = c("central" = colour_intervals, 
                                 "conf" = colour_intervals_alt), 
                      labels = c("50% of forecasts are in this range", 
                                 "90% of forecasts are in this range"), 
                      name = NULL, 
                      aesthetics = c("colour", "fill")) + 
  scale_size_manual(values = c("central" = label_size, 
                               "conf" = label_size_small), 
                    guide = "none")

output_chart(chart = chart_forecasts, 
             path = here("visualising-forecast-uncertainty"), 
             orientation = "wide", 
             xlab = "", 
             ylab = "", 
             legend_position = "top")
