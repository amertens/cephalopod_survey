# Install/update once
#install.packages(c("restatapi", "eurostat"))

library(restatapi); library(eurostat)
library(dplyr); library(tidyr)
library(here)

library(restatapi)    # only this call is from restatapi

# 1. download the full TSV once (≈ 60 MB compressed)
demo <- get_eurostat_bulk("demo_pjan")   # cached automatically

countries   <- c("SI","FR","DE","PT","IT","PL","ES","SE","NL","DK","UK","EL","CZ","BE")
year_needed <- 2024
age_breaks  <- c(17, 24, 34, 44, 54, 64, Inf)
age_labels  <- c("18-24","25-34","35-44","45-54","55-64","65+")

# 2. filter & aggregate
weights_2024 <- demo |>
  filter(time == year_needed,
         geo  %in% countries,
         sex  %in% c("M","F"),
         grepl("^Y\\d+$", age)) |>            # single‑year ages only
  mutate(age_num  = as.integer(sub("^Y","", age)),
         age_band = cut(age_num,
                        breaks  = age_breaks,
                        labels  = age_labels,
                        right   = TRUE)) |>
  group_by(geo, sex, age_band) |>
  filter(!is.na(age_band)) %>%
  summarise(pop = sum(values), .groups = "drop") |>
  group_by(geo, sex) |>
  mutate(weight = pop / sum(pop)) |>
  ungroup() |>
  select(country = geo, sex, age_band, weight)

write.csv(weights_2024, here("data/survey_weights_2024_EU14.csv"), row.names = FALSE)
