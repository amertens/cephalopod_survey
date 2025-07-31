

d <- read.csv(here("data", "Octopus Farming - FINAL DATA - 030725.csv"))
weights <- read.csv(here("data", "survey_weights_2024_EU14.csv"))


d$COUNTRY <- factor(d$COUNTRY, levels = c(1:14, 99), labels = c("Slovenia", "France", "Germany", "Portugal", "Italy", "Poland", "Spain", "Sweden", "Netherlands", "Denmark", "UK", "Greece", "Czechia", "Belgium", "None of the above"))
table(d$COUNTRY)

d$dAGE <- factor(d$dAGE, levels = c(1:6), labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
table(d$COUNTRY, d$dAGE, d$Gender)
table(d$dAGE)


d=readRDS(here("data", "cleaned_octopus_farming_data.RDS"))
table(d$COUNTRY, d$dAGE, d$Gender)

d %>% group_by(COUNTRY, Gender, dAGE) %>%
  summarise(n = n(), .groups = "drop")
  



library(dplyr)
library(tidyr)
library(readr)

# Load your cleaned data and census weights
library(here)
d <- readRDS(here("data", "cleaned_octopus_farming_data.RDS"))
weights <- read_csv(here("data", "survey_weights_2024_EU14.csv"))

# Step 1: Clean and summarize survey data for Male and Female only
survey_summary <- d %>%
  filter(Gender %in% c("Male", "Female")) %>%
  mutate(
    sex = ifelse(Gender == "Male", "M", "F"),
    age_band = dAGE,
    country = COUNTRY
  ) %>%
  count(country, sex, age_band, name = "n") %>%
  group_by(country, sex) %>%
  mutate(survey_share = n / sum(n)) %>%
  ungroup()

# Add country code mapping
country_map <- c(
  "Belgium"      = "BE",
  "Czechia"      = "CZ",
  "Denmark"      = "DK",
  "Germany"      = "DE",
  "Greece"       = "EL",
  "Spain"        = "ES",
  "France"       = "FR",
  "Italy"        = "IT",
  "Netherlands"  = "NL",
  "Poland"       = "PL",
  "Portugal"     = "PT",
  "Sweden"       = "SE",
  "Slovenia"     = "SI",
  "UK"           = "UK"
)

survey_summary <- survey_summary %>%
  mutate(country = recode(as.character(country), !!!country_map))

# Step 2: Merge with census weights
merged <- survey_summary %>%
  left_join(weights, by = c("country", "sex", "age_band")) %>%
  mutate(representation_ratio = survey_share / weight)

# Step 3: View summary
representation_table <- merged %>%
  select(country, sex, age_band, representation_ratio) %>%
  pivot_wider(names_from = age_band, values_from = representation_ratio)

print(representation_table, n = Inf)

library(forcats)

summary(merged$representation_ratio)

# Step 1: Use the merged data (after join and ratio calculation)
# If you've already run up to `merged <- ...`, use this:

heat_data <- merged %>%
  mutate(
    label = paste0(country, " (", sex, ")"),
    label = fct_reorder(label, as.numeric(factor(country))*10 + ifelse(sex == "F", 1, 0))
  ) %>%
  select(label, age_band, representation_ratio)

# Step 2: Plot
ggplot(heat_data, aes(x = age_band, y = label, fill = representation_ratio)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = 1, limits = c(0.5, 1.5), oob = scales::squish,
                       name = "Survey / Census") +
  labs(
    title = "Survey Representativeness vs Census Weights (by Sex × Age × Country)",
    x = "Age Band", y = "Country (Sex)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# export to CSV
write.csv(merged, here("results/survey_vs_population_representativeness.csv"), row.names = FALSE)
