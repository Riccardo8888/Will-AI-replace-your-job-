# Install packages
# Uncomment these lines first run; then comment them again.
# install.packages("tidyverse")
# install.packages("countrycode")
# install.packages("glmnet")
# install.packages("caret")

# Load packages
library(tidyverse)   # data wrangling
library(countrycode) # convert country codes
library(glmnet)      # LASSO
library(caret)       # preprocessing (scaling)

root <- "." # adjust to your root path

# List the skills penetration datasets
skill_dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE) %>%
  .[str_starts(basename(.), "skills_penetration_")]

skill_dirs

skills_list <- lapply(skill_dirs, function(d) {
  read_csv(file.path(d, "data.csv")) %>%
    mutate(
      Industry = str_trim(Industry),             # removes weird leading space
      industry_source = basename(d)   
    )
})

skills_all <- bind_rows(skills_list)

glimpse(skills_all)

skills_all <- skills_all %>%
  mutate(
    country_iso2  = Country,
    country_iso3  = countrycode(country_iso2, "iso2c", "iso3c")
  )

# Find all investment datasets in the directory
inv_dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE) %>%
  .[str_starts(basename(.), "investment_")]

inv_dirs

# Read each data.csv and FORCE Sum_of_deals and Year to be numeric since it was tweaking
inv_list <- lapply(inv_dirs, function(d) {
  # Read the CSV in this folder
  df <- read_csv(file.path(d, "data.csv"), show_col_types = FALSE)
  
  # Make sure the key numeric columns are numeric in every file
  if ("Sum_of_deals" %in% names(df)) {
    df <- df %>%
      mutate(Sum_of_deals = suppressWarnings(as.numeric(Sum_of_deals)))
  }
  
  if ("Year" %in% names(df)) {
    df <- df %>%
      mutate(Year = suppressWarnings(as.integer(Year)))
  }
  
  # Clean industry and record which folder it came from
  df <- df %>%
    mutate(
      INDUSTRY        = str_trim(INDUSTRY),
      industry_source = basename(d)
    )
  
  return(df)
})

# Stack it all into one big data frame
inv_all <- bind_rows(inv_list)

glimpse(inv_all)

# Remove EU aggregates (I'll keep it simple for now)
bad_countries <- c("EU27", "EU27_2020", "EU28", "EA20")

inv_all <- inv_all %>%
  filter(!Country %in% bad_countries)

# Map detailed investment industries to the 5 broad sectors
industry_map <- tribble(
  ~INDUSTRY_clean,                             ~Industry_skill,
  "Education and training",                    "Education",
  "Financial and insurance services",          "Financial services",
  "IT infrastructure and hosting",             "information and media",
  "robots, sensors and IT hardware",           "information and media",
  "Digital security",                          "information and media",
  "Media, social platforms and marketing",     "information and media",
  "Business processes and support services",   "Professional services",
  "Consumer services",                         "Professional services",
  "Travel, leisure and hospitality",           "Professional services",
  "Government, security and defence",          "Professional services",
  "Food and beverages",                        "Manufacturing",
  "Consumer products",                         "Manufacturing",
  "Agriculture",                               "Manufacturing",
  "Construction and air conditioning",         "Manufacturing",
  "Energy, raw materials and utilities",       "Manufacturing",
  "Logistics, wholesale and retail",           "Manufacturing",
  "Mobility and autonomous vehicles",          "Manufacturing",
  "Real estate",                               "Manufacturing",
  "Environmental services",                    "Manufacturing",
  "Other",                                     "Manufacturing"
)

# Clean INDUSTRY name, create ISO3 country code, and attach mapping
inv_all2 <- inv_all %>%
  mutate(
    INDUSTRY_clean = str_trim(INDUSTRY),
    country_iso3   = Country      # these are already ISO3 codes tbh
  ) %>%
  left_join(industry_map, by = "INDUSTRY_clean")

# drop rows where mapping failed (Industry_skill is NA) (OPTIONAL)
inv_all2 <- inv_all2 %>%
  filter(!is.na(Industry_skill))

glimpse(inv_all2)

# Aggregate total and mean deals by country and broad industry
inv_agg <- inv_all2 %>%
  group_by(country_iso3, Industry_skill) %>%
  summarise(
    deals_mean  = mean(Sum_of_deals, na.rm = TRUE),
    deals_total = sum(Sum_of_deals, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(inv_agg)

# Read each data.csv and combine
skills_list <- lapply(skill_dirs, function(d) {
  read_csv(file.path(d, "data.csv"), show_col_types = FALSE) %>%
    mutate(
      Industry      = str_trim(Industry),
      industry_src  = basename(d),
      country_iso2  = Country,
      country_iso3  = countrycode(country_iso2, "iso2c", "iso3c")
    )
})

skills_all <- bind_rows(skills_list)

glimpse(skills_all)

# Keep the important columns
skills_clean <- skills_all %>%
  select(
    country_iso3,
    Country_label,
    Industry,
    AI_engineering_skills_penetration_factor
  )

glimpse(skills_clean)

# Merge skills penetration with investment data
data_industry <- skills_clean %>%
  left_join(inv_agg,
            by = c("country_iso3", "Industry" = "Industry_skill"))

glimpse(data_industry)

# Save for later use
write_csv(data_industry, "data_industry.csv")
