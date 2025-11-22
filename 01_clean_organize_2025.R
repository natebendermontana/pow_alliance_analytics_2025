# clean and organize

knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,  
                      message = FALSE)
options(scipen = 999)

library(readxl)
library(ggthemes)
library(ggplot2)
library(forcats)
library(here)
library(tidyr)
library(tidytext)
library(tictoc)
library(scales)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)

theme_set(theme_fivethirtyeight())

####################
# Clean up filenames
####################
data_folder <- here("data/")
files <- list.files(data_folder)
files

new_names <- c(
  #"2024 Data for Nate - Athlete Alliance AAA and Creative Grant Projects.xlsx" = "2024_aaa_grant_projects.xlsx",
  "POW Empowerment Grant  (Responses).xlsx" = "2025_empowerment_grants.xlsx",
  "PR Placements.xlsx" = "2025_pr_placements.xlsx",
  "All Athlete, Creative, Science Alliances-2025-11-18-10-42-41.xlsx" = "2025_us_active_alliance_members.xlsx",
  "2025 Alliance Gathering_ Attendence Data.xlsx" = "2025_alliance_appreciation_event.xlsx",
  "2025 Alliance Engagement Tracking.xlsx" = "2025_alliance_mobilization.xlsx",
#  "2024 Alliance Data for Nate - Compilation of Engagements from Newsletters.xlsx" = "2024_alliance_engagements.xlsx",
#  "SOCIAL DATA FROM SPROUT Relationships-Overview-Post-Count-2025-01-01-2025-11-19-1763493637.csv" = "2025_alliance_instagram.csv",
#  "1.9.25 NEW Data - alliance data - disaggregation.xlsx" = "2024_alliance_disaggregation.xlsx",
#  "1.9.25 NEW Data - Story Data.xlsx" = "2024_alliance_insta_stories.xlsx",
 # "1.9.25 NEW Data - Updated 2024 Alliance Data for Nate - Dash Hudson Instagram Posting Data.xlsx" = "2024_alliance_insta_posts.xlsx",
 # "2024 Leadership Summit - Athlete Attendees.xlsx" = "2024_leadership_summit.xlsx",
"2025 Reconciliation & Public Lands Campaigns - Alliance Contacts .xlsx" = "2025_reconciliation_pl_campaigns.xlsx",
"SSOT - Reconciliation LTE_OPed Tracker.xlsx" = "2025_reconciliation_tracking.xlsx"
)

for (old_name in names(new_names)) {
  file.rename(
    from = file.path(data_folder, old_name),
    to = file.path(data_folder, new_names[old_name])
  )
}

excel_files <- list.files(data_folder, pattern = "\\.xlsx$", full.names = TRUE)

# Loop through each Excel file and convert it to CSV
for (file in excel_files) {
  csv_file <- sub("\\.xlsx$", ".csv", file)
  data <- read_excel(file)
  write.csv(data, csv_file, row.names = FALSE)
  cat("Converted:", file, "to", csv_file, "\n")
}

csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
csv_files

#####################################
### Total US Athlete Alliance Members
#####################################
df_athlete_ids <-  read.csv(here("data",  "2024_us_active_alliance_members.csv")) %>% 
  clean_names()%>% 
  select(salesforce_id, first_name, last_name)

df_alliance_raw <- read.csv(here("data",  "2025_us_active_alliance_members.csv"), skip=9) %>% 
  select(-NA.) %>% 
  slice_head(n = -5) %>% 
  clean_names() %>% 
  mutate(
    alliance_group = trimws(alliance_group),
    alliance_group = na_if(alliance_group, "NA"),
    alliance_group = na_if(alliance_group, "<NA>"),
    alliance_group = na_if(alliance_group, "")
  ) %>%
  filter(!alliance_group %in% c("Creative", "Science")) %>%
  drop_na(alliance_group)

# colnames(df_alliance_raw) <- c(
#   "first_name",        # First.Name -> first_name
#   "last_name",         # Last.Name -> last_name
#   "investment_level",  # Investment.Level -> investment_level
#   "alliance_status",   # Alliance.Status -> alliance_status
#   "alliance_type",     # Alliance.Type -> alliance_type
#   "alliance_group",    # Alliance.Group -> alliance_group
#   "facebook_url",      # Facebook -> facebook_url
#   "instagram_url",     # Instagram -> instagram_url
#   "twitter_x_url",     # Twitter -> twitter_x_url
#   "salesforce_id"      # Salesforce.ID -> salesforce_id
# )

# new_columns <- c("salesforce_id", "first_name", "last_name", "alliance_type", "alliance_group", 
#                  "alliance_status", "investment_level", "facebook_url", "instagram_url", "twitter_x_url")
# 
# df_alliance_raw[new_columns[!new_columns %in% colnames(df_alliance_raw)]] <- NA

df_alliance <- df_alliance_raw %>% 
  left_join(df_athlete_ids, by = c("first_name", "last_name")) %>% 
  filter(!is.na(salesforce_id) & salesforce_id != "") %>% 
  mutate(investment_level = case_when(
    investment_level == "Higher" ~ "low",
    investment_level == "Further" ~ "med",
    investment_level == "Deeper" ~ "high",
    # almost all of the athletes with NA investment level are Prospects. 
    # Just going to assume these folks are low investment
    is.na(investment_level) ~ "low"
  ),
  first_name = ifelse(first_name == "Torey lee", "Torey Lee", first_name),
  first_name = ifelse(first_name == "Philip", "Phil", first_name),
  last_name = ifelse(last_name == "Reyes-acosta", "Reyes-Acosta", last_name),
  last_name = ifelse(last_name == "Zynobia newman", "Zynobia", last_name),
  last_name = ifelse(last_name == "O'keeffe", "O'Keeffe", last_name),
  last_name = ifelse(last_name == "Digiulian", "DiGiulian", last_name),         
  last_name = ifelse(last_name == "Mccloy", "McCloy", last_name),
  last_name = ifelse(last_name == "O'leary", "O'Leary", last_name),
  last_name = ifelse(last_name == "Rubio-macwright", "Rubio MacWright", last_name),
  # clean up instagram URLs for matching with the Dash-Hudson social data later
  # instagram_username = instagram_url %>%
  #   str_to_lower() %>%  # Convert everything to lowercase
  #   str_remove_all("^@") %>%  # Remove leading '@' if present
  #   str_remove_all("^(https?://)?(www\\.)?instagram\\.com/") %>%  # Remove URL prefixes (with or without http/https)
  #   str_remove_all("\\?.*$") %>%  # Remove trailing query strings
  #   str_remove_all("^www\\.") %>%  # Remove "www." prefix
  #   str_remove_all("/$") %>%  # Remove trailing slashes
  #   str_split(",\\s*") %>%  # Split by commas (with optional spaces)
  #   sapply(function(x) x[1]) %>%  # Take the first username
  #   str_trim()  # Trim any extra whitespace
  ) %>% 
  # # clean up a couple rows manually
  # mutate(
  #   instagram_username = case_when(
  #     instagram_username == "captain calhoun" ~ "captaincalhoun",  # Adjust "captain calhoun"
  #     instagram_username == "jeaneecranemauzy@gmail.com" ~ NA_character_,  # Replace email with NA
  #     TRUE ~ instagram_username)  # Keep all other values unchanged
  # ) %>%  
  mutate(
    full_name = paste(first_name, last_name)
  ) %>% 
  select(salesforce_id, full_name, everything())

# manually update some missing Instagram usernames
# df_alliance <- df_alliance %>%
#   mutate(
#     instagram_username = case_when(
#       first_name == "Jeremy" & last_name == "Jones" ~ "jeremyjones",
#       first_name == "Jessie" & last_name == "Diggins" ~ "jessiediggins",
#       TRUE ~ instagram_username
#     )
#   )

# Total US Alliance
tot_us_athletes <- df_alliance %>% 
  summarize(unique_id_count = n_distinct(salesforce_id)) %>% 
  pull(unique_id_count)
print(paste0("There are ", tot_us_athletes, " US Alliance members"))


#####################################
### PR Placements
#####################################

df_pr_raw <- read.csv(here("data",  "2025_pr_placements.csv"), skip = 2) %>% 
  select(-NA.) %>%
  clean_names() %>% 
  drop_na(date)


# pr_placement_date <- c(rep("Q1", 8), rep("Q2", 6), rep("Q3", 5), rep("Q4", 2))

df_pr <- df_pr_raw %>%
  mutate(
    date_clean = str_replace(date, "Late 2025", "December 2025"),
    date_clean = my(date_clean),
    quarter = paste0("Q", quarter(date_clean))
  ) %>% 
  # split on semicolons first
  mutate(alliance_member_s = str_split(alliance_member_s, ";")) %>%
  unnest(alliance_member_s) %>%
  mutate(alliance_member_s = str_trim(alliance_member_s)) %>%
  rowwise() %>%
  mutate(
    alliance_member_s = list({
      x <- alliance_member_s
      
      # Handle "Ben & Zach Anderson"
      if (str_detect(x, " & ")) {
        # extract surname (last word)
        surname <- word(x, -1)
        
        # extract first names before surname
        firsts <- str_remove(x, paste0(" ", surname, "$"))
        firsts <- str_split(firsts, " & ")[[1]]
        
        # rebuild full names
        paste(firsts, surname)
      } else {
        x
      }
    })
  ) %>%
  unnest(alliance_member_s) %>%
  ungroup() %>% 
  rename(full_name = alliance_member_s) %>% 
  
  
  # filter(!is.na(Q1) & !Q1 %in% c("Q1", "Q2", "Q3", "Q4", "First Name")) %>% 
  # mutate(engagement_date_qtr = c(rep("Q1", 8), rep("Q2", 6), rep("Q3", 5), rep("Q4", 2))
  # ) %>% 
  # rename(
  #   first_name = Q1,
  #   last_name = `...2`,
  #   engagement_title = `...3`
  # ) %>%
  # bring in the salesforce_id
  left_join(
    df_alliance %>% select(full_name, salesforce_id), 
    by = c("full_name") 
  )

# manual_dates <- tibble(
#   first_name = c(
#     "Jared", "Gus", "Alex", "Graham", "Christopher", "Amie", "Graham", "Bea", 
#     "Christopher", "Jeremy", "Torey Lee", "Gus", "Graham", "Bea", "Torey Lee", 
#     "Barry", "Jeremy", "Amie"
#   ),
#   last_name = c(
#     "Shumate", "Schumacher", "Deibold", "Zimmerman", "Blevins", "Engerbretson", "Zimmerman",
#     "Kim", "Blevins", "Jones", "Brooks", "Schumacher", "Zimmerman", "Kim", "Brooks",
#     "Wicks", "Jones", "Engerbretson"
#   ),
#   engagement_date_qtr = c(
#     "Q1", "Q1", "Q1", "Q1", "Q1", "Q1", "Q2", "Q2",
#     "Q2", "Q2", "Q2", "Q2", "Q3", "Q3", "Q3", "Q3", "Q4", "Q4"
#   ),
#   engagement_date = as.Date(c(
#     NA, "2024-02-09", NA, "2024-03-06", "2024-07-24", "2024-02-25", "2024-08-21", NA, 
#     NA, NA, NA, "2024-06-30", NA, NA, NA, NA, "2024-09-23", "2024-10-28"
#   ))
# )

# if i couldn't find a specific date for a PR piece, I input the date as the first day of the quarter that the piece was published.
quarter_start_dates <- c(
  "Q1" = as.Date("2024-01-01"),
  "Q2" = as.Date("2024-04-01"),
  "Q3" = as.Date("2024-07-01"),
  "Q4" = as.Date("2024-10-01")
)

df_pr <- df_pr %>%
  left_join(manual_dates, by = c("first_name", "last_name", "engagement_date_qtr")) %>%
  mutate(
    engagement_date = if_else(
      is.na(engagement_date),  
      quarter_start_dates[engagement_date_qtr],  
      engagement_date  
    )
  ) %>%
  select(-engagement_date_qtr)  



# Calculate the number of NA salesforce_id removed - these are non-athlete Alliance members
na_counts <- list()
na_counts$df_pr <- sum(is.na(df_pr$salesforce_id))

df_pr <- df_pr %>%
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, first_name, last_name, everything())
