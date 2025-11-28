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
library(fuzzyjoin)
library(purrr)

theme_set(theme_fivethirtyeight())


################################################################################
# Functions
################################################################################
match_names <- function(df, df_confirmed, max_dist_total = 0.25) {
  
  clean_name <- function(x) {
    x %>%
      tolower() %>%
      gsub("[^a-z ]", "", .) %>%
      gsub("\\s+", " ", .) %>%
      trimws()
  }
  
  df_in <- df %>%
    mutate(
      row_id      = row_number(),
      first_clean = clean_name(first_name),
      last_clean  = clean_name(last_name),
      fi          = substr(first_clean, 1, 1),
      li          = substr(last_clean, 1, 1)
    )
  
  df_conf <- df_confirmed %>%
    mutate(
      first_clean = clean_name(first_name),
      last_clean  = clean_name(last_name),
      fi          = substr(first_clean, 1, 1),
      li          = substr(last_clean, 1, 1)
    )
  
  # Block by first/last initial, then compute distances
  candidates <- df_in %>%
    inner_join(df_conf,
               by = c("fi" = "fi", "li" = "li"),
               suffix = c(".in", ".conf"))
  
  if (nrow(candidates) > 0) {
    candidates <- candidates %>%
      mutate(
        dist_first  = stringdist::stringdist(first_clean.in, first_clean.conf, method = "jw"),
        dist_last   = stringdist::stringdist(last_clean.in,  last_clean.conf,  method = "jw"),
        dist_total  = dist_first + dist_last
      )
    
    best <- candidates %>%
      group_by(row_id) %>%
      slice_min(dist_total, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        row_id,
        first_name_conf = first_name.conf,
        last_name_conf  = last_name.conf,
        dist_total
      )
  } else {
    best <- df_in %>%
      transmute(
        row_id,
        first_name_conf = NA_character_,
        last_name_conf  = NA_character_,
        dist_total      = NA_real_
      )
  }
  
  df_in %>%
    left_join(best, by = "row_id") %>%
    mutate(
      match_type = case_when(
        !is.na(dist_total) & dist_total == 0                          ~ "exact",
        !is.na(dist_total) & dist_total > 0 & dist_total <= max_dist_total ~ "fuzzy",
        TRUE                                                         ~ "unmatched"
      ),
      first_name_original = first_name,
      last_name_original  = last_name,
      first_name_new = ifelse(match_type == "fuzzy", first_name_conf, NA_character_),
      last_name_new  = ifelse(match_type == "fuzzy", last_name_conf, NA_character_)
    ) %>%
    select(
      match_type,
      first_name_original, last_name_original,
      first_name_new, last_name_new
    )
}


################################################################################
# Clean up filenames and load data
################################################################################
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

# Convert Excel → CSV, overwrite allowed if CSV already exists
for (file in excel_files) {
  csv_file <- sub("\\.xlsx$", ".csv", file)
  data <- readxl::read_excel(file)
  write.csv(data, csv_file, row.names = FALSE)
  cat("Created/updated CSV:", csv_file, "\n")
}

# Just return a vector of CSVs that now exist
csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
csv_files

# load data
df_athlete_ids <-  read.csv(here("data",  "2024_us_active_alliance_members.csv")) %>% 
  clean_names() %>% 
  select(salesforce_id, first_name, last_name, facebook, instagram, twitter)

df_alliance_raw <- read.csv(here("data",  "2025_us_active_alliance_members.csv"), skip=9) %>% 
  select(-NA.) %>% 
  slice_head(n = -5) %>% 
  clean_names() 

results <- match_names(df_alliance, df_athlete_ids)



################################################################################
### Total US Athlete Alliance Members
################################################################################

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
  mutate(
    alliance_group = trimws(alliance_group),
    alliance_group = na_if(alliance_group, "NA"),
    alliance_group = na_if(alliance_group, "<NA>"),
    alliance_group = na_if(alliance_group, "")
  ) %>%
  filter(!alliance_group %in% c("Creative", "Science")) %>%
  drop_na(alliance_group)

clean_name <- function(x) {
  x %>%
    tolower() %>%
    gsub("[^a-z ]", "", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

df_alliance %>%
  mutate(
    first_clean = clean_name(first_name),
    last_clean  = clean_name(last_name)
  ) %>%
  anti_join(
    df_athlete_ids %>%
      mutate(
        first_clean = clean_name(first_name),
        last_clean  = clean_name(last_name)
      ) %>%
      select(first_clean, last_clean),
    by = c("first_clean", "last_clean")
  ) %>%
  select(first_name, last_name)




















%>% 
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
  instagram_username = instagram %>%
    str_to_lower() %>%  # Convert everything to lowercase
    str_remove_all("^@") %>%  # Remove leading '@' if present
    str_remove_all("^(https?://)?(www\\.)?instagram\\.com/") %>%  # Remove URL prefixes (with or without http/https)
    str_remove_all("\\?.*$") %>%  # Remove trailing query strings
    str_remove_all("^www\\.") %>%  # Remove "www." prefix
    str_remove_all("/$") %>%  # Remove trailing slashes
    str_split(",\\s*") %>%  # Split by commas (with optional spaces)
    sapply(function(x) x[1]) %>%  # Take the first username
    str_trim()  # Trim any extra whitespace
  ) %>%
  # # clean up a couple rows manually
  mutate(
    instagram_username = case_when(
      instagram_username == "captain calhoun" ~ "captaincalhoun",  # Adjust "captain calhoun"
      instagram_username == "jeaneecranemauzy@gmail.com" ~ NA_character_,  # Replace email with NA
      TRUE ~ instagram_username)  # Keep all other values unchanged
  ) %>%
  mutate(
    full_name = paste(first_name, last_name)
  )  %>%
# manually update some missing Instagram usernames
  mutate(
    instagram_username = case_when(
      first_name == "Jeremy" & last_name == "Jones" ~ "jeremyjones",
      first_name == "Jessie" & last_name == "Diggins" ~ "jessiediggins",
      first_name == "Cody" & last_name == "Townsend" ~ "codytownsend",
      first_name == "Josh" & last_name == "Jespersen" ~ "joshjespersen",
      first_name == "Zeppelin" & last_name == "Zeerip" ~ "zeppelinzeerip",
      first_name == "Willis" & last_name == "Brown" ~ "willisbrown",
      first_name == "Jeanee" & last_name == "Crane-mauzy" ~ "jeaneecranemauzy",
      first_name == "Alyssa" & last_name == "Gonzalez" ~ "_alyssagonzalez",
      TRUE ~ instagram_username
    )
  ) %>% 
  select(salesforce_id, full_name, first_name, last_name, everything())



# Total US Alliance
tot_us_athletes <- df_alliance %>% 
  summarize(unique_id_count = n_distinct(salesforce_id)) %>% 
  pull(unique_id_count)
print(paste0("There are ", tot_us_athletes, " US Alliance members"))


################################################################################
### PR Placements
################################################################################

df_pr_raw <- read.csv(here("data",  "2025_pr_placements.csv"), skip = 2) %>% 
  select(-NA.) %>%
  clean_names() %>% 
  drop_na(date)

df_opeds_raw <- read.csv(here("data", "2025_reconciliation_tracking.csv")) %>% 
  clean_names() %>% 
  filter(alliance=="Athlete Alliance") %>% 
  rename(full_name = name) %>%
  mutate(date_clean = as.Date(lte_date_submission),
         format = type) %>%
  mutate(
    full_name = case_when(
      full_name == "Lyndsy Dyer" ~ "Lynsey Dyer", 
      full_name == "Nick Russel" ~ "Nick Russell", 
      full_name == "Blake Keough" ~ "Blake Keogh", 
    TRUE ~ full_name 
  )) %>% 
  select(full_name, format, date_clean)
  

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
  full_join(df_opeds_raw, by = c("full_name", "date_clean", "format")) %>% 
  # bring in the salesforce_id
  left_join(
    df_alliance %>% select(full_name, first_name, last_name, salesforce_id), 
    by = c("full_name") 
  ) %>% 
  select(salesforce_id, first_name, last_name, full_name, everything())

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
# quarter_start_dates <- c(
#   "Q1" = as.Date("2024-01-01"),
#   "Q2" = as.Date("2024-04-01"),
#   "Q3" = as.Date("2024-07-01"),
#   "Q4" = as.Date("2024-10-01")
# )

# df_pr <- df_pr %>%
#  left_join(manual_dates, by = c("first_name", "last_name", "engagement_date_qtr")) %>%
#   mutate(
#     engagement_date = if_else(
#       is.na(engagement_date),  
#       quarter_start_dates[engagement_date_qtr],  
#       engagement_date  
#     )
#   ) %>%
#   select(-engagement_date_qtr)  


# Calculate the number of NA salesforce_id removed - these are non-athlete Alliance members
sum(is.na(df_pr$salesforce_id))

df_pr <- df_pr %>%
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, full_name, first_name, last_name, everything())


################################################################################
### AAA and Empowerment grants
################################################################################
df_grants_aaa_raw <- read.csv(here("data",  "2024_aaa_grant_projects.csv"))

df_grants_aaa <- df_grants_aaa_raw %>% 
  rename(
    name = "Alliance.Member.Featured..Creator..Funding.Allocated.to",
    engagement_title = "Project.Name...Link..webpage.or.deck."
  ) %>%
  filter(!is.na(name)) %>% 
  separate(name, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>% 
  mutate(
    last_name = ifelse(last_name == "Olseger", "Osleger", last_name),
    engagement_date = case_when(
      engagement_title == "Chasing Washburn" ~ as.Date("2024-06-01"),
      engagement_title == "OUTLIER: Trust" ~ as.Date("2024-06-01"),
      engagement_title == "Full Circle Everest" ~ as.Date("2024-06-01"),
      engagement_title == "E.A.R.T.H. Gloster" ~ as.Date("2024-06-01"),
      engagement_title == "The Hypocrite" ~ as.Date("2024-02-21"),
      engagement_title == "Drilling Willow" ~ as.Date("2024-06-01"),
      engagement_title == "How To Recall" ~ as.Date("2024-08-01"),
      engagement_title == "Navajo Solar Sunrise" ~ as.Date("2024-06-01"),
      engagement_title == "Feast or Famine" ~ as.Date("2024-06-01"),
      engagement_title == "Origins" ~ as.Date("2024-06-01"),
      TRUE ~ as.Date(NA)
    )
  ) %>% 
  left_join(
    df_alliance %>% select(first_name, last_name, salesforce_id), 
    by = c("first_name", "last_name") 
  )

# add in dates for projects from looking them up manually
# griffin post chasing washburn: 06/01/24
# dani reyes acosta outlier trust: 06/01/24
# phil henderson full circle everest: 06/01/24
# peyton thomas earth gloster: 06/01/24
# amie engerbretson the hypocrite: 02/21/24
# len necefer drilling willow: 06/01/24
# christopher blevins: 08/01/24
# kitty calhoun navajo solar sunrise: 06/01/24
# chris cosentino feast or famine: 06/01/24
# dillon osleger origins: 06/01/24

na_counts$df_grants_aaa <- sum(is.na(df_grants_aaa$salesforce_id))

df_grants_aaa <- df_grants_aaa %>%
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, first_name, last_name, everything())

#### Empower grants
df_grants_empower_raw <- read.csv(here("data",  "2025_empowerment_grants.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_grants_empower <- df_grants_empower_raw %>% 
  select(name, pow_alliance, date, grant_purpose) %>% 
  filter(!pow_alliance %in% c("Science", "Creative"),
         !is.na(name)) %>% 
  rename(full_name = name) %>% 
  mutate(grant_type = "empowerment") %>% 
  select(-pow_alliance)

df_grants_rapidresponse <- df_grants_empower_raw %>% 
  slice_tail(n = 1) %>% 
  rename(full_name = timestamp) %>% 
  select(full_name) %>% 
  separate_rows(full_name, sep = "\\s*,\\s*") %>% 
  mutate(
    full_name = gsub("\\s*\\([^)]*\\)", "", full_name),
    grant_type = "rapid response"
  ) %>% 
  separate(full_name, into = c("first_name", "last_name"), sep = " ") %>% 
  mutate(
    full_name = paste(first_name, last_name),
    grant_purpose = NA,
    date = NA
  ) %>% 
  select(full_name, grant_type, grant_purpose, date)

df_grants_empower <- df_grants_empower %>% 
  rbind(df_grants_rapidresponse)




colnames(df_grants_empower_raw) <- c("name", "engagement_date", "engagement_desc")

clean_date <- function(date_value) {
  # Attempt to interpret as an Excel serial date
  if (!is.na(suppressWarnings(as.numeric(date_value)))) {
    return(as.Date(as.numeric(date_value), origin = "1899-12-30"))
  }
  # Attempt to interpret as a standard date string (e.g., MM/DD/YYYY)
  if (!is.na(as.Date(date_value, format = "%m/%d/%Y", tryFormats = c("%m/%d/%Y", "%m/%d/%y")))) {
    return(as.Date(date_value, format = "%m/%d/%Y"))
  }
  # If not a valid date, return NA
  return(NA)
}

# Apply the clean_date function to the engagement_date column
df_grants_empower_raw <- df_grants_empower_raw %>%
  rowwise() %>%
  mutate(engagement_date = clean_date(engagement_date)) %>%
  ungroup() %>% 
  mutate(
    engagement_date = as.character(engagement_date),
    engagement_date = case_when(
      name == "Elena Hight" & is.na(engagement_date) ~ "2024-10-18",
      name == "Fiona Max" & is.na(engagement_date) ~ "2024-11-12",
      name == "Jeff Colt" & is.na(engagement_date) ~ "2024-10-29",
      name == "Nate Bender" & is.na(engagement_date) ~ "2024-12-12",
      name == "Jared Shumate" & is.na(engagement_date) ~ "2024-10-24",
      name == "Tina Muir / Running for Real" & is.na(engagement_date) ~ "2024-09-4",
      TRUE ~ engagement_date
    ),
    engagement_date = as.Date(engagement_date, format = "%Y-%m-%d")
  )

# manually fix some especially problematic dates that are in the "0204" format
df_grants_empower_raw <- df_grants_empower_raw %>%
  mutate(
    engagement_date = case_when(
      engagement_date == as.Date("0204-05-02", format = "%Y-%m-%d") ~ as.Date("2024-05-02"),
      engagement_date == as.Date("0204-09-20", format = "%Y-%m-%d") ~ as.Date("2024-09-20"),
      TRUE ~ engagement_date # Retain other dates as is
    )
  )

# Several grants have multiple athletes associated w/ them. Here I split things up to ensure proper athlete ID matching later on.
# 1) check if the secondary person is in the alliance member database. if yes, then create another row to link the empowerment grant to them
# 2) if no, then just store the secondary name in a new "grant_empower_partners" col
df_grants_empower_raw <- df_grants_empower_raw %>%
  mutate(
    engagement_partners = ifelse(
      str_detect(name, "&|,"), 
      str_trim(str_remove(name, "^[^&,]*[&,]")), # Extract everything after "," or "&"
      NA
    ),
    name = str_trim(str_extract(name, "^[^&,]*")), # Keep the first name before "," or "&"
    name = case_when(
      name == "Scott" ~ "Scott Jurek", 
      name == "Tina Muir / Running for Real" ~ "Tina Muir", 
      TRUE ~ name 
    )
  )

# And finally, we need to acknowledge that some empowerment grants had multiple POW members involved. Let's create duplicates of those projects so we can credit each involved alliance member. 
# list of partner names we need to create dupe rows for:
rows_to_duplicate <- c("Scott Jurek", "Kait Boyle", "Micheli Oliver") 

df_grants_duplicates <- df_grants_empower_raw %>%
  filter(name %in% rows_to_duplicate & !is.na(engagement_partners)) %>% 
  mutate(name = engagement_partners,       
         engagement_partners = NA)

# some extra finagling because Jenny and Abby both worked with Scott on a grant and I need to do one more step of creating another duplicate project to split Jenny and Abby out.
extras <- df_grants_duplicates %>% 
  filter(name == "Jenny Jurek, Abby Levine") %>%
  separate_rows(name, sep = ",\\s*")

df_grants_duplicates <- df_grants_duplicates %>% 
  bind_rows(extras) %>% 
  filter(name != "Jenny Jurek, Abby Levine")

df_grants_empower <- bind_rows(df_grants_empower_raw, df_grants_duplicates)

df_grants_empower <- df_grants_empower %>%
  separate(name, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  mutate(last_name = ifelse(last_name == "Schumaker", "Schumacher", last_name),
         last_name = ifelse(last_name == "Levine", "Levene", last_name),
         last_name = ifelse(last_name == "Roden", "Rodden", last_name),
         first_name = ifelse(first_name == "Emilie", "Emile", first_name)
  ) %>%   
  left_join(
    df_alliance %>% select(first_name, last_name, salesforce_id), 
    by = c("first_name", "last_name")
  )

na_counts$df_grants_empower <- sum(is.na(df_grants_empower$salesforce_id))

df_grants_empower <- df_grants_empower %>%
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, first_name, last_name, everything())


################################################################################
### Newsletters - general engagements
################################################################################
df_newsletters_engagements_raw <- read.csv(here("data",  "2025_alliance_mobilization.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

clean_bad_dates <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  # Start output as all NA
  out <- rep(as.Date(NA), length(x))
  
  # 1. Excel serial dates "45831.0"
  excel_serial <- grepl("^[0-9]+(\\.0+)?$", x)
  if (any(excel_serial)) {
    nums <- as.numeric(gsub("\\.0+$", "", x[excel_serial]))
    out[excel_serial] <- as.Date(nums, origin = "1899-12-30")
  }
  
  # 2. Date ranges like "6/10-20/2025" or "7/20-22/25"
  range_idx <- grepl("/", x) & grepl("-", x)
  if (any(range_idx)) {
    rng <- x[range_idx]
    
    # Fix known broken forms like "7/14//2025"
    rng <- gsub("//", "/", rng)
    
    # Extract year (2-digit or 4-digit)
    year <- sub(".*([0-9]{2,4})$", "\\1", rng)
    year <- ifelse(nchar(year) == 2, paste0("20", year), year)
    
    # Extract first MM/DD
    mmdd <- sub("-.*", "", rng)
    
    # If first date is missing year, add it
    mmdd_full <- ifelse(grepl("[0-9]{4}$", mmdd),
                        mmdd,
                        paste0(mmdd, "/", year))
    
    parsed <- suppressWarnings(as.Date(mmdd_full, format = "%m/%d/%Y"))
    out[range_idx] <- parsed
  }
  
  # 3. Comma-separated multiple dates: "10/3, 10, 17, 24/2025"
  list_idx <- grepl(",", x)
  if (any(list_idx)) {
    vals <- x[list_idx]
    first_part <- sub(",.*", "", vals)        # take leftmost
    year <- sub(".*([0-9]{4}).*", "\\1", vals)
    composed <- paste0(first_part, "/", year) # first_date/year
    parsed <- suppressWarnings(as.Date(composed, format = "%m/%d/%Y"))
    out[list_idx] <- parsed
  }
  
  # 4. Fix simple broken formats like "7/14//2025"
  broken_idx <- grepl("//", x)
  if (any(broken_idx)) {
    fixed <- gsub("//", "/", x[broken_idx])
    parsed <- suppressWarnings(as.Date(fixed, format = "%m/%d/%Y"))
    out[broken_idx] <- parsed
  }
  
  # 5. Standard M/D/YYYY
  remain <- which(is.na(out) & grepl("/", x))
  if (length(remain) > 0) {
    parsed <- suppressWarnings(as.Date(x[remain], format = "%m/%d/%Y"))
    out[remain] <- ifelse(!is.na(parsed), parsed, out[remain])
  }
  
  # 6. Last fallback
  remain2 <- which(is.na(out))
  if (length(remain2) > 0) {
    parsed <- suppressWarnings(as.Date(x[remain2], tryFormats = c("%Y-%m-%d")))
    out[remain2] <- parsed
  }
  
  return(out)
}

df_newsletters_engagements <- df_newsletters_engagements_raw %>%
    mutate(
      engagement_date = clean_bad_dates(start_end_date)
    ) %>% 
  select(start_end_date, engagement_date, everything()) %>% 
  filter(status == "Done") %>% 
  # split on semicolons first
  mutate(athlete_s = str_split(athlete_s, ",")) %>%
  unnest(athlete_s) %>%
  mutate(athlete_s = str_trim(athlete_s),
         athlete_s = gsub("\\s*\\([^)]*\\)", "", athlete_s)
  )

##################### in progress

  
    #     last_name = ifelse(last_name == "Peterson", "Petersen", last_name),
  #   last_name = ifelse(last_name == "Schumaker", "Schumacher", last_name),
  #   last_name = ifelse(last_name == "DiGuilian", "DiGiulian", last_name),
  #   last_name = ifelse(last_name == "O'Keefe", "O'Keeffe", last_name),
  #   last_name = ifelse(last_name == "Lee Brooks", "Brooks", last_name),
  #   last_name = ifelse(last_name == "Siergrist", "Siegrist", last_name),
  #   last_name = ifelse(last_name == "Chavarraga", "Chavarriaga", last_name),
  #   first_name = ifelse(first_name == "Torey", "Torey Lee", first_name),
  #   first_name = ifelse(first_name == "Emilé", "Emile", first_name)
  left_join(
    df_alliance %>% select(first_name, last_name, salesforce_id), 
    by = c("first_name", "last_name")
  )

na_counts$df_newsletters_engagements <- sum(is.na(df_newsletters_engagements$salesforce_id))

df_newsletters_engagements <- df_newsletters_engagements %>%
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, first_name, last_name, everything())


## Alliance Appreciation Events
df_appreciation_events_raw <- read.csv(here("data",  "2024_alliance_appreciation_event.csv"), header = T, stringsAsFactors = FALSE)

df_appreciation_events <- df_appreciation_events_raw %>% 
  filter(CONFIRMED=="TRUE") %>% 
  select(NAME, LOCATION) %>%
  separate(NAME, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%  
  rename(engagement_location = LOCATION) %>% 
  mutate(
    engagement_location = case_when(
      engagement_location == "CA / RENO" ~ "reno_ca",
      engagement_location == "SALT LAKE CITY, UT" ~ "slc_ut",
      engagement_location == "FRONT RANGE, CO" ~ "frontrange_co",
      engagement_location == "SW COLORADO" ~ "sw_co",
      engagement_location == "MONTANA" ~ "unknown_mt",
      engagement_location == "BEND, OR" ~ "bend_or",
      engagement_location == "NEW ENGLAND" ~ "unknown_vt",
      engagement_location == "ROARING FORK, CO" ~ "roaringfork_co",
      TRUE ~ engagement_location  
    ),
    first_name = ifelse(first_name == "Torey", "Torey Lee", first_name),
    last_name = ifelse(last_name == "Merill", "Merrill", last_name),
    
    # Add engagement_date based on engagement_location
    engagement_date = case_when(
      engagement_location == "bend_or" ~ as.Date("2024-06-20"),
      engagement_location == "sw_co" ~ as.Date("2024-07-16"),
      engagement_location == "roaringfork_co" ~ as.Date("2024-07-26"),
      engagement_location == "unknown_mt" ~ as.Date("2024-07-24"),
      engagement_location == "frontrange_co" ~ as.Date("2024-07-23"),
      engagement_location == "reno_ca" ~ as.Date("2024-07-26"),
      engagement_location == "unknown_vt" ~ as.Date("2024-10-24"),
      # MAKING THIS UP FOR THE MOMENT - CONFIRM BEFORE GOING LIVE
      engagement_location == "slc_ut" ~ as.Date("2024-07-23"),
      TRUE ~ as.Date(NA)  
    )
  ) %>% 
  left_join(
    df_alliance %>% select(first_name, last_name, salesforce_id), 
    by = c("first_name", "last_name") 
  )

na_counts$df_appreciation_events <- sum(is.na(df_appreciation_events$salesforce_id))

df_appreciation_events <- df_appreciation_events %>%
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, first_name, last_name, everything())


################################################################################
### Newsletters - general engagements
################################################################################
df_appreciation_events_raw <- read.csv(here("data",  "2025_alliance_appreciation_event.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_appreciation_events <- df_appreciation_events_raw %>% 
  select(-event_event_name, -x2, -email) %>% 
  filter(!is.na(contact))

