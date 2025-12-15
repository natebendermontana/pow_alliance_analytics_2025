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
# Reference Tables
################################################################################
df_name_corrections <- tibble::tribble(
  ~field,       ~original,              ~corrected,
  
  # ---- FIRST NAME CORRECTIONS ----
  "first_name", "Torey lee",             "Torey Lee",
  "first_name", "Torey",                 "Torey Lee",
  "first_name", "Philip",                "Phil",
  "first_name", "Emilie",                "Emile",
  "first_name", "Emilé",                 "Emile",
  "first_name", "Samuel",                 "Sam",
  "first_name", "Ti",                 "Tiona",
  
  # ---- LAST NAME CORRECTIONS ----
  "last_name",  "Reyes-acosta",           "Reyes-Acosta",
  "last_name",  "Zynobia newman",         "Zynobia",
  "last_name",  "O'keeffe",               "O'Keeffe",
  "last_name",  "O'Keefe",                "O'Keeffe",
  "last_name",  "O'leary",                "O'Leary",
  "last_name",  "Digiulian",              "DiGiulian",
  "last_name",  "DiGuilian",              "DiGiulian",
  "last_name",  "Mccloy",                 "McCloy",
  "last_name",  "Rubio-macwright",        "Rubio MacWright",
  "last_name",  "Rubio macwright",        "Rubio MacWright",
  "last_name",  "Schumaker",              "Schumacher",
  "last_name",  "Levine",                 "Levene",
  "last_name",  "Roden",                  "Rodden",
  "last_name",  "Peterson",               "Petersen",
  "last_name",  "Lee Brooks",             "Brooks",
  "last_name",  "Siergrist",              "Siegrist",
  "last_name",  "Chavarraga",             "Chavarriaga",
  "last_name",  "Merill",                 "Merrill",
  "last_name",  "Hutchinson",             "Hutcheson",
  "last_name",  "Olseger",                "Osleger",
  "last_name", "Uehisa jurek",            "Jurek",
  
  # ---- Full NAME CORRECTIONS ----
  "full_name", "Lyndsy Dyer",  "Lynsey Dyer", 
  "full_name", "Nick Russel",   "Nick Russell", 
  "full_name", "Blake Keough",  "Blake Keogh",
  "full_name", "Blake Koegh",  "Blake Keogh",
  "full_name", "Carolina Rubio-MacWright",  "Carolina Rubio MacWright"
  
  
  
  
)

################################################################################
# Functions
################################################################################
func_apply_name_corrections <- function(df, corrections) {
  apply_field <- function(df, field_name) {
    if (!field_name %in% names(df)) return(df)
    corr <- corrections %>%
      dplyr::filter(field == field_name) %>%
      dplyr::select("original", "corrected") %>%
      dplyr::distinct()
    if (nrow(corr) == 0) return(df)
    df %>%
      dplyr::left_join(
        corr,
        by = stats::setNames("original", field_name)
      ) %>%
      dplyr::mutate(
        !!field_name := dplyr::coalesce(.data$corrected, .data[[field_name]])
      ) %>%
      dplyr::select(-corrected)
  }
  df %>%
    apply_field("full_name") %>%
    apply_field("first_name") %>%
    apply_field("last_name")
}

func_clean_date <- function(x) {
  
  x_orig <- x
  x <- as.character(x)
  x <- trimws(x)
  
  out <- rep(as.Date(NA), length(x))
  
  # ---- 0. Already valid ISO ----
  iso_idx <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(iso_idx)) {
    out[iso_idx] <- as.Date(x[iso_idx])
  }
  
  # ---- 1. Excel serial dates ----
  excel_idx <- is.na(out) & grepl("^[0-9]+(\\.0+)?$", x)
  if (any(excel_idx)) {
    nums <- as.numeric(sub("\\.0+$", "", x[excel_idx]))
    out[excel_idx] <- as.Date(nums, origin = "1899-12-30")
  }
  
  # ---- 2. Date ranges: take first date ----
  range_idx <- is.na(out) & grepl("/", x) & grepl("-", x)
  if (any(range_idx)) {
    rng <- gsub("//", "/", x[range_idx])
    
    year <- sub(".*([0-9]{2,4})$", "\\1", rng)
    year <- ifelse(nchar(year) == 2, paste0("20", year), year)
    
    mmdd <- sub("-.*", "", rng)
    mmdd_full <- ifelse(
      grepl("[0-9]{4}$", mmdd),
      mmdd,
      paste0(mmdd, "/", year)
    )
    
    parsed <- suppressWarnings(as.Date(mmdd_full, format = "%m/%d/%Y"))
    out[range_idx] <- parsed
  }
  
  # ---- 3. Comma-separated lists: take first date ----
  list_idx <- is.na(out) & grepl(",", x)
  if (any(list_idx)) {
    vals <- x[list_idx]
    first_part <- sub(",.*", "", vals)
    year <- sub(".*([0-9]{4}).*", "\\1", vals)
    composed <- paste0(first_part, "/", year)
    parsed <- suppressWarnings(as.Date(composed, format = "%m/%d/%Y"))
    out[list_idx] <- parsed
  }
  
  # ---- 4. Broken slashes ----
  broken_idx <- is.na(out) & grepl("//", x)
  if (any(broken_idx)) {
    fixed <- gsub("//", "/", x[broken_idx])
    parsed <- suppressWarnings(as.Date(fixed, format = "%m/%d/%Y"))
    out[broken_idx] <- parsed
  }
  
  # ---- 5. General parsing via lubridate ----
  remain <- is.na(out)
  if (any(remain)) {
    parsed <- lubridate::parse_date_time(
      x[remain],
      orders = c(
        "ymd", "y-m-d",
        "mdy", "m/d/y",
        "B d, Y", "B d Y",
        "B d",
        "m/d", "m/d,Y"
      ),
      tz = "UTC",
      quiet = TRUE
    )
    out[remain] <- as.Date(parsed)
  }
  
  out
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
  select(salesforce_id, first_name, last_name, alliance_type, alliance_group, facebook, instagram, twitter) %>% 
  func_apply_name_corrections(df_name_corrections) %>% 
  mutate(
    full_name = paste(first_name, last_name)) %>% 
  filter(full_name != "229.0 NA") %>% 
  select(salesforce_id, full_name, first_name, last_name, everything())

df_alliance_raw <- read.csv(here("data",  "2025_us_active_alliance_members.csv"), skip=9) %>% 
  select(-NA.) %>% 
  slice_head(n = -5) %>% 
  clean_names() 




################################################################################
### Total US Athlete Alliance Members
################################################################################

df_alliance <- df_alliance_raw %>% 
  mutate(
    alliance_group = trimws(alliance_group),
    alliance_group = na_if(alliance_group, "NA"),
    alliance_group = na_if(alliance_group, "<NA>"),
    alliance_group = na_if(alliance_group, "")
  ) %>%
  filter(!alliance_group %in% c("Creative", "Science")) %>%
  drop_na(alliance_group) %>% 
  mutate(investment_level = case_when(
    investment_level == "Higher" ~ "low",
    investment_level == "Further" ~ "med",
    investment_level == "Deeper" ~ "high",
    # almost all of the athletes with NA investment level are Prospects. 
    # Just going to assume these folks are low investment
    is.na(investment_level) ~ "low"),
  alliance_member_since = as.Date(alliance_member_since, format = "%m/%d/%Y")) %>% 
# clean up names
  func_apply_name_corrections(df_name_corrections) %>%
  # bring in salesforce ID for existing members
  left_join(df_athlete_ids %>% select(first_name, last_name, salesforce_id, instagram), by = c("first_name", "last_name")) %>% 
  # clean up instagram URLs for matching with the Dash-Hudson social data later
  mutate(
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
  # create new salesforce IDs for any rows that do not have them for whatever reason
  mutate(
    salesforce_id = if_else(
      is.na(salesforce_id) | salesforce_id == "",
      paste0(
        "x_",
        replicate(
          n(),
          paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
        )
      ),
      salesforce_id
    )
  ) %>% 
# finally, select just the cols we want
  select(salesforce_id, full_name, first_name, last_name, investment_level, alliance_group, alliance_status, alliance_member_since, 
         mailing_zip_postal_code, mailing_state_province_text_only, instagram_username)


x_2024athletes_not_in_2025 <- df_athlete_ids %>%
  anti_join(
    df_alliance,
    by = c("first_name", "last_name")
  ) %>%
  transmute(
    athlete_name = paste(first_name, last_name)
  )

x_2025athletes_missing_ids <- df_alliance %>%
  filter(is.na(salesforce_id) | salesforce_id == "") %>%
  transmute(
    alliance_name = paste(first_name, last_name)
  )

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
  mutate(
    date_clean = func_clean_date(lte_date_submission),
    type = type
  ) %>% 
  mutate(
    format = "op-ed") %>% 
  select(full_name, format, date_clean)
  
df_pr <- df_pr_raw %>%
  mutate(
    date_clean = str_replace(date, "Late 2025", "December 2025"),
    date_clean = func_clean_date(date_clean)
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

  full_join(df_opeds_raw, by = c("full_name", "date_clean", "format")) %>% 
  # bring in the salesforce_id
  left_join(
    df_alliance %>% select(full_name, first_name, last_name, salesforce_id), 
    by = c("full_name") 
  ) %>% 
  mutate(
    quarter = if_else(
      is.na(date_clean),
      NA_character_,
      paste0("Q", lubridate::quarter(date_clean))
    )
  ) %>% 
  # remove the non-athlete PR mentions
  filter(
    !is.na(salesforce_id) | full_name == "Multiple"
  ) %>% 
  select(salesforce_id, first_name, last_name, full_name, status, outlet, format, date_clean, quarter)




################################################################################
### AAA and Empowerment grants
################################################################################

#### Empower grants
df_grants_empower_raw <- read.csv(here("data",  "2025_empowerment_grants.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_grants_empower <- df_grants_empower_raw %>% 
  select(name, pow_alliance, date, grant_purpose) %>% 
  filter(!pow_alliance %in% c("Science", "Creative"),
         !is.na(name)) %>% 
  rename(full_name = name) %>% 
  mutate(grant_type = "empowerment") %>% 
  mutate(
    date_clean = func_clean_date(date)
    ) %>% 
  select(-pow_alliance, -date)

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
    date_clean = NA
  ) %>% 
  select(full_name, grant_type, grant_purpose, date_clean)

df_grants_all <- df_grants_empower %>% 
  rbind(df_grants_rapidresponse)


################################################################################
### Newsletters - general engagements
################################################################################
df_newsletters_engagements_raw <- read.csv(here("data",  "2025_alliance_mobilization.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_newsletters_engagements <- df_newsletters_engagements_raw %>%
    mutate(
      date_clean = func_clean_date(start_end_date)
    ) %>% 
  filter(status == "Done") %>% 
  # split the athlete names column on semicolons first
  mutate(athlete_s = str_split(athlete_s, ",")) %>%
  unnest(athlete_s) %>%
  mutate(full_name = str_trim(athlete_s),
         full_name = gsub("\\s*\\([^)]*\\)", "", full_name)
  ) %>% 
  # filter out Empowerment Grants - these are accounted for in df_grants_all
  filter(department != "Empowerment Grant") %>% 
  # fix several issues with several names
  mutate(
    full_name = case_when(
      full_name == "Abby Levene. In attendance: Jenny Jurek" ~ "Abby Levene",
      full_name == "and Dr. Ian Bolliger."                   ~ "Ian Bolliger",
      full_name == "Audience: Amie Engerbretson"                   ~ "Amie Engerbretson",
      full_name == "Dr. Gigi Owen. Alliance/Staff/Board: Phil Henderson"                   ~ "Phil Henderson",
      full_name == "moderated by Addie Thompson"                   ~ "Addie Thompson",
      full_name == "Panel Speakers: Jeremy Jones"                   ~ "Jeremy Jones",
      full_name == "speakers: Science: Alex Lee"                   ~ "Alex Lee",
      TRUE                                                   ~ full_name
    ),
    # strip out any cases of "Dr. "
      full_name = stringr::str_replace_all(full_name, "\\bDr\\.\\s*", "")
  ) %>% 
  # select only the cols we need
  select(date_clean, full_name, campaign_name, status, department) %>% 
  # clean up names and bring in IDs
  func_apply_name_corrections(df_name_corrections) %>% 
  left_join(df_athlete_ids %>% select(full_name, salesforce_id), by = "full_name") %>% 
  select(salesforce_id, full_name, date_clean, everything())





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

