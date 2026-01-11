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
df_name_corrections <- tribble(
  ~field, ~original,      ~corrected,
  "full_name", "Samuel Linnet",     "Sam Linnet",
  "full_name", "Lyndsy Dyer",  "Lynsey Dyer", 
  "full_name", "Nick Russel",   "Nick Russell", 
  "full_name", "Blake Keough",  "Blake Keogh",
  "full_name", "Blake Koegh",  "Blake Keogh",
  "full_name", "Carolina Rubio-MacWright",  "Carolina Rubio MacWright",
  "full_name", "Carolina Rubio macwright",  "Carolina Rubio MacWright",
  "full_name", "Ti Eversole",                 "Tiona Eversole",
  "full_name", "Fiona O'Keefe",               "Fiona O'Keeffe",
  "full_name", "Fiona O'keeffe",               "Fiona O'Keeffe",
  "full_name", "Gus Schumaker",               "Gus Schumacher",
  "full_name", "Abbey Levine",               "Abbey Levene",
  "full_name", "Beth Roden",               "Beth Rodden",
  "full_name", "Drew Peterson",               "Drew Petersen",
  "full_name", "Jonathan Siergrist",               "Jonathan Siegrist",
  "full_name", "Vanessa Chavarraga",               "Vanessa Chavarriaga",
  "full_name", "Thorn Merill",               "Thorn Merrill",
  "full_name", "Hilary Hutchinson",               "Hilary Hutcheson",
  "full_name", "Philip Henderson",               "Phil Henderson",
  "full_name", "Emilie Zynobia Newman",               "Emilé Zynobia Newman",
  "full_name", "Ti Eversole",               "Tiona Eversole",
  "full_name", "Sasha DiGuilian",               "Sasha Digiulian",
  "full_name", "Dillon Olseger",               "Dillon Osleger",
  "full_name", "Jenny Uehisa Jurek",               "Jenny Jurek",
  "full_name", "Paddy O'Leary",               "Patrick O'Leary",
  "full_name", "Mikey Schafer",               "Mikey Schaefer",
  "full_name", "Caroline Geich",               "Caroline Gleich",
  "full_name", "Sarah Strum",               "Sarah Sturm",
  "full_name", "Cody Cirollo",               "Cody Cirillo"
  
)




################################################################################
# Functions
################################################################################

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

standardize_name <- function(x) {
  x %>%
    # ensure character
    as.character() %>%
    # trim leading/trailing whitespace
    str_trim() %>%
    # remove titles at start
    str_remove(
      regex(
        "^(mr|mrs|ms|miss|mx|dr)\\.?\\s+",
        ignore_case = TRUE
      )
    ) %>%
    # replace non-letter characters (but KEEP accented letters)
    str_replace_all("[^\\p{L}'\\s]", " ") %>%
    # collapse multiple spaces
    str_squish() %>%
    # lowercase everything
    str_to_lower() %>%
    # capitalize first letter of each word
    str_replace_all(
      "\\b[a-z]",
      ~ str_to_upper(.x)
    )
}

split_first_last <- function(x) {
  tibble(
    first_name = str_trim(str_remove(x, "\\s+\\S+$")),
    last_name  = str_extract(x, "\\S+$")
  )
}


apply_manual_name_corrections <- function(x, field, corrections_df) {
  lookup <- corrections_df %>%
    dplyr::filter(.data$field == field) %>%
    dplyr::select(original, corrected)
  
  out <- x
  
  idx <- match(out, lookup$original)
  
  out[!is.na(idx)] <- lookup$corrected[idx[!is.na(idx)]]
  
  out
}


################################################################################
# Clean up filenames and load data
################################################################################
raw_data_folder   <- here("data", "raw")
clean_data_folder <- here("data", "clean")

new_names <- c(
  # 2024 athlete data already in clean folder
  
  # 2025 athletes
  "All Athlete, Creative, Science Alliances-2025-11-18-10-42-41.xlsx" = "2025_us_active_alliance_members.csv",
  
  # Grants - Empowerment
  "POW Empowerment Grant  (Responses).xlsx" = "2025_empowerment_grants.csv",
  
  # Grants - AAA
  # tbd
  
  # Social media (Instagram) posts
  "SOCIAL DATA FROM SPROUT Relationships-Overview-Post-Count-2025-01-01-2025-11-19-1763493637.csv" = "2025_instagram_posts.csv"
  
  # PR
  "PR Placements.xlsx" = "2025_pr_placements.csv",
  
  # POW gatherings
  "2025 Alliance Gathering_ Attendence Data.xlsx" = "2025_alliance_appreciation_event.csv",
  
  # Newsletters
  "2025 Alliance Engagement Tracking.xlsx" = "2025_alliance_newsletters.csv",
  
  # Petitions
  "2025 POW Quorum Engagement-2025-11-24-07-25-38.xlsx" = "2025_petitions.csv",
  
  # Public Lands Campaigns
  "2025 Reconciliation & Public Lands Campaigns - Alliance Contacts .xlsx" = "2025_reconciliation_pl_campaigns.csv",
  
  # PR - Op Eds
  "SSOT - Reconciliation LTE_OPed Tracker.xlsx" = "2025_pr_opeds.csv"
  
)

# list Excel files in raw folder
excel_files <- list.files(
  raw_data_folder,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# convert Excel → CSV (rename only on output)
for (file in excel_files) {
  raw_name <- basename(file)
  csv_name <- if (raw_name %in% names(new_names)) {
    new_names[[raw_name]]
  } else {
    sub("\\.xlsx$", ".csv", raw_name)
  }
  csv_path <- file.path(clean_data_folder, csv_name)
  data <- readxl::read_excel(file)
  write.csv(data, csv_path, row.names = FALSE)
  cat("Created/updated CSV:", csv_path, "\n")
}

# return vector of CSVs now in clean folder
csv_files <- list.files(
  clean_data_folder,
  pattern = "\\.csv$",
  full.names = TRUE
)

csv_files


################################################################################
### All 2024 athlete IDs (df_2024_athlete_ids) and 2025 Athlete list (df_2025_alliance_raw)
################################################################################

df_2024_athlete_ids <-  read.csv(here("data", "clean",  "2024_us_active_alliance_members.csv")) %>% 
  clean_names() %>% 
  select(salesforce_id, first_name, last_name, alliance_type, alliance_group, facebook, instagram, twitter) %>% 
  mutate(
    full_name = paste(first_name, last_name)) %>% 
  filter(full_name != "229.0 NA") %>% 
  mutate(full_name = standardize_name(full_name),
         full_name = apply_manual_name_corrections(full_name, 
                                                   field = "full_name",
                                                   corrections_df = df_name_corrections),
         first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
         last_name  = str_extract(full_name, "\\S+$")) %>% 
  select(salesforce_id, full_name, first_name, last_name, everything())


df_2025_alliance_raw <- read.csv(here("data", "clean",  "2025_us_active_alliance_members.csv"), skip=9) %>% 
  clean_names()






################################################################################
### Total US Athlete Alliance Members
################################################################################

df_2025_alliance <- df_2025_alliance_raw %>% 
  # remove unecessary column
  select(-na) %>% 
  # There are two instances of Jenny Jurek. Keep only the first 
  group_by(last_name) %>%
  filter(
    last_name != "Uehisa jurek" | row_number() == 1
  ) %>%
  ungroup() %>% 
  # remove last five rows which are not valid data
  slice_head(n = -5) %>% 
  # standardize names
  mutate(
      full_name = paste(first_name, last_name),
      full_name = standardize_name(full_name),
      full_name = apply_manual_name_corrections(full_name, 
                                                field = "full_name",
                                                corrections_df = df_name_corrections),
      first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
      last_name  = str_extract(full_name, "\\S+$"),
    # clean up alliance groups
    alliance_group = trimws(alliance_group),
    alliance_group = na_if(alliance_group, "NA"),
    alliance_group = na_if(alliance_group, "<NA>"),
    alliance_group = na_if(alliance_group, "")
  ) %>%
  filter(!alliance_group %in% c("Creative", "Science"),
         !first_name == "Tk") %>%
  drop_na(alliance_group) %>% 
  mutate(investment_level = case_when(
    investment_level == "Higher" ~ "low",
    investment_level == "Further" ~ "med",
    investment_level == "Deeper" ~ "high",
    # almost all of the athletes with NA investment level are Prospects. 
    # Just going to assume these folks are low investment
    is.na(investment_level) ~ "low"),
  alliance_member_since = as.Date(alliance_member_since, format = "%m/%d/%Y")) %>% 
  # bring in salesforce ID for existing members
  left_join(df_2024_athlete_ids %>% select(first_name, last_name, salesforce_id, instagram), by = c("first_name", "last_name")) %>% 
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
  str_trim(),  # Trim any extra whitespace
  # clean up a couple rows manually
    instagram_username = case_when(
      instagram_username == "captain calhoun" ~ "captaincalhoun",  # Adjust "captain calhoun"
      instagram_username == "jeaneecranemauzy@gmail.com" ~ "jeaneecranemauzy",  # Replace email with NA
      TRUE ~ instagram_username),  # Keep all other values unchanged
  # manually update some missing Instagram usernames
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
  # filter to unique instances of salesforce_id
  distinct(salesforce_id, .keep_all = TRUE) %>% 
# finally, select just the cols we want
  select(salesforce_id, full_name, first_name, last_name, investment_level, alliance_group, alliance_status, alliance_member_since, 
         mailing_zip_postal_code, mailing_state_province_text_only, instagram_username)

x_2024athletes_not_in_2025 <- df_2024_athlete_ids %>%
  anti_join(
    df_2025_alliance,
    by = c("first_name", "last_name")
  ) %>%
  transmute(
    athlete_name = paste(first_name, last_name)
  )

x_2025athletes_not_in_2024 <- df_2025_alliance %>%
  filter(str_starts(salesforce_id, "x_")) 

# Total US Alliance
tot_us_athletes <- df_2025_alliance %>% 
  summarize(unique_id_count = n_distinct(salesforce_id)) %>% 
  pull(unique_id_count)
print(paste0("There are ", tot_us_athletes, " US Alliance members"))


################################################################################
### Reconciliation & Public Lands Campaigns
################################################################################
# grab only the PPL stuff
df_pl_campaigns_raw <- read.csv(here("data", "clean",  "2025_reconciliation_pl_campaigns.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_pl_campaigns <- df_pl_campaigns_raw %>% 
  slice_head(n=98) %>% 
  select(first_name, last_name, social_campaign) %>% 
  filter(!is.na(first_name)) %>% 
  # break out moonrise collective and separate the names
  mutate(
    full_name = case_when(
      first_name == "Moonrise Creative" ~
        "Sierra Schlag, Katie Cooney, Sara Beam Robbins, Iz La Motte",
      
      is.na(last_name) ~
        first_name,
      
      TRUE ~
        paste(first_name, last_name)
    )
  ) %>%
  separate_rows(full_name, sep = ",\\s*") %>% 
  select(-first_name, -last_name) %>% 
  mutate(
    full_name = standardize_name(full_name),
    full_name = apply_manual_name_corrections(full_name, 
                                              field = "full_name",
                                              corrections_df = df_name_corrections),
    first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
    last_name  = str_extract(full_name, "\\S+$")) %>% 
    left_join(df_2025_alliance %>% select(full_name, salesforce_id), by = "full_name") %>% 
  # treat those with NA salesforce_id as non-athletes, to be counted and removed later
  mutate(
    salesforce_id = if_else(
      is.na(salesforce_id) | str_trim(salesforce_id) == "",
      "non-athlete",
      salesforce_id
    ),
    engagement_type = "pl_campaign"
  ) %>% 
  filter(!is.na(salesforce_id)) %>% 
  select(salesforce_id, full_name, first_name, last_name, social_campaign, engagement_type)



# keep only OPEDS for Lindsy, Nick, Brody. All the rest are already represented in df_pr as format=="op-ed"
df_opeds_other <- df_pl_campaigns_raw %>% 
  filter(row_number() >= 120, 
         !is.na(first_name)) %>% 
  rename(full_name = first_name) %>%
  mutate(
    full_name = standardize_name(full_name),
    full_name = apply_manual_name_corrections(full_name, 
                                              field = "full_name",
                                              corrections_df = df_name_corrections),
    format = "op-ed") %>% 
  filter(full_name %in% c("Brody Leven", "Nick Russell", "Lynsey Dyer")) %>% 
  select(full_name, format)


################################################################################
### PR Placements
################################################################################

df_pr_raw <- read.csv(here("data", "clean",  "2025_pr_placements.csv"), skip = 2) %>% 
  clean_names() 

df_opeds_raw <- read.csv(here("data", "clean", "2025_pr_opeds.csv")) %>% 
  clean_names() %>% 
  filter(alliance=="Athlete Alliance") %>% 
  rename(full_name = name) %>%
  mutate(
    full_name = standardize_name(full_name),
    full_name = apply_manual_name_corrections(full_name, 
                                              field = "full_name",
                                              corrections_df = df_name_corrections),
    first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
    last_name  = str_extract(full_name, "\\S+$"),
    date_clean = func_clean_date(lte_date_submission),
    type = type,
    format = "op-ed") %>% 
  select(full_name, format, date_clean)
  
df_pr <- df_pr_raw %>%
  select(-na) %>%
  drop_na(date) %>% 
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

  mutate(
    full_name = standardize_name(full_name),
    full_name = apply_manual_name_corrections(full_name, 
                                              field = "full_name",
                                              corrections_df = df_name_corrections)) %>% 
  
  full_join(df_opeds_raw, by = c("full_name", "date_clean", "format")) %>% 
  bind_rows(df_opeds_other) %>% 
  # bring in the salesforce_id
  left_join(
    df_2025_alliance %>% select(full_name, first_name, last_name, salesforce_id), 
    by = c("full_name") 
  ) %>% 
  select(-first_name, -last_name) %>% 
  mutate(
    first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
    last_name  = str_extract(full_name, "\\S+$"),
    engagement_type = "pr",
    quarter = if_else(
      is.na(date_clean),
      NA_character_,
      paste0("Q", lubridate::quarter(date_clean))
    ),
  # treat those with NA salesforce_id as non-athletes, to be counted and removed later
    salesforce_id = if_else(
      is.na(salesforce_id) | str_trim(salesforce_id) == "",
      "non-athlete",
      salesforce_id
    )
  ) %>% 
  # remove the non-athlete PR mentions
#  filter(
    # DO WE WANT TO REMOVE THESE "MULTIPLE" RECORDS?
#    !full_name == "Multiple"
#  ) %>% 
  select(salesforce_id, first_name, last_name, full_name, status, outlet, format, date_clean, quarter, engagement_type)




################################################################################
### AAA and Empowerment grants
################################################################################

#### AAA grants tbd

#### Empower grants
df_grants_empower_raw <- read.csv(here("data", "clean",  "2025_empowerment_grants.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_grants_empower <- df_grants_empower_raw %>% 
  select(name, pow_alliance, date, grant_purpose) %>% 
  filter(!pow_alliance %in% c("Science", "Creative"),
         !is.na(name)) %>% 
  rename(full_name = name) %>% 
  mutate(
    full_name = standardize_name(full_name),
    full_name = apply_manual_name_corrections(full_name, 
                                              field = "full_name",
                                              corrections_df = df_name_corrections),
    first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
    last_name  = str_extract(full_name, "\\S+$"),
    engagement_type = "grant_empowerment",
    date_clean = func_clean_date(date)
    ) %>% 
  # bring in the salesforce_id
  left_join(
    df_2025_alliance %>% select(full_name, salesforce_id), 
    by = c("full_name") 
  ) %>% 
  mutate(
  # treat those with NA salesforce_id as non-athletes, to be counted and removed later
  salesforce_id = if_else(
    is.na(salesforce_id) | str_trim(salesforce_id) == "",
    "non-athlete",
    salesforce_id
    )) %>% 
  select(salesforce_id, full_name, first_name, last_name, grant_purpose, engagement_type, date_clean)

df_grants_rapidresponse <- df_grants_empower_raw %>% 
  slice_tail(n = 1) %>% 
  rename(full_name = timestamp) %>% 
  select(full_name) %>% 
  separate_rows(full_name, sep = "\\s*,\\s*") %>% 
  mutate(
    full_name = gsub("\\s*\\([^)]*\\)", "", full_name),
    engagement_type = "grant_rapid_response",
      full_name = standardize_name(full_name),
      full_name = apply_manual_name_corrections(full_name, 
                                                field = "full_name",
                                                corrections_df = df_name_corrections),
      first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
      last_name  = str_extract(full_name, "\\S+$"),
    grant_purpose = NA,
    date_clean = NA
  ) %>% 
  # bring in the salesforce_id
  left_join(
    df_2025_alliance %>% select(full_name, salesforce_id), 
    by = c("full_name") 
  ) %>% 
  mutate(
    # treat those with NA salesforce_id as non-athletes, to be counted and removed later
    salesforce_id = if_else(
      is.na(salesforce_id) | str_trim(salesforce_id) == "",
      "non-athlete",
      salesforce_id
    )) %>% 
  select(salesforce_id, full_name, first_name, last_name, engagement_type, grant_purpose, date_clean)

df_grants_all <- df_grants_empower %>% 
  rbind(df_grants_rapidresponse)


################################################################################
### Newsletters - general engagements
################################################################################
df_newsletters_engagements_raw <- read.csv(here("data", "clean",  "2025_alliance_newsletters.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_newsletters_engagements <- df_newsletters_engagements_raw %>%
    mutate(
      date_clean = func_clean_date(start_end_date)
    ) %>% 
  ### DO WE WANT TO FILTER TO ONLY "DONE" STATUS? 
  filter(status == "Done") %>% 
  # split the athlete names column on semicolons first
  mutate(athlete_s = str_split(athlete_s, ",")) %>%
  unnest(athlete_s) %>%
  # fix several issues with several names
  mutate(
    full_name = str_trim(athlete_s),
    full_name = gsub("\\s*\\([^)]*\\)", "", full_name),
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
  mutate(engagement_type = "newsletters",
         full_name = standardize_name(full_name),
         full_name = apply_manual_name_corrections(full_name, 
                                                   field = "full_name",
                                                   corrections_df = df_name_corrections),
         first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
         last_name  = str_extract(full_name, "\\S+$")
  ) %>% 
  # filter out Empowerment Grants - these are accounted for in df_grants_all
  filter(department != "Empowerment Grant") %>% 

  left_join(df_2025_alliance %>% select(full_name, salesforce_id), by = "full_name") %>% 
  mutate(
    # treat those with NA salesforce_id as non-athletes, to be counted and removed later
    salesforce_id = if_else(
      is.na(salesforce_id) | str_trim(salesforce_id) == "",
      "non-athlete",
      salesforce_id
    )) %>% 
  select(salesforce_id, full_name, first_name, last_name, engagement_type, date_clean, full_name, campaign_name, status, department)

df_newsletters_engagements %>% filter(salesforce_id=="non-athlete") %>% View()



################################################################################
### Alliance Appreciation Events
################################################################################
df_appreciation_events_raw <- read.csv(here("data", "clean",  "2025_alliance_appreciation_event.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

df_appreciation_events <- df_appreciation_events_raw %>% 
  mutate(
    event_name = stringr::str_replace(event_name, "^.*?:\\s*", "")
  ) %>% 
  rename(full_name = contact) %>% 
  filter(!is.na(full_name)) %>% 
  mutate(engagement_type = "appreciation_events",
         full_name = standardize_name(full_name),
         full_name = apply_manual_name_corrections(full_name, 
                                                   field = "full_name",
                                                   corrections_df = df_name_corrections),
         first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
         last_name  = str_extract(full_name, "\\S+$")
  ) %>% 
  left_join(
    df_2025_alliance %>% select(full_name, salesforce_id), 
    by = c("full_name") 
  ) %>% 
  mutate(
    # treat those with NA salesforce_id as non-athletes, to be counted and removed later
    salesforce_id = if_else(
      is.na(salesforce_id) | str_trim(salesforce_id) == "",
      "non-athlete",
      salesforce_id
    )) %>% 
  select(salesforce_id, full_name, first_name, last_name, engagement_type, event_name)


################################################################################
### Social Media
################################################################################
df_socials_raw <- read.csv(here("data", "clean",  "2025_instagram_posts.csv"), header = T, stringsAsFactors = FALSE) %>% 
  clean_names()

cols_to_rename <- c("username",
                    "followers", "followers_gained", "number_of_posts_ft_pow",
                    "avg_total_engagements", "avg_total_engagements_ft_pow",
                    "avg_est_reach", "avg_est_reach_ft_pow",
                    "avg_eng_rate_of_all_posts", "avg_eng_rate_ft_pow",
                    "avg_effectiveness", "avg_effectiveness_ft_pow",
                    "avg_emv", "total_emv"
)

df_socials <- df_socials_raw %>% 
  select(-notes, -date_added) %>% 
  # Clean and rename column names
  rename_with(~ .x %>%
                tolower() %>%                              
                str_replace_all("\\.", "_") %>%            
                str_replace_all("(?i)you", "pow") %>%    
                str_replace_all("__", "_") %>% 
                str_replace_all("_$", "")) %>% #remove trailing underscores   
  # second rename_with call separated from the first for readability. 
  # This one prefixes "insta_" onto all the relevant cols so later analysis is easier. 
  rename_with(
    .fn = ~ paste0("insta_", .),
    .cols = all_of(cols_to_rename)
  ) %>% 
  # Clean `relationship_tags` column and filter out non-athletes
  mutate(relationship_tags = str_replace_all(relationship_tags, '^"|"$', ""),
         insta_username = case_when(
           insta_username == "__alyssagonzalez" ~ "_alyssagonzalez",
             TRUE                      ~ insta_username
           )) %>% 
  filter(relationship_tags=="Athlete") %>% 
left_join(
  # grabbing alliance_group for QA purposes
  df_2025_alliance %>% select(salesforce_id, first_name, last_name, instagram_username),
  by = c("insta_username" = "instagram_username")
) %>% 
  mutate(full_name = paste(first_name, last_name)) %>% 
  # remove the existing first and last names; these will be re-created after standardization
  select(-first_name, -last_name) %>% 
  mutate(engagement_type = "social_posts",
         full_name = standardize_name(full_name),
         full_name = apply_manual_name_corrections(full_name, 
                                                   field = "full_name",
                                                   corrections_df = df_name_corrections),
         first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
         last_name  = str_extract(full_name, "\\S+$")
  ) %>% 
  select(-relationship_tags) %>% 
  select(salesforce_id, full_name, first_name, last_name, insta_username, everything())





# STORIES - tbd. Unclear what these are. 


################################################################################
### Petitions
################################################################################
df_petitions_raw <- read.csv(here("data", "clean",  "2025_petitions.csv"), header = T, stringsAsFactors = FALSE, skip = 11) %>% 
  clean_names()

df_petitions <- df_petitions_raw %>%
  select(-na) %>%
  # drop all rows below where full_name=="Total", which is the Salesforce total row
  filter(
    row_number() < which(full_name == "Total")[1]
  ) %>% 
  # strip titles out of names
  mutate(engagement_type = "petitions",
         full_name = standardize_name(full_name),
         full_name = apply_manual_name_corrections(full_name, 
                                                   field = "full_name",
                                                   corrections_df = df_name_corrections)
         ) %>% 
  # 1. Carry names downward within grouped blocks
  fill(full_name, .direction = "down") %>%
  # 2. Remove subtotal rows
  filter(full_name != "Subtotal") %>%
  # 3. Keep only real campaign rows
  filter(!is.na(campaign_name)) %>%
  # 5. Enforce one row per participation
  distinct() %>% 
  left_join(df_2025_alliance %>% select(full_name, salesforce_id), by = "full_name") %>% 
  mutate(
    engagement_date = mdy(member_first_associated_date),
    # Bring in first and last name now that full_name has been cleaned and fill() filled down to each row for ppl 
    # who have multiple engagements
    first_name = str_trim(str_remove(full_name, "\\s+\\S+$")),
    last_name  = str_extract(full_name, "\\S+$"),
    # treat those with NA salesforce_id as non-athletes, to be counted and removed later
    salesforce_id = if_else(
      is.na(salesforce_id) | str_trim(salesforce_id) == "",
      "non-athlete",
      salesforce_id
    )) %>% 
  select(salesforce_id, full_name, first_name, last_name, campaign_name, engagement_type, engagement_date)



################################################################################
### Combine dataframes
################################################################################


# anti_join(dupes_rm_newsletter_engagements, by = c("first_name", "last_name", "engagement_type", "engagement_date", "engagement_desc")) %>% 
  

