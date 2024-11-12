# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Library ----
library(openxlsx)
library(readr)
library(readxl)
library(tidyverse)
library(kableExtra)
library(scales)
library(janitor)
library(gridExtra)
library(hms)
library(lubridate)

# Functions ----

# Data Loading ----
path.data <- "../Data/Raw/"
files <- list.files(path.data)

region.path <- paste0(path.data, files[grepl("region", files)])
organisation.path <- paste0(path.data, files[grepl("organisation", files)])
currency.path <- paste0(path.data, files[grepl("currency", files)])
lang.path <- paste0(path.data, files[grepl("lang_list", files)])

lang_list <- read_csv(lang.path) %>%
  rename("id_language" = "id") %>%
  select(id_language, code)

country_list <- read_excel("../../DB Input/Contries_iso3.xls") %>% rename(country = Country)

region <- read_csv(region.path) %>%
  rename("id_region" = "id") %>%
  mutate(
    region = ifelse(region == "Single-country region", country, region),
    region = ifelse(region %in% c("Cyprus", "Greece"), "Greece & Cyprus", region),
    region = ifelse(region %in% c("United Kingdom", "Ireland"), "United Kingdom & Ireland", region),
    region = ifelse(region == c("Mexico, Central America & Caribean"), "Mexico, Central America & Caribbean", region), 
    region = ifelse(region == c("Arab countries"), "Arab Countries", region)
    
  ) %>% 
  left_join(country_list)
region$Code[region$country == "Netherlands Antilles"] <- "ANT"

organisation <- read_csv(organisation.path) %>%
  rename("id_organisation" = "id")

currency <- read_csv(currency.path) %>%
  rename("id_currency" = "id") %>%
  select(id_currency, code) %>%
  rename("currency" = "code")

path.aiic <- "../Data/AIIC_db/"
files.aiic <- list.files(path.aiic)
## interpreter ----
path <- paste0(path.data, files[grepl("interpreter", files)])
interpreter <- read_csv(path) %>%
  select(id, max_section) %>%
  rename("id_interpreter" = "id") %>%
  mutate(
    max_section = ifelse(max_section == -1 | is.na(max_section) | max_section == "NULL", 0, max_section),
    finish = ifelse(max_section == 12, "Yes", "No"),
    max_section = str_pad(max_section, 2, "left", 0)
  )

interpreter_f <- interpreter %>%
  filter(max_section == 12)

## basic ----
path <- paste0(path.data, files[grepl("basic", files)])
basic <- read_csv(path) %>%
  mutate_all(.funs = ~ ifelse(. == "NULL", NA, .)) %>%
  select(-id) %>%
  mutate(
    age_num = ifelse(age == is.na(age), NA, 2024 - age),
    age_num = ifelse(age_num == 125, NA, age_num),
    sex = plyr::mapvalues(sex, 1:4, c("Male", "Female", "None / Other", "Would rather not say")),
    sex_adjusted = ifelse(!(sex %in% c("Male", "Female")), "Not reported", sex ),
    
    aiic_status = plyr::mapvalues(aiic_status, 1:4, c("Active Member", "Associate Member", "Precandidate", "Candidate")),
    employment_status = plyr::mapvalues(employment_status, 0:4, c("Unreported", "Freelance", "Partly Freelance", "Staff", "Partly Staff")),
    member_num = ifelse(member_since == is.na(member_since), NA, 2024 - member_since),

    formal_education = plyr::mapvalues(formal_education, 1:4, c("Non-university", "BA or equivalent", "Master’s", "PhD")),
    type_education = plyr::mapvalues(type_education, 1:4, c("Non-university", "BA or equivalent", "Master’s", "PhD")),
    conference_education = plyr::mapvalues(conference_education, 0:1, c("No", "Yes")),
    freelance_affiliation = plyr::mapvalues(conference_education, 0:1, c("No", "Yes")),
    id_organisation = as.numeric(id_organisation)
  ) %>%
  left_join(region) %>%
  relocate(c("region", "country", "Code"), .after = id_region) %>%
  left_join(organisation) %>%
  relocate(c("organisation", ), .after = id_organisation) %>%
  mutate_all(.funs = ~ ifelse(. == 0, NA, .)) %>%
  select(-id_region, -id_organisation) %>%
  mutate(age_agg = cut(age_num, c(seq(19, 80, 10), 150), c(paste0(seq(20, 70, 10), " - ", seq(29, 79, 10)), "> 79")),
         member_agg = cut(member_num, c(seq(-1, 50, 10), 200), c(paste0(seq(0, 40, 10), " - ", seq(9, 49, 10)), "> 49")))

## Working Languages----
### lang_cat ----
path <- paste0(path.data, files[grepl("lang_cat", files)])
lang_cat <- read_csv(path) %>%
  left_join(lang_list) %>%
  mutate(category = plyr::mapvalues(category, 1:3, c("A", "B", "C"))) %>%
  select(-id, -id_language) %>%
  rename("combi_lang" = "code") %>%
  relocate("category", .after = combi_lang)

### official_pair ----
path <- paste0(path.data, files[grepl("official_pair", files)])
official_pair <- read_csv(path) %>%
  select(-id) %>%
  rename("id_language" = "from_lang") %>%
  left_join(lang_list) %>%
  rename("official_from" = "code") %>%
  select(-id_language) %>%
  rename("id_language" = "to_lang") %>%
  left_join(lang_list) %>%
  rename("official_to" = "code") %>%
  select(-id_language) %>%
  mutate(pair_use = plyr::mapvalues(pair_use, 1:4, c("Always", "Regularly", "Infrequently", "Never"))) %>%
  relocate(pair_use, .after = official_to)

### lang_pair ----
path <- paste0(path.data, files[grepl("lang_pair", files)])
lang_pair <- read_csv(path) %>%
  select(-id) %>%
  rename("id_language" = "from_lang") %>%
  left_join(lang_list) %>%
  rename("lang_from" = "code") %>%
  select(-id_language) %>%
  rename("id_language" = "to_lang") %>%
  left_join(lang_list) %>%
  rename("lang_to" = "code") %>%
  select(-id_language) %>%
  mutate(pair_use = plyr::mapvalues(pair_use, 1:4, c("Always", "Regularly", "Infrequently", "Never"))) %>%
  relocate(pair_use, .after = lang_to)

### lang_study ----
path <- paste0(path.data, files[grepl("lang_study", files)])
lang_study <- read_csv(path) %>%
  select(-id) %>%
  rename("id_language" = "from_lang") %>%
  left_join(lang_list) %>%
  rename("study_lang" = "code") %>%
  select(-id_language)

## market ----
path <- paste0(path.data, files[grepl("market", files)])
market <- read_csv(path) %>%
  select(-id) %>%
  mutate(workload = plyr::mapvalues(workload, 1:5, c("Much more work", "More work", "Stable", "Less work", "Much less work"))) %>%
  mutate_all(.funs = ~ ifelse(is.na(.), 0, .))

### country ----
path <- paste0(path.data, files[grepl("country", files)])
country <- read_csv(path) %>% 
  group_by(id_interpreter) %>% 
  arrange(id_interpreter, id) %>% 
  mutate(rank = row_number()) %>% 
  select(-id) %>% 
  rename(id_region = country_list) %>% 
  left_join(region) %>% 
  select(-id_region) 

## days_mode ----
path <- paste0(path.data, files[grepl("days_mode", files)])
days_mode <- read_csv(path) %>%
  select(-id) %>%
  mutate_all(.funs = ~ ifelse(is.na(.), 0, .))

## days_condition ----
path <- paste0(path.data, files[grepl("days_condition", files)])
days_condition <- read_csv(path) %>%
  select(-id) %>%
  mutate_all(.funs = ~ ifelse(is.na(.), 0, .))

## length_session ----
path <- paste0(path.data, files[grepl("length_session", files)])
length_session <- read_csv(path) %>%
  select(-id) %>%
  mutate_at(.vars = c(names(.)[grepl("way_work", names(.))]), .funs = ~ plyr::mapvalues(., 1:4, c("0-25%", "25-50%", "50-75%", "75-100%"))) %>%
  mutate_at(.vars = c(names(.)[grepl("meeting|session", names(.))]), .funs = ~ as_hms(ifelse(. == 00:00:00, NA, .)))

## remuneration ----
path <- paste0(path.data, files[grepl("remuneration", files)])
remuneration <- read_csv(path) %>%
  select(-id) %>%
  left_join(currency) %>%
  relocate(currency, .after = id_currency) %>%
  select(-id_currency)

## other_income ----
path <- paste0(path.data, files[grepl("other_income", files)])
other_income <- read_csv(path) %>%
  select(-id) %>%
  mutate(
    other_gainful_employment_percentage = plyr::mapvalues(other_gainful_employment_percentage, 1:4, c("Less than 10%", "10-30%", "31-50%", "Over 50%")),
    remuneration_copyright = plyr::mapvalues(remuneration_copyright, 0:2, c("No", "Yes, a fixed amount per day", "Yes, a percentage of the daily fee")),
    main_retirement_savings_percentage = plyr::mapvalues(main_retirement_savings_percentage, 0:4, c("Nothing", "1-10%", "11-20%", "21-30%", "Over 30%"))
  ) %>%
  left_join(currency) %>%
  relocate(currency, .after = remuneration_copyright_total) %>%
  select(-id_currency)

## quality_life ----
path <- paste0(path.data, files[grepl("quality_life", files)])
quality_life <- read_csv(path) %>%
  select(-id)

names(quality_life)[!(grepl("id_interpreter|additional_stress_factors|overall_job_satisfaction|general_comments", names(quality_life)))]
frq <- c("Always", "Regularly", "Infrequently", "Never")

## health ----
path <- paste0(path.data, files[grepl("health", files)])
health <- read_csv(path) %>%
  select(-id) %>%
  mutate(prevention = ifelse(is.na(prevention), 0, prevention))

## other_condition ----
path <- paste0(path.data, files[grepl("other_condition", files)])
other_condition <- read_csv(path) %>%
  select(-id) %>%
  left_join(currency) %>%
  relocate(currency, .after = CPE_amount) %>%
  select(-id_currency)

## final_quest ----
path <- paste0(path.data, files[grepl("final_quest", files)])
final_quest <- read_csv(path) %>%
  select(-id)

# Final Data ----
## One row data ----
data <- interpreter %>%
  left_join(basic) %>%
  left_join(market) %>%
  left_join(days_mode) %>%
  left_join(days_condition) %>%
  left_join(length_session) %>%
  left_join(other_income) %>%
  left_join(quality_life) %>%
  left_join(health) %>%
  left_join(other_condition) %>%
  left_join(final_quest)

write.xlsx(data, "../Data/AIIC2301_Data.xlsx")

## Multiple row data ----
### lang_cat ----
data <- lang_cat
write.xlsx(data, "../Data/AIIC2301_lang_cat_Data.xlsx")

### official_pair ----
data <- official_pair
write.xlsx(data, "../Data/AIIC2301_official_pair_Data.xlsx")

### lang_pair ----
data <- lang_pair
write.xlsx(data, "../Data/AIIC2301_lang_pair_Data.xlsx")

### lang_study ----
data <- lang_study
write.xlsx(data, "../Data/AIIC2301_lang_study_Data.xlsx")

### lang_study ----
data <- country
write.xlsx(data, "../Data/AIIC2301_country.xlsx")

### remuneration ----
data <- remuneration
write.xlsx(data, "../Data/AIIC2301_remuneration_Data.xlsx")

# AIIC Data ----
path.aiic <- "../Data/AIIC_db/"
files.aiic <- list.files(path.aiic)

path <- paste0(path.aiic, files.aiic[grepl("extraction_members", files.aiic)])
members_extraction <- read.xlsx(path) 

path <- paste0(path.aiic, files.aiic[grepl("extraction_precand", files.aiic)])
precandidates_extraction  <- read.xlsx(path) %>% 
  rename("ID" = "AIIC_OrigID", "Region.Link:.Region.Name.EN" = "Region.Link") %>% 
  mutate(Member.Since = NA) %>% 
  relocate("Member.Since",.after = Portal.Type)

data <- rbind(members_extraction, precandidates_extraction) %>%
  select(-ID, -Portal.Type, -`City.in.English:.Long.Name_EN`, -`Region.Link:.Region.Name.EN`) %>% 
  mutate(Member.Since = as.numeric(format(as.Date(Member.Since, origin = "1899-12-30"),"%Y")),
         Member.Status = ifelse(Member.Status == "Pre-candidate", "Precandidate", Member.Status))

names(data) <- c("aiic_status", "member_since", "employment_status", "sex", "age", "Email", "Code")
data <- data %>% 
  mutate(employment_status = plyr::mapvalues(employment_status, c("Freelance Interpreter", "Staff Interpreter"), c("Freelance", "Staff")),
         sex = plyr::mapvalues(sex,  c("M", "F"), c("Male", "Female")),
         age_num = ifelse(age == is.na(age), NA, 2024 - age),
         Code = ifelse(Code == "SUR", "SRB", Code)) %>% 
  left_join(region)

write.xlsx(data, "../Data/AIIC2301_AIIC_Data.xlsx")

# Codebook ----
tables.names <- c("interpreter", "basic", "lang_cat", "official_pair", "lang_pair", "lang_study", "market", "days_mode", "days_condition", "length_session", "remuneration", "other_income", "quality_life", "health", "other_condition", "final_quest")

wb <- createWorkbook()

## Variable Names ----
sheet <- "Variable Names"
addWorksheet(wb, sheet)

df <- NULL
for (i in tables.names) {
  aux <- get(i) %>%
    names() %>%
    data.frame(names = ., section = i)

  if (i != "interpreter") {
    aux <- aux %>% filter(names != "id_interpreter")
  }

  df <- rbind(df, aux)
}

var.names <- read.xlsx("../Input/Codebook_imp.xlsx")

codebook <- df %>%
  left_join(var.names)

writeData(wb, sheet, codebook)

## Variable Categories ----
sheet <- "Variable Categories"
addWorksheet(wb, sheet)

df <- NULL
t <- 1
for (i in tables.names) {
  aux <- get(i)
  var.names <- names(aux)
  v <- 1
  for (j in var.names) {
    aux <- get(i) %>%
      select_at(c(j)) %>%
      unique()

    num <- aux %>%
      t() %>%
      as.vector()

    if (is.numeric(num) & length(num) > 5) {
      aux <- data.frame(category = "Integer")
    } else if (is.character(num) & length(num) > 5 & j != "max_section") {
      aux <- data.frame(category = "String")
    } else {
      aux <- aux %>%
        rename("category" = j)
    }

    aux <- aux %>%
      mutate(
        var = j,
        section = paste0(str_pad(t, 2, "left", 0), ". ", i)
      )

    df <- rbind(df, aux)

    if (i != "interpreter") {
      df <- df %>% filter(var != "id_interpreter")
    }
    v <- v + 1
  }
  t <- t + 1
}

var.categories <- df %>%
  na.omit() %>%
  relocate(section, var, category) %>%
  arrange(section, var, category)

writeData(wb, sheet, var.categories)

## Languages ----
sheet <- "Languages"
addWorksheet(wb, sheet)
lang_list <- read_csv(lang.path) %>%
  rename("id_language" = "id")
writeData(wb, sheet, lang_list)

## Region ----
sheet <- "Region"
addWorksheet(wb, sheet)
writeData(wb, sheet, region)

## Currency  ----
sheet <- "Currency"
addWorksheet(wb, sheet)
currency <- read_csv(currency.path) %>%
  rename("id_currency" = "id")
writeData(wb, sheet, currency)

## Organization ----
sheet <- "Organization"
addWorksheet(wb, sheet)
writeData(wb, sheet, organisation)

saveWorkbook(wb, "../Output/Codebook.xlsx", overwrite = T)
