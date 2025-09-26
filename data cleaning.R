

library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)
library(modelsummary)
library(lme4)
library(marginaleffects)
library(broom.mixed)
library(emmeans)
library(fixest)
library(lme4)

setwd("Dropbox/Officer Profiles/Final Project Files/data")


############## Data ###################

survey <- read_csv("survey.csv") %>%
  clean_names() %>%
  slice(-1, -2, -3)


survey <- subset(survey, attention == "Business,Opinion")


############## Sample Descriptives ###################


# Gender
gender_tab <- survey %>%
  count(gender) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  mutate(variable = "Gender") %>%
  rename(category = gender)

# Race
race_tab <- survey %>%
  count(race) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  mutate(variable = "Race/Ethnicity") %>%
  rename(category = race)

# Partisanship
party_tab <- survey %>%
  count(partisanship) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  mutate(variable = "Partisanship") %>%
  rename(category = partisanship)

# Education
edu_tab <- survey %>%
  count(education) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  mutate(variable = "Education") %>%
  rename(category = education)

# Age
age_tab <- survey %>%
  count(age) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  mutate(variable = "Age") %>%
  rename(category = age)

# Income
income_tab <- survey %>%
  count(income) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  mutate(variable = "Income") %>%
  rename(category = income)

# Combine all descriptives
desc <- bind_rows(
  gender_tab,
  race_tab,
  party_tab,
  edu_tab,
  age_tab,
  income_tab
) %>%
  select(variable, category, n, percent)

desc_tab <- desc %>%
  kable("latex", booktabs = TRUE,
        col.names = c("Variable", "Category", "N", "%"),
        caption = "") %>%
  kable_styling(latex_options = c("HOLD_position")) %>%
  collapse_rows(columns = 1, latex_hline = "none", valign = "top")

save_kable(desc_tab, "tables/desc_tab.tex")



############## Setting up Trait Recognition ###################




# --- build image_num -> profile mapping ---

# 1) column positions for the survey you already read (with real colnames)
var_pos <- tibble(
  var       = names(survey),
  col_index = seq_along(names(survey))
)

# 2) anchor on the race columns for each image_num
race_cols <- var_pos %>%
  filter(str_detect(var, "^[x]?[0-9]+_race_rec$")) %>%
  mutate(image_num = as.integer(str_match(var, "([0-9]+)_race_rec$")[,2]))

# 3) grab the FIRST ROW (with <img src=...>) to get the exact URLs by column position
raw_all <- read_csv("survey.csv", col_names = T, show_col_types = FALSE)
header_row1 <- as.character(raw_all[1, ])

url_map <- tibble(
  col_index = seq_along(header_row1),
  header    = header_row1,
  url       = str_extract(header_row1, "https?://[^\\\"]+")
)

# 4) join to get url per image_num, then map url -> profile via your key
key <- read_csv("profile_urls.csv", show_col_types = FALSE)  # cols: profile, url

image_to_profile <- race_cols %>%
  left_join(url_map, by = "col_index") %>%
  left_join(key,     by = "url") %>%
  select(image_num, profile)

traits <- survey %>%
  select(response_id, matches("^x?\\d+_(race|gen|som|age)_rec$")) %>%
  pivot_longer(
    -response_id,
    names_to      = c("image_num","trait"),
    names_pattern = "^(?:x)?(\\d+)_(race|gen|som|age)_rec$",
    values_to     = "value"
  ) %>%
  pivot_wider(names_from = trait, values_from = value) %>%
  mutate(image_num = as.integer(image_num)) %>%
  left_join(image_to_profile, by = "image_num") %>%
  transmute(
    response_id,
    profile,
    race       = na_if(str_squish(race), ""),
    somatotype = na_if(str_squish(som),  ""),
    gender     = na_if(str_squish(gen),  ""),
    age     = na_if(str_squish(age),  "")
    
  ) %>%
  arrange(response_id, profile)


traits_final <- traits %>%
  filter(!(is.na(race) & is.na(gender) & is.na(somatotype))) %>%
  mutate(profile_cell = str_extract(profile, "^[0-9]+[a-z]")
  )%>%
  filter(str_starts(response_id, "R_"))



profile_age_mode <- traits_final %>%
  count(profile_cell, age) %>%
  group_by(profile_cell) %>%
  slice_max(n, with_ties = FALSE) %>%   # pick most frequent category
  ungroup() %>%
  select(profile_cell, modal_age = age)

profile_age_weight <- traits_final %>%
  mutate(age_num = case_when(
    age == "Under 26" ~ 1,
    age == "26-34"    ~ 2,
    age == "35+"      ~ 3
  )) %>%
  group_by(profile_cell) %>%
  summarize(age_score = mean(age_num, na.rm = TRUE), .groups = "drop")



# simple lookup table
cells <- tibble::tribble(
  ~profile_cell, ~true_race, ~true_gender, ~true_body_type,
  "1a","White","Male","Ectomorph",
  "1b","White","Male","Mesomorph",
  "1c","White","Male","Endomorph",
  "2a","White","Female","Ectomorph",
  "2b","White","Female","Mesomorph",
  "2c","White","Female","Endomorph",
  "3a","Black","Male","Ectomorph",
  "3b","Black","Male","Mesomorph",
  "3c","Black","Male","Endomorph",
  "4a","Black","Female","Ectomorph",
  "4b","Black","Female","Mesomorph",
  "4c","Black","Female","Endomorph",
  "5a","Latino","Male","Ectomorph",
  "5b","Latino","Male","Mesomorph",
  "5c","Latino","Male","Endomorph",
  "6a","Latino","Female","Ectomorph",
  "6b","Latino","Female","Mesomorph",
  "6c","Latino","Female","Endomorph",
  "7a","Asian","Male","Ectomorph",
  "7b","Asian","Male","Mesomorph",
  "7c","Asian","Male","Endomorph",
  "8a","Asian","Female","Ectomorph",
  "8b","Asian","Female","Mesomorph",
  "8c","Asian","Female","Endomorph"
)

# merge onto your data
traits_final <- traits_final %>%
  left_join(cells, by = "profile_cell")

traits_final <- traits_final %>%
  mutate(
    race = recode(race,
                  "Black or African American" = "Black",
                  "Latino or Hispanic"        = "Latino"
    ),
    somatotype = recode(somatotype,
                        "Muscular, athletic build"     = "Mesomorph",
                        "Slim, narrow build"           = "Ectomorph",
                        "Rounder body, higher body fat"= "Endomorph"
    )
  )

# 0) Build comparison flags (same as before)
comparison_table <- traits_final %>%
  select(profile_cell, true_gender, true_body_type, true_race,
         gender, somatotype, race) %>%
  mutate(
    match_gender    = (true_gender    == gender),
    match_body_type = (true_body_type == somatotype),
    match_race      = (true_race      == race)
  )

# 1) Clean/standardize true_gender just in case (Male/Female title case)
comparison_table <- comparison_table %>%
  mutate(true_gender = case_when(
    tolower(true_gender) %in% c("male", "m")   ~ "Male",
    tolower(true_gender) %in% c("female", "f") ~ "Female",
    TRUE ~ as.character(true_gender)
  ))


by_true_gender <- comparison_table %>%
  group_by(true_gender) %>%
  summarise(
    Gender_correct    = sum(match_gender,    na.rm = TRUE),
    BodyType_correct  = sum(match_body_type, na.rm = TRUE),
    Race_correct      = sum(match_race,      na.rm = TRUE),
    Gender_n          = sum(!is.na(match_gender)),
    BodyType_n        = sum(!is.na(match_body_type)),
    Race_n            = sum(!is.na(match_race)),
    .groups = "drop"
  )


acc_long <- by_true_gender %>%
  pivot_longer(cols = ends_with(c("_correct","_n")),
               names_to = c("Trait",".value"),
               names_pattern = "(.*)_(correct|n)") %>%
  mutate(
    Trait = recode(Trait,
                   "Gender"   = "Gender",
                   "BodyType" = "Body Type",
                   "Race"     = "Race"),
    true_gender = factor(true_gender, levels = c("Female","Male")),
    `Percent Agreement` = round(correct / n * 100, 1)
  )

chance <- tibble::tibble(
  Trait = c("Gender","Body Type","Race"),
  p0    = c(0.5, 1/3, 0.25)
)

acc_long <- acc_long %>%
  left_join(chance, by = "Trait") %>%
  mutate(
    pval = pmap_dbl(list(correct, n, p0),
                    ~ binom.test(..1, ..2, p = ..3,
                                 alternative = "greater")$p.value),
    `By-chance p` = signif(pval, 3)
  )

trait_acc <- acc_long %>%
  arrange(true_gender, Trait) %>%
  select(Trait,  n, correct, `Percent Agreement`, `pval`) %>%
  kbl(format="latex", booktabs=TRUE,
      col.names=c("Trait","N","Correct","Percent Agreement","By-chance p"),
      caption="") %>%
  kable_styling(latex_options= c("hold_position", "striped")) %>%
  group_rows("Female Officers", 1, 3) %>%
  group_rows("Male Officers",   4, 6)

save_kable(trait_acc, "tables/trait_acc.tex")




############## Setting up Trait Change Recognition ###################


change_cols <- var_pos %>%
  filter(str_detect(var, "^(?:x)?(?:[1-9]|1[0-6])_change$")) %>%
  mutate(series_num = as.integer(str_match(var, "^(?:x)?(\\d+)_change$")[,2]))



series_to_profile <- change_cols %>%
  left_join(url_map, by = "col_index") %>%
  left_join(key,     by = "url") %>%
  select(series_num, profile)


changes <- survey %>%
  select(response_id, matches("^x?\\d+_change$")) %>%
  pivot_longer(
    -response_id,
    names_to      = "series_num",
    names_pattern = "^(?:x)?(\\d+)_change$",
    values_to     = "distinct_trait_raw"
  ) %>%
  mutate(
    series_num     = as.integer(series_num),
    distinct_trait = str_squish(distinct_trait_raw),
    distinct_trait = na_if(distinct_trait, ""),
    # normalize common label variants just in case
    distinct_trait = dplyr::case_when(
      str_detect(distinct_trait, regex("^race",   TRUE)) ~ "Race or ethnicity",
      str_detect(distinct_trait, regex("^gender", TRUE)) ~ "Gender",
      str_detect(distinct_trait, regex("^body",   TRUE)) ~ "Body type",
      str_detect(distinct_trait, regex("^facial", TRUE)) ~ "Facial expression",
      str_detect(distinct_trait, regex("^uniform",TRUE)) ~ "Uniform",
      str_detect(distinct_trait, regex("^age",    TRUE)) ~ "Age",
      TRUE ~ distinct_trait
    )
  ) %>%
  select(-distinct_trait_raw)

# 2) Attach triad "series" metadata via first image URL -> profile key
change_recognition <- changes %>%
  left_join(series_to_profile, by = "series_num") %>%
  mutate(
    profile_cell = str_extract(profile, "^[0-9]+[a-z]")
  ) %>%
  arrange(response_id, profile, series_num)

change_recog_final <- change_recognition %>%
  select(response_id, profile, profile_cell, series_num, distinct_trait)


change_recog_final <- change_recog_final %>%
  filter(!(is.na(distinct_trait))) 


change_recog_final <- change_recog_final%>%
  mutate(series_name = case_when(
    series_num == 1  ~ "body type (white males)",
    series_num == 2  ~ "body type (black males)",
    series_num == 3  ~ "body type (asian males)",
    series_num == 4  ~ "body type (latino males)",
    series_num == 5  ~ "body type (white females)",
    series_num == 6  ~ "body type (black females)",
    series_num == 7  ~ "body type (asian females)",
    series_num == 8  ~ "body type (latino females)",
    series_num == 9  ~ "race (ecto male)",
    series_num == 10 ~ "race (meso male)",
    series_num == 11 ~ "race (endo male)",
    series_num == 12 ~ "race (ecto female)",
    series_num == 13 ~ "race (meso female)",
    series_num == 14 ~ "race (endo female)",
    series_num == 15 ~ "gender (white ecto)",
    series_num == 16 ~ "gender (black ecto)",
    TRUE ~ NA_character_
  ))%>%
  filter(str_starts(response_id, "R_"))


change_recog_final <- change_recog_final %>%
  # collapse true_change into 3 categories
  mutate(true_change_general = case_when(
    series_num %in% 1:8   ~ "Body Type",
    series_num %in% 9:14  ~ "Race or ethnicity",
    series_num %in% 15:16 ~ "Gender",
    TRUE ~ NA_character_
  )) %>%
  # collapse respondent’s answer into 3 categories
  mutate(distinct_trait_general = case_when(
    str_detect(distinct_trait, regex("body", ignore_case = TRUE))  ~ "Body Type",
    str_detect(distinct_trait, regex("race|ethnic", ignore_case = TRUE)) ~ "Race or ethnicity",
    str_detect(distinct_trait, regex("gender", ignore_case = TRUE)) ~ "Gender",
    TRUE ~ NA_character_
  )) %>%
  # flag match
  mutate(match = if_else(true_change_general == distinct_trait_general, 1L, 0L, missing = NA_integer_))

p0 <- 1/6

summary_table <- change_recog_final %>%
  group_by(true_change_general) %>%
  summarise(
    n       = n(),
    correct = sum(match, na.rm = TRUE),
    accuracy = mean(match, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # add binomial test p-value
  rowwise() %>%
  mutate(
    p_value = binom.test(correct, n, p = p0, alternative = "greater")$p.value
  ) %>%
  ungroup() %>%
  mutate(
    accuracy = scales::percent(accuracy, accuracy = 0.1),
    p_value  = signif(p_value, 3)
  ) %>%
  arrange(desc(as.numeric(sub("%","",accuracy)) / 100))



change_acc <- summary_table %>%
  select(
    `Trait Change` = true_change_general,
    N = n,
    Correct = correct,
    `Respondent Accuracy` = accuracy,
    `By-chance p` = p_value
  ) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = ""
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 10
  )


save_kable(change_acc, "tables/change_acc.tex")



m_fixest <- feols(
  match ~ true_change_general, change_recog_final )

etable(m_fixest)

############## Setting up Officer Impressions ###################
library(fixest)



impressions <- survey %>%
  # keep id + all *_impression_* columns (allow optional leading x)
  select(gender, race, partisanship, education, income, response_id,
         matches("^x?\\d+_impression_\\d+$")) %>%
  pivot_longer(
    cols = -c(response_id, gender, race, partisanship, education, income),
    names_to      = c("image_num","item"),
    names_pattern = "^(?:x)?(\\d+)_impression_(\\d+)$",
    values_to     = "rating"
  ) %>%
  mutate(
    image_num = as.integer(image_num),
    item      = as.integer(item),
    rating    = suppressWarnings(as.numeric(str_squish(rating)))
  ) %>%
  left_join(image_to_profile, by = "image_num") %>%
  mutate(
    profile_cell = str_extract(profile, "^[0-9]+[a-z]")
  ) %>%
  select(response_id, profile, profile_cell, item, rating,
         gender, race, partisanship, education, income) %>%
  arrange(response_id, profile, item)

# example labels; replace with your actual wording
item_map <- tibble::tibble(
  item = 1:9,
  label = c("Competent","Intelligent","Capable","Efficient","Skilled",
            "Assertive","Confident","Independent","Competitive")
)

impressions <- impressions %>%
  left_join(item_map, by = "item") %>%
  relocate(label, .after = item)


impressions_final <- impressions %>%
  filter(!(is.na(rating))) 

impressions_wide <- impressions_final %>%
  group_by(response_id, profile, profile_cell, item, gender, race, partisanship, education, income) %>%
  summarize(rating = first(na.omit(rating)), .groups = "drop") %>%
  pivot_wider(
    id_cols     = c(response_id, profile, profile_cell, gender, race, partisanship, education, income),
    names_from  = item,                 # or use `label` if you prefer names
    values_from = rating,
    names_prefix = "imp_",
    values_fill = NA_real_
  ) %>%
  arrange(response_id, profile)


impressions_wide <- impressions_wide %>%
  mutate(
    comp = rowMeans(across(c(imp_1, imp_2, imp_3)), na.rm = TRUE),
    agcy = rowMeans(across(c(imp_4, imp_5, imp_6)), na.rm = TRUE),
    warm = rowMeans(across(c(imp_7, imp_8, imp_9)), na.rm = TRUE)  # <-- careful, was imp_6 twice
  )

impressions_wide <- impressions_wide %>%
  left_join(cells, by = "profile_cell")%>%
  left_join(profile_age_mode, by = "profile_cell")%>%
  left_join(profile_age_weight, by = "profile_cell")

impressions_wide <- impressions_wide %>%
  mutate(profile = str_replace(profile, "^(\\d+[a-z])$", "\\11"))


# Descriptives

desc_tbl <- impressions_wide %>%
  summarise(
    N_comp   = sum(!is.na(comp)),   Mean_comp = mean(comp, na.rm = TRUE),   SD_comp = sd(comp, na.rm = TRUE),
    Min_comp = min(comp, na.rm = TRUE), Max_comp = max(comp, na.rm = TRUE),
    N_agcy   = sum(!is.na(agcy)),   Mean_agcy = mean(agcy, na.rm = TRUE),   SD_agcy = sd(agcy, na.rm = TRUE),
    Min_agcy = min(agcy, na.rm = TRUE), Max_agcy = max(agcy, na.rm = TRUE),
    N_warm   = sum(!is.na(warm)),   Mean_warm = mean(warm, na.rm = TRUE),   SD_warm = sd(warm, na.rm = TRUE),
    Min_warm = min(warm, na.rm = TRUE), Max_warm = max(warm, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(everything(),
                      names_to = c("Stat","Scale"),
                      names_pattern = "(.*)_(comp|agcy|warm)",
                      values_to = "Value") %>%
  tidyr::pivot_wider(names_from = Scale, values_from = Value) %>%
  mutate(Stat = recode(Stat,
                       N="N",
                       Mean="Mean",
                       SD="SD",
                       Min="Min",
                       Max="Max")) %>%
rename("Competence" = comp ,
       "Agency" = agcy,
       "Warmth" = warm) %>%
  tidyr::pivot_longer(cols = c(Competence, Agency, Warmth),
                      names_to = "Scale",
                      values_to = "Value") %>%

  tidyr::pivot_wider(names_from = Stat, values_from = Value)%>%
  mutate(across(c(Mean, SD), ~ round(.x, 2))) 


imp_desc <- desc_tbl %>%
  kable("latex", booktabs = TRUE,
        caption = "") %>%
  kable_styling(latex_options = c("hold_position"))

save_kable(imp_desc, "tables/imp_desc.tex")


# Models 

impressions_wide <- impressions_wide |>
  mutate(true_gender = relevel(factor(true_gender), ref = "Male"))


m <- feols(comp ~ true_race * true_gender * true_body_type | response_id, data = impressions_wide)
m2 <- feols(agcy ~ true_race * true_gender * true_body_type | response_id, data = impressions_wide)
m3 <- feols(warm ~ true_race * true_gender * true_body_type | response_id, data = impressions_wide)


etable(m,m2,m3)

m <- feols(comp ~ true_race * true_gender * true_body_type, data = impressions_wide)
m2 <- feols(agcy ~ true_race * true_gender * true_body_type, data = impressions_wide)
m3 <- feols(warm ~ true_race * true_gender * true_body_type, data = impressions_wide)


etable(m,m2,m3)

men <- impressions_wide%>%
  filter(true_gender == "Male")

men <- men |>
  mutate(true_race = relevel(factor(true_race), ref = "White"))


m <- feols(comp ~ true_race * true_body_type  | response_id, data = men)
m2 <- feols(agcy ~ true_race *  true_body_type | response_id, data = men)
m3 <- feols(warm ~ true_race *  true_body_type | response_id, data = men)


etable(m,m2,m3)

m <- feols(comp ~ true_race * true_body_type , data = men)
m2 <- feols(agcy ~ true_race *  true_body_type , data = men)
m3 <- feols(warm ~ true_race *  true_body_type , data = men)


etable(m,m2,m3)


m  <- lmer(comp  ~ true_race * true_body_type + (1 | response_id), data = men)
m2 <- lmer(agcy  ~ true_race * true_body_type + (1 | response_id), data = men)
m3 <- lmer(warm  ~ true_race * true_body_type + (1 | response_id), data = men)

modelsummary(list("Competence" = m, "Agency" = m2, "Warmth" = m3),
             stars = TRUE,
             gof_omit = "ICC|Log.Lik|AIC|BIC")  # can filter what fit stats to keep

m <- feols(comp ~ true_race + true_gender + true_body_type+ gender+race+ partisanship+education+income, data = impressions_wide)
m2 <- feols(agcy ~ true_race + true_gender + true_body_type + gender+race+ partisanship+education+income, data = impressions_wide)
m3 <- feols(warm ~ true_race + true_gender + true_body_type + gender+race+ partisanship+education+income, data = impressions_wide)


etable(m,m2,m3)

m <- feols(comp ~ true_race + true_gender * true_body_type+ gender+race+ partisanship+education+income, data = impressions_wide)
m2 <- feols(agcy ~ true_race + true_gender * true_body_type + gender+race+ partisanship+education+income, data = impressions_wide)
m3 <- feols(warm ~ true_race + true_gender * true_body_type + gender+race+ partisanship+education+income, data = impressions_wide)

m <- feols(comp ~ true_race + true_gender + true_body_type +modal_age | response_id, data = impressions_wide)
m2 <- feols(agcy ~ true_race + true_gender + true_body_type +modal_age| response_id, data = impressions_wide)
m3 <- feols(warm ~ true_race + true_gender + true_body_type +modal_age| response_id, data = impressions_wide)


etable(m,m2,m3)

# Pub Model Tables

impressions_wide <- impressions_wide |>
  mutate(true_gender = relevel(factor(true_gender), ref = "Male"))


impressions_wide <- impressions_wide |>
  mutate(true_race = relevel(factor(true_race), ref = "White"))


m <- feols(comp ~ true_race + true_gender + true_body_type | response_id, data = impressions_wide)
m2 <- feols(agcy ~ true_race + true_gender + true_body_type | response_id, data = impressions_wide)
m3 <- feols(warm ~ true_race + true_gender + true_body_type | response_id, data = impressions_wide)


etable(m,m2,m3)




models <- list(
  "Competence" = m,
  "Agency"     = m2,
  "Warmth"     = m3
)

keep_and_label <- c(
  "(Intercept)" = "Intercept",
  "true_raceBlack"     = "Black officer",
  "true_raceLatino"    = "Latino officer",
  "true_raceAsian"     = "Asian officer",
  "true_raceWhite"     = "White officer",
  "true_genderFemale"  = "Female officer",
  "true_genderMale"  = "Male officer",
  "true_body_typeMesomorph" = "Mesomorph",
  "true_body_typeEndomorph" = "Endomorph"
)

full_table <- msummary(
  models,
  coef_map = keep_and_label,                 # ← keeps ONLY these terms, with these labels & order
  stars    = c('*'=.05, '**'=.01, '***'=.001),
  gof_omit = "AIC|BIC|Log.Lik|F|Adj|Within|Pseudo|Std.Errors",
  vcov     = ~ response_id,                  # cluster by respondent (optional but recommended)
  output   = "latex",
  file     = "tables/impressions_models.tex",
  add_rows = tibble::tribble(
    ~term, ~Competence, ~Agency, ~Warmth,
    "Respondent FE", "Yes", "Yes", "Yes",
    "SE clustered by respondent", "Yes", "Yes", "Yes"
  ),
  notes = "Reference categories: Asian, Male, Ectomorph. Models include respondent fixed effects; standard errors clustered by respondent."
)

latex_code <- as.character(full_table)

writeLines(latex_code, "tables/impressions_models.tex")

# Plots 






m  <- feols(comp ~ true_race * true_gender * true_body_type | response_id,
            data = impressions_wide, cluster = ~ response_id)
m2 <- feols(agcy ~ true_race * true_gender * true_body_type | response_id,
            data = impressions_wide, cluster = ~ response_id)
m3 <- feols(warm ~ true_race * true_gender * true_body_type | response_id,
            data = impressions_wide, cluster = ~ response_id)


get_ce <- function(mod, outcome_label){
  bind_rows(
    avg_comparisons(mod, variables = "true_race")      %>% mutate(Dimension = "Race"),
    avg_comparisons(mod, variables = "true_gender")    %>% mutate(Dimension = "Gender"),
    avg_comparisons(mod, variables = "true_body_type") %>% mutate(Dimension = "Body Type")
  ) %>% mutate(outcome = outcome_label)
}


ce_all <- bind_rows(
  get_ce(m,  "Competence"),
  get_ce(m2, "Agency"),
  get_ce(m3, "Warmth")
)

ce_all <- ce_all %>%
  mutate(
    Treatment = str_replace(contrast, ".* - ", ""),  # keeps the reference; swap if you prefer the focal
    Treatment = ifelse(Dimension == "Race",
                       str_replace(contrast, " - White", ""),
                       ifelse(Dimension == "Gender",
                              str_replace(contrast, "- Male", ""),
                              str_replace(contrast, "- Ectomorph", "")))
  )

ce_all_plot <- ce_all %>%
  group_by(outcome, Dimension, Treatment) %>%
  summarise(estimate = mean(estimate),
            conf.low = mean(conf.low),
            conf.high = mean(conf.high),
            .groups = "drop") %>%
  group_by(outcome, Dimension) %>%
  mutate(Treatment = fct_reorder(Treatment, estimate)) %>%
  ungroup()



plot <- ggplot(ce_all_plot, aes(x = Treatment, y = estimate, color = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey40") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .25) +
  coord_flip() +
  facet_grid(outcome ~ Dimension, scales = "free_y", space = "free_y") +
  labs(
    x = "",
    y = "Average Effect Size"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.text.y = element_text(size = 9),
    legend.position = "none"
  )


ggsave("figures/maineffects.pdf", plot = plot,
       width = 6, height = 4, dpi = 600)




#### Conditional Means ####



m_comp  <- feols(comp ~ true_race + true_gender + true_body_type | response_id,
                 data = impressions_wide)
m_agcy  <- feols(agcy ~ true_race + true_gender + true_body_type | response_id,
                 data = impressions_wide)
m_warm <- feols(warm ~ true_race + true_gender + true_body_type | response_id,
                 data = impressions_wide)


comp_means <- emmeans(m_comp,  ~ true_race + true_gender + true_body_type)
agcy_means <- emmeans(m_agcy,  ~ true_race + true_gender + true_body_type)
warm_means <- emmeans(m_warm,  ~ true_race + true_gender + true_body_type)




# 1) Make sure each df has Outcome first
comp_df <- as.data.frame(comp_means); comp_df$Outcome <- "Competence"
agcy_df <- as.data.frame(agcy_means); agcy_df$Outcome <- "Agency"
warm_df <- as.data.frame(warm_means); warm_df$Outcome <- "Warmth"

# 2) Bind and create OfficerCategory, then group-order by Body Type -> emmean
all_means <- dplyr::bind_rows(comp_df, agcy_df, warm_df) %>%
  dplyr::mutate(
    OfficerCategory = paste(true_race, true_gender, true_body_type, sep = " / "),
    true_body_type  = factor(true_body_type, levels = c("Ectomorph","Mesomorph","Endomorph"))
  ) %>%
  dplyr::group_by(Outcome) %>%
  dplyr::arrange(true_body_type, emmean, .by_group = TRUE) %>%
  dplyr::mutate(OfficerCategory = factor(OfficerCategory, levels = unique(OfficerCategory))) %>%
  dplyr::ungroup()

# Step 1: Build custom ordering based on Warmth outcome
warm_order <- all_means %>%
  filter(Outcome == "Warmth") %>%
  arrange(true_body_type, true_gender, desc(emmean)) %>%   # body type → gender → warmth
  pull(OfficerCategory) %>%
  unique()

# Step 2: Apply that order globally
all_means <- all_means %>%
  mutate(
    OfficerCategory = factor(OfficerCategory, levels = warm_order),
    true_body_type  = factor(true_body_type,
                             levels = c("Ectomorph","Mesomorph","Endomorph")),
    true_gender     = factor(true_gender,
                             levels = c("Male","Female")) # ensures male first
  )

# Compute means by outcome
mean_lines <- all_means %>%
  group_by(Outcome) %>%
  summarise(xint = mean(emmean), .groups = "drop")

# Plot with per-outcome dashed reference lines
plot <- ggplot(all_means, aes(x = emmean, y = OfficerCategory,
                      color = true_body_type, shape = true_body_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(data = mean_lines, aes(xintercept = xint),
             linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Ectomorph" = "dodgerblue",
                                "Mesomorph" = "darkred",
                                "Endomorph" = "darkgreen")) +
  scale_y_discrete(labels = ~ str_remove(.x, " / (Endomorph|Mesomorph|Ectomorph)$")) +
  labs(
    x = "Predicted Rating (Conditional Mean)",
    y = "Officer Category",
    color = "Body Type",
    shape = "Body Type"
  ) +
  facet_wrap(~ Outcome, nrow = 1, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")




ggsave("figures/conditionalmeans.pdf", plot = plot,
       width = 6, height = 4, dpi = 600)






############################# AGE INCLUDED ############################# 


# Pub Tables

impressions_wide <- impressions_wide |>
  mutate(true_gender = relevel(factor(true_gender), ref = "Male"))


impressions_wide <- impressions_wide |>
  mutate(true_race = relevel(factor(true_race), ref = "White"))


m <- feols(comp ~ true_race + true_gender + true_body_type + modal_age | response_id, data = impressions_wide)
m2 <- feols(agcy ~ true_race + true_gender + true_body_type + modal_age | response_id, data = impressions_wide)
m3 <- feols(warm ~ true_race + true_gender + true_body_type + modal_age | response_id, data = impressions_wide)


etable(m,m2,m3)




models <- list(
  "Competence" = m,
  "Agency"     = m2,
  "Warmth"     = m3
)

keep_and_label <- c(
  "(Intercept)" = "Intercept",
  "true_raceBlack"     = "Black officer",
  "true_raceLatino"    = "Latino officer",
  "true_raceAsian"     = "Asian officer",
  "true_raceWhite"     = "White officer",
  "true_genderFemale"  = "Female officer",
  "true_genderMale"  = "Male officer",
  "true_body_typeMesomorph" = "Mesomorph",
  "true_body_typeEndomorph" = "Endomorph",
  "modal_age35+" = "Late Career"
)

full_table <- msummary(
  models,
  coef_map = keep_and_label,                 # ← keeps ONLY these terms, with these labels & order
  stars    = c('*'=.05, '**'=.01, '***'=.001),
  gof_omit = "AIC|BIC|Log.Lik|F|Adj|Within|Pseudo|Std.Errors",
  vcov     = ~ response_id,                  # cluster by respondent (optional but recommended)
  output   = "latex",
  file     = "tables/impressions_models.tex",
  add_rows = tibble::tribble(
    ~term, ~Competence, ~Agency, ~Warmth,
    "Respondent FE", "Yes", "Yes", "Yes",
    "SE clustered by respondent", "Yes", "Yes", "Yes"
  ),
  notes = "Reference categories: White, Male, Ectomorph. Models include respondent fixed effects; standard errors clustered by respondent."
)

latex_code <- as.character(full_table)

writeLines(latex_code, "tables/impressions_models_age.tex")




# Conditional Means
m_comp  <- feols(comp ~ true_race + true_gender + true_body_type +modal_age | response_id,
                 data = impressions_wide)
m_agcy  <- feols(agcy ~ true_race + true_gender + true_body_type +modal_age | response_id,
                 data = impressions_wide)
m_warm <- feols(warm ~ true_race + true_gender + true_body_type +modal_age | response_id,
                data = impressions_wide)




comp_means <- emmeans(m_comp, ~ true_race + true_gender + true_body_type)
agcy_means <- emmeans(m_agcy, ~ true_race + true_gender + true_body_type)
warm_means <- emmeans(m_warm, ~ true_race + true_gender + true_body_type)



# 1) Make sure each df has Outcome first
comp_df <- as.data.frame(comp_means); comp_df$Outcome <- "Competence"
agcy_df <- as.data.frame(agcy_means); agcy_df$Outcome <- "Agency"
warm_df <- as.data.frame(warm_means); warm_df$Outcome <- "Warmth"

# 2) Bind and create OfficerCategory, then group-order by Body Type -> emmean
all_means <- dplyr::bind_rows(comp_df, agcy_df, warm_df) %>%
  dplyr::mutate(
    OfficerCategory = paste(true_race, true_gender, true_body_type, sep = " "),
    true_body_type  = factor(true_body_type, levels = c("Ectomorph","Mesomorph","Endomorph"))
  ) %>%
  dplyr::group_by(Outcome) %>%
  dplyr::arrange(true_body_type, emmean, .by_group = TRUE) %>%
  dplyr::mutate(OfficerCategory = factor(OfficerCategory, levels = unique(OfficerCategory))) %>%
  dplyr::ungroup()

# Step 1: Build custom ordering based on Warmth outcome
warm_order <- all_means %>%
  filter(Outcome == "Warmth") %>%
  arrange(true_body_type, true_gender, desc(emmean)) %>%   # body type → gender → warmth
  pull(OfficerCategory) %>%
  unique()

# Step 2: Apply that order globally
all_means <- all_means %>%
  mutate(
    OfficerCategory = factor(OfficerCategory, levels = warm_order),
    true_body_type  = factor(true_body_type,
                             levels = c("Ectomorph","Mesomorph","Endomorph")),
    true_gender     = factor(true_gender,
                             levels = c("Female","Male")) # ensures male first
  )

# Compute means by outcome
mean_lines <- all_means %>%
  group_by(Outcome) %>%
  summarise(xint = mean(emmean), .groups = "drop")

# Plot with per-outcome dashed reference lines
plot <- ggplot(all_means, aes(x = emmean, y = OfficerCategory,
                      color = true_body_type, shape = true_body_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(data = mean_lines, aes(xintercept = xint),
             linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Ectomorph" = "dodgerblue",
                                "Mesomorph" = "darkred",
                                "Endomorph" = "darkgreen")) +
  scale_y_discrete(labels = ~ str_remove(.x, " (Endomorph|Mesomorph|Ectomorph)$")) +
  labs(
    x = "Predicted Rating (Conditional Mean)",
    y = "Officer Category",
    color = "Body Type",
    shape = "Body Type"
  ) +
  facet_wrap(~ Outcome, nrow = 1, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")

ggsave("figures/conditionalmeans_age.pdf", plot = plot,
       width = 6, height = 4, dpi = 600)




## Plot 

m  <- feols(comp ~ true_race + true_gender + true_body_type + modal_age | response_id,
            data = impressions_wide, cluster = ~ response_id)
m2 <- feols(agcy ~ true_race +true_gender + true_body_type + modal_age  | response_id,
            data = impressions_wide, cluster = ~ response_id)
m3 <- feols(warm ~ true_race + true_gender+ true_body_type + modal_age  | response_id,
            data = impressions_wide, cluster = ~ response_id)


get_ce <- function(mod, outcome_label){
  bind_rows(
    avg_comparisons(mod, variables = "true_race")      %>% mutate(Dimension = "Race"),
    avg_comparisons(mod, variables = "true_gender")    %>% mutate(Dimension = "Gender"),
    avg_comparisons(mod, variables = "true_body_type") %>% mutate(Dimension = "Body Type")
  ) %>% mutate(outcome = outcome_label)
}


ce_all <- bind_rows(
  get_ce(m,  "Competence"),
  get_ce(m2, "Agency"),
  get_ce(m3, "Warmth")
)

ce_all <- ce_all %>%
  mutate(
    Treatment = str_replace(contrast, ".* - ", ""),  # keeps the reference; swap if you prefer the focal
    Treatment = ifelse(Dimension == "Race",
                       str_replace(contrast, " - White", ""),
                       ifelse(Dimension == "Gender",
                              str_replace(contrast, "- Male", ""),
                              str_replace(contrast, "- Ectomorph", "")))
  )

ce_all_plot <- ce_all %>%
  group_by(outcome, Dimension, Treatment) %>%
  summarise(estimate = mean(estimate),
            conf.low = mean(conf.low),
            conf.high = mean(conf.high),
            .groups = "drop") %>%
  group_by(outcome, Dimension) %>%
  mutate(Treatment = fct_reorder(Treatment, estimate)) %>%
  ungroup()



plot <- ggplot(ce_all_plot, aes(x = Treatment, y = estimate, color = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey40") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .25) +
  coord_flip() +
  facet_grid(outcome ~ Dimension, scales = "free_y", space = "free_y") +
  labs(
    x = "",
    y = "Average Effect Size"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.text.y = element_text(size = 9),
    legend.position = "none"
  )


ggsave("figures/maineffects_agecontrol.pdf", plot = plot,
       width = 6, height = 4, dpi = 600)



######## AGE + RANDOM INTERCEPTS ########





# Pub Tables






### Conditional Means



m_comp <- lmer(comp ~ true_race + true_gender + true_body_type +
                modal_age + (1 | response_id) + (1 | profile),
               data = impressions_wide)

m_agcy <- lmer(agcy ~ true_race + true_gender + true_body_type + 
               modal_age +  (1 | response_id) + (1 | profile),
               data = impressions_wide)

m_warm <- lmer(warm ~ true_race + true_gender + true_body_type +
              modal_age +   (1 | response_id) + (1 | profile),
               data = impressions_wide)


tidy(m_comp, effects = "fixed")
tidy(m_agcy, effects = "fixed")
tidy(m_warm, effects = "fixed")


comp_means <- emmeans(m_comp, ~ true_race + true_gender + true_body_type)
agcy_means <- emmeans(m_agcy, ~ true_race + true_gender + true_body_type)
warm_means <- emmeans(m_warm, ~ true_race + true_gender + true_body_type)



# 1) Make sure each df has Outcome first
comp_df <- as.data.frame(comp_means); comp_df$Outcome <- "Competence"
agcy_df <- as.data.frame(agcy_means); agcy_df$Outcome <- "Agency"
warm_df <- as.data.frame(warm_means); warm_df$Outcome <- "Warmth"

# 2) Bind and create OfficerCategory, then group-order by Body Type -> emmean
all_means <- dplyr::bind_rows(comp_df, agcy_df, warm_df) %>%
  dplyr::mutate(
    OfficerCategory = paste(true_race, true_gender, true_body_type, sep = " / "),
    true_body_type  = factor(true_body_type, levels = c("Ectomorph","Mesomorph","Endomorph"))
  ) %>%
  dplyr::group_by(Outcome) %>%
  dplyr::arrange(true_body_type, emmean, .by_group = TRUE) %>%
  dplyr::mutate(OfficerCategory = factor(OfficerCategory, levels = unique(OfficerCategory))) %>%
  dplyr::ungroup()

# Step 1: Build custom ordering based on Warmth outcome
warm_order <- all_means %>%
  filter(Outcome == "Warmth") %>%
  arrange(true_body_type, true_gender, desc(emmean)) %>%   # body type → gender → warmth
  pull(OfficerCategory) %>%
  unique()

# Step 2: Apply that order globally
all_means <- all_means %>%
  mutate(
    OfficerCategory = factor(OfficerCategory, levels = warm_order),
    true_body_type  = factor(true_body_type,
                             levels = c("Ectomorph","Mesomorph","Endomorph")),
    true_gender     = factor(true_gender,
                             levels = c("Male","Female")) # ensures male first
  )

# Compute means by outcome
mean_lines <- all_means %>%
  group_by(Outcome) %>%
  summarise(xint = mean(emmean), .groups = "drop")

# Plot with per-outcome dashed reference lines
ggplot(all_means, aes(x = emmean, y = OfficerCategory,
                      color = true_body_type, shape = true_body_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(data = mean_lines, aes(xintercept = xint),
             linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Ectomorph" = "dodgerblue",
                                "Mesomorph" = "darkred",
                                "Endomorph" = "darkgreen")) +
  scale_y_discrete(labels = ~ str_remove(.x, " / (Endomorph|Mesomorph|Ectomorph)$")) +
  labs(
    x = "Predicted Rating (Conditional Mean)",
    y = "Officer Category",
    color = "Body Type",
    shape = "Body Type"
  ) +
  facet_wrap(~ Outcome, nrow = 1, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")

