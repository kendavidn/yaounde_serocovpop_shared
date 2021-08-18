knitr::opts_chunk$set(echo = FALSE, warning = F, message = F, dpi = 300, cache = T,
                      fig.width = 6, 
                      fig.height = 1.75,
                      fig.align = "center"
                      #, 
                      #dev="cairo_pdf"
                      )

# force figures not to float
knitr::opts_chunk$set(fig.pos = "!H", out.extra = "")


# params$mode <- "pdf"

if(!require("pacman")) install.packages("pacman")
library(pacman)

pkgs <- 
  c(
    "renv",
    "readxl",
    "here",
    "remotes",
    "janitor",
    "stringi",
    "renv",
    "xlsx",
    "reshape2",
    "viridisLite",
    "styler",
    "paletteer",
    "ggtext",
    "epiR",
    "purrr",
    "scales",
    "lubridate",
    "ISOweek",
    "cowplot",
    "magrittr",
    "flextable",
    "huxtable",
    "lme4",
    "ggallin",
    "scatterpie",
    "ggnewscale",
    "DescTools",
    "packcircles",
    "eulerr",
    "cAIC4",
    "lmerTest",
    "car",
    "glmglrt",
    "survey",
    "srvyr",
    "bootComb",
    "patchwork",
    "anytime",
    "sf",
    "ggspatial",
    "tidyverse"
  )

pacman::p_load(char = pkgs)

# remotes::install_github("NightingaleHealth/ggforestplot@547617e63fa481a5f28ffc56c07d46be4af688b2")
library(ggforestplot)

## print loaded packages (for Nature communications list of software)
# data.frame(sessioninfo::package_info()) %>%
#   subset(attached == TRUE, c(package, loadedversion)) %>%
#   transmute(output_string = paste0(package, " (", loadedversion, ")")) %>%
#   pull(1) %>%
#   paste(collapse = ", ")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Palettes and themes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_palette <- paletteer_d("awtools::bpalette") %>% as.character() %>% str_sub(end = 7)

scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = my_palette)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = my_palette)
}


# theme
my_theme <- theme_classic() +
  theme(text = element_text(size = 12.5, family = "Avenir Next"),
        rect = element_blank(), # transparent background
        plot.title = (element_text(face = "bold", hjust = 0.5, size = 13)),
        plot.subtitle = (element_text( hjust = 0.5, size = 8, color = alpha("black", 0.7))),
        plot.caption = element_text(size = 6.5, color = "gray50", hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(1,"line"),
        axis.title = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = alpha("gray50", 0.1), size = 0.25), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color = 'white'),
        panel.spacing.x = unit(3, "mm"), 
        title = element_text(face = "bold"), 
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        panel.background = element_rect(fill = "transparent")
        #strip.background = element_blank()
  )

theme_set(my_theme)

options(scipen=999) # turn off scientific notation

options(tibble.print_max = 35, tibble.print_min = 35)

GeomText$default_aes$family <- "Avenir Next"
GeomLabel$default_aes$family <- "Avenir Next"
GeomRichText$default_aes$family <- "Avenir Next"

## colors

my_green <- "#32969B"
my_darkgreen <- "#105659"
my_verydarkgreen <- "#0e453e"
my_lightgreen <- "#87ccc7"
my_orange <- "#ff983d"
my_darkorange <- "#b33605"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Read in category dictionary, for recoding multi-answer columns ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## read in clean names and replace
data_dict <- read_excel(here("data/data_dictionary.xlsx")) 
raw_category <- transmute(data_dict, raw_category = ifelse(is.na(raw_category), "PLACEHOLDER_TEXT_",  raw_category)) 
clean_category <- transmute(data_dict, clean_category = ifelse(is.na(clean_category), "PLACEHOLDER_TEXT_",  clean_category)) 

# place together and arrange so order is preserved
category_dictionary_tib <- 
  tibble(raw_category, clean_category) %>% 
  mutate(length = str_length(raw_category)) %>% 
  arrange(-length)

## now extract the arranged vectors
raw_category <- category_dictionary_tib$raw_category 
clean_category <- category_dictionary_tib$clean_category 

## combine into final dictionary 
category_dictionary <- setNames(object = clean_category, nm = raw_category)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Read and process data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yao_raw <- read_csv(file = here("data/yaounde_covid_seroprev_dataset.csv"))

yao_clean <-
  yao_raw %>%
  ## add counter column. 1 for all real records. Useful for counting later (where we use 0 for fake records)
  mutate(counter = 1) %>% 
  mutate(all_respondents = "1") %>% 
  # filter out records not validated. There are 4 people from one household that were accidentally interviewed. We drop them.
  filter(cat_validation_status == "validation_status_approved") %>% 
  # For paper, drop everyone who did not get tested for antibodies
  filter(!is.na(cat_igg_result) & cat_igg_result != "") %>% 
  ## remove rows with double slash in id
  mutate(id_quest = str_replace(id_quest, "//", "/")) %>%
  ## remove accents
  mutate(id_quest = stri_trans_general(id_quest, "Latin-ASCII")) %>%
  ## separate questionnaire id then recombine to form unique household id and individual id
  separate(id_quest, into = c("loc_hhld", "id_hhld_letter", "id_hhld_num", "id_ind_num"), sep = "/", remove = F) %>%
  mutate(loc_hhld = str_replace_all(loc_hhld, "NKOMNKANA", "NKOMKANA")) %>%
  mutate(id_hhld = paste(loc_hhld, id_hhld_num, sep = "_")) %>%
  mutate(id_ind = paste(loc_hhld, id_hhld_num, id_ind_num, sep = "_")) %>%
  relocate(id_hhld, id_ind, .before = everything()) %>%
  ## arrange by these ids
  arrange(loc_hhld, id_hhld_num, id_ind_num) %>%
  # filter(id_quest != "MOKOLO/KAR/013/0003") %>%
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Clean multi-answer columns ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mutate(across( .cols = matches("mcat"), 
               .fns = ~ str_replace_all(.x, " ", "--") 
)) %>% 
  mutate(across( .cols = matches("mcat"), 
                 .fns = ~ str_replace_all(.x, category_dictionary)
  )) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Clean single-answer columns ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# education level
mutate(cat_educ = case_when(cat_educ == "ecole_secondaire" ~ "Secondary", 
                            cat_educ == "ecole_primaire" ~ "Primary", 
                            cat_educ == "universit" ~ "University", 
                            cat_educ == "aucune_instruction_officielle" ~ "No formal instruction", 
                            cat_educ == "doctorat" ~ "Doctorate", 
                            cat_educ == "autre" ~ "Other", 
                            TRUE ~ cat_educ)) %>% 
  mutate(cat_symp_severity = case_when(cat_symp_severity == "sympt_mes_mod_r_s" ~ "Moderate", 
                                       cat_symp_severity == "sympt_mes_s_v_res" ~ "Severe", 
                                       TRUE ~ cat_symp_severity)) %>% 
  mutate(has_mem_lost_job = case_when(has_mem_lost_job == "perte_totale_du_travail_revenus" ~ "Complete loss of work income", 
                                      has_mem_lost_job == "pas_de_perte_de_travail_revenus" ~ "No loss of work income", 
                                      has_mem_lost_job == "diminution_du_temps_de_travail_revenus" ~ "Reduction in work income", 
                                      TRUE ~ has_mem_lost_job)) %>%
  mutate(has_partic_COVID_study = dplyr::recode(has_partic_COVID_study, 
                                         "ne_sais_pas" = "Don't Know")) %>% 
  mutate(has_tested = dplyr::recode(has_tested, 
                             "ne_r_ponds_pas__inappropri__ne_sait_pas_" = "No response"
  )) %>% 
  mutate(is_smoker = dplyr::recode(is_smoker, 
                            "No_fumeur__je_n_ai_jamais_fum" = "Non-smoker", 
                            "fumeur__je_fume_actuellement" = "Smoker", 
                            "ex_fumeur__j_ai_fum__mais_ne_fume_plus" = "Ex-smoker"
  ), 
  is_smoker = if_else(str_detect(is_smoker, "jamais"), 
                      "Non-smoker", 
                      is_smoker) ) %>% 
  mutate(has_contact_COVID = dplyr::recode(has_contact_COVID, 
                                    "je_ne_sais_pas" = "I don't know"
  )) %>% 
  mutate(has_contact_traveler = dplyr::recode(has_contact_traveler, 
                                        "je_ne_sais_pas" = "I don't know"
  )) %>% 
  mutate(rate_virus_serious = dplyr::recode(rate_virus_serious, 
                                     "plus_haut_que_les_autres_personnes" = "More than other people's", 
                                     "moins_que_les_autres_personnes" = "Less than other people's", 
                                     "comme_les_autres_personnes" = "The same as other people's"
  )) %>% 
  mutate(cat_sero_test_done = dplyr::recode(cat_sero_test_done, 
                                     "test_s_rologique_non_fait" = "Serologic test not done", 
                                     "test_s_rologique_fait" = "Serologic test done")) %>% 
  mutate(cat_igg_result = dplyr::recode(cat_igg_result, 
                                 "positif" = "Positive", 
                                 "n_gatif" = "Negative")) %>% 
  mutate(cat_igm_result = dplyr::recode(cat_igm_result, 
                                 "positif" = "Positive", 
                                 "n_gatif" = "Negative")) %>% 
  mutate(cat_antigen_result = dplyr::recode(cat_antigen_result, 
                                     "positif" = "Positive", 
                                     "n_gatif" = "Negative")) %>% 
  mutate(cat_PCR_result = dplyr::recode(cat_PCR_result, 
                                 "positif" = "Positive", 
                                 "n_gatif" = "Negative", 
                                 "test_non_interpr_table" = "Non-interpretable", 
  )) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Clean across multiple columns ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## replace ne repond pas variants with No response
mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "ne_r_pond_pas__inappropri__ne_sait_pas__", "No response"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "ne_r_pond_pas__inappropri__ne_sait_pas_", "No response"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "ne_peut_pas_r_pondre__inappropri__ne_sai", "No response"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "ne_r_pond_pas__inappropri__ne_", "No response"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "je_n_ai_pas_fais_de_d_marches", "I have not tried"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "je_n_ai_pas_fais_d_marches", "I have not tried"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "clairement_oui", "Definitely yes"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "clairement_non", "Definitely not"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "en_partie", "Partly"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "non", "No"))) %>%
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "oui", "Yes"))) %>% 
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "femme", "Female"))) %>% 
  mutate(across(.cols = function(.x) is.character(.x) | is.factor(.x) , .fns = ~ str_replace_all(.x, "homme", "Male"))) %>% 
  # Undo some unwanted cleaning side effects  ----
  mutate(mcat_recommend = str_replace_all(mcat_recommend, 
                                        "Recommended a No-COVID consultation", "Recommended a non-COVID consultation")) %>% 
  # calculate BMI
  mutate(val_BMI =  val_weight_kg/(((val_height_cm)/100)^2)  ) %>% 
  # # positivity categories
  # mutate(cat_pos = if_else(cat_igg_result == "Positive" | cat_igm_result == "Positive", 
  #                          "Positive", 
  #                          "Negative")) %>% 
  # mutate(cat_pos = factor(cat_pos, levels = c("Positive", "Negative"))) %>% 
  # positivity categories
  ## positivity category was originally meant to be either igg or igm
  ## but we changed to just igg
  mutate(cat_pos = cat_igg_result) %>% 
  mutate(cat_pos = factor(cat_pos, levels = c("Positive", "Negative"))) %>%  
  mutate(cat_igg_result_num = dplyr::recode(cat_igg_result, 
                                      "Negative" = "0", 
                                      "Positive" = "1"), 
         cat_igg_result_num = as.numeric(cat_igg_result_num)) %>% 
  mutate(cat_igm_result_num = dplyr::recode(cat_igm_result, 
                                     "Negative" = "0", 
                                     "Positive" = "1"), 
         cat_igm_result_num = as.numeric(cat_igm_result_num)) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ Infection Regression prep ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # set dependent var for regression
  mutate(cat_pos_num = dplyr::recode(cat_pos, "Positive"  = 1, "Negative" = 0)) %>% 
  # age categories
  mutate(cat_age = cut(val_age, 
                       breaks = c(4.9,14,29,44,65,100), 
                       labels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +")), 
         cat_age = fct_relevel(cat_age, "30 - 44")
         ) %>% 
  # bmi categories
  mutate(cat_BMI = cut(val_BMI, 
                       breaks = c(0, 18.49, 24.99, 29.99, 100), 
                       labels = c("\\< 18.5 (Underweight)", "18.5 - 24.9", "25 - 30 (Overweight)", " \\> 30 (Obese)")), 
         # set reference level for regressions
         cat_BMI = factor(cat_BMI, levels = c("18.5 - 24.9", "\\< 18.5 (Underweight)", "25 - 30 (Overweight)", " \\> 30 (Obese)") )
  ) %>% 
  # contact with traveler 
  mutate(has_contact_traveler = ifelse(is.na(has_contact_traveler), "I don't know", has_contact_traveler), 
         has_contact_traveler = dplyr::recode(has_contact_traveler, 
                                        "No" = "No contact with traveler", 
                                        "Yes" = "Recent contact with traveler", 
                                        "I don't know" = "Unsure about traveler contact"), 
         has_contact_traveler = factor(has_contact_traveler, 
                                        levels = c("No contact with traveler",
                                                   "Recent contact with traveler",
                                                   "Unsure about traveler contact"))
         ) %>% 
  # contact with COVID 
  mutate(has_contact_COVID = ifelse(is.na(has_contact_COVID), "I don't know", has_contact_COVID), 
         has_contact_COVID = dplyr::recode(has_contact_COVID, 
                                        "No" = "No COVID contact", 
                                        "Yes" = "Recent COVID contact", 
                                    "I don't know" = "Unsure about COVID contact"), 
         has_contact_COVID = factor(has_contact_COVID, 
                                        levels = c("No COVID contact",
                                                   "Recent COVID contact",
                                                   "Unsure about COVID contact"))
  ) %>% 
  # chronic conditions
  mutate(has_chronic = dplyr::recode(has_chronic, 
                              "No response" = "No comorbidity", 
                              "No" = "No comorbidity", 
                              "Yes" = "Has comorbidity")) %>%  
  mutate(has_chronic = factor(has_chronic, levels = c("No comorbidity", "Has comorbidity"))) %>% 
  # is breadwinner 
  mutate(is_breadwin = dplyr::recode(is_breadwin, 
                              "No response" = "Not breadwinner", 
                              "No" = "Not breadwinner",
                              "Yes" = "Breadwinner")) %>% 
  mutate(is_breadwin = factor(is_breadwin, levels = c("Not breadwinner", "Breadwinner"))) %>% 
  # hhld area
  mutate(loc_hhld_area = str_to_title(loc_hhld_area),
         loc_hhld_area = dplyr::recode(loc_hhld_area, 
                                "Citeverte" = "Cité Verte", 
                                "Tsingaoliga" = "Tsinga Oliga"), 
         loc_hhld_area = fct_relevel(loc_hhld_area, "Cité Verte")) %>% 
  # number in household
  mutate(n_hhld_indiv = n_hhld_adults + n_hhld_children) %>% 
  mutate(cat_n_hhld_indiv = cut(n_hhld_indiv, 
                                breaks = c(0, 2, 5, 20), 
                                labels = c("1 - 2", "3 - 5", "\\> 5" )), 
         # set reference level for regressions
         cat_n_hhld_indiv = factor(cat_n_hhld_indiv, levels = c("1 - 2", "3 - 5", "\\> 5" ) )) %>% 
  # does household have children?
  mutate(has_hhld_children = ifelse(n_hhld_children > 0, "With children", "No children"), 
         has_hhld_children = factor(has_hhld_children, levels = c("No children", "With children"))) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ Symptomatic Regression prep ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mutate(COVID_symptom_count = rowSums(select(., matches("had_symp") & matches('cough|fever|fatigue|headache|muscle_pain|sore_throat|runny|short_breath|nausea|diarrhoea')))) %>% 
  mutate(has_COVID_symptoms = "No",
         has_COVID_symptoms = ifelse(had_symp_lost_smell == 1, "Yes", has_COVID_symptoms), 
         has_COVID_symptoms = ifelse(had_symp_fever == 1 & had_symp_cough == 1 , "Yes", has_COVID_symptoms), 
         has_COVID_symptoms = ifelse(COVID_symptom_count >= 3 , "Yes", has_COVID_symptoms)) %>% 
  mutate(has_COVID_symptoms_num  = ifelse(has_COVID_symptoms == "Yes", 1, 0)) %>% 
  mutate(is_smoker = factor(is_smoker, levels = c("Non-smoker", "Ex-smoker", "Smoker"))) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ Make sure factors are factors ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mutate(cat_sex = factor(cat_sex, levels = c("Female", "Male"))) %>% 
  mutate(cat_educ = as.factor(cat_educ)) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ Determinants of health-seeking behavior regression prep ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # combine two variables: have you visited healthcare for COVID-like symptoms and have you visited for non-COVID-like symptoms
  mutate(mcat_consult_nospace = str_replace_all(mcat_consult, " ", "_"),
         mcat_consult_non_COVID_nospace = str_replace_all(mcat_consult_non_COVID, " ", "_")) %>% 
  unite(col = mcat_consult_any, sep = "--", mcat_consult_nospace, mcat_consult_non_COVID_nospace, na.rm = T) %>% 
  mutate(mcat_consult_any = str_split(mcat_consult_any, pattern = "--")) %>%  # split
  mutate(mcat_consult_any = purrr::map(.x = .$mcat_consult_any, .f = ~unique(.x))) %>%  # drop duplicates
  mutate(mcat_consult_any =  purrr::map_chr(.x = .$mcat_consult_any, .f = ~paste(.x, collapse = "--")), # recombine
         mcat_consult_any = str_replace_all(mcat_consult_any, "_", " ")) %>% # replace underscore
  mutate(mcat_consult_any = ifelse(mcat_consult_any == "", NA, mcat_consult_any)) %>% 
  # acute symptoms and chronic conditions
  mutate(has_acute_symp = ifelse(mcat_symp == "No symptoms", "No", "Yes")) %>% 
  mutate(has_chron_or_symp = (has_acute_symp == "Yes" | has_chronic == "Has comorbidity")) %>% 
  # Only two individuals responded with "Other" THey consulted a pharmacist and a kinesthesiologist
  mutate(has_consult_formal_care = str_detect(mcat_consult_any, "Private|Public|Nurse|Doctor|Other"), 
         has_consult_formal_care = replace_na(has_consult_formal_care, FALSE)) %>% 
# COVID-compatible symptoms
  mutate(has_COVID_compatible_symp = ifelse(mcat_symp == "No symptoms", "No", "Yes")) %>% 
  mutate(has_COVID_compatible_symp = ifelse(mcat_symp == "Other", "No", has_COVID_compatible_symp))


## read in DHS age-sex data (for weighting)
yaounde_dhs_age_sex <- 
  read_excel(here("data/yaounde_age_2018_2020.xlsx"), sheet = 2) %>% 
  select(age_lower, age_upper, cat_age, male, female, total) %>% 
  # create wider age categories, then resummarise
  mutate(cat_age = cut(age_lower, c(5,15,30,45,65,105), right = FALSE )) %>% 
  group_by(cat_age) %>% 
  summarise(n_male_dhs = sum(male),
            n_female_dhs = sum(female)) %>% 
  # NA category is under 5s
  filter(!is.na(cat_age)) %>% 
  mutate(cat_age = factor(cat_age, labels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  pivot_longer(cols = c(2:3), values_to = "stratum_size_dhs", names_to = "cat_sex") %>% 
  mutate(cat_sex = dplyr::recode(cat_sex, 
                          "n_male_dhs" = "Male", 
                          "n_female_dhs" = "Female"))


## create weights object
age_sex_weights <- 
  yao_clean %>% 
  filter(!is.na(cat_igg_result)) %>% 
  group_by(cat_age, cat_sex) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(stratum_size_sample = sum(n)) %>% 
  ungroup() %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  ## weight is basically "how many real people does each sampled person represent"
  ## this is calculated as: (stratum size in ref. pop)/(stratum size in sample)
  mutate(stratum_weight = stratum_size_dhs/stratum_size_sample) %>% 
  ## normalize so that the sum of weights is the number of individuals in sample
  mutate(weight_per_individual = stratum_weight * sum(stratum_size_sample)/sum(stratum_size_dhs)) %>% 
  ## drop unneeded
  select(cat_age, cat_sex, stratum_size_dhs, stratum_size_sample, stratum_weight, weight_per_individual)


# ~~~~++  bind weights to yao data frame ----
yao <- 
  yao_clean %>% 
  left_join(age_sex_weights, by = c("cat_age", "cat_sex")) 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Household representatives subset ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Subset and count households with at least one head
yao_hhld_heads <- 
  yao %>%
  # keep only household heads
  filter(is_head == "Yes") %>%
  # pick the first household head if multiple
  group_by(id_hhld) %>%
  arrange(id_ind) %>%
  slice_head(n = 1) %>%
  # ungroup
  ungroup()

yao_hhld_first_adults <- 
  yao %>% 
  # add counter for excluding non-headed households
  mutate(is_head_counter = if_else(is_head == "Yes", 1, 0)) %>%
  group_by(id_hhld) %>%
  # keep households without a head
  filter(sum(is_head_counter) == 0) %>%
  # keep adults
  filter(val_age >= 18) %>%
  # take the first adult 
  arrange(id_ind_num) %>% 
  slice_head(n = 1) %>%
  # ungroup
  ungroup()


yao_hhld  <- 
  yao_hhld_heads %>% 
  bind_rows(yao_hhld_first_adults)


yao_has_chron_or_symp <- 
  yao %>% 
  filter(has_chron_or_symp)


