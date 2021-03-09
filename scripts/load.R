knitr::opts_chunk$set(echo = FALSE, warning = F, message = F, dpi = 300, cache = F,
                      fig.width = 6, 
                      fig.height = 1.75,
                      fig.align = "center"
                      #, 
                      #dev="cairo_pdf"
                      )

# force figures not to float
knitr::opts_chunk$set(fig.pos = "!H", out.extra = "")


# params$mode <- "pdf"

library(pacman)

p_load(
  "readxl",
  "here",
  "janitor",
  "stringi",
  "usethis",
  "renv",
  "plotly",
  "vegan",
  "reshape2",
  "viridis",
  "styler",
  "paletteer",
  "scales",
  "ggtext",
  "inspectdf",
  "purrr",
  "scales",
  "lubridate",
  "ISOweek",
  "highcharter",
  "leaflet",
  "grid",
  "gridExtra",
  "gridGraphics",
  "cowplot",
  "magrittr",
  "devtools",
  "ggforestplot", # devtools::install_github("NightingaleHealth/ggforestplot")
  "gt", 
  "huxtable",
  "lme4",
  "ggallin", # for log transformation accommodating negative values
  "scatterpie",
  "ggspatial",
  "ggnewscale",
  "prevalence",
  "sysfonts",
  "DescTools",
  "packcircles",
  "DescTools",
  "eulerr",
  "patch",  # remotes::install_github("r-rudra/patch")
  "tidyverse",
  "sf",
  "patchwork"
)

#p_unload("all")
# 
# font_add_google("Avenir Next")
# library("extrafont")
# extrafont::font_import()
# extrafont::loadfonts()

source(system.file("embedded","usecases.R",package = "patch"))


# usethis::use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))

# 
# # palette
# my_palette <- c("#56bfa3", "#f79f57" ,"#7570B3","#56B4E9",  #greenblue #lightorange #purplepastel  #lightblue
#                 "#3758a6" , "#CC79A7" , "#91142c", "#eb4034", #darkblue #pinkpurple #wine #orange
#                 "#a3b0c4", "#870476", "#479444", "#3cd6d6" ) # grey, # royalpurple #darkgreen # cyan
# 
my_palette <- paletteer_d("awtools::bpalette") %>% as.character() %>% str_sub(end = 7)

scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = my_palette)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = my_palette)
}

#addTaskCallback(function(...) {set.seed(11);TRUE})

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


# 
# category_names <- 
#   read_excel(here("data/EPICO19_-_all_versions_26 11 approved.xlsx"), sheet = 3) %>% 
#   select(raw_name) %>% 
#   separate(raw_name, into = c("var", "category"), sep = "/") %>% 
#   select(category) 
# 
# write_excel_csv(category_names, file = here("data/category_names.csv"), na = "")
#   
# 


yao_raw <- 
  read_excel(here("data/survey_data_dec_11.xlsx"), sheet = 2) %>% 
  type_convert()


## read in clean names and replace
data_dict <- read_excel(here("data/survey_data_dec_11.xlsx"), sheet = 3)

raw_name <- data_dict$raw_name
clean_name <- data_dict$clean_name
axis_name <- data_dict$axis_name 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  We sort the replacements in descending order of length ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this should reduce problems 
raw_category <- 
  data_dict %>% 
  select(raw_category) %>% 
  mutate(raw_category = ifelse(is.na(raw_category), "PLACEHOLDER_TEXT_PLACEHOLDER_TEXT",  raw_category)) 

clean_category <- 
  data_dict %>% 
  select(clean_category) %>% 
  mutate(clean_category = ifelse(is.na(clean_category), "PLACEHOLDER_TEXT_PLACEHOLDER_TEXT",  clean_category)) 

# place together and arrange so order is preserved
category_dictionary_tib <- 
  tibble(raw_category, clean_category) %>% 
  mutate(length = str_length(raw_category)) %>% 
  arrange(-length)

raw_category <-  category_dictionary_tib$raw_category 
clean_category <-  category_dictionary_tib$clean_category 

category_dictionary <- setNames(object = clean_category, nm = raw_category)


yao <-
  yao_raw %>%
  ## add counter column. 1 for all real records. Useful for counting later (where we use 0 for fake records)
  mutate(counter = 1) %>% 
  ## rename to new names
  rename_with(.cols = any_of(raw_name), .fn = ~ clean_name[which(raw_name == .x)]) %>%
  # select(where( ~ !(all(is.na(.x)) | all(.x=="")) )) %>%
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
  mutate(has_partic_COVID_study = recode(has_partic_COVID_study, 
                                         "ne_sais_pas" = "Don't Know")) %>% 
  mutate(has_tested = recode(has_tested, 
                             "ne_r_ponds_pas__inappropri__ne_sait_pas_" = "No response"
  )) %>% 
  mutate(is_smoker = recode(is_smoker, 
                            "No_fumeur__je_n_ai_jamais_fum" = "Non-smoker", 
                            "fumeur__je_fume_actuellement" = "Smoker", 
                            "ex_fumeur__j_ai_fum__mais_ne_fume_plus" = "Ex-smoker"
  ), 
  is_smoker = if_else(str_detect(is_smoker, "jamais"), 
                      "Non-smoker", 
                      is_smoker) ) %>% 
  mutate(has_contact_COVID = recode(has_contact_COVID, 
                                    "je_ne_sais_pas" = "I don't know"
  )) %>% 
  mutate(has_contact_traveler = recode(has_contact_traveler, 
                                        "je_ne_sais_pas" = "I don't know"
  )) %>% 
  mutate(rate_virus_serious = recode(rate_virus_serious, 
                                     "plus_haut_que_les_autres_personnes" = "More than other people's", 
                                     "moins_que_les_autres_personnes" = "Less than other people's", 
                                     "comme_les_autres_personnes" = "The same as other people's"
  )) %>% 
  mutate(had_break_med = recode(had_break_med, 
                                "je_n_ai_pas_fais_de_d_marches" = "I haven't taken any steps"
  )) %>% 
  mutate(cat_aid_frequency = recode(cat_aid_frequency, 
                                    "plus_souvent" = "More often", 
                                    "moins_souvent" = "Less often", 
                                    "egal" = "With the same frequency" )) %>% 
  mutate(cat_sero_test_done = recode(cat_sero_test_done, 
                                     "test_s_rologique_non_fait" = "Serologic test not done", 
                                     "test_s_rologique_fait" = "Serologic test done")) %>% 
  mutate(cat_igg_result = recode(cat_igg_result, 
                                 "positif" = "Positive", 
                                 "n_gatif" = "Negative")) %>% 
  mutate(cat_igm_result = recode(cat_igm_result, 
                                 "positif" = "Positive", 
                                 "n_gatif" = "Negative")) %>% 
  mutate(cat_antigen_result = recode(cat_antigen_result, 
                                     "positif" = "Positive", 
                                     "n_gatif" = "Negative")) %>% 
  mutate(cat_PCR_result = recode(cat_PCR_result, 
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
  # Convert dates to dates
  mutate(across(.cols = starts_with("dt_"), .fns = ~ as.Date(.x))) %>% 
  # calculate BMI
  mutate(val_BMI =  val_weight_kg/(((val_height_cm)/100)^2)  ) %>% 
  # filter out records not validated. There are 4 people from one household that were accidentally interviewed. We drop them.
  filter(cat_validation_status == "validation_status_approved") %>% 
  # # positivity categories
  # mutate(cat_pos = if_else(cat_igg_result == "Positive" | cat_igm_result == "Positive", 
  #                          "Positive", 
  #                          "Negative")) %>% 
  # mutate(cat_pos = factor(cat_pos, levels = c("Positive", "Negative"))) %>% 
  # positivity categories
  mutate(cat_pos = NA_character_) %>% 
  mutate(cat_pos = ifelse(cat_igg_result == "Positive" , 
                           "Positive", 
                           cat_pos)) %>% 
  mutate(cat_pos = ifelse(cat_igg_result == "Negative" , 
                           "Negative", 
                           cat_pos)) %>% 
  mutate(cat_pos = factor(cat_pos, levels = c("Positive", "Negative"))) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ Infection Regression prep ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # set dependent var for regression
  mutate(cat_pos_num = recode(cat_pos, "Positive"  = 1, "Negative" = 0)) %>% 
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
         has_contact_traveler = recode(has_contact_traveler, 
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
         has_contact_COVID = recode(has_contact_COVID, 
                                        "No" = "No COVID contact", 
                                        "Yes" = "Recent COVID contact", 
                                    "I don't know" = "Unsure about COVID contact"), 
         has_contact_COVID = factor(has_contact_COVID, 
                                        levels = c("No COVID contact",
                                                   "Recent COVID contact",
                                                   "Unsure about COVID contact"))
  ) %>% 
  # chronic conditions
  mutate(has_chronic = recode(has_chronic, 
                              "No response" = "No comorbidity", 
                              "No" = "No comorbidity", 
                              "Yes" = "Has comorbidity")) %>%  
  mutate(has_chronic = factor(has_chronic, levels = c("No comorbidity", "Has comorbidity"))) %>% 
  # is breadwinner 
  mutate(is_breadwin = recode(is_breadwin, 
                              "No response" = "Not breadwinner", 
                              "No" = "Not breadwinner",
                              "Yes" = "Breadwinner")) %>% 
  mutate(is_breadwin = factor(is_breadwin, levels = c("Not breadwinner", "Breadwinner"))) %>% 
  # respect of distancing rules
  mutate(is_respecting_distancing = factor(is_respecting_distancing, levels = c("Definitely yes", "Partly", "Definitely not", "No response" ) )) %>% 
  # hhld area
  mutate(loc_hhld_area = str_to_title(loc_hhld_area),
         loc_hhld_area = recode(loc_hhld_area, 
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
  mutate(COVID_symptom_count = rowSums(select(., matches("has_symp") & matches('cough|fever|fatigue|headache|muscle_pain|sore_throat|runny|short_breath|nausea|diarrhoea')))) %>% 
  mutate(has_COVID_symptoms = "No",
         has_COVID_symptoms = ifelse(has_symp_lost_smell == 1, "Yes", has_COVID_symptoms), 
         has_COVID_symptoms = ifelse(has_symp_fever == 1 & has_symp_cough == 1 , "Yes", has_COVID_symptoms), 
         has_COVID_symptoms = ifelse(COVID_symptom_count >= 3 , "Yes", has_COVID_symptoms)) %>% 
  mutate(has_COVID_symptoms_num  = ifelse(has_COVID_symptoms == "Yes", 1, 0)) %>% 
  mutate(is_smoker = factor(is_smoker, levels = c("Non-smoker", "Ex-smoker", "Smoker"))) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ Socioecon impact questions ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mutate(has_confin_stopped_work = factor(has_confin_stopped_work, levels = c("Yes", "No", "No response"))) %>% 
  mutate(has_faced_violence = recode(has_faced_violence, "No response" = "NR")) %>% 
  mutate(has_rev_dropped = recode(has_rev_dropped, "No response" = "NR")) %>% 
  mutate(has_confin_disrupted_life = recode(has_confin_disrupted_life, "No response" = "NR")) %>% 
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
         has_consult_formal_care = replace_na(has_consult_formal_care, FALSE)
  ) %>% 
  mutate(has_diffic_pay_medic = recode(has_diffic_pay_medic, 
                                       "I have not tried" = "No financial difficulty",
                                       "No" = "No financial difficulty",
                                       "No response" = "No financial difficulty", 
                                       "Yes" = "Some financial difficulty")) %>% 
  mutate(has_diffic_travel_care = recode(has_diffic_travel_care, 
                                       "I have not tried" = "No travel difficulty",
                                       "No" = "No travel difficulty",
                                       "No response" = "No travel difficulty", 
                                       "Yes" = "Some travel difficulty")) %>% 
  mutate(thinks_clinics_dangerous_COVID = ifelse(str_detect(mcat_diffic_find_care, "dangerous"),
                                              "Yes, dangerous", 
                                              "Not dangerous")) %>% 
  mutate(thinks_clinics_closed_COVID = ifelse(str_detect(mcat_diffic_find_care, "closed"),
                                              "Yes, closed", 
                                              "Not closed")) %>% 
  mutate(thinks_clinics_price_hiked_COVID = ifelse(str_detect(mcat_diffic_find_care, "price"),
                                              "Yes, price-hiked", 
                                              "Not price-hiked")) %>% 
  mutate(is_fearful_stigma = recode(is_fearful_stigma, 
                                    "Yes" = "Worried about stigma", 
                                    "No" = "Not worried about stigma",
                                    "No response" = "Not worried about stigma"))
  


  

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


# 
# ## write IgG serology data to file. Matching the Geneva paper 
# yao %>% 
#   mutate(counter = 1) %>% 
#   mutate(week = lubridate::week(dt_quest)) %>% 
#   mutate(pos = ifelse(cat_igg_result == "Positive", 1, 0 ), 
#          neg = ifelse(pos == 1, 0, 1)
#          ) %>% 
#   mutate(ind = ifelse(is.na(pos),  1, 0)) %>% 
#   mutate(pos = replace_na(pos, 0)) %>% 
#   mutate(neg = replace_na(neg, 0)) %>% 
#   mutate(id_hhld = as.numeric(as.factor(id_hhld))) %>% 
#   group_by(id_hhld) %>% 
#   mutate(id_ind = as.numeric(as.factor(id_hhld)),
#          id_ind = paste(id_hhld, id_ind, sep = "-")) %>%
#   mutate(cat_sex = ifelse(cat_sex == "Male", 1, 0)) %>%  
#   select(ind_id = id_ind, 
#          new_household_id = id_hhld, 
#          Sex = cat_sex,
#          age = val_age,
#          week, 
#          pos, 
#          ind,
#          neg) %>% 
#   write_csv("generated_data/serocov-pop_data_yaounde.csv")
# 
# 
