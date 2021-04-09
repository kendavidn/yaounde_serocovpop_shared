

sex_count <- yao_regn %>% count_per_group(cat_sex)

col_string <- "cat_sex"

the_formula <- paste("cat_pos_num ~", col_string, "+ (1 | id_hhld)") 

sex_regn <- 
  yao_regn %>% 
  glmer(formula = the_formula,
        data = .,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa")) 



wald_chi_square_p_val <- 
  car::Anova(sex_regn) %>% 
  data.frame() %>% 
  .[1,3]
