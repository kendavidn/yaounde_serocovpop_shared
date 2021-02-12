
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   UTILITY FUNCTIONS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

printpick <- function(p) {
  if (params$mode == "word" | params$mode == "pdf") {
    return(p)
  } else if (params$mode == "html") {
    return(ggplotly(p))
  } else {
    print("params not set")
  }
}


gt_or_kable <- function(tab) {
  if (params$mode == "html" | params$mode == "pdf") {
    gt(tab)
  } else if (params$mode == "word"){
    kable(tab)
  } else {
    print("params not set")
  }
}

countprop <- function(df, col, fct_reorder = T, fct_inseq = F, date_parse = F,  na_relevel = F) {
  out <- df %>%
    group_by({{ col }}) %>%
    summarise(n = sum(counter)) %>% 
    mutate(prop = n / sum(unique(n))) %>%
    group_by({{ col }}) %>%
    mutate(countprop = paste0("**", n, "**", "<br>", "<span style='color:gray30'>", round(100 * prop, 0), "%", "</span>")) %>% 
    mutate(countprop_nobreak = paste0("**", n, "**", " (", "<span style='color:gray30'>", round(100 * prop, 0), "%)", "</span>")) %>% 
    ungroup()
  
  if (fct_reorder == T) {
    out <- out %>%
      mutate("{{col}}" := as.factor({{ col }})) %>%
      mutate("{{col}}" := fct_reorder({{ col }}, n))
  }
  if (na_relevel == T) {
    out <- out %>%
      mutate("{{col}}" := fct_relevel({{ col }}, "No response", after = Inf))
  }
  if (fct_inseq == T) {
    out <- out %>%
      mutate("{{col}}" := fct_inseq({{ col }}, n))
  }
  if (date_parse == T) {
    out <- out %>%
      mutate("{{col}}" := as_date({{ col }}))
  }
  
  return(out)
}


# ~~ Rename for countplot axis function ---------------------------


rename_for_countplot_axis <- function(df) {
  the_clean_name <- names(df)[1]
  cleaned_name <- axis_name[which(clean_name == the_clean_name)]
  out <-
    df %>%
    rename_with(.cols = 1, .fn = ~cleaned_name)
  return(out)
}


# ~~ Count plot function ---------------------------

countplot <- function(df, col, drop_unused = F, multi_color = F, flip = F,
                      vjust_label = -0.2, hjust_label = 0.5, remove_titles = F, date_parse = F) {
  p <-
    df %>%
    ggplot() +
    geom_blank() +
    theme(legend.position = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    coord_cartesian(clip = "off") + 
    theme(axis.text.x = element_text(face = "bold", color = "black"),
          axis.text.y = element_text(face = "bold", color = "black"))

  ifelse(multi_color == TRUE,
    p <- p + geom_col(aes(x = {{ col }}, y = n, fill = {{ col }}), width = 0.7) ,
    p <- p + geom_col(aes(x = {{ col }}, y = n), fill = "#1c6db0", width = 0.7)
  )
  
  ifelse(flip == TRUE,
    p <- p + geom_richtext(aes(x = {{ col }}, y = n, label = countprop_nobreak),
      size = 2.5, alpha = 0.85, vjust = vjust_label, hjust = hjust_label,
      label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")
    ),
    p <- p + geom_richtext(aes(x = {{ col }}, y = n, label = countprop),
      size = 2.5, alpha = 0.85, vjust = vjust_label, hjust = hjust_label,
      label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")
    ) )
    
  if(date_parse == FALSE){ 
           p + scale_x_discrete(drop = drop_unused, expand = expansion(add = c(0.5, 0.5)))
  }

  if (flip == TRUE) {
    p <- p + coord_flip(clip = "off")
  }

  if (remove_titles == TRUE) {
    p <- p +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  }

  return(p)
}


# ~~ Count plot print function ---------------------------


countplotprint <- function(df, col, flip = F, fct_reorder = F, fct_inseq = F, date_parse = F, multi_color = F, na_relevel = F, drop_unused = F, 
                           vjust_label = -0.2, hjust_label = 0.5, remove_titles = T) {
  
  if(flip == TRUE){
    vjust_label <- 0.5 
    hjust_label <- -0.2
  } 
  
  ## summarise then rename the variables
  df1 <- 
    df %>%
    countprop({{ col }}, fct_reorder = fct_reorder, fct_inseq = fct_inseq, date_parse = date_parse, na_relevel = na_relevel) %>%
    rename_for_countplot_axis()

  ## extract variable name to pass to plot function
  the_axis_name <- names(df1)[1]

  p <-
    df1 %>%
    countplot(.data[[the_axis_name]], drop_unused = drop_unused, multi_color = multi_color, 
              flip = flip, vjust_label = vjust_label, hjust_label = hjust_label, remove_titles = remove_titles, date_parse)
  
  
  ## print pdf or html
  printpick(p)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   plot_upset FUNCTION ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_upset <- function(df, cat_col, denom = NA, id_col = "id_ind", intersect_cutoff = 0, layout = NA, sep = "--",
                       set_size_lab = "Set size, \n % who ticked this option", 
                       intersect_size_lab = "Intersection size, \n % who ticked this combination"
                       ){
  
  if(is.na(denom)){
    denom <- df
    
  }
  
  
  # Drop individuals with combinations that occur too infrequently
  # intersection size and matrix plot will use cut data frame
  # set size will still use the full data frame
  # but it will need to drop sets which are entirely not featured in the comb matrix
  
  df_cut <- 
    df %>% 
    group_by({{cat_col}}) %>% 
    add_tally() %>% 
    filter(n > intersect_cutoff) %>% 
    ungroup()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Calculate intersection size----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  intersect_size <-
    df_cut %>%
    count({{cat_col}}) %>%
    mutate(prop = n / length(unique(denom[[id_col]]))) %>%
    mutate(countprop = paste0("**", n, "**", ",<br>", "<span style='color:gray30'>", 
                              round(100 * prop, 0), "%", "</span>")) %>% 
    mutate("{{cat_col}}" := fct_reorder({{cat_col}}, -n)) %T>% 
    {.[1] %>% pull() %>% levels() ->> intersect_size_order}
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Calculate set size  ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sets to keep are those sets will be featured in matrix plot
  sets_to_keep <- 
    intersect_size_order %>% 
    as_tibble() %>% 
    separate_rows(1,  sep = sep) %>% 
    pull(1) %>% unique()
  
  set_size <- 
    df %>% 
    select({{cat_col}}) %>% 
    separate_rows({{cat_col}},sep = sep) %>% 
    count({{cat_col}}) %>% 
    filter({{cat_col}} %in% sets_to_keep) %>% 
    mutate("{{cat_col}}" := fct_reorder({{cat_col}}, n)) %>%
    mutate(prop = n / length(unique(denom[[id_col]]))) %>%
    mutate(countprop = paste0("**", n, "**", ", ", "<span style='color:gray30'>", 
                              round(100 * prop, 0), "%", "</span>")) %T>% 
    {.[1] %>% pull() %>% levels() ->> set_size_order}
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Derive intersection matrix from intersect_size object ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## extract column names for upset data frame
  
  set_names <- 
    intersect_size[,1] %>% 
    mutate("{{cat_col}}" := as.character({{cat_col}})) %>% 
    separate_rows(1, sep = sep) %>% 
    pull(1) %>% unique()
  
  ##  convert from tibble to df, fill in df then back to tibble
  
  intersect_matrix_init <- data.frame(intersect_size[,1])
  
  for (row in 1:nrow(intersect_matrix_init)) {
    for (i in set_names) {
      if (str_detect(intersect_matrix_init[row, 1], i)) {
        intersect_matrix_init[row, i] <- 1
      }
    }
  }
  
  intersect_matrix <- 
    intersect_matrix_init %>% 
    as_tibble() %>% 
    mutate(across(.cols = 2:ncol(.), .fns = ~ replace_na(.x, 0))) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = "set") %>% 
    mutate("{{cat_col}}" :=factor({{cat_col}}, levels = intersect_size_order )) %>% 
    mutate(set = factor(set, levels = set_size_order ) )# %>% 
    # filter(!is.na(set))
    
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Plot data ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  intersect_matrix_plot <- 
    intersect_matrix %>% 
    ggplot() + 
    geom_point(aes(y = set , x = {{cat_col}}, size = value, alpha = value)) + 
    geom_stripes(data = intersect_matrix,
                 aes(y = set)) +
    scale_size_continuous(range = c(2.2,3.8)) + 
    scale_alpha_continuous(range = c(0.08,1)) + 
    scale_x_discrete(expand = expansion(add = c(0.75, 0.75))) +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
    theme(legend.position = "none",
          axis.text.x = element_blank(), 
          axis.line.x.bottom = element_blank(),
          axis.ticks.x.bottom = element_blank(), 
          axis.title.x = element_text(size = 7, color = "gray30", hjust = 1, face = "plain"), 
          axis.title.y = element_blank(), 
          axis.text.y = element_text(hjust = 0, size = 10), 
          axis.ticks.y = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() , 
          plot.margin = unit(c(0,0,0,0), "lines")
    )
  
  if (intersect_cutoff == 0 ){
    intersect_matrix_plot <- intersect_matrix_plot + 
      labs(x = "")
  } else if (intersect_cutoff == 1 ){
    intersect_matrix_plot <- intersect_matrix_plot + 
      labs(x = paste0("Only combinations with more than ", intersect_cutoff ," individual are shown."))
  } else if (intersect_cutoff > 1 ){
    intersect_matrix_plot <- intersect_matrix_plot + 
      labs(x = paste0("Only combinations with more than ", intersect_cutoff," individuals are shown."))
  } 
  
  
  intersect_size_plot <- 
    intersect_size %>% 
    ggplot() +
    geom_col(aes(x = {{cat_col}}, y = n), fill = my_green, width = 0.5) +
    geom_richtext(aes(x = {{cat_col}} , y = n, label = countprop), 
                  size = 2.5, alpha = 0.85, vjust = -0.2, 
                  label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_x_discrete(expand = expansion(add = c(0.75, 0.75))) +
    coord_cartesian(clip = "off") +
    labs(y = "") +
    labs(title = intersect_size_lab) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      plot.margin = unit(c(0,0,0,0), "lines"),
      axis.ticks.x.bottom = element_blank(),
      axis.title.y = element_blank(), 
      plot.title = element_text(size = 8),
      axis.line.y = element_line()
    )
  
  
  set_size_plot <-  
    set_size %>% 
    ggplot() +
    geom_col(aes(x = {{cat_col}}, y = n), fill = my_green, width = 0.5) +
    geom_richtext(aes(x = {{cat_col}} , y = n, label = countprop), 
                  size = 2.5, alpha = 0.85, hjust = 1.14, 
                  label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")
                  ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_reverse(expand = expansion(mult = c(0.4, 0))) +
    coord_flip(clip = "off") +
    labs(x = "") + 
    labs(y = set_size_lab) + 
    theme(
      axis.title.x = element_text(size = 8),
      plot.margin = unit(c(0,0,0,0), "lines"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.x = element_line()
    )
  
  
  if (is.na(layout)) {
    layout <- c(
      area(1, 1, 6, 3), # spacer
      area(1, 4, 6, 10), # intersection size (top right)
      area(7, 1, 10, 3), # set size (bottom left)
      area(7, 4, 10, 10) # intersection matrix (bottom right)
    )
  }
  
  # plot(layout)
  
  out_p <- 
    plot_spacer() + 
    intersect_size_plot + 
    set_size_plot + 
    intersect_matrix_plot + 
    plot_layout(design = layout) #&  
    #theme(plot.background = element_rect(color = "#f5f5f5"))  
  
  
  print(out_p)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Lang Rei confidence interval ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Point and confidence interval estimates of true prevalence
# from independent binomial samples for the target population, sensitivity and specificity
lang_rei_CI <-  function(
  nprev,       # Sample size for prevalence
  kprev,       # Frequency of positive diagnoses in sample of size nprev
  nsens,       # Sample size for sensitivity
  ksens,       # Frequency of positive diagnoses in sample of size nsens
  nspec,       # Sample size for specificity
  kspec,       # Frequency of negative diagnoses in sample of size nspec
  conflevel=.95) # Confidence level
{
  # Observed relative frequencies
  obs.prev = kprev/nprev
  obs.sens = ksens/nsens
  obs.spec = kspec/nspec
  
  # Rogan-Gladen point estimate of true prevalence
  est.prev = (obs.prev+obs.spec-1)/(obs.sens+obs.spec-1)
  est.prev = min(1,max(0,est.prev))
  
  # Adjustments
  zcrit = qnorm((1+conflevel)/2)
  plus  = 2
  
  nprev. = nprev+zcrit^2
  kprev. = kprev+zcrit^2/2
  
  nsens. = nsens+plus
  ksens. = ksens+plus/2
  
  nspec. = nspec+plus
  kspec. = kspec+plus/2
  
  obs.prev. = kprev./nprev.
  obs.sens. = ksens./nsens.
  obs.spec. = kspec./nspec.
  
  est.prev. = (obs.prev.+obs.spec.-1)/(obs.sens.+obs.spec.-1)
  
  # Youden index
  Youden. = obs.sens.+obs.spec.-1
  
  # Standard error of est.prev.
  se.est.prev. = sqrt(
    obs.prev.*(1-obs.prev.)/nprev. +
      obs.sens.*(1-obs.sens.)/nsens. * est.prev.^2 +
      obs.spec.*(1-obs.spec.)/nspec. * (1-est.prev.)^2
  )/abs(Youden.)
  
  # Shift parameter
  dprev = 2*zcrit^2*
    (est.prev.*obs.sens.*(1-obs.sens.)/nsens. - (1-est.prev.)*obs.spec.*(1-obs.spec.)/nspec.)
  
  # Adjusted confidence limits
  LCL = est.prev.+dprev - zcrit*se.est.prev.
  UCL = est.prev.+dprev + zcrit*se.est.prev.
  LCL = min(1,max(0,LCL))
  UCL = min(1,max(0,UCL))
  
  return(data.frame(LCL, UCL))
}
# 
# # Example 1.
# CI_Binom(
#   nprev=241, # Sample size for prevalence
#   kprev=5, # Frequency of positive diagnoses in sample of size nprev
#   nsens=positives, # Sample size for sensitivity
#   ksens=true_positives, # Frequency of positive diagnoses in sample of size nsens
#   nspec=negatives, # Sample size for specificity
#   kspec=true_negatives, # Frequency of negative diagnoses in sample of size nspec
#   conflevel=.95) # Confidence level
# 
# 
# # se = calculated sensitivity
# # nse = sample size of positive samples from validation study
# # sp = calculated specificity
# # nsp = sample size of negative samples from validation study
# # ap = apparent prevalence
# # p = rogan-gladen true prevalence
# # np = sample size for prevalence
# # z = critical value
# 
# # see equation 16 in https://pubmed.ncbi.nlm.nih.gov/24416798/ for formula
# lang_rei_CI <- function(se, nse, sp, nsp, ap, p, np, z){
#   
#   var_p <- sqrt( (  ((ap*(1-ap))/np) + ((p^2*se*(1-se))/nse) + (1+p)^2 + ((sp*(1-sp))/nsp)    )
#                  /
#                    ((se+sp-1)^2)
#   )
#   
#   dp <- 2 * z^2 * (p * ((se*(1-se))/nse) - (1-p)*((sp*(1-sp))/nsp))
#   
#   upper_int <- p + dp + (z * sqrt(var_p))
#   lower_int <- p + dp - (z * sqrt(var_p))
#   
#   return(data.frame(upper_int = upper_int, 
#                     lower_int = lower_int))
# }


# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #   MANUAL UPSET FUNCTION ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~  Calculate intersection size----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# intersect_size <-
#   yao %>%
#   count(cat_breadwin) %>%
#   mutate(prop = n / sum(unique(n))) %>%
#   mutate(countprop = paste0("**", n, "**", ",<br>", "<span style='color:gray30'>", 
#                             round(100 * prop, 1), "%", "</span>")) %>% 
#   mutate(cat_breadwin = fct_reorder(cat_breadwin, -n)) %T>% 
#   {.[1] %>% pull() %>% levels() ->> intersect_size_order}
# 
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~  Calculate set size  ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set_size <- 
#   yao %>% 
#   select(id_ind, cat_breadwin) %>% 
#   separate_rows(cat_breadwin,sep = "--") %>% 
#   count(cat_breadwin) %>% 
#   mutate(cat_breadwin = fct_reorder(cat_breadwin, n)) %>%
#   mutate(prop = n / length(unique(yao$id_ind))) %>%
#   mutate(countprop = paste0("**", n, "**", ",<br>", "<span style='color:gray30'>", 
#                             round(100 * prop, 1), "%", "</span>")) %T>% 
#   {.[1] %>% pull() %>% levels() ->> set_size_order}
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~  Derive intersection matrix ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## extract column names for upset data frame
# 
# set_names <- 
#   intersect_size[,1] %>% 
#   mutate(cat_breadwin = as.character(cat_breadwin)) %>% 
#   separate_rows(1, sep = "--") %>% 
#   pull(1) %>% unique()
# 
# ##  convert from tibble to df, fill in df then back to tibble
# 
# intersect_matrix_init <- data.frame(intersect_size[,1])
# 
# for (row in 1:nrow(intersect_matrix_init)) {
#   for (i in set_names) {
#     if (str_detect(intersect_matrix_init[row, 1], i)) {
#       intersect_matrix_init[row, i] <- 1
#     }
#   }
# }
# 
# intersect_matrix <- 
#   intersect_matrix_init %>% 
#   as_tibble() %>% 
#   mutate(across(.cols = 2:ncol(.), .fns = ~ replace_na(.x, 0))) %>% 
#   pivot_longer(cols = 2:ncol(.), names_to = "set") %>% 
#   mutate(cat_breadwin = factor(cat_breadwin, levels = intersect_size_order )) %>% 
#   mutate(set = factor(set, levels = set_size_order ) )
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~  Plot data ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# intersect_matrix_plot <- 
#   intersect_matrix %>% 
#   ggplot() + 
#   geom_point(aes(y = set , x = cat_breadwin, size = value)) + 
#   geom_stripes(data = intersect_matrix,
#                aes(y = set)) +
#   scale_size_continuous(range = c(-1,5)) + 
#   theme(legend.position = "none",
#         axis.text.x = element_blank(), 
#         axis.line.x.bottom = element_blank(),
#         axis.ticks.x.bottom = element_blank(), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank(), 
#         axis.text.y = element_text(hjust = 0, size = 10), 
#         axis.ticks.y = element_blank()
#   )
# 
# 
# intersect_size_plot <- 
#   intersect_size %>% 
#   ggplot() +
#   geom_col(aes(x = cat_breadwin, y = n), fill = "skyblue3", width = 0.7) +
#   geom_richtext(aes(x = cat_breadwin , y = n, label = countprop), size = 3, alpha = 0.85, vjust = -0.2 ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.07))) +
#   scale_x_discrete(expand = expansion(mult = c(0, 0))) +
#   coord_cartesian(clip = "off") +
#   labs(y = "", title = "Intersection size \n(and % who chose this combination)") +
#   theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.ticks.x.bottom = element_blank(),
#     axis.title.y = element_text(hjust = 0.5), 
#     plot.title = element_text(size = 10)
#   )
# 
# 
# set_size_plot <-  
#   set_size %>% 
#   ggplot() +
#   geom_col(aes(x = cat_breadwin, y = n), fill = "skyblue3", width = 0.7) +
#   geom_richtext(aes(x = cat_breadwin , y = n, label = countprop), size = 3, alpha = 0.85, hjust = 1 ) +
#   scale_x_discrete(expand = expansion(mult = c(0, 0))) +
#   scale_y_reverse(expand = expansion(mult = c(0.2, 0))) +
#   coord_flip(clip = "off") +
#   labs(x = "", y = "Set size \n(and % who chose this option)") + 
#   theme(
#     axis.ticks.x.bottom = element_blank(), 
#     axis.text.y = element_blank(), 
#     axis.ticks.y = element_blank()
#   )
# 
# 
# layout <- c(
#   area(1, 1, 5, 3), # spacer
#   area(1, 4, 5, 10), # intersection size (top right)
#   area(6, 1, 7, 3), # set size (bottom left)
#   area(6, 4, 7, 10) # intersection matrix (bottom right)
# )
# 
# # plot(layout)
# 
# # plot_spacer() + 
# #   intersect_size_plot + 
# #   set_size_plot + 
# #   intersect_matrix_plot + 
# #   plot_layout(design = layout)
# # 
# # 
# # 
# # 
# 
# 















