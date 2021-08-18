
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   UTILITY FUNCTIONS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   plot_upset FUNCTION ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_upset <- function(df, 
                       cat_col, 
                       denom = NA, 
                       id_col = "id_ind", 
                       intersect_cutoff = 0, 
                       layout = NA, 
                       sep = "--",
                       intersect_size_lim_lower = NA, 
                       intersect_size_lim_upper = NA,
                       set_size_lim_lower = NA,
                       set_size_lim_upper = NA,
                       set_size_scale_y_expand_lower = 0.4,
                       intersect_size_scale_x_expand_upper = 0.2, 
                       fill = my_green,
                       intersect_max_bars = Inf, 
                       # restrict_sets_to = NA,
                       set_size_lab = "Set size, \n % who ticked this option", 
                       intersect_size_lab = "Intersection size, \n % who ticked this combination"
                       ){
  
  if(is.na(denom)){
    denom <- df
    
  }
  
  # Drop individuals with combinations that occur too infrequently
  # intersection size and matrix plot will use cut data frame
  # set size will still use the full data frame at first
  # but it will need, at the end, drop sets which are not featured in the combination matrix
  
  df_cut <- 
    df %>% 
    #group_by({{cat_col}}) %>% 
    group_by(!!enquo(cat_col) ) %>% 
    add_tally() %>% 
    filter(n > intersect_cutoff) %>% 
    ungroup()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Calculate intersection size----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  intersect_size <-
    df_cut %>%
    count(!!enquo(cat_col)) %>%
    mutate(prop = n / length(unique(denom[[id_col]]))) %>%
    mutate(prop_pct = 100 * prop) %>%
    mutate(countprop = paste0("**", round(100 * prop, 0), "%","**", "<br>", "<span style='color:gray30'>", 
                              "(", n, ")", "</span>")) %T>% 
    mutate(!!enquo(cat_col) := fct_reorder(!!enquo(cat_col), -n)) %>% 
    arrange(-n) %>% 
    slice_head(n = intersect_max_bars) %>% 
    mutate(!!enquo(cat_col) := as.character(!!enquo(cat_col))) %T>% 
    {.[1] %>% pull() ->> intersect_size_order}
  
  
  
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
    select(!!enquo(cat_col)) %>% 
    separate_rows(!!enquo(cat_col),sep = sep) %>% 
    count(!!enquo(cat_col)) %>% 
    filter(!!enquo(cat_col) %in% sets_to_keep) %>% 
    #{if (!is.na(restrict_sets_to)) filter(., !!enquo(cat_col) %in% restrict_sets_to) else .} %>% 
    mutate(!!enquo(cat_col) := fct_reorder(!!enquo(cat_col), n)) %>%
    mutate(prop = n / length(unique(denom[[id_col]]))) %>%
    mutate(prop_pct = 100 * prop) %>%
    mutate(countprop = paste0("**", round(100 * prop, 0), "%","**", "&nbsp;&nbsp;<span style='color:gray30'>", 
                              "(", n, ")", "</span>")) %T>% 
    {.[1] %>% pull() %>% levels() ->> set_size_order}
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Derive intersection matrix from intersect_size object ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## extract column names for upset data frame
  
  set_names <- 
    intersect_size[,1] %>% 
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
    mutate(!!enquo(cat_col) :=factor(!!enquo(cat_col), levels = intersect_size_order )) %>% 
    mutate(set = factor(set, levels = set_size_order ) )# %>% 
    # filter(!is.na(set))
    
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Plot data ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  intersect_matrix_plot <- 
    intersect_matrix %>% 
    ggplot() + 
    geom_point(aes(y = set , x = !!enquo(cat_col), size = value, alpha = value)) + 
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
    mutate(!!enquo(cat_col):= fct_reorder(!!enquo(cat_col), -n)) %>% 
    ggplot() +
    geom_col(aes(x = !!enquo(cat_col), y = prop_pct), fill = fill, width = 0.5) +
    geom_richtext(aes(x = !!enquo(cat_col) , y = prop_pct, label = countprop), 
                  size = 2.5, alpha = 0.85, vjust = -0.2, label.color = NA,
                  label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) +
    scale_y_continuous(expand = expansion(mult = c(0, intersect_size_scale_x_expand_upper)), 
                       limits = c(intersect_size_lim_lower, intersect_size_lim_upper)) +
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
      plot.title = element_text(size = 8, face = "bold"),
      axis.line.y = element_line()
    )
  
  
  set_size_plot <-  
    set_size %>% 
    ggplot() +
    geom_col(aes(x = !!enquo(cat_col), y = prop_pct), fill = fill, width = 0.5) +
    geom_richtext(aes(x = !!enquo(cat_col) , y = prop_pct, label = countprop), 
                  size = 2.5, alpha = 0.85, hjust = 1.14, label.color = NA,
                  label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")
                  ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_reverse(expand = expansion(mult = c(set_size_scale_y_expand_lower, 0)), 
                    limits = c(set_size_lim_lower, set_size_lim_upper)
                    ) +
    coord_flip(clip = "off") +
    labs(x = "") + 
    labs(y = set_size_lab) + 
    theme(
      axis.title.x = element_text(size = 8, face = "bold"),
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
  
  
  return(out_p)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   plot_upset2 FUNCTION ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_upset2 <- function(df,
                       cat_col,
                       denom = NA,
                       id_col = "id_ind",
                       intersect_cutoff = 0,
                       layout = NA,
                       sep = "--",
                       subtitle = "",
                       fill = my_green,
                       intersect_max_bars = Inf,
                       # restrict_sets_to = NA,
                       set_size_lab = "Set size, \n % who ticked this option",
                       intersect_size_lab = "Intersection size, \n % who ticked this combination", 
                       theme_axis_text_x_top_size = 6,
                       scale_size_continuous_range = c(2.2,3.8)
){

  if(is.na(denom)){
    denom <- df
    
  }

  # df = yao %>%
  #   filter(cat_igg_result == "Positive") %>%
  #   filter(mcat_symp != "No symptoms")
  # 
  # cat_col = expr(mcat_symp)
  # sep = "--"
  # intersect_max_bars = 10
  # denom =  yao %>% filter(cat_igg_result == "Negative")
  # set_size_lab = "Prevalence \n(% and No. with this symptom)"
  # intersect_size_lab = "Co-prevalence, \n % and no. with this combination of symptoms"
  # intersect_cutoff = 0
  # id_col = "id_ind"
  # fill = my_green
  # subtitle = ""

  
  # Drop individuals with combinations that occur too infrequently
  # intersection size and matrix plot will use cut data frame
  # set size will still use the full data frame at first
  # but it will need, at the end, drop sets which are not featured in the combination matrix
  
  df_cut <- 
    df %>% 
    #group_by({{cat_col}}) %>% 
    group_by(!!enquo(cat_col) ) %>% 
    add_tally() %>% 
    filter(n > intersect_cutoff) %>% 
    ungroup()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Calculate intersection size----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  intersect_size <-
    df_cut %>%
    count(!!enquo(cat_col)) %>%
    mutate(prop = n / length(unique(denom[[id_col]]))) %>%
    mutate(prop_pct = 100 * prop) %>%
    mutate(countprop = paste0("**", round(100 * prop, 0), "%","**", ", ", "<span style='color:gray30'>", 
                              "(", n, ")", "</span>")) %>% 
    mutate(!!enquo(cat_col) := fct_reorder(!!enquo(cat_col), -n)) %>% 
    arrange(-n) %>% 
    slice_head(n = intersect_max_bars) %>% 
    mutate(!!enquo(cat_col) := as.character(!!enquo(cat_col))) %T>% 
    {.[1] %>% pull() ->> intersect_size_order}
  
  
  
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
    select(!!enquo(cat_col)) %>% 
    separate_rows(!!enquo(cat_col),sep = sep) %>% 
    count(!!enquo(cat_col)) %>% 
    filter(!!enquo(cat_col) %in% sets_to_keep) %>% 
    #{if (!is.na(restrict_sets_to)) filter(., !!enquo(cat_col) %in% restrict_sets_to) else .} %>% 
    mutate(!!enquo(cat_col) := fct_reorder(!!enquo(cat_col), n)) %>%
    mutate(prop = n / length(unique(denom[[id_col]]))) %>%
    mutate(prop_pct = 100 * prop) %>%
    mutate(countprop = paste0("**", round(100 * prop, 0), "%","**", ", ", "<span style='color:gray30'>", 
                              n, "</span>"))  %T>% 
    {.[1] %>% pull() %>% levels() ->> set_size_order}
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Derive intersection matrix from intersect_size object ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## extract column names for upset data frame
  
  set_names <- 
    intersect_size[,1] %>% 
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
    mutate(!!enquo(cat_col) :=factor(!!enquo(cat_col), levels = intersect_size_order )) %>% 
    mutate(set = factor(set, levels = set_size_order ) )# %>% 
  # filter(!is.na(set))
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Plot data ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  intersect_matrix_plot <- 
    intersect_matrix %>% 
    mutate(!!enquo(cat_col):= fct_rev(!!enquo(cat_col))) %>% 
    ggplot() + 
    geom_point(aes(x = set , y = !!enquo(cat_col), size = value, alpha = value)) + 
    geom_stripes(data = intersect_matrix,
                 aes(y = !!enquo(cat_col))) +
    scale_size_continuous(range = scale_size_continuous_range) + 
    scale_alpha_continuous(range = c(0.08,1)) + 
    scale_y_discrete(expand = expansion(add = c(0.75, 0.75)), position = "left") +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5)), position = "top") +
    labs(subtitle = subtitle) + 
    theme_classic() +
    theme(legend.position = "none",
          axis.text.y = element_blank(), 
          axis.ticks.x.bottom = element_blank(), 
          axis.text.x.top = element_text(size = theme_axis_text_x_top_size, angle = 60, hjust = 0, color = "black", family = "Avenir Next"),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          plot.title.position = "plot",
          plot.subtitle = element_text(hjust = 0, size = 12, face = "bold"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() , 
          panel.border = element_blank()
          #panel.border = element_rect(colour = "black", fill=NA, size = 1),
          #axis.line.y.right = element_line(colour = "white", size = 2)
    ) 
  
  if (intersect_cutoff == 0 ){
    intersect_matrix_plot <- intersect_matrix_plot + 
      labs(y = "")
  } else if (intersect_cutoff == 1 ){
    intersect_matrix_plot <- intersect_matrix_plot + 
      labs(y = paste0("Only combinations with more than ", intersect_cutoff ," individual are shown."))
  } else if (intersect_cutoff > 1 ){
    intersect_matrix_plot <- intersect_matrix_plot + 
      labs(y = paste0("Only combinations with more than ", intersect_cutoff," individuals are shown."))
  } 
  
  
  intersect_size_plot <- 
    intersect_size %>% 
    mutate(!!enquo(cat_col):= fct_reorder(!!enquo(cat_col), n)) %>% 
    ggplot() +
    geom_col(aes(y = !!enquo(cat_col), x = prop_pct), fill = fill, width = 0.5) +
    geom_richtext(aes(y = !!enquo(cat_col) , x = prop_pct, label = countprop), 
                  size = 2.5, alpha = 0.85,  hjust = -0.1, label.color = NA,
                  label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_y_discrete(expand = expansion(add = c(0.75, 0.75))) +
    coord_cartesian(clip = "off") +
    labs(x = intersect_size_lab) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 8, family = "Avenir Next"),
      plot.title = element_blank(), 
      #panel.border = element_rect(colour = "black", fill=NA, size = 1),
      panel.border = element_blank(),
      axis.line.y.left = element_line(colour = "white", size = 2)
    ) 
  
  
  #out_plots <- wrap_plots(intersect_matrix_plot, intersect_size_plot, nrow =1, widths = c(0.3, 0.7))
  out_plots <- 
    cowplot::plot_grid(intersect_matrix_plot, intersect_size_plot, 
                       align = "h", 
                       nrow =1,rel_widths = c(0.3, 0.7))
          
  return(out_plots)
  
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




ggplot_adjPrevSensSpecCI <- function (prevCI, sensCI, specCI, N = 1000000, method = "hdi", 
                                      alpha = 0.05, doPlot = FALSE, prev = NULL, sens = NULL, 
                                      spec = NULL, ylim = NULL) {

  prevDist <- bootComb::getBetaFromCI(qLow = prevCI[1], qUpp = prevCI[2], 
                            alpha = alpha)
  sensDist <- bootComb::getBetaFromCI(qLow = sensCI[1], qUpp = sensCI[2], 
                            alpha = alpha)
  specDist <- bootComb::getBetaFromCI(qLow = specCI[1], qUpp = specCI[2], 
                            alpha = alpha)
  distList <- list(prevDist$r, sensDist$r, specDist$r)
  combFun <- function(pars) {
    bootComb::adjPrevSensSpec(prevEst = pars[[1]], sens = pars[[2]], 
                    spec = pars[[3]])
  }
  if (!is.null(prev) & !is.null(sens) & !is.null(spec)) {
    adjPrev <- bootComb::adjPrevSensSpec(prev, sens, spec)
  }
  adjPrevCI <- bootComb::bootComb(distList = distList, combFun = combFun, 
                        N = N, method = method, coverage = 1 - alpha, doPlot = FALSE, 
                        legPos = NULL, returnBootVals = TRUE, validRange = c(0, 
                                                                             1))
  
  ## build plot
  x <- seq(0, 1, length = 1000)
  yPrev <- dbeta(x, prevDist$pars[1], prevDist$pars[2])
  ySens <- dbeta(x, sensDist$pars[1], sensDist$pars[2])
  ySpec <- dbeta(x, specDist$pars[1], specDist$pars[2])
  ylim_max <- max(yPrev, ySens, ySpec)
  
  parameter_densities <- 
    data.frame(x, yPrev, ySens, ySpec) %>% 
    as_tibble() %>% 
    pivot_longer(cols = 2:4, names_prefix = "y") %>% 
    ggplot() + 
    geom_line(aes(x = x, y = value, color = name), size = 1.3) + 
    labs(y = "Density", x = "Probability parameter", 
         title = "") + 
    scale_color_manual(values = paletteer_d("ggsci::default_jco"), 
                       labels = c("Crude prevalence", "Sensitivity", "Specificity"), 
                       name = "")
  
  seroprev_estimate_densities <- 
  tibble(values = adjPrevCI$bootstrapValues) %>% 
    ggplot() + 
    geom_histogram(aes(x = values), bins = 100, fill = "dodgerblue2", 
                   color = "dodgerblue4") + 
    annotate("segment", 
             x = adjPrevCI$conf.int[1], xend = adjPrevCI$conf.int[1], 
             y = 0, yend = Inf, color = "black", linetype = "dashed", size = 1) +
    annotate("segment", 
             x = adjPrevCI$conf.int[2], xend = adjPrevCI$conf.int[2], 
             y = 0, yend = Inf, color = "black", linetype = "dashed", size = 1) + 
    annotate("segment", 
             x = adjPrev, xend = adjPrev, 
             y = 0, yend = Inf, color = "darkred", size = 1) + 
    labs(x = "Adjusted seroprevalence", 
         y = "Density", 
         title = "") + 
    annotate("segment", x = 0.7, xend = 0.75, y = 2000, yend = 2000,
             color = "darkred", size = 0.8) + 
    annotate("text", label = "Estimate", x = 0.79, y = 2000,  size = 3) + 
    annotate("segment", linetype = "dashed", x = 0.7, xend = 0.75, y = 1750, yend = 1750,
             color = "black", size = 0.8) + 
    annotate("text", label = "95% CI", x = 0.79, y = 1750,  size = 3) 
  
  
  output_plot <- 
    cowplot::plot_grid(parameter_densities, seroprev_estimate_densities, 
                       nrow = 2, labels = "AUTO")
  
  return(output_plot)
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














