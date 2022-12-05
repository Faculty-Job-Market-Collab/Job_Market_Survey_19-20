

#calculate two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 1)
  return(percent)
}

`%not_in%` <- Negate(`%in%`)

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#plotting functions----
get_plot_summary <- function(data, x, y){ #x and y must be provided in quotations
  
  df <- data %>% 
    select(!!sym(x), !!sym(y), id) %>% #!!sym() allows tidyverse functions to evaluate x and y as column names
    group_by(!!sym(x), !!sym(y)) %>% 
    summarise(n=n()) %>% 
    spread(key = !!sym(y), value = n) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% #replace na values w/ 0 to allow percent calculations
    mutate(percent_res = get_percent(true, false),
           n = true+false,
           r = paste0("r=", true)) %>% 
    mutate('{x}' := paste0(!!sym(x), " (n=", n, ")"))#add n of each group to the group name; '{}' := allows mutate to evaluate the variable x as a column
  
  return(df)
}


#for data dump---
plot_raw_data <- function(df, s, p, f){
  
  df <- df %>% 
    select(id, section, question, response, !!sym(f)) %>% 
    filter(section == s) %>% 
    filter(question == p) %>% 
    filter(!is.na(response)) %>% distinct() %>% 
    count(!!sym(f), response) %>% 
    mutate(percent = get_percent(n, sum(n)))
  
  sum_n <- sum(df$n)
  
  plot <- if(nrow(df)== 0){NULL}else{
    if(is.null(f)){
      ggplot(df)+
        geom_col(aes_string(x = "response", y = "percent"))+
        coord_flip()+
        scale_x_discrete(drop = FALSE)+
        labs(title = paste0("% of ", sum_n, " respondents: question = ", p))+
        theme(legend.position = "bottom")
    }else{
      ggplot(df)+
        geom_col(aes_string(x = "response", y = "percent", fill = f))+
        coord_flip()+
        scale_x_discrete(drop = FALSE)+
        labs(title = paste0("% of ", sum_n, " respondents: question = ", p))+
        theme(legend.position = "bottom")
    }}
  
  return(plot)
}

#for social science 

get_wilcox_tbl <- function(x, y){ 
  x <- get(x)
  
  x %>% #wilcox test output
    unlist() %>% 
    as_tibble(rownames = "attribute") %>% 
    add_column(figure = y)
}

get_pair_wilcox_tbl <- function(x, y){ 
  x <- get(x)
  
  raw <- x %>% #wilcox test output
    unlist() %>% 
    as_tibble(rownames = "attribute") %>% 
    add_column(figure = y)
  
  pval <- dimnames(x$p.value)
}

save_wilcox <- function(wilcox, file){
  
  mw_p <- wilcox
  
  mw_val <- unlist(mw_p) %>% as_tibble_col()
  
  mw_key <- names(mw_p) %>% as_tibble_col(column_name = "key")
  
  mw_tbl <- cbind(mw_key, mw_val)
  
  write_csv(mw_tbl, file)
  
  print("save csv")
}

plot_mwu_bar_data <- function(df, q, f, f_text, mwu, l, bin){
  
  df_test <- df %>% 
    filter(question == q) %>% distinct()
  
  print(head(df))
  
  q_bin <- if(bin == "yes"){
    print("bin")
    paste0(q, "_binned")
  }else{NULL}
  
  print(q_bin)
  
  df_bin <- if(bin == "yes"){
    df %>% filter(question == q_bin) %>% distinct()
  }else{NULL}
  
  print(head(df_bin))
  
  q_x <- str_replace_all(q, "_", " ")
  
  print(q_x)
  
  l_long <- case_when(
    l == "small" ~ "bin_levels_small",
    l == "big" ~ "bin_levels_big")
  
  l_long <- if(!is.na(l_long)){
    get(l_long)
  }else{NULL}
  
  mwu_tbl <- if(mwu == "yes"){
    print(f)
    
    resp <- df_test[["response"]]
    
    f_col <- df_test[[f]]
    
    if(f == "simple_gender"){
      
      f_col <- f_col %not_in% "No Response"
    }
    
    wilcox.test(as.numeric(resp) ~ f_col,
                        na.rm=TRUE, paired=FALSE, 
                        exact=FALSE, conf.int=TRUE)
  }else{NULL}
  
  plot <- if(nrow(df) == 0){NULL}else{
    if(l == "none"){
      
      base <- ggplot(data = df_test, 
                     aes_string(x="response", fill=f))+
        geom_bar(position = "dodge")
      
      if(mwu == "yes"){
        print("1")
        
        base +
        coord_flip() +
        labs(y="Number of responses", x=paste0("Number of ", q_x),
             fill = f_text, 
             subtitle = paste0("Mann Whitney U p-value = ", 
                              round(mwu_tbl[[3]], digits = 3)))
      
        }else{
          print("2")
          
          base + 
            coord_flip()+
            labs(y="Number of responses", x=paste0("Number of ", q_x),
            fill = f_text)
             }
    }else{
      
      base <- ggplot(data = df_bin, aes_string(x="factor(
          response, levels = l_long, labels = l_long)", fill=f))+
        geom_bar(position = "dodge")+
        scale_fill_discrete(drop=FALSE) +
        scale_x_discrete(drop=FALSE)+
        coord_flip()
      
      if(mwu == "yes"){
        print("3")
        
        base + 
          labs(y="Number of responses", #x=paste0("Number of ", q_x),
               fill = f_text, 
               subtitle = paste0("Mann Whitney U p-value = ",
                                round(mwu_tbl[[3]], digits = 3)))
      }else{
        print("4")
        
        base + 
          labs(y="Number of responses", x=paste0("Number of ", q_x),
               fill = f_text)
      }
      }}
  
  return(plot)
}

plot_mwu_box_data <- function(df, q, f, f_text, mwu){
  
  df_test <- df %>% 
    filter(question == q) %>% distinct()
  
  print(head(df))
  
  q_x <- str_replace_all(q, "_", " ")
  
  print(q_x)

  mwu_tbl <- if(mwu == "yes"){
    print(f)
    
    resp <- df_test[["response"]]
    
    f_col <- df_test[[f]]
    
    if(f == "simple_gender"){
      
      f_col <- f_col %not_in% "No Response"
    }
    
    wilcox.test(as.numeric(resp) ~ f_col,
                na.rm=TRUE, paired=FALSE, 
                exact=FALSE, conf.int=TRUE)
  }else{NULL}
  
  plot <- if(nrow(df) == 0){NULL}else{
    base <- ggplot(data = df_test, 
                   aes_string(x="as.numeric(response)", y = f, fill=f))+
      geom_boxplot()+
      coord_flip()
      
      if(mwu == "yes"){
        base +
          labs(y=f_text, x=paste0("Number of ", q_x),
               subtitle = paste0("Mann Whitney U p-value = ", 
                                round(mwu_tbl[[3]], digits = 3)))
        
      }else{
        base + 
          labs(y=f_text, x=paste0("Number of ", q_x))
      }
    }
  
  return(plot)
}

#functions to bin responses----
get_small_bins <- function(x){case_when(
  x >= 300 ~ "300+",
  x >= 200 ~ "200-299",
  x >= 100 ~ "100-199",
  x >= 50 ~ "50-99",
  x >= 40  ~ "40-49",
  x >= 30 ~ "30-39",
  x >= 20 ~ "20-29",
  x >= 15 ~ "15-19",
  x >=10 ~ "10-14",
  x >=5 ~ "5-9",
  TRUE ~ as.character(x)
)}

get_big_bins <- function(x){case_when(
  x >= 4000 ~ "4000+",
  x >= 3000 ~ "3000-3999",
  x >= 2000 ~ "2000-2999",
  x >= 1500 ~ "1500-1999",
  x >= 1000 ~ "1000-1499",
  x >= 500 ~ "500-999",
  x >= 400 ~ "400-499",
  x >= 300 ~ "300-399",
  x >= 200 ~ "200-299",
  x >= 150 ~ "150-199",
  x >= 100 ~ "100-149",
  x >= 50 ~ "50-99",
  x >= 20 ~ "20-49",
  x >= 10 ~ "10-19",
  x >=1 ~ "< 10",
  TRUE ~ as.character(x)
)}

UniquePanelCoords <- ggplot2::ggproto(
  "UniquePanelCoords", ggplot2::CoordCartesian,
  
  num_of_panels = 1,
  panel_counter = 1,
  layout = NULL,
  
  setup_layout = function(self, layout, params) {
    self$num_of_panels <- length(unique(layout$PANEL))
    self$panel_counter <- 1
    self$layout <- layout # store for later
    layout
  },
  
  setup_panel_params =  function(self, scale_x, scale_y, params = list()) {
    train_cartesian <- function(scale, limits, name, given_range = c(NA, NA)) {
      if (anyNA(given_range)) {
        expansion <- ggplot2:::default_expansion(scale, expand = self$expand)
        range <- ggplot2:::expand_limits_scale(scale, expansion, coord_limits = limits)
        isna <- is.na(given_range)
        given_range[isna] <- range[isna]
      }
      out <- list(
        ggplot2:::view_scale_primary(scale, limits, given_range),
        sec = ggplot2:::view_scale_secondary(scale, limits, given_range),
        arrange = scale$axis_order(),
        range = given_range
      )
      names(out) <- c(name, paste0(name, ".", names(out)[-1]))
      out
    }
    
    this_layout <- self$layout[ self$panel_counter,, drop = FALSE ]
    self$panel_counter <- 
      if (self$panel_counter < self$num_of_panels) {
        self$panel_counter + 1
      } else 1
    
    # determine merge column names by removing all "standard" names
    layout_names <- setdiff(names(this_layout),
                            c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
    limits_names <- setdiff(names(self$panel_limits),
                            c("xmin", "xmax", "ymin", "ymax"))
    
    limit_extras <- setdiff(limits_names, layout_names)
    if (length(limit_extras) > 0) {
      stop("facet names in 'panel_limits' not found in 'layout': ",
           paste(sQuote(limit_extras), collapse = ","))
    } else if (length(limits_names) == 0 && NROW(self$panel_limits) == 1) {
      # no panels in 'panel_limits'
      this_panel_limits <- cbind(this_layout, self$panel_limits)
    } else {
      this_panel_limits <- merge(this_layout, self$panel_limits, all.x = TRUE, by = limits_names)
    }
    
    if (isTRUE(NROW(this_panel_limits) > 1)) {
      stop("multiple matches for current panel in 'panel_limits'")
    }
    
    # add missing min/max columns, default to "no override" (NA)
    this_panel_limits[, setdiff(c("xmin", "xmax", "ymin", "ymax"),
                                names(this_panel_limits)) ] <- NA
    
    c(train_cartesian(scale_x, self$limits$x, "x",
                      unlist(this_panel_limits[, c("xmin", "xmax"), drop = TRUE])),
      train_cartesian(scale_y, self$limits$y, "y",
                      unlist(this_panel_limits[, c("ymin", "ymax"), drop = TRUE])))
  }
)

coord_cartesian_panels <- function(panel_limits, expand = TRUE, default = FALSE, clip = "on") {
  ggplot2::ggproto(NULL, UniquePanelCoords,
                   panel_limits = panel_limits,
                   expand = expand, default = default, clip = clip)
}