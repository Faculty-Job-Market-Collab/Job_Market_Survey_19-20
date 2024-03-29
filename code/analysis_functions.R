

#calculate a two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100 #ensure values are coerced to numeric
  percent <- round(z, digits = 2) #ensure no more than 2 decimal places
  return(percent)
}

#correllary to %in% function
`%not_in%` <- Negate(`%in%`)

#identify the mode for a single metric from a tidy dataset
get_mode <- function(data, metric){
  
  print(metric)
  
  df <- get(data) %>% #pull in data and filter for desired metric
    filter(question == metric) %>% 
    distinct()
  
  calc_max <- df %>% count(question, response) %>% #count n of each response
    pull(n) %>% unlist() %>% 
    max()#find highest n value
  
  calc_mode <- df %>% count(question, response) %>% #retain question column & count n of each response
    arrange(desc(n)) %>% filter(n == calc_max) #pull rows with ns that match the max
  
  mode_df <- calc_mode %>% 
    rowid_to_column() %>% #create identifiers for multiple modes to allow spread()
    rename(mode_value = response, mode_n = n)
  
  return(mode_df)
}


#calculate outliers using mad
get_mad_outliers <- function(data, col_name){ #place dataset and column name in quotes
  
  df <- get(data) %>% 
    select(!!sym(col_name)) %>% 
    mutate('{col_name}' := as.numeric(!!sym(col_name)))
  
  calc_sd <- sd(df[[deparse(ensym(col_name))]], na.rm = TRUE)
  
  calc_med <- median(df[[deparse(ensym(col_name))]], na.rm = TRUE) #calculate column median
  
  abs_dev_df <- df %>% 
    mutate(abs_dev = abs(!!sym(col_name) - as.numeric(calc_med)))
  
  est_c <- calc_sd/(median(abs_dev_df$abs_dev, na.rm = TRUE)) #estimate mad constant
  
  calc_mad <- round(est_c * median(abs_dev_df$abs_dev, na.rm = TRUE))
  
  calc_max <- round(as.numeric(calc_med)+(3*as.numeric(calc_mad)))
  
  mad_df <- tibble(
    data = col_name,
    mad = calc_mad,
    max = calc_max
  )
  
  return(mad_df)
}

is_mad_outlier <- function(mad_data, x, y){
  
  data <- get(mad_data)
  
  paper_max <- data[[1,3]]
  abstract_max <- data[[2,3]]
  corres_max <- data[[3,3]]
  first_max <- data[[4,3]]
  citation_max <- data[[5,3]]
  hindex_max <- data[[6,3]]
  apps_max <- data[[7,3]]
    
  output <- case_when(
    y == "peer-reviewed_papers" & as.numeric(x) > paper_max ~ "yes",
    y == "conference_abstracts" & as.numeric(x) > abstract_max ~ "yes",
    y == "corresponding_author" & as.numeric(x) > corres_max ~ "yes",
    y == "first_author" & as.numeric(x) > first_max ~ "yes",
    y == "scholar_citations_all" & as.numeric(x) > citation_max ~ "yes",
    y == "scholar_hindex" & as.numeric(x) > hindex_max ~ "yes",
    y == "apps_submitted" & as.numeric(x) > apps_max ~ "yes",
    TRUE ~ "no"
  )
  
  return(output)
}

#identify outlier values, returns binary response
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#plotting functions----
#calculate percent respondents and generate basic bar plot given standard tidy dataset
plot_perc_resp <- function(data, total){ #total is not in quotations
  data %>% 
    count(response) %>% 
    mutate(percent = get_percent(n, total)) %>% 
    ggplot(aes(x=response, y=percent))+
    geom_col(position = "dodge")+
    scale_y_continuous(expand = c(0,0))+ #ensure bars directly abut axis, no padding
    labs(y="Respondents (%)")
}

#generate data set calculating the group-based percent of variables with binary values
get_plot_summary <- function(data, x, y, binary){ #x and y must be provided in quotations
  
  df <- data %>% 
    select(!!sym(x), !!sym(y), id) %>% #!!sym() allows tidyverse functions to evaluate x and y as column names
    distinct() 
  
  calc_perc <- if(binary==TRUE){
    df %>% 
      count(!!sym(x), !!sym(y)) %>% 
      spread(key = !!sym(y), value = n) %>% 
      mutate_all(~replace(., is.na(.), 0)) %>%  #replace na values w/ 0 to allow percent calculations
      mutate(percent_res = get_percent(true, false),
           n = true+false#, #row-based total
           #r = paste0("r=", true) #added during covid analysis of rescinded offers
    ) %>% 
      mutate('{x}' := paste0(!!sym(x), " (n=", n, ")"))#add n of each group to the group name; '{}' := allows mutate to evaluate the variable x as a column
    
    }else{
      group_count <- df %>% count(!!sym(x)) %>% 
        rename("total" = n)
      
     df %>% 
       count(!!sym(x), !!sym(y)) %>% 
       left_join(., group_count, by = x) %>% 
       mutate(percent = get_percent(n, total))
    }
  
  return(calc_perc)
}


#for data dump---
#calculate grouped percent and generate minimal bar plot
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

#convert wilcox test output to tbl and save as csv
save_wilcox <- function(wilcox, file){
  
  mw_p <- wilcox #pull p-values from wilcox test
  
  mw_val <- unlist(mw_p) %>% 
    as_tibble_col() #convert test output to tibble
  
  mw_key <- names(mw_p) %>% 
    as_tibble_col(column_name = "key") #ensure values are named
  
  mw_tbl <- cbind(mw_key, mw_val) #generate final table
  
  write_csv(mw_tbl, file)
  
  print("save csv")
}

#filter dataset, calculate wilcox, and return barplot
plot_mwu_bar_data <- function(df, q, f, f_text, mwu, l, bin){
  
  df_test <- df %>% filter(question == q) %>% distinct()
  
  print(head(df)) #verify dataset
  
  q_bin <- if(bin == "yes"){#identify name of binned column
    print("bin")
    paste0(q, "_binned") 
  }else{NULL}
  
  print(q_bin) #verify col name
  
  df_bin <- if(bin == "yes"){#filter dataset for binned col
    df %>% 
      filter(question == q_bin) %>% 
      distinct() 
  }else{NULL}
  
  print(head(df_bin)) #verify binned dataset or none
  
  q_x <- str_replace_all(q, "_", " ") #generate value for x-axis title
  
  print(q_x) #verify x-axis
  
  l_long <- case_when( #identify which bin was used
    l == "small" ~ "bin_levels_small",
    l == "big" ~ "bin_levels_big")
  
  l_long <- if(!is.na(l_long)){ #pull in bin levels
    get(l_long)
  }else{NULL}
  
  mwu_tbl <- if(mwu == "yes"){ #conduct/return wilcox test
    print(f)
    
    resp <- df_test[["response"]] #get column with data values
    
    f_col <- df_test[[f]] #get column with comparison variables
    
    if(f == "simple_gender"){ 
      
      f_col <- f_col %not_in% "No Response" #filter out "no resp" values for simple_gender
    }
    
    wilcox.test(as.numeric(resp) ~ f_col,#run wilcox test
                        na.rm=TRUE, paired=FALSE, 
                        exact=FALSE, conf.int=TRUE)
  }else{NULL}
  
  plot <- if(nrow(df) == 0){NULL}else{ #check against empty datasets & generate plot
    if(l == "none"){ #get plot without bin levels
      
      base <- ggplot(data = df_test, 
                     aes_string(x="response", fill=f))+
        geom_bar(position = "dodge")
      
      if(mwu == "yes"){ #add wilcox test results
        
        base +
        coord_flip() +
        labs(y="Number of responses", x=paste0("Number of ", q_x),
             fill = f_text, 
             subtitle = paste0("Mann Whitney U p-value = ", 
                              round(mwu_tbl[[3]], digits = 3)))
      
        }else{ #complete plot w/o wilcox
          
          base + 
            coord_flip()+
            labs(y="Number of responses", x=paste0("Number of ", q_x),
            fill = f_text)
             }
    }else{ #pull bin levels and use to generate bar plot
      
      base <- ggplot(data = df_bin, aes_string(x="factor(
          response, levels = l_long, labels = l_long)", fill=f))+
        geom_bar(position = "dodge")+
        scale_fill_discrete(drop=FALSE) +
        scale_x_discrete(drop=FALSE)+
        coord_flip()
      
      if(mwu == "yes"){ #add wilcox test results
        
        base + 
          labs(y="Number of responses", #x=paste0("Number of ", q_x),
               fill = f_text, 
               subtitle = paste0("Mann Whitney U p-value = ",
                                round(mwu_tbl[[3]], digits = 3)))
      }else{ #complete plot w/o wilcox results
        
        base + 
          labs(y="Number of responses", x=paste0("Number of ", q_x),
               fill = f_text)
      }
      }}
  
  return(plot)
}

#filter dataset, calculate wilcox, and return boxplot
plot_mwu_box_data <- function(df, q, f, f_text, mwu){
  
  df_test <- df %>% #filter dataset
    filter(question == q) %>% distinct()
  
  print(head(df)) #verify dataset
  
  q_x <- str_replace_all(q, "_", " ") #generate x-axis title
  
  print(q_x) #verify x-axis title

  mwu_tbl <- if(mwu == "yes"){ #conduct wilcox test
    print(f)
    
    resp <- df_test[["response"]] #get numerical values
    
    f_col <- df_test[[f]] #get factored variable
    
    if(f == "simple_gender"){ #drop "No resp" values
      
      f_col <- f_col %not_in% "No Response"
    }
    
    wilcox.test(as.numeric(resp) ~ f_col,
                na.rm=TRUE, paired=FALSE, 
                exact=FALSE, conf.int=TRUE)
  }else{NULL}
  
  plot <- if(nrow(df) == 0){NULL}else{ #check against empty df and generate plot
    base <- ggplot(data = df_test, 
                   aes_string(x="as.numeric(response)", y = f, fill=f))+
      geom_boxplot()+
      coord_flip()
      
      if(mwu == "yes"){ #add wilcox test restults
        base +
          labs(y=f_text, x=paste0("Number of ", q_x),
               subtitle = paste0("Mann Whitney U p-value = ", 
                                round(mwu_tbl[[3]], digits = 3)))
        
      }else{ #complete plots w/o wilcox
        base + 
          labs(y=f_text, x=paste0("Number of ", q_x))
      }
    }
  
  return(plot)
}

#functions to bin responses----

#set bin levels
bin_levels_small <- c("0", "1", "2", "3", "4", "5-9", "10-14", "15-19", 
                      "20-29", "30-39", "40-49", "50-99", "100-199", 
                      "200-299", "300+")

bin_levels_big <- c("< 10", "10-19", "20-49", "50-99", "100-149", 
                    "150-199", "200-299", "300-399", "400-499", "500-999", 
                    "1000-1499", "1500-1999", "2000-2999", 
                    "3000-3999", "4000+")

get_small_bins <- function(x){case_when(
  as.numeric(x) >= 300 ~ "300+",
  as.numeric(x) >= 200 ~ "200-299",
  as.numeric(x) >= 100 ~ "100-199",
  as.numeric(x) >= 50 ~ "50-99",
  as.numeric(x) >= 40  ~ "40-49",
  as.numeric(x) >= 30 ~ "30-39",
  as.numeric(x) >= 20 ~ "20-29",
  as.numeric(x) >= 15 ~ "15-19",
  as.numeric(x) >=10 ~ "10-14",
  as.numeric(x) >=5 ~ "5-9",
  TRUE ~ as.character(x)
)}

get_big_bins <- function(x){case_when(
  as.numeric(x) >= 4000 ~ "4000+",
  as.numeric(x) >= 3000 ~ "3000-3999",
  as.numeric(x) >= 2000 ~ "2000-2999",
  as.numeric(x) >= 1500 ~ "1500-1999",
  as.numeric(x) >= 1000 ~ "1000-1499",
  as.numeric(x) >= 500 ~ "500-999",
  as.numeric(x) >= 400 ~ "400-499",
  as.numeric(x) >= 300 ~ "300-399",
  as.numeric(x) >= 200 ~ "200-299",
  as.numeric(x) >= 150 ~ "150-199",
  as.numeric(x) >= 100 ~ "100-149",
  as.numeric(x) >= 50 ~ "50-99",
  as.numeric(x) >= 20 ~ "20-49",
  as.numeric(x) >= 10 ~ "10-19",
  as.numeric(x) >=1 ~ "< 10",
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