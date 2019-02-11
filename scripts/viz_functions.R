wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}


plot_strengths_and_weaknesses <- function(count_data, by_question = TRUE,
                                          type = NULL, within_cat = NULL){
  
  ## clean up the questions
  ## sort them by best to worse and extract their scores
  questions <- count_data %>% group_by(variable) %>%
    summarize(average = sum(response * value)) %>% as.data.frame()
  
  question_rankings <- questions[order(questions$average, decreasing = TRUE),]
  ranked_questions <- question_rankings$variable
  question_scores <- question_rankings$average
  question_rankings$variable <- 1
  question_rankings$average <- question_rankings$average - 1 ## !!! just until i get the x axis shifted
  
  ## prep variables for plot title
  n_clear_strength <- sum(question_scores > 4.2)
  n_slight_strength <- sum((question_scores > 3.4)&(question_scores <= 4.2))
  n_neutral <- sum((question_scores > 2.6)&(question_scores <= 3.4))
  n_slight_weakness <- sum((question_scores > 1.8)&(question_scores <= 2.6))
  n_clear_weakness <- sum(question_scores <= 1.8)
  
  n <- length(ranked_questions)
  ncs <- paste(n_clear_strength, "clear agreement")
  nss <- paste(n_slight_strength, "slight agreement")
  neu <- paste(n_neutral, "neutral")
  nsw <- paste(n_slight_weakness, "slight disagreement")
  ncw <- paste(n_clear_weakness, "clear disagreement")
  
  #if (n_clear_strength == 1) {ncs <- substr(ncs,1,nchar(ncs)-1)}
  #if (n_slight_strength == 1) {nss <- substr(nss,1,nchar(nss)-1)}
  #if (n_neutral == 1) {neu <- substr(neu,1,nchar(neu)-1)}
  #if (n_slight_weakness == 1) {nsw <- substr(nsw,1,nchar(nsw)-2)}
  #if (n_clear_weakness == 1) {ncw <- substr(ncw,1,nchar(ncw)-2)}
  
  
  if (by_question) {
    title <- paste("The",var,"category is made up of", n,
                   "statements with the following distribution:\n",
                   ncw, ',', nsw, ',', neu, ',', nss, ',', ncs)
  } else {
    if (type == "categories") {
      line <- "are distributed as follows:\n"
      title <- paste("The",n, type, line,
                     ncw, ',', nsw, ',', neu, ',', nss, ',', ncs)
    } else {
      if (is.null(within_cat)){
        line <- "(including 'no response') are distributed as follows:\n"
        title <- paste("The",type, line,
                       ncw, ',', nsw, ',', neu, ',', nss, ',', ncs)
        
      } else {
        line <- "are distributed as follows:\n"
        title <- paste("Within the",within_cat,"category, the", type, line,
                       ncw, ',', nsw, ',', neu, ',', nss, ',', ncs)
      }
      
    }
    
  }
  
  
  ## create the frame for the plot
  frame <- data.frame(response = 1:5,
                      variable = rep('',5),
                      value = c(0.8, 0.8, 0.8, 0.8, 0.8))
  band_plot <- ggplot(frame)
  
  ## create the rest of the plot
  
  band_plot <- band_plot +
    geom_bar(aes(y = value,
                 x = variable,
                 fill= factor(response,levels = c(1,2,3,4,5))),
             stat = 'identity') +
    geom_point(data = question_rankings,
               aes(y = average, x = variable),
               shape=4, size= 5, alpha = 0.8, colour = 'black') +
    coord_flip() + theme_classic() +
    labs(title = title, y = NULL, x = NULL) + 
    theme(legend.title = element_blank(),
          legend.position="none",
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=10)) +
    scale_fill_manual(labels = c("strongly agree", "agree",
                                 "neutral",
                                 "disagree", "strongly disagree"),
                      values=c("#167a1c", "#6d9670", "#efefef", "#c17474", "#c11717")) +
    scale_y_continuous(breaks=c(0.4,1.2,2,2.8,3.6),
                       labels=c("clear\ndisagreement", "slight\ndisagreement",
                                "neutral",
                                "slight\nagreement", "clear\nagreement"),
                       limits = c(0,4))
  
  ## display the plot
  print(band_plot)
  
  ## output the ranked questions and ns
  list(ranked_questions,
       c(n_clear_strength, n_slight_strength, n_neutral, n_slight_weakness, n_clear_weakness))
  
  
}


melt_data <- function(count_data, col_cats, categories, var = NULL){
  if (!is.null(var)){
    cat_data <- count_data[,c(1,which(col_cats == var)+1)]
  } else {
    cat_data <- count_data
  }
  
  cat_data_melted <- reshape2::melt(cat_data, id = 'response')
  cat_data_melted$variable <- categories$question[match(cat_data_melted$variable,
                                                        categories$q_nums)]
  cat_data_melted
}

plot_histogram <- function(statement, count_data, error_data = NULL, var, ranked_questions, titleyn = TRUE){
  
  hist_data <- data.frame(variable = factor(c("strongly disagree", "disagree",
                                              "neutral",
                                              "agree", "strongly agree"),
                                            levels = c("strongly disagree", "disagree",
                                                       "neutral",
                                                       "agree", "strongly agree")),
                          value = count_data$value[count_data$variable == ranked_questions[statement]])
  
  if (titleyn) {
    title <- ranked_questions[statement]
  } else {
    title <- "no title"
  }
  
  
  histogram_plot <- ggplot(hist_data)
  histogram_plot <- histogram_plot +
    geom_bar(aes(y = value,
                 x = variable,
                 fill = variable),
             stat = 'identity')
  
  if (!is.null(error_data)) {
    histogram_plot <- histogram_plot +
      geom_segment(data = error_data,
                 aes(x = response, xend = response,
                     y = min, yend = max),
                 size = 0.5)
  }
  
  histogram_plot <- histogram_plot +
    labs(y = "Proportion of employees giving each response", x = NULL) + 
    coord_flip(ylim = c(0,sum(hist_data$value)))
  
  if (title != "no title"){
    histogram_plot <- histogram_plot + ggtitle(wrapper(title, width = 75))
  }
    
  histogram_plot <- histogram_plot + 
    theme(legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size=10,face="bold"),
          axis.title=element_text(size=10)) + 
    scale_fill_manual(labels = c("strongly disagree", "disagree",
                                 "neutral",
                                 "agree", "strongly agree"),
                      values=c("#c11717", "#c17474", "#bcbcbc", "#6d9670", "#167a1c"))
  histogram_plot
  
}

plot_histo_loop <- function(n, count_data, cat, ranked_questions, counter = 0, titleyn = TRUE) {
  res <- list()
  if (n > 0){
    for (i in 1:n){
      counter <- counter + 1
      res[[i]] <- plot_histogram(counter, count_data, NULL, cat, ranked_questions, titleyn)
    }
  }

  invisible(lapply(res, print))
  counter
}

plot_heatmap <- function(cat_means, row_var, row_var_as_row = FALSE, ylab = NULL, xlab = NULL) {
  
  ## prep the data
  #row_values <- cat_means[,row_var]
  
  melted_data <- reshape2::melt(cat_means, id = row_var)
  melted_data[,row_var] <- factor(melted_data[,row_var], levels = cat_means[,row_var])
  
  if (row_var_as_row) {
    x_var <- 'variable'; y_var <- row_var
  } else {
    x_var <- row_var; y_var <- 'variable'
  }
  
  
  ## create the plot
  p <- ggplot(melted_data, aes_string(x = x_var, y = y_var)) +
    geom_tile(aes(fill = value-3),colour = "white") + 
    geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient2(low = "#c11717",
                         mid = "#efefef",
                         high = "#167a1c",
                         na.value = "transparent",
                         breaks=c(2,1,0,-1,-2),
                         labels=c("strongly agree",
                                  "agree",
                                  "neutral",
                                  "disagree",
                                  "strongly disagree"),
                         limits=c(-2,2)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0)) +
    labs(y = ylab, x = xlab)  +
    guides(fill=guide_legend(title="Colour represents\naverage agreement\nwith statement")) +
    scale_x_discrete(position = "top") 
  
  
  p
  
}

plot_stacked_bars <- function(proportion_data, id = 'response', levels = 1:5,
                              title = NULL, xlab = NULL, ylab = NULL){
  
  melted_data <- reshape2::melt(proportion_data, id = id)
  melted_data[,id] <- factor(melted_data[,id],
                             levels)
  
  stacked_bars <- ggplot(melted_data)
  
  stacked_bars <- stacked_bars +
    geom_bar(aes_string(y = 'value',
                        x = 'variable',
                        fill = id),
             stat = 'identity') +
    coord_flip() +
    labs(title = title, y = ylab, x = xlab) + 
    theme(legend.title = element_blank()) +
    scale_fill_manual(labels = c("strongly agree", "agree",
                                 "neutral",
                                 "disagree", "strongly disagree"),
                      values=c("#167a1c", "#6d9670",
                               "#efefef",
                               "#c17474", "#c11717"))
  
  stacked_bars
  
}

plot_histogram_large <- function(count_data, question_data, var){
  
  hist_data <- data.frame(variable = factor(c("strongly disagree", "disagree",
                                              "neutral",
                                              "agree", "strongly agree"),
                                            levels = c("strongly disagree", "disagree",
                                                       "neutral",
                                                       "agree", "strongly agree")),
                          value = count_data[,var])
  
  title = paste("Average response profile across all statements in the", var,
                "category.\nLines represent the range for individual statements within the category.")
  
  histogram_plot <- ggplot(hist_data)
  histogram_plot <- histogram_plot +
    geom_bar(aes(y = value,
                 x = variable,
                 fill = variable),
             stat = 'identity') +
    geom_segment(data = question_data,
                 aes(x = response, xend = response,
                     y = min, yend = max),
                 size = 1) +
    labs(y = "Proportion of employees giving each response", x = 'Response') + 
    coord_cartesian(ylim = c(0,sum(hist_data$value))) +
    #ggtitle(wrapper(title, width = 90)) + 
    theme(legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size=10,face="bold"),
          axis.title=element_text(size=10)) + 
    scale_fill_manual(labels = c("strongly disagree", "disagree",
                                 "neutral",
                                 "agree", "strongly agree"),
                      values=c("#c11717", "#c17474", "#bcbcbc", "#6d9670", "#167a1c"))
  histogram_plot
  
}

plot_demog_bars <- function(tidy_data, demog, sort_order = NULL, default_sort = FALSE, cols = 1:107) {
  
  all_groups <- unique(tidy_data[,demog])
  
  df_counts_by_demog <- data.frame()
  for (group in all_groups) {
    df_demog_counts <- counts_from_tidy(tidy_data[tidy_data[,demog] == group,cols],
                                        question_columns = cols,
                                        proportions = FALSE)
    df_demog_counts[,demog] <- group
    df_counts_by_demog <- plyr::rbind.fill(df_counts_by_demog, df_demog_counts)
  }
  
  df_counts_all_by_demog <- data.frame(variable = df_counts_by_demog[,demog],
                                       response = df_counts_by_demog$response,
                                       value = df_counts_by_demog[,cols] %>% rowSums())
  df_counts_all_by_demog <- df_counts_all_by_demog %>% spread(key = variable, value = value)
  df_counts_all_by_demog <- sweep(df_counts_all_by_demog,2,colSums(df_counts_all_by_demog),`/`)
  df_counts_all_by_demog$response <- 1:5
  
  if (is.null(sort_order)) {
    values <- (df_counts_all_by_demog[,2:ncol(df_counts_all_by_demog)] * 1:5) %>% colSums()
    sort_order <- names(values)[order(values, decreasing = default_sort)]
  }
  
  df_counts_all_by_demog <- sort_columns(df_counts_all_by_demog,
                                         2:(length(sort_order)+1), sort_order)
  
  df_counts_all_by_demog
}