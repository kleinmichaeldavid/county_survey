

category_means_from_tidy <- function(tidy_data, categories, keep_column) {
  
  ## find unique categories and match each column to a cat
  column_cats <- categories$category[match(names(tidy_data), categories$q_nums)]
  unique_cats <- unique(categories$category)
  
  ## init new dataframe
  df_emp_summary <- data.frame(employee = 1:nrow(tidy_data),
                               group = tidy_data[,keep_column])
  
  for (cat in unique_cats) {
    df_emp_summary[,cat] <- tidy_data[,which(column_cats == cat)] %>% rowMeans(na.rm = T)
  }
  
  df_emp_summary
}

group_means_for_categories <- function(cat_data, keep_column) {
  
  df_summary <- cat_data %>% group_by_(keep_column) %>%
    summarize(count = n(),
              Recognition = mean(Recognition),
              `Conflict Resolution` = mean(`Conflict Resolution`),
              Culture = mean(Culture),
              `Work / Life Balance` = mean(`Work / Life Balance`),
              `Leadership (SMT)` = mean(`Leadership (SMT)`),
              `Service Commitment` = mean(`Service Commitment`),
              `Meaningful Work & Purpose` = mean(`Meaningful Work & Purpose`),
              `Teamwork` = mean(`Teamwork`),
              `Learning & Development` = mean(`Learning & Development`),
              `Communication` = mean(`Communication`),
              `Management Effectiveness` = mean(`Management Effectiveness`),
              `Conduct / Ethics` = mean(`Conduct / Ethics`),
              `Health & Safety` = mean(`Health & Safety`),
              `Resources / Support` = mean(`Resources / Support`),
              `Pay & Benefits` = mean(`Pay & Benefits`)) %>%
    as.data.frame()
  df_summary[,keep_column] <- as.character(df_summary[,keep_column])
  df_summary[,keep_column][df_summary[,keep_column] == ""] <- "<no response>"
  df_summary
  
}


determine_column_order <- function(summary_data, columns, decreasing = FALSE){
  names(summary_data[,columns][,order(colMeans(summary_data[,columns]), decreasing = decreasing)])
}

determine_row_order <- function(summary_data, name_column, compute_columns, decreasing = FALSE){
  summary_data[order(rowMeans(summary_data[,compute_columns]), decreasing = decreasing),name_column] %>%
    as.character()
}

sort_columns <- function(data, columns, sort_order) {
  
  other_columns <- (1:ncol(data))[!(1:ncol(data) %in% columns)]
  
  new_order <- match(sort_order,names(data)[columns])
  
  sorted_data <- cbind(select(data,other_columns),#data[,other_columns],
                       data[,columns][,new_order])
  sorted_data
}

sort_rows <- function(data, sort_on, sort_order) {
  
  new_order <- match(sort_order,data[,sort_on])
  
  sorted_data <- data[new_order,]
  
  sorted_data
}

question_counts_to_categories <- function(count_data, categories, resp_col = 'response', proportions = TRUE) {
  
  ## remove response col
  #df_responses <- select(count_data, resp_col)
  df_questions <- count_data[,names(count_data) != resp_col]
  
  ## find unique categories and match each column to a cat
  column_cats <- categories$category[match(names(df_questions), categories$q_nums)]
  unique_cats <- unique(categories$category)
  
  ## init the new df
  df_count_cats <- data.frame(response = 1:5)
  
  ## add the category columns one by one
  for (cat in unique_cats) {
    df_count_cats[,cat] <- df_questions[,which(column_cats == cat)] %>% rowSums(na.rm = T) ## could do rowMeans instead...
  }
  
  ## convert to proportions, potentially
  if (proportions) {
    df_count_cats <- sweep(df_count_cats,2,colSums(df_count_cats),`/`)
    df_count_cats$response <- 1:5
  }
  
  ## output
  df_count_cats
  
}

counts_from_tidy <- function(tidy_data, question_columns = 1:107, proportions = TRUE) {
  
  ## init df for counts
  df_counts <- data.frame(response = c(1,2,3,4,5))
  
  ## fill question by question
  for (c in question_columns) {
    name <- names(tidy_data)[c]
    counts <- plyr::count(tidy_data, c)
    matches <- match(counts[,name], df_counts$response)
    matches <- matches[!is.na(matches)]
    df_counts[matches,name] <- counts$freq[1:length(matches)]
  }
  df_counts[is.na(df_counts)] <- 0
  
  ## convert to proportions, potentially
  if (proportions) {
    df_counts <- sweep(df_counts,2,colSums(df_counts),`/`)
    df_counts$response <- 1:5
  }
  
  df_counts
}


