###########################################
# set directory
path <- "D:/Desktop/108112_Source"

###########################################

##### (1) Importing packages
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(rio)))        # export()
suppressMessages(suppressWarnings(library(dtwclust)))   # dtw_lb()
suppressMessages(suppressWarnings(library(caret)))      # createFolds()
suppressMessages(suppressWarnings(library(parallel)))   # Parlapply()

##### (2) Loading data
### (2-1)Training data
# Retrieve Training data label
setwd(path)
training_data <- read.csv("Merged_data.csv")
fold_index <- training_data[,"fold"]
True_label <- as.character(fold_index)

# Format Training data into list
list_training_data <- function(){
  #Data directory
  root <- paste0(path, "/comp_data_csv/")
  setwd(root)
  fold_name <- list.files(pattern="G*")
  working_directory <- paste(root,fold_name,"/", sep ="")
  # Retrive datasets into list
  data_list <- list()
  for( i in seq_len(length(working_directory))){
    wdsep <- working_directory[i]
    setwd(wdsep)
    fold_name <- list.files(pattern="G*")
    #file
    for ( j in seq_len(length(fold_name))){
      file <- fold_name[j]
      file_read <-  read.csv(file)
      data_list[[file]] <- file_read
    }
  }
  # Treatment 
  settle <- function(df){
    df <- df[-1]
    names(df) <- NULL
    data_list_tidy <- as.list(df)
    return(data_list_tidy)
  }
  data_list_tidy <- sapply(data_list, settle)
  # Organize
  training_data_list <- list()
  for( i in seq_len(length(data_list_tidy))){
    item_num <- length(data_list_tidy[[i]])
    for( j in seq_len(item_num)){
      training_data_list <- c(training_data_list, data_list_tidy[[i]][j])
    }
  }
  cat("process completed - training dataset \n")
  return(training_data_list)
}
training_data_list <- list_training_data()

# Reinterpolate training data list  to same length
training_data_list_new <- reinterpolate(training_data_list, new.length = max(lengths(training_data_list)))

### (2-2) Testing data
# Format Testing data into list
list_testing_data <- function(){
  #Data directory
  root <- paste0(path, "/testing_csv/")
  setwd(root)
  file_name <- list.files(pattern=".csv")
  file_dire <- paste(root,file_name, sep ="")
  name_list <- unlist(strsplit(file_name, ".csv"))
  # Retrive datasets into list
  data_list <- list()
  for( i in seq_len(length(file_dire))){
    file <- file_dire[i]
    file_read <-  read.csv(file)
    data_list[[name_list[i]]] <- file_read
  }
  # Treatment 
  settle <- function(df){
    df <- df[-1]
    names(df) <- NULL
    data_list_tidy <- as.list(df)
    return(data_list_tidy)
  }
  data_list_tidy <- sapply(data_list, settle)
  # Organize
  testing_data_list <- list()
  for( i in seq_len(length(data_list_tidy))){
    item_num <- length(data_list_tidy[[i]])
    for( j in seq_len(item_num)){
      testing_data_list <- c(testing_data_list, data_list_tidy[[i]][j])
    }
    name_index <- sapply(data_list, function(x) (dim(x)[2]-1))
    file_name <- rep(name_list, name_index)
  }
  cat("process completed - testing dataset \n")
  return(list(testing_data_list = testing_data_list, file_name = file_name) )
}
testing_data <-  list_testing_data()
testing_data_list <- testing_data$testing_data_list
testing_data_file_name <- testing_data$file_name

### Reinterpolate testig data to same length(equal to training data length)
testing_data_list_new <- reinterpolate(testing_data_list, new.length = max(lengths(training_data_list)))

##### (3) Main function
### (3-1) KNN 
classify_series <- function(query, database, NN = 1, Filter = T, window_size = 30L) {
  # Similarity
  similarity <- dtw_lb(database, query, window.size = window_size,nn.margin = 2L)
  similarity_sorted_index <- order(similarity)
  # Nearest neighbor
  Nearest_index <-similarity_sorted_index[1:NN]
  index <- as.character(fold_index)
  Nearest_Nei_label <- index[Nearest_index]
  # Find the Nearest element
  Nearest_label <- sort(table(Nearest_Nei_label), decreasing = T)[1]
  niche <- ceiling(NN/2)
  Check <- Nearest_label >= niche
  if(Check){
    final_label <- names(Nearest_label)
  }else{
    final_label <- "pending"
  }
  
  if(Filter){
    return(final_label)
  }else{
    return( list(Nearest_Neighbor_set = Nearest_Nei_label,  Nearest_element = final_label) )
  }
  
}

##### (4)Model 
# Model 1 
NN <- 1; window_size <- 30L
formals(classify_series)[c("NN", "window_size")] <- list(NN = NN, window_size = window_size)
Model1 <- classify_series

# Model 2 
NN <- 5; window_size <- 10L
formals(classify_series)[c("NN", "window_size")] <- list(NN = NN, window_size = window_size)
Model2 <- classify_series

# Model 3
NN <- 3; window_size <- 0L
formals(classify_series)[c("NN", "window_size")] <- list(NN = NN, window_size = window_size)
Model3 <- classify_series

##### (5) Prediction
# Variable
Model <- list(Model1, Model2, Model3)
Prediction_func <- function(func,data){
  Result <- rep(NA, length(data))
  for( i in seq_len(length(Result))){
    Result[i] <- func(query = data[[i]], database =  training_data_list_new)
  }
  return(Result)
}
Prediction_list <- lapply(Model, function(x) Prediction_func(func = x, data = testing_data_list_new) )

Prediction_df <- data.frame(matrix(unlist(Prediction_list), nrow = length(Prediction_list[[1]])))
colnames(Prediction_df) <-c("Model1", "Model2", "Model3") 
cat("process completed - prediction \n")

Result_df <- cbind(Prediction_df, file = testing_data_file_name)

# Votting
vote <- apply(Prediction_df, 1, function(x) names(which.max(table(x))) )
vote_df <- data.frame(file = as.numeric(testing_data_file_name), Prediction = vote)

# Non-concord index
nc_index <- Prediction_df[which(apply(Prediction_df, 1, function(x) length(table(x)) >1 )),] %>% rownames() %>%as.numeric()
nc_index_file <- Result_df[nc_index,] %>% pull(file) 

##### Final decision
Split_df <- split(vote_df, vote_df$file)

#Decision function
Prediction_result <- sapply(Split_df, function(x) names(which.max(table(x[,"Prediction"]))))

##### Export result
result_data <- data.frame(file_name = names(Prediction_result),
                          Prediction = Prediction_result)
# export location
export(x = result_data,
       file = paste0(path, "/108112_TestResult.xlsx"))
cat("process completed - output to xlsx \n")
