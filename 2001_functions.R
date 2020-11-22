#### Helper Functions to read and process 2001 census data ####

#### FUNCTION 1: clean_data_2001 ####
# helper function to do initial data cleaning for get_2001 function
clean_data_2001 <- function(data){
    # Get rid of col 1 because it's useless
    data <- data[, 2:ncol(data)]
    
    # Clean column names
    names(data) <- as.character(1:ncol(data))
    
    # Get rid of initial rows that have NA's
    start_index <- grep("AREA", pull(data, 1))[1]
    return(data[start_index:nrow(data),])
}

#### FUNCTION 2: get_2001 ####
# more robust function to obtain each municipalities data and save to a data frame matched with ID
get_2001 <- function(data){
  
  # check to see if you have clean data or original data, and convert to clean
  if (names(data)[1] != "1") df <- clean_data_2001(data) # rename columns, get rid of initial NA's
  else df <- data
  
  # relevant index of rows and columns
  index <- grep("AREA", pull(df, 1)) # get index of municipalities + 1 for the summary
  id <- parse_number(pull(df[index,],1))
  id <- as.character(id)
  id <- paste("0", id, sep = "")
  
  index <- c(index, grep("RESUMEN", pull(df,1)))
  total_c <- ncol(df) # column that contains the total data over age, always the last column
  
  # initialize the return data frame
  result <- data.frame(id = character(0),
                       result = numeric(0)
  )
  
  # iterate over each municipality
  for (i in 1:(length(index) - 1)){
    # municipality name
    result[i,1] <- id[i]
    total <- grep("Total", pull(df[index[i]:index[i+1],],1)) # get index of total
    noaplica <- grep("No Aplica :", pull(df[index[i]:index[i+1],],1)) # get index of no aplica 
    tablavacia <- grep("Tabla ", pull(df[index[i]:index[i+1],],1))
    if(is_empty(tablavacia)){
      result[i,2] <- sum(as.numeric(pull(df[index[i] + total - 1,total_c])), as.numeric(pull(df[index[i] + noaplica - 1, 2])), na.rm = T)
    }
    else{
      result[i,2] = 0
    }
  }
  return(result)
}





