#### Helper Functions to read and process 2001 census data ####

#### FUNCTION 1: get_summary_2001 ####
# function to obtain the RESUMEN portion of the excel sheets from 2001 
get_summary_2001 <- function(data){
    resumen_index <- grep("RESUMEN", data$...2) # Find the row index of where summary begins in the sheet
    
    temp <- data[(resumen_index + 3):(resumen_index + 10),2:(ncol(data) - 1)] # subset to obtain only the relevant cells
    names(temp) <- c("ethnic_group", as.character(temp[1, 2:ncol(temp)])) # extract the age variable names (some sheets have missing age values)
    temp <- temp[2:nrow(temp),]
    return(gather(temp, age, population, 2:(ncol(temp)))) # convert to long format and return
}

# need to account for some files not containing all the ethnic group options (for example caebl=17 doesn't have mojeno)

#### FUNCTION 2: clean_data_2001 ####
# helper function to do initial data cleaning for get_mun_2001 function
clean_data_2001 <- function(data){
    # Get rid of col 1 because it's useless
    data <- data[, 2:ncol(data)]
    
    # Clean column names
    names(data) <- as.character(1:ncol(data))
    
    # Get rid of initial rows that have NA's
    start_index <- grep("AREA", pull(data, 1))[1]
    return(data[start_index:nrow(data),])
}

#### FUNCTION 3: get_mun_2001 ####
# more robust function to obtain each municipalities data and save to a list 
get_mun_2001 <- function(data){
    
    # check to see if you have clean data or original data, and convert to clean
    if (names(data)[1] != "1") df <- clean_data_2001(data) # rename columns, get rid of initial NA's
    else df <- data
    
    # relevant index of rows and columns
    index <- grep("AREA", pull(df, 1)) # get index of municipalities
    resumen_i <- grep("RESUMEN", pull(df,1)) # get index of summmary section: RESUMEN 
    total_c <- ncol(df) # column that contains the total data over age, always the last column
    
    # initialize the return data frame
    result <- data.frame(name = character(),
                         ind = integer(),
                         not_ind = integer()
    )
    
    # iterate over each municipality
    for (i in 1:length(index)){
        # municipality name
        result[i,1] <- as.character(df[index[i], 2]) # name of mun is stored in second column
        
        # get index of NINGUNO within the municipality
        end = ifelse(i == length(index), resumen_i, index[i+1])
        index_nin <- grep("NINGUNO", pull(df,1)[index[i]: end]) # possible that NINGUNO doesn't exist
        total_index <- grep("Total", pull(df,1)[index[i]: end])
        
        # if NINGUNO is missing in the municipality
        if (length(index_nin) == 0){
            # population of indigenous
            
            if(length(total_index) == 0){
                result[i,2] = NA
            }
            
            else{
                result[i,2] <- sum(as.numeric(pull(df[(index[i] + 4):(index[i] + total_index - 2), total_c])), na.rm = T) # use index of total instead of NINGUNO if NINGUNO is missing
            }
            
            # population of non-indigenous
            result[i,3] <- NA
        }
        
        # if NINGUNO is present in the municipality
        if (length(index_nin) != 0){
        
            # population of indigenous
            if (index_nin == 5){
                result[i,2] <- NA # if no indigenous responses, exist
            }
            else {
                result[i,2] <- sum(as.numeric(pull(df[(index[i] + 4):(index[i] + index_nin - 2), total_c])), na.rm = T) 
            }
            
            # population of non-indgenous
            result[i,3] <- as.numeric(pull(df[index_nin + index[i]-1, total_c]))
        }

    }
    return(result)
}

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


get_2001(m_9)




