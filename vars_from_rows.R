## vars_from_rows.R | v2022.01.18

# Will create array variables with values pulled from columns names.
# Assumes that all column names should be analyzed. Also assumes that
# disparate array values are separated by a character that only serves 
# that specific function.
vars_from_rows <- function(df,          # Dataframe to be analyzed
                           nvar,        # Number of variables within the columns names
                           position,    # What position in the string does the variable appear
                           split = "_") # The character that separates different variable elements
{
  # Dependent packages----
  library(tidyverse)
  
  # Errors----
  # if (length(vars) != length(strsplit(names(df)[1],
  #                                     split = split,
  #                                     fixed = T)[[1]])){
  #   stop(paste0("The number of variables you have entered (",
  #               length(vars),
  #               " is different than the number of distinct elements detected within the columns names. Please double check your formatting and try again."))
  # }
  # 
  # ## Creating a new list to house variables ----
  # list <- list(c(NA))
  
  # Split the first column name and find how many elements it has----
  # for (i in 1:length(vars)){
  # Create a new variable from the i position in the array vars----
  # Assign to that variable all of the unique elements that can be found from that same
  # position amongst the different column names throughout the dataframe.
    return(unique(unlist(strsplit(rownames(df), 
                                  split = split, 
                                  fixed = T))[seq(position,
                                                  nrow(df) * nvar,
                                                  nvar)]))
  # }
}