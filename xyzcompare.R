## xyzcompare.R | v2022.01.18

# Will compare the coordinates of every row in specified columns
# and note where differences exist.
xyzcompare <- function(df = .,          # Dataframe to be analyzed
                       vars,        # An array of variable names in the order in which they appear in every column name 
                       split = "_", # The character that separates different variable elements
                       dims_first = TRUE) # Logical stating whether dimensions occur before their respective values
{
  # Dependent packages----
  library(tidyverse)
  
  # Print out ----
  print(paste("xyzcompare has started |", Sys.time()))
  
  summary <- NA
  # Iterate through every value of variable 1
  for (i in vars[[1]]){
    #Iterate through every value of variable 2
    for (j in vars[[2]]){
      # Find every instance among the column names where both variable 1 and variable 2 are present
      for (k in which(str_detect(colnames(df),
                                 pattern = paste0("^", i, split, j, split)) == TRUE)){
        # and Iterate through every single row in those columns.
        for (m in 1:length(rownames(df))){
          # If the dimension columns are situated before their respective value columns
          if (dims_first == TRUE){
            # Ignoring columns that have values instead of coordinates
            if (k %% 2 != 0 & all(!is.na(df[,k]))){
              # Ignoring comparison standard columns
              if (k != which(str_detect(colnames(df),
                                        pattern = paste0("^", i,split, j, split)) == TRUE)[1]){
                # Comparing the coordinates in the first column to every other column
                if (df[m,k] != df[m,which(str_detect(colnames(df),
                                                     pattern = paste0("^", i,split, j, split)) == TRUE)[1]]){
                  # And taking note whenever they don't match. 
                  summary <- c(summary, paste0(i,"_", j, " | Row: ", m))
                }
              }
            }
          }
          # If the dimension columns are situated after their respective value columns
          if (dims_first == FALSE){
            # Ignoring columns that have values instead of coordinates
            if (k %% 2 == 0 & all(!is.na(df[,k]))){
              # Ignoring comparison standard columns
              if (k != which(str_detect(colnames(df),
                                        pattern = paste0("^", i,split, j, split)) == TRUE)[1]){
                # Comparing the coordinates in the first column to every other column
                if (df[m,k] != df[m,which(str_detect(colnames(df),
                                                     pattern = paste0("^", i,split, j, split)) == TRUE)[1]]){
                  # And taking note whenever they don't match. 
                  summary <- c(summary, paste0(i,"_", j, " | Row: ", m))
                }
              }
            }
          }
        }
      }     
    }
    # Print out ----
    print(paste("xyzcompare is", 
                round(((which(vars[[1]] == i))/length(vars[[1]]) * 100),1),
                "% complete |", 
                Sys.time()))
    
  }
  return(summary)
}