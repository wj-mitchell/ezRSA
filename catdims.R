## catdims.R | v2022.01.18

# Will concatenate x, y, and z dimensions from neuro data into singular columns
# across a wide-formatted dataframe and remove superfluous columns. Coordinates
# must be in column headers and at the end of the name, like: "_x", "_y", "_z".
catdims <- function(df = .,
                    progress = 10)
{
  
  # Dependent functions----
  library(tidyverse)
  
  # Print out ----
  print(paste("catdims has started |", Sys.time()))
  
  # Identifying x columns----
  xcols <- str_detect(colnames(df),
                      pattern = "_x$")
  
  # Identifying y and z columns----
  yzcols <- str_detect(colnames(df),
                       pattern = "_y$|_z$")
  
  # Print out ----
  print(paste("catdims is concatenating all of the dimension columns |", Sys.time()))
  
  # Concatenating values from x, y, and z columns into x column across each row and across whole dataframe.----
  # Assumes y column always follows x column and z always follows y column.
  for (i in 1:length(xcols)){
    if (xcols[i] == TRUE){
      df[,i] <- paste(df[,i], 
                      df[,i + 1],
                      df[,i + 2],
                      sep = ",") 
      if (round(((i/length(xcols)) * 100), 3) %% progress == 0){
        print(paste(round(((i/length(xcols)) * 100), 0),
                    "% dimensions concatenated | ", 
                    Sys.time()))
      }
    }
  }
  
  # Changes the name of x column to reflect the fact that it now contains all coordinates----
  colnames(df) <- str_replace_all(string = colnames(df),
                                       pattern = "_x$", 
                                       replacement = "_xyz")
  
  # Print out ----
  print(paste("catdims is removing the old dimension columns |", Sys.time()))
  
  # Removes superfluous y and z columns----
  df <- df[,-which(yzcols == TRUE)]
  
  # Changing NA values ----
  df[df[,] == "NA,NA,NA"] <- NA
  
  return(df)
}