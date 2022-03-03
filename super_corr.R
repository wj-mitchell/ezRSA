## super_corr.R | v2022.01.18

# Will create a new dataframe containing the correlation matrices, which themselves contain correlations between the identified variables
super_corr <- function(df = .,          # Dataframe to be analyzed
                       wtnvars,     # A list of variable names across which correlations should be run 
                       btwvars,     # A list of variable names across which correlations should not be run
                       progress = 10)
{
  # Dependent packages----
  library(tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/vars_from_cols.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/paste_all.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/remove_NAs.R", local = T)
  
  ## Creating an array of variable names ----
  # This needs to be improved in case names don't follow this order
  vars <- c(unlist(btwvars),unlist(wtnvars))
  
  ## Creating variables from the contents of column names ----
  for (i in 1:length(vars)){
    assign(vars[i],
           vars_from_cols(df = df,
                          nvar = length(vars),
                          position = i,
                          split = "_"))
  }
  
  ## Noting the row names of the correlative dataframe ----
  rows <- names(df)
  
  ## Noting the column names of the correlative dataframe ----
  cols <- paste_all(list = list(get(wtnvars[1]),
                                get(wtnvars[2])),
                    sep = "_")
  
  ## Creating a correlative Dataframe ----
  df_cor <- make_df(rows = rows,
                    cols = cols)
  
  print(paste("Correlations Started! ", Sys.time()))
  
  ## Running correlations across within-subject variables, but not between subjects. 
  # Iterate through every value of variable 1
  for (i in 1:length(get(btwvars[1]))){
    #Iterate through every value of variable 2
    for (j in 1:length(get(btwvars[2]))){
      # Find every instance among the column names where both variable 1 and variable 2 are present
      # Only run correlations on columns within that range.
      range <- which(str_detect(colnames(df), pattern = paste0("^", get(btwvars[1])[i],"_", get(btwvars[2])[j], "_")) == TRUE)
      # For every column within that range
      for (k in 1:length(range)){
        # Iterate through the other columns in that range
        for (m in 2:length(range)){
          # and if they are a column for which we haven't run correlations yet
          if (k < m){
            #Run a correlation and place it in the appropriate slot in the new dataframe
            df_cor[range[m], k] <- cor(as.numeric(df[,range[k]]), 
                                       as.numeric(df[,range[m]]), 
                                       use ="pairwise.complete.obs", method = "spearman")
          }
        }
      }
    }
    # Print out ----
    if (round(((i/length(get(btwvars[1]))) * 100),0) %% progress == 0){
      print(paste("super_corr is", 
                  round(((i/length(get(btwvars[1]))) * 100), 0),
                  "% complete |", 
                  Sys.time()))
    }
  }
  return(df_cor)
}
