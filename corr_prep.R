## corr_prep.R | v2022.01.11

# corr_prep should be the second step that we run. It will take our dataframe from data_cat and 
# 1) use the column names to identify the unique values each variable could take, 2) remove 
# superfluous rows with only NA values, 2) check that the dimensions for each participant
# align, and 3) remove those dimension columns leaving us with a dataframe of values which we
# could feed into super_corr.
corr_prep <- function(df = ., 
                      # Dataframe should be formatted such that each row represents
                      # a voxel coordinate and each column represents a unique combination
                      # of elements (e.g., Pt A's data while watching Movie B in Run C).
                      # NAs can, and likely will, be present. R will ignore these when
                      # running correlations.
                      vars = NA) 
                      # An array of variable names constituting the unique elements 
                      # being compared in your RSA. These should be contained within
                      # your column names, separated by underscores, and entered in 
                      # the order they appear. So, if an example column name for neuro 
                      # data was "P101_amygdala_run1_y", I could write: 
                      # vars = c("PID", "ROI", "Run", "Dimension"). Value of NA is acceptable.

{
  # Setup ----
  ## Package Loading ----
  pacman::p_load(tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/remove_NAs.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/catdims.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/vars_from_cols.R", local = T)
  # source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/xyzcompare.R", local = T)
  
  # Print out ----
  print(paste("corr_prep has started |", Sys.time()))
  
  # Creating variables from the contents of column names ----
  for (i in 1:length(vars)){
    assign(vars[i],
           vars_from_cols(df = df,
                          nvar = length(vars),
                          position = i,
                          split = "_"))
  }
  
  # Removing any rows that only contain NA values ----
  # Concatenating the X,Y, & Z coordinates into a single column ----
  df <- df %>%
        remove_NAs(cols = FALSE) %>%
        catdims()
  
  # # Checking that the the coordinates for all future comparisons match each other.
  # summary <- xyzcompare(df = df,
  #                       vars = list(PID, ROI),
  #                       dims_first = FALSE)
  # 
  # ## Highlighting Errors ----
  # if (!is.na(summary)){
  #   stop("When comparing the coordinates in your data, inconsistencies were found. Please see the output below.
  #        In order to ensure that RSA is correlating the right values, please make sure that the coordinates of 
  #        voxels for any given person and region are organized in a consistent order.")
  #   print(summary)
  # }
  # 
  # ## Outputting data ----
  # if(is.na(summary)){
  #   print("We'd compared your input and found no dimension issue. The coordinates for each participant and region 
  #         aligned as expected. We are now removing the coordinates.")
    
    ## Removing dimension columns ----
    df <- df[,-which(str_detect(names(df),
                                pattern = "_xyz$"))]
    
    ## Renaming remaining columns ----
    names(df) <- str_replace_all(names(df),
                                 pattern = "_val$",
                                 replacement = "")
    
    return(df)
}