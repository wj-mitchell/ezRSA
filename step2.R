## step2.R | v2022.01.11

# I used step2 because that's what I colloquially think of this as. I'm sure other 
# RSA approaches might handle this differently.
step2 <- function(df, # Dataframe should be formatted such that each row represents
                      # a voxel coordinate and each column represents a unique combination
                      # of elements (e.g., Pt A's data while watching Movie B in Run C).
                      # NAs can, and likely will, be present. R will ignore these when
                      # running correlations.
                  vars = NA, # An array of variable names constituting the unique elements 
                             # being compared in your RSA. These should be contained within
                             # your column names, separated by underscores, and entered in 
                             # the order they appear. So, if an example column name for neuro 
                             # data was "P101_amygdala_run1_y", I could write: 
                             # vars = c("PID", "ROI", "Run", "Dimension"). Value of NA is acceptable.
                  output = "long") # Future functionality that will allow for matrix output,
                                   # perhaps helpful for spatial analyses, or long format,
                                   # helpful for a traditional ANOVA or regression.
{
  # Setup ----
  ## Package Loading ----
  pacman::p_load(tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/removeNAs.R")
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/catdims.R")
  # source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/colvars.R")
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/xyzcompare.R")
  
  vars = c("PID", "ROI", "Run", "Cond", "Dim")
  
  # Removing any rows that only contain NA values ----
  df <- removeNAs(df = df)
  
  # Concatenating the X,Y, & Z coordinates into a single column ----
  df <- catdims(df = df)
  
  # Creating variables from the contents of column names ----
  for (i in 1:length(vars)){
  # Create a new variable from the i position in the array vars----
  # Assign to that variable all of the unique elements that can be found from that same
  # position amongst the different column names throughout the dataframe.
  assign(vars[i], unique(unlist(strsplit(colnames(df), 
                                         split = "_", 
                                         fixed = T))[seq(i,
                                                         length(colnames(df)) * length(vars),
                                                         length(vars))]))
  }
  
  # Checking that the the coordinates for all future comparisons match each other.
  summary <- xyzcompare(df = df,
                        vars = list(PID, ROI))
  
  # Removes superfluous y and z columns----
  df <- df[,-which(yzcols == TRUE)]
  
  df_cor <- data.frame(matrix(NA, 
                              nrow = length(PIDs) * length(Vids), 
                              ncol = length(PIDs), 
                              dimnames = list(colnames(df), PIDs)))

for (e in 1:length(Vids)){
  for (i in 1:length(PIDs)){
    for (c in 1:length(PIDs)){
      if (i>=c)
        df_cor[((e - 1) * length(PIDs)) + i, c] <- NA
      if (i<c)
        df_cor[((e - 1) * length(PIDs)) + i, c] <- cor(as.numeric(df[,((e - 1) * length(PIDs)) + i]), 
                                                       as.numeric(df[,((e - 1) * length(PIDs)) + c]), 
                                                       use ="pairwise.complete.obs", method = "spearman")
      }
    }
}



rows <-((length(PIDs)^2) * length(Vids))
df_long <- data.frame(matrix(NA, 
                             nrow = rows,
                             ncol = 6, 
                             dimnames = list(1:rows, c("Video", "PID1", "Role1", "PID2", "Role2","CorrVal"))))



for (i in 1:length(PIDs)){
  for (c in 1:length(PIDs)){
    for (e in 1:length(Vids)){
      df_long$CorrVal[(((((e - 1) * length(PIDs)) + c) - 1) * length(PIDs)) + i] <- df_cor[((e - 1) * length(PIDs)) + c, i]
      df_long$Video[(((((e - 1) * length(PIDs)) + c) - 1) * length(PIDs)) + i] <- Vids[e]
      df_long$PID1[(((((e - 1) * length(PIDs)) + c) - 1) * length(PIDs)) + i] <- PIDs[c]
      df_long$PID2[(((((e - 1) * length(PIDs)) + c) - 1) * length(PIDs)) + i] <- PIDs[i]
    }
  }
}


df_long <- subset(df_long, (df_long$PID1 != df_long$PID2) & !is.na(df_long$CorrVal))

}