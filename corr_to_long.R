corr_to_long <- function(df = .,
                         btwvars,
                         wtnvars,
                         progress = 10)
{
  library(tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/paste_all.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/vars_from_rows.R", local = T)
  
  
  print(paste("corr_to_long started! ", Sys.time()))
  
  ## Creating an array of variable names ----
  # This needs to be improved in case names don't follow this order
  vars <- c(unlist(btwvars),unlist(wtnvars))
  
  ## Creating variables from the contents of column names ----
  for (i in 1:length(vars)){
    assign(vars[i],
           vars_from_rows(df = df,
                          nvar = length(vars),
                          position = i,
                          split = "_"))
  }
  
  array = c()
  for (i in 1:nrow(df)){
    array = c(array, df[i,])
    if (round(((i/nrow(df)) * 100), 2) %% progress == 0){
      print(paste(round(((i/nrow(df)) * 100), 2),
                  "% of correlations have been transferred  | ", 
                  Sys.time()))
    }
  }
  
  compares <- paste_all(list = list(get(wtnvars[1]),
                                    get(wtnvars[2])),
                        sep = "_")
  
  rows <- 1:length(array)
  cols <- c(unlist(btwvars), "Comparison1", "Comparison2", "Correlation")

  df_new <- make_df(rows = rows,
                    cols = cols)
  
  print(paste("Transferring data to final dataframe! ", Sys.time()))
  
  df_new$Correlation <- unlist(array)
  df_new$PID <- sort(rep(PID, (length(compares)^2 * length(ROI))))
  df_new$ROI <- rep(sort(rep(ROI, length(compares)^2)), length(PID))
  df_new$Comparison1 <- names(array)
  df_new$Comparison2 <- rep(sort(rep(compares, length(compares))), length(PID) * length(ROI))
  df_new <- subset(df_new, !is.na(df_new$Correlation))
  df_new$Correlation.fz <- psych::fisherz(as.numeric(df_new$Correlation))
  
return(df_new)
}