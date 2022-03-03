TheWholeShebang <- function(data_dir = NA,
                            data_file = "S:/Helion_Group/studies/rsa_moral/data/deriv/df.csv",
                            all_vars = c("PID", "ROI", "Run", "Cond", "Dim"),
                            btw_vars = c("PID", "ROI"),
                            wtn_vars = c("Run", "Cond")){
  library(tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/data_cat.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/corr_prep.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/super_corr.R", local = T)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/ezRSA/corr_to_long.R", local = T)

  if (!is.na(data_dir) & is.na(data_file)){
    df <- data_cat(dir = data_dir)
  }
  
  if (is.na(data_dir) & !is.na(data_file)){
    df <- read.csv(file = data_file,
                   header = T,
                   sep = ",",
                   row.names = 1,
                   na.strings = c("NA", ""))
  }
    
  df <- df %>%
        corr_prep(vars = all_vars) %>%
        super_corr(btwvars = btw_vars,
                   wtnvars = wtn_vars) %>%
        corr_to_long(btwvars = btw_vars,
                     wtnvars = wtn_vars)
  return(df)
}