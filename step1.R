## step1.R | v2022.02.02

# I used step1 because that's what I colloquially think of this as. I'm sure other 
# RSA approaches might handle this differently.
step1 <- function(dir, # Parent directory containing all of the files to be uploaded; 
                       # should not contain any files that will not be uploaded or analyzed. 
                       # Files must be of .csv type and contain four dimensions in the following
                       # order: x, y, z, value. These should also ideally be contained with in 
                       # 4 rows of 1 column and separated by commas.
                  checkpoint = 100, # Numeric value determining after how many files R should 
                                   # output a progress report. Can take a numeric value or 'NA' 
                                   # to be turned off.
                  type = NA) # Future functionality to differentiate between neural and behavioral data. 
{## Package Loading ----
  pacman::p_load(assertthat, tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/restructure.R", local = TRUE)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = TRUE)
  
  ## Options ----
  options(scipen=100)
  options(digits=3)
  options(contrasts = c("contr.helmert", "contr.poly"))

  ## Initial Errors ----
  if (!is.string(dir)){
    stop(print("dir must be entered as a string. Please update dir to comply."))
  }
  if (!is.numeric(checkpoint) & checkpoint <= 0){
    stop(print("checkpoint must be entered as a positive integer or 'NA'. Please update checkpoint to comply."))
  }
  if (!is.na(checkpoint)){
    if (!any(str_detect(list.files(path = dir,
                                   recursive = T,
                                   full.names = T),"\\.csv"))){
      stop(paste("Submitted files must be in .csv format. A total of",
                 length(str_detect(list.files(path = dir, recursive = T, full.names = T),"\\.csv")[str_detect(list.files(path = dir, recursive = T, full.names = T),"\\.csv") == FALSE]), 
                 "files do not have a .csv extension:",
                 str_detect(list.files(path = dir, recursive = T, full.names = T),"\\.csv")[str_detect(list.files(path = dir, recursive = T, full.names = T),"\\.csv") == FALSE],
                 sep = " "))
    }
  }
  
  ## Creating the Primary Dataframe ----
  dims <- c("x","y","z","val")
  
  ## Creating Columns Based Upon Filenames -----
  
  cols <- list.files(path = dir, 
                     recursive = T) %>%
          restructure(array = .)

  ## Defining Rows ----
  rows <-1
  
  ## Creating Dataframe ----
  df <- make_df(rows = rows,
                cols = cols)

  ## Creating Checkpoints ----
  if (!is.na(checkpoint)){
    k <- seq(checkpoint,
             length(list.files(path = dir, recursive = T)), 
             checkpoint)
  }

  # Datawriting Loop ----
  ## For All Files Within Parent File ....
  for (i in 1:length(list.files(path = dir, recursive = T))){
    
    ### Checking for Row Formatting Errors ----
    if (length(rownames(read.csv(file = list.files(path = dir,
                                                   recursive = T,
                                                   full.names = T)[i],
                               header = F,
                               sep=",",
                               dec = ".",
                               stringsAsFactors = F))) != length(dims)){
      stop(paste("The file",
                 list.files(path = dir,
                            recursive = T,
                            full.names = F)[i],
                 "does not contain 4 rows of data, which is required formatting for
                 this function to work properly.",
                 sep = " "))
    }

    ### Checking for Column Formatting Errors ----
    if (length(colnames(read.csv(file = list.files(path = dir,
                                                 recursive = T,
                                                 full.names = T)[i],
                               header = F,
                               sep=",",
                               dec = ".",
                               stringsAsFactors = F))) != 1){
      stop(paste("The file",
                 list.files(path = dir,
                            recursive = T,
                            full.names = F)[i],
                 "contains more than 1 column of data, which is does not comply with the formatting required for
                 this function to work properly.",
                 sep = " "))
    }

    ## Reading in the Data from File i ----
    ## Transposing the Dataframe ----
    ## Separating Cells By Commas ----
    df_temp <- as.data.frame(strsplit(x = t(read.csv(file = list.files(path = dir,
                                                                       recursive = T,
                                                                       full.names = T)[i],
                                                     header = F,
                                                     sep=",",
                                                     dec = ".",
                                                     stringsAsFactors = F)),
                                      split = " ",
                                      fixed = T))

    ## Resizing df If Necessary ----
    if (length(rownames(df)) < length(rownames(df_temp))){
      rows <-1:(length(rownames(df_temp)) - length(rownames(df)))
      df_temp2 <- data.frame(matrix(NA,
                              nrow = length(rows),
                              ncol = length(cols),
                              dimnames = list(rows, cols)))
      df <- rbind(df, df_temp2)
      rm(df_temp2)
    }
    
    ## Noting Filename ----
    filename <- list.files(path = dir, 
                           recursive = T)[i] %>%
                str_replace_all(pattern = "^.*\\/|\\.csv$", 
                                replacement = "") %>%
                str_replace_all(pattern = "^.*\\-|^.*\\.", 
                                replacement = "") 
    
    ## Copying Data to Final df ----
    df[1:length(rownames(df_og)), grep(filename, colnames(df))] <- df_og
    
    ## Outputting Progress ----
    if (any(i == k) & !is.na(checkpoint)){
      print(paste0("Files completed as of ", Sys.time(), ": ", i))
    }
  } 
  rm(df_temp)
  return(df)
}