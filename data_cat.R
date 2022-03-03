## data_cat.R | v2022.02.25

# I've found RSA easiest to appraoch if I house all of the data within a single, well-structured dataframe. 
# As such, this should be the first step in your RSA data processing pipeline.
data_cat <- function(dir, # Parent directory containing all of the files to be uploaded; 
                               # should not contain any files that will not be uploaded or analyzed. 
                               # Files must be of .csv type and contain four dimensions in the following
                               # order: x, y, z, value. These should also ideally be contained with in 
                               # 4 rows of 1 column and separated by commas.
                     checkpoint = NA, # Numeric value determining after how many files R should 
                                       # output a progress report. Can take a numeric value or 'NA' 
                                       # to be turned off.
                     type = NA) # Future functionality to differentiate between neural and behavioral data. 

{## Package Loading ----
  pacman::p_load(assertthat, tidyverse)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/restructure.R", local = TRUE)
  source("C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/stinkR/make_df.R", local = TRUE)

  ## Initial Errors ----
  if (!is.string(dir)){
    stop(print("dir must be entered as a string. Please update dir to comply."))
  }
  if (!is.na(checkpoint)){
    if (!is.numeric(checkpoint) & checkpoint <= 0){
      stop(print("checkpoint must be entered as a positive integer or 'NA'. Please update checkpoint to comply."))
    }
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
  if (!is.na(checkpoint)){
    if (length(list.files(path = dir, recursive = T)) <= checkpoint){
      stop(paste("The checkpoint marker you indicated is smaller than the number of files you have to upload (",
                 length(list.files(path = dir, recursive = T)),
                 "). Please respecify checkpoint to be a lower value, or leave it blank and let the function automatically specify checkpoints for you!",
                 sep = " "))
    }
  }
  
  ## Printing Steps ----
  print("Creating a new dataframe to house all of the data . . .")
  
  ## Creating the Primary Dataframe ----
  dims <- c("x","y","z","val")
  
  ## Creating Columns Based Upon Filenames -----
  
  cols <- list.files(path = dir, 
                     recursive = T) %>%
          restructure(array = .,
                      subcols = dims)

  ## Defining Rows ----
  rows <-1
  
  ## Creating Dataframe ----
  df <- make_df(rows = rows,
                cols = cols)
  rm(rows)
  
  ## Printing Steps ----
  print("Initiating transfer of data to new dataframe . . .")

  ## Creating Checkpoints ----
  if (!is.na(checkpoint)){
    k <- seq(checkpoint,
             length(list.files(path = dir, recursive = T)), 
             checkpoint)
  }
  
  if (is.na(checkpoint)){
    k <- seq(1,
             length(list.files(path = dir, recursive = T)), 
             round(length(list.files(path = dir, recursive = T))/4))
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
    df_temp <- list.files(path = dir,
                                 recursive = T,
                                 full.names = T)[i] %>%
               read.csv(header = F,
                         sep=",",
                         dec = ".",
                         stringsAsFactors = F) %>%
    ## Transposing the Dataframe ----
               t() %>%
    ## Separating Cells By Commas ----
               strsplit(split = " ",
                         fixed = T) %>%
               as.data.frame()

    ## Resizing df If Not Enough Rows ----
    if (length(rownames(df)) < length(rownames(df_temp))){
      rows <-1:(length(rownames(df_temp)) - length(rownames(df)))
      df_temp2 <- make_df(rows = rows,
                          cols = cols)
      df <- rbind(df, df_temp2)
      rm(df_temp2, rows)
    }
    
    ## Noting Filename ----
    filename <- list.files(path = dir, 
                           recursive = T)[i] %>%
                str_replace_all(pattern = "^.*\\/|\\.csv$", 
                                replacement = "") %>%
                str_replace_all(pattern = "^.*\\-|^.*\\.", 
                                replacement = "") 
    
    ## Copying Data to Final df Based Upon Filename----
    df[1:length(rownames(df_temp)), grep(filename, colnames(df))] <- df_temp
    
    ## Outputting Progress ----
    if (any(i == k)){
      print(paste0("Files completed as of ", 
                   Sys.time(), 
                   ": ", 
                   i, 
                   " (", 
                   round((i/length(list.files(path = dir, recursive = T)) * 100),digits = 0) ,"% complete)"))
    }
  } 
  
  ## Printing Steps ----
  print("Data transfer complete . . .")
  
  ## Printing Steps ----
  print("Data transposed during transfer")
  
  rm(df_temp, cols, dims, filename, i, k)
  return(df)
}