## step2.R | v2022.01.11

# I used step2 because that's what I colloquially think of this as. I'm sure other 
# RSA approaches might handle this differently.
step2 <- function(df, # Dataframe should be formatted such that each row represents
                      # a voxel coordinate and each column represents a unique combination
                      # of elements (e.g., Pt A's data while watching Movie B in Run C).
                      # NAs can, and likely will, be present. R will ignore these when
                      # running correlations. 
                  output = "long") # Future functionality that will allow for matrix output,
                                   # perhaps helpful for spatial analyses, or long format,
                                   # helpful for a traditional ANOVA or regression.
){
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
