#Task
#For each year (1990, 2000, 2010) and for each MSA (variable name METAREA):
  
#- Drop households with negative or zero income (HHINCOME)
#- Compute the Variance 10th percentile 90th percentile of house value (VALUEH) for household that 
#  own their main dwelling (OWNERSHIP), gross rent (RENTGRS) for household that do not own their main dwelling (OWNERSHIP) 
#- Compute the same above statistics for total household income (HHINCOME) conditional on (OWNERSHIP)

#Created by Akshat Prakash

#Progress uptil now:
#   Calculated 10th, 90th percentiles and variances separately for cases given above for each year for each msa
#   Graphing not studied yet. Will dive into that soon

#SOURCE .. (TO TEST FILE, PLEASE ALTER ABSOLUTE_DIRECTORY VARIABLE TO 
#           DIRECTORY OF UNZIPPED CSV FILE)
absolute_directory <- "C:\\Users\\akshatp\\Desktop\\DataScienceInEconomics\\HomeTask1ProfLaurence\\dataset4.csv" 
home_df <- read.csv(absolute_directory)

#STRUCTURE OF DATABASE (in .csv):
 # | YEAR | SERIAL | HHWT | METAREA | OWNERSHP | RENTGRS | HHINCOME | VALUEH |
 # unit of measurement is a household

#split dataframe into list of dataframes by year and name them
home_dfs_by_year <- split(home_df, home_df$YEAR)
names(home_dfs_by_year) <- c("1990", "2000", "2010")

#get data of years 1990, 2000, 2010 in a list with duplicates removed, removed na
#and only households with strictly positive income (HHINCOME)
home_dfs_by_year <- lapply(home_dfs_by_year, function (df) subset(df, !duplicated(df) &
                                                                      df$HHINCOME > 0 & 
                                                                      complete.cases(df)))

#HELPER FUNCTIONS TO REMOVE NULL DATA

#remove households with VALUEHs as 9999999 or 9999998 or 0000000 as in NA
remove_missing_VALUEHs <- function(df) {
  subset(df, df$VALUEH != 9999999 & 
             df$VALUEH != 9999998 & 
             df$VALUEH != 0000000)
}

#remove housholds with OWNERSHIP as 0 as in NA
remove_missing_OWNERSHIP <- function(df) {
  subset(df, df$OWNERSHP != 0)
}

#MASTER FUNCTION TO CALCULATE 10th, 90th PERCENTILES AND VARIANCE
tenth_ninetieth_percentile_variance_by_year_by_msas <- function (year_df, value = "VALUEH", ownership = 1, type = "percentile") {
  year_by_msas <- split(year_df, year_df$METAREA) 
  if (type == "percentile") {
    lapply(year_by_msas, function(msa) quantile(subset(msa, msa$OWNERSHP == ownership)[[value]], 
                                                probs = c(0.10,0.90)))
  } else if (type == "variance"){
    lapply(year_by_msas, function(msa) var(subset(msa, msa$OWNERSHP == ownership)[[value]]))
  }
} 

#CLEANING OUT NULL VALUES

#remove all records with OWNERSHIP as 0
#code 1 means owns dwelling
#code 2 means does not own dwelling
home_dfs_ownership_corrected <- lapply(home_dfs_by_year, remove_missing_OWNERSHIP)

#remove missing VALUEHs from all years and name the years
home_dfs_values_cleaned <- lapply(home_dfs_ownership_corrected, remove_missing_VALUEHs)



#CALCULATING 10th, 90th PERCENTILES AND VARIANCES FOR CASES GIVEN

#Statistics computed stored in variables 
# - percentile_10th_90th_VALUEH_own_dwelling
# - variance_VALUEH_own_dwelling
# - percentile_10th_90th_RENTGRS_not_own_dwelling
# - variance_RENTGRS_not_own_dwelling
# - percentile_10th_90th_HHINCOME_own_dwelling
# - variance_HHINCOME_own_dwelling
# - percentile_10th_90th_HHINCOME_not_own_dwelling
# - variance_HHINCOME_not_own_dwelling


#compute 10th, 90th percentile and variance of VALUEH for households that own main dwelling
#for each year and each msa
percentile_10th_90th_VALUEH_own_dwelling <- lapply(home_dfs_values_cleaned, 
                                                   tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                                   value = "VALUEH", ownership = 1,
                                                   type="percentile")
variance_VALUEH_own_dwelling <- lapply(home_dfs_values_cleaned, 
                                       tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                       value = "VALUEH", ownership = 1,
                                       type="variance")

#compute 10th, 90th percentile and variance of RENTGRS for households that do not own main dwelling 
#for each year and each msa
percentile_10th_90th_RENTGRS_not_own_dwelling <- lapply(home_dfs_ownership_corrected, 
                                                        tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                                        value = "RENTGRS", ownership = 2,
                                                        type="percentile")

variance_RENTGRS_not_own_dwelling <- lapply(home_dfs_ownership_corrected, 
                                            tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                            value = "RENTGRS", ownership = 2,
                                            type="variance")

#compute 10th, 90th percentile and variance of HHINCOME for households that own main dwelling
#for each year and each msa
percentile_10th_90th_HHINCOME_own_dwelling <- lapply(home_dfs_ownership_corrected, 
                                                     tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                                     value = "HHINCOME", ownership = 1,
                                                     type="percentile")

variance_HHINCOME_own_dwelling <- lapply(home_dfs_ownership_corrected, 
                                         tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                         value = "HHINCOME", ownership = 1,
                                         type="variance")

#compute 10th, 90th percentile and variance of HHINCOME for households that own do not own main dwelling
#for each year and each msa
percentile_10th_90th_HHINCOME_not_own_dwelling <- lapply(home_dfs_ownership_corrected, 
                                                         tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                                         value = "HHINCOME", ownership = 2,
                                                         type="percentile")

variance_HHINCOME_not_own_dwelling <- lapply(home_dfs_ownership_corrected, 
                                             tenth_ninetieth_percentile_variance_by_year_by_msas, 
                                             value = "HHINCOME", ownership = 2,
                                             type="variance")

