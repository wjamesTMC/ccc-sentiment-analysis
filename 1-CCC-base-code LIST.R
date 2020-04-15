##############################################################################
#
# CCC Sentiment Analysis 
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/ccc-sentiment analysis.git
#
##############################################################################

#-----------------------------------------------------------------------------
#
# Library setups
#
#-----------------------------------------------------------------------------

# Import libraries
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(reshape2)
library(stringi)
library(stringr)
library(expss)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)
library(rmarkdown)
library(kableExtra)
library(googlesheets)

#--------------------------------------------------------------------
#
# File opens and cleanup
#
#--------------------------------------------------------------------

#
# Email file - the log of emails from CCC reps with classifications
#

# Open email file
e_fln <- gs_title("CCC 19 08 12-18 Altered")
e_dat <- gs_read(e_fln, stringsAsFactors = FALSE)

# Establish a single category for various technical permutations
e_dat[e_dat == "Technical/Access"]           <- "Technical"
e_dat[e_dat == "Technical/Device"]           <- "Technical"
e_dat[e_dat == "Technical/Downloading"]      <- "Technical"
e_dat[e_dat == "Technical/Feature"]          <- "Technical"
e_dat[e_dat == "Technical/Bug/System Issue"] <- "Technical"
e_dat[e_dat == "Broken Link"]                <- "Technical"

# Remove duplicates
e_dat <- e_dat %>% filter(C_Type != "Duplicate", C_Type != "")
e_dat <- as.data.frame(e_dat)

#
# Vocabulary file - the list of terms we are using to predict classifications
#

# Open vocabulary file 
v_fln <- gs_title("CCC Email Vocabulary")
v_dat <- gs_read(v_fln, stringsAsFactors = FALSE)

#--------------------------------------------------------------------
#
# Testing for various classifications
#
#--------------------------------------------------------------------

#
# Classification "Junk"
#

# Determine what the CCC staff classified as Junk
match_term  <- "Junk or No Answer Required"
act_result  <- e_dat %>% filter(e_dat$C_Type == match_term) 
act_num     <- nrow(act_result)

# Establish the Junk vocablulary
v_dat  <- as.data.frame(v_dat %>% filter(Junk == "X"))

# Predict match term based on input vocabulary
for(i in 1:nrow(e_dat)) {
     if(is.na(e_dat$Desc[i]) == TRUE) {
          e_dat$Desc[i] <- "None"
     }
        
     for(j in 1:nrow(v_dat)) {
          if(str_detect(e_dat$Desc[i], v_dat$Word[j]) == TRUE) {
               e_dat$N_Type[i]  <- match_term
               e_dat$M_Count[i] <- e_dat$M_Count[i] + 1
          }
     }
}

# Write out the results
e_ofl <- e_dat %>% filter(C_Type == match_term | N_Type == match_term)

# Write out the results (res_df)
output_file <- gs_title("CCC Junk Match Results")
gs_edit_cells(output_file, ws = 1, input = e_ofl, anchor = "A2", byrow = FALSE,
              col_names = NULL, trim = FALSE, verbose = TRUE)



