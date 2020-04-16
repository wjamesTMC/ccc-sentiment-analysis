##############################################################################
#
# CCC Sentiment Analysis - Vocabulary extraction and building
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
library(tokenizers)
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
e_fln <- gs_title("CCC 19 08 12-18 Test_1")
e_dat <- gs_read(e_fln, stringsAsFactors = FALSE)

# Establish a single category for various technical permutations
e_dat[e_dat == "Technical/Access"]           <- "Technical"
e_dat[e_dat == "Technical/Device"]           <- "Technical"
e_dat[e_dat == "Technical/Downloading"]      <- "Technical"
e_dat[e_dat == "Technical/Feature"]          <- "Technical"
e_dat[e_dat == "Technical/Bug/System Issue"] <- "Technical"
e_dat[e_dat == "Broken Link"]                <- "Technical"

# Remove duplicates and blank descriptions, if any
e_dat <- e_dat %>% filter(C_Type != "Duplicate", C_Type != "")
e_dat <- e_dat %>% filter(Desc != "<None>")
e_dat <- as.data.frame(e_dat)

# Pull in Trillion Word list
base_url <- "https://programminghistorian.org/assets/basic-text-processing-in-r"
url <- sprintf("%s/sotu_text/236.txt", base_url)
wf <- read_csv(sprintf("%s/%s", base_url, "word_frequency.csv"))

# Establish the category list
cat_list <- unique(e_dat$C_Type)

# Outer loop to build a vocab list for each category
for(x in 1:length(cat_list)) {
     
     # Establish the category for the inner loop
     cat_name <- cat_list[x]
     
     # Pull all the rows for that category from the data file
     category <- e_dat %>% filter(C_Type == cat_name)
     cat_len <- nrow(category)
     
     # Create a dataframe to hold the results
     cat_df <- data.frame(cat_word = 1:cat_len, cat_name = cat_name, cat_count = 1:cat_len)
     
     # Loop through the category and build a dataframe of the results
     new_df_line <- data.frame(cat_word = 1, cat_name = cat_name, cat_count = 1)
     
     # Now the inner loop to generate the results
     for(i in 1:cat_len) {
          text  <- category[i,3]
          words <- tokenize_words(text)
          tab <- table(words)
          tab <- data_frame(word = names(tab), count = as.numeric(tab))
          tab <- arrange(tab, desc(count))
          tab <- inner_join(tab, wf)
          wds <- filter(tab, frequency < 0.001)
          if(nrow(wds) > 0) {
               for(j in 1:nrow(wds)) {
                    new_df_line[1,1] <- wds[j,1]
                    new_df_line[1,3] <- wds[j,2]
                    cat_df <- rbind(cat_df, new_df_line)
               }
          }
     }
     # Remove unused rows
     cat_df <- cat_df %>% filter(cat_word != cat_count)
     cat_df <- arrange(cat_df, desc(cat_count))
     
     # Remove duplicates (some emails can appear twice)
     cat_df <- unique(cat_df)
     
     # Limit lines for writing - Google Sheets limits to 1000 lines
     if(nrow(cat_df) > 1000) {
          cat_df <- cat_df %>% filter(cat_count > 2)
     }
     
     # Write out the results
     output_file_name <- paste(x, " - ", cat_name)
     
     output_file <- gs_new(output_file_name)
     gs_edit_cells(output_file, ws = 1, input = cat_df, anchor = "A1", byrow = FALSE,
                   col_names = NULL, trim = FALSE, verbose = TRUE)
}

# END OF CURRENT CODING EXPERIMENT - NEED TO DETERMINE UNIQUE VOCAB BY CATEGORY

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



