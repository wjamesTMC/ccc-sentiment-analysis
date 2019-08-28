##############################################################################
#
# CCC Sentiment Analysis 
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/ccc-sentiment analysis.git
#
##############################################################################

#--------------------------------------------------------------------
#
#
# Library setups
#

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
library(expss)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)
library(rmarkdown)
library(kableExtra)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open file
#


# Import and Open the data file / Establish the data set

# data_filename <- readline("What file do you want to work with? [YY MM DD-DD.csv]? ")
data_filename <- "19 08 05-11.csv"
dat <- read.csv(data_filename, stringsAsFactors = FALSE)

vocab_filename <- "0_Input_Vocabulary.csv"
comms          <- read.csv(vocab_filename, stringsAsFactors = FALSE)
pos_vocab      <- comms %>% filter(Tone == "Positive")
neg_vocab      <- comms %>% filter(Tone == "Negative")
neu_vocab      <- comms %>% filter(Tone == "Neutral")

#
# Clean data file to set vector names
#

dat <- rename(dat, replace = c("Clients_CCC__c" = "Client",
                               "Products_Topics_CCC__c" = "Topic"))
              
# Select the desired columns
dat <- dat %>% select(Subject, Description, Client, Topic, Type)

# Replace missing values (zeros) with NR and shorten "do not use"
dat[dat == 0]                    <- "NR"
dat[dat == ""]                   <- "NR"
dat[dat == "No"]                 <- "NR"
dat[dat == "no"]                 <- "NR"
dat[dat == "No further comment"] <- "NR"
dat[dat == "none"]               <- "NR"
dat[dat == "Junk"]               <- "NR"
dat[dat == "No Answer"]          <- "NR"

Subj_list   <- unique(dat$Subject)
Client_list <- unique(dat$Client)
Topic_list  <- unique(dat$Topic)
Type_list   <- unique(dat$Topic)

# Verify the number of unique values of the various factors
sapply(dat, function(x)length(unique(x)))

# Calc the number of comments (remove "NR" from the count)
num_descs <- length(unique(dat$Description))

#--------------------------------------------------------------------
#
# Sentiment analysis - Cumulative
#
#--------------------------------------------------------------------

#
# Positive vocabulary elements
#

# Build dataframe for positives
pos_df   <- data.frame(Word  = pos_vocab$Term,
                       Count = 1:nrow(pos_vocab),
                       Type  = "Pos")

# Loop to identify positive words in the comments field
pct <- 0
for(i in 1:nrow(pos_vocab)) {
  x <- str_detect(dat$Desc, pos_vocab$Term[i])
  pos_df[i, 2] <- length(x[x == TRUE])
  pct <- pct + length(x[x == TRUE])
}

# Remove words with zero counts
pos_df  <- pos_df %>% filter(Count != 0)

# Sort from high to low
pos_df <- arrange(pos_df, desc(Count), Word)

# Print out the top 10 words
pos_df[1:10, ]

#
# Negative vocabulary elements
#

# Build dataframe for negatives
neg_df   <- data.frame(Word  = neg_vocab$Term,
                       Count = 1:nrow(neg_vocab),
                       Type  = "Neg")


# Loop to identify negative words in the comments field
nct <- 0
for(i in 1:nrow(neg_vocab)) {
  x <- str_detect(dat$Desc, neg_vocab$Term[i])
  neg_df[i, 2] <- length(x[x == TRUE])
  nct <- nct + length(x[x == TRUE])
}

# Remove words with zero counts
neg_df   <- neg_df %>% filter(Count != 0)

# Sort from high to low
neg_df <- arrange(neg_df, desc(Count), Word)

# Print out the top 10 words
neg_df[1:10, ]

#
# Neutral vocabulary elements
#

# Build dataframe for negatives
neu_df   <- data.frame(Word  = neu_vocab$Term,
                       Count = 1:nrow(neu_vocab),
                       Type  = "Neu")


# Loop to identify neutral words in the comments field
neu_ct <- 0
for(i in 1:nrow(neu_vocab)) {
  x <- str_detect(dat$Desc, neu_vocab$Term[i])
  neu_df[i, 2] <- length(x[x == TRUE])
  neu_ct <- neu_ct + length(x[x == TRUE])
}

# Remove words with zero counts
neu_df   <- neu_df %>% filter(Count != 0)

# Sort from high to low
neu_df <- arrange(neu_df, desc(Count), Word)

# Print out the top 10 words
neu_df[1:10, ]

# Create datafrane and append negatives and neutrals
cum_count_df <- pos_df
cum_count_df <- rbind(cum_count_df, neg_df)
cum_count_df <- rbind(cum_count_df, neu_df)

# Determine overall positive and negative indexes (pos / neg words / comments
pos_index <- sum(pos_df$Count) / num_descs
neg_index <- sum(neg_df$Count) / num_descs
neu_index <- sum(neu_df$Count) / num_descs

# Create a filename and write out the results
filename <- paste("0_Output_SA_", data_filename, ".csv")
filename <- stri_replace_all_fixed(filename, " ", "")
write.csv(cum_count_df, file = filename)

cat(" Number of input lines          :", nrow(dat), "\n",
    "Number of descriptions          :", num_descs, "\n",
    "Ratio of descriptions to lines  :", num_descs / nrow(dat), "\n", "\n",
    "Number of positive words        :", pct, "\n",
    "Positive words to descs ratio   :", pos_index, "\n", "\n",
    "Number of negative words        :", nct, "\n",
    "Negative words to descs ratio   :", neg_index, "\n", "\n",
    "Number of neutral words         :", neu_ct, "\n",
    "Neutral words to descs ratio    :", neu_index, "\n")

# Display results and statistics
cat("Vocabulary word counts / occurrences")
# kable(pos_df) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# kable(neg_df) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

pos_df
neg_df
neu_df

#--------------------------------------------------------------------
#
# Sentiment analysis - Identify descriptions with high negative rating
# 
#--------------------------------------------------------------------

as.numeric(neg_limit <- readline("How many negative words? "))
neg_desc_df <- data.frame(Desc_num = 100, Desc_text = 100, Num_neg = 100)

nct <- 0
j   <- 1
for(i in 1:nrow(neg_vocab)) {
     x <- str_detect(dat$Desc, neg_vocab$Term[i])
     neg_df[i, 2] <- length(x[x == TRUE])
     if(neg_df[i, 2] >= neg_limit) { 
          neg_desc_df[j, 1] <- i
          neg_desc_df[j, 2] <- dat$Desc[i]
          neg_desc_df[j, 3] <- length(x[x == TRUE])
          j <- j + 1
          }
     nct <- nct + length(x[x == TRUE])
}

head(neg_desc_df, 5)

#--------------------------------------------------------------------
#
# Sentiment analysis - by Week
# 
#--------------------------------------------------------------------

# Set the number of surveys 
survey_num <- length(unique(dat$Surveyed))

# Dataframe to collect all the data and calculations
survey_inf <- data.frame(Survey         = survey_num,
                         Resp           = survey_num,
                         Comms          = survey_num,
                         c_to_r_ratio   = survey_num,
                         PosWds         = survey_num,
                         pw_to_c_ratio  = survey_num,
                         NegWds         = survey_num,
                         nw_to_c_ratio  = survey_num,
                         NeuWds         = survey_num,
                         neu_to_c_ratio = survey_num)

# Loop to examine each survey and assign values to a dataframe
for(i in 1:survey_num) {
  
  # Set the specific survey data
  survey_dat  <- dat %>% filter(Surveyed == unique(dat$Surveyed)[i])
  survey_comments <- length(unique(survey_dat$Comments))
  ifelse(str_detect(survey_dat$Comments, "NR"),
         survey_comments <- survey_comments- 1, survey_comments)
  
  #
  # Positive vocabulary elements
  #
  
  # Build dataframe
  sur_pos_df <- data.frame(Survey = unique(dat$Surveyed)[i],
                           Word   = pos_vocab$Term,
                           Count  = 0,
                           Type   = "Pos")
  
  # Loop to count the occurrences of positive words
  pct <- 0
  for(j in 1:nrow(pos_vocab)) {
     x <- str_detect(survey_dat$Comments, pos_vocab$Term[j])
     sur_pos_df[j, 3] <- length(x[x == TRUE])
     pct <- pct + length(x[x == TRUE])
  }

  # Remove words with zero counts
  sur_pos_df  <- sur_pos_df %>% filter(Count != 0)
  
  # Sort from high to low
  sur_pos_df <- arrange(sur_pos_df, desc(Count), Word)
  sur_pos_df
  
  #
  # Negative vocabulary elements
  #

  # Build dataframe
  sur_neg_df <- data.frame(Survey = unique(dat$Surveyed)[i],
                           Word   = neg_vocab$Term,
                           Count  = 0,
                           Type   = "Neg")
  
  # Loop to count the occurrences of negative words
  nct <- 0
  for(j in 1:nrow(neg_vocab)) {
    x <- str_detect(survey_dat$Comments, neg_vocab$Term[j])
    sur_neg_df[j, 3] <- length(x[x == TRUE])
    nct <- nct + length(x[x == TRUE])
  }

  # Remove words with zero counts
  sur_neg_df  <- sur_neg_df %>% filter(Count != 0)
  
  # Sort from high to low
  sur_neg_df <- arrange(sur_neg_df, desc(Count), Word)
  sur_neg_df
  
  #
  # Neutral vocabulary elements
  #
  
  # Build dataframe
  sur_neu_df <- data.frame(Survey = unique(dat$Surveyed)[i],
                          Word   = neu_vocab$Term,
                          Count  = 0,
                          Type   = "Neu")
  
  # Loop to count the occurrences of neutral words
  neu_ct <- 0
  for(j in 1:nrow(neu_vocab)) {
     x <- str_detect(survey_dat$Comments, neu_vocab$Term[j])
     sur_neu_df[j, 3] <- length(x[x == TRUE])
     neu_ct <- neu_ct + length(x[x == TRUE])
  }
  
  # Remove words with zero counts
  sur_neu_df  <- sur_neu_df %>% filter(Count != 0)

  # Sort from high to low
  sur_neu_df <- arrange(sur_neu_df, desc(Count), Word)
  sur_neu_df
  
  # Create combined dataframe
  sur_cum_df <- sur_pos_df
  
  # Append to the cumulative dataframe
  sur_cum_df <- rbind(sur_cum_df, sur_neg_df)
  # sur_cum_df <- rbind(sur_cum_df, sur_neu_df)
  
  # Create the unique filename by survey and write out the results
  filename <- paste("0_Output_", sur_cum_df[i,1],".csv")
  filename <- stri_replace_all_fixed(filename, " ", "")
  write.csv(sur_cum_df, file = filename)
  
  #
  # Print results of individual surveys
  #
  
  #kable(sur_cum_df) %>%
  #  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  
  # Populate summary dataframe
  survey_inf[i, 1] <- unique(dat$Surveyed)[i]
  survey_inf[i, 2] <- nrow(survey_dat)
  survey_inf[i, 3] <- survey_comments
  survey_inf[i, 4] <- round(survey_comments / nrow(survey_dat), digits = 2)
  survey_inf[i, 5] <- pct
  survey_inf[i, 6] <- round(pct / survey_comments, digits = 2)
  survey_inf[i, 7] <- nct
  survey_inf[i, 8] <- round(nct / survey_comments, digits = 2)
  survey_inf[i, 9] <- neu_ct
  survey_inf[i,10] <- round(neu_ct / survey_comments, digits = 2)
    
  # Display results and statistics
  cat("Results for", survey_inf[i, 1], "\n")
  cat("Number of responses             :", survey_inf[i, 2], "\n")
  cat("Number of comments              :", survey_inf[i, 3], "\n")
  cat("Comments to responses ratio     :", survey_inf[i, 4], "\n")
  cat("Number of positive words        :", survey_inf[i, 5], "\n")
  cat("Positive words to comments ratio:", survey_inf[i, 6], "\n")
  cat("Number of negative words        :", survey_inf[i, 7], "\n")
  cat("Negative words to comments ratio:", survey_inf[i, 8], "\n")
  cat("Number of neutral words         :", survey_inf[i, 9], "\n")
  cat("Neutral words to comments ratio :", survey_inf[i,10], "\n")
  cat("\n")
  
}

survey_inf

#--------------------------------------------------------------------
#
# Build graphics from summary dataframe
#
#--------------------------------------------------------------------

# Number of coments and responses by survey
num_c_and_r <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=Resp, color = "Responses"), group=1, size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments"), group=1, size=2) +
  scale_colour_manual("", 
                      breaks = c("Responses", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Count of Comments and Responses", subtitle = "Numbers of each by Survey") + ylab("Number") +
  theme(legend.position = c(0.18,0.85))

# Ratio of comments to responses
ratio_c_to_r <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=c_to_r_ratio, color = "Ratios", group=1), size=2) +
  scale_colour_manual("", breaks = c("Ratios"), values = c("mediumblue")) +
  labs(title = "Ratio of Comments to Responses", subtitle = "Ratio By Survey") + ylab("Proportion of Comments") +
  theme(legend.position = c(0.15,0.88))

# Arrange the two plots for pasting into deck
grid.arrange(num_c_and_r, ratio_c_to_r, ncol = 2)

# Positive words vs. comments
pw_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=PosWds, color = "Positive Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Positive Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.2,0.85))

# Negative words vs. comments
nw_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=NegWds, color = "Negative Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Negative Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Negative Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.22,0.85))

# Neutral words vs. comments
neu_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=NeuWds, color = "Neutral Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Neutral Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Neutral Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.22,0.85))

# Arrange the two plots for pasting into deck
grid.arrange(pw_vs_c, nw_vs_c, neu_vs_c, ncol = 3)

# Positive and negative words to comments ratios
p_vs_n <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=pw_to_c_ratio, color = "Positive", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=nw_to_c_ratio, color = "Negative", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=neu_to_c_ratio, color = "Neutral", group=1), size=2) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Negative Words", "Neutral Words"),
                      values = c("indianred4", "gray40", "green4")) +
  labs(title = "Ratios of Positive Negative & Neutral Words to Comments", subtitle = "Ratio Comparisons by Survey") +
  ylab("# Words / # Comments") 

# Arrange the two plots for pasting into deck
grid.arrange(p_vs_n, ncol = 2)


#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


