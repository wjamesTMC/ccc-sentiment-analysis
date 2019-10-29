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
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open file
#

# Import and Open the data file / Establish the data set
data_filename <- "19 08 05-11.csv"
data_filename <- "19 08 12-18.csv"
dat <- read.csv(data_filename, stringsAsFactors = FALSE)

vocab_filename <- "0_Input_Vocabulary.csv"
comms          <- read.csv(vocab_filename, stringsAsFactors = FALSE)
pos_vocab      <- comms %>% filter(Tone == "Positive")
neg_vocab      <- comms %>% filter(Tone == "Negative")
neu_vocab      <- comms %>% filter(Tone == "Neutral")

#
# Clean data file to set vector names
#

# Clean up the column names
dat <- rename(dat, replace = c("Clients_CCC__c" = "Client",
                               "Products_Topics_CCC__c" = "Topic",
                               "Type" = "O_Type"))

# Eestablish a new type column to compare ability to predict rigth category
dat<- dat %>% mutate(N_Type = "TBD", M_Count = 0) %>%
     select(Id, Subject, Description, Topic, O_Type, N_Type, M_Count)
     
# Establish a single category for various technical permutatioins
dat[dat == "Technical/Access"]           <- "Technical"
dat[dat == "Technical/Device"]           <- "Technical"
dat[dat == "Technical/Downloading"]      <- "Technical"
dat[dat == "Technical/Feature"]          <- "Technical"
dat[dat == "Technical/Bug/System Issue"] <- "Technical"
dat[dat == "Broken Link"]                <- "Technical"

# Fornow, remove duplicates - but you have to figure out how to trap them
dat <- dat %>% filter(O_Type != "Duplicate", O_Type != "")

#*******************************************************************************
#
# Section 1: Examine description and classify as to Type category
#
#*******************************************************************************

#
# Detect emails that are just out of the office notices
#

match_term <- "Out of office"

# Vocabulary words to check
detect_outofoffice <- c("out office",
                        "out of office",
                        "out of the office",
                        "be on vacation",
                        "away from the office",
                        "not be in the office",
                        "will reply to you",
                        "das Büro nicht besetzt",
                        "our email has been received and will be replied to")

# Check each of the words against the Descriptions
Pattern = paste(detect_outofoffice, collapse = "|")
result <- grepl(Pattern, dat$Description)

# Set up df to collect instances where we are not identifying correctly
res_df <- data.frame("Id" = 1:nrow(dat),
                     "O_Type" = 1:nrow(dat),
                     "N_Type" = 1:nrow(dat))

# If a word does show up, mark it with a "1" and save off the result
for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- match_term
          dat$M_Count[i] <- 1
          res_df[i, 1] <- dat$Id[i]
          res_df[i, 2] <- dat$O_Type[i]
          res_df[i, 3] <- dat$N_Type[i]
     }
}

# Distill the results down to where we do not have a match
reps_log <- nrow(dat %>% filter(dat$O_Type == match_term))
prog_log <- nrow(res_df %>% filter(Id != N_Type))
matches  <- nrow(res_df %>% filter(Id != O_Type) %>% filter(O_Type == N_Type))
accuracy <- (matches / reps_log) * 100

# Compute the percentage of correct matches out of the whole set
cat("Reps identified", reps_log, "instances")
cat("The program detected", prog_log, "instances")
cat("There were", matches, "matches")
cat("The program identified", (matches / reps_log) * 100, "% correctly")
cat("The program identified", prog_log - matches, "incorrectly")

error_log <- res_df %>% filter(Id != N_Type) %>% filter(O_Type != N_Type)

# Clear out the correctly identified items
dat <- dat %>% filter(dat$M_Count == 0)

#
# Detect emails where issue is delivery
#

match_term <- "Delivery"

# Vocabulary
detect_delivery <- c("did not appear",
                     "did not come",
                     "delivery",
                     "DELIVERY",
                     "have not received",
                     "in my mailbox",
                     "order has not been received",
                     "am not receiving",
                     "have stopped getting",
                     "have not been receiving",
                     "HAVE YET TO RECEIVE",
                     "have yet to receive",
                     "has not arrived",
                     "issue missing",
                     "issue is missing",
                     "Shipment Notification",
                     "have shipped the following item",
                     "ntil last week I received")

Pattern = paste(detect_delivery, collapse = "|")
result <- grepl(Pattern, dat$Description)

# Set up df to collect instances where we are not identifying correctly
res_df <- data.frame("Id" = 1:nrow(dat),
                     "O_Type" = 1:nrow(dat),
                     "N_Type" = 1:nrow(dat))

# If a word does show up, mark it with a "1" and save off the result
for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- match_term
          dat$M_Count[i] <- 1
          res_df[i, 1] <- dat$Id[i]
          res_df[i, 2] <- dat$O_Type[i]
          res_df[i, 3] <- dat$N_Type[i]
     }
}

# Distill the results down to where we do not have a match
reps_log <- nrow(dat %>% filter(dat$O_Type == match_term))
prog_log <- nrow(res_df %>% filter(Id != N_Type))
matches  <- nrow(res_df %>% filter(Id != O_Type) %>% filter(O_Type == N_Type))
accuracy <- (matches / reps_log) * 100

# Compute the percentage of correct matches out of the whole set
cat("Reps identified", reps_log, "instances")
cat("The program detected", prog_log, "instances")
cat("There were", matches, "matches")
cat("The program identified", (matches / reps_log) * 100, "% correctly")
cat("The progrem identified", prog_log - matches, "incorrectly")

error_log <- res_df %>% filter(Id != N_Type) %>% filter(O_Type != N_Type)

# Clear out the correctly identified items
dat <- dat %>% filter(dat$M_Count == 0)

#
# Detect emails where user is having a technical problem
#

match_term <- "Technical"

# Vocabulary words to check
detect_tech <- c("404",
                 "an't get in",
                 "access",
                 "attempting",
                 "an't access",
                 "an't get in",
                 "annot get in",
                 "assist me",
                 "annot find",
                 "annot set up",
                 "an't set up",
                 "annot register",
                 "blurry",
                 "broken link",
                 "CAN'T OPEN",
                 "can't open",
                 "can't sign in",
                 "code missing",
                 "cuts out",
                 "cutting out",
                 "does not open",
                 "does not work",
                 "does NOT work",
                 "doesn't open",
                 "doesn't work",
                 "download",
                 "eed help",
                 "eed Help",
                 "error message",
                 "et rid of",
                 "FAQ",
                 "finally found",
                 "font size",
                 "frustrated",
                 "hat happened",
                 "have not received",
                 "haven't received",
                 "haven't rec'd",
                 "hen I log into",
                 "homescreen",
                 "how to send",
                 "s this correct?",
                 "jump",
                 "lank page",
                 "lease help me",
                 "logged in",
                 "login",
                 "lost my app",
                 "magnification",
                 "my IPad",
                 "my iPad",
                 "navigate",
                 "no audio",
                 "not allowing me",
                 "not been able to access",
                 "ot permitted",
                 "no audio",
                 "ot able to",
                 "ot able",
                 "ot available",
                 "ot back on line",
                 "ot installed",
                 "ow do I",
                 "Page Not Found",
                 "password",
                 "Password",
                 "problem",
                 "reroutes",
                 "reset",
                 "restart",
                 "ried",
                 "s it possible",
                 "server",
                 "skipped",
                 "sort this out",
                 "stopped",
                 "the audio",
                 "too long",
                 "trying to access",
                 "trying to check",
                 "swipe",
                 "unable to",
                 "unlock",
                 "urgent",
                 "URGENT")

Pattern = paste(detect_tech, collapse = "|")
result <- grepl(Pattern, dat$Description)

# Set up df to collect instances where we are not identifying correctly
res_df <- data.frame("Id" = 1:nrow(dat),
                     "O_Type" = 1:nrow(dat),
                     "N_Type" = 1:nrow(dat))

# If a word does show up, mark it with a "1" and save off the result
for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- match_term
          dat$M_Count[i] <- 1
          res_df[i, 1] <- dat$Id[i]
          res_df[i, 2] <- dat$O_Type[i]
          res_df[i, 3] <- dat$N_Type[i]
     }
}

# Distill the results down to where we do not have a match
reps_log <- nrow(dat %>% filter(dat$O_Type == match_term))
prog_log <- nrow(res_df %>% filter(Id != N_Type))
matches  <- nrow(res_df %>% filter(Id != O_Type) %>% filter(O_Type == N_Type))
accuracy <- (matches / reps_log) * 100

# Compute the percentage of correct matches out of the whole set
cat("Reps identified", reps_log, "instances")
cat("The program detected", prog_log, "instances")
cat("There were", matches, "matches")
cat("The program identified", (matches / reps_log) * 100, "% correctly")
cat("The program identified", prog_log - matches, "incorrectly")

error_log <- res_df %>% filter(Id != N_Type) %>% filter(O_Type != N_Type)

# Clear out the correctly identified items
dat <- dat %>% filter(dat$M_Count == 0)

#
# Detect emails where the category is  "Junk"
#

match_term <- "Junk"

# Vocabulary
detect_junk <- c("5W PUBLIC RELATIONS",
                 "Aqui",
                 "DocuSign",
                 "@info",
                 "@import",
                 "a new poll",
                 "a new study",
                 "A new study",
                 "acesse aqui",
                 "advertising",
                 "ad budget",
                 "Adword",
                 "Adquira Aqui",
                 "all guns",
                 "Altcoin",
                 "Amtrak",
                 "an opinion I wrote",
                 "an article I wrote",
                 "and HomeAway",
                 "announces",
                 "ATTENTION",
                 "Auto-Estima",
                 "aximize your",
                 "Barrister",
                 "Baston Elevate",
                 "bigdeal",
                 "blog",
                 "Bomba",
                 "book series",
                 "BPO",
                 "buy in",
                 "CALMatters",
                 "CEO",
                 "clergy",
                 "click?",
                 "Click here",
                 "Clique Aqui",
                 "clinical",
                 "combined with other offers",
                 "comedian",
                 "company",
                 "competitors",
                 "CONFERENCE",
                 "conference",
                 "connect on LinkedIn",
                 "content on my website",
                 "constantcontact",
                 "onstant Contact",
                 "onstant contact",
                 "consultant",
                 "contest",
                 "contributing factor of Alzheimer’s",
                 "Corporate Office",
                 "D&S Newsletter",
                 "deadline",
                 "Did you know",
                 "domination",
                 "donation",
                 "earn how to",
                 "egister Now",
                 "egister now",
                 "egister today",
                 "Email: tw0910342479@yahoo.com",
                 "email marketing",
                 "empower",
                 "Enews",
                 "equities market",
                 "ESTIMULANTE",
                 "Eversmile",
                 "ew Video",
                 "ew video",
                 "exporter",
                 "exual",
                 "Experimente Aqui",
                 "f you don't want to receive these emails from Facebook",
                 "factory",
                 "FINALMENTE",
                 "finalmente",
                 "financial assistance",
                 "first look at",
                 "FOR IMMEDIATE RELEASE",
                 "For immediate release",
                 "FREE CHUL",
                 "fuck",
                 "globalmedia",
                 "GoTranscript",
                 "GrandPad",
                 "hacker",
                 "hipping method",
                 "hipping Method",
                 "his offer",
                 "I am Mr",
                 "icrophones",
                 "iew this email in your browser",
                 "iew this email online",
                 "ilitary",
                 "implant",
                 "IMPORTANT ANNOUNCEMENT",
                 "infectious diseases",
                 "info@",
                 "INSTAGRAM",
                 "investment",
                 "ISBN",
                 "Join the conversation",
                 "lementary schools",
                 "licensed",
                 "lick the play button",
                 "linkedin.com",
                 "lucky tickets",
                 "luckytickets",
                 "Macho Man",
                 "manufacturer",
                 "MD",
                 "mission trips",
                 "monster",
                 "MONSTER",
                 "my campaign",
                 "NCIA",
                 "ndustrial",
                 "NEW CAREER",
                 "new career",
                 "newest products",
                 "News Release",
                 "NOSSO VIDEO",
                 "nosso video",
                 "Nova Câmera Discreta",
                 "nstagram",
                 "ntensificador",
                 "o view it",
                 "oday's topic summary",
                 "online marketing",
                 "ou’re receiving this email because",
                 "opportunities",
                 "Para visualizá-lo on-line",
                 "paste this link into your browser:",
                 "pgrade your mail",
                 "pgrade Your Mailbox",
                 "Politique",
                 "ponsor",
                 "Power Erect",
                 "promo code",
                 "psychologist",
                 "ptimization",
                 "quickly promote",
                 "review your purchase",
                 "rewarding job",
                 "Ruslan Kolbaev",
                 "RPO",
                 "Sale",
                 "sarasfarm2",
                 "saving money fo",
                 "eminar",
                 "smear",
                 "Smith Publicity",
                 "stimulante",
                 "5W PUBLIC RELATIONS,",
                 "the content on my website",
                 "The Nation Press Room",
                 "therapy",
                 "to view it",
                 "o view this email online",
                 "traffic",
                 "Trump's",
                 "ttendee",
                 "TurfMutt",
                 "Upcoming Events",
                 "upcoming events",
                 "update your preferences",
                 "URGENT NEWS",
                 "urgent news",
                 "Veja o Vídeo",
                 "vent summary",
                 "vent Summary",
                 "VICTIM",
                 "victim",
                 "Vipmail",
                 "visit our website",
                 "visiting our website",
                 "Vrbo",
                 "WaterBrook",
                 "webafrica",
                 "Welcome to your Daily",
                 "white supremacy",
                 "WHO study",
                 "winning author",
                 "xclusivo",
                 "xhibitor",
                 "xperimente",
                 "your industry",
                 "yo.utube")

# Use Grep to check each of these terms against the Description field
Pattern = paste(detect_junk, collapse = "|")
result <- grepl(Pattern, dat$Description)

# Set up df to collect instances where we are not identifying correctly
res_df <- data.frame("Id" = 1:nrow(dat),
                     "O_Type" = 1:nrow(dat),
                     "N_Type" = 1:nrow(dat))

# If a word does show up, mark it with a "1" and save off the result
for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- match_term
          dat$M_Count[i] <- 1
          res_df[i, 1] <- dat$Id[i]
          res_df[i, 2] <- dat$O_Type[i]
          res_df[i, 3] <- dat$N_Type[i]
     }
}

# Distill the results down to where we do not have a match
reps_log <- nrow(dat %>% filter(dat$O_Type == match_term))
prog_log <- nrow(res_df %>% filter(Id != N_Type))
matches  <- nrow(res_df %>% filter(Id != O_Type) %>% filter(O_Type == N_Type))
accuracy <- (matches / reps_log) * 100

# Compute the percentage of correct matches out of the whole set
cat("Reps identified", reps_log, "instances")
cat("The program detected", prog_log, "instances")
cat("There were", matches, "matches")
cat("The program identified", (matches / reps_log) * 100, "% correctly")
cat("The program identified", prog_log - matches, "incorrectly")

error_log <- res_df %>% filter(Id != N_Type) %>% filter(O_Type != N_Type)

# Clear out the correctly identified items
dat <- dat %>% filter(dat$M_Count == 0)


#
# Detect emails that reflect no answer
#

match_term <- "No Answer"

# Vocabulary
detect_noanswer <- c("Dear Customer",
                     "Kindle Personal Document Service",
                     "Representative's Email",
                     "This note is to confirm that the message with subject '",
                     "Your contact, on account ",
                     "Your form has a new entry. Here are all the answers.",
                     "This note is to confirm that the message with subject",
                     "These update emails are optional",
                     "Daily Thought",
                     "GrandPad",
                     "Ignore this message. Message")

Pattern = paste(detect_noanswer, collapse = "|")
result <- grepl(Pattern, dat$Description)

# Set up df to collect instances where we are not identifying correctly
res_df <- data.frame("Id" = 1:nrow(dat),
                     "O_Type" = 1:nrow(dat),
                     "N_Type" = 1:nrow(dat))

# If a word does show up, mark it with a "1" and save off the result
for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- match_term
          dat$M_Count[i] <- 1
          res_df[i, 1] <- dat$Id[i]
          res_df[i, 2] <- dat$O_Type[i]
          res_df[i, 3] <- dat$N_Type[i]
     }
}

# Distill the results down to where we do not have a match
res_df <- res_df %>% filter(Id != O_Type) %>% filter(O_Type != N_Type)
reps_count <- nrow(dat %>% filter(dat$O_Type == match_term))
error_rate <- nrow(res_df) / reps_count

# Compute the percentage of correct matches out of the whole set
cat("Reps identified", reps_count, "instances")
cat("The program detected", sum(dat$M_Count), "instances")
cat("There were", nrow(res_df), "differences - an accuracy rate of", (1 - error_rate) * 100,"%")


# Collect all instances where the original classification = match term
noa_success <- dat %>% filter(dat$O_Type == match_term) %>% 
     select(Id, M_Count) %>% filter(M_Count == 0)

# Compare the number of identified lines with rep identified
success <- sum(dat$M_Count) / nrow(noa_success)
cat("Success rate for No Answer is:", success * 100, "%")

dat <- dat %>% filter(dat$M_Count == 0)

# Write out the correct matches to the output dataframe
out_df <- rbind(out_df, dat %>% filter(dat$M_Count == 1))

# Remove the matches from the working dataframe to begin the next sequence
dat <- dat %>% filter(dat$M_Count != 1)


##################
#
# CHECK OUTPUT
#
##################

out_df <- out_df %>% mutate(test_value = 0)
for(i in 1:nrow(out_df)) {
     if(out_df$O_Type[i] == out_df$N_Type[i]) {
          out_df$test_value[i] = 1
     }
}

success <- sum(out_df$test_value) / nrow(out_df)
cat("Success rate is:", success)

# File name and then write out the out_df dataframe to the .csv file
output_file <- "New.csv"
out_df <- out_df %>% select(Id, O_Type, N_Type, M_Count, Description)
write.csv(out_df, output_file)

#######################
# CONTINUE ON WITH OTHER CATEGORIES
#######################

#
# Detect emails that deal with billing problems
#

# Vocabulary to check
detect_billing <- c("renew", 
                    "cancel",
                    "contribution",
                    "reasurer",
                    "subscription", 
                    "address", 
                    "credit card",
                    "expired",
                    "expiration",
                    "billing address",
                    "mailing address",
                    "p.o. box",
                    "PO box",
                    "payment",
                    "Zahlungen",
                    "cancel subscription") 

Pattern = paste(detect_billing, collapse = "|")
result <- grepl(Pattern, dat$Description)

for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- "Billing/Payment"
          dat$M_Count[i] <- 1
     }
}

# Write out the correct matches to the output dataframe
out_df <- rbind(out_df, dat %>% filter(dat$M_Count == 1))

# Remove the matches from the working dataframe to begin the next sequence
dat <- dat %>% filter(dat$M_Count != 1)



#
# Detect emails related to account modifications
#

# Vocabulary
detect_acctmodification <- c("membership",
                             "member",
                             "disolve",
                             "dissolution",
                             "branch church",
                             "church records",
                             "account",
                             "my order",
                             "ordered",
                             "ailing",
                             "Email",
                             "email",
                             "name",
                             "Name",
                             "address",
                             "Address",
                             "mailing",
                             "Mailing",
                             "delivery",
                             "ontinue receiving",
                             "embership number",
                             "passed away",
                             "passed on")

Pattern = paste(detect_acctmodification, collapse = "|")
result <- grepl(Pattern, dat$Description)

for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- "Account Modification"
          dat$M_Count[i] <- 1
     }
}

# Write out the correct matches to the output dataframe
out_df <- rbind(out_df, dat %>% filter(dat$M_Count == 1))

# Remove the matches from the working dataframe to begin the next sequence
dat <- dat %>% filter(dat$M_Count != 1)



for(i in 1:nrow(dat)) {
     if(result[i] == TRUE) {
          dat$N_Type[i] <- "Delivery"
          dat$M_Count[i] <- 1
     }
}

# Write out the correct matches to the output dataframe
out_df <- rbind(out_df, dat %>% filter(dat$M_Count == 1))

# Remove the matches from the working dataframe to begin the next sequence
dat <- dat %>% filter(dat$M_Count != 1)


# House orders
house order

# Correction/Typo
Share your Comments
correction
corrected



# Design/format
more readable
gray type
Italics
struggle with the type
#
# output_file <- gs_new("CCC Test_191002", input = dat)
#



##################################################
# CURRENT END
#################################################$

test_sum <- nrow(dat %>% filter(O_Type == "Junk"))
success <- sum(dat$M_Count) / test_sum
cat("Success rate is:", success)
# Open historical data file and append current week's data


detect_unsubscribe


detect_genquestion
detect_gift
"gift subscriptioin"
"one year gift"

detect_misdirected
"ent package"

detect_newsletter
detect_Permission/Copyright
detect_refund
detect_renewal
detect_search
detect_submission
detect_subscribe
detect_suggestiondetect_delivery
"ave not received"
detect_correction
detect_legal
"permission to use"
"permission to copy"

detect_duplicate

#
# Section 2: Classify by product
#

#
# Section 3: classify as positive, negative, or neutral
#

detect_feedbackpos
detect_feedbackneg
detect_feddbackneu


# Change remaining NA values to "other"
detect_other


dat$N_Type



# Remove lines with known not relevant strings
for(i in 1:nrow(dat)) {
     if(str_detect(dat$Description[i], "To view this email online,") == TRUE) {
          dat$N_Type[i] <- "Junk"
     }
     if(str_detect(dat$Description[i], "FOR IMMEDIATE RELEASE") == TRUE) {
          dat$N_Type[i] <- "Junk"
     }
}

# dat$N_Type
#        
# dat <- dat %>% filter(!str_detect(Description, 'Ignore this message'))
# dat <- dat %>% filter(!str_detect(Description, 'out of the office'))
# dat <- dat %>% filter(!str_detect(Description, 'for immediate release'))
# dat <- dat %>% filter(!str_detect(Description, 'Kindle Personal Document Service'))
# dat <- dat %>% filter(!str_detect(Description, 'connect on LinkedIn'))
# dat <- dat %>% filter(!str_detect(Description, ' Please share this with your members'))
# dat <- dat %>% filter(!str_detect(Description, 'Sent from my Samsung device'))
# dat <- dat %>% filter(!str_detect(Description, 'This note is to confirm that the message'))
# dat <- dat %>% filter(!str_detect(Description, 'Sent from my iPhone'))
# dat <- dat %>% filter(!str_detect(Description, 'Longyear Museum'))
# dat <- dat %>% filter(!str_detect(Description, 'View this email in your browser'))
# dat <- dat %>% filter(!str_detect(Description, 'dataprotection+unsubscribe@csps.com'))
# dat <- dat %>% filter(!str_detect(Description, 'Test Comment'))
# dat <- dat %>% filter(!str_detect(Description, 'murdered'))

# Remove lines where Type is junk or misdirected or otherwise erroneous
# dat <- dat %>% filter(Type != "Junk")
# dat <- dat %>% filter(Type != "Duplicate")
# dat <- dat %>% filter(Type != "Misdirected")
# dat <- dat %>% filter(Type != "")
# dat <- dat %>% filter(Type != "Out of Office")
# dat <- dat %>% filter(Type != "No Answer")
# dat <- dat %>% filter(Description != "")

Subj_list   <- unique(dat$Subject)
Client_list <- unique(dat$Client)
Topic_list  <- unique(dat$Topic)
Type_list   <- unique(dat$N_Type)

# Verify the number of unique values of the various factors
cat("Summary of input file after cleanup")
cat("Unique IDs:          ", sapply(dat, function(x)length(unique(x)))[1])
cat("Unique Subjects:     ", sapply(dat, function(x)length(unique(x)))[2])
cat("Unique Descriptions: ", sapply(dat, function(x)length(unique(x)))[3])
cat("Unique Clients:      ", sapply(dat, function(x)length(unique(x)))[4])
cat("Unique Topics:       ", sapply(dat, function(x)length(unique(x)))[5])
cat("Unique Categories:   ", sapply(dat, function(x)length(unique(x)))[6])

# Calc the number of comments (remove "NR" from the count)
num_descs <- sapply(dat, function(x)length(unique(x)))[3]

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

# Build dataframe for neutral words
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

# Create dataframe and append negatives and neutrals
cum_count_df <- pos_df
cum_count_df <- rbind(cum_count_df, neg_df)
cum_count_df <- rbind(cum_count_df, neu_df)

# Determine overall positive and negative indexes (pos / neg words / comments
pos_index <- sum(pos_df$Count) / num_descs
neg_index <- sum(neg_df$Count) / num_descs
neu_index <- sum(neu_df$Count) / num_descs

# Create a filename and write out the list of P-N-N vocabulary detected
filename <- paste("0_Output_SA_", data_filename)
filename <- stri_replace_all_fixed(filename, " ", "")
write.csv(cum_count_df, file = filename)

# Save off the cleaned up data file
filename <- paste("0_Output_DF_", data_filename)
filename <- stri_replace_all_fixed(filename, " ", "")
write.csv(dat, file = filename)

cat(" Number of input lines           :", nrow(dat), "\n",
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

# Set threshold for negative words
neg_limit <- 2

# What we want to do here is look through the cut-down data file, examine each
# description, count the number of negative words, and save off that particular
# entry so they can be written to a file for management to examine

# First build a data frame to hold the identified entries. Set the size to 
# equal the number of descriptions and later clip off unused rows
neg_desc_df <- data.frame(Desc_ID = num_descs, Desc_text = num_descs, Neg_words = num_descs)

# Loop to test each negative word against each description
neg_word_count <- 0
df_counter     <- 1

for(i in 1:num_descs) {
     
     # This line removeS punctuation in the description to leave just text
     dat$Desc[i] <- gsub("[[:punct:]]", "", dat$Desc[i])
     
     # Now remove all line breaks
     dat$Desc[i] <- gsub("[\r\n]", "", dat$Desc[i])
     
     for(j in 1:nrow(neg_vocab)) {
          
          # Look for instances where a negative word is found
          x <- str_detect(dat$Desc[i], neg_vocab$Term[j])
          
          # if we find an instance, count it
          if(x == TRUE) {
               neg_word_count = neg_word_count + 1
          }
     }
     if(neg_word_count > neg_limit) {
          neg_desc_df[df_counter, 1] <- dat$Id[i]
          neg_desc_df[df_counter, 2] <- dat$Desc[i]
          neg_desc_df[df_counter, 3] <- neg_word_count
          df_counter <- df_counter + 1
     }
     # Reset the neg word counter to 0
     neg_word_count <- 0
}

# Print out results
for(i in 1:nrow(neg_desc_df)) {
     cat("Number ", i, "of", nrow(neg_desc_df), "- ID is: ", neg_desc_df[i,1], " ", "Negative words: ", neg_desc_df[i,3], "\n", "\n")
     cat(substr(neg_desc_df[i,2], start = 1, stop = 500), "\n", "\n")
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


