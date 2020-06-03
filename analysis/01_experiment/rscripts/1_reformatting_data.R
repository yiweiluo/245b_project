# Data wrangling in the R tidyverse

# Set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

#Load necessary packages
library(tidyverse)
library(lme4)

# Load datasets. R will automatically read the contents of these files into tibbles (which are tidyverse versions of data.frames).
subjinfo = read_csv("../data/vaccine_preds_pilot_1_LIVE-subject-info.csv")
head(subjinfo)
trialinfo = read_csv("../data/vaccine_preds_pilot_1_LIVE-trials.csv")
head(trialinfo)
idinfo = read_csv("../data/vaccine_preds_pilot_1_LIVE-workerids.csv")
head(idinfo)
subjinfo$anon_workerid <- idinfo$anon_workerid

# Regularize subject info
reg_enjoy <- function(x) {
  if (x == 0){
    result <- ('worse than average')
  }
  else if (x == 1){
    result <- ('average')
  }
  else if (x == 2){
    result <- ('better than average')
  }
  else {
    result <- x
  }
  return(result)
}

reg_education <- function(x) {
  if (x == 0){
    result <- ('some HS')
  }
  else if (x == 1){
    result <- ('graduated HS')
  }
  else if (x == 2){
    result <- ('some college')
  }
  else if (x == 3){
    result <- ('graduated college')
  }
  else {
    result <- ('higher degree')
  }
  return(result)
}

subjinfo$language <- tolower(subjinfo$language)
subjinfo$enjoyment <- modify(subjinfo$enjoyment,reg_enjoy)
subjinfo$education <- modify(subjinfo$education,reg_education)

# Explore subject demographics
dplyr::count(subjinfo,language)
dplyr::count(subjinfo,enjoyment)
dplyr::count(subjinfo,assess)
dplyr::count(subjinfo,gender)
dplyr::count(subjinfo,education)

# Histogram of time taken, age
ggplot(subjinfo, aes(x=time_taken)) + geom_histogram()
ggplot(subjinfo, aes(x=age)) + geom_histogram()

# Check that no one took the HIT twice
duplicated(subjinfo$workerid)

# Exclude from data anyone who responded "no" to assessment Q
subjinfo <- subjinfo %>% filter(assess == 'Yes')
yes_subjs <- subjinfo %>% filter(assess == 'Yes')
trialinfo <- trialinfo %>% filter(workerid %in% yes_subjs$anon_workerid)

# Get a single measure of subj own stance
get_own_stance <- function(s) {
  s <- str_replace(str_replace(s,"\\[",""),"\\]","")
  split_s <- strsplit(s,", ")[[1]]
  resp_1 <- strsplit(split_s[2],"\\: ")[[1]][2]
  resp_2 <- strsplit(split_s[4],"\\: ")[[1]][2]
  resp_3 <- strsplit(split_s[6],"\\: ")[[1]][2]
  resp_4 <- strsplit(split_s[8],"\\: ")[[1]][2]
  resp_1 <- as.numeric(str_replace(str_replace(resp_1,"\\}","")[1],"\\]",''))
  resp_2 <- as.numeric(str_replace(str_replace(resp_2,"\\}","")[1],"\\]",''))
  resp_3 <- as.numeric(str_replace(str_replace(resp_3,"\\}","")[1],"\\]",''))
  resp_4 <- as.numeric(str_replace(str_replace(resp_4,"\\}","")[1],"\\]",''))
  return(resp_1 + resp_2 - resp_3 + resp_4)
}

subjinfo$own_stance_index <- modify(subjinfo$own_stance,get_own_stance)
ggplot(subjinfo, aes(x=anon_workerid,y=own_stance_index)) + geom_point()

# New columns in trialinfo for subject, verb, comp-clause for each stimulus
get_stim_subj <- function(s) {
  split_s <- strsplit(s,", ")
  subj <- str_replace_all(strsplit(split_s[[1]][1],"\\: ")[[1]][2],"\'","")
  return(subj)
}

get_stim_verb <- function(s) {
  split_s <- strsplit(s,", ")
  verb <- str_replace_all(strsplit(split_s[[1]][2],"\\: ")[[1]][2],"\'","")
  return(verb)
}

get_stim_cc <- function(s) {
  split_s <- strsplit(s,", ")
  cc <- str_replace(str_replace_all(strsplit(split_s[[1]][3],"\\: ")[[1]][2],"\'",""),"\\}",'')
  return(cc)
}

get_stim_verb_cat <- function(verb) {
  if (verb %in% c("argue","claim","insist")) {
    cat <- "neg"
  }
  else if (verb %in% c("find","point out","show","suspect")) {
    cat <- "pos"
  }
  else {
    cat <- "neut"
  }
  return(cat)
}

trialinfo$subj <- modify(trialinfo$stim,get_stim_subj)
trialinfo$verb <- modify(trialinfo$stim,get_stim_verb)
trialinfo$verb_cat <- modify(trialinfo$verb,get_stim_verb_cat)
trialinfo$cc <- modify(trialinfo$stim,get_stim_cc)

dplyr::count(trialinfo,verb) # should all be 72 (24 subjs*3)
dplyr::count(trialinfo,subj) # may be different since randomly chosen per stim

# Remove workerid
subjinfo <- select (subjinfo,-c(workerid))

# Save reformatted data
write_csv(subjinfo, "../data/reformatted_subjinfo.csv", na = "NA", append = FALSE, col_names = TRUE,
          quote_escape = "double")
write_csv(trialinfo, "../data/reformatted_trialinfo.csv", na = "NA", append = FALSE, col_names = TRUE,
          quote_escape = "double")
