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

# Check that no one took the HIT twice
duplicated(subjinfo$workerid)

# Exclude from data anyone who responded "no" to assessment Q
subjinfo <- subjinfo %>% filter(assess == 'Yes')
yes_subjs <- subjinfo %>% filter(assess == 'Yes')
trialinfo <- trialinfo %>% filter(workerid %in% yes_subjs$anon_workerid)

# Explore subject demographics
dplyr::count(subjinfo,language)
dplyr::count(subjinfo,enjoyment)
dplyr::count(subjinfo,assess)
dplyr::count(subjinfo,gender)
dplyr::count(subjinfo,education)

# Histogram of time taken, age
ggplot(subjinfo, aes(x=time_taken)) + geom_histogram(binwidth=1.5) + ggtitle('Distribution of time taken to complete HIT') + xlab('Time taken (minutes)') + ylab('Number of subjects')
ggplot(subjinfo, aes(x=age)) + geom_histogram(binwidth=5) + ylab('Number of subjects') + xlab('Age (years)') + ggtitle('Distribution of subject age')

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

get_q1_stance <- function(s) {
  s <- str_replace(str_replace(s,"\\[",""),"\\]","")
  split_s <- strsplit(s,", ")[[1]]
  resp_1 <- strsplit(split_s[2],"\\: ")[[1]][2]
  resp_1 <- as.numeric(str_replace(str_replace(resp_1,"\\}","")[1],"\\]",''))
  return(resp_1)
}

get_q2_stance <- function(s) {
  s <- str_replace(str_replace(s,"\\[",""),"\\]","")
  split_s <- strsplit(s,", ")[[1]]
  resp_1 <- strsplit(split_s[4],"\\: ")[[1]][2]
  resp_1 <- as.numeric(str_replace(str_replace(resp_1,"\\}","")[1],"\\]",''))
  return(resp_1)
}

get_q4_stance <- function(s) {
  s <- str_replace(str_replace(s,"\\[",""),"\\]","")
  split_s <- strsplit(s,", ")[[1]]
  resp_1 <- strsplit(split_s[8],"\\: ")[[1]][2]
  resp_1 <- as.numeric(str_replace(str_replace(resp_1,"\\}","")[1],"\\]",''))
  return(resp_1)
}

subjinfo$own_stance_index <- modify(subjinfo$own_stance,get_own_stance)
subjinfo$own_stance_1 <- modify(subjinfo$own_stance,get_q1_stance)
subjinfo$own_stance_index <- as.numeric(subjinfo$own_stance_index)
ggplot(subjinfo, aes(x=own_stance_index)) + geom_histogram()

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

stims <- vector(mode="list", length=30)
stim_ids <- vector(mode="list", length=30)
stim_stance <- vector(mode="list", length=30)
stim_texts <- c("the benefits of getting vaccinated far outweigh the risks",
                "many diseases can be contained with vaccines",
                "vaccines are a safe and effective way to save lives",
                "vaccines are effective at preventing diseases like measles",
                "there is no link between vaccines and autism",
                "the link between vaccines and autism is unfounded",
                "vaccines do not cause autism",
                "without the vaccine",
                "vaccines have saved countless lives",
                "we have basically eradicated measles",
                "children should be required to get vaccinated",
                "vaccinating children is a crucial measure for public health",
                "concerns over vaccine injuries are generally unfounded",
                "the vaccine-autism link is entirely a myth",
                "the study linking vaccines to autism has been debunked",
                "vaccines were responsible for some cases of autism in children",
                "thimerosol",
                "the mercury contained in vaccines is linked to autism risk",
                "vaccines are not as effective as people think",
                "vaccines pose non-trivial risks",
                "parents should have a choice in whether they vaccinate their children",
                "vaccine benefits are exaggerated and vaccine risks are downplayed by pharmaceutical companies",
                "many children have shown adverse reactions to vaccines",
                "getting vaccinated can pose serious risks",
                "vaccines are not as safe as many people imagine",
                "the campaign to get children vaccinated is driven in large part by drug companies",
                "people have sometimes still gotten sick",
                "vaccines do not guarantee immunity",
                "the absolute safety of vaccines is a misconception",
                "we need to be more aware of the risks posed by vaccines")
names(stims) <- c("the benefits of getting vaccinated far outweigh the risks",
"many diseases can be contained with vaccines",
"vaccines are a safe and effective way to save lives",
"vaccines are effective at preventing diseases like measles",
"there is no link between vaccines and autism",
"the link between vaccines and autism is unfounded",
"vaccines do not cause autism",
"without the vaccine",
"vaccines have saved countless lives",
"we have basically eradicated measles",
"children should be required to get vaccinated",
"vaccinating children is a crucial measure for public health",
"concerns over vaccine injuries are generally unfounded",
"the vaccine-autism link is entirely a myth",
"the study linking vaccines to autism has been debunked",
"vaccines were responsible for some cases of autism in children",
"thimerosol",
"the mercury contained in vaccines is linked to autism risk",
"vaccines are not as effective as people think",
"vaccines pose non-trivial risks",
"parents should have a choice in whether they vaccinate their children",
"vaccine benefits are exaggerated and vaccine risks are downplayed by pharmaceutical companies",
"many children have shown adverse reactions to vaccines",
"getting vaccinated can pose serious risks",
"vaccines are not as safe as many people imagine",
"the campaign to get children vaccinated is driven in large part by drug companies",
"people have sometimes still gotten sick",
"vaccines do not guarantee immunity",
"the absolute safety of vaccines is a misconception",
"we need to be more aware of the risks posed by vaccines")

for (i in 1:30) {
  stims[[i]] <- i
  stim_ids[[stim_texts[i]]] <- i
  if (i <= 15) {
    stim_stance[[stim_texts[i]]] <- "pro-vax comp. clause"
  }
  else {
    stim_stance[[stim_texts[i]]] <- "anti-vax comp. clause"
  }
}

cc_to_float <- function(c) {
  return(stim_stance[[c]])
}

cc_to_id <- function(c) {
  return(stim_ids[[c]])
}

trialinfo$subj <- modify(trialinfo$stim,get_stim_subj)
trialinfo$verb <- modify(trialinfo$stim,get_stim_verb)
trialinfo$verb_cat <- modify(trialinfo$verb,get_stim_verb_cat)
trialinfo$cc <- modify(trialinfo$stim,get_stim_cc)
trialinfo$cc_float <- modify(trialinfo$cc,cc_to_float)
trialinfo$cc_id <- modify(trialinfo$cc,cc_to_id)

dplyr::count(trialinfo,verb) # should all be 72 (24 subjs*3)
dplyr::count(trialinfo,subj) # may be different since randomly chosen per stim

# Remove workerid
subjinfo <- select (subjinfo,-c(workerid))

# Join subj info to trialinfo
trialinfo <- inner_join(trialinfo,subjinfo,by=c("workerid" = "anon_workerid"))

# Save reformatted data
write_csv(subjinfo, "../data/reformatted_subjinfo.csv", na = "NA", append = FALSE, col_names = TRUE,
          quote_escape = "double")
write_csv(trialinfo, "../data/reformatted_trialinfo.csv", na = "NA", append = FALSE, col_names = TRUE,
          quote_escape = "double")
