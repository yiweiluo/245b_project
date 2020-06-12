# Plotting data

library(languageR)
library(lme4)
library(ggplot)
library(tidyverse)

# Set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load reformatted data
subjinfo = read_csv("../data/reformatted_subjinfo.csv")
head(subjinfo)
trialinfo = read_csv("../data/reformatted_trialinfo.csv")
head(trialinfo)

# Visualize prior belief
trialinfo$prior_belief[trialinfo$own_stance_index< 2] <- "anti-vax"
trialinfo$prior_belief[trialinfo$own_stance_index>= 2] <- "pro-vax"
trialinfo$belief_strength <- cut(trialinfo$own_stance_index,
                     breaks=c(-Inf, 1, 2.33, Inf),
                     labels=c("strong","weak","strong"))
subjinfo$prior_belief[subjinfo$own_stance_index< 2] <- "anti-vax"
subjinfo$prior_belief[subjinfo$own_stance_index>= 2] <- "pro-vax"
subjinfo$belief_strength <- cut(subjinfo$own_stance_index,
                                breaks=c(-Inf, 1, 2.33, Inf),
                                labels=c("strong","weak","strong"))
ggplot(subjinfo, aes(x=prior_belief, color=belief_strength, fill=belief_strength)) + geom_bar() +
  ylab("Number of participants") +
  xlab("Participant prior belief") + theme_minimal()
ggplot(subjinfo, aes(x=own_stance_index)) + geom_histogram() + 
  ylab("Count") +
  xlab("Prior belief index") + theme_minimal()

# Age
ggplot(subjinfo, aes(x=age)) + geom_histogram(bins = 10) + ylab("Number of participants") +
  xlab("Age (years)") + theme_minimal()

# Check normality of response
ggplot(trialinfo, aes(x=response)) + geom_density()

# Mean on aggregate for each predicate category
ggplot(trialinfo, aes(x=verb_cat, y=response)) + geom_boxplot() + xlab('Predicate category') + ylab('Level of agreement') + ggtitle('Agreement by predicate category')

# Mean on aggregate for each predicate
ggplot(trialinfo, aes(x=reorder(verb, -response, FUN=median), y=response)) + geom_boxplot() + xlab('Predicate') + ylab('Level of agreement') + ggtitle('Agreement by predicate')

# By-subject mean for each predicate category
ggplot(trialinfo, aes(x=verb_cat, y=response)) + geom_point() + facet_wrap(~ workerid)

# By-subject for each predicate
level_order = c('claim','argue','insist','believe','think','say','show','find','suspect','point out')
trialinfo$verb <- factor(trialinfo$verb, levels=level_order)
ggplot(trialinfo, aes(x=verb, y=response, color=verb_cat)) + geom_point() + facet_wrap(~ workerid) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ylab("Subject level of agreement")

# Calculate mean and SD
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
trialinfo2 <- data_summary(trialinfo, varname="response", 
                    groupnames=c("verb_cat","verb","prior_belief","cc_float","belief_strength"))

# Plot with CI around mean
ggplot(trialinfo2, aes(x=verb, y=response, group=verb_cat, color=verb_cat)) +
  facet_grid(prior_belief ~ cc_float)+
  geom_point(shape=1) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ylab("Subject level of agreement")

ggplot(trialinfo2, aes(x=verb_cat, y=response, group=verb_cat, color=verb_cat)) +
  facet_grid(prior_belief ~ cc_float)+
  geom_point(shape=1) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  ylab("Participant level of agreement") +
  xlab("Predicate category")

# Plot with prior belief strength and value
ggplot(trialinfo2, aes(x=verb, y=response, group=verb_cat, color=verb_cat)) +
  facet_grid(belief_strength ~ cc_float)+
  geom_point(shape=1) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ylab("Subject level of agreement")

ggplot(trialinfo2, aes(x=verb_cat, y=response, group=verb_cat, color=verb_cat)) +
  facet_grid(prior_belief ~ cc_float)+
  geom_point(shape=1) +
  stat_summary_bin(aes(y=), fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  ylab("Subject level of agreement")

# Separate pro and anti-vax participants
cc_float.labs <- c("pro-vax embedded prop", "anti-vax embedded prop")
names(cc_float.labs) <- c("1","-1")

belief_strength.labs <- c("strong prior belief", "weak prior belief")
names(belief_strength.labs) <- c("strong","weak")

ggplot(filter(trialinfo2,prior_belief == 'pro-vax'), aes(x=verb_cat, y=response, group=verb_cat)) +
  facet_grid(belief_strength ~ cc_float, labeller = labeller(cc_float = cc_float.labs, belief_strength = belief_strength.labs))+
  geom_point(shape=1) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  ylab("Participant level of agreement") +
  xlab("Predicate category")+
  ggtitle("Pro-vax participants") + theme_minimal()

ggplot(filter(trialinfo2,prior_belief == 'anti-vax'), aes(x=verb_cat, y=response, group=verb_cat)) +
  facet_grid(belief_strength ~ cc_float, labeller = labeller(cc_float = cc_float.labs, belief_strength = belief_strength.labs))+
  geom_point(shape=1) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  ylab("") +
  xlab("Predicate category")+
  ggtitle("Anti-vax participants") + theme_minimal()
