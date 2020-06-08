# Plotting data

library(languageR)
library(lme4)
library(ggpubr)

# Set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load reformatted data
subjinfo = read_csv("../data/reformatted_subjinfo.csv")
head(subjinfo)
trialinfo = read_csv("../data/reformatted_trialinfo.csv")
head(trialinfo)

all_preds = unique(trialinfo$verb)

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

# Group by subject prior stance and cc stance
stance_names <- list(
  as.logical(FALSE)="prior belief index < 2",
  as.logical(TRUE)="prior belief index > 2"
)
belief_labeller <- function(variable,value){
  return(stance_names[value])
}
ggplot(trialinfo, aes(x=verb, y=response, color=verb_cat)) + geom_point() + facet_grid((own_stance_index > 2) ~ cc_float, labeller = labeller(own_stance_index = c("weak","strong"))) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ylab("Subject level of agreement")

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
                    groupnames=c("verb_cat","verb","own_stance_index","cc_float"))

# Plot with CI around mean
ggplot(trialinfo2, aes(x=verb, y=response, group=verb_cat, color=verb_cat)) +
  facet_grid((own_stance_index > 2) ~ cc_float)+
  geom_point(shape=1) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 15)+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ylab("Subject level of agreement")

