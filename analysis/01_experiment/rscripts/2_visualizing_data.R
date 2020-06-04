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

# Group subjects by prior stance
ggplot(trialinfo, aes(x=verb, y=response, color=verb_cat)) + geom_point() + facet_grid(. ~ own_stance_index > 2.5, labeller = labeller(own_stance_index = c("weak","strong"))) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ylab("Subject level of agreement")
