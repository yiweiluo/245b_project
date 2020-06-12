# Linear regression

library(languageR)
library(lme4)
library(MuMIn)
require(ggplot2)
library(lmerTest)

# Set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load reformatted data
subjinfo = read_csv("../data/reformatted_subjinfo.csv")
head(subjinfo)
trialinfo = read_csv("../data/reformatted_trialinfo.csv")
head(trialinfo)

# Check whether varying the predicate has an effect using two 
# different linear regressions.

trialinfo$prior_belief[trialinfo$own_stance_index< 2] <- "anti-vax subject"
trialinfo$prior_belief[trialinfo$own_stance_index>= 2] <- "pro-vax subject"
trialinfo$belief_strength <- cut(trialinfo$own_stance_index,
                                 breaks=c(-Inf, 1, 2.2, Inf),
                                 labels=c("strong","weak","strong"))
ggplot(trialinfo, aes(x=prior_belief, color=belief_strength)) + geom_bar()

# Create factors for string variables
trialinfo$verb_cat <- as.factor(trialinfo$verb_cat)
trialinfo$subj <- as.factor(trialinfo$subj)
trialinfo$cc_float <- as.factor(trialinfo$cc_float)
trialinfo$workerid <- as.factor(trialinfo$workerid)
trialinfo$cc_id <- as.factor(trialinfo$cc_id)
trialinfo$prior_belief <- as.factor(trialinfo$prior_belief)
trialinfo$belief_strength <- as.factor(trialinfo$belief_strength)
trialinfo$verb_cat <- as.numeric(trialinfo$verb_cat)
trialinfo$subj <- as.numeric(trialinfo$subj)
trialinfo$cc_float <- as.numeric(trialinfo$cc_float)
trialinfo$workerid <- as.numeric(trialinfo$workerid)
trialinfo$cc_id <- as.numeric(trialinfo$cc_id)
trialinfo$prior_belief <- as.numeric(trialinfo$prior_belief)
trialinfo$belief_strength <- as.numeric(trialinfo$belief_strength)
trialinfo$verb_cat <- scale(trialinfo$verb_cat,center=TRUE,scale=FALSE)
trialinfo$subj <- scale(trialinfo$subj,center=TRUE,scale=FALSE)
trialinfo$cc_float <- scale(trialinfo$cc_float,center=TRUE,scale=FALSE)
trialinfo$workerid <- scale(trialinfo$workerid,center=TRUE,scale=FALSE)
trialinfo$cc_id <- scale(trialinfo$cc_id,center=TRUE,scale=FALSE)
trialinfo$prior_belief <- scale(trialinfo$prior_belief,center=TRUE,scale=FALSE)
trialinfo$belief_strength <- scale(trialinfo$belief_strength,center=TRUE,scale=FALSE)

# Linear regression for all data

m = lmer(response ~ verb_cat*subj*prior_belief*belief_strength*cc_float + (1+verb_cat*subj*cc_float | workerid) + (1+verb_cat*subj*prior_belief*belief_strength | cc_id),
       data = trialinfo)
summary(m)
anova(m)

# Linear regression split by own_stance_index
m = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
       data = filter(trialinfo,own_stance_index>2.2 | own_stance_index < 1))
summary(m)

m = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
       data = filter(trialinfo,own_stance_index<=2.2 | own_stance_index >= 1))
summary(m)

# Linear regression split by cc type
m_pro = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
       data = filter(trialinfo,cc_float==2))
summary(m_pro)
m_anti = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
           data = filter(trialinfo,cc_float==1))
summary(m_anti)

# marginal R^2 (variance explained by fixed effects): .05
# conditional R^2 (variance explained by fixed and random effects jointly): .45
r.squaredGLMM(m)

