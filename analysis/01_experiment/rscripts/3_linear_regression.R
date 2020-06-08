# Linear regression

library(languageR)
library(lme4)
library(MuMIn)

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

# Create factors for string variables
trialinfo$verb_cat <- as.factor(trialinfo$verb_cat)
trialinfo$subj <- as.factor(trialinfo$subj)
trialinfo$cc_float <- as.factor(trialinfo$cc_float)
trialinfo$workerid <- as.factor(trialinfo$workerid)
trialinfo$verb_cat <- as.numeric(trialinfo$verb_cat)
trialinfo$subj <- as.numeric(trialinfo$subj)
trialinfo$cc_float <- as.numeric(trialinfo$cc_float)
trialinfo$workerid <- as.numeric(trialinfo$workerid)

# Linear regression for all data
m = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
                data = trialinfo)
summary(m)

# Linear regression split by cc type
m_pro = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
       data = filter(trialinfo,cc_float==2))
summary(m_pro)
m_anti = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
           data = filter(trialinfo,cc_float==1))
summary(m_anti)

# Split additionally by subj prior belief
m_pro_pro = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
           data = filter(trialinfo,cc_float==2,own_stance_index>2))
summary(m_pro_pro)

m_anti_pro = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
               data = filter(trialinfo,cc_float==1,own_stance_index>2))
summary(m_anti_pro)

m_pro_anti = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
               data = filter(trialinfo,cc_float==2,own_stance_index<=2))
summary(m_pro_anti)

m_anti_anti = lm(response ~ verb_cat*subj*own_stance_index + (1+verb_cat*subj | workerid) + (1+verb_cat*subj*own_stance_index | cc_float),
                data = filter(trialinfo,cc_float==1,own_stance_index<=2))
summary(m_anti_anti)

# Re-try with 10 different predicate levels
trialinfo$verb <- as.factor(trialinfo$verb,levels=c("claim","argue","insist",
                                                    "believe","think","say",
                                                    "point out","suspect","show",'find'),
                            ordered=TRUE)
trialinfo$verb <- as.numeric(trialinfo$verb)

# all together
m_pro = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
           data = trialinfo)
summary(m_pro)

# split by predicate type
m_pro = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
           data = filter(trialinfo,cc_float==2))
summary(m_pro)
# own_stance_index has sig. positive coefficient--for pro-vax ccs, 
# the more the predicate is "factive", the more you agree
m_anti = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
            data = filter(trialinfo,cc_float==1))
summary(m_anti)
# own_stance_index has sig. neg. coefficient--for anti-vax ccs, 
# the less factive a predicate is, the more you agree

# Split additionally by subj prior belief
m_pro_pro = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
               data = filter(trialinfo,cc_float==2,own_stance_index>2))
summary(m_pro_pro)

m_anti_pro = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
                data = filter(trialinfo,cc_float==1,own_stance_index>2))
summary(m_anti_pro)
# own_stance_index has sig. neg. coefficient

m_pro_anti = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
                data = filter(trialinfo,cc_float==2,own_stance_index<=2))
summary(m_pro_anti)

m_anti_anti = lm(response ~ verb*subj*own_stance_index + (1+verb*subj | workerid) + (1+verb*subj*own_stance_index | cc_float),
                 data = filter(trialinfo,cc_float==1,own_stance_index<=2))
summary(m_anti_anti)

# marginal R^2 (variance explained by fixed effects): .05
# conditional R^2 (variance explained by fixed and random effects jointly): .45
r.squaredGLMM(m)

