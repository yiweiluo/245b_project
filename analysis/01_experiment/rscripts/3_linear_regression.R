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

# Code predicate by its category
verb_cat_to_float <- function(verb_cat) {
  if (verb_cat == "neg") {
    res <- as.numeric(-1.0)
  }
  else if (verb_cat == "neut") {
    res <- as.numeric(0.0)
  }
  else {
    res <- as.numeric(1.0)
  }
  return(res)
}

subj_to_float <- function(s) {
  if (s == "Scientists") {
    res <- 1
  }
  else if (s == "Researchers") {
    res <- 2
  }
  else {
    res <- 3
  }
  return(res)
}

trialinfo$verb_cat_float <- as.numeric(modify(trialinfo$verb_cat,verb_cat_to_float))
trialinfo$subj_float <- as.numeric(modify(trialinfo$subj,subj_to_float))
m = lm(response ~ verb_cat_float*subj_float*own_stance_index + (1+verb_cat_float*subj_float | workerid) + (1+verb_cat_float*subj_float*own_stance_index | cc_float),
         data = trialinfo, REML=T)
summary(m)

# Code predicate by its ID


# marginal R^2 (variance explained by fixed effects): .05
# conditional R^2 (variance explained by fixed and random effects jointly): .45
r.squaredGLMM(m)

# 1. What was the linear model's R-squared? Re-compute it here.
m.s = lm(RT ~ Frequency, data=lexdec)
summary(m.s)

# 2. Let's add random by-subject and by-item intercepts. (What counts as an item?)
m = lmer(RT ~ Frequency + (1|Subject) + (1|Word), data=lexdec, REML=F)
summary(m)

# Let's look more closely at the random effects:
ranef(m)
mean(ranef(m)$Subject[[1]])
head(ranef(m)$Subject[1])
max(ranef(m)$Subject[1])
min(ranef(m)$Subject[1])

mean(ranef(m)$Word[[1]])
head(ranef(m)$Word[1])
max(ranef(m)$Word[1])
min(ranef(m)$Word[1])

# Let's add random by-subject slopes for frequency.
m = lmer(RT ~ Frequency + (1 + Frequency|Subject) + (1|Word), data=lexdec, REML=F)
summary(m)

# To get rid of correlations between random effects:
m.nocorr = lmer(RT ~ Frequency + (1|Subject) + (0 + Frequency|Subject) + (1|Word), data=lexdec, REML=F)
summary(m.nocorr)

# There are now two adjustment columns by subject, one for the intercepts and one for the slopes. We can see using summary() that there is greater variance in the intercepts than in the slopes. 
head(ranef(m)$Subject)
summary(ranef(m)$Subject)

# What does that correlation of -.92 mean?
plot(ranef(m)$Subject)

# How do we get p-values? The lmer authors make various suggestions here:
?pvalues

# To compute p-values for fixed effects via model comparison:
m.0 = lmer(RT ~ (1|Subject) + (1|Word), data=lexdec)
summary(m.0)

m.1 = lmer(RT ~ Frequency + (1|Subject) + (1|Word), data=lexdec)
summary(m.1)

anova(m.0,m.1)

# To compute p-values for fixed effects via lmerTest:
install.packages("lmerTest")
m.2 = lmerTest::lmer(RT ~ Frequency + (1|Subject) + (1|Word), data=lexdec)
summary(m.2)



