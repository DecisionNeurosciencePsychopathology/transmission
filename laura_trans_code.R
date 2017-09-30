library(readr)
library(lme4)
library(ggplot2)
#library(dplyr)
library(tidyr)
library(psych)
library(gdata)
# library(R.matlab)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
# library(ggbiplot)
library(corrplot)
library(lsmeans)
# library(factoextra)
# library(ggfortify)
library(readxl)
# library(linbin)
library(MASS)
# library(lattice)
# library(fastICA)
# library(plotly)

df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED.csv",
"\t", escape_double = FALSE, trim_ws = TRUE)

View(df)

# library(VIM)
# df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


# hist(df$MOMMH)
# describe(df$ENVRNMTL)
# unique(df$ENVRNMTL)

# designate factors

df$GROUP1245 <- as.factor(df$GROUP1245)
df$GROUP12467 <- as.factor(df$GROUP12467)


# recode a variable: everyone with a h/o suicidal behavior among 2nd degree relatives
df$seconddegSB <- 0
df$seconddegSB[df$GPATT=="B" | df$GPATT=="C" |df$HALFSIBATT == "B"|df$HALFSIBATT == "C" | df$GCATT == "B" |df$GCATT == "C"] <- 1
df$seconddegSB[df$GPATT=="b" | df$GPATT=="c" |df$HALFSIBATT == "b"|df$HALFSIBATT == "c" | df$GCATT == "b" |df$GCATT == "c"] <- 1
df$seconddegSB[is.na(df$GPATT) | is.na(df$GPATT)| is.na(df$HALFSIBATT)  | is.na(df$HALFSIBATT) | is.na(df$GCATT) | is.na(df$GCATT)] <- "NA"

df$firstdegSB <- df$firstdegSA

df$firstdegSC <- as.factor(df$firstdegSC)

df$race <- "NA"
df$race[df$RACETEXT=="WHITE"] <- 0
#minority are 1
df$race[df$RACETEXT=="AFRICAN AMERICAN" | df$RACETEXT == "ASIAN PACIFIC"] <- 1

# get counts of suicide attempts by relation cagetory

# 1st degree
df$num1stExposuresSA <- df$num1stExposuresSB - df$num1stExposuresSC
df$num2ndExposuresSA <- df$num2ndExposuresSB - df$num2ndExposuresSC
df$numEnvExposuresSA <- df$numEnvExposuresSB - df$numEnvExposuresSC

df$num1stExposuresSC[df$num1stExposuresSC==9] <- NA
df$num1stExposuresSB[df$num1stExposuresSB==9] <- NA
df$num1stExposuresSA[df$num1stExposuresSA==9] <- NA

df$num2ndExposuresSC[df$num2ndExposuresSC==9] <- NA
df$num2ndExposuresSB[df$num2ndExposuresSB==9] <- NA
df$num2ndExposuresSA[df$num2ndExposuresSA==9] <- NA

df$numEnvExposuresSC[df$numEnvExposuresSC==9] <- NA
df$numEnvExposuresSB[df$numEnvExposuresSB==9] <- NA
df$numEnvExposuresSA[df$numEnvExposuresSA==9] <- NA


df <- as.data.frame(df)

d = melt(df, na.rm = FALSE, measure.vars = c("num1stExposuresSC","num1stExposuresSA", "num2ndExposuresSC", "num2ndExposuresSA", "numEnvExposuresSC", "numEnvExposuresSA"))

# discard the stupid variables

d <- d[,c(1:45,115:117)]
d$relation <- d$variable
d$events <- d$value
d$sev[d$relation == "num1stExposuresSC" | d$relation == "num2ndExposuresSC" | d$relation == "numEnvExposuresSC"] <- "suicide"
d$sev[d$relation == "num1stExposuresSA" | d$relation == "num2ndExposuresSA" | d$relation == "numEnvExposuresSA"] <- "attempt"
d$rel <- NA
d$rel[d$relation == "num1stExposuresSC" | d$relation == "num1stExposuresSA"] <- "1st"
d$rel[d$relation=="num2ndExposuresSC" | d$relation=="num2ndExposuresSA"] <- "2nd"
d$rel[d$relation=="numEnvExposuresSC" | d$relation=="numEnvExposuresSA"] <- "ENV"

d$blood <- NA
d$blood[d$relation == "num1stExposuresSC" | d$relation == "num1stExposuresSA" | d$relation=="num2ndExposuresSC" | d$relation=="num2ndExposuresSA"] <- "rel"
d$blood[d$relation=="numEnvExposuresSC" | d$relation=="numEnvExposuresSA"] <- "nonrel"


d1e <- d[d$rel=="1st" | d$rel=="ENV",]


## check if distribution of events roughly fits NB

nbfit <- suppressWarnings(fitdistr(na.omit(d1e$events), "negative binomial"))
print(nbfit$estimate)
simulated <- rnegbin(nbfit$n,nbfit$estimate[1],nbfit$estimate[2])
actual <- d1e$events

# just simple visual diagnostics
histogram(~ simulated + actual)
# conclusion -- not a perfect fit, but OK


# build a model

# estimate theta for nb

theta.resp <- theta.ml(na.omit(d1e$events), mean(na.omit(d1e$events)), length(d1e$events), limit = 50, eps = .Machine$double.eps^.25, trace = FALSE)


summary(m1 <- glm(events ~  sev*rel*GROUP12467 + (1:ID), family = negative.binomial(theta = theta.resp), data = d1e))
car::Anova(m1, type = "III")

lsmip(m1, sev ~ GROUP12467 | rel, ylab = "log(response rate)", xlab = "type ", type = "predicted" )
lsmip(m1, GROUP12467 ~ rel | sev, ylab = "log(response rate)", xlab = "type ", type = "predicted" )

summary(m2 <- glm(events ~  sev*GROUP12467 + rel*GROUP12467 + (1:ID), family = negative.binomial(theta = theta.resp), data = d))
car::Anova(m2, type = "III")
lsmip(m2, GROUP12467 ~ sev, ylab = "log(event rate)", xlab = "type ", type = "predicted" )
ls2 <- lsmeans(m2, "GROUP12467", by = "sev")
plot(ls2, horiz = F)


theta.ed <- theta.ml(na.omit(d$events), mean(na.omit(d$events)), length(d$events), limit = 50, eps = .Machine$double.eps^.25, trace = FALSE)
summary(m3 <- glm(events ~  sev*GROUP12467 + blood*GROUP12467 + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3, type = "III")
lsmip(m3, sev ~ GROUP12467 , ylab = "log(response rate)", xlab = "type ", type = "predicted" )
ls3 <- lsmeans(m3, "GROUP12467", by = "blood")
plot(ls3, horiz = F)

# demographics
summary(m3a <- glm(events ~  GROUP12467  + BASELINEAGE*sev*blood + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3a, type = "III")
ls3a <- lsmeans(m3a, "BASELINEAGE", by = "sev", at = list(BASELINEAGE=c(50,65,80)))
plot(ls3a, horiz = F)

summary(m3a1 <- glm(events ~  GROUP12467  + BASELINEAGE*sev*blood + EDUCATION  + race + AGEDEPONSET + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3a1, type = "III")

summary(m3a2 <- glm(events ~  GROUP12467  + BASELINEAGE*sev*blood + EDUCATION  + race + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3a2, type = "III")


summary(m3b <- glm(events ~  AGEATFIRSTATTEMPT*sev + AGEATFIRSTATTEMPT*blood + BASELINEAGE + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3b, type = "III")
ls3b <- lsmeans(m3b, "AGEATFIRSTATTEMPT", by = "sev", at = list(AGEATFIRSTATTEMPT=c(20,50,80)))
plot(ls3b, horiz = F)
ls3b1 <- lsmeans(m3b, "AGEATFIRSTATTEMPT", by = "blood", at = list(AGEATFIRSTATTEMPT=c(20,50,80)))
plot(ls3b1, horiz = F)

summary(m3c <- glm(events ~  AGEATFIRSTATTEMPT*sev + AGEATFIRSTATTEMPT*blood + BASELINEAGE +  (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3c, type = "III")


summary(m4 <- glm(events ~   GROUP1245 + BASELINEAGE*sev*blood + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m4, type = "III")

summary(m5 <- glm(events ~  GROUP1245  + BASELINEAGE*sev*blood + EDUCATION  + race + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m5, type = "III")
ls5 <- lsmeans(m5, "GROUP1245")
plot(ls5, horiz = F)
multcomp::cld(ls5)
d$GROUP[d$GROUP=='DEPRESSION-IDEATOR'] <- "IDEATOR"
d$group_early <- d$GROUP
d$group_early[d$AGEATFIRSTATTEMPT<60] <- "EARLY_ATTEMPT"
d$group_early[d$AGEATFIRSTATTEMPT>59] <- "LATE_ATTEMPT"

# d$group_v_early <- d$GROUP
# d$group_v_early[d$AGEATFIRSTATTEMPT<60] <- "MID_ATTEMPT"
# d$group_v_early[d$AGEATFIRSTATTEMPT>59] <- "LATE_ATTEMPT"
# d$group_v_early[d$AGEATFIRSTATTEMPT<25] <- "EARLY_ATTEMPT"


summary(m6 <- glm(events ~  group_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m6, type = "III")
ls6 <- lsmeans(m6, "group_early", by = (c("sev","blood")))
plot(ls6)
ls6a <- lsmeans(m6, "group_early")
plot(ls6a)
multcomp::cld(ls6a)


# summary(m7 <- glm(events ~  group_v_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
# car::Anova(m7, type = "III")
# ls7 <- lsmeans(m7, "group_v_early", by = (c("sev","blood")))
# plot(ls7)
# ls7a <- lsmeans(m7, "group_v_early")
# plot(ls7a)
# multcomp::cld(ls7a)



# get group characteristics
chars <- df[,c(2,5,7,8,9,10)]
describe.by(chars,group = chars$GROUP1245)

# 
# m1 <- glm(firstdegSB ~ GROUP12467, family = binomial(link = "logit"), data = df)
# summary(m1)
# anova(m1, test = "LR")
# ls1 <- lsmeans(m1,"GROUP1245")
# plot(ls1, horiz = F)
# 
# m2 <- glm(firstdegSB ~ GROUP1245*BASELINEAGE, family = binomial(link = "logit"), data = df)
# summary(m2)
# anova(m2, test = "LR")
# 
# # does m2 fit better than m1?
# anova(m1,m2, test = "LR")
# 
# m3 <- glm(firstdegSB ~ GROUP12467 + BASELINEAGE, family = binomial(link = "logit"), data = df)
# summary(m3)
# anova(m3, test = "LR")
# anova(m2,m3, test = "LR")
# 
# m4 <- glm(firstdegSB ~ GROUP12467 + BASELINEAGE + GENDERTEXT + race, family = binomial(link = "logit"), data = df)
# summary(m4)
# anova(m4, test = "LR")
# anova(m3,m4, test = "LR")
# ls4 <- lsmeans(m4,"GROUP12467")
# plot(ls4, horiz = F)
# 
# 
# # does first degree Hx vary by attempt characteristics
# am1 <- glm(firstdegSB ~ AGEATFIRSTATTEMPT, family = binomial(link = "logit"), data = df[df$GROUP1245==5,])
# summary(am1)
# 
# am2 <- glm(firstdegSB ~ AGEATFIRSTATTEMPT + TOTALATTEMPTS, family = binomial(link = "logit"), data = df[df$GROUP1245==5,])
# summary(am2)
# 
# 
# cm1 <-  glm(firstdegSC ~ GROUP12467 + BASELINEAGE , family = binomial(link = "logit"), data = df)
# summary(cm1)
# anova(cm1, test = "LR")
# lscm1 <- lsmeans(cm1,"GROUP12467")
# plot(cm1, horiz = F)
sc_tbl <- table(df$GROUP12467,df$firstdegSC)
sc_tbl_chsq <- chisq.test(sc_tbl)