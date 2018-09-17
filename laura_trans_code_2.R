
# hist(df$MOMMH)
# describe(df$ENVRNMTL)
# unique(df$ENVRNMTL)

# install.packages(c("car","readr", "lme4", "ggplot2", "tidyr", "psych", "gdata", "xtable", 
#                    "Hmisc", "nnet", "reshape2", "corrplot", "lsmeans", "readxl", "MASS", 
#                    "stargazer", "compareGroups", "multcompView","RColorBrewer","VIM","fifer"))
# 

#library(fifer)
library(sjPlot)
library(RColorBrewer)
library(multcompView)
library (car)
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
library(stargazer)
library(compareGroups)
# library(lattice)
# library(fastICA)
# library(plotly)
library(effects)

rm(list = ls())

#df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED_10.4.17.dat",
#"\t", escape_double = FALSE, trim_ws = TRUE)
#setwd("~/Box Sync/skinner/projects_analyses/Project Transmission")

#df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED_10.4.17.csv",
#"\t", escape_double = FALSE, trim_ws = TRUE)

## these are the old data before Laura's recoding of aunts and uncles into 2nd degree relatives
# df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED.csv",
# "\t", escape_double = FALSE, trim_ws = TRUE)


#setwd("~/Box Sync/skinner/projects_analyses/Project Transmission")

#For Anna:
#setwd("~/Dropbox/USA/Pittsburgh/GitHub/transmission/transmission")
#df <- read_delim("FAMHX_DEMOG_COUNTS_MERGED_10.4.17.csv",
#                ",", escape_double = FALSE, trim_ws = TRUE)


#at home
setwd("C:/Users/Laura/Box Sync/skinner/projects_analyses/Project Transmission")
df <- read_delim("C:/Users/Laura/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED_10.4.17.dat",
                  "\t", escape_double = FALSE, trim_ws = TRUE)

df <- df[df$ID!='207224'& df$ID!='216806' & df$ID!='217142' & df$ID!='217988' & df$ID!='218506'& df$ID!='219855',]

# #For Alex:
# setwd("~/Box Sync/skinner/projects_analyses/Project Transmission")
# 
# 
# df <- read_delim("FAMHX_DEMOG_COUNTS_MERGED_10.4.17.csv",
#                  "\t", escape_double = FALSE, trim_ws = TRUE)

# load("trans.Rda")
# library(VIM)
# df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

View(df)

#checking missingness
library(mice)
md.pattern(df)

library(VIM)
df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#missing amongst attempters only
df_aggr = aggr(df_onlyAtt, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


# designate factors

df$GROUP1245 <- as.factor(df$GROUP1245)
df$GROUP12467 <- as.factor(df$GROUP12467)
df$SubstanceLifetime[df$GROUP1245==1] <- NA
df$HRSDNOSUICIDE[df$GROUP1245==1] <- NA
df$AnxietyLifetime[df$GROUP1245==1] <- NA
df$TOTALATTEMPTS[df$GROUP1245!=5] <- NA
df$MAXLETHALITY[df$GROUP1245!=5] <- NA

df$SubstanceLifetime <- as.factor(df$SubstanceLifetime)
df$AnxietyLifetime <- as.factor(df$AnxietyLifetime)
df$firstdegaffective <- as.factor(df$firstdegaffective)
df$firstdegsubstance <- as.factor(df$firstdegsubstance)
df$spouseaffective <- as.factor(df$spouseaffective)
df$spousesubstance<- as.factor(df$spousesubstance)
df$marital2lvl<- as.factor(df$marital2lvl)

df$GROUP[df$GROUP=='DEPRESSION-IDEATOR'] <- "IDEATOR"
df$group_early <- df$GROUP
df$group_early[df$group_early=="CONTROL"] <- "Non-psychiatric controls"
df$group_early[df$group_early=="DEPRESSION"] <- "Non-suicidal depressed"
df$group_early[df$group_early=="IDEATOR"] <- "Suicide ideators"

df$group_early[df$AGEATFIRSTATTEMPT<60] <- "Early-onset attempters"
df$group_early[df$AGEATFIRSTATTEMPT>59] <- "Late-onset attempters"
df$group_early <- as.factor(df$group_early)
df$group_early = factor(df$group_early, levels(df$group_early)[c(3,4,5,1,2)])
df$group_early_no_break <- df$group_early
levels(df$group_early) <- gsub(" ", "\n", levels(df$group_early))
df$group_early_no_break=gsub("\n", " ", df$group_early)
df$group_early_no_break <- as.factor(df$group_early_no_break)
df$group_early_no_break = factor(df$group_early_no_break, levels(df$group_early_no_break)[c(3,4,5,1,2)])

# recode a variable: everyone with a h/o suicidal behavior among 2nd degree relatives
df$seconddegSB <- 0
df$seconddegSB[df$GPATT=="B" | df$GPATT=="C" |df$HALFSIBATT == "B"|df$HALFSIBATT == "C" | df$GCATT == "B" |df$GCATT == "C"] <- 1
df$seconddegSB[df$GPATT=="b" | df$GPATT=="c" |df$HALFSIBATT == "b"|df$HALFSIBATT == "c" | df$GCATT == "b" |df$GCATT == "c"] <- 1
df$seconddegSB[is.na(df$GPATT) | is.na(df$GPATT)| is.na(df$HALFSIBATT)  | is.na(df$HALFSIBATT) | is.na(df$GCATT) | is.na(df$GCATT)] <- "NA"

df$firstdegSB <- df$firstdegSA
df$firstdegSA <- as.factor(df$firstdegSA)
df$firstdegSB <- as.factor(df$firstdegSB)
df$firstdegSC <- as.factor(df$firstdegSC)


df$bloodSB<-0
df$bloodSB[df$firstdegSB==1|df$seconddegSB==1]<-1
df$bloodSB[is.na(df$firstdegSB)|is.na(df$seconddegSB)]<-"NA"

df$envSB<-0
df$envSB[df$numEnvExposuresSB>=1]<-1
df$envSB[is.na(df$numEnvExposuresSB)] <- "NA"
df$envSB

df$race2lvl <- "NA"
df$race2lvl[df$RACETEXT=="WHITE"] <- 0
#minority are 1
df$race2lvl[df$RACETEXT=="AFRICAN AMERICAN" | df$RACETEXT == "ASIAN PACIFIC"] <- 1

#familial affective disorders
df$firstdegaffective <- 0
df$firstdegaffective[df$MOMMH=="B" | df$MOMMH=="b" |df$MOMMH == "C"|df$MOMMH == "c" | 
                  df$DADMH=="B" | df$DADMH=="b" |df$DADMH == "C"|df$DADMH == "c" |
                  df$SIBMH=="B" | df$SIBMH=="b" |df$SIBMH == "C"|df$SIBMH == "c"] <- 1
df$firstdegaffective

#familial substance misuse
df$firstdegsubstance <- 0
df$firstdegsubstance[df$MOMMH=="E" | df$MOMMH=="e"|df$DADMH=="E" | df$DADMH=="e"|df$SIBMH=="E" | df$SIBMH=="e"] <- 1
df$firstdegsubstance

#souse affective disorder
df$spouseaffective <- 0
df$spouseaffective[df$SPOUSEMH=="B" | df$SPOUSEMH=="b" |df$SPOUSEMH == "C"|df$SPOUSEMH == "c"] <- 1
df$spouseaffective

#spouse substabce misuse
df$spousesubstance <- 0
df$spousesubstance[df$SPOUSEMH=="E" | df$SPOUSEMH=="e"] <- 1
df$spousesubstance

#marital status, binary; partenered=1, single=0
df$MARITALTEXT
df$marital2lvl<-0
df$marital2lvl[df$MARITALTEXT=="MARRIED"|df$MARITALTEXT=="CO-HABITATING"]<-1
df$marital2lvl

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

# change reference group to something else
#d$group_early <- relevel(d$group_early, ref = c("Non-suicidal\ndepressed"))

# resetting the group order for the figures
 d$group_early <- factor(d$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))

# discard the stupid variables

#d <- d[,c(1:45,113:ncol(d))]
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


df$BIS_TOT<-rowSums(df[,c("BIS_NONPLAN","BIS_COGNIT","BIS_MOTOR")], na.rm = FALSE)
df$BIS_TOT

save(df, file="trans.Rda")


## remove random adopted guy from dataset
df <- df[df$ID != '217142',]
df$numRelExposuresSB

## check if distribution of events roughly fits NB

nbfit <- suppressWarnings(fitdistr(na.omit(d$events), "negative binomial"))
print(nbfit$estimate)
simulated <- rnegbin(nbfit$n,nbfit$estimate[1],nbfit$estimate[2])
actual <- d$events

# just simple visual diagnostics
histogram(~ simulated + actual)
# conclusion -- not a perfect fit, but OK


# build a model

# estimate theta for nb

theta.resp <- theta.ml(na.omit(d1e$events), mean(na.omit(d1e$events)), length(d1e$events), limit = 50, eps = .Machine$double.eps^.25, trace = FALSE)

#group12467, severity, relation; three way significant, no main effect of group
summary(m1 <- glm(events ~  sev*rel*GROUP12467 + (1:ID), family = negative.binomial(theta = theta.resp), data = d))
car::Anova(m1, type = "III")
ls1 <- lsmeans(m1, "GROUP12467", by = "rel")
plot(ls1, horiz = F)


lsmip(m1, sev ~ GROUP12467 | rel, ylab = "log(response rate)", xlab = "type ", type = "predicted" )
lsmip(m1, GROUP12467 ~ rel | sev, ylab = "log(response rate)", xlab = "type ", type = "predicted" )

#did not enter the three way interaction, did two ways separately. severity by group interaction is interesting, all groups exposed to more att than comp except LL
summary(m2 <- glm(events ~  sev*GROUP12467 + rel*GROUP12467 + (1:ID), family = negative.binomial(theta = theta.resp), data = d))
car::Anova(m2, type = "III")
lsmip(m2, GROUP12467 ~ sev, ylab = "log(event rate)", xlab = "type ", type = "predicted" )
ls2 <- lsmeans(m2, "GROUP12467", by = "sev")
plot(ls2, horiz = F)

#difference in exposure to attempts (not SC) by group
theta.ed <- theta.ml(na.omit(d$events), mean(na.omit(d$events)), length(d$events), limit = 50, eps = .Machine$double.eps^.25, trace = FALSE)
summary(m3 <- glm(events ~  sev*GROUP12467*blood + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3, type = "III")
lsmip(m3, sev ~ GROUP12467 | blood , ylab = "log(response rate)", xlab = "type ", type = "predicted" )
ls3 <- lsmeans(m3, "GROUP12467", by = "blood")
plot(ls3, horiz = F)

# demographics- adding in age
summary(m3a <- glm(events ~  GROUP12467  + BASELINEAGE*sev*blood + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3a, type = "III")
ls3a <- lsmeans(m3a, "BASELINEAGE", by = c("sev", at = list(BASELINEAGE=c(50,65,80))))
plot(ls3a, horiz = F)
ls3a.2 <- lsmeans(m3a.2, "BASELINEAGE", by = c("sev", "blood"), at = list(BASELINEAGE=c(50,65,80)))


summary(m3a1 <- glm(events ~  GROUP12467  + BASELINEAGE*sev*blood + EDUCATION  + race2lvl + AGEDEPONSET + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m3a1, type = "III")

summary(m3a2 <- glm(events ~  GROUP12467  + BASELINEAGE*sev*blood + EDUCATION  + race2lvl + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
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

summary(m5 <- glm(events ~  GROUP1245  + BASELINEAGE*sev*blood + EDUCATION  + race2lvl + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m5, type = "III")
ls5 <- lsmeans(m5, "GROUP1245")
plot(ls5, horiz = F)
multcomp::cld(ls5)
# d$group_v_early <- d$GROUP
# d$group_v_early[d$AGEATFIRSTATTEMPT<60] <- "MID_ATTEMPT"
# d$group_v_early[d$AGEATFIRSTATTEMPT>59] <- "LATE_ATTEMPT"
# d$group_v_early[d$AGEATFIRSTATTEMPT<25] <- "EARLY_ATTEMPT"


summary(m6 <- glm(events ~  group_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race2lvl  + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
car::Anova(m6, type = "III")
ls6 <- lsmeans(m6, "group_early", by = (c("sev","blood")))
plot(ls6)
ls6a <- lsmeans(m6, "group_early")
plot(ls6a)
multcomp::cld(ls6a)


# summary(m7 <- glm(events ~  group_v_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race2lvl + PTSDLifetime + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
# car::Anova(m7, type = "III")
# ls7 <- lsmeans(m7, "group_v_early", by = (c("sev","blood")))
# plot(ls7)
# ls7a <- lsmeans(m7, "group_v_early")
# plot(ls7a)
# multcomp::cld(ls7a)


summary(m8 <- glm(events ~  group_early + blood  + BASELINEAGE +  EDUCATION  + race2lvl  + (1:ID), family = negative.binomial(theta = theta.ed), data = d[d$sev=="suicide",]))
car::Anova(m8, type = "III")
ls8 <- lsmeans(m8, "group_early")
plot(ls8)
multcomp::cld(ls8)

# exclude healthy controls
dd <- d[d$GROUP1245 != "1",]

#show that the three-way interaction was only significant because of healthy controls
summary(m6.no_healthy <- glm(events ~  group_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race2lvl  + (1:ID), family = negative.binomial(theta = theta.ed), data = dd))
car::Anova(m6.no_healthy, type = "III")
ls6 <- lsmeans(m6.no_healthy, "group_early", by = (c("sev","blood")))
plot(ls6)
ls6a <- lsmeans(m6.no_healthy, "group_early")
plot(ls6a)
multcomp::cld(ls6a)


# dichotomize exposure
d$exp <- d$events>0
summary(m9 <- glm(exp ~  group_early*sev +group_early*blood + scale(BASELINEAGE)*sev +  scale(EDUCATION)  + race2lvl  + (1:ID), family = binomial, data = d))
car::Anova(m9, type = "III")
lsmip(m9,  sev ~ group_early |blood, ylab = "log(response rate)", xlab = "type ", type = "predicted" )
ls9 <- lsmeans(m9, "group_early", by = "blood")
plot(ls9, horiz = F)
multcomp::cld(ls9)
vif(m9)

d$rel <- factor(d$rel)
d$rel <- relevel(d$rel, ref = 'ENV')
car::Anova(m10, type = "III")
ls10 <- lsmeans(m10, "group_early")
contrast(ls10, method = "pairwise", adjust ="tukey")
plot(ls10, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")
vif(m10)

#THE GOOD MODEL!!!!! basic model without any demog
summary(m10_M1 <- glm(exp ~  group_early*rel + rel*sev + scale(BASELINEAGE)*sev + (1:ID), family = binomial, data = d))
car::Anova(m10_M1, type = "III")
ls10_M1 <- lsmeans(m10_M1, "group_early")
contrast(ls10_M1, method = "pairwise", adjust ="tukey")
plot(ls10_M1, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")
 
#THE GOOD MODELWITHOUT AGE 7/11/18
summary(m10_M1_NoAge <- glm(exp ~  group_early*rel + rel*sev + (1:ID), family = binomial, data = d))
car::Anova(m10_M1_NoAge, type = "III")
ls10_M1 <- lsmeans(m10_M1, "group_early")
contrast(ls10_M1_NoAge, method = "pairwise", adjust ="tukey")
plot(ls10_M1_NoAge, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")


# # AD: a nice new way to visualize model coefficients- Whisker Plot
# library(sjPlot)
# plot_model(m10_M1, show.p = TRUE, show.values = TRUE)
# 
# v1 <- c("Lagged decision time", "Trial", "Stay vs. switch",  "Exploratory choice", 
#         "Controls vs. attempters", "Depressed vs. attempters", "Ideators vs. attempters", 
#         "Reward","Reward * Absolute PE", "Reward * Controls vs attempters", "Reward * Depressed vs attempters", "Reward * Ideators vs attempters",
#         "Absolute prediction error (PE)", "Absolute PE * Controls vs attempters", "Absolute PE * Depressed vs attempters", "Absolute PE * Ideators vs attempters",
#         "Max value","Max value * Controls vs attempters", "Max value * Depressed vs attempters", "Max value * Ideators vs attempters")
# 
# model_terms1 <- labels(terms(m10_M1))
#  
# s11 <- summary(m10_M1)
# coef11 <- s11$coefficients
# terms11 <- labels(coef11)[[1]]
# terms11 <- terms11[2:20]
# 
# p1 <- plot_model(m10_M1,  p.kr = FALSE, terms = terms11, order.terms = c(19,8,18,17,7,16:9,6,5,4:1),
#                  
#                  show.p = TRUE, show.values = TRUE, vline.color = "slategray3", group.terms = c(8,7,6,6,5,1,1,1,1,2,2,2,2,3,3,4,4,4,4),
#                  axis.labels = terms11[c(19,8,18,17,7,16:9,6,5,4:1)], axis.title = "coefficients", value.offset = 0.4, colors = c( "plum", "turquoise", "green4", "navy", "green", "yellow", "orchid", "green1"),
#                  title = "Sample 1, Experiment 1")
#  
# p1 <- p1 + theme(axis.text.y = element_text(color = "black"))


#re-running m10_M1 with blood instead of rel for the multi panel figure
summary(m10_M1blood <- glm(exp ~  group_early*blood + blood*sev + scale(BASELINEAGE)*sev + (1:ID), family = binomial, data = d))
car::Anova(m10_M1blood, type = "III")
ls10_M1blood <- lsmeans(m10_M1blood, "group_early")
contrast(ls10_M1blood, method = "pairwise", adjust ="tukey")
plot(ls10_M1blood, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")

#demog sensitivity
summary(m10_M2 <- glm(exp ~  group_early*rel + rel*sev + scale(BASELINEAGE)*sev + race2lvl + EDUCATION + GENDERTEXT + marital2lvl + (1:ID), family = binomial, data = d))
car::Anova(m10_M2, type = "III")
ls10_M2 <- lsmeans(m10_M2, "group_early")
contrast(ls10_M2, method = "pairwise", adjust ="tukey")
plot(ls10_M2, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")

#psychopathology sensitivity
d$SubstanceLifetime[d$group_early == "Non-psychiatric\ncontrols"] <- 0
d$AnxietyLifetime[d$group_early == "Non-psychiatric\ncontrols"] <- 0

summary(m10_M3 <- glm(exp ~  group_early*rel + rel*sev + scale(BASELINEAGE)*sev + race2lvl + EDUCATION + GENDERTEXT + marital2lvl  + AnxietyLifetime + SubstanceLifetime + (1:ID), family = binomial, data = d))
car::Anova(m10_M3, type = "III")
ls10_M3 <- lsmeans(m10_M3, "group_early")
contrast(ls10_M3, method = "pairwise", adjust ="tukey")
plot(ls10_M3, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")


adf <- df[df$GROUP1245==5,]

ggplot(adf[!adf$bloodSB=='NA',], aes(x = AGEATFIRSTATTEMPT, y = bloodSB, color = bloodSB)) + geom_jitter()

ggplot(adf[!adf$bloodSB=='NA',], aes(x = bloodSB, y = AGEATFIRSTATTEMPT, color = bloodSB)) + geom_boxplot()
ggplot(adf[!adf$numEnvExposuresSB=='NA',], aes(x = numEnvExposuresSB>1, y = AGEATFIRSTATTEMPT, color = numEnvExposuresSB>1)) + geom_boxplot()


ggplot(adf, aes(x = AGEATFIRSTATTEMPT, y = numEnvExposuresSB>0, color = numEnvExposuresSB>0)) + geom_jitter()


# environmental only

summary(m_env <- glm(numEnvExposuresSB>0 ~  group_early + scale(BASELINEAGE) + race2lvl + EDUCATION + GENDERTEXT + marital2lvl, family = binomial, data = df))
plot_model(m_env)

# # do not worry about this interaction plot -- just did it to rule out familial clustering once and for all
# ls10a <- lsmeans(m10, "group_early", by = "rel")
# plot(ls10a, horiz = F)
# multcomp::cld(ls10a)
# 
# # evaluate interaction- nonrel, more exp to completions
# ls10b <- lsmeans(m10, "sev", by = "rel")
# plot(ls10b, horiz = F)
# multcomp::cld(ls10b)
# 
# # evaluate interaction- older, more exp to completions
# ls10c <- lsmeans(m10, "BASELINEAGE", by = "sev", at = list(BASELINEAGE = c(40,60,80)))
# plot(ls10c, horiz = F)
# multcomp::cld(ls10c)
# 
# # evaluate main effect- caucasions, more exposure
# ls10d <- lsmeans(m10, "race2lvl")
# plot(ls10d, horiz = F)
# multcomp::cld(ls10d)
# 
# anova(m9,m10,test = "Rao")
# 
# summary(m10a <- glm(exp[d$blood == 'rel'] ~  group_early + sev + BASELINEAGE*sev +  EDUCATION  + race2lvl + (1:ID), family = binomial, data = d))
# car::Anova(m10a, type = "III")
# ls10e <- lsmeans(m10a, "group_early")
# contrast(ls10e, method = "pairwise", adjust ="tukey")
# plot(ls10e, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")
# 
# anova(m10a,m10,test="Rao")
# 
# # evaluate interaction- nonrel, more completions- not the yn model interaction
# ls10aBlood <- lsmeans(m10a, "sev", by = "blood")
# plot(ls10aBlood, horiz = F)
# multcomp::cld(ls10aBlood)
# 
# 
# summary(m11 <- glm(exp ~  group_early + rel*sev + BASELINEAGE*sev +  EDUCATION  + race2lvl + (1:ID), family = binomial, data = d))
# car::Anova(m11, type = "III", test.statistic = c("F"))
# ls11 <- lsmeans(m11, "group_early")
# multcomp::cld(ls11, sort = FALSE)
# plot(ls11, horiz = F)
# contrast(ls11, method = "pairwise", adjust ="tukey")
# 
# 
# # evaluate interaction- older people, less attempts
# ls11age <- lsmeans(m11, "sev", by = "BASELINEAGE", at = list(BASELINEAGE = c(40,60,80)))
# plot(ls11age, horiz = F)
# multcomp::cld(ls11age)
# 
# 
# # evaluate interaction- environment, more completions
# ls11env <- lsmeans(m11, "sev", by = "rel")
# plot(ls11env, horiz = F)
# multcomp::cld(ls11env)
# 
# anova(m11,m11b,test = "Rao")
# 
# #checking if we have a good reason to keep education in there.
# summary(m11b <- glm(exp ~  group_early + rel*sev + BASELINEAGE*sev +  race2lvl + (1:ID), family = binomial, data = d))
# car::Anova(m11b, type = "III")
# ls11b <- lsmeans(m11b, "group_early")
# plot(ls11b, horiz = F)
# 
# anova(m11,m11b,test = "Rao")
# 
# 
# names(d)
# aggregate(d[,54], list(d$group_early), mean, na.rm= TRUE)
# 
# 
# # specifically test group*relation to rule out familial clustering: NS, does not improve fit
# summary(m11a <- glm(exp ~  group_early*sev*rel + BASELINEAGE*sev +  EDUCATION  + race2lvl + (1:ID), family = binomial, data = d))
# car::Anova(m11a, type = "III")
# anova(m11,m11a, test = "Rao")
# ls11a2 <- lsmeans(m11a, "group_early", by = c("rel","sev"))
# plot(ls11a2)
# 
# #since plot does not work, try without HC group
# dd$exp <- dd$events>0
# summary(m11_noHealthy <- glm(exp ~  group_early*sev*rel + BASELINEAGE*sev +  EDUCATION  + race2lvl + (1:ID), family = binomial, data = dd))
# car::Anova(m11_noHealthy, type = "III")
# anova(m11,m11_noHealthy, test = "Rao")
# ls11a2_noHealthy <- lsmeans(m11_noHealthy, "group_early", by = c("rel","sev"))
# plot(ls11a2_noHealthy)
# 
# # plot NS interaction for qualitative look:  still a problem with 2nd degree
# ls11a <- lsmeans(m11a, "group_early", by = "rel")
# plot(ls11a)
# 
# 
# # test substance and anxiety -- not significant
# summary(m12 <- glm(exp ~  group_early + sev*rel + BASELINEAGE*sev +  EDUCATION  + race2lvl + SubstanceLifetime + AnxietyLifetime + (1:ID), family = binomial, data = d))
# car::Anova(m12, type = "III")
# summary(m12a <- glm(exp ~  group_early + sev*rel + BASELINEAGE*sev +  race2lvl + SubstanceLifetime + AnxietyLifetime + (1:ID), family = binomial, data = d))
# car::Anova(m12a, type = "III")
# anova(m11b,m12, test = "Rao")

# BLOOD DATASET- new dataset without enviromental exposures

df <- as.data.frame(df)
dblood = melt(df, na.rm = FALSE, measure.vars = c("num1stExposuresSC","num1stExposuresSA", "num2ndExposuresSC", "num2ndExposuresSA"))
dblood$group_early <- relevel(dblood$group_early, ref = "Non-suicidal\ndepressed")

#resetting group order
dblood$group_early <- factor(dblood$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))

# discard the stupid variables

dblood$relation <- dblood$variable
dblood$events <- dblood$value
dblood$sev[dblood$relation == "num1stExposuresSC" | dblood$relation == "num2ndExposuresSC"] <- "suicide"
dblood$sev[dblood$relation == "num1stExposuresSA" | dblood$relation == "num2ndExposuresSA"] <- "attempt"
dblood$rel <- NA
dblood$rel[dblood$relation == "num1stExposuresSC" | dblood$relation == "num1stExposuresSA"] <- "1st"
dblood$rel[dblood$relation=="num2ndExposuresSC" | dblood$relation=="num2ndExposuresSA"] <- "2nd"

barchart(dblood$relation)

dblood1e <- d[dblood$rel=="1st",]

## check if distribution of events roughly fits NB

nbfit <- suppressWarnings(fitdistr(na.omit(dblood$events), "negative binomial"))
print(nbfit$estimate)
simulated <- rnegbin(nbfit$n,nbfit$estimate[1],nbfit$estimate[2])
actual <- dblood$events

# just simple visual diagnostics
histogram(~ simulated + actual)

# build a model

# estimate theta for nb
theta.resp <- theta.ml(na.omit(dblood1e$events), mean(na.omit(dblood1e$events)), length(dblood1e$events), limit = 50, eps = .Machine$double.eps^.25, trace = FALSE)

#dichotomize exposure in dblood
dblood$exp <- dblood$events>0

#THIS ONE 04.10.18
#summary(m10blood <- glm(exp ~  group_early + sev + BASELINEAGE*sev +  EDUCATION  + race2lvl + MARITALTEXT + (1:ID), family = binomial, data = dblood))
summary(m10blood <- glm(exp ~  group_early + sev + BASELINEAGE*sev + (1:ID), family = binomial, data = dblood))
car::Anova(m10blood, type = "III")
ls10blood <- lsmeans(m10blood, "group_early")
contrast(ls10blood, method = "pairwise", adjust ="tukey")
multcomp::cld(ls10blood, sort = FALSE)
plot(ls10blood, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")

#STARGAZER COMPARING FINAL MODELS AND SENSITIVITY ANALYSES

# #same as below, unlabeled for certainty
# stargazer(m10blood, type="html", out="trans_blood.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
# stargazer(m10_M1,m10_M2,m10_M3, type="html", out="trans_sensitivity.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

#labeled for paper
stargazer(m10blood, type="html", out="trans_blood.labeled.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels=c("Exposures in Relatives"), covariate.labels=c("Depressed Controls","Ideators","Early-onset Attempters","Late-onset Attempters",
                                                                         "Severity (Suicide)", "Age", "Severity*Age"))

stargazer(m10_M1,m10_M2,m10_M3, type="html", out="trans_sensitivity.labeled.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels=c("Exposures"), covariate.labels=c("Depressed Controls","Ideators","Early-onset Attempters","Late-onset Attempters",
                                                            "Relationship: 1st Degree Relative", "Relationship: 1st Degree Relative",
                                                            "Severity (Suicide)", "Age", "Race", "Education", "Sex", "Marital Status",
                                                            "Anxiety Disorder", "Substance use disorder", "Depressed*1st",
                                                            "Ideator*1st", "EoAttempter*1st", "LoAttempter*1st",
                                                            "Depressed*2nd", "Ideator*2nd", "EoAttempter*2nd",
                                                            "LoAttempter*2nd", "1st*Suicide Severity", "2nd*Suicide Severity",
                                                            "Severity*Age"))

# summary(m11blood <- glm(exp ~  group_early*sev + BASELINEAGE +  EDUCATION  + race2lvl + (1:ID), family = binomial, data = dblood))
# car::Anova(m11blood, type = "III")
# ls11blood <- lsmeans(m11blood, "group_early")
# contrast(ls11blood, method = "pairwise", adjust ="tukey")
# plot(ls11blood, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")
# 
# ls11blood_b <- lsmeans(m11blood, "group_early", by = "sev")
# plot(ls11blood_b, horiz = F)
# multcomp::cld(ls11blood_b, sort = FALSE)


#plot figure m10
CLD <- multcomp::cld(ls10,
                     alpha=0.05,
                     Letters=letters,
                     adjust="tukey")
CLD$.group=gsub(" ", "", CLD$.group)

# CLD$g=c("Non-psychiatric controls", "Non-suicidal depressed", "Suicide ideators", "Early-onset attempters", "Late-onset attempters")

#plot figure m10,10blood merged
# CLD_blood <- multcomp::cld(ls10blood,
#                            alpha=0.05,
#                            Letters=letters,
#                            adjust="tukey")
# CLD_blood$.group=gsub(" ", "", CLD_blood$.group)
# View(CLD_all)
# CLD_all <- rbind(CLD, CLD_blood)
# CLD_all$models <- c(rep("any exposure",5), rep("family exposure", 5))


# PLOTS

#releveling for the plots
ls10blood <- lsmeans(m10_M1blood, "group_early", by = "blood")
CLD_blood <- multcomp::cld(ls10blood,
                           alpha=0.05,
                           Letters=letters,
                           adjust="tukey")
CLD_blood$.group=gsub(" ", "", CLD_blood$.group)

ls10rel <- lsmeans(m10, "group_early", by = "rel")
CLD_rel <- multcomp::cld(ls10rel,
                         alpha=0.05,
                         Letters=letters,
                         adjust="tukey")
CLD_rel$.group=gsub(" ", "", CLD_rel$.group)

#releveling for the plots
CLD$group_early <- factor(CLD$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))
CLD_rel$group_early <- factor(CLD_rel$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))
CLD_blood$group_early <- factor(CLD_blood$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))
CLD_rel$rel <- factor(CLD_rel$rel, levels = c("ENV","2nd", "1st"))

#plot main figure
pdf(file = "PANEL_B.pdf", width = 8, height = 5)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
p_main <- ggplot(CLD,
       aes(x     = group_early,
           y     = lsmean,
           label = .group)) +
  xlab(NULL) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_errorbar(aes(ymin  =  asymp.LCL,
                    ymax  =  asymp.UCL),
                width =  0.2,
                size  =  0.7,
                position = pd) +
  theme_bw() +
  ylab("Least square log probability of exposure \nLower prevalence   <-   ->   Higher prevalence") +
  xlab("Study groups") +
  ggtitle ("Panel B: Exposure to suicidal behavior by group") +
  geom_text(nudge_x = c(0.1, 0.1, 0.1, -0.1, 0.1),
            nudge_y = c(0.95,  0.95, 0.9,  0.8, 0.8),
            color   = "black") +
#scale_color_manual(values = c("grey80", "grey60", "grey45", "grey30", "grey15"))
scale_color_manual(values = c("black", "black", "black", "black", "black"))
p_main
dev.off()
p_main

# #plots by rel
# CLD_rel$group_early <- factor(CLD_rel$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))
# pdf(file = "rel_Figure.gray.pdf", width = 8, height = 6)
# pd = position_dodge(0.8)    ### How much to jitter the points on the plot
# ggplot(CLD_rel,
#        aes(x     = group_early,
#            y     = lsmean,
#            label = .group,
#            col = rel)) +
#   xlab(NULL) +
#   geom_point(shape  = 15,
#              size   = 4,
#              position = pd) +
#   geom_errorbar(aes(ymin  =  asymp.LCL,
#                     ymax  =  asymp.UCL),
#                 width =  0.2,
#                 size  =  0.7,
#                 position = pd) +
#   theme_classic() +
#   theme(axis.title   = element_text(face = "bold"),
#         axis.text    = element_text(face = "bold"),
#         plot.caption = element_text(hjust = 0),
#         legend.title= element_blank()) +
#   ylab("Least square log probability of exposures \nLower prevalence   <-   ->   Higher prevalence") +
#   ggtitle ("Occurrence of suicidal behavior by degree of relation",
#            subtitle = "Binary logistic mixed-effects model") +
#   labs(caption  = paste0("\n",
#                          "Boxes indicate estimated marginal mean logit probability.\n",
#                          "Error bars indicate the 95% ",
#                          "confidence interval of the LS mean. \n",
#                          "Means sharing a letter are ",
#                          "not significantly different ",
#                          "(Tukey-adjusted comparisons)."),
#        hjust=0.5) +
#   geom_text(#position = "identity",
#             nudge_x = c(-0.1, -0.1, -0.1, -0.1, -0.1, 0.4, 0.4, 0.4, 0.4, 0.4, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
#             nudge_y = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
#             color   = c("grey80","grey80","grey80","grey80","grey80",
#                         "grey60", "grey60","grey60","grey60","grey60",
#                         "black","black","black","black","black")) +
#             # color   = c("darkolivegreen3","darkolivegreen3","darkolivegreen3","darkolivegreen3","darkolivegreen3",
#             #             "orange2", "orange2","orange2","orange2","orange2",
#             #             "orchid3","orchid3","orchid3","orchid3","orchid3")) +
#   scale_color_manual(values = c("grey80", "grey60", "black"))
# # scale_color_manual(values = c("darkolivegreen3","orchid3","orange2"))
# dev.off()

CLD_rel
# NEW rel plot
CLD_rel$group_early <- factor(CLD_rel$group_early, levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators", "Early-onset\nattempters","Late-onset\nattempters"))
CLD_rel$intercept[CLD_rel$group_early == "Non-psychiatric\ncontrols"] <- min(CLD_rel$lsmean[CLD_rel$group_early == "Non-psychiatric\ncontrols"])
CLD_rel$intercept[CLD_rel$group_early == "Non-suicidal\ndepressed"] <- min(CLD_rel$lsmean[CLD_rel$group_early == "Non-suicidal\ndepressed"])
CLD_rel$intercept[CLD_rel$group_early == "Suicide\nideators"] <- min(CLD_rel$lsmean[CLD_rel$group_early == "Suicide\nideators"])
CLD_rel$intercept[CLD_rel$group_early ==  "Early-onset\nattempters"] <- min(CLD_rel$lsmean[CLD_rel$group_early ==  "Early-onset\nattempters"])
CLD_rel$intercept[CLD_rel$group_early == "Late-onset\nattempters"] <- min(CLD_rel$lsmean[CLD_rel$group_early == "Late-onset\nattempters"])

pdf(file = "PANEL_C.pdf", width = 8, height = 4)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
p_rel <- ggplot(CLD_rel,
       aes(x     = rel,
           y     = lsmean,
           group = group_early)) +
        theme_bw() +
  geom_point(shape  = 15,
             size   = 4,
             position = pd,
             color = c('black', 'grey60','grey60','black', 'grey60','grey60','black', 'grey60','grey60','black', 'grey60','grey60','black', 'grey60','grey60')) +
#  geom_line()  +
#  geom_smooth(method = "lm", se = FALSE, color = 'grey80') +
  facet_grid(. ~ group_early) +
#  geom_hline(aes(yintercept = intercept), CLD_rel, color = 'grey80', size = 1.5, linetype="dashed") +
  ggtitle ("Panel C: Exposure to suicidal behavior by group and degree of relation") +
  xlab("Degree of relation") +
  ylab("Least square log probability of exposures \nLower prevalence   <-   ->   Higher prevalence")
  

p_rel
dev.off()
p_rel

## NEW rel plots: mock data
# No evidence for familial transmission
CLD_mock <- cbind(c(1.2, 1, 1.1, 3, 3.1, 3.15), c('ENV', '2nd', '1st', 'ENV', '2nd', '1st'), c('hypothetical\ncontrols','hypothetical\ncontrols','hypothetical\ncontrols','hypothetical\nearly-onset attempters','hypothetical\nearly-onset attempters','hypothetical\nearly-onset attempters'))
colnames(CLD_mock) <- c('lsmean', 'rel', 'group')
CLD_mock <- data.frame(CLD_mock)
CLD_mock$rel <- factor(CLD_mock$rel, levels = c('ENV', '2nd', '1st'))
CLD_mock$lsmean <- as.numeric(as.character(CLD_mock$lsmean))
CLD_mock$intercept[CLD_mock$group == 'hypothetical\ncontrols'] <- min(CLD_mock$lsmean[CLD_mock$group == "hypothetical\ncontrols"])
CLD_mock$intercept[CLD_mock$group == 'hypothetical\nearly-onset attempters'] <- min(CLD_mock$lsmean[CLD_mock$group == 'hypothetical\nearly-onset attempters'])
CLD_mock$intercept <- as.numeric(CLD_mock$intercept)


pd = position_dodge(0.8)    ### How much to jitter the points on the plot
p_mock <- ggplot(CLD_mock,
                 aes(x     = rel,
                     y     = lsmean,
                     group = group)) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd,
             color = c('black', 'grey60','grey60','black', 'grey60','grey60')) +
#  geom_line()  +
#  geom_smooth(method = "lm", se = FALSE, color = 'grey80') +
  facet_grid(. ~ group) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
  geom_hline(aes(yintercept = intercept), CLD_mock, color = 'grey80', size = 1, linetype="dashed") +
  ylim(0,5) +
  ggtitle ("Panel A: Illustrative diagram\nEvidence for social but not familial transmission") +
  xlab("Degree of relation") +
  ylab("Probability of exposures")


# evidence for familial transmission
CLD_mock2 <- cbind(c(1.2, 1, 1.1, 3, 3.5, 4.5), c('ENV', '2nd', '1st', 'ENV', '2nd', '1st'), c('hypothetical\ncontrols','hypothetical\ncontrols','hypothetical\ncontrols','hypothetical\nearly-onset attempters','hypothetical\nearly-onset attempters','hypothetical\nearly-onset attempters'))
colnames(CLD_mock2) <- c('lsmean', 'rel', 'group')
CLD_mock2 <- data.frame(CLD_mock2)
CLD_mock2$rel <- factor(CLD_mock2$rel, levels = c('ENV', '2nd', '1st'))
CLD_mock2$lsmean <- as.numeric(as.character(CLD_mock2$lsmean))
CLD_mock2$intercept[CLD_mock2$group == 'hypothetical\ncontrols'] <- min(CLD_mock2$lsmean[CLD_mock2$group == "hypothetical\ncontrols"])
CLD_mock2$intercept[CLD_mock2$group == 'hypothetical\nearly-onset attempters'] <- min(CLD_mock2$lsmean[CLD_mock2$group == 'hypothetical\nearly-onset attempters'])
CLD_mock2$intercept <- as.numeric(CLD_mock2$intercept)

pd = position_dodge(0.8)    ### How much to jitter the points on the plot
p_mock2 <- ggplot(CLD_mock2,
          aes(x     = rel,
              y     = lsmean,
           group = group)) +
      geom_point(shape  = 15,
             size   = 4,
             position = pd,
             color = c('black', 'grey60','grey60','black', 'grey60','grey60')) +
#  geom_line()  +
#  geom_smooth(method = "lm", se = FALSE, color = 'grey80') +
  facet_grid(. ~ group) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(),axis.title.y=element_blank()) +
  geom_hline(aes(yintercept = intercept), CLD_mock2, color = 'grey80', size = 1, linetype="dashed") +
  ylim(0,5) +
  ggtitle ("\nEvidence for social and familial transmission") +
  xlab("Degree of relation")
  
library(grid)
library(gridExtra)
pdf(file = "PANEL_A.pdf", width = 8.5, height = 3.5)
grid.arrange(p_mock, p_mock2, ncol = 2)
p_mock_combined <- grid.arrange(p_mock, p_mock2, ncol = 2)
dev.off()

# pdf(file = "combined_figure.pdf", width = 8, height = 6)
# p_mock_combined
# 
# grid.arrange(p_main, p_mock_combined, p_rel, ncol = 1)
# dev.off()

#plot blood vs env
CLD_blood$group_early <- factor(CLD_blood$group_early, 
                                levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators",
                                           "Early-onset\nattempters","Late-onset\nattempters"))

pdf(file = "blood_Figure.pdf", width = 8, height = 6)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
ggplot(CLD_blood,
       aes(x     = group_early,
           y     = lsmean,
           label = .group,
           col = blood)) +
  xlab(NULL) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_errorbar(aes(ymin  =  asymp.LCL,
                    ymax  =  asymp.UCL),
                width =  0.2,
                size  =  0.7,
                position = pd) +
  theme_classic() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        legend.title= element_blank()) +
  ylab("Least square log probability of exposures \nLower prevalence   <-   ->   Higher prevalence") +
  ggtitle ("Occurrence of suicidal behavior in family vs nonrelatives",
           subtitle = "Binary logistic mixed-effects model") +
  labs(caption  = paste0("\n",
                         "Boxes indicate estimated marginal mean logit probability.\n",
                         "Error bars indicate the 95% ",
                         "confidence interval of the LS mean. \n",
                         "Means sharing a letter are ",
                         "not significantly different ",
                         "(Tukey-adjusted comparisons)."),
       hjust=0.5) +
  geom_text(#position = "identity",
    nudge_x = c(-0.07, -0.07, -0.07, -0.07, -0.07, 0.3, 0.3, 0.3, 0.3, 0.3),
    nudge_y = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5),
    color   = c("gray50","gray50","gray50","gray50","gray50",
                "violetred4", "violetred4", "violetred4", "violetred4", "violetred4")) +
  scale_color_manual(values = c("gray50","violetred3"))
dev.off()



#plot blood vs env
CLD_blood$group_early <- factor(CLD_blood$group_early, 
                                levels = c("Non-psychiatric\ncontrols","Non-suicidal\ndepressed", "Suicide\nideators",
                                           "Early-onset\nattempters","Late-onset\nattempters"))

pdf(file = "blood_Figure.pdf", width = 8, height = 6)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
ggplot(CLD_blood,
       aes(x     = group_early,
           y     = lsmean,
           label = .group,
           col = blood)) +
  xlab(NULL) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_errorbar(aes(ymin  =  asymp.LCL,
                    ymax  =  asymp.UCL),
                width =  0.2,
                size  =  0.7,
                position = pd) +
  theme_classic() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        legend.title= element_blank()) +
  ylab("Least square log probability of exposures \nLower prevalence   <-   ->   Higher prevalence") +
  ggtitle ("Occurrence of suicidal behavior in family vs nonrelatives",
           subtitle = "Binary logistic mixed-effects model") +
  labs(caption  = paste0("\n",
                         "Boxes indicate estimated marginal mean logit probability.\n",
                         "Error bars indicate the 95% ",
                         "confidence interval of the LS mean. \n",
                         "Means sharing a letter are ",
                         "not significantly different ",
                         "(Tukey-adjusted comparisons)."),
       hjust=0.5) +
  geom_text(#position = "identity",
    nudge_x = c(-0.07, -0.07, -0.07, -0.07, -0.07, 0.3, 0.3, 0.3, 0.3, 0.3),
    nudge_y = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5),
    color   = c("gray50","gray50","gray50","gray50","gray50",
                "violetred4", "violetred4", "violetred4", "violetred4", "violetred4")) +
  scale_color_manual(values = c("gray50","violetred3"))
dev.off()


# 'pdf(file = "Exposure by group PRETTY.m10and10blood.pdf", width = 8, height = 6)
# pd = position_dodge(0.8)    ### How much to jitter the points on the plot
# ggplot(CLD,
#        aes(x     = group_early,
#            y     = lsmean,
#            label = .group)) +
#   xlab(NULL) +
#   geom_point(shape  = 15,
#              size   = 4,
#              position = pd) +
#   geom_errorbar(aes(ymin  =  asymp.LCL,
#                     ymax  =  asymp.UCL),
#                 width =  0.2,
#                 size  =  0.7,
#                 position = pd) +
#   theme_bw() +
#   theme(axis.title   = element_text(face = "bold"),
#         axis.text    = element_text(face = "bold"),
#         plot.caption = element_text(hjust = 0)) +
#   ylab("Least square log probability of suicidal behavior among family or friends \nLower prevalence   <-   ->   Higher prevalence") +
#   ggtitle ("Occurrence of suicidal behavior among family or friends by group",
#            subtitle = "Binary logistic mixed-effects model") +
#   labs(caption  = paste0("\n",
#                          "Boxes indicate estimated marginal mean logit probability.\n",
#                          "Error bars indicate the 95% ",
#                          "confidence interval of the LS mean. \n",
#                          "Means sharing a letter are ",
#                          "not significantly different ",
#                          "(Tukey-adjusted comparisons)."),
#        hjust=0.5) +
#   geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1),
#             nudge_y = c(0.8,  0.8, 0.8,  0.8, 0.8),
#             color   = "black")
# dev.off()

# #plot figure m10blood
# CLD_blood <- multcomp::cld(ls10blood,
#                      alpha=0.05,
#                      Letters=letters,
#                      adjust="tukey")
# CLD_blood$.group=gsub(" ", "", CLD_blood$.group)
# # CLD$g=c("Non-psychiatric controls", "Non-suicidal depressed", "Suicide ideators", "Early-onset attempters", "Late-onset attempters")
#pdf(file = "Exposure by group PRETTY.finalModelWithEnv.pdf", width = 8, height = 6)
# pd = position_dodge(0.8)    ### How much to jitter the points on the plot
# ggplot(CLD,
#        aes(x     = group_early,
#            y     = lsmean,
#            label = .group)) +
#   xlab(NULL) +
#   geom_point(shape  = 15,
#              size   = 4,
#              position = pd) +
#   geom_errorbar(aes(ymin  =  asymp.LCL,
#                     ymax  =  asymp.UCL),
#                 width =  0.2,
#                 size  =  0.7,
#                 position = pd) +
#   theme_bw() +
#   theme(axis.title   = element_text(face = "bold"),
#         axis.text    = element_text(face = "bold"),
#         plot.caption = element_text(hjust = 0)) +
#   ylab("Least square log probability of suicidal behavior among family or friends \nLower prevalence   <-   ->   Higher prevalence") +
#   ggtitle ("Occurrence of suicidal behavior among blood relatives by group",
#            subtitle = "Binary logistic mixed-effects model") +
#   labs(caption  = paste0("\n",
#                          "Boxes indicate estimated marginal mean logit probability.\n",
#                          "Error bars indicate the 95% ",
#                          "confidence interval of the LS mean. \n",
#                          "Means sharing a letter are ",
#                          "not significantly different ",
#                          "(Tukey-adjusted comparisons)."),
#        hjust=0.5) +
#   geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1),
#             nudge_y = c(0.8,  0.8, 0.8,  0.8, 0.8),
#             color   = "black")
# dev.off()


#percentages/distributions for 10/17 presentation

n_all <- table(df$group_early)

a_1st <- table(df$group_early[df$num1stExposuresSA != 0])
a_2nd <- table(df$group_early[df$num2ndExposuresSA != 0])
a_rel <- table(df$group_early[df$num2ndExposuresSA != 0 | df$num1stExposuresSA != 0])
a_env <- table(df$group_early[df$numEnvExposuresSA != 0])

c_1st <- table(df$group_early[df$num1stExposuresSC != 0])
c_2nd <- table(df$group_early[df$num2ndExposuresSC != 0])
c_rel <- table(df$group_early[df$num2ndExposuresSC != 0 | df$num1stExposuresSC != 0])
c_env <- table(df$group_early[df$numEnvExposuresSC != 0])

ac_1st <- table(df$group_early[df$num1stExposuresSA != 0 | df$num1stExposuresSC != 0])
ac_2nd <- table(df$group_early[df$num2ndExposuresSA != 0 | df$num2ndExposuresSC != 0])
ac_env <- table(df$group_early[df$numEnvExposuresSA != 0 | df$numEnvExposuresSC != 0])


na_1st <- table(df$num1stExposuresSA == 0)
nc_1st <- table(df$num1stExposuresSC == 0)
na_2nd <- table(df$num2ndExposuresSA == 0)

nc_2nd <- table(df$num2ndExposuresSC == 0)
na_env <- table(df$numEnvExposuresSA == 0)
nc_env <- table(df$numEnvExposuresSC == 0)

#as dataframes
n_all <- data.frame(n_all)
ac_1st <- data.frame(ac_1st)
ac_2nd <- data.frame(ac_2nd)
ac_env <- data.frame(ac_env)

# running a pretty bar plot of descriptives
dff <- cbind(n_all$Freq, ac_env$Freq, ac_1st$Freq, ac_2nd$Freq)
dimnames(dff) <- list(Groups=c("Healthy","Depressed","Ideators","Early-onset", "Late-onset"),
                      Exposures = c("Total group 'n'", "Non-relative exposures", "1st degree exposures", "2nd degree exposures"))
dat <- as.table(dff)
dat2 <- data.frame(dat)

p <-
  ggplot(dat2, aes(Groups, Freq)) +
  theme(panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 85)) +
  scale_fill_brewer(palette="PiYG") +
  geom_bar(aes(fill = Exposures), stat="identity", position="dodge", width=.9)
p
#p + labs(title = "DSM personality traits", y = "mean score")

#TABLES
names(df)
# Table 1 with the variables Kati suggested
chars <- df[,c(127,2,7,121,9,126,18,40,42,25,130,13,14,11,17,119,120,122,123,124,125)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
tc2 <- createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1, 
                   show.p.mul = TRUE, show.ratio = TRUE)
export2html(tc2, "Table1.kati2.html")

names(df)

# Table 1 with SB only
chars <- df[,c(110,112,114,12)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
tc4 <- createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1, 
                   show.p.mul = TRUE, show.ratio = TRUE)
export2html(tc4, "SB_rel_table.html")


# Table 1 with SB percents only
chars <- df[,c(119,118,120,133)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
tc4 <- createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1, 
                   show.p.mul = TRUE, show.ratio = TRUE)
export2html(tc4, "SB_%_table.html")

# Table with personality
chars <- df_neo[,c(133:137)]
describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
tc3 <- createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1, 
                   show.p.mul = TRUE, show.ratio = TRUE)
export2html(tc3, "Table.Pers.html")

# # MORE COMPARISONS FOR A POSSIBLE FUTURE PAPER
# chars <- df[df$GROUP1245==5,c(19:37)]
# # describe.by(chars,group = df$group_early_no_break)
# c1 <- compareGroups(chars,df$group_early_no_break[df$GROUP1245==5], bivar=TRUE)
# t1 <- createTable(c1,hide = NA, hide.no = 0, digits = 1, show.n = TRUE)
# export2html(t1, "early_vs_late.html")
# 
# chars <- df[,c(19:37)]
# # describe.by(chars,group = df$group_early_no_break)
# c2 <- compareGroups(chars,df$group_early_no_break, bivar=TRUE)
# t2 <- createTable(c2,hide = NA, hide.no = 0, digits = 1, show.n = TRUE)
# export2html(t2, "early_vs_late_comparison_groups.html")
# 
# # check anger: nothing with either measure, consider dropping from battery
# summary(tam1 <- lm(ARSTOTAL ~ group_early_no_break + BASELINEAGE + GENDERTEXT, data = df))
# anova(tam1)
# plot(lsmeans(tam1, "group_early_no_break"))
# cld(lsmeans(tam1, "group_early_no_break"))
# 

###### additional analyses ######
# creating new variables
df$suicidal <- NA
df$suicidal[df$GROUP1245 == '4'|df$GROUP1245 == '5'] <- 1
df$suicidal[df$GROUP1245 == '1'|df$GROUP1245 == '2'] <- 0
df$suicidal <- as.factor(df$suicidal)

df$attempting <- NA
df$attempting[df$GROUP1245 == '5'] <- 1
df$attempting[df$GROUP1245 == '1'|df$GROUP1245 == '2'|df$GROUP1245 == '4'] <- 0
df$attempting <- as.factor(df$attempting)


# correlation tables
library(corrplot)
library(data.table)
chars <- df_noHealthy[,c(19,20,29,30,31,32,33,130,34,35,36,37)]
corrplot.mixed(cor(chars, method = 'spearman', use = 'na.or.complete'), lower.col = 'black', number.cex = 1.1)
corrplot(cor(chars, method = 'spearman', use = 'na.or.complete'), number.cex = 1.1)

chars2 <- df_noHealthy[,c(19,20,24,33,130)]
corrplot.mixed(cor(chars2, method = 'spearman', use = 'na.or.complete'), lower.col = 'black', number.cex = 1.1)
corrplot(cor(chars2, method = 'spearman', use = 'na.or.complete'), number.cex = 1.1)

#additional models predicting suicidality (attempt + ideation)
library(dplyr)
neo_suppl <- read_excel('LAURA 05-10-17 NEO 3 UPDATED 05-15-18.xlsx')
df_neo <- left_join(df,neo_suppl, by=c("ID"))

#creating a dataset without HC
df_neo_noHealthy <- df_neo[df$GROUP1245!='1',]

#1st degree exposure

df_neo_noHealthy$GENDERTEXT <- as.factor(df_noHealthy$GENDERTEXT)

# PERSONALITY ZONE
#1st degree
summary(m1_additional_1stdeg_IIP <- glm(suicidal ~  I(num1stExposuresSB>0) * scale(IIP15INTAMBV) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_IIP, type = "III")
vif(m1_additional_1stdeg_IIP)

summary(m1_additional_1stdeg_1 <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(NEUROTICISM) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_1, type = "III")
vif(m1_additional_1stdeg_1)

summary(m1_additional_1stdeg_2 <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(EXTRAVERSION) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_2, type = "III")
vif(m1_additional_1stdeg_2)

summary(m1_additional_1stdeg_3 <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(OPENNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_3, type = "III")
vif(m1_additional_1stdeg_3)

summary(m1_additional_1stdeg_4 <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(AGREEABLENESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_4, type = "III")
vif(m1_additional_1stdeg_4)

summary(m1_additional_1stdeg_5 <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(CONSCIENTIOUSNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_5, type = "III")
vif(m1_additional_1stdeg_5)

summary(m1_additional_1stdeg_BIS <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(BIS_TOT) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_BIS, type = "III")
vif(m1_additional_1stdeg_BIS)

summary(m1_additional_1stdeg_SPSIICSSUB <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(SPSIICSSUB) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_SPSIICSSUB, type = "III")
vif(m1_additional_1stdeg_SPSIICSSUB)

summary(m1_additional_1stdeg_UPPSPNEGURGENCY <- glm(suicidal ~  I(num1stExposuresSB>0)*scale(UPPSPNEGURGENCY) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_1stdeg_UPPSPNEGURGENCY, type = "III")
vif(m1_additional_1stdeg_UPPSPNEGURGENCY)


#2nd degree
summary(m1_additional_2nddeg_demo <- glm(suicidal ~  I(num2ndExposuresSB>0) * IIP15INTAMBV + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_demo, type = "III")
vif(m1_additional_2nddeg_demo)

summary(m1_additional_2nddeg_1 <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(NEUROTICISM) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_1, type = "III")
vif(m1_additional_2nddeg_1)

summary(m1_additional_2nddeg_2 <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(EXTRAVERSION) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_2, type = "III")
vif(m1_additional_2nddeg_2)

summary(m1_additional_2nddeg_3 <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(OPENNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_3, type = "III")
vif(m1_additional_2nddeg_3)

summary(m1_additional_2nddeg_4 <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(AGREEABLENESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_4, type = "III")
vif(m1_additional_2nddeg_4)

summary(m1_additional_2nddeg_5 <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(CONSCIENTIOUSNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_5, type = "III")
vif(m1_additional_2nddeg_5)

summary(m1_additional_2nddeg_BIS <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(BIS_TOT) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_BIS, type = "III")
vif(m1_additional_2nddeg_BIS)

summary(m1_additional_2nddeg_SPSIICSSUB <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(SPSIICSSUB) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_SPSIICSSUB, type = "III")
vif(m1_additional_2nddeg_SPSIICSSUB)

summary(m1_additional_2nddeg_UPPSPNEGURGENCY <- glm(suicidal ~  I(num2ndExposuresSB>0)*scale(UPPSPNEGURGENCY) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_2nddeg_UPPSPNEGURGENCY, type = "III")
vif(m1_additional_2nddeg_UPPSPNEGURGENCY)


#env
summary(m1_additional_Envdeg_demo <- glm(suicidal ~  I(numEnvExposuresSB>0) * IIP15INTAMBV + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_demo, type = "III")
vif(m1_additional_Envdeg_demo)

summary(m1_additional_Envdeg_1 <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(NEUROTICISM) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_1, type = "III")
vif(m1_additional_Envdeg_1)

summary(m1_additional_Envdeg_2 <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(EXTRAVERSION) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_2, type = "III")
vif(m1_additional_Envdeg_2)

summary(m1_additional_Envdeg_3 <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(OPENNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_3, type = "III")
vif(m1_additional_Envdeg_3)

summary(m1_additional_Envdeg_4 <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(AGREEABLENESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_4, type = "III")
vif(m1_additional_Envdeg_4)

summary(m1_additional_Envdeg_5 <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(CONSCIENTIOUSNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_5, type = "III")
vif(m1_additional_Envdeg_5)

summary(m1_additional_Envdeg_BIS <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(BIS_TOT) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_BIS, type = "III")
vif(m1_additional_Envdeg_BIS)

summary(m1_additional_Envdeg_SPSIICSSUB <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(SPSIICSSUB) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_SPSIICSSUB, type = "III")
vif(m1_additional_Envdeg_SPSIICSSUB)

summary(m1_additional_Envdeg_UPPSPNEGURGENCY <- glm(suicidal ~  I(numEnvExposuresSB>0)*scale(UPPSPNEGURGENCY) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Envdeg_UPPSPNEGURGENCY, type = "III")
vif(m1_additional_Envdeg_UPPSPNEGURGENCY)

#blood
summary(m1_additional_Reldeg_demo <- glm(suicidal ~  I(numRelExposuresSB>0) * IIP15INTAMBV + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_demo, type = "III")
vif(m1_additional_Reldeg_demo)

summary(m1_additional_Reldeg_1 <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(NEUROTICISM) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_1, type = "III")
vif(m1_additional_Reldeg_1)

summary(m1_additional_Reldeg_2 <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(EXTRAVERSION) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_2, type = "III")
vif(m1_additional_Reldeg_2)

summary(m1_additional_Reldeg_3 <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(OPENNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_3, type = "III")
vif(m1_additional_Reldeg_3)

summary(m1_additional_Reldeg_4 <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(AGREEABLENESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_4, type = "III")
vif(m1_additional_Reldeg_4)

summary(m1_additional_Reldeg_5 <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(CONSCIENTIOUSNESS) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_5, type = "III")
vif(m1_additional_Reldeg_5)

summary(m1_additional_Reldeg_BIS <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(BIS_TOT) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_BIS, type = "III")
vif(m1_additional_Reldeg_BIS)

summary(m1_additional_Reldeg_SPSIICSSUB <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(SPSIICSSUB) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_SPSIICSSUB, type = "III")
vif(m1_additional_Reldeg_SPSIICSSUB)

summary(m1_additional_Reldeg_UPPSPNEGURGENCY <- glm(suicidal ~  I(numRelExposuresSB>0)*scale(UPPSPNEGURGENCY) + scale(BASELINEAGE) + GENDERTEXT + scale(EDUCATION), family = binomial, data = df_neo_noHealthy))
car::Anova(m1_additional_Reldeg_UPPSPNEGURGENCY, type = "III")
vif(m1_additional_Reldeg_UPPSPNEGURGENCY)

# models predicting risk for attempting only

ls10_M1blood <- lsmeans(m10_M1blood, "group_early")
contrast(ls10_M1blood, method = "pairwise", adjust ="tukey")
plot(ls10_M1blood, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")


##correlations and plot that Kati wanted to see
df_onlyAtt <- df[df$GROUP1245 == '5',]
View(df_onlyAtt)

chars3 <- df_onlyAtt[,c(11,12,14,108,114)]
chars3$numRelExposuresSB <- as.factor(chars3$numRelExposuresSB)
chars3$numEnvExposuresSB <- as.factor(chars3$numEnvExposuresSB)


corrplot.mixed(cor(chars3, method = 'spearman', use = 'na.or.complete'), lower.col = 'black', number.cex = 1.1)
corrplot(cor(chars3, method = 'spearman', use = 'na.or.complete'), number.cex = 1.1)


plot(df_onlyAtt$MAXLETHALITY,df_onlyAtt$numRelExposuresSB, type = 'h', xlab = 'Lethality', ylab = 'Exposure to SB in relatives')
plot(df_onlyAtt$MAXLETHALITY,df_onlyAtt$numEnvExposuresSB, type = 'h', xlab = 'Lethality', ylab = 'Exposure to SB in relatives')


plot(df_onlyAtt$AGEATFIRSTATTEMPT,df_onlyAtt$numRelExposuresSB, type = 'h', xlab = 'Age of onset', ylab = 'Exposure to SB in relatives')
plot(df_onlyAtt$AGEATFIRSTATTEMPT,df_onlyAtt$numEnvExposuresSB, type = 'h', xlab = 'Age of onset', ylab = 'Exposure to SB in non-relatives')

plot(df_onlyAtt$TOTALATTEMPTS,df_onlyAtt$numRelExposuresSB, type = 'h', xlab = 'Number of attempts', ylab = 'Exposure to SB in relatives')
plot(df_onlyAtt$TOTALATTEMPTS,df_onlyAtt$numEnvExposuresSB, type = 'h', xlab = 'Number of attempts', ylab = 'Exposure to SB in non-relatives')

histogram(df_onlyAtt$AGEATFIRSTATTEMPT, df_onlyAtt$numEnvExposuresSB)

library(ggplot2)
dev.off()
g <- ggplot(chars3, aes(AGEATFIRSTATTEMPT))
g + theme_set(theme_classic())
+ geom_density(aes(fill=numRelExposuresSB), alpha=0.8) + 
  labs(title="Histogram by exposure", 
       subtitle="Age of onset of 1st attempt",
       x="Age at 1st attempt",
       fill="Number of family exposures")


g <- ggplot(chars3, aes(AGEATFIRSTATTEMPT))
g + geom_bar(aes(fill=numRelExposuresSB), width = 1) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Family exposures by age of onset",
       x="Age at 1st attempt",
       fill="Family exposures") +
  scale_fill_manual(values=c("grey80", "orchid1", "orchid3", "orchid4"))

g1 <- ggplot(chars3, aes(AGEATFIRSTATTEMPT))
g1 + geom_bar(aes(fill=numEnvExposuresSB), width = 1) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Environmental exposures by age of onset",
       x="Age at 1st attempt",
       fill="Environmental exposures") +
  scale_fill_manual(values=c("grey80","turquoise1", "turquoise2", "turquoise3", "turquoise4"))

#trying to get basic group differences in personality
df_neo_condensed <- df_neo[,c(1,116,130,133:137)]
as.data.frame(df_neo_condensed)
names(df_neo_condensed)
chisq.test(2,c(3:8))

