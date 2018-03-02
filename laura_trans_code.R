
# hist(df$MOMMH)
# describe(df$ENVRNMTL)
# unique(df$ENVRNMTL)

#install.packages(c("car","readr", "lme4", "ggplot2", "tidyr", "psych", "gdata", "xtable", "Hmisc", "nnet", 
#"reshape2", "corrplot", "lsmeans", "readxl", "MASS", "stargazer", "compareGroups", "multcompView","RColorBrewer"))

#install.packages("fifer")
#library(fifer)

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
setwd("~/Dropbox/USA/Pittsburgh/GitHub/transmission/transmission")
df <- read_delim("FAMHX_DEMOG_COUNTS_MERGED_10.4.17.csv",
                 ",", escape_double = FALSE, trim_ws = TRUE)


#at home
setwd("C:/Users/Laura/Box Sync/skinner/projects_analyses/Project Transmission")
df <- read_delim("C:/Users/Laura/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED_10.4.17.dat",
                  "\t", escape_double = FALSE, trim_ws = TRUE)
View(df)
names(df)


load("trans.Rda")
# library(VIM)
# df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#checking missingness
library(mice)
md.pattern(df)

library(VIM)
df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


# designate factors

df$GROUP1245 <- as.factor(df$GROUP1245)
df$GROUP12467 <- as.factor(df$GROUP12467)
df$SubstanceLifetime[df$GROUP1245==1] <- NA
df$HRSDNOSUICIDE[df$GROUP1245==1] <- NA
df$AnxietyLifetime[df$GROUP1245==1] <- NA
df$TOTALATTEMPTS[df$GROUP1245!=5] <- NA
df$MAXLETHALITY[df$GROUP1245!=5] <- NA

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
df$SubstanceLifetime <- as.factor(df$SubstanceLifetime)
df$AnxietyLifetime <- as.factor(df$AnxietyLifetime)

# recode a variable: everyone with a h/o suicidal behavior among 2nd degree relatives
df$seconddegSB <- 0
df$seconddegSB[df$GPATT=="B" | df$GPATT=="C" |df$HALFSIBATT == "B"|df$HALFSIBATT == "C" | df$GCATT == "B" |df$GCATT == "C"] <- 1
df$seconddegSB[df$GPATT=="b" | df$GPATT=="c" |df$HALFSIBATT == "b"|df$HALFSIBATT == "c" | df$GCATT == "b" |df$GCATT == "c"] <- 1
df$seconddegSB[is.na(df$GPATT) | is.na(df$GPATT)| is.na(df$HALFSIBATT)  | is.na(df$HALFSIBATT) | is.na(df$GCATT) | is.na(df$GCATT)] <- "NA"

df$firstdegSB <- df$firstdegSA

df$firstdegSC <- as.factor(df$firstdegSC)

df$bloodSB<-0
df$bloodSB[df$firstdegSB==1|df$seconddegSB==1]<-1
df$bloodSB[is.na(df$firstdegSB)|is.na(df$seconddegSB)]<-"NA"
View(df$bloodSB)

df$race <- "NA"
df$race[df$RACETEXT=="WHITE"] <- 0
#minority are 1
df$race[df$RACETEXT=="AFRICAN AMERICAN" | df$RACETEXT == "ASIAN PACIFIC"] <- 1

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

# discard the stupid variables

d <- d[,c(1:45,113:ncol(d))]
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

save(df, file="trans.Rda")

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
# d$group_v_early <- d$GROUP
# d$group_v_early[d$AGEATFIRSTATTEMPT<60] <- "MID_ATTEMPT"
# d$group_v_early[d$AGEATFIRSTATTEMPT>59] <- "LATE_ATTEMPT"
# d$group_v_early[d$AGEATFIRSTATTEMPT<25] <- "EARLY_ATTEMPT"


summary(m6 <- glm(events ~  group_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race  + (1:ID), family = negative.binomial(theta = theta.ed), data = d))
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


summary(m8 <- glm(events ~  group_early + blood  + BASELINEAGE +  EDUCATION  + race  + (1:ID), family = negative.binomial(theta = theta.ed), data = d[d$sev=="suicide",]))
car::Anova(m8, type = "III")
ls8 <- lsmeans(m8, "group_early")
plot(ls8)
multcomp::cld(ls8)

# exclude healthy controls
dd <- d[d$GROUP1245 != "1",]


#show that the three-way interaction was only significant because of healthy controls
summary(m6.no_healthy <- glm(events ~  group_early*sev*blood  + BASELINEAGE*sev +  EDUCATION  + race  + (1:ID), family = negative.binomial(theta = theta.ed), data = dd))
car::Anova(m6.no_healthy, type = "III")
ls6 <- lsmeans(m6.no_healthy, "group_early", by = (c("sev","blood")))
plot(ls6)
ls6a <- lsmeans(m6.no_healthy, "group_early")
plot(ls6a)
multcomp::cld(ls6a)


# dichotomize exposure
d$exp <- d$events>0
summary(m9 <- glm(exp ~  group_early*sev +group_early*blood + BASELINEAGE*sev +  EDUCATION  + race  + (1:ID), family = binomial, data = d))
car::Anova(m9, type = "III")
lsmip(m9,  sev ~ group_early |blood, ylab = "log(response rate)", xlab = "type ", type = "predicted" )
ls9 <- lsmeans(m9, "group_early", by = "blood")
plot(ls9, horiz = F)
multcomp::cld(ls9)


##FINAL MODEL WITH ENV
d$rel <- factor(d$rel)
d$rel <- relevel(d$rel, ref = 'ENV')
summary(m10 <- glm(exp ~  group_early*rel + rel*sev + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m10, type = "III")
ls10 <- lsmeans(m10, "group_early")
contrast(ls10, method = "pairwise", adjust ="tukey")
plot(ls10, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")


# do not worry about this interaction plot -- just did it to rule out familial clustering once and for all
ls10a <- lsmeans(m10, "group_early", by = "rel")
plot(ls10a, horiz = F)
multcomp::cld(ls10a)

# evaluate interaction- nonrel, more exp to completions
ls10b <- lsmeans(m10, "sev", by = "rel")
plot(ls10b, horiz = F)
multcomp::cld(ls10b)

# evaluate interaction- older, more exp to completions
ls10c <- lsmeans(m10, "BASELINEAGE", by = "sev", at = list(BASELINEAGE = c(40,60,80)))
plot(ls10c, horiz = F)
multcomp::cld(ls10c)

# evaluate main effect- caucasions, more exposure
ls10d <- lsmeans(m10, "race")
plot(ls10d, horiz = F)
multcomp::cld(ls10d)


anova(m9,m10,test = "Rao")


summary(m10a <- glm(exp[d$blood == 'rel'] ~  group_early + sev + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m10a, type = "III")
ls10e <- lsmeans(m10a, "group_early")
contrast(ls10e, method = "pairwise", adjust ="tukey")
plot(ls10e, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")

anova(m10a,m10,test="Rao")

# evaluate interaction- nonrel, more completions- not the yn model interaction
ls10aBlood <- lsmeans(m10a, "sev", by = "blood")
plot(ls10aBlood, horiz = F)
multcomp::cld(ls10aBlood)


summary(m11 <- glm(exp ~  group_early + rel*sev + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m11, type = "III", test.statistic = c("F"))
ls11 <- lsmeans(m11, "group_early")
multcomp::cld(ls11, sort = FALSE)
plot(ls11, horiz = F)
contrast(ls11, method = "pairwise", adjust ="tukey")


# evaluate interaction- older people, less attempts
ls11age <- lsmeans(m11, "sev", by = "BASELINEAGE", at = list(BASELINEAGE = c(40,60,80)))
plot(ls11age, horiz = F)
multcomp::cld(ls11age)


# evaluate interaction- environment, more completions
ls11env <- lsmeans(m11, "sev", by = "rel")
plot(ls11env, horiz = F)
multcomp::cld(ls11env)


anova(m11,m11b,test = "Rao")

#checking if we have a good reason to keep education in there.
summary(m11b <- glm(exp ~  group_early + rel*sev + BASELINEAGE*sev +  race + (1:ID), family = binomial, data = d))
car::Anova(m11b, type = "III")
ls11b <- lsmeans(m11b, "group_early")
plot(ls11b, horiz = F)


anova(m11,m11b,test = "Rao")


names(d)
aggregate(d[,54], list(d$group_early), mean, na.rm= TRUE)


# specifically test group*relation to rule out familial clustering: NS, does not improve fit
summary(m11a <- glm(exp ~  group_early*sev*rel + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m11a, type = "III")

anova(m11,m11a, test = "Rao")

ls11a2 <- lsmeans(m11a, "group_early", by = c("rel","sev"))
plot(ls11a2)

#since plot does not work, try without HC group
dd$exp <- dd$events>0

summary(m11_noHealthy <- glm(exp ~  group_early*sev*rel + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = dd))
car::Anova(m11_noHealthy, type = "III")
anova(m11,m11_noHealthy, test = "Rao")

ls11a2_noHealthy <- lsmeans(m11_noHealthy, "group_early", by = c("rel","sev"))
plot(ls11a2_noHealthy)

# plot NS interaction for qualitative look:  still a problem with 2nd degree
ls11a <- lsmeans(m11a, "group_early", by = "rel")
plot(ls11a)


# test substance and anxiety -- not significant
summary(m12 <- glm(exp ~  group_early + sev*rel + BASELINEAGE*sev +  EDUCATION  + race + SubstanceLifetime + AnxietyLifetime + (1:ID), family = binomial, data = d))
car::Anova(m12, type = "III")

summary(m12a <- glm(exp ~  group_early + sev*rel + BASELINEAGE*sev +  race + SubstanceLifetime + AnxietyLifetime + (1:ID), family = binomial, data = d))
car::Anova(m12a, type = "III")

anova(m11b,m12, test = "Rao")

stargazer(m10,m10blood, type="html", out="trans.blood.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels=c("Exposures"), covariate.labels=c("Depressed","Ideators","Early-onset Attempters","Late-onset Attempters", 
                                                  "Relationship: 2nd Degree Relative", "Relationship: environment", 
                                                  "Suicide Severity (Completion)", "Age", "Education", "Race", 
                                                  "Depressed*2nd", "Ideator*2nd", "EoAttempter*2nd", "LoAttempter*2nd",
                                                  "Depressed*Environment", "Ideator*Environment", "EoAttempter*Environment", 
                                                  "LoAttempter*Environment", "2nd*Suicide Severity", "Environment*Suicide Severity",
                                                  "Suicide Severity*Age"))


stargazer(m10, m11, type="html", out="trans.labeled.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels=c("Exposures"), covariate.labels=c("Depressed","Ideators","Early-onset Attempters","Late-onset Attempters", 
                                                            "Relationship: 2nd Degree Relative", "Relationship: environment", 
                                                            "Suicide Severity (Completion)", "Age", "Education", "Race", 
                                                            "Depressed*2nd", "Ideator*2nd", "EoAttempter*2nd", "LoAttempter*2nd",
                                                            "Depressed*Environment", "Ideator*Environment", "EoAttempter*Environment", 
                                                            "LoAttempter*Environment", "2nd*Suicide Severity", "Environment*Suicide Severity",
                                                            "Suicide Severity*Age"))


# new dataset without enviromental exposures

df <- as.data.frame(df)

dblood = melt(df, na.rm = FALSE, measure.vars = c("num1stExposuresSC","num1stExposuresSA", "num2ndExposuresSC", "num2ndExposuresSA"))

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
# conclusion -- not a perfect fit, but OK


# build a model

# estimate theta for nb

theta.resp <- theta.ml(na.omit(dblood1e$events), mean(na.omit(dblood1e$events)), length(dblood1e$events), limit = 50, eps = .Machine$double.eps^.25, trace = FALSE)

#dichotomize exposure in dblood
dblood$exp <- dblood$events>0

summary(m10blood <- glm(exp ~  group_early + sev + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = dblood))
car::Anova(m10blood, type = "III")
ls10blood <- lsmeans(m10blood, "group_early")
contrast(ls10blood, method = "pairwise", adjust ="tukey")
multcomp::cld(ls10blood, sort = FALSE)
plot(ls10blood, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")

summary(m11blood <- glm(exp ~  group_early*sev + BASELINEAGE +  EDUCATION  + race + (1:ID), family = binomial, data = dblood))
car::Anova(m11blood, type = "III")
ls11blood <- lsmeans(m11blood, "group_early")
contrast(ls11blood, method = "pairwise", adjust ="tukey")
plot(ls11blood, type ~ d$group_early, horiz=F, ylab = "exposure to suicidal behavior", xlab = "Group")

ls11blood_b <- lsmeans(m11blood, "group_early", by = "sev")
plot(ls11blood_b, horiz = F)
multcomp::cld(ls11blood_b, sort = FALSE)


#plot figure m10
CLD <- multcomp::cld(ls10,
                     alpha=0.05,
                     Letters=letters,
                     adjust="tukey")
CLD$.group=gsub(" ", "", CLD$.group)
# CLD$g=c("Non-psychiatric controls", "Non-suicidal depressed", "Suicide ideators", "Early-onset attempters", "Late-onset attempters")


pdf(file = "Exposure by group PRETTY.m10.pdf", width = 8, height = 6)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
ggplot(CLD,
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
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Least square log probability of suicidal behavior among family or friends \nLower prevalence   <-   ->   Higher prevalence") +
  ggtitle ("Occurrence of suicidal behavior among family or friends by group",
           subtitle = "Binary logistic mixed-effects model") +
  labs(caption  = paste0("\n",
                         "Boxes indicate the LS mean log probability.\n",
                         "Error bars indicate the 95% ",
                         "confidence interval of the LS mean. \n",
                         "Means sharing a letter are ",
                         "not significantly different ",
                         "(Tukey-adjusted comparisons)."),
       hjust=0.5) +
  geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1),
            nudge_y = c(0.8,  0.8, 0.8,  0.8, 0.8),
            color   = "black")
dev.off()

#plot figure m10blood
CLD <- multcomp::cld(ls11,
                     alpha=0.05,
                     Letters=letters,
                     adjust="tukey")
CLD$.group=gsub(" ", "", CLD$.group)
# CLD$g=c("Non-psychiatric controls", "Non-suicidal depressed", "Suicide ideators", "Early-onset attempters", "Late-onset attempters")


pdf(file = "Exposure by group PRETTY.m10blood.pdf", width = 8, height = 6)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
ggplot(CLD,
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
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Least square log probability of suicidal behavior among family or friends \nLower prevalence   <-   ->   Higher prevalence") +
  ggtitle ("Occurrence of suicidal behavior among blood relatives by group",
           subtitle = "Binary logistic mixed-effects model") +
  labs(caption  = paste0("\n",
                         "Boxes indicate the LS mean log probability.\n",
                         "Error bars indicate the 95% ",
                         "confidence interval of the LS mean. \n",
                         "Means sharing a letter are ",
                         "not significantly different ",
                         "(Tukey-adjusted comparisons)."),
       hjust=0.5) +
  geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1),
            nudge_y = c(0.8,  0.8, 0.8,  0.8, 0.8),
            color   = "black")
dev.off()


#plot figure m11
CLD <- multcomp::cld(ls11,
                     alpha=0.05,
                     Letters=letters,
                     adjust="tukey")
CLD$.group=gsub(" ", "", CLD$.group)
# CLD$g=c("Non-psychiatric controls", "Non-suicidal depressed", "Suicide ideators", "Early-onset attempters", "Late-onset attempters")


pdf(file = "Exposure by group PRETTY.m11.pdf", width = 8, height = 6)
pd = position_dodge(0.8)    ### How much to jitter the points on the plot
ggplot(CLD,
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
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Least square log probability of suicidal behavior among family or friends \nLower prevalence   <-   ->   Higher prevalence") +
  ggtitle ("Occurrence of suicidal behavior among family or friends by group",
           subtitle = "Binary logistic mixed-effects model") +
  labs(caption  = paste0("\n",
                         "Boxes indicate the LS mean log probability.\n",
                         "Error bars indicate the 95% ",
                         "confidence interval of the LS mean. \n",
                         "Means sharing a letter are ",
                         "not significantly different ",
                         "(Tukey-adjusted comparisons)."),
       hjust=0.5) +
  geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1),
             nudge_y = c(0.8,  0.8, 0.8,  0.8, 0.8),
             color   = "black")
dev.off()
 
#names(df)
#View(df)
# get group characteristics and make Table 1
chars <- df[,c(2,7,8,9,10,18,40,42,14,11,17)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1)
export2html(createTable(c), "Table1.html")

# playing with Table 1
chars <- df[,c(2,7,8,9,10,18,40,42,14,11,17,106)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
tc <- createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1, show.p.mul = TRUE)
export2html(tc, "Table1.expanded.html")

# playing with Table 1- adding the variables Kati suggested
chars <- df[,c(2,7,8,9,10,18,40,42,14,11,17,106,25,28,13,94,119,124,125,126,127,128,129)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
tc2 <- createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1, 
                   show.p.mul = TRUE, show.ratio = TRUE)
export2html(tc2, "Table1.kati.html")

names(chars)
names(df)


# more comparisons of early vs late for a possible future paper
chars <- df[df$GROUP1245==5,c(19:37)]
# describe.by(chars,group = df$group_early_no_break)
c1 <- compareGroups(chars,df$group_early_no_break[df$GROUP1245==5], bivar=TRUE)
t1 <- createTable(c1,hide = NA, hide.no = 0, digits = 1, show.n = TRUE)
export2html(t1, "early_vs_late.html")

chars <- df[,c(19:37)]
# describe.by(chars,group = df$group_early_no_break)
c2 <- compareGroups(chars,df$group_early_no_break, bivar=TRUE)
t2 <- createTable(c2,hide = NA, hide.no = 0, digits = 1, show.n = TRUE)
export2html(t2, "early_vs_late_comparison_groups.html")

# check anger: nothing with either measure, consider dropping from battery
summary(tam1 <- lm(ARSTOTAL ~ group_early_no_break + BASELINEAGE + GENDERTEXT, data = df))
anova(tam1)
plot(lsmeans(tam1, "group_early_no_break"))
cld(lsmeans(tam1, "group_early_no_break"))


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
