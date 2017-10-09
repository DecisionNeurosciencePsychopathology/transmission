<<<<<<< HEAD
#install.packages(c("car","readr", "lme4", "ggplot2", "tidyr", "psych", "gdata", "xtable", "Hmisc", "nnet", 
#"reshape2", "corrplot", "lsmeans", "readxl", "MASS", "stargazer", "compareGroups"))
  
=======
#install.packages(c("car","readr", "lme4", "ggplot2", "tidyr", "psych", "gdata", "xtable", "Hmisc", "nnet",
#"reshape2", "corrplot", "lsmeans", "readxl", "MASS"))

>>>>>>> origin/master
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

<<<<<<< HEAD
<<<<<<< HEAD
#df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED.csv",
#"\t", escape_double = FALSE, trim_ws = TRUE)
#setwd("~/Box Sync/skinner/projects_analyses/Project Transmission")
=======
df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED.csv",
"\t", escape_double = FALSE, trim_ws = TRUE)
setwd("~/Box Sync/skinner/projects_analyses/Project Transmission")
>>>>>>> origin/master
# View(df)

  # at home
# setwd("C:/Users/Laura/Box Sync/skinner/projects_analyses/Project Transmission")
#
# df <- read_delim("C:/Users/Laura/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED.csv",
#                  "\t", escape_double = FALSE, trim_ws = TRUE)

# at work
#df <- read_delim("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_COUNTS_MERGED.csv",
#"\t", escape_double = FALSE, trim_ws = TRUE)

View(df)

# library(VIM)
# df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


# hist(df$MOMMH)
# describe(df$ENVRNMTL)
# unique(df$ENVRNMTL)

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

# dichotomize exposure
d$exp <- d$events>0
summary(m9 <- glm(exp ~  group_early + sev*blood + BASELINEAGE*sev +  EDUCATION  + race  + (1:ID), family = binomial, data = d))
car::Anova(m9, type = "III")
ls9 <- lsmeans(m9, "group_early")
plot(ls9, horiz = F)
multcomp::cld(ls9)

summary(m10 <- glm(exp ~  group_early + sev*blood + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m10, type = "III")
ls10 <- lsmeans(m10, pairwise ~ group_early)
# plot(ls10, horiz = F)


summary(m11 <- glm(exp ~  group_early + sev*rel + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m11, type = "III")
ls11 <- lsmeans(m11, "group_early")
plot(ls11)

# specifically test group*relation to rule out familial clustering: NS, does not improve fit
summary(m11a <- glm(exp ~  group_early*sev*rel + BASELINEAGE*sev +  EDUCATION  + race + (1:ID), family = binomial, data = d))
car::Anova(m11a, type = "III")
anova(m11,m11a, test = "Rao")

# plot NS interaction for qualitative look:  still a problem with 2nd degree
ls11a <- lsmeans(m11a, "group_early", by = "rel")
plot(ls11a)


# test substance and anxiety -- not significant
summary(m12 <- glm(exp ~  group_early + sev*rel + BASELINEAGE*sev +  EDUCATION  + race + SubstanceLifetime + AnxietyLifetime + (1:ID), family = binomial, data = d))
car::Anova(m12, type = "III")
# anova(m11,m12, test = "Rao")

#plot figure
CLD <- multcomp::cld(ls11,
                     alpha=0.05,
                     Letters=letters,
                     adjust="tukey")
CLD$.group=gsub(" ", "", CLD$.group)
# CLD$g=c("Non-psychiatric controls", "Non-suicidal depressed", "Suicide ideators", "Early-onset attempters", "Late-onset attempters")


pdf(file = "Exposure by group PRETTY.pdf", width = 8, height = 6)
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


# get group characteristics and make Table 1
chars <- df[,c(2,7,8,9,10, 18,40,42, 14,11, 17)]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,df$group_early_no_break)
createTable(c,hide = c(GENDERTEXT = "MALE", list(RACETEXT = c("WHITE", "ASIAN PACIFIC"))), hide.no = 0, digits = 1)
export2html(createTable(c), "Table1.html")


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
