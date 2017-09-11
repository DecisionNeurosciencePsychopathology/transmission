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

df <- read_csv("~/Box Sync/skinner/projects_analyses/Project Transmission/FAMHX_DEMOG_MERGED.7.17.17.csv")
View(df)

library(VIM)
df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


hist(df$MOMMH)
describe(df$ENVRNMTL)
unique(df$ENVRNMTL)

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
# get group characteristics
chars <- df[,c(2,5,7,8,9,10)]
describe.by(chars,group = chars$GROUP1245)


m1 <- glm(firstdegSB ~ GROUP12467, family = binomial(link = "logit"), data = df)
summary(m1)
anova(m1, test = "LR")
ls1 <- lsmeans(m1,"GROUP1245")
plot(ls1, horiz = F)

m2 <- glm(firstdegSB ~ GROUP1245*BASELINEAGE, family = binomial(link = "logit"), data = df)
summary(m2)
anova(m2, test = "LR")

# does m2 fit better than m1?
anova(m1,m2, test = "LR")

m3 <- glm(firstdegSB ~ GROUP12467 + BASELINEAGE, family = binomial(link = "logit"), data = df)
summary(m3)
anova(m3, test = "LR")
anova(m2,m3, test = "LR")

m4 <- glm(firstdegSB ~ GROUP12467 + BASELINEAGE + GENDERTEXT + race, family = binomial(link = "logit"), data = df)
summary(m4)
anova(m4, test = "LR")
anova(m3,m4, test = "LR")
ls4 <- lsmeans(m4,"GROUP12467")
plot(ls4, horiz = F)


# does first degree Hx vary by attempt characteristics
am1 <- glm(firstdegSB ~ AGEATFIRSTATTEMPT, family = binomial(link = "logit"), data = df[df$GROUP1245==5,])
summary(am1)

am2 <- glm(firstdegSB ~ AGEATFIRSTATTEMPT + TOTALATTEMPTS, family = binomial(link = "logit"), data = df[df$GROUP1245==5,])
summary(am2)


cm1 <-  glm(firstdegSC ~ GROUP12467 + BASELINEAGE , family = binomial(link = "logit"), data = df)
summary(cm1)
anova(cm1, test = "LR")
lscm1 <- lsmeans(cm1,"GROUP12467")
plot(cm1, horiz = F)
sc_tbl <- table(df$GROUP12467,df$firstdegSC)
sc_tbl_chsq <- chisq.test(sc_tbl)