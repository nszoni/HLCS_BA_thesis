# NFO ---------------------------------------------------------------------

#Életpálya 2006, 1. hullám
#Author: Nguyen Nam Son
#Date: 05-10-2020

# Setup -------------------------------------------------------------------

library(AER)
library(haven)
library(ggplot2)
library(dplyr)
library(plyr)
library(data.table)
library(plm)
library(stargazer)
library(labelled)
library(sjlabelled)
library(summarytools)
library(reshape2)
library(Hmisc)
library(corrplot)
library(caret)

wd <- file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

df <- read_dta("eletpalya_a.dta")

# Selecting variables -----------------------------------------------------

df2 <- df %>% select(c(azon,
                        af002a01,
                        af006x01,
                        af006x02,
                        af007xxx,
                        af008xxx,
                        af009xxx,
                        af010x01,
                        af010x02,
                        af011xxx,
                        af012xxx,
                        af013xxx,
                        af070a15, 
                        af070b15,
                        af070c15,
                        af070d15,
                        af080xxx,
                        af081xxx,
                        af084xxx,
                        af133axx,
                        ad030exx,
                        af140xxx, 
                        af145dxx,
                        af167xxx, 
                        af190xxx,
                        af156xxx,
                        af179xxx,
                        af200xxx,
                        af201xxx,
                        ad025axx, 
                        ad025cxx, 
                        af209xxx, 
                        ahomesc, 
                        acognisc, 
                        aemotisc, 
                        amisk,
                        afisk, 
                        atest, 
                        acsaljov,
                        m_zpsc,
                        o_zpsc, 
                        ad003axx,
                        ad003bxx,
                        ad003cxx,
                        ad003dxx,
                        ad003exx,
                        ad004axx,
                        ad004bxx,
                        af071xxx,
                        af072xxx))

names(df2) <- c('ID',
                'gender',
                'mbio',
                'mstep',
                'age_at_sepm',
                'msep_reason',
                'age_at_remf',
                'fbio',
                'fstep',
                'age_at_sepf',
                'fsep_reason',
                'age_at_remm',
                'rschange1',
                'rschange2',
                'rschange3',
                'rschange4',
                'pmeet',
                'pttalk',
                'peduc_asp',
                'schooltalk',
                'studyparent',
                'xtraclass',
                'mrel',
                'mnsal',
                'fnsal',
                'methnic',
                'fethnic',
                'wnbrh',
                'cnbrh',
                'workdesk',
                'comp',
                'internet',
                'homesc',
                'cognisc',
                'emotisc',
                'mdegree',
                'fdegree',
                'nsibling',
                'fam_income',
                'math_comp',
                'read_comp',
                'math',
                'gram',
                'liter',
                'behav',
                'dilig',
                'intfgrade',
                'decfgrade',
                'rep4',
                'rep58')

# Removing labels and assigning NAs ---------------------------------------

concatFgrade <- function(df2){
  df2$fgrade <<- as.numeric(paste(df2$intfgrade, df2$decfgrade, sep = "."))
}

concatFgrade(df2)
df2 <- df2[,!(names(df2) %in% c("intfgrade", "decfgrade"))]

df2[df2 == -6 | df2 == 99 | df2 == 999 | df2 == 9999] <- NA
df2$fgrade[df2$fgrade > 5] <- NA

df2[,!names(df2) %in% c("age_at_sepf",
                        "age_at_sepm",
                        "age_at_remf",
                        "age_at_remm",
                        "mrel",
                        "methnic",
                        "fethnic",
                        "fsep_reason",
                        "mdegree",
                        "fdegree")][df2[,!names(df2) %in% c("age_at_sepf",
                                                               "age_at_sepm",
                                                               "age_at_remf",
                                                               "age_at_remm",
                                                               "mrel",
                                                               "methnic",
                                                               "fethnic",
                                                               "fsep_reason",
                                                               "mdegree",
                                                               "fdegree")] == 9 ] <- NA

attach(df2)

#Creating categorical variable for family structure

df2$fam_str <- as.factor(ifelse((fbio == 1) & (mbio == 1), 'tparent', #two-parent family
                         ifelse((fbio == 1) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)), 'singlef', #single-father family
                         ifelse((mbio == 1) & (fbio %in% c(2, NA)) & (fstep %in% c(2, NA)), 'singlem', #single-mother family
                         ifelse((fbio == 1) & (mstep == 1), 'stepm', #step-mother family
                         ifelse((mbio == 1) & (fstep == 1) & (mrel == 1), 'rem_stepf', # married step-father family
                         ifelse((mbio == 1) & (fstep == 1) & (mrel %in% c(3,5,7)), 'cohab_stepf', #cohabiting step-father family
                         ifelse((mstep == 1) & (fstep == 1), 'tfoster', #foster-two-parent family
                         ifelse((mstep == 1) & (fbio %in% c(2, NA)) & (fstep %in% c(2, NA)), 'smfoster', #foster-single-mother family
                         ifelse((fstep == 1) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)), 'sffoster', #foster-single-father family
                         ifelse((fbio %in% c(2, NA)) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)) & (fstep %in% c(2, NA)), 'alone', NA))))))))))) #does not live with anyone

df2$intact <- as.factor(ifelse((fbio == 1) & (mbio == 1), 1, 0))

#number of school changes due to moving
df2_temp <- df2[, names(df2) %in% c('rschange1', 'rschange2', 'rschange3', 'rschange4')]
df2_temp$nschange <- apply(df2_temp, 1, function(x) length(which(x == 2)))
df2$nschange <- df2_temp$nschange

#measure of expels
df2_temp$nexp <- apply(df2_temp, 1, function(x) length(which(x == 4)))
df2$nexp <- df2_temp$nexp

#measure of dropouts
df2_temp$ndpout <- apply(df2_temp, 1, function(x) length(which(x == 5)))
df2$ndpout <- df2_temp$ndpout

#create index for parental involvement
df2$pscinv <- df2$pmeet + df2$pttalk + df2$studyparent

# parental investments
df2_temp2 <- df2_temp <- df2[, names(df2) %in% c('xtraclass', 'workdesk', 'comp', 'internet')]
df2_temp2$npinv <- apply(df2_temp2, 1, function(x) length(which(x == 2)))
df2$npinv <- df2_temp2$npinv

#minority dummy
df2$minor <- as.factor(ifelse((df2$fethnic == 7) | (df2$methnic == 7), 1, 0))

#separation types
df2$divordth <- as.factor(ifelse((df2$intact == 0) & ((df2$msep_reason == 4) | (df2$fsep_reason == 4)), 1, #divorce
                                 ifelse((df2$intact == 0) & ((df2$msep_reason == 6) | (df2$fsep_reason == 8)), 2, #death
                                        ifelse((df2$intact == 1), NA, 3)))) #other and NA

# Mean tables across family structures ------------------------------------

mfinc <- aggregate(df2[, c('fam_income', 'mnsal', 'fnsal')],
                   list(df2$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mgrades <- aggregate(df2[, c('fgrade', 'math_comp', 'read_comp', 'math', 'gram', 'liter', 'behav', 'dilig')],
                        list(df2$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mscores <- aggregate(df2[, c('homesc', 'cognisc', 'emotisc')],
                     list(df2$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

nbrh <- aggregate(df2[, c('wnbrh', 'cnbrh')],
                     list(df2$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mfinc, "~/thesis_eletpalya/mfinc.txt", sep="\t")
write.table(mgrades, "~/thesis_eletpalya/mygrades.txt", sep="\t")
write.table(mscores, "~/thesis_eletpalya/mscores.txt", sep="\t")
write.table(nbrh, "~/thesis_eletpalya/nbrh.txt", sep="\t")

# Descriptive summaries ----------------------------------------------------

df2sum <- df2[, !names(df2) %in% c('ID', 'mbio', 'fbio', 'mstep', 'fstep', 'age_at_remm', 'age_at_remf')] %>% 
  remove_all_labels() %>% dfSummary(., plain.ascii = FALSE, style = "grid", 
                                    graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp") 

df2sum$Missing <- NULL
view(df2sum, file = "~/thesis_eletpalya/df2sum.html")

# Cross tabulations -------------------------------------------------------

fam_str1 <- freq(df2$fam_str, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

intact <- freq(df2$intact, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(fam_str1, "~/thesis_eletpalya/fam_str1.txt", sep="\t")
write.table(intact, "~/thesis_eletpalya/intact.txt", sep="\t")

gndr <- ctable(df2$gender, df2$intact, prop = "c", chisq = TRUE)
write.table(gndr$proportions, "~/thesis_eletpalya/gndr.txt", sep="\t")

rep4 <- ctable(df2$rep4, df2$intact, prop = "c", chisq = TRUE)
write.table(rep4$proportions, "~/thesis_eletpalya/rep4.txt", sep="\t")

rep58 <- ctable(df2$rep58, df2$intact, prop = "c", chisq = TRUE)
write.table(rep58$proportions, "~/thesis_eletpalya/rep58.txt", sep="\t")

#residential mobility
resmob <- ctable(df2$nschange, df2$intact, prop = "c", chisq = TRUE)
write.table(resmob$proportions, "~/thesis_eletpalya/resmobpp.txt", sep="\t")

#suspension and expel
exp <- ctable(df2$nexp, df2$intact, prop = "c", chisq = TRUE)
write.table(exp$proportions, "~/thesis_eletpalya/exp.txt", sep="\t")

#leaving because of weak performance (~ drop out)
dropout <- ctable(df2$ndpout, df2$intact, prop = "c", chisq = TRUE)
write.table(dropout$proportions, "~/thesis_eletpalya/dropout.txt", sep="\t")

#for parental involvement
ctable(df2$peduc_asp, df2$intact, prop = "c", chisq = TRUE, OR = TRUE)

#the lower the better
mpscinv <- aggregate(df2[, c("pscinv", "pmeet", "pttalk", "studyparent")],
                   list(df2$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mpscinv, "~/thesis_eletpalya/mpscinv.txt", sep="\t")

#additional parental investments not in home scale
npinv <- ctable(df2$npinv, df2$intact, prop = "c", chisq = TRUE)
write.table(npinv$proportions, "~/thesis_eletpalya/npinv.txt", sep="\t")

#minority backround (gypsy)
minor <- freq(df2$minor, report.nas = FALSE, 
              cumul = FALSE, headings = FALSE)
write.table(minor, "~/thesis_eletpalya/minor.txt", sep="\t")

#reason of separation
divordth <- freq(df2$divordth, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(divordth, "~/thesis_eletpalya/divordth.txt", sep="\t")

#separation age means
sepage <- summary(df2[, c("age_at_sepm", "age_at_sepf")])
write.table(sepage, "~/thesis_eletpalya/sepage.txt", sep="\t")

# Feature selection----------------------------------------------------

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cormatdf <- df2[, c('gender',
                    'mnsal',
                    'fnsal',
                    'homesc',
                    'pscinv',
                    'npinv',
                    'nschange',
                    'age_at_sepf')]

cor <- cormatdf %>% remove_all_labels() %>% cor(use = "complete.obs") %>% round(., 2)
res1 <- rcorr(as.matrix(cormatdf))
res2 <- flattenCorrMatrix(res1$r, res1$P)

write.table(res2, file = "~/thesis_eletpalya/res2.txt", sep="\t")

# Insignificant correlation are crossed
corrplot(res1$r, type="upper", order="hclust", p.mat = res1$P, sig.level = 0.05, tl.col = "black", tl.srt = 45)
print(findCorrelation(cor, cutoff = 0.5))

# Creating panel df -------------------------------------------------------

# pdf <- pdata.frame(df2, index <- c("azon", "hullam")) #cross sectional and wave dimensions
# pdim(pdf)

# Models ------------------------------------------------------------------

#bivariate pooled OLS regression
 
ols_bi <- lm(fgrade ~ intact, data = df2)
 
#multivariate pooled OLS regression for final grade
#sepage and divordeath is not representative (too much NAs)

ols_m1 <- lm(fgrade ~ intact + mnsal + fnsal + gender + homesc + pscinv + npinv + nschange, data = df2)

#given that it is a intact (non-intact) family
nintact <- subset(df2, intact == 0)
intact <- subset(df2, intact == 1)

#ols for disrupted families
ols_m2 <- lm(fgrade ~ mnsal + fnsal + gender + homesc + pscinv + npinv + nschange, data = intact)
ols_m3 <- lm(fgrade ~ mnsal + fnsal + gender + homesc + pscinv + npinv + nschange + divordth + age_at_sepf, data = nintact)

stargazer(ols_bi, ols_m1, ols_m2, ols_m3, type = 'text')

#diff in diff models


