# NFO ---------------------------------------------------------------------

#THESIS Életpálya 2006-2008
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

df2006 <- read_dta("eletpalya_a.dta")
df2007 <- read_dta("eletpalya_b.dta")
df2008 <- read_dta("eletpalya_c.dta")

df2006$year <- 2006
df2007$year <- 2007
df2008$year <- 2008

# Selecting variables -----------------------------------------------------

df2006 <- df2006 %>% select(c(year,
                        azon,
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

names(df2006) <- c('year',
                'ID',
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

rbind(df2006, df2007, df2008)

# Creating panel df -------------------------------------------------------

pdf <- pdata.frame(dftotal, index <- c("ID", "year")) #cross sectional and wave dimensions
pdim(pdf)

# Removing labels and assigning NAs ---------------------------------------

concatFgrade <- function(dftotal){
  dftotal$fgrade <<- as.numeric(paste(dftotal$intfgrade, dftotal$decfgrade, sep = "."))
}

concatFgrade(dftotal)
dftotal <- dftotal[,!(names(dftotal) %in% c("intfgrade", "decfgrade"))]

dftotal[dftotal == -6 | dftotal == 99 | dftotal == 999 | dftotal == 9999] <- NA
dftotal$fgrade[dftotal$fgrade > 5] <- NA

dftotal[,!names(dftotal) %in% c("age_at_sepf",
                        "age_at_sepm",
                        "age_at_remf",
                        "age_at_remm",
                        "mrel",
                        "methnic",
                        "fethnic",
                        "fsep_reason",
                        "mdegree",
                        "fdegree")][dftotal[,!names(dftotal) %in% c("age_at_sepf",
                                                               "age_at_sepm",
                                                               "age_at_remf",
                                                               "age_at_remm",
                                                               "mrel",
                                                               "methnic",
                                                               "fethnic",
                                                               "fsep_reason",
                                                               "mdegree",
                                                               "fdegree")] == 9 ] <- NA

attach(dftotal)

#Creating categorical variable for family structure

dftotal$fam_str <- as.factor(ifelse((fbio == 1) & (mbio == 1), 'tparent', #two-parent family
                         ifelse((fbio == 1) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)), 'singlef', #single-father family
                         ifelse((mbio == 1) & (fbio %in% c(2, NA)) & (fstep %in% c(2, NA)), 'singlem', #single-mother family
                         ifelse((fbio == 1) & (mstep == 1), 'stepm', #step-mother family
                         ifelse((mbio == 1) & (fstep == 1) & (mrel == 1), 'rem_stepf', # married step-father family
                         ifelse((mbio == 1) & (fstep == 1) & (mrel %in% c(3,5,7)), 'cohab_stepf', #cohabiting step-father family
                         ifelse((mstep == 1) & (fstep == 1), 'tfoster', #foster-two-parent family
                         ifelse((mstep == 1) & (fbio %in% c(2, NA)) & (fstep %in% c(2, NA)), 'smfoster', #foster-single-mother family
                         ifelse((fstep == 1) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)), 'sffoster', #foster-single-father family
                         ifelse((fbio %in% c(2, NA)) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)) & (fstep %in% c(2, NA)), 'alone', NA))))))))))) #does not live with anyone

dftotal$intact <- as.factor(ifelse((fbio == 1) & (mbio == 1), 1, 0))

#number of school changes due to moving
dftotal_temp <- dftotal[, names(dftotal) %in% c('rschange1', 'rschange2', 'rschange3', 'rschange4')]
dftotal_temp$nschange <- apply(dftotal_temp, 1, function(x) length(which(x == 2)))
dftotal$nschange <- dftotal_temp$nschange

#measure of expels
dftotal_temp$nexp <- apply(dftotal_temp, 1, function(x) length(which(x == 4)))
dftotal$nexp <- dftotal_temp$nexp

#measure of dropouts
dftotal_temp$ndpout <- apply(dftotal_temp, 1, function(x) length(which(x == 5)))
dftotal$ndpout <- dftotal_temp$ndpout

#create index for parental involvement
dftotal$pscinv <- dftotal$pmeet + dftotal$pttalk + dftotal$studyparent

# parental investments
dftotal_temp2 <- dftotal_temp <- dftotal[, names(dftotal) %in% c('xtraclass', 'workdesk', 'comp', 'internet')]
dftotal_temp2$npinv <- apply(dftotal_temp2, 1, function(x) length(which(x == 2)))
dftotal$npinv <- dftotal_temp2$npinv

#minority dummy
dftotal$minor <- as.factor(ifelse((dftotal$fethnic == 7) | (dftotal$methnic == 7), 1, 0))

#separation types
dftotal$divordth <- as.factor(ifelse((dftotal$intact == 0) & ((dftotal$msep_reason == 4) | (dftotal$fsep_reason == 4)), 1, #divorce
                                 ifelse((dftotal$intact == 0) & ((dftotal$msep_reason == 6) | (dftotal$fsep_reason == 8)), 2, #death
                                        ifelse((dftotal$intact == 1), NA, 3)))) #other and NA

# Mean tables across family structures ------------------------------------

mfinc <- aggregate(dftotal[, c('fam_income', 'mnsal', 'fnsal')],
                   list(dftotal$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mgrades <- aggregate(dftotal[, c('fgrade', 'math_comp', 'read_comp', 'math', 'gram', 'liter', 'behav', 'dilig')],
                        list(dftotal$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mscores <- aggregate(dftotal[, c('homesc', 'cognisc', 'emotisc')],
                     list(dftotal$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

nbrh <- aggregate(dftotal[, c('wnbrh', 'cnbrh')],
                     list(dftotal$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mfinc, "~/thesis_eletpalya/mfinc.txt", sep="\t")
write.table(mgrades, "~/thesis_eletpalya/mygrades.txt", sep="\t")
write.table(mscores, "~/thesis_eletpalya/mscores.txt", sep="\t")
write.table(nbrh, "~/thesis_eletpalya/nbrh.txt", sep="\t")

# Descriptive summaries ----------------------------------------------------

dftotalsum <- dftotal[, !names(dftotal) %in% c('ID', 'mbio', 'fbio', 'mstep', 'fstep', 'age_at_remm', 'age_at_remf')] %>% 
  remove_all_labels() %>% dfSummary(., plain.ascii = FALSE, style = "grid", 
                                    graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp") 

dftotalsum$Missing <- NULL
view(dftotalsum, file = "~/thesis_eletpalya/dftotalsum.html")

# Cross tabulations -------------------------------------------------------

fam_str1 <- freq(dftotal$fam_str, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

intact <- freq(dftotal$intact, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(fam_str1, "~/thesis_eletpalya/fam_str1.txt", sep="\t")
write.table(intact, "~/thesis_eletpalya/intact.txt", sep="\t")

gndr <- ctable(dftotal$gender, dftotal$intact, prop = "c", chisq = TRUE)
write.table(gndr$proportions, "~/thesis_eletpalya/gndr.txt", sep="\t")

rep4 <- ctable(dftotal$rep4, dftotal$intact, prop = "c", chisq = TRUE)
write.table(rep4$proportions, "~/thesis_eletpalya/rep4.txt", sep="\t")

rep58 <- ctable(dftotal$rep58, dftotal$intact, prop = "c", chisq = TRUE)
write.table(rep58$proportions, "~/thesis_eletpalya/rep58.txt", sep="\t")

#residential mobility
resmob <- ctable(dftotal$nschange, dftotal$intact, prop = "c", chisq = TRUE)
write.table(resmob$proportions, "~/thesis_eletpalya/resmobpp.txt", sep="\t")

#suspension and expel
exp <- ctable(dftotal$nexp, dftotal$intact, prop = "c", chisq = TRUE)
write.table(exp$proportions, "~/thesis_eletpalya/exp.txt", sep="\t")

#leaving because of weak performance (~ drop out)
dropout <- ctable(dftotal$ndpout, dftotal$intact, prop = "c", chisq = TRUE)
write.table(dropout$proportions, "~/thesis_eletpalya/dropout.txt", sep="\t")

#for parental involvement
ctable(dftotal$peduc_asp, dftotal$intact, prop = "c", chisq = TRUE, OR = TRUE)

#the lower the better
mpscinv <- aggregate(dftotal[, c("pscinv", "pmeet", "pttalk", "studyparent")],
                   list(dftotal$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mpscinv, "~/thesis_eletpalya/mpscinv.txt", sep="\t")

#additional parental investments not in home scale
npinv <- ctable(dftotal$npinv, dftotal$intact, prop = "c", chisq = TRUE)
write.table(npinv$proportions, "~/thesis_eletpalya/npinv.txt", sep="\t")

#minority backround (gypsy)
minor <- freq(dftotal$minor, report.nas = FALSE, 
              cumul = FALSE, headings = FALSE)
write.table(minor, "~/thesis_eletpalya/minor.txt", sep="\t")

#reason of separation
divordth <- freq(dftotal$divordth, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(divordth, "~/thesis_eletpalya/divordth.txt", sep="\t")

#separation age means
sepage <- summary(dftotal[, c("age_at_sepm", "age_at_sepf")])
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

cormatdf <- dftotal[, c('gender',
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

# Models ------------------------------------------------------------------

#bivariate pooled OLS regression
 
ols_bi <- lm(fgrade ~ intact, data = dftotal)
 
#multivariate pooled OLS regression for final grade
#sepage and divordeath is not representative (too much NAs)

ols_m1 <- lm(fgrade ~ intact + mnsal + fnsal + gender + homesc + pscinv + npinv + nschange, data = dftotal)

#given that it is a intact (non-intact) family
nintact <- subset(dftotal, intact == 0)
intact <- subset(dftotal, intact == 1)

#ols for disrupted families
ols_m2 <- lm(fgrade ~ mnsal + fnsal + gender + homesc + pscinv + npinv + nschange, data = intact)
ols_m3 <- lm(fgrade ~ mnsal + fnsal + gender + homesc + pscinv + npinv + nschange + divordth + age_at_sepf, data = nintact)

stargazer(ols_bi, ols_m1, ols_m2, ols_m3, type = 'text')

#diff in diff models

###################
#   END OF CODE   #
###################
