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
                        af087xxx,
                        af093xxx,
                        af095xxx,
                        af096xxx,
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
                        af219axx,
                        af219bxx,
                        af219cxx,
                        af219dxx,
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
                'full',
                'grade',
                'samesc',
                'nsamew',
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
                'txtbook',
                'transc',
                'xtraclass',
                'sctrip',
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
                'pcons',
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

df2007 <- df2007 %>% select(c(year,
                              azon,
                              b83,
                              b88,
                              b3,
                              b5,
                              b9,
                              b6,
                              b11,
                              b13,
                              b17,
                              b14,
                              b140a,
                              b117,
                              b118,
                              b77e,
                              b77f,
                              b77g,
                              b77h,
                              b44c,
                              b58c,
                              b38,
                              b52,
                              b23,
                              b24,
                              b30,
                              amisk,
                              afisk,
                              atest,
                              acsaljov,
                              b222,
                              b22s,
                              b65,
                              b65a,
                              b116,
                              b196,
                              b129i,
                              b129k,
                              b129l,
                              b129o,
                              b129p,
                              b130eg,
                              b130tiz,
                              b89,
                              b133,
                              b136,
                              b143,
                              b144a,
                              b140d,
                              b140e,
                              b140f,
                              b140g,
                              b140h,
                              b142e,
                              b142f,
                              b142g,
                              b142h,
                              b142i,
                              b163d,
                              b163e,
                              b163f,
                              b163h,
                              b163i,
                              b163o,
                              b163q,
                              b163t))

names(df2007) <- c('year',
                   'ID',
                   'full',
                   'grade',
                   'mbio',
                   'samebio',
                   'mstep',
                   'msep_reason',
                   'fbio',
                   'samebio',
                   'fstep',
                   'fsep_reason',
                   'rschange',
                   'pmeet',
                   'pttalk',
                   'txtbook',
                   'transc',
                   'xtraclass',
                   'sctrip',
                   'mnsal',
                   'fnsal',
                   'methnic',
                   'fethnic',
                   'wnbrh',
                   'cnbrh',
                   'internet',
                   'mdegree',
                   'fdegree',
                   'nsibling',
                   'pcons',
                   'minor',
                   'movedly',
                   'faminc',
                   'rfaminc',
                   'fullly',
                   'subscr',
                   'math',
                   'gram',
                   'liter',
                   'behav',
                   'dilig',
                   'intfgrade',
                   'decfgrade',
                   'sgrade',
                   'nadv',
                   'schange',
                   'sclass',
                   'sclassr',
                   'xpmiss',
                   'xpbehav',
                   'xpgrds',
                   'xpdrug',
                   'xppinv',
                   'xpmiss2',
                   'xpbehav2',
                   'xpgrds2',
                   'xpdrug2',
                   'xppinv2',
                   'xpdrug3',
                   'xppinv3',
                   'xpother3',
                   'xpbehav3',
                   'dpoutmot',
                   'dpoutdiv',
                   'dpoutwrk',
                   'dpoutind')

df2008 <- df2008 %>% select(c(year,
                              azon,
                              c73,
                              c77,
                              c3,
                              c4,
                              c7,
                              c5,
                              c8,
                              c9,
                              c12,
                              c10,
                              c119,
                              c96,
                              c97,
                              c67e,
                              c67f,
                              c67g,
                              c67h,
                              c33c,
                              c42c,
                              c20,
                              c21,
                              c15,
                              amisk,
                              afisk,
                              atest,
                              acsaljov,
                              c50,
                              c50a,
                              c161,
                              c95,
                              c104a,
                              c104c,
                              c104d,
                              c104g,
                              c104h,
                              c105e,
                              c105t,
                              c78,
                              c108,
                              c115,
                              c144d,
                              c120,
                              c121))

names(df2008) <- c('year',
                   'ID',
                   'full',
                   'grade',
                   'mbio',
                   'samembio',
                   'mstep',
                   'msep_reason',
                   'fbio',
                   'samefbio',
                   'fstep',
                   'fsep_reason',
                   'rschange',
                   'pmeet',
                   'pttalk',
                   'txtbook',
                   'transc',
                   'xtraclass',
                   'sctrip',
                   'mnsal',
                   'fnsal',
                   'wnbrh',
                   'cnbrh',
                   'internet',
                   'mdegree',
                   'fdegree',
                   'nsibling',
                   'pcons',
                   'faminc',
                   'rfaminc',
                   'subscr',
                   'fullly',
                   'math',
                   'gram',
                   'liter',
                   'behav',
                   'dilig',
                   'intfgrade',
                   'decfgrade',
                   'sgrade',
                   'nadv',
                   'schange',
                   'xp',
                   'sclass',
                   'sclassr')

#rbind(df2006, df2007, df2008)

# Creating panel df -------------------------------------------------------

pdf <- pdata.frame(df2006, index <- c("ID", "year")) #cross sectional and wave dimensions
pdim(pdf)

# Removing labels and assigning NAs ---------------------------------------

concatFgrade <- function(x){
  x$fgrade <<- as.numeric(paste(x$intfgrade, x$decfgrade, sep = "."))
}

concatFgrade(df2006)
concatFgrade(df2007)
concatFgrade(df2008)
df2006 <- df2006[,!(names(df2006) %in% c("intfgrade", "decfgrade"))]
df2007 <- df2007[,!(names(df2007) %in% c("intfgrade", "decfgrade"))]
df2008 <- df2008[,!(names(df2008) %in% c("intfgrade", "decfgrade"))]

df2006[df2006 == -6 | df2006 == 99 | df2006 == 999 | df2006 == 9999] <- NA
df2006$fgrade[df2006$fgrade > 5] <- NA

df2006[,!names(df2006) %in% c("age_at_sepf",
                        "age_at_sepm",
                        "age_at_remf",
                        "age_at_remm",
                        "mrel",
                        "methnic",
                        "fethnic",
                        "fsep_reason",
                        "mdegree",
                        "fdegree")][df2006[,!names(df2006) %in% c("age_at_sepf",
                                                               "age_at_sepm",
                                                               "age_at_remf",
                                                               "age_at_remm",
                                                               "mrel",
                                                               "methnic",
                                                               "fethnic",
                                                               "fsep_reason",
                                                               "mdegree",
                                                               "fdegree")] == 9 ] <- NA

attach(df2006)

#Creating categorical variable for family structure

df2006$fam_str <- as.factor(ifelse((fbio == 1) & (mbio == 1), 'tparent', #two-parent family
                         ifelse((fbio == 1) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)), 'singlef', #single-father family
                         ifelse((mbio == 1) & (fbio %in% c(2, NA)) & (fstep %in% c(2, NA)), 'singlem', #single-mother family
                         ifelse((fbio == 1) & (mstep == 1), 'stepm', #step-mother family
                         ifelse((mbio == 1) & (fstep == 1) & (mrel == 1), 'rem_stepf', # married step-father family
                         ifelse((mbio == 1) & (fstep == 1) & (mrel %in% c(3,5,7)), 'cohab_stepf', #cohabiting step-father family
                         ifelse((mstep == 1) & (fstep == 1), 'tfoster', #foster-two-parent family
                         ifelse((mstep == 1) & (fbio %in% c(2, NA)) & (fstep %in% c(2, NA)), 'smfoster', #foster-single-mother family
                         ifelse((fstep == 1) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)), 'sffoster', #foster-single-father family
                         ifelse((fbio %in% c(2, NA)) & (mbio %in% c(2, NA)) & (mstep %in% c(2, NA)) & (fstep %in% c(2, NA)), 'alone', NA))))))))))) #does not live with anyone

df2006$intact <- as.factor(ifelse((fbio == 1) & (mbio == 1), 1, 0))

#number of school changes due to moving
df2006_temp <- df2006[, names(df2006) %in% c('rschange1', 'rschange2', 'rschange3', 'rschange4')]
df2006_temp$nschange <- apply(df2006_temp, 1, function(x) length(which(x == 2)))
df2006$nschange <- df2006_temp$nschange

#measure of expels
df2006_temp$nexp <- apply(df2006_temp, 1, function(x) length(which(x == 4)))
df2006$nexp <- df2006_temp$nexp

#measure of dropouts
df2006_temp$ndpout <- apply(df2006_temp, 1, function(x) length(which(x == 5)))
df2006$ndpout <- df2006_temp$ndpout

#create index for parental involvement
df2006$pscinv <- df2006$pmeet + df2006$pttalk + df2006$studyparent

# parental investments
df2006_temp2 <- df2006_temp <- df2006[, names(df2006) %in% c('xtraclass', 'workdesk', 'comp', 'internet')]
df2006_temp2$npinv <- apply(df2006_temp2, 1, function(x) length(which(x == 2)))
df2006$npinv <- df2006_temp2$npinv

#minority dummy
df2006$minor <- as.factor(ifelse((df2006$fethnic == 7) | (df2006$methnic == 7), 1, 0))

#separation types
df2006$divordth <- as.factor(ifelse((df2006$intact == 0) & ((df2006$msep_reason == 4) | (df2006$fsep_reason == 4)), 1, #divorce
                                 ifelse((df2006$intact == 0) & ((df2006$msep_reason == 6) | (df2006$fsep_reason == 8)), 2, #death
                                        ifelse((df2006$intact == 1), NA, 3)))) #other and NA

# Mean tables across family structures ------------------------------------

mfinc <- aggregate(df2006[, c('fam_income', 'mnsal', 'fnsal')],
                   list(df2006$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mgrades <- aggregate(df2006[, c('fgrade', 'math_comp', 'read_comp', 'math', 'gram', 'liter', 'behav', 'dilig')],
                        list(df2006$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mscores <- aggregate(df2006[, c('homesc', 'cognisc', 'emotisc')],
                     list(df2006$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

nbrh <- aggregate(df2006[, c('wnbrh', 'cnbrh')],
                     list(df2006$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mfinc, "~/thesis_eletpalya/mfinc.txt", sep="\t")
write.table(mgrades, "~/thesis_eletpalya/mygrades.txt", sep="\t")
write.table(mscores, "~/thesis_eletpalya/mscores.txt", sep="\t")
write.table(nbrh, "~/thesis_eletpalya/nbrh.txt", sep="\t")

# Descriptive summaries ----------------------------------------------------

df2006sum <- df2006[, !names(df2006) %in% c('ID', 'mbio', 'fbio', 'mstep', 'fstep', 'age_at_remm', 'age_at_remf')] %>% 
  remove_all_labels() %>% dfSummary(., plain.ascii = FALSE, style = "grid", 
                                    graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp") 

df2006sum$Missing <- NULL
view(df2006sum, file = "~/thesis_eletpalya/df2006sum.html")

# Cross tabulations -------------------------------------------------------

fam_str1 <- freq(df2006$fam_str, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

intact <- freq(df2006$intact, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(fam_str1, "~/thesis_eletpalya/fam_str1.txt", sep="\t")
write.table(intact, "~/thesis_eletpalya/intact.txt", sep="\t")

gndr <- ctable(df2006$gender, df2006$intact, prop = "c", chisq = TRUE)
write.table(gndr$proportions, "~/thesis_eletpalya/gndr.txt", sep="\t")

rep4 <- ctable(df2006$rep4, df2006$intact, prop = "c", chisq = TRUE)
write.table(rep4$proportions, "~/thesis_eletpalya/rep4.txt", sep="\t")

rep58 <- ctable(df2006$rep58, df2006$intact, prop = "c", chisq = TRUE)
write.table(rep58$proportions, "~/thesis_eletpalya/rep58.txt", sep="\t")

#residential mobility
resmob <- ctable(df2006$nschange, df2006$intact, prop = "c", chisq = TRUE)
write.table(resmob$proportions, "~/thesis_eletpalya/resmobpp.txt", sep="\t")

#suspension and expel
exp <- ctable(df2006$nexp, df2006$intact, prop = "c", chisq = TRUE)
write.table(exp$proportions, "~/thesis_eletpalya/exp.txt", sep="\t")

#leaving because of weak performance (~ drop out)
dropout <- ctable(df2006$ndpout, df2006$intact, prop = "c", chisq = TRUE)
write.table(dropout$proportions, "~/thesis_eletpalya/dropout.txt", sep="\t")

#for parental involvement
ctable(df2006$peduc_asp, df2006$intact, prop = "c", chisq = TRUE, OR = TRUE)

#the lower the better
mpscinv <- aggregate(df2006[, c("pscinv", "pmeet", "pttalk", "studyparent")],
                   list(df2006$intact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mpscinv, "~/thesis_eletpalya/mpscinv.txt", sep="\t")

#additional parental investments not in home scale
npinv <- ctable(df2006$npinv, df2006$intact, prop = "c", chisq = TRUE)
write.table(npinv$proportions, "~/thesis_eletpalya/npinv.txt", sep="\t")

#minority backround (gypsy)
minor <- freq(df2006$minor, report.nas = FALSE, 
              cumul = FALSE, headings = FALSE)
write.table(minor, "~/thesis_eletpalya/minor.txt", sep="\t")

#reason of separation
divordth <- freq(df2006$divordth, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(divordth, "~/thesis_eletpalya/divordth.txt", sep="\t")

#separation age means
sepage <- summary(df2006[, c("age_at_sepm", "age_at_sepf")])
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

cormatdf <- df2006[, c('gender',
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
 
ols_bi <- lm(fgrade ~ intact, data = df2006)
 
#multivariate pooled OLS regression for final grade
#sepage and divordeath is not representative (too much NAs)

ols_m1 <- lm(fgrade ~ intact + mnsal + fnsal + gender + homesc + pscinv + npinv + nschange, data = df2006)

#given that it is a intact (non-intact) family
nintact <- subset(df2006, intact == 0)
intact <- subset(df2006, intact == 1)

#ols for disrupted families
ols_m2 <- lm(fgrade ~ mnsal + fnsal + gender + homesc + pscinv + npinv + nschange, data = intact)
ols_m3 <- lm(fgrade ~ mnsal + fnsal + gender + homesc + pscinv + npinv + nschange + divordth + age_at_sepf, data = nintact)

stargazer(ols_bi, ols_m1, ols_m2, ols_m3, type = 'text')

#diff in diff models

###################
#   END OF CODE   #
###################
