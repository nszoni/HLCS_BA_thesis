# NFO ---------------------------------------------------------------------

#THESIS Életpálya 2006-2008
#Author: Nguyen Nam Son
#Date: 05-10-2020

# Setup -------------------------------------------------------------------

#install packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(AER, haven, ggplot2, dplyr, 
               plyr, data.table, plm,
               stargazer, labelled, sjlabelled,
               summarytools, reshape2, Hmisc,
               corrplot, caret, foreign, lmtest,
               broom, knitr)

wd <- file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

df2006_start <- read_dta("eletpalya_a.dta")
df2007_start <- read_dta("eletpalya_b.dta")
df2008_start <- read_dta("eletpalya_c.dta")
df2009_start <- read_dta("eletpalya_d.dta")

df2006_start$year <- 2006
df2007_start$year <- 2007
df2008_start$year <- 2008
df2009_start$year <- 2009

# Selecting variables -----------------------------------------------------

df2006 <- df2006_start %>% select(c(year,
                                    azon,
                                    regio,
                                    af087xxx,
                                    af093xxx,
                                    af095xxx,
                                    af096xxx,
                                    af002a01,
                                    af002a02,
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
                                    af210xxx,
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
                'region',
                'full',
                'grade',
                'samesc',
                'nsamew',
                'sex',
                'byear',
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
                'nsib',
                'pcons',
                'welf',
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

df2007 <- df2007_start %>% select(c(year,
                                    azon,
                                    regio,
                                    bl01szul,
                                    bl01neme,
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
                                    b142a,
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
                                    bmisk,
                                    bfisk,
                                    btest,
                                    acsaljov,
                                    b64,
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
                   'region',
                   'byear',
                   'sex',
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
                   'schange1',
                   'schange2',
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
                   'nsib',
                   'pcons',
                   'welf',
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

df2008 <- df2008_start %>% select(c(year,
                                  azon,
                                  regio,
                                  cl01szul,
                                  cl01neme,
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
                                  cmisk,
                                  cfisk,
                                  ctest,
                                  acsaljov,
                                  c49,
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
                                  c144d,
                                  c120,
                                  c121))

names(df2008) <- c('year',
                   'ID',
                   'region',
                   'byear',
                   'sex',
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
                   'schange',
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
                   'nsib',
                   'pcons',
                   'welf',
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
                   'xp',
                   'sclass',
                   'sclassr')

df2009 <- df2009_start %>% select(c(year,
                                    azon,
                                    regio,
                                    dl01szul,
                                    dl01neme,
                                    d81,
                                    d85,
                                    d3,
                                    d4,
                                    d7,
                                    d5,
                                    d8,
                                    d9,
                                    d12,
                                    d10,
                                    d127,
                                    d104,
                                    d105,
                                    d64e,
                                    d64f,
                                    d64g,
                                    d64h,
                                    d33c,
                                    d42c,
                                    d20,
                                    d21,
                                    d15,
                                    dmisk,
                                    dfisk,
                                    dtest,
                                    acsaljov,
                                    d49,
                                    d79,
                                    d50,
                                    d50a,
                                    d103,
                                    d114a,
                                    d114c,
                                    d114d,
                                    d114g,
                                    d114h,
                                    d115e,
                                    d115t,
                                    d86,
                                    d116,
                                    d145d,
                                    d128,
                                    d129))

names(df2009) <- c('year',
                   'ID',
                   'region',
                   'byear',
                   'sex',
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
                   'schange',
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
                   'nsib',
                   'pcons',
                   'welf',
                   'minor',
                   'faminc',
                   'rfaminc',
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
                   'xp',
                   'sclass',
                   'sclassr')

# Detailed family structure -----------------------------------------------

attach(df2006)
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

# Family structure dummy
df2006$nintact <- as.factor(ifelse((fbio == 1) & (mbio == 1), 0, 1))

detach(df2006)

# Unifying differences before union -----------------------------------------

#number of school changes due to moving before 2006
df2006_temp <- df2006[, names(df2006) %in% c('rschange1', 'rschange2', 'rschange3', 'rschange4')]
df2006_temp$nschange <- apply(df2006_temp, 1, function(x) length(which(x == 2)))
df2006$nschange <- df2006_temp$nschange
df2006$schange <- 0

#school changes due to movement between observed years
df2007$schange <- ifelse(df2007$schange1 == 1 | df2007$schange2 == 1, 1, 0)
df2008$schange <- ifelse(df2008$schange == 1, 1, 0)
df2008$schange <- ifelse(df2008$schange == 1, 1, 0)

#repeats the previous grade
df2006$sgrade <- ifelse(df2006$grade == 1, 1, 0) 

#changing school due to expel
df2006_temp$nexp <- apply(df2006_temp, 1, function(x) length(which(x == 4)))
df2006$nexp <- df2006_temp$nexp
df2006$xp <- ifelse(df2006$nexp >= 1, 1, 0)

df2007$xp <- ifelse((df2007$xpmiss == 1 |
                  df2007$xpbehav == 1 |
                  df2007$xpgrds == 1 |
                  df2007$xpdrug == 1 |
                  df2007$xppinv == 1 |
                  df2007$xpmiss2 == 1 |
                  df2007$xpbehav2 == 1 |
                  df2007$xpgrds2 == 1 |
                  df2007$xpdrug2 == 1 |
                  df2007$xppinv2 == 1 |
                  df2007$xpbehav3 == 1 |
                  df2007$xpdrug3 == 1 |
                  df2007$xppinv3 == 1 |
                  df2007$xpother3 == 1), 1, 0)

#merging time invariant variables such as ethnicity and sex
df2006 <- merge(df2006, df2007[,c("ID", "minor")], by = "ID")
df2008 <- merge(df2008, df2007[,c("ID", "minor")], by = "ID")

df2006$male <- ifelse(df2006$sex == 1, 1, 0)
df2007$male <- ifelse(df2007$sex == 1, 1, 0)
df2008$male <- ifelse(df2008$sex == 1, 1, 0)
df2009$male <- ifelse(df2009$sex == 1, 1, 0)

#separation types
df2006$divordth <- as.factor(ifelse(((df2006$msep_reason == 4) | (df2006$fsep_reason == 4)), 1, #divorce
                                 ifelse(((df2006$msep_reason == 6) | (df2006$fsep_reason == 8)), 2, #death
                                        ifelse((df2006$mbio == 1) & (df2006$fbio == 1), NA, 3)))) #other and NA

#fixing grade scale
df2007$grade[df2007$grade == 0] <- NA
df2008$grade[df2008$grade == 0] <- NA
df2008$grade[df2008$grade == 0] <- NA
df2006$grade[df2006$grade == 9] <- NA
df2006$grade[df2006$grade == 1] <- 8
df2006$grade[df2006$grade == 2] <- 9
df2006$grade[df2006$grade == 0] <- NA

# Mean tables across family structures for starting year of 2006 ------------------------------------

mfinc <- aggregate(df2006[, c('pcons', 'mnsal', 'fnsal')],
                   list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mgrades <- aggregate(df2006[, c('fgrade', 'math_comp', 'read_comp', 'math', 'gram', 'liter', 'behav', 'dilig')],
                        list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mscores <- aggregate(df2006[, c('homesc', 'cognisc', 'emotisc')],
                     list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

nbrh <- aggregate(df2006[, c('wnbrh', 'cnbrh')],
                     list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

pinv <- aggregate(df2006[, c('txtbook', 'transc', 'xtraclass', 'sctrip')],
                   list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mfinc, "~/thesis_eletpalya/mfinc.txt", sep="\t")
write.table(mgrades, "~/thesis_eletpalya/mygrades.txt", sep="\t")
write.table(mscores, "~/thesis_eletpalya/mscores.txt", sep="\t")
write.table(nbrh, "~/thesis_eletpalya/nbrh.txt", sep="\t")
write.table(pinv, "~/thesis_eletpalya/pinv.txt", sep="\t")

# Descriptive summaries 2006 ----------------------------------------------------

df2006sum <- df2006[, !names(df2006) %in% c('ID', 'mbio', 'fbio', 'mstep', 'fstep', 'age_at_remm', 'age_at_remf')] %>% 
  remove_all_labels() %>% dfSummary(., plain.ascii = FALSE, style = "grid", 
                                    graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp") 

df2006sum$Missing <- NULL
view(df2006sum, file = "~/thesis_eletpalya/df2006sum.html")

# Cross tabulations 2006-------------------------------------------------------

fam_str1 <- freq(df2006$fam_str, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

nintact <- freq(df2006$nintact, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(fam_str1, "~/thesis_eletpalya/fam_str1.txt", sep="\t")
write.table(nintact, "~/thesis_eletpalya/nintact.txt", sep="\t")

gndr <- ctable(df2006$gender, df2006$nintact, prop = "c", chisq = TRUE)
write.table(gndr$proportions, "~/thesis_eletpalya/gndr.txt", sep="\t")

rep4 <- ctable(df2006$rep4, df2006$nintact, prop = "c", chisq = TRUE)
write.table(rep4$proportions, "~/thesis_eletpalya/rep4.txt", sep="\t")

rep58 <- ctable(df2006$rep58, df2006$nintact, prop = "c", chisq = TRUE)
write.table(rep58$proportions, "~/thesis_eletpalya/rep58.txt", sep="\t")

#residential mobility
resmob <- ctable(df2006$nschange, df2006$nintact, prop = "c", chisq = TRUE)
write.table(resmob$proportions, "~/thesis_eletpalya/resmobpp.txt", sep="\t")

#suspension and expel
exp <- ctable(df2006$nexp, df2006$nintact, prop = "c", chisq = TRUE)
write.table(exp$proportions, "~/thesis_eletpalya/exp.txt", sep="\t")

#leaving because of weak performance (~ drop out)
dropout <- ctable(df2006$ndpout, df2006$nintact, prop = "c", chisq = TRUE)
write.table(dropout$proportions, "~/thesis_eletpalya/dropout.txt", sep="\t")

#for parental involvement
ctable(df2006$peduc_asp, df2006$nintact, prop = "c", chisq = TRUE, OR = TRUE)

#the lower the better
mpscinv <- aggregate(df2006[, c("pscinv", "pmeet", "pttalk", "studyparent")],
                   list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mpscinv, "~/thesis_eletpalya/mpscinv.txt", sep="\t")

#additional parental investments not in home scale
pinv <- ctable(df2006$pinv, df2006$nintact, prop = "c", chisq = TRUE)
write.table(pinv$proportions, "~/thesis_eletpalya/pinv.txt", sep="\t")

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


# Merging waves -------------------------------------------------------

vars <- c("ID",
          "year",
          "region",
          "byear",
          "minor",
          "male",
          "full",
          "grade",
          "mbio",
          "mstep",
          "fbio",
          "fstep",
          "schange",
          "pmeet",
          "pttalk",
          "txtbook",
          "transc",
          "xtraclass",
          "sctrip",
          "mnsal",
          "fnsal",
          "wnbrh",
          "cnbrh",
          "internet",
          "mdegree",
          "fdegree",
          "nsib",
          "pcons",
          'welf',
          'math',
          'gram',
          'liter',
          'behav',
          'dilig',
          'intfgrade',
          'decfgrade',
          "xp")

df2006 <- remove_all_labels(df2006)
df2007 <- remove_all_labels(df2007)
df2008 <- remove_all_labels(df2008)
df2009 <- remove_all_labels(df2009)

dftotal <- rbind(df2006[, vars],
                 df2009[, vars])

# EDA ---------------------------------------------------------------------

#Merge grade decimals

concatFgrade <- function(dftotal, fgrade, intfgrade, decfgrade){
  dftotal$fgrade <- as.numeric(paste(dftotal$intfgrade, dftotal$decfgrade, sep = "."))
}

dftotal$fgrade <- concatFgrade(dftotal, "fgrade", "infgrade", "decfgrade")
dftotal <- dftotal[,!(names(dftotal) %in% c("intfgrade", "decfgrade"))]

#Replace missing values with NA

dftotal[dftotal == -6 | dftotal == 99 | dftotal == 88 | dftotal == 999 | dftotal == 9999] <- NA
dftotal$fgrade[dftotal$fgrade > 5 | dftotal$fgrade < 1] <- NA
dftotal[,!names(dftotal) %in% c("grade",
                                "age_at_sepf",
                                "age_at_sepm",
                                "age_at_remf",
                                "age_at_remm",
                                "mrel",
                                "methnic",
                                "fethnic",
                                "mdegree",
                                "fdegree")][dftotal[,!names(dftotal) %in% c("grade",
                                                                            "age_at_sepf",
                                                                            "age_at_sepm",
                                                                            "age_at_remf",
                                                                            "age_at_remm",
                                                                            "mrel",
                                                                            "methnic",
                                                                            "fethnic",
                                                                            "mdegree",
                                                                            "fdegree",
                                                                            "rfaminc")] == 9 ] <- NA

#create age column
dftotal$age <- dftotal$year - dftotal$byear

# Family structure dummy
dftotal$nintact <- as.factor(ifelse((dftotal$fbio == 1) & (dftotal$mbio == 1), 0, 1))

#measure of dropouts as studied or not
dftotal$study <- ifelse(dftotal$full == 5, 0, 1)

#create index for parental school involvement
dftotal$pscinv <- dftotal$pmeet + dftotal$pttalk

#parental investments
dftotal$pinv <- dftotal$txtbook + dftotal$transc + dftotal$xtraclass + dftotal$sctrip

#treating gypsy as minor (dummy)
dftotal$minor <- ifelse(dftotal$minor == 7, 1, 0)

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

cormatdf <- dftotal[c('fgrade',
                    'mnsal',
                    'fnsal',
                    'pinv',
                    'pcons',
                    'nsib')]

cor <- cormatdf %>% remove_all_labels() %>% cor(use = "complete.obs") %>% round(., 2)
res1 <- rcorr(as.matrix(cormatdf))
res2 <- flattenCorrMatrix(res1$r, res1$P) #table form

write.table(res2, file = "~/thesis_eletpalya/res2.txt", sep="\t")

# Insignificant correlation are crossed
corrplot(res1$r, type="upper", order="hclust", p.mat = res1$P, sig.level = 0.05, tl.col = "black", tl.srt = 45)
print(findCorrelation(cor, cutoff = 0.5)) #drop mnsal or pcons

# Models ------------------------------------------------------------------
#!!SUBSET FOR THOSE WHO WHERE IN INTACT FAMILIES IN 2006
#e.g. reported intact family in Y-1 = 2006, Y1 = 2009, Y0 = somewhere between the two

dftotal$y09 <- ifelse(dftotal$year == 2009, 1, 0)

#create and rebalance panel dframe (DiD does not need a panel data, only repeated cross section data)
# pdf <- pdata.frame(dftotal, index <- c("ID", "post2007")) #cross sectional and wave dimensions
# pdf <- make.pbalanced(pdf, balance.type = "shared.individuals")
# pdim(pdf)

#bivariate pooled OLS regression
 
ols_bi <- lm(fgrade ~ nintact, data = dftotal)
 
#multivariate pooled OLS regression for final grade
#sepage and divordeath is not representative (too much NAs)

ols_m1 <- lm(fgrade ~ nintact + mnsal + fnsal + gender + pscinv + pinv + schange, data = dftotal)

#given that it is a intact (non-intact) family
intact <- subset(dftotal, nintact == 0)
nintact <- subset(dftotal, nintact == 1)

#ols for disrupted families
ols_m2 <- lm(fgrade ~ mnsal + fnsal + gender + pscinv + pinv + schange, data = intact)
ols_m3 <- lm(fgrade ~ mnsal + fnsal + gender + pscinv + pinv + schange + divordth + age_at_sepf, data = nintact)

stargazer(ols_bi, ols_m1, ols_m2, ols_m3, type = 'text')

#diff in diff models

coeftest(lm(fgrade ~ nintact*y09, data = dftotal))

mod1 <- lm(fgrade ~ nintact*y09, data = dftotal) #pure effect
mod2 <- lm(fgrade ~ nintact*y09 + age + I(age^2/100) + male + minor + + full, data = dftotal) #adding time-invariant features
mod3 <- lm(fgrade ~ nintact*y09 + I(mnsal/1000) + factor(welf) + age + I(age^2/100) + male + minor + full, data = dftotal) #adding income related features
mod4 <- lm(fgrade ~ nintact*y09 + I(mnsal/1000) + factor(welf) + age + I(age^2/100) + male + minor + full + mdegree + nsib, data = dftotal) #other socioeconomic influences
mod5 <- lm(fgrade ~ nintact*y09 + I(mnsal/1000) + factor(welf) + age + I(age^2/100) + male + minor + full + mdegree + nsib + cnbrh + factor(region), data = dftotal) #controls for surroundings 

stargazer(mod1, mod2, mod3, mod4, mod5, type = 'text',
          title="DD of the parental separation on final grade",
          header=FALSE, digits=2)

# Accuracy tests ----------------------------------------------------------

#Hypothesis testing
#kable(anova(mod1, mod2), 
#      caption="Chow test for the 'intact' equation")

#Model selection criteria
r1 <- as.numeric(glance(mod1))
r2 <- as.numeric(glance(mod2))
tab <- data.frame(rbind(r1, r2))[,c(1,2,8,9)]
row.names(tab) <- c("nintact","nintact + controls")
kable(tab, 
      caption="Model comparison, 'nintact' ", digits=4, 
      col.names=c("Rsq","AdjRsq","AIC","BIC"))

#Ramsey test of higher-order polynomials (H0: higher order polynomials are needed)
resettest(mod2, power=2:3, type="fitted")

#VIF (variance inflation factor) test for multicollinearity
tab <- tidy(vif(mod5)[, c(1)])
kable(tab, 
      caption="Variance inflation factors for the 'fgrade' regression model",
      col.names=c("regressor", "VIF"))
#age and age^2 are highly correlated as expected but we can ignore that since one variable is the
#linear transformation of the other, therefore the econometric problem is not present.

#Heteroskedasticity of error terms w/ Breusch-Pagan test
kable(tidy(bptest(mod2)), 
      caption="Breusch-Pagan heteroskedasticity test")
#we can reject the homoskedasticity

# Robustness checks -------------------------------------------------------

# Lags and Leads: If D causes Y then current and lagged values should have an effect on Y,
# but future values of D should not.
# 
# Placebo permutation test: Randomly assign the intervention(s) to create the sampling distribution of the null hypothesis.
# 
# Use different comparison groups. Different groups should have the same affect.
# 
# Use an outcome variable that you know is not affected by the intervention. If DiD estimates not zero, then there is some other difference between groups.

#Common trend assumption or SUTVA
#plot of counterfactual

b1 <- coef(mod5)[[1]]
b2 <- coef(mod5)[["nintact1"]]
b3 <- coef(mod5)[["y08"]]
delta <- coef(mod5)[["nintact1:y08"]]
C <- b1+b2+b3+delta
E <- b1+b3
B <- b1+b2
A <- b1
D <- E+(B-A)

plot(1, type="n", main = "Estimated impact of parental separation", xlab="Period", ylab="Final Grade", xaxt="n",
     xlim=c(-0.01, 1.01), ylim=c(-4.04, -3.87))
segments(x0=0, y0=A, x1=1, y1=E, lty=1, col=2, lwd = 5)#control
segments(x0=0, y0=B, x1=1, y1=C, lty=3, col=3, lwd = 5)#treated
segments(x0=0, y0=B, x1=1, y1=D, lty=4, col=4, lwd = 5) #counterfactual
legend("center", legend=c("control", "treated", 
                          "counterfactual"), lty=c(1,3,4), col=c(2,3,4), cex = 0.75)
axis(side=1, at=c(0,1), labels=NULL)


#Serial correlation test with Breusch-Godfrey/Wooldridge test
pbgtest(mod5, type = "F")

###################
#   END OF CODE   #
###################
