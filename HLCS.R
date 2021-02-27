# NFO ---------------------------------------------------------------------

#THESIS Hungarian Life Course Survey 2006-2009
#Author: Nguyen Nam Son
#Date: 05-10-2020

# Setup -------------------------------------------------------------------

#install packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(AER, usdm, haven, ggplot2, dplyr, sandwich,
               plyr, data.table, plm, MASS, ggdag,
               stargazer, labelled, sjlabelled,
               summarytools, reshape2, Hmisc,
               corrplot, caret, foreign, lmtest,
               broom, knitr, did, DRDID, devtools, ggpubr, xtable)

# devtools::install_github("bcallaway11/did")

wd <- file.path("~", "HLCS_BA_thesis", "kesz")
setwd(wd)

set.seed(1234)

df2006_start <- read_dta("eletpalya_a.dta")
df2007_start <- read_dta("eletpalya_b.dta")
df2008_start <- read_dta("eletpalya_c.dta")
df2009_start <- read_dta("eletpalya_d.dta")
compscores08_start <- read_dta("okm06-okm08_eletpalya.dta")

df2006_start$year <- 2006
df2007_start$year <- 2007
df2008_start$year <- 2008
df2009_start$year <- 2009

# Selecting variables -----------------------------------------------------

df2006 <- df2006_start %>% dplyr::select(c(year,
                                    azon,
                                    regio,
                                    af087xxx,
                                    af123xxx,
                                    af088xxx,
                                    af092axx,
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
                                    af157xxx,
                                    af179xxx,
                                    af180xxx,
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
                                    af072xxx,
                                    af023xxx,
                                    ad028xxx,
                                    af205xxx,
                                    aekor03,
                                    aekor46,
                                    aekor714,
                                    aekor151,
                                    iskid))

names(df2006) <- c('year',
                'ID',
                'region',
                'full1',
                'full2',
                'maint',
                'sctype',
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
                'methnic1',
                'methnic2',
                'fethnic1',
                'fethnic2',
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
                'math_comp06',
                'read_comp06',
                'math',
                'gram',
                'liter',
                'behav',
                'dilig',
                'intfgrade',
                'decfgrade',
                'rep4',
                'rep58',
                'brthw',
                'hstat',
                'hsqm',
                'ekor03',
                'ekor46',
                'ekor714',
                'ekor151',
                'scID')

df2007 <- df2007_start %>% dplyr::select(c(year,
                                    azon,
                                    regio,
                                    bl01szul,
                                    bl01neme,
                                    b83,
                                    b85,
                                    b87,
                                    b88,
                                    b3,
                                    b4,
                                    b5,
                                    b9,
                                    b6,
                                    b11,
                                    b12,
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
                                    b39,
                                    b52,
                                    b53,
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
                                    b163t,
                                    b28,
                                    bekor03,
                                    bekor46,
                                    bekor714,
                                    bekor151,
                                    iskid))

names(df2007) <- c('year',
                   'ID',
                   'region',
                   'byear',
                   'sex',
                   'full',
                   'maint',
                   'sctype',
                   'grade',
                   'mbio',
                   'mdeath',
                   'samembio',
                   'mstep',
                   'msep_reason',
                   'fbio',
                   'fdeath',
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
                   'methnic1',
                   'methnic2',
                   'fethnic1',
                   'fethnic2',
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
                   'dpoutind',
                   'hsqm',
                   'ekor03',
                   'ekor46',
                   'ekor714',
                   'ekor151',
                   'scID')

df2008 <- df2008_start %>% dplyr::select(c(year,
                                  azon,
                                  regio,
                                  cl01szul,
                                  cl01neme,
                                  c73,
                                  c75,
                                  c77,
                                  c80,
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
                                  c121,
                                  m_zpsc,
                                  o_zpsc,
                                  c198,
                                  c24,
                                  cekor03,
                                  cekor46,
                                  cekor714,
                                  cekor151,
                                  iskid))

names(df2008) <- c('year',
                   'ID',
                   'region',
                   'byear',
                   'sex',
                   'full',
                   'maint',
                   'grade',
                   'sctype',
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
                   'sclassr',
                   'math_comp06',
                   'read_comp06',
                   'hstat',
                   'hsqm',
                   'ekor03',
                   'ekor46',
                   'ekor714',
                   'ekor151',
                   'scID')

df2009 <- df2009_start %>% dplyr::select(c(year,
                                    azon,
                                    regio,
                                    dl01szul,
                                    dl01neme,
                                    d81,
                                    d83,
                                    d85,
                                    d88,
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
                                    d78,
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
                                    d129,
                                    d188,
                                    d24,
                                    dekor03,
                                    dekor46,
                                    dekor714,
                                    dekor151,
                                    iskid))

names(df2009) <- c('year',
                   'ID',
                   'region',
                   'byear',
                   'sex',
                   'full',
                   'maint',
                   'grade',
                   'sctype',
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
                   'ethnic1',
                   'ethnic2',
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
                   'sclassr',
                   'hstat',
                   'hsqm',
                   'ekor03',
                   'ekor46',
                   'ekor714',
                   'ekor151',
                   'scID')

compscores08 <- compscores08_start %>% dplyr::select(c(azon_06,
                                                m_zpsc,
                                                o_zpsc,))

names(compscores08) <- c("ID", "math_comp08", "read_comp08")

df2006 <- remove_all_labels(df2006)
df2007 <- remove_all_labels(df2007)
df2008 <- remove_all_labels(df2008)
df2009 <- remove_all_labels(df2009)
compscores08 <- remove_all_labels(compscores08)

# Detailed family structure for 2006 -----------------------------------------------

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
#fixing department type for 2006
df2006$full <- ifelse(df2006$full1 == 1, df2006$full1, df2006$full2)

#number of school changes due to moving before 2006 ~ mobility rate
df2006_temp <- df2006[, names(df2006) %in% c('rschange1', 'rschange2', 'rschange3', 'rschange4')]
df2006_temp$nschange <- apply(df2006_temp, 1, function(x) length(which(x == 2)))
df2006$nschange <- df2006_temp$nschange

#school changes due to movement between observed years
df2007$schange <- ifelse(df2007$schange1 == 1 | df2007$schange2 == 1, 1, 0)
df2008$schange <- ifelse(df2008$schange == 1, 1, 0)
df2009$schange <- ifelse(df2009$schange == 1, 1, 0)

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
df2006$roma <- ifelse((df2006$mbio == 1 & (df2006$methnic1 == 7 | df2006$methnic2 == 7)) | (df2006$fbio == 1 & (df2006$fethnic1 == 7 | df2006$fethnic2 == 7)), 1, 0)
df2007$roma <- ifelse((df2007$mbio == 1 & (df2007$methnic1 == 7 | df2007$methnic2 == 7)) | (df2007$fbio == 1 & (df2007$fethnic1 == 7 | df2007$fethnic2 == 7)), 1, 0)
df2008 <- merge(df2008, df2007[,c("ID", "roma")], by = "ID")
df2009$roma <- ifelse(df2009$ethnic1 == 7 | df2009$ethnic2 == 7, 1, 0)

df2006$male <- ifelse(df2006$sex == 1, 1, 0)
df2007$male <- ifelse(df2007$sex == 1, 1, 0)
df2008$male <- ifelse(df2008$sex == 1, 1, 0)
df2009$male <- ifelse(df2009$sex == 1, 1, 0)

#separation types
df2006$divordth <- as.factor(ifelse(((df2006$msep_reason == 4) | (df2006$fsep_reason == 4)), 1, #divorce
                                 ifelse(((df2006$msep_reason == 6) | (df2006$fsep_reason == 8)), 2, #death
                                        ifelse((df2006$mbio == 1) & (df2006$fbio == 1), NA, 3)))) #other and NA

df2006$death <- as.factor(ifelse((df2006$msep_reason == 6) | (df2006$fsep_reason == 8), 1, 0))
df2007$death <- as.factor(ifelse((df2007$mdeath == 1) | (df2007$fdeath == 1), 1, 0))
df2008$death <- as.factor(ifelse((df2008$msep_reason == 5) | (df2008$fsep_reason == 5), 1, 0))
df2009$death <- as.factor(ifelse((df2009$msep_reason == 5) | (df2009$fsep_reason == 5), 1, 0))

#whether changed school due to moving last year
df2006$schange <- 0 #in pre treatment, moving does not ensue
df2007$schange <- ifelse(df2007$schange1 == 1 | df2007$schange1 == 1, 1, 0)
df2008$schange <- ifelse(df2008$schange == 1, 1, 0)
df2009$schange <- ifelse(df2009$schange == 1, 1, 0)

#fixing grade scale
df2007$grade[df2007$grade == 0] <- NA
df2008$grade[df2008$grade == 0] <- NA
df2009$grade[df2009$grade == 0] <- NA
df2006$grade[df2006$grade == 9] <- NA
df2006$grade[df2006$grade == 1] <- 8
df2006$grade[df2006$grade == 2] <- 9
df2006$grade[df2006$grade == 3] <- NA
df2006$grade[df2006$grade == 4] <- NA
df2006$sctype[df2006$sctype == 8] <- NA

#health features and homesc
df2007 <- merge(df2007, df2006[,c("ID", "brthw", "hstat", "homesc", "nschange")], by = "ID")
df2008 <- merge(df2008, df2006[,c("ID", "brthw", "homesc", "nschange")], by = "ID")
df2009 <- merge(df2009, df2006[,c("ID", "brthw", "homesc", "nschange")], by = "ID")

# Mean tables across family structures for starting year of 2006 ------------------------------------

df20062[df2006 == -6 | df2006 == 99 | df2006 == 88 | df2006 == 999 | df2006 == 9999] <- NA
df2006[,!names(df2006) %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome','scID')][df2006[,!names(df2006) %in%
                                                                                                                       c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome','scID')] == 9 ] <- NA

mfinc <- aggregate(df2006[, c('pcons', 'mnsal', 'welf')],
                   list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mgrades <- aggregate(df2006[, c('fgrade', 'math_comp', 'read_comp', 'math', 'gram', 'liter', 'behav', 'dilig')],
                        list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mscores <- aggregate(df2006[, c('homesc', 'cognisc', 'emotisc')],
                     list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

nbrh <- aggregate(df2006[, c('wnbrh', 'cnbrh')],
                     list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

pinv <- aggregate(df2006[, c('txtbook', 'transc', 'xtraclass', 'sctrip', 'pinv')],
                   list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mfinc, "~/HLCS_BA_thesis/tables/mfinc.txt", sep="\t")
write.table(mgrades, "~/HLCS_BA_thesis/tables/mygrades.txt", sep="\t")
write.table(mscores, "~/HLCS_BA_thesis/tables/mscores.txt", sep="\t")
write.table(nbrh, "~/HLCS_BA_thesis/tables/nbrh.txt", sep="\t")
write.table(pinv, "~/HLCS_BA_thesis/tables/pinv.txt", sep="\t")

# Descriptive summaries 2006 ----------------------------------------------------

df2006sum <- df2006[, !names(df2006) %in% c('ID', 'mbio', 'fbio', 'mstep', 'fstep', 'age_at_remm', 'age_at_remf')] %>% 
  remove_all_labels() %>% dfSummary(., plain.ascii = FALSE, style = "grid", 
                                    graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp") 

df2006sum$Missing <- NULL
view(df2006sum, file = "~/HLCS_BA_thesis/tables/df2006sum.html")

# Cross tabulations 2006-------------------------------------------------------

fam_str1 <- freq(df2006$fam_str, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

nintact <- freq(df2006$nintact, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(fam_str1, "~/HLCS_BA_thesis/tables/fam_str1.txt", sep="\t")
write.table(nintact, "~/HLCS_BA_thesis/tables/nintact.txt", sep="\t")

gndr <- ctable(df2006$gender, df2006$nintact, prop = "c", chisq = TRUE)
write.table(gndr$proportions, "~/HLCS_BA_thesis/tables/gndr.txt", sep="\t")

rep4 <- ctable(df2006$rep4, df2006$nintact, prop = "c", chisq = TRUE)
write.table(rep4$proportions, "~/HLCS_BA_thesis/tables/rep4.txt", sep="\t")

rep58 <- ctable(df2006$rep58, df2006$nintact, prop = "c", chisq = TRUE)
write.table(rep58$proportions, "~/HLCS_BA_thesis/tables/rep58.txt", sep="\t")

#residential mobility
resmob <- ctable(df2006$nschange, df2006$nintact, prop = "c", chisq = TRUE)
write.table(resmob$proportions, "~/HLCS_BA_thesis/tables/resmobpp.txt", sep="\t")

#suspension and expel
exp <- ctable(df2006$nexp, df2006$nintact, prop = "c", chisq = TRUE)
write.table(exp$proportions, "~/HLCS_BA_thesis/tables/exp.txt", sep="\t")

#leaving because of weak performance (~ drop out)
dropout <- ctable(df2006$ndpout, df2006$nintact, prop = "c", chisq = TRUE)
write.table(dropout$proportions, "~/HLCS_BA_thesis/tables/dropout.txt", sep="\t")

#for parental involvement
ctable(df2006$peduc_asp, df2006$nintact, prop = "c", chisq = TRUE, OR = TRUE)

#the lower the better
PSI <- aggregate(df2006[, c("PSI", "pmeet", "pttalk", "studyparent")],
                   list(df2006$nintact), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(PSI, "~/HLCS_BA_thesis/tables/PSI.txt", sep="\t")

#additional parental investments not in home scale
pinv <- ctable(df2006$pinv, df2006$nintact, prop = "c", chisq = TRUE)
write.table(pinv$proportions, "~/HLCS_BA_thesis/tables/pinv.txt", sep="\t")

#roma
roma <- ctable(df2006$roma, df2006$nintact, prop = "c", chisq = TRUE)
write.table(roma$proportions, "~/HLCS_BA_thesis/tables/roma.txt", sep="\t")

#reason of separation
divordth <- freq(df2006$divordth, report.nas = FALSE, 
                 cumul = FALSE, headings = FALSE)

write.table(divordth, "~/HLCS_BA_thesis/tables/divordth.txt", sep="\t")

#separation age means
sepage <- summary(df2006[, c("age_at_sepm", "age_at_sepf")])
write.table(sepage, "~/HLCS_BA_thesis/tables/sepage.txt", sep="\t")

# Feature selection----------------------------------------------------

df2006[df2006 == -6 | df2006 == 99 | df2006 == 88 | df2006 == 999 | df2006 == 9999] <- NA
df2006[,!names(df2006) %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome')][df2006[,!names(df2006) %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome')] == 9 ] <- NA
df2006$mnsal[is.na(df2006$mnsal)] <- 0
df2006$fnsal[is.na(df2006$fnsal)] <- 0
df2006$lhincome <- log(df2006$mnsal + df2006$fnsal)
df2006$lhincome[df2006$lhincome == -Inf] <- NA
df2006$nmin <- df2006$ekor03 + df2006$ekor46 + df2006$ekor714 + df2006$ekor151
df2006$fgrade <- as.numeric(paste(df2006$intfgrade, df2006$decfgrade, sep = "."))
df2006$fgrade[df2006$fgrade > 5 | df2006$fgrade < 1] <- NA
df2006$pinv <- df2006$txtbook + df2006$transc + df2006$xtraclass + df2006$sctrip

# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }

cormatdf <- df2006[c('lhincome',
                     'homesc',
                     'pinv',
                     'brthw',
                     'mdegree',
                     'welf',
                     'nmin')]

# cor <- cormatdf %>% remove_all_labels() %>% cor(use = "complete.obs") %>% round(., 2)
# res1 <- rcorr(as.matrix(cormatdf))
# res2 <- flattenCorrMatrix(res1$r, res1$P) #table form
# 
# write.table(res2, file = "~/HLCS_BA_thesis/tables/res2.txt", sep="\t")
# 
# # Insignificant correlation are crossed
# corrplot(res1$r, type="upper", order="hclust", p.mat = res1$P, sig.level = 0.01, tl.col = "black", tl.srt = 45)
# print(findCorrelation(cor, cutoff = 0.5)) #nothing highly correlates

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

corstars(cormatdf, "pearson", "upper", "latex")

vif <- usdm::vif(remove_all_labels(cormatdf))
row.names(vif) <- c("Household Income", "HOME", "Parental Investments", "Birthweight", "Maternal Degree", "Welfare level", "#Minors")

xtd <- xtable(vif["VIF"],
              digits = 2,
              auto = TRUE,
              caption = "Variance Inflation Factors",
              type = "latex")
print(xtd, include.rownames=TRUE)

# VAM (Value Added Model) for Final grades--------------------------------------------------------------------
vars <- c("ID",
          "year",
          "region",
          "byear",
          "roma",
          "male",
          "full",
          "sctype",
          "grade",
          "maint",
          "mbio",
          "fbio",
          "nschange",
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
          'death',
          'brthw',
          'hstat',
          'homesc',
          'hsqm',
          'ekor03',
          'ekor46',
          'ekor714',
          'ekor151',
          'scID')

dftotal <- rbind(df2006[, vars],
                 df2009[, vars])

dftotal$fgrade <- as.numeric(paste(dftotal$intfgrade, dftotal$decfgrade, sep = "."))
dftotal <- dftotal[,!(names(dftotal) %in% c("intfgrade", "decfgrade"))]
dftotal$fgrade[dftotal$fgrade > 5 | dftotal$fgrade < 1] <- NA

dftotal[dftotal == -6 | dftotal == 99 | dftotal == 88 | dftotal == 999 | dftotal == 9999] <- NA
dftotal[,!names(dftotal) %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome','scID')][dftotal[,!names(dftotal) %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome','scID')] == 9 ] <- NA

#low birthweight
dftotal$lbrthw <- ifelse(dftotal$brthw < 2500, 1, 0) 

#create age column
dftotal$age <- dftotal$year - dftotal$byear

# Family structure dummy
dftotal$nintact <- as.factor(ifelse((dftotal$fbio == 1) & (dftotal$mbio == 1), 0, 1))

#measure of dropouts as studied or not
dftotal$study <- ifelse(dftotal$full == 5, 0, 1)

#create index for parental school involvement
dftotal$PSI <- (dftotal$pmeet + dftotal$pttalk)*(-1)

#log household income
dftotal$mnsal[is.na(dftotal$mnsal)] <- 0
dftotal$fnsal[is.na(dftotal$fnsal)] <- 0
dftotal$lhincome <- log(dftotal$mnsal + dftotal$fnsal)
dftotal$lhincome[dftotal$lhincome == -Inf] <- NA

#apartment sqm / minors in household
dftotal$nmin <- dftotal$ekor03 + dftotal$ekor46 + dftotal$ekor714 + dftotal$ekor151
dftotal$sqmpm <- dftotal$hsqm/dftotal$nmin
dftotal$sqmpm[dftotal$sqmpm == Inf] <- NA
#too much missing values in house parameters (2008-2009)

#parental school investments
dftotal$lpinv <- log(dftotal$txtbook + dftotal$transc + dftotal$xtraclass + dftotal$sctrip)
dftotal$lpinv[dftotal$lpinv == -Inf] <- NA

dftotal$nintact <- as.numeric(levels(dftotal$nintact))[dftotal$nintact]

#school types
dftotal$sctype <- ifelse(dftotal$sctype %in% c(2,3), 1, ifelse(dftotal$sctype %in% c(4,5,6), 2, 3))

dftotal <- dftotal %>%
  group_by(ID) %>%
  mutate(sep = as.factor(sum(nintact, na.rm = TRUE)))

df2006$fgrade <- as.numeric(paste(df2006$intfgrade, df2006$decfgrade, sep = "."))
df2006$fgrade[df2006$fgrade > 5 | df2006$fgrade < 1] <- NA
df2006$math[df2006$math > 5 | df2006$math < 1] <- NA

dftotal <- inner_join(dftotal, df2006[, c("ID", "fgrade", "math")], by = "ID")

dftotal <- dftotal[dftotal$year == 2009,]

names(dftotal)[names(dftotal) == 'fgrade.x'] <- 'fgrade09'
names(dftotal)[names(dftotal) == 'fgrade.y'] <- 'fgrade06'

vam1 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0"), data = dftotal)
vam2 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0") + male + roma + age + factor(hstat) + lbrthw, data = dftotal)
vam3 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0") + male + roma + age + factor(hstat) + lbrthw + factor(mdegree) + lhincome + factor(welf), data = dftotal)
vam4 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0") + male + roma + age + factor(hstat) + lbrthw + factor(mdegree) + lhincome + factor(welf) + homesc + nmin, data = dftotal)
vam5 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0") + male + roma + age + factor(hstat) + lbrthw + factor(mdegree) + lhincome + factor(welf) + homesc + nmin + factor(maint) + factor(region) + factor(sctype), data = dftotal)
vam6 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0") + male + roma + age + factor(hstat) + lbrthw + factor(mdegree) + lhincome + factor(welf) + homesc + nmin + factor(maint) + factor(region) + factor(sctype)+ PSI + lpinv, data = dftotal)
vam7 <- lm(fgrade09 ~ fgrade06 + relevel(sep, ref = "0") + male + roma + age + factor(hstat) + lbrthw + factor(mdegree) + lhincome + factor(welf) + homesc + nmin + factor(maint) + factor(region) + factor(sctype)+ PSI + lpinv + nschange, data = dftotal)

stargazer(vam1,vam2,vam3,vam4,vam5,vam6,vam7,
          title="VAM of Parental Separation Effect on GPA",
          header=FALSE, 
          digits=3,
          font.size = "footnotesize",
          align = TRUE,
          column.sep.width = "-15pt",
          no.space = TRUE,
          omit = c("fgrade06", "male", "roma", "age", "mdegree", 'lhincome', "homesc", "welf", "nmin", "maint",'sctype','hstat', 'lbrthw', 'region', "nschange", "PSI", "pinv", 'Constant'),
          dep.var.labels = c("2009 GPA"),
          covariate.labels = c("Separated between 2006 and 2008",
                               "Separated before 2006"),
          add.lines = list(c("Control variables:"),
                           c("2006 GPA", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Student Characteristics", "", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("SES", "", "", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Home Environment", "", "", "", "Yes", "Yes", "Yes", "Yes"),
                           c("School FE", "", "", "", "", "Yes", "Yes", "Yes"),
                           c("PSI", "", "", "", "", "", "Yes", "Yes"),
                           c("Residential Mobility", "", "", "", "", "", "", "Yes")),
          omit.stat = c("rsq", "f", "ser"),
          type = "latex")

bptest(vam7)

coeftest(vam7, vcov = hccm) #standard robust error needed?

#Model selection criteria
r1 <- as.numeric(glance(vam1))
r2 <- as.numeric(glance(vam2))
r3 <- as.numeric(glance(vam3))
r4 <- as.numeric(glance(vam4))
r5 <- as.numeric(glance(vam5))
r6 <- as.numeric(glance(vam6))
r7 <- as.numeric(glance(vam7))
tab <- data.frame(rbind(r1, r2, r3, r4, r5, r6, r7))[,c(2,8,9)]
row.names(tab) <- c("No Covariates (1)","+ Student Characteristics (2)", "+ SES (3)", "+ Home Environment (4)", "+ School Characteristics (5)", "+ PSI (6)", "+ Residential Mobility (7)")
colnames(tab) <- c("AdjRsq", "AIC", "BIC")

xtd <- xtable(tab,
              digits = 2,
              auto = TRUE,
              caption = "Penalization Statistics",
              type = "latex")
print(xtd, include.rownames=TRUE)

# Staggered DID -----------------------------------------------------------

vars <- c("ID",
          "year",
          "region",
          "byear",
          "roma",
          "male",
          "full",
          "sctype",
          "grade",
          "maint",
          "mbio",
          "fbio",
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
          "death",
          'brthw',
          'hstat',
          'ekor03',
          'ekor46',
          'ekor714',
          'ekor151',
          'homesc',
          'nschange',
          'scID')

dftotal2 <- rbind(df2006[, vars],
                  df2007[, vars],
                  df2008[, vars],
                  df2009[, vars])

concatFgrade <- function(dftotal2, fgrade, intfgrade, decfgrade){
  dftotal2$fgrade <- as.numeric(paste(dftotal2$intfgrade, dftotal2$decfgrade, sep = "."))
}

dftotal2$fgrade <- concatFgrade(dftotal2, "fgrade", "infgrade", "decfgrade")
dftotal2 <- dftotal2[,!(names(dftotal2) %in% c("intfgrade", "decfgrade"))]

#Replace missing values with NA

dftotal2[dftotal2 == -6 | dftotal2 == 99 | dftotal2 == 88 | dftotal2 == 999 | dftotal2 == 9999] <- NA
dftotal2$fgrade[dftotal2$fgrade > 5 | dftotal2$fgrade < 1] <- NA
dftotal2$math[dftotal2$math > 5 | dftotal2$math < 1] <- NA
dftotal2[,!names(dftotal2) %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome','scID')][dftotal2[,!names(dftotal2) 
                        %in% c("mdegree", "grade",'ekor03','ekor46','ekor714','ekor151','lhincome','scID')] == 9 ] <- NA

#low birthweight
dftotal2$lbrthw <- ifelse(dftotal2$brthw < 2500, 1, 0) 

#create age column
dftotal2$age <- dftotal2$year - dftotal2$byear

#measure of dropouts as studied or not
dftotal2$study <- ifelse(dftotal2$full == 5, 0, 1)

#create index for parental school involvement
dftotal2$PSI <- (dftotal2$pmeet + dftotal2$pttalk)*(-1)

#parental investments
dftotal2$lpinv <- log(dftotal2$txtbook + dftotal2$transc + dftotal2$xtraclass + dftotal2$sctrip)
dftotal2$lpinv[dftotal2$lpinv == -Inf] <- NA

#log household income
dftotal2$mnsal[is.na(dftotal2$mnsal)] <- 0
dftotal2$fnsal[is.na(dftotal2$fnsal)] <- 0
dftotal2$lhincome <- log(dftotal2$mnsal + dftotal2$fnsal)
dftotal2$lhincome[dftotal2$lhincome == -Inf] <- NA

#minors in household
dftotal2$nmin <- dftotal2$ekor03 + dftotal2$ekor46 + dftotal2$ekor714 + dftotal2$ekor151
#too much missing values in house parameters (2008-2009)

#school types
dftotal2$sctype <- ifelse(dftotal2$sctype %in% c(2,3), 1, ifelse(dftotal2$sctype %in% c(4,5,6), 2, 3))

# DID for upper bound -----------------------------------------------------
# Family structure dummy (upper-bound estimation)
dftotal2$nintact <- as.factor(ifelse((dftotal2$fbio == 1) & (dftotal2$mbio == 1), 0, 1))

dftotal21 <- dftotal2[!(dftotal2$year == 2006 & dftotal2$nintact == 1),] #drop already treated in pretreatment (2006)

dftotal21$nintact <- as.numeric(levels(dftotal21$nintact))[dftotal21$nintact]
first_treat <- aggregate(year ~ ID, data = dftotal21[dftotal21$nintact==1, ], FUN = min)
names(first_treat) <- c("ID", "first_treat")

dftotal21 <- dftotal21 %>%
  left_join(first_treat, by = "ID")

dftotal21$first_treat <- ifelse(is.na(dftotal21$first_treat), 0, dftotal21$first_treat)

dftotal3 <- na.omit(dftotal21[,c("ID","year","fgrade","first_treat")]) #baseline
dftotal4 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","fgrade","first_treat")]) #student char
dftotal5 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","fgrade","first_treat")]) #ses
dftotal6 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","fgrade","first_treat")]) #home
dftotal7 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","fgrade","first_treat")]) #school char
dftotal8 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","PSI","lpinv","fgrade","first_treat")]) #PSI and lpinv
dftotal9 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","PSI","lpinv","nschange","fgrade","first_treat")]) #Residential Mobility

#unconditional att(g,t)
att_gt <- att_gt(yname = "fgrade",
                 tname = "year",
                 gname = "first_treat",
                 control_group = "nevertreated",
                 xformla = ~1,
                 data = dftotal3,
                 bstrap = TRUE,
                 panel = FALSE,
                 est_method = "reg")

#conditional att(g,t) exogeneous features
att_gtc1 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat),
                   control_group = "nevertreated",
                   data = dftotal4,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding ses
att_gtc2 <- att_gt(yname = "fgrade",
                  tname = "year",
                  gname = "first_treat",
                  xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree),
                  control_group = "nevertreated",
                  data = dftotal5,
                  bstrap=TRUE,
                  panel = FALSE,
                  est_method = "reg")

#adding home environment
att_gtc3 <- att_gt(yname = "fgrade",
                  tname = "year",
                  gname = "first_treat",
                  xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin,
                  control_group = "nevertreated",
                  data = dftotal6,
                  bstrap=TRUE,
                  panel = FALSE,
                  est_method = "reg")

#adding school FE
att_gtc4 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype),
                   control_group = "nevertreated",
                   data = dftotal7,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding PSI
att_gtc5 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype) + PSI + lpinv,
                   control_group = "nevertreated",
                   data = dftotal8,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding residential mobility
att_gtc6 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype) + PSI + lpinv + nschange,
                   control_group = "nevertreated",
                   data = dftotal9,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

convertMP <- function(x){
  att <- data.table(Group = x$group, Time = x$t, ATT = x$att, se = x$se)
  p <- paste(paste("Observarions =",x$n), "Covariates:", paste("P-value for pre-test of parallel trends assumption:",x$Wpval), sep = "\n")
  xt <- xtable(att,
            Label = paste("Dependent Variable: GPA", paste("Observations =",x$n), "Covariates: -", paste("P-value for pre-test of parallel trends assumption:",x$Wpval), sep = "\n"),
            digits = 3,
            auto = TRUE,
            caption = "Group-time average treatment effect",
            type = "latex",
            label = p)
  print(xt, include.rownames=FALSE)
}

convertMP(att_gt)
convertMP(att_gtc1)
convertMP(att_gtc2)
convertMP(att_gtc3)
convertMP(att_gtc4)
convertMP(att_gtc5)
convertMP(att_gtc6)

ggdid(att_gt)
ggdid(att_gtc1)
ggdid(att_gtc2)
ggdid(att_gtc3)
ggdid(att_gtc4)
ggdid(att_gtc5)
ggdid(att_gtc6)

did.dynuc <- aggte(att_gt, type="dynamic")
did.dync1 <- aggte(att_gtc1, type="dynamic")
did.dync2 <- aggte(att_gtc2, type="dynamic")
did.dync3 <- aggte(att_gtc3, type="dynamic")
did.dync4 <- aggte(att_gtc4, type="dynamic")
did.dync5 <- aggte(att_gtc5, type="dynamic")
did.dync6 <- aggte(att_gtc6, type="dynamic")

summary(did.dynuc)
summary(did.dync1)
summary(did.dync2)
summary(did.dync3)
summary(did.dync4)
summary(did.dync5)
summary(did.dync6)

# Aggregated ATT ----------------------------------------------------------

attd <- data.table(Covariates = c("No Covariates (1)","+ Student Characteristics (2)", "+ SES (3)", "+ Home Environment (4)", "+ School Characteristics (5)", "+ PSI (6)", "+ Residential Mobility (7)"), 
                   ATT = c(did.dynuc$overall.att, did.dync1$overall.att, did.dync2$overall.att, did.dync3$overall.att, did.dync4$overall.att, did.dync5$overall.att, did.dync6$overall.att), 
                   se = c(did.dynuc$overall.se, did.dync1$overall.se, did.dync2$overall.se, did.dync3$overall.se, did.dync4$overall.se, did.dync5$overall.se, did.dync6$overall.se),
                   PTA = c(att_gt$Wpval, att_gtc1$Wpval, att_gtc2$Wpval, att_gtc3$Wpval, att_gtc4$Wpval, att_gtc5$Wpval, att_gtc6$Wpval))
xtd <- xtable(attd,
             digits = 3,
             auto = TRUE,
             caption = "Upper-bound Aggregated Group-time Average Treatment Effects",
             type = "latex")
print(xtd, include.rownames=FALSE)

# Plot event dynamic studies ----------------------------------------------

ggdid(did.dynuc) + 
  geom_smooth(aes(did.dynuc$egt, did.dynuc$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.1), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Unconditional Design")

ggdid(did.dync1) + 
  geom_smooth(aes(did.dync1$egt, did.dync1$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.4), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student")

ggdid(did.dync2) + 
  geom_smooth(aes(did.dync2$egt, did.dync2$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.25), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES")

ggdid(did.dync3) + 
  geom_smooth(aes(did.dync3$egt, did.dync3$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home")

ggdid(did.dync4) + 
  geom_smooth(aes(did.dync4$egt, did.dync4$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home + School")

ggdid(did.dync5) + 
  geom_smooth(aes(did.dync5$egt, did.dync5$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home + School + PSI")

ggdid(did.dync6) + 
  geom_smooth(aes(did.dync6$egt, did.dync6$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home + School + PSI + Residential Mobility")

# DID for lower bound -----------------------------------------------------
dftotal2$nintact2 <- as.factor(ifelse((dftotal2$fbio == 1) & (dftotal2$mbio == 1), 0,
                                       ifelse(((dftotal2$fbio == 1) & (dftotal2$mbio %in% c(2, NA))) | ((dftotal2$mbio == 1) & (dftotal2$fbio %in% c(2, NA))), 1, NA)))  #single bio family

dftotal22 <- dftotal2[!(dftotal2$year == 2006 & dftotal2$nintact2 == 1),]#drop already treated in pretreatment (2006)

dftotal22$nintact2 <- as.numeric(levels(dftotal22$nintact2))[dftotal22$nintact2]
first_treat2 <- aggregate(year ~ ID, data = dftotal22[dftotal22$nintact2==1, ], FUN = min)
names(first_treat2) <- c("ID", "first_treat2")

dftotal22 <- dftotal22 %>%
  left_join(first_treat2, by = "ID")

dftotal22$first_treat2 <- ifelse(is.na(dftotal22$first_treat2), 0, dftotal22$first_treat2)

dftotal3 <- na.omit(dftotal22[,c("ID","year","fgrade","first_treat2")]) #baseline
dftotal4 <- na.omit(dftotal22[,c("ID","year","male","roma","age","lbrthw","hstat","fgrade","first_treat2")]) #student char
dftotal5 <- na.omit(dftotal22[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","fgrade","first_treat2")]) #ses
dftotal6 <- na.omit(dftotal22[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","fgrade","first_treat2")]) #home
dftotal7 <- na.omit(dftotal22[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","fgrade","first_treat2")]) #school char
dftotal8 <- na.omit(dftotal22[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","PSI","lpinv","fgrade","first_treat2")]) #PSI and lpinv
dftotal9 <- na.omit(dftotal22[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","PSI","lpinv","nschange","fgrade","first_treat2")]) #Residential Mobility

#unconditional att(g,t)
att_gt <- att_gt(yname = "fgrade",
                 tname = "year",
                 gname = "first_treat2",
                 control_group = "nevertreated",
                 xformla = ~1,
                 data = dftotal3,
                 bstrap = TRUE,
                 panel = FALSE,
                 est_method = "reg")

#conditional att(g,t) exogeneous features
att_gtc1 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat2",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat),
                   control_group = "nevertreated",
                   data = dftotal4,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding ses
att_gtc2 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat2",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree),
                   control_group = "nevertreated",
                   data = dftotal5,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding home environment
att_gtc3 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat2",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin,
                   control_group = "nevertreated",
                   data = dftotal6,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding school FE
att_gtc4 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat2",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype),
                   control_group = "nevertreated",
                   data = dftotal7,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding PSI
att_gtc5 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat2",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype) + PSI + lpinv,
                   control_group = "nevertreated",
                   data = dftotal8,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding residential mobility
att_gtc6 <- att_gt(yname = "fgrade",
                   tname = "year",
                   gname = "first_treat2",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype) + PSI + lpinv + nschange,
                   control_group = "nevertreated",
                   data = dftotal9,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")


convertMP(att_gt)
convertMP(att_gtc1)
convertMP(att_gtc2)
convertMP(att_gtc3)
convertMP(att_gtc4)
convertMP(att_gtc5)
convertMP(att_gtc6)

ggdid(att_gt)
ggdid(att_gtc1)
ggdid(att_gtc2)
ggdid(att_gtc3)
ggdid(att_gtc4)
ggdid(att_gtc5)
ggdid(att_gtc6)

did.dynuc <- aggte(att_gt, type="dynamic")
did.dync1 <- aggte(att_gtc1, type="dynamic")
did.dync2 <- aggte(att_gtc2, type="dynamic")
did.dync3 <- aggte(att_gtc3, type="dynamic")
did.dync4 <- aggte(att_gtc4, type="dynamic")
did.dync5 <- aggte(att_gtc5, type="dynamic")
did.dync6 <- aggte(att_gtc6, type="dynamic")

summary(did.dynuc)
summary(did.dync1)
summary(did.dync2)
summary(did.dync3)
summary(did.dync4)
summary(did.dync5)
summary(did.dync6)

# Aggregated ATT ----------------------------------------------------------

attd <- data.table(Covariates = c("No Covariates (1)","+ Student Characteristics (2)", "+ SES (3)", "+ Home Environment (4)", "+ School Characteristics (5)", "+ PSI (6)", "+ Residential Mobility (7)"), 
                   ATT = c(did.dynuc$overall.att, did.dync1$overall.att, did.dync2$overall.att, did.dync3$overall.att, did.dync4$overall.att, did.dync5$overall.att, did.dync6$overall.att), 
                   se = c(did.dynuc$overall.se, did.dync1$overall.se, did.dync2$overall.se, did.dync3$overall.se, did.dync4$overall.se, did.dync5$overall.se, did.dync6$overall.se),
                   PTA = c(att_gt$Wpval, att_gtc1$Wpval, att_gtc2$Wpval, att_gtc3$Wpval, att_gtc4$Wpval, att_gtc5$Wpval, att_gtc6$Wpval))
xtd <- xtable(attd,
              digits = 3,
              auto = TRUE,
              caption = "Lower-bound Aggregated Group-time Average Treatment Effects",
              type = "latex")
print(xtd, include.rownames=FALSE)

# Plot event dynamic studies ----------------------------------------------

ggdid(did.dynuc) + 
  geom_smooth(aes(did.dynuc$egt, did.dynuc$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.1), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Unconditional Design")

ggdid(did.dync1) + 
  geom_smooth(aes(did.dync1$egt, did.dync1$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.4), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student")

ggdid(did.dync2) + 
  geom_smooth(aes(did.dync2$egt, did.dync2$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.25), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES")

ggdid(did.dync3) + 
  geom_smooth(aes(did.dync3$egt, did.dync3$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home")

ggdid(did.dync4) + 
  geom_smooth(aes(did.dync4$egt, did.dync4$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home + School")

ggdid(did.dync5) + 
  geom_smooth(aes(did.dync5$egt, did.dync5$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home + School + PSI")

ggdid(did.dync6) + 
  geom_smooth(aes(did.dync6$egt, did.dync6$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on GPA",
       subtitle = "Covariates: Student + SES + Home + School + PSI + Residential Mobility")

# Additional Results for Math ---------------------------------------------

dftotal3 <- na.omit(dftotal21[,c("ID","year","math","first_treat")]) #baseline
dftotal4 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","math","first_treat")]) #student char
dftotal5 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","math","first_treat")]) #ses
dftotal6 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","math","first_treat")]) #home
dftotal7 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","math","first_treat")]) #school char
dftotal8 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","PSI","lpinv","math","first_treat")]) #PSI and lpinv
dftotal9 <- na.omit(dftotal21[,c("ID","year","male","roma","age","lbrthw","hstat","lhincome","welf","mdegree","homesc","nmin","region","maint","sctype","PSI","lpinv","nschange","math","first_treat")]) #Residential Mobility

#unconditional att(g,t)
att_gt <- att_gt(yname = "math",
                 tname = "year",
                 gname = "first_treat",
                 control_group = "nevertreated",
                 xformla = ~1,
                 data = dftotal3,
                 bstrap = TRUE,
                 panel = FALSE,
                 est_method = "reg")

#conditional att(g,t) exogeneous features
att_gtc1 <- att_gt(yname = "math",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat),
                   control_group = "nevertreated",
                   data = dftotal4,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding ses
att_gtc2 <- att_gt(yname = "math",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree),
                   control_group = "nevertreated",
                   data = dftotal5,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding home environment
att_gtc3 <- att_gt(yname = "math",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin,
                   control_group = "nevertreated",
                   data = dftotal6,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding school char
att_gtc4 <- att_gt(yname = "math",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype),
                   control_group = "nevertreated",
                   data = dftotal7,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding PSI
att_gtc5 <- att_gt(yname = "math",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype) + PSI + lpinv,
                   control_group = "nevertreated",
                   data = dftotal8,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")

#adding residential mobility
att_gtc6 <- att_gt(yname = "math",
                   tname = "year",
                   gname = "first_treat",
                   xformla = ~ male + roma + age + lbrthw + factor(hstat) + lhincome + factor(welf) + factor(mdegree) + homesc + nmin + factor(region) + factor(maint) + factor(sctype) + PSI + lpinv + nschange,
                   control_group = "nevertreated",
                   data = dftotal9,
                   bstrap=TRUE,
                   panel = FALSE,
                   est_method = "reg")


convertMP(att_gt)
convertMP(att_gtc1)
convertMP(att_gtc2)
convertMP(att_gtc3)
convertMP(att_gtc4)
convertMP(att_gtc5)
convertMP(att_gtc6)

ggdid(att_gt)
ggdid(att_gtc1)
ggdid(att_gtc2)
ggdid(att_gtc3)
ggdid(att_gtc4)
ggdid(att_gtc5)
ggdid(att_gtc6)

did.dynuc <- aggte(att_gt, type="dynamic")
did.dync1 <- aggte(att_gtc1, type="dynamic")
did.dync2 <- aggte(att_gtc2, type="dynamic")
did.dync3 <- aggte(att_gtc3, type="dynamic")
did.dync4 <- aggte(att_gtc4, type="dynamic")
did.dync5 <- aggte(att_gtc5, type="dynamic")
did.dync6 <- aggte(att_gtc6, type="dynamic")

summary(did.dynuc)
summary(did.dync1)
summary(did.dync2)
summary(did.dync3)
summary(did.dync4)
summary(did.dync5)
summary(did.dync6)

# Aggregated ATT ----------------------------------------------------------

attd <- data.table(Covariates = c("No Covariates (1)","+ Student Characteristics (2)", "+ SES (3)", "+ Home Environment (4)", "+ School Characteristics (5)", "+ PSI (6)", "+ Residential Mobility (7)"), 
                   ATT = c(did.dynuc$overall.att, did.dync1$overall.att, did.dync2$overall.att, did.dync3$overall.att, did.dync4$overall.att, did.dync5$overall.att, did.dync6$overall.att), 
                   se = c(did.dynuc$overall.se, did.dync1$overall.se, did.dync2$overall.se, did.dync3$overall.se, did.dync4$overall.se, did.dync5$overall.se, did.dync6$overall.se),
                   PTA = c(att_gt$Wpval, att_gtc1$Wpval, att_gtc2$Wpval, att_gtc3$Wpval, att_gtc4$Wpval, att_gtc5$Wpval, att_gtc6$Wpval))
xtd <- xtable(attd,
              digits = 3,
              auto = TRUE,
              caption = "Upper-bound Aggregated Group-time Average Treatment Effects",
              type = "latex")
print(xtd, include.rownames=FALSE)

# Plot event dynamic studies ----------------------------------------------

ggdid(did.dynuc) + 
  geom_smooth(aes(did.dynuc$egt, did.dynuc$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.1), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Unconditional Design")

ggdid(did.dync1) + 
  geom_smooth(aes(did.dync1$egt, did.dync1$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.4), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Covariates: Student")

ggdid(did.dync2) + 
  geom_smooth(aes(did.dync2$egt, did.dync2$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.25), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Covariates: Student + SES")

ggdid(did.dync3) + 
  geom_smooth(aes(did.dync3$egt, did.dync3$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Covariates: Student + SES + Home")

ggdid(did.dync4) + 
  geom_smooth(aes(did.dync4$egt, did.dync4$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Covariates: Student + SES + Home + School")

ggdid(did.dync5) + 
  geom_smooth(aes(did.dync5$egt, did.dync5$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.2), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Covariates: Student + SES + Home + School + PSI")

ggdid(did.dync6) + 
  geom_smooth(aes(did.dync6$egt, did.dync6$att.egt), lwd = 0.8, col = "black") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") +
  geom_text(aes(x=0.1, label="separation", y=-0.3), colour="red", angle=90, text=element_text(size=11)) +
  labs(title = "Dynamic Aggregated ATT of Parental Separation on Math Grade",
       subtitle = "Covariates: Student + SES + Home + School + PSI + Residential Mobility")


# Directed Acyclical Graph (DAG) ------------------------------------------

theme_set(theme_dag())

separation_dag <- dagify(gpa ~ home + school + psi + resmob + ses + stud,
                         home ~ sep + ses,
                         school ~ sep + ses,
                         psi ~ sep + ses,
                         resmob ~ sep,
                         ses ~ sep,
                         labels = c("gpa" = "GPA", 
                                    "home" = "Home Environment",
                                    "school" = "School Characteristics",
                                    "psi" = "Parental School Involvement",
                                    "resmob" = "Residential Mobility",
                                    "ses" = "Socioeconomic Status",
                                    "stud" = "Student Characteristics",
                                    "sep" = "Parental Separation"),
                         latent = "sep",
                         outcome = "gpa")

ggdag(separation_dag, text = FALSE, use_labels = "label")


###################
#   END OF CODE   #
###################
