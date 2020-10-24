

# NFO ---------------------------------------------------------------------

#Életpálya 2006, 1. hullám
#Author: Nguyen Nam Son
#Date: 05-10-2020

# Setup -------------------------------------------------------------------

library(haven)
library(ggplot2)
library(dplyr)
library(data.table)
library(plm)
library(stargazer)
library(labelled)
library(sjlabelled)
library(summarytools)

wd <- file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

df <- read_dta("eletpalya_a.dta")

df2 <- df %>% select(c(azon,
                        hullam,
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
                        af020xxx, 
                        af095xxx,
                        af096xxx,
                        af069xxx,
                        af070a15, 
                        af070b15,
                        af070c15,
                        af070d15,
                        af080xxx,
                        af081xxx,
                        af084xxx,
                        af133axx,
                        ad030exx,
                        af135exx, 
                        af140xxx, 
                        af145cxx,
                        af145dxx,
                        af146xxx,
                        af167xxx, 
                        af190xxx,
                        af156xxx,
                        af179xxx,
                        af200xxx,
                        af201xxx,
                        ad024bxx,
                        ad025axx, 
                        ad025cxx, 
                        af209xxx, 
                        ahomesc, 
                        acognisc, 
                        aemotisc, 
                        amedes, 
                        amvan,
                        amisk, 
                        amdolg, 
                        afvan,
                        afedes, 
                        afisk, 
                        afdolg, 
                        atest, 
                        aszeg,
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
                        af072xxx,
                        af127axx))
# Data cleaning -----------------------------------------------------------

names(df2) <- c('ID',
                'nwave',
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
                'citizenship',
                'same_school',
                'rschange',
                'othersc',
                'rschange1',
                'rschange2',
                'rschange3',
                'rschange4',
                'pmeet',
                'pttalk',
                'peduc_asp',
                'schooltalk',
                'studyparent',
                'housework',
                'xtraclass',
                'flove',
                'mrel',
                'tsfather',
                'mnsal',
                'fnsal',
                'methnic',
                'fethnic',
                'wnbrh',
                'cnbrh',
                'nbooks',
                'workdesk',
                'comp',
                'internet',
                'homesc',
                'cognisc',
                'emotisc',
                'mcrtaker',
                'ismother',
                'mdegree',
                'mactivity',
                'isfather',
                'fcrtaker',
                'fdegree',
                'factivity',
                'nsibling',
                'pind',
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
                'rep58',
                'seceduc')

# Removing labels and assigning NAs ---------------------------------------

get_labels(df2)

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
                        "nbooks",
                        "fsep_reason",
                        "mdegree",
                        "fdegree",
                        "mactivity",
                        "factivity",
                        "no_seceduc")][df2[,!names(df2) %in% c("age_at_sepf",
                                                               "age_at_sepm",
                                                               "age_at_remf",
                                                               "age_at_remm",
                                                               "mrel",
                                                               "methnic",
                                                               "fethnic",
                                                               "nbooks",
                                                               "fsep_reason",
                                                               "mdegree",
                                                               "fdegree",
                                                               "mactivity",
                                                               "factivity",
                                                               "no_seceduc")] == 9 ] <- NA
#factorizing
# names <- c('msep_reason',
#            'fsep_reason',
#            'rschange',
#            'rschange1',
#            'rschange2',
#            'rschange3',
#            'rschange4',
#            'pmeet',
#            'pttalk',
#            'schooltalk', 
#            'studyparent',
#            'peduc_asp',
#            'housework',
#            'flove',
#            'mrel',
#            'tsfather',
#            'methnic',
#            'fethnic',
#            'wnbrh',
#            'cnbrh',
#            'mactivity',
#            'factivity',
#            'mdegree',
#            'fdegree',
#            'pind')
# 
# df2[, fct] <- as.factor(df2[, fct])
# 
# bool <- c('gender',
#           'mbio',
#           'mstep',
#           'fbio',
#           'fstep',
#           'same_school',
#           'othersc',
#           'xtraclass',
#           'workdesk',
#           'comp',
#           'internet',
#           'mcrtaker',
#           'ismother',
#           'isfather',
#           'fcrtaker')
# 
# df2[, bool] <- as.logical(df2[, bool])

# Descriptive summaries ----------------------------------------------------

glimpse(df2)
df2 %>% remove_all_labels() %>% dfSummary(., style = "grid", 
                                          graph.magnif = 0.75, valid.col = FALSE)

# Cross tabulations -------------------------------------------------------

ctable(df2$gender, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$gender, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$gender, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$gender, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

ctable(df2$same_school, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$same_school, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$same_school, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$same_school, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

ctable(df2$othersc, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$othersc, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$othersc, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$othersc, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

ctable(df2$rep4, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$rep4, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$rep4, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$rep4, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

ctable(df2$rep58, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$rep58, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$rep58, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$rep58, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

ctable(df2$seceduc, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$seceduc, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$seceduc, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$seceduc, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

ctable(df2$pind, df2$fbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$pind, df2$mbio, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$pind, df2$mstep, prop = "r", chisq = TRUE, OR = TRUE)
ctable(df2$pind, df2$fstep, prop = "r", chisq = TRUE, OR = TRUE)

# Creating panel df -------------------------------------------------------

pdf <- pdata.frame(df2, index <- c("azon", "hullam")) #cross sectional and wave dimensions
pdim(pdf)

# Treating dependent variables---------------------------------------------------------

#those with final grades
final_grade = subset(pdf, (fgrade <= 5.0 & !is.na(fgrade)))

#those with math grade
math_grade = subset(pdf, (math <= 5.0 & !is.na(math)))

#with grammar grade
gram_grade = subset(pdf, (gram <= 5.0 & !is.na(gram)))

#with  literature grade
liter_grade = subset(pdf, (liter <= 5.0 & !is.na(liter)))

#with behavior grade
behav_grade = subset(pdf, (behav <= 5.0 & !is.na(behav)))

#with diligence grade
behav_grade = subset(pdf, (dilig <= 5.0 & !is.na(dilig)))

#with math competence test score
math_ctest = subset(pdf, !is.na(math_comp))

#with reading competence test score
math_ctest = subset(pdf, !is.na(read_comp))

#repeated school before 4th grade (dummy)
rep_4 = subset(pdf, !is.na(rep4))

#repeated school between 5th and 8th (dummy)
rep_5to8 = subset(pdf, !is.na(rep58))

#wants to study further (dummy)
study_further = subset(pdf, seceduc %in% c(1,2,3))

# Filtered panels ------------------------------------------

#father only families
father_o_fam = subset(pdf, (fbio == 1 & mbio == 2 & mstep == 2))

#mother only families
mother_o_fam = subset(pdf, (mbio == 1 & fbio == 2 & fstep == 2))

#stepmother families
stepmother_fam = subset(pdf, (fbio == 1 & mstep == 1))

#stepfather families
stepfather_fam = subset(pdf, (mbio == 1 & fstep == 1))

#foster families
foster_fam = subset(pdf, (mstep == 1 & fstep == 1))

# Models ------------------------------------------------------------------
#bivariate relationship between family structure and final grade


