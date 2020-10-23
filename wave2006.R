
# -------------------------------------------------------------------------

#Életpálya 2006, 1. hullám
#Author: Nguyen Nam Son
#Date: 05-10-2020

# -------------------------------------------------------------------------
#loading packages

library(haven)
library(ggplot2)
library(dplyr)
library(data.table)
library(plm)
library(stargazer)
library(labelled)
library(sjmisc)

wd <- file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

df <- read_dta("eletpalya_a.dta")

pdf <- pdata.frame(df, index <- c("azon", "hullam")) #cross sectional and wave dimensions
pdim(pdf)

pdf2 <- pdf %>% select(c(azon,
                        hullam,
                        af002a01,
                        af006x01,
                        af006x02,
                        af007xxx,
                        af008xxx,
                        af010x01,
                        af010x02,
                        af011xxx,
                        af012xxx,
                        af020xxx, 
                        af095xxx, 
                        af070a15, 
                        af070b15,
                        af070c15,
                        af070d15,
                        af133axx,
                        ad030exx,
                        af135exx, 
                        af140xxx, 
                        af145cxx, 
                        af176xxx, 
                        af190xxx,
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
                        af122xxx,
                        af127axx))
# Data cleaning -----------------------------------------------------------

names(pdf2) <- c('ID',
                'nwave',
                'gender',
                'mbio',
                'mstep',
                'age_at_sepm',
                'msep_reason',
                'fbio',
                'fstep',
                'age_at_sepf',
                'fsep_reason',
                'citizenship',
                'same_school',
                'rschange1',
                'rschange2',
                'rschange3',
                'rschange4',
                'schooltalk',
                'studyparent',
                'housework',
                'xtraclass',
                'flove',
                'mnsal',
                'fnsal',
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
                'no_seceduc',
                'seceduc')


# Removing labels and assigning NAs ---------------------------------------

get_labels(pdf2)

pdf3 <- pdf2 %>% remove_val_labels()

concatFgrade <- function(pdf3){
  pdf3$fgrade <<- as.numeric(paste(pdf3$intfgrade, pdf3$decfgrade, sep = "."))
}

concatFgrade(pdf3)

pdf3[pdf3 == -6 | pdf3 == 99 | pdf3 == 999 | pdf3 == 9999] <- NA
pdf3$fgrade[pdf3$fgrade > 5] <- NA

pdf3[,!names(pdf3) %in% c("fsep_reason","mdegree","fdegree","mactivity","factivity","no_seceduc")][pdf3[,!names(pdf3) %in% c("fsep_reason","mdegree","fdegree","mactivity","factivity","no_seceduc")] == 9 ] <- NA

summary(pdf3)

# Treating dependent variables---------------------------------------------------------

#those with final grades
final_grade = subset(pdf2, (fgrade <= 5.0 & !is.na(fgrade)))

#those with math grade
math_grade = subset(pdf2, (math <= 5.0 & !is.na(math)))

#with grammar grade
gram_grade = subset(pdf2, (gram <= 5.0 & !is.na(gram)))

#with  literature grade
liter_grade = subset(pdf2, (liter <= 5.0 & !is.na(liter)))

#with behavior grade
behav_grade = subset(pdf2, (behav <= 5.0 & !is.na(behav)))

#with diligence grade
behav_grade = subset(pdf2, (dilig <= 5.0 & !is.na(dilig)))

#with math competence test score
math_ctest = subset(pdf2, !is.na(math_comp))

#with reading competence test score
math_ctest = subset(pdf2, !is.na(read_comp))

#repeated school before 4th grade (dummy)
rep_4 = subset(pdf2, !is.na(rep4))

#repeated school between 5th and 8th (dummy)
rep_5to8 = subset(pdf2, !is.na(rep58))

#wants to study further (dummy)
study_further = subset(pdf2, seceduc %in% c(1,2,3))


# Filtered panels ------------------------------------------

#father only families
father_o_fam = subset(pdf2, (fbio == 1 & mbio == 2 & mstep == 2))

#mother only families
mother_o_fam = subset(pdf2, (mbio == 1 & fbio == 2 & fstep == 2))

#stepmother families
stepmother_fam = subset(pdf2, (fbio == 1 & mstep == 1))

#stepfather families
stepfather_fam = subset(pdf2, (mbio == 1 & fstep == 1))

#foster families
foster_fam = subset(pdf2, (mstep == 1 & fstep == 1))
