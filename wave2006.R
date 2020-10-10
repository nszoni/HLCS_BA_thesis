
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

wd <- file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

df <- read_dta("eletpalya_a.dta")

pdf <- pdata.frame(df, index <- c("azon", "xhullam")) #cross sectional and wave dimensions
pdim(pdf)

#omitting irrelevant variables
pdf1 <- pdf %>% select(c(sorszam, suly, onsuly_minta, hullam, chullam, xhullam, azon,
                        evfolyam, diakid, csaksni, sniszam, t2, t1ev, m_zpsc, o_zpsc,
                        o, m, af001xxx, af002a01, af002a02, af002a03, af002a04, af002a05,
                        af002a07, af002b01, af002b03, af002b04, af002b05, af002b07, af002b08,
                        af002b12, af002c01, af002c03, af002c04, af002c05, af002c07, af002c08,
                        af002c12, af002d01, af002d03, af002d04, af002d05, af002d07, af002d08,
                        af002d12, af002e01, af002e03, af002e04, af002e05, af002e07, af002e08,
                        af002e12,  af002f01, af002f03, af002f04, af002f05, af002f07, af002f08,
                        af002f12, af002h01, af002h03, af002h04, af002h05, af002h07, af002h08,
                        af002h12, af002i01, af002i03, af002i04, af002i05, af002i07, af002i08,
                        af002i12, af002j01, af002j03, af002j04, af002j05, af002j07, af002j08,
                        af002j12, af002k01, af002k03, af002k04, af002k05, af002k07, af002k08,
                        af002k12,  af002l01, af002l03, af002l04, af002l05, af002l07, af002l08,
                        af002l12, af002m01, af002m03, af002m04, af002m05, af002m07, af002m08,
                        af002m12,  af002n01, af002n03, af002n04, af002n05, af002n07, af002n08,
                        af002n12, af002o01, af002o03, af002o04, af002o05, af002o07, af002o08,
                        af002o12, af006x01, af006x02, af007xxx, af008xxx, af009xxx, af010x01,
                        af010x02, af011xxx,  af012xxx,  af013xxx,  af014xxx,  af015xxx,
                        af020xxx, af070a15, af070b15, af070c15, af070d15, af071xxx, af072xxx,
                        af074axx, af074bxx, af074cxx, af074dxx, af074exx, af074fxx, af080xxx,
                        af081xxx, af082cxx, af083axx, af084xxx, af086xxx, af087xxx, af093xxx,
                        af094xxx, af095xxx, af096xxx, af122xxx, af124xxx, af127axx, af129xxx,
                        af133axx, af133bxx, af133cxx, af133dxx, af133exx, af133fxx, af133gxx,
                        af133hxx, af133ixx, af134xxx, af135axx, af135bxx, af135cxx, af135dxx,
                        af135exx, af135fxx, af139xxx, af140xxx, af141xxx, af142xxx, af145axx,
                        af145bxx, af145cxx, af145dxx, af146xxx, af147xxx, af148xxx, af149xxx,
                        af152xxx, af152xxx, af156xxx, af167xxx, af175xxx, af179xxx, af190xxx,
                        af207axx, af207bxx, af208xxx, af209xxx, af210xxx, af218b01, af219axx,
                        af219cxx, af219dxx, adev, ad002axx, ad003axx, ad003bxx, ad003cxx,
                        ad003dxx, ad003exx, ad004axx, ad024axx, ad024bxx, ad025axx, ad025cxx,
                        ad030exx, ad039xxx, ad041xxx, ahome1, ahome8, ahome9, ahome10, ahome13,
                        ascore, ainvalid, ahomesc, acognisc, aemotisc, amvan, amedes, amcsall,
                        amisk, amdolg, amnemz1, amkeres, amgyerek, afvan, afedes, afcsall, afisk,
                        afdolg, afnemz1, afkeres, afgyerek, acsaljov, aszeg, atest))

pdf2 <- pdf %>% select(c(azon,
                        xhullam,
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
                        amkeres, 
                        afvan,
                        afedes, 
                        afisk, 
                        afdolg, 
                        afkeres, 
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
                'mnsal2',
                'isfather',
                'fcrtaker',
                'fdegree',
                'factivity',
                'fnsal2',
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
                'rep_4',
                'rep_58',
                'no_seceduc',
                'seceduc')


# Treating dependent variables---------------------------------------------------------

concatFgrade <- function(pdf2){
  pdf2$fgrade <<- as.numeric(paste(pdf2$intfgrade, pdf2$decfgrade, sep = "."))
}

concatFgrade(pdf2)

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
rep_4 = subset(pdf2, !is.na(rep_4))

#repeated school between 5th and 8th (dummy)
rep_5to8 = subset(pdf2, !is.na(rep_58))

#wants to study further (dummy)
study_further = subset(pdf2, seceduc %in% c(1,2,3))


# Filtered panels ------------------------------------------

#for observing differences father only families
father_o_fam = subset(pdf2, (age_at_sepm %in% range(0,20) & fbio == 1 & mbio == 2 & mstep == 2))

#for observing differences in mother only families
mother_o_fam = subset(pdf2, (age_at_sepf %in% range(0,20) & mbio == 1 & fbio == 2 & fstep == 2))

#stepmother families
stepmother_fam = subset(pdf2, (age_at_sepm %in% range(0,20) & fbio == 1 & mstep == 1))

#stepfather families
stepfather_fam = subset(pdf2, (age_at_sepf %in% range(0,20) & mbio == 1 & fstep == 1))

#foster families
foster_fam = subset(pdf2, (age_at_sepf %in% range(0,20) & mstep == 1 & fstep == 1))
