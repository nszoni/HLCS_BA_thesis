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

wd <- file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

df <- read_dta("eletpalya_a.dta")

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
# Data cleaning -----------------------------------------------------------

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

df2$fam_str2 <- as.factor(ifelse((fbio == 1) & (mbio == 1), 'intact', 'nintact'))

# Mean tables across family structures ------------------------------------

mfinc <- aggregate(df2[, c('fam_income', 'mnsal', 'fnsal')],
                   list(df2$fam_str2), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mgrades <- aggregate(df2[, c('fgrade', 'math_comp', 'read_comp', 'math', 'gram', 'liter', 'behav', 'dilig')],
                        list(df2$fam_str2), function(x) c(round(mean(x, na.rm = TRUE), 2)))

mscores <- aggregate(df2[, c('homesc', 'cognisc', 'emotisc')],
                     list(df2$fam_str2), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mfinc, "~/thesis_eletpalya/mfinc.txt", sep="\t")
write.table(mgrades, "~/thesis_eletpalya/mygrades.txt", sep="\t")
write.table(mscores, "~/thesis_eletpalya/mscores.txt", sep="\t")

# Descriptive summaries ----------------------------------------------------

df2sum <- df2[, !names(df2) %in% c('ID', 'mbio', 'fbio', 'mstep', 'fstep', 'age_at_remm', 'age_at_remf')] %>% 
  remove_all_labels() %>% dfSummary(., plain.ascii = FALSE, style = "grid", 
                                    graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp") 

df2sum$Missing <- NULL
view(df2sum, file = "~/thesis_eletpalya/df2sum.html")

# Cross tabulations -------------------------------------------------------
msep <- ctable(df2$msep_reason, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(msep$proportions, "~/thesis_eletpalya/msep.txt", sep="\t")

fsep <- ctable(df2$fsep_reason, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(fsep$proportions, "~/thesis_eletpalya/fsep.txt", sep="\t")

fam_str <- ctable( df2$fam_str, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(fam_str$proportions, "~/thesis_eletpalya/fam_str.txt", sep="\t")

gndr <- ctable(df2$gender, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(gndr$proportions, "~/thesis_eletpalya/gndr.txt", sep="\t")

rep4 <- ctable(df2$rep4, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(rep4$proportions, "~/thesis_eletpalya/rep4.txt", sep="\t")

rep58 <- ctable(df2$rep58, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(rep58$proportions, "~/thesis_eletpalya/rep58.txt", sep="\t")

#number of school changes due to moving
df2_temp <- df2[, names(df2) %in% c('rschange1', 'rschange2', 'rschange3', 'rschange4')]
df2_temp$nschange <- apply(df2_temp, 1, function(x) length(which(x == 2)))
df2$nschange <- df2_temp$nschange

#residential mobility
resmob <- ctable(df2$nschange, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(resmob$cross_table, "~/thesis_eletpalya/resmobct.txt", sep="\t")
write.table(resmob$proportions, "~/thesis_eletpalya/resmobpp.txt", sep="\t")

#suspension and expel
df2_temp$nexp <- apply(df2_temp, 1, function(x) length(which(x == 4)))
df2$nexp <- df2_temp$nexp
exp <- ctable(df2$nexp, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(exp$proportions, "~/thesis_eletpalya/exp.txt", sep="\t")

#leaving because of weak performance (~ drop out)
df2_temp$ndpout <- apply(df2_temp, 1, function(x) length(which(x == 5)))
df2$ndpout <- df2_temp$ndpout
dropout <- ctable(df2$ndpout, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(dropout$proportions, "~/thesis_eletpalya/dropout.txt", sep="\t")

#for parental involvement
ctable(df2$peduc_asp, df2$fam_str2, prop = "c", chisq = TRUE, OR = TRUE)

#create index for parental involvement
df2$pscinv <- df2$pmeet + df2$pttalk + df2$studyparent
#the lower the better
mpscinv <- aggregate(df2[, c("pscinv", "pmeet", "pttalk", "studyparent")],
                   list(df2$fam_str2), function(x) c(round(mean(x, na.rm = TRUE), 2)))

write.table(mpscinv, "~/thesis_eletpalya/mpscinv.txt", sep="\t")

#additional parental investments not in home scale
df2_temp2 <- df2_temp <- df2[, names(df2) %in% c('xtraclass', 'workdesk', 'comp', 'internet')]
df2_temp2$npinv <- apply(df2_temp2, 1, function(x) length(which(x == 2)))
df2$npinv <- df2_temp2$npinv
npinv <- ctable(df2$npinv, df2$fam_str2, prop = "c", chisq = TRUE)
write.table(npinv$proportions, "~/thesis_eletpalya/npinv.txt", sep="\t")

# Correlation matrices for feature selection----------------------------------------------------

cormatdf <- df2[, c('fgrade',
                   'math_comp',
                   'read_comp',
                   'math',
                   'gram',
                   'liter',
                   'behav',
                   'dilig',
                   'age_at_sepm',
                   'age_at_sepf',
                   'mnsal',
                   'fnsal',
                   'homesc',
                   'fam_income',
                   'pscinv',
                   'nschange')]

cormat <- cormatdf %>% remove_all_labels() %>% cor(use = "complete.obs") %>% round(., 2)

# Reorder matrix according to coefficients and hierarchical clustering order
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  dd[is.na(dd)] <- 0
  dd[is.nan(dd)] <- 0
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

cormat <- reorder_cormat(cormat)
lower_tri <- get_lower_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(lower_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1) )+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  labs(title = "Correlation heatmap between the non-categorial variables",
       subtitle = "ordered according to hclust")
  
# Creating panel df -------------------------------------------------------

# pdf <- pdata.frame(df2, index <- c("azon", "hullam")) #cross sectional and wave dimensions
# pdim(pdf)
# 
# # Treating dependent variables---------------------------------------------------------
# 
# #those with final grades
# final_grade = subset(pdf, (fgrade <= 5.0 & !is.na(fgrade)))
# 
# #those with math grade
# math_grade = subset(pdf, (math <= 5.0 & !is.na(math)))
# 
# #with grammar grade
# gram_grade = subset(pdf, (gram <= 5.0 & !is.na(gram)))
# 
# #with  literature grade
# liter_grade = subset(pdf, (liter <= 5.0 & !is.na(liter)))
# 
# #with behavior grade
# behav_grade = subset(pdf, (behav <= 5.0 & !is.na(behav)))
# 
# #with diligence grade
# behav_grade = subset(pdf, (dilig <= 5.0 & !is.na(dilig)))
# 
# #with math competence test score
# math_ctest = subset(pdf, !is.na(math_comp))
# 
# #with reading competence test score
# math_ctest = subset(pdf, !is.na(read_comp))
# 
# #repeated school before 4th grade (dummy)
# rep_4 = subset(pdf, !is.na(rep4))
# 
# #repeated school between 5th and 8th (dummy)
# rep_5to8 = subset(pdf, !is.na(rep58))
# 
# #wants to study further (dummy)
# study_further = subset(pdf, seceduc %in% c(1,2,3))
# 
# # Filtered panels ------------------------------------------
# 
# #two-parent biological families
# two_p_fam = subset(pdf, fam_str = 'tparent')
# 
# #father only families
# father_o_fam = subset(pdf, fam_str == 'singlef')
# 
# #mother only families
# mother_o_fam = subset(pdf, fam_str == 'singlem')
# 
# #stepmother families
# stepmother_fam = subset(pdf, fam_str == 'stepm')
# 
# #stepfather families
# stepfather_fam = subset(pdf, fam_str == 'stepf')
# 
# #foster families
# foster_fam = subset(pdf, fam_str == 'foster')
# 
# # Models ------------------------------------------------------------------
# #bivariate pooled OLS regression
# 
# ols_bi <- lm(fgrade ~ fam_str, data = df2)
# summary(ols_bi)
# 
# #multivariate pooled OLS regression
# 
# ols_m1 <- lm(fgrade ~ fam_str + fam_income + gender + homesc + cognisc + emotisc, data = df2)
# summary(ols_m1)
# 
# stargazer(ols_bi, ols_m1, type = 'text')
# 
# #IV estimation with 2SLS to treat the question of endogeneity
# 
# # -------------------------------------------------------------------------
# # 2SLS involves estimating two regressions: In the first stage,
# # the endogenous variable (log price in our example) is regressed
# # on the instrument or instruments (tdiff), along with any other
# # exogenous variables (controls). The estimated coefficients from
# # the first-stage regression are used to predict the endogenous variable (log price).
# # In the second stage, the dependent variable (log quantity) is regressed 
# # on the predicted values of the endogenous variable (predicted log price), 
# # along with the exogenous controls.
# # -------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
