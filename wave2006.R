
# -------------------------------------------------------------------------

#Életpálya 2006, 1. hullám
#Author: Nguyen Nam Son
#Date: 05-10-2020

# -------------------------------------------------------------------------
library(haven)
library(ggplot2)
library(dplyr)
library(data.table)
library(plm)

wd = file.path("~", "thesis_eletpalya", "kesz")
setwd(wd)

h1 = read_dta("eletpalya_a.dta")
vlist = read.csv("eletpalya6_valtozolista.csv",encoding = "UTF-8", header = TRUE)
names(h1)
