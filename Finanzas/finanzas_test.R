library("data.table")
library("fst")
library('tidyverse')


#limpio la memoria
rm(list=ls())
gc()

setwd("C:/Users/Luxor/Documents/DM Finanzas/datasetsOri/")

data <- fst::read.fst( 'parquete_premium.rdata' ) %>% filter(foto_mes =='201904' )


records <- data %>% count(foto_mes)
