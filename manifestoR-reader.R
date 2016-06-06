# Leslie Huang
# run manifestoR

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/")

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "quantedaData", "ggplot2", "stringr", "manifestoR")
lapply(libraries, require, character.only=TRUE)

mp_setapikey("manifesto_apikey.txt")

my_corpus <- mp_maindataset()

mp_view_originals(party == 41320 & date == 200909)
txt <- content(my_corpus[["42110_2006"]])
class(txt)
