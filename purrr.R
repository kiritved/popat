rm(list=ls(all.names = T))
set.seed(1234)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("kbv",
       "tidyverse",
       "magrittr",
       "rlist",
       "pipeR",
       "dplyr",
       "repurrrsive",
       "stringr",
       "stringdist")

p_loaded()
d=repurrrsive::gap_simple
d
fuck=function(r){
 # p=unique(r[1])
  my=lm(lifeExp~year,r)
  my=summary(my)
    rv=my$r.squared
}

d1=d %>% split(.$country) %>% map_df(fuck) %>% list.sort((.))
d1
d2=as.data.frame(d1);d2;str(d2)

d=repurrrsive::sw_people
d %>% list.count()
d[1] 
d[[1]]
fl=function(s){
  print(s)
  rv=str_length(s)
}
fq=function(v){
    rv=list.map(v,fl(v))
    rv=list(name=v,length=rv)
}
d %>% #list.sample(3) %>% 
  list.select(name) %>% list.map(fq(.)) 
v="kirit ved"
l=fl(v);l
