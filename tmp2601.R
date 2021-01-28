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
url <- "https://renkun-ken.github.io/rlist-tutorial/data/people.json"
people <- list.load(url)
p=people
#### no of people in the list
p %>>% list.count()
#### no of people with condition
p %>>% list.filter("music" %in% Interests) %>>% list.count
p %>>% list.filter("music" %in% .$Interests) %>>% list.count
p %>>% list.filter(!("music" %in% Interests)) %>>% list.count
p %>>% 
        list.select(Name,Age,Expertise) %>>% 
        list.filter(.$Expertise$R>3) %>>% 
        list.count()
p %>>% 
        list.select(Name,Age,Expertise) %>>% 
        list.filter(.$Expertise$R>3 & .$Age>30) %>>% 
        list.count()
p %>>% list.table(.$Interests)        
p %>>% list.table(.$Expertise)        
p %>>% 
        list.select(Name,Age,Expertise,te=sum(as.numeric(.$Expertise))) %>>% 
        list.filter(sum(as.numeric(.$Expertise))>13) %>>% list.count
p %>>% 
        list.select(Name,Age,Expertise,te=sum(as.numeric(.$Expertise))) %>>% 
        list.filter(sum(as.numeric(.$Expertise))>12)%>>% 
        ### for ascending order
        #list.sort(sum(as.numeric(.$Expertise)))
        ## for descending order
        list.sort((sum(as.numeric(.$Expertise))))
p %>>% 
        list.sort((sum(as.numeric(.$Expertise))))  %>>% 
        
        list.select(Name,Age,sum(as.numeric(.$Expertise))) %>>% 
        list.mapv(list.select(Name))
p %>>% 
        list.sort((sum(as.numeric(.$Expertise))))  %>>% 
        
        list.select(Name,te=sum(as.numeric(.$Expertise))) %>>% 
        list.mapv(
                .$te
        )

p %>>% list.cases(.$Interests)
p %>>% list.class(.$Interests) %>>% list.count
p %>>% list.table(.$Interests)
p %>>% list.table(.$Expertise)
p %>>% list.table(.$Expertise)
p %>>% list.table(names(.$Expertise))
p %>>% list.filter(.$Expertise$Cpp >0)%>>% list.count()
p %>>% list.filter(!is.na(.$Expertise$Cpp))%>>% list.count()
p %>>% list.filter(.$Age>30) %>>% list.subset()
cnt=0
for(i in p){
        cnt=cnt+1
        print(paste(cnt,i[1],i[2],i[3],i[4],sep="="))
}

list.iter(p, cat(Name,
                 "-",
                 Age,
                 "-",
                 Interests,
                 "-",
                 unlist(names(Expertise)),
                 "-",
                 unlist(Expertise),
                 "-",
                 sum(unlist(Expertise))
                 , "\n")
          )
p %>>% list.iter(Name) %>>% str
p %>>% list.mapv(.$Name)
fx=function(v){
        rv=sum(as.numeric(v))
        #rv=list(v,rv)
}
p %>>% list.select(Expertise,fx(.$Expertise))
p1=list(a=1:7,b=letters[1:5])
p2=list(a=4:12,b=letters[5:15])
l=list(p1,p2);l
l %>>% list.filter("e" %in% .$b) %>% list.extract(1)
l %>>% list.exclude("f" %in% .$b)
p %>>% list.map("ASya" %in% Name)
p %>% list.sort(.$Name)
p %>% list.sort((.$Age))
p  %>% 
        list.select(Name,Age) %>%
list.filter(
        length(unlist(.$Interests))==0
        ) 

 d=kbv::mytest_data()
 d
 d$fl1=as.factor(d$fl1)
 d$fl2=as.factor(d$fl2)
 d$x=as.numeric(d$x)
 
 str(d)
 fx=function(v){
   print((v))
   if (class(v) == "factor"){
     rv=table(v)
   }
   else
   {
     rv=mean(v)
   }
 }
 fx0=function(v){
   rv=map(v,fx)
 }
 d %>% as_tibble() %>% split(.$fl2) %>%  map(fx0)
 d %>% as_tibble() %>% group_by(fl2) %>% nest() %>% map_depth(2,fx0)
 