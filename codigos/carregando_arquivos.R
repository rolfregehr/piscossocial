library(read.dbc)
library(tidyverse)
library(stringr)

pasta_lidos <- "D:/OneDrive/Projetos R/PS/lidos/"


#PRIMEIRA LEITURA


arq = list.files("D:/OneDrive/Projetos R/PS/")
arq = arq[str_detect(arq, ".dbc")]

cont = 0

for (i in arq) {
  
  if (cont == 0) {
    
    atend_ps = read.dbc(i)
    atend_ps$NAT_JUR = NULL
    
  } 
  else {
    
    temp = read.dbc(i)
    temp$NAT_JUR = NULL
    
    atend_ps = rbind(atend_ps, temp)
    atend_ps = atend_ps[!is.na(atend_ps$PERMANEN),]
  }
  
  
  print(i)
  cont = cont+1
  print(cont)
  print(length(atend_ps$CNS_PAC))
  file.copy(arq[i], paste(pasta_lidos, arq[i], sep=""))
  file.remove(arq[i])
  
  }
