rm(list = ls())
arquivos <- list.files("e:/ps/", full.names = T)


# seleciona o período e uf, etc

arquivos <- arquivos[which(str_sub(arquivos, 11, 12) >= 18)]





#entender a estrutura dos dados
library(read.dbc)
library(tidyverse)

temp <- read.dbc(arquivos[1000])
glimpse(temp)

rm(temp)

dados_ps <- read.dbc(arquivos[1]) %>% select(INICIO, TPIDADEPAC, IDADEPAC,
                                             SEXOPAC, RACACOR, CIDPRI) %>% 
  filter(!is.na(INICIO)) %>% 
  group_by(INICIO, TPIDADEPAC, IDADEPAC, SEXOPAC, RACACOR, CIDPRI) %>% summarise(qtde = n())

unique(dados_ps$TPIDADEPAC)  


for (i in 2:length(arquivos)) {
  temp <-  read.dbc(arquivos[i]) %>% select(INICIO, TPIDADEPAC, IDADEPAC,
                                            SEXOPAC, RACACOR, CIDPRI) %>% 
    filter(!is.na(INICIO)) %>% 
    group_by(INICIO, TPIDADEPAC, IDADEPAC, SEXOPAC, RACACOR, CIDPRI) %>% summarise(qtde = n())
  dados_ps <- rbind(dados_ps, temp)
  
  print(arquivos[i])
}


save(dados_ps, file="./rda/dados_ps_20211208.rda")

