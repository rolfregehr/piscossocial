rm(list = ls())
arquivos <- list.files("e:/ps/", full.names = T)

library(read.dbc)
library(tidyverse)

temp <- read.dbc("PSSE2008.dbc")
glimpse(temp)

# rm(temp)

dados_ps <- read.dbc(arquivos[1]) %>% select(INICIO, CNS_PAC, , TPIDADEPAC, IDADEPAC,
                                             SEXOPAC, RACACOR, CIDPRI) %>% 
  filter(!is.na(INICIO)) %>% 
  group_by(INICIO, CNS_PAC, , TPIDADEPAC, IDADEPAC, SEXOPAC, RACACOR, CIDPRI) %>% summarise(qtde = n())

  
for (i in 2:length(arquivos)) {
  temp <-  read.dbc(arquivos[i]) %>% select(INICIO, CNS_PAC, , TPIDADEPAC, IDADEPAC,
                                            SEXOPAC, RACACOR, CIDPRI) %>% 
    filter(!is.na(INICIO)) %>% 
    group_by(INICIO, CNS_PAC, , TPIDADEPAC, IDADEPAC, SEXOPAC, RACACOR, CIDPRI) %>% summarise(qtde = n())
  dados_ps <- rbind(dados_ps, temp)
  
  print(arquivos[i])
}


save(dados_ps, file="./rda/dados_ps_20211208.rda")

