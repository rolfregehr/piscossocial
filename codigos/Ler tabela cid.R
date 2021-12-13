library(tidyverse)
tabela_cid <- read.delim("D:/OneDrive/Projetos R/dados_gerais/tb_cid.txt", header = F)
tabela_cid$codigo <- str_sub(tabela_cid$V1, 1, 4)
tabela_cid$descricao <- str_sub(tabela_cid$V1, 5, 104)

tabela_cid$codigo <- if_else(str_sub(tabela_cid$codigo, 4, 4) == " ", str_sub(tabela_cid$codigo, 1, 3), tabela_cid$codigo)
tabela_cid$V1 = NULL

save(tabela_cid, file = "D:/OneDrive/Projetos R/dados_gerais/tabela_cid.rda")
