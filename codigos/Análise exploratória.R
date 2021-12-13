rm(list = ls())
load("./rda/dados_ps_20211208.rda")

library(lubridate)
library(tidyverse)

# Ajustes ####
dados_ps$cid_3d <- str_sub(dados_ps$CIDPRI, 1, 3)
dados_ps$cid_1d <- str_sub(dados_ps$CIDPRI, 1, 1)


#Recodificação de RACA/COR 
unique(dados_ps$RACACOR)
dados_ps$RACACOR <- recode(dados_ps$RACACOR, "01" = "Branca", "02" = "Preta", "03" = "Parda", "04" = "Amarela", "05" = "Indígena", "99" = "NI")
unique(dados_ps$RACACOR)

#Ajuste das datas
dados_ps$INICIO <- ymd(dados_ps$INICIO)

# Cálculo da idade
dados_ps$IDADEPAC <- as.numeric(dados_ps$IDADEPAC)
dados_ps$idade <- if_else(dados_ps$TPIDADEPAC == "4",
                          dados_ps$IDADEPAC,
                          if_else(dados_ps$TPIDADEPAC == "5",
                                  100+dados_ps$IDADEPAC, 0))


# atendimentos por cid_3d e sexo biológico ######


png("./png/atendimentos_alcool.png", width = 1920, height = 1080)

dados_ps %>% 
  filter(cid_3d == "F10" & INICIO >= '2016-01-01' & idade <=70 ) %>% 
  group_by(SEXOPAC, idade) %>%
  summarise(atendimentos=sum(qtde)) %>% 
  ggplot(aes(x = idade, y = atendimentos, color = SEXOPAC)) + 
  geom_line(size=.8)+
  scale_color_manual(values= c("#FFA000", "#294577"))+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title = "Atendimentos por Transtornos Mentais e Comportamentais Devidos ao Uso de Álcool",
       subtitle = "Período: 01/01/2019 a 30/09/2021")+
  xlab("Idade")


dev.off()


png("./png/atendimentos_depressão.png", width = 1920, height = 1080)

dados_ps %>% 
  filter(cid_3d %in% c("F32", "F33") & INICIO >= '2019-01-01') %>% 
  group_by(SEXOPAC, IDADEPAC) %>%
  summarise(atandimentos=n()) %>% 
  ggplot(aes(x = IDADEPAC, y = atandimentos, color = SEXOPAC)) + 
  geom_line(size=.8)+
  scale_color_manual(values= c("#FFA000", "#294577"))+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title = "Atendimentos por  Episódios Depressivos e Transtorno Depressivo Recorrente",
       subtitle = "Período: 01/01/2019 a 30/09/2021")+
  xlab("Idade")

dev.off()



png("./png/atendimentos_ansiedade.png", width = 1920, height = 1080)

dados_ps %>% 
  filter(cid_3d %in% c("F40", "F41") & INICIO >= '2019-01-01') %>% 
  group_by(SEXOPAC, IDADEPAC) %>%
  summarise(atandimentos=n()) %>% 
  ggplot(aes(x = IDADEPAC, y = atandimentos, color = SEXOPAC)) + 
  geom_line(size=.8)+
  scale_color_manual(values= c("#FFA000", "#294577"))+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title = "Atendimentos por Transtornos Fóbico-ansiosos e Outros Transtornos Ansiosos",
       subtitle = "Período: 01/01/2019 a 30/09/2021")+
  xlab("Idade")


dev.off()
