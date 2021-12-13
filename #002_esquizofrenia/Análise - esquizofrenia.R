rm(list = ls())
load("D:/OneDrive/Projetos R/PS/rda/dados_ps_20211208.rda")

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


load("D:/OneDrive/Projetos R/dados_gerais/tabela_cid.rda")

dados_ps <- left_join(dados_ps, tabela_cid, by = c("cid_3d" = "codigo") )




ps_esquizofrenia <- dados_ps %>% filter(descricao == "Esquizofrenia ")

# F20 por sexo, idade e cor

ps_esquizofrenia %>% 
  group_by(SEXOPAC, idade, RACACOR) %>% 
  summarise(atendimentos = sum(qtde)) %>% 
  filter(idade >= 10 & idade <= 70) %>% 
  ggplot(aes(x = idade, y = atendimentos, color = SEXOPAC)) + 
  geom_line(size=.8)+
  scale_color_manual(values= c("#FFA000", "#294577"))+
  scale_x_continuous(breaks = seq(0,70,8))+
  labs(title = "Atendimentos psicossociais",
       subtitle = "Período: 01/01/2018 a 31/10/2021")+
  xlab("Idade")+
  ylab("Atendimentos")+
  theme_bw()+
  facet_wrap(~RACACOR)+ 
  theme(legend.position = "none") 



ggsave("D:/OneDrive/Saúde Mental do Homem/Vídeos/002 - Atendimentos PS/f20_cid3d_sexo_idade.png")



