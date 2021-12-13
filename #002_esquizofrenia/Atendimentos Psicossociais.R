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

# Frequencia de atendimento por cid_3d ####

cid_3d <- dados_ps %>% 
  group_by(descricao) %>% 
  summarise(atendimentos = sum(qtde)/sum(dados_ps$qtde)) %>% 
  arrange((atendimentos)) %>% 
  mutate(descricao=factor(descricao, levels = descricao)) %>% 
  top_n(15)

cid_3d %>%  ggplot(aes(x=descricao, y = atendimentos))+
  geom_segment(aes(xend=descricao, yend=0, color = "#F2D3CE"), size = 1)+
  geom_point(color = "#486185", size = 2)+
  coord_flip() +
  labs(title = "Proporção das 15 maiores causas de atendimento",
       subtitle = "Dados de 2018 a 2021 (até out)")+
  ylab("Proporção")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
  
ggsave("D:/OneDrive/Saúde Mental do Homem/Vídeos/002 - Atendimentos PS/proporcao_cid_3.png")



# atendimentos por sexo biológico ######



ps_cid3d_sexo <- dados_ps %>% 
  filter(INICIO >= '2018-01-01' & idade <=70 ) %>% 
  group_by(SEXOPAC) %>%
  summarise(atendimentos=sum(qtde) / sum(dados_ps$qtde[which(dados_ps$INICIO >= '2018-01-01' & dados_ps$idade <=70 )]))


ps_cid3d_sexo %>% 
  ggplot(aes(x = SEXOPAC, y = atendimentos, fill = SEXOPAC)) + 
  geom_col()+
  scale_fill_manual(values= c("#FFA000", "#294577"))+
  labs(title = "Atendimentos psicossociais",
       subtitle = "Período: 01/01/2018 a 31/10/2021")+
  theme_bw()+
  theme(legend.position = "none")+
  xlab("")+
  ylab("Proporção de atendimentos")

ggsave("D:/OneDrive/Saúde Mental do Homem/Vídeos/002 - Atendimentos PS/ps_sexo.png")

# atendimentos por cid_3d, idade e sexo biológico ######

cid_interesse <- unique(cid_3d$descricao[10:15]) # os 6 mais frequentes

ps_cid3d_sexo_idade <- dados_ps %>% 
  filter(INICIO >= '2018-01-01' & idade <=70 ) %>% 
  group_by(descricao, SEXOPAC, idade) %>%
  summarise(atendimentos=sum(qtde)) %>% 
  filter(descricao %in% cid_interesse)


ps_cid3d_sexo_idade %>%  
  mutate(descricao = factor(unique(descricao), levels = c("Esquizofrenia ",
                                                  "Episodios depressivos ",
                                                  "Transtornos mentais e comportamentais devidos ao uso de álcool ",
                                                  "Outras substancias psicoativas ",
                                                  "Transtornos ansiosos ",
                                                  "Transtorno mental não especificado em outra parte "))) %>% 
  ggplot(aes(x = idade, y = atendimentos, color = SEXOPAC)) + 
  geom_line(size=.8)+
  scale_color_manual(values= c("#FFA000", "#294577"))+
  scale_x_continuous(breaks = seq(0,70,8))+
  labs(title = "Atendimentos psicossociais",
       subtitle = "Período: 01/01/2018 a 31/10/2021")+
  xlab("Idade")+
  ylab("Atendimentos")+
  theme_bw()+
  facet_wrap(~descricao)+ 
  theme(legend.position = "none") 
 
  

  ggsave("D:/OneDrive/Saúde Mental do Homem/Vídeos/002 - Atendimentos PS/ps_cid3d_sexo_idade.png")


