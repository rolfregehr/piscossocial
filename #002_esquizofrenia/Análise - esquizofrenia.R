rm(list = ls())
load("D:/OneDrive/Projetos R/PS/rda/dados_ps_20211220.rda")

library(lubridate)
library(tidyverse)

# Ajustes ####
dados_ps$cid_3d <- str_sub(dados_ps$CIDPRI, 1, 3)
dados_ps$cid_1d <- str_sub(dados_ps$CIDPRI, 1, 1)


#Recodificação de RACA/COR #####
unique(dados_ps$RACACOR)
dados_ps$RACACOR <- recode(dados_ps$RACACOR, "01" = "Branca", "02" = "Preta", "03" = "Parda", "04" = "Amarela", "05" = "Indígena", "99" = "NI")
unique(dados_ps$RACACOR)

#Ajuste das datas####
dados_ps$INICIO <- ymd(dados_ps$INICIO)

# Cálculo da idade ####
dados_ps$IDADEPAC <- as.numeric(dados_ps$IDADEPAC)
dados_ps$idade <- if_else(dados_ps$TPIDADEPAC == "4",
                          dados_ps$IDADEPAC,
                          if_else(dados_ps$TPIDADEPAC == "5",
                                  100+dados_ps$IDADEPAC, 0))


load("D:/OneDrive/Projetos R/dados_gerais/tabela_cid.rda")

dados_ps <- left_join(dados_ps, tabela_cid, by = c("cid_3d" = "codigo") )




ps_esquizofrenia <- dados_ps %>% filter(descricao == "Esquizofrenia ")

# F20 por sexo, idade e cor####

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





# por pac ####

# cria DF com CNS, idade e sexo
cns <-  data.frame(cns = unique(ps_esquizofrenia$CNS_PAC)) # cria o DF com informações de CNS

ps_esquizofrenia_pac <- ps_esquizofrenia[, c(2, 5, 11)]

cns_pac <- left_join(cns, ps_esquizofrenia_pac, by = c("cns" = "CNS_PAC")) %>% 
  distinct(cns, SEXOPAC, idade)

cns_pac <- cns_pac %>% group_by(cns, SEXOPAC) %>% summarise(idade = min(idade)) %>% 
  distinct(cns, SEXOPAC, idade)


cns_pac %>% 
  group_by(SEXOPAC, idade) %>% 
  summarise(n = n()) %>% 
  filter(idade >= 10 & idade <= 70) %>% 
  ggplot(aes(x = idade, y = n, color = SEXOPAC)) + 
  geom_line(size=.8)+
  scale_color_manual(values= c("#FFA000", "#294577"))+
  scale_x_continuous(breaks = seq(0,70,2))+
  labs(title = "Quantidade de pessoas atendidas",
       subtitle = "Período: 01/01/2018 a 31/10/2021")+
  xlab("Idade")+
  ylab("Número de pessoas")+
  theme_bw()

ggsave("D:/OneDrive/Saúde Mental do Homem/Vídeos/002 - Atendimentos PS/pessoas_sexo_idade.png")


cns_pac %>% 
  filter(idade >= 10 & idade <= 70) %>% 
  group_by(SEXOPAC) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = SEXOPAC, y = n, fill = SEXOPAC, label = formatC(n, format="f", big.mark = ".", digits=0)))+  
  geom_bar(stat = "identity")+
  geom_text(size = 5, position = position_stack(vjust = 0.1), color = c("black", "white"))+
  scale_fill_manual(values= c("#FFA000", "#294577"))+
  labs(title = paste("Quantidade de pessoas atendidas"),
       subtitle = "Período: 01/01/2018 a 31/10/2021")+
  xlab("Idade")+
  ylab("Número de pessoas")+
  theme_bw()

ggsave("D:/OneDrive/Saúde Mental do Homem/Vídeos/002 - Atendimentos PS/pessoas_sexo.png")


