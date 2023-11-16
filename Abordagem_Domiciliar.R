
# abre as bibliotecas
library(readxl)
library(tidyverse)
library(stringr)
library(janitor)

# cria uma tabela com codigo e componente do deficit e inadequação
# o que é considerado déficit e o que é considerado inadequação?
tabela <- tibble("Codigo" = c("Déficit", "Inadequação", 
                              "D01", "D02", "D03", "D04", "D05", "D06", "D07", 
                              "I01", "I02", "I03", "I04", "I05", "I06", "I07", 
                              "I08", "I09"),
                 "Componente" = c("Total Déficit", "Total Inadequação", 
                                  "Domicílio Improvisado", "Material de Parede Inadequado", 
                                  "Densidade Excessiva em Apartamento ou Lote Condominial", 
                                  "Coabitação", "Aluguel com Inadequações", 
                                  "Moradia em Cômodo", "Famílias em Situação de Rua",
                                  "Densidade Excessiva", "Ausência de Banheiro", 
                                  "Material do Piso", "Água Canalizada (Urbano)", 
                                  "Esgotamento Sanitário", 
                                  "Abastecimento de Água por Rede Pública (Urbano)", 
                                  "Energia Elétrica", "Coleta de Lixo (Urbano)", 
                                  "Ônus Excessivo com Aluguel"))

# define as condições do condomínio simples
condominio_simples <- c("AP","ANDAR","PAVIMENTO","BL","TORRE","PREDIO","FUNDO",
                        "FUNDOS","FRENTE","FRT","CIMA","BAIXO","SUPERIOR","INFERIOR",
                        "TERREA","DIREITA","ESQUERDA","MEIO","EDICULA","PRINCIPAL",
                        "PORAO","BAR","GARAGEM","TRAILER","BARRACO","CDHU","BLOCO",
                        "TOR","TORR","APART","APT","APARTAMENTO","APTO","ED","EDIF",
                        "EDIFICIO")

# importa os dados
cadunico <- read_excel("~/Desktop/IC FAPESP/CadÚnico Diadema/CadUnico_familia.xlsx")

# agrupa por famílias
cadunico_familias <- cadunico[,c(1:66)] %>%
  distinct() %>%
  mutate(ID = as.character(`Codigo familiar`),
         `Familias novas` = "ID0")

# identifica familias em situação de rua ##!!DIADEMA NÃO FORNECEU ESSA INFO
# cadunico_D07 <- cadunico_familias %>%
 # filter(`Codigo familiar` %in% filter(cadunico, `Situacao de Rua` == 1)$`Codigo familiar`) %>%
 # group_by(`Codigo familiar`) %>%
 # summarize(D07 = "Inadequado") %>%
 # mutate(`Familias novas` = "ID7",
        # ID = paste0(`Codigo familiar`, " ", `Familias novas`))

# identifica famílias em coabitação
cadunico_coab1 <- cadunico_familias %>% 
  filter(`Quantidade de familias no domicilio` > 1) %>%
  select(`Codigo familiar`) %>%
  mutate(D04 = "Inadequado",
         `Familias novas` = "ID1",
         ID = paste0(`Codigo familiar`, " ", `Familias novas`))

cadunico_coab2 <- cadunico_familias %>% 
  filter(`Quantidade de familias no domicilio` > 2) %>%
  select(`Codigo familiar`) %>%
  mutate(D04 = "Inadequado",
         `Familias novas` = "ID2",
         ID = paste0(`Codigo familiar`, " ", `Familias novas`))

cadunico_coab3 <- cadunico_familias %>% 
  filter(`Quantidade de familias no domicilio` > 3) %>%
  select(`Codigo familiar`) %>%
  mutate(D04 = "Inadequado",
         `Familias novas` = "ID3",
         ID = paste0(`Codigo familiar`, " ", `Familias novas`))

# cria base final de familias
cadunico_familias <- bind_rows(cadunico_familias, #cadunico_D07, 
                               cadunico_coab1, 
                               cadunico_coab2, cadunico_coab3) %>%
                              mutate(ID = row_number())

# computa os componentes
cadunico_componentes <- cadunico_familias %>%
  
        # Déficit: domicílio improvisado (D01)
  mutate(D01 = case_when(`Especie do domicilio` == 
                           "particular improvisado" ~ "Inadequado",
                         `Especie do domicilio` %in% 
                           c("particular permanente", "coletivo") ~ "Adequado"),
         
         # Déficit: material de parede inadequado (D02)
         D02 = case_when(`Material predominante nas paredes externas do domicilio` 
                         %in% c("taipa nao revestida", "madeira aproveitada", 
                                "palha", "outro material") ~ "Inadequado",
                         `Material predominante nas paredes externas do domicilio` 
                         %in% c("alvenaria/tijolo sem revestimento", 
                                "alvenaria/tijolo com revestimento", 
                                "madeira aparelhada", "taipa revestida") ~ "Adequado"),
         
         # Déficit: densidade excessiva em apartamento ou lote condominial (D03)
         D03 = case_when((`Quantidade de pessoas no domicilio`/
                            `Comodo servindo como dormitorio do domicilio` >= 3) 
                         & (str_detect(`Complemento`, 
                            paste(condominio_simples, collapse = "|")) == TRUE)  ~ "Inadequado",
                         `Quantidade de pessoas no domicilio`/
                           `Comodo servindo como dormitorio do domicilio` >= 3 
                         & str_detect(`Complemento`, 
                            paste(condominio_simples, collapse = "|")) == FALSE ~ "Adequado"),
         
         # Déficit: coabitação (D04)
         D04 = case_when((`Quantidade de familias no domicilio` > 1) ~ "Inadequado",
                          `Quantidade de familias no domicilio` <= 1 ~ "Adequado"),
         
         # Déficit: aluguel com inadequações (D05)
         D05 = case_when((`Valor de despesas com aluguel` > 0) & 
                           (`Quantidade de pessoas no domicilio`/
                              `Comodo servindo como dormitorio do domicilio`
                            >= 3 | `Existencia de banheiro` == "nao" |
                              `Material predominante no piso do domicilio` %in% 
                              c("terra", "madeira aproveitada") |
                              (`Agua canalizada no domicilio` == "nao" &
                                 `Situacao do domicilio` == "urbana")) ~ "Inadequado",
                         `Valor de despesas com aluguel` > 0 & 
                           `Quantidade de pessoas no domicilio`/
                           `Comodo servindo como dormitorio do domicilio` < 3 & 
                           `Existencia de banheiro` == "sim" & 
                           `Material predominante no piso do domicilio` %in% 
                           c("ceramica lajota ou pedra", "cimento", "madeira aparelhada",
                             "carpete", "outro material") & 
                           (`Agua canalizada no domicilio` == "sim" & 
                              `Situacao do domicilio` == "urbana") ~ "Adequado"),
         
         # Déficit: moradia em cômodos (D06)
         D06 = case_when(`Quantidade de comodos do domicilio` <= 1 ~ "Inadequado",
                         `Quantidade de comodos do domicilio` > 1 ~ "Adequado"),
         
         # Déficit: famílias em situação de rua (D07)
         # D07 = if_else(`Código familiar` %in% cadunico_rua$`Código familiar`, "Inadequado", "Adequado"),
         
         # Inadequação: densidade excessiva (I01)
         I01 = case_when(`Quantidade de pessoas no domicilio`/
                           `Comodo servindo como dormitorio do domicilio` < 3 ~ "Adequado",
                         `Quantidade de pessoas no domicilio`/
                           `Comodo servindo como dormitorio do domicilio` >= 3 ~ "Inadequado"),
         
         # Inadequação: ausência de banheiro (I02)
         I02 = case_when(`Existencia de banheiro` == "sim" ~ "Adequado",
                         `Existencia de banheiro` == "nao" ~ "Inadequado"),
         
         # Inadequação: material do piso (I03)
         I03 = case_when(`Material predominante no piso do domicilio` %in% 
                           c("ceramica lajota ou pedra", "cimento", "madeira aparelhada", 
                             "carpete", "outro material") ~ "Adequado",
                         `Material predominante no piso do domicilio` %in% 
                           c("terra", "madeira aproveitada") ~ "Inadequado"),
         
         # Inadequação: água canalizada (urbano) (I04)
         I04 = case_when(`Agua canalizada no domicilio` == "sim" & `Situacao do domicilio` 
                         == "urbana" ~ "Adequado",
                         `Agua canalizada no domicilio` == "nao" & `Situacao do domicilio` 
                         == "urbana" ~ "Inadequado"),
         
         # Inadequação: esgotamento sanitário (I05)
         I05 = case_when(`Forma de escoamento sanitario` %in% 
                           c("rede coletora de esgoto ou pluvial", "fossa septica") ~ "Adequado",
                         `Forma de escoamento sanitario` %in% c("fossa rudimentar", 
                          "vala a ceu aberto", "direto para um rio lago ou mar", "outra forma") ~ "Inadequado"),
         
         # Inadequação: abastecimento de água por rede pública (urbano) (I06)
         I06 = case_when(`Forma de abastecimento de agua` == "rede geral de distribuicao" 
                         & `Situacao do domicilio` == "urbana" ~ "Adequado",
                         `Forma de abastecimento de agua` %in% 
                           c("poco ou nascente", "cisterna", "outra forma") 
                         & `Situacao do domicilio` == "urbana" ~ "Inadequado"),
         
         # Inadequação: energia elétrica (I07)
         I07 = case_when(`Tipo de iluminacao` %in% c("eletrica com medidor proprio", 
                          "eletrica com medidor comunitario") ~ "Adequado",
                         `Tipo de iluminacao` %in% c("eletrica sem medidor", "oleo querosene ou gas", 
                           "vela", "outra forma") ~ "Inadequado"),
         
         # Inadequação: coleta de lixo (urbano)
         I08 = case_when(`Forma de coleta do lixo` %in% c("coletado diretamente", "coletado indiretamente") 
                         & `Situacao do domicilio` == "urbana" ~ "Adequado",
                         `Forma de coleta do lixo` %in% c("queimado ou enterrado na propriedade", 
                         "jogado em terreno baldio ou logradouro", "jogado em rio ou mar", "outro destino") 
                         & `Situacao do domicilio` == "urbana" ~ "Inadequado"),
         
         # Inadequação: ônus excessivo com aluguel
         I09 = case_when(`Valor de despesas com aluguel`/`Valor da renda total da familia` < 0.3 ~ "Adequado",
                         `Valor de despesas com aluguel`/`Valor da renda total da familia` >= 0.3 ~ "Inadequado")) %>%
  
  select(ID, `Codigo familiar`, `Familias novas`, `Situacao do domicilio`, 
         D01, D02, D03, D04, D05, D06, #D07, 
         I01, I02, I03, I04, I05, I06, I07, I08, I09) %>%
  mutate_at(vars(-c(ID, `Codigo familiar`, `Familias novas`)), 
            ~replace(., is.na(.), "Sem Dados ou Não Aplicável")) %>%
  mutate(`Déficit` = if_else(D01 == "Inadequado" | D02 == "Inadequado" 
                             | D03 == "Inadequado" | D04 == "Inadequado" 
                             | D05 == "Inadequado" | D06 == "Inadequado", 
                             #| D07 == "Inadequado", 
                             "Inadequado", "Adequado"),
         `Inadequação` = if_else(I01 == "Inadequado" | I02 == "Inadequado" | 
                                   I03 == "Inadequado" | I04 == "Inadequado" | 
                                   I05 == "Inadequado" | I06 == "Inadequado" | 
                                   I07 == "Inadequado" | I08 == "Inadequado" | 
                                   I09 == "Inadequado", "Inadequado", "Adequado"),
         `Déficit ou Inadequação` = case_when(`Déficit` == "Inadequado" ~ "Déficit",
                                              `Déficit` == "Adequado" & 
                                                `Inadequação` == "Inadequado" ~ "Inadequação",
                                              `Déficit` == "Adequado" 
                                              & `Inadequação` == "Adequado" ~ "Adequado"))

# tabela sintese
tabela1 <- cadunico_componentes %>%
  #filter(D07 == "Inadequado") %>%
  pivot_longer(cols = -c(ID, `Codigo familiar`, `Familias novas`, `Situacao do domicilio`, `Déficit ou Inadequação`), 
               names_to = "Codigo", 
               values_to = "Condicao") %>%
  pivot_wider(names_from = "Condicao",
              values_from = "Condicao") %>%
  group_by(`Codigo`) %>%
  summarize(Adequado = sum(!is.na(Adequado)),
            Inadequado = sum(!is.na(Inadequado)),
            `Sem Dados ou Não Aplicável` = sum(!is.na(`Sem Dados ou Não Aplicável`))) %>%
  filter(Codigo %in% c("D01", "D02", "D03", "D04", "D05", "D06", #"D07", 
                       "Déficit")) %>%
  mutate("%" = Inadequado/6020) ## !!! sempre conferir esse valor

tabela1

tabela2 <- cadunico_componentes %>%
  filter(`Déficit ou Inadequação` != "Deficit") %>%
  pivot_longer(cols = -c(ID, `Codigo familiar`, `Familias novas`, `Codigo familiar`, 
                         `Situacao do domicilio`, `Déficit ou Inadequação`), 
               names_to = "Codigo", 
               values_to = "Condicao") %>%
  pivot_wider(names_from = "Condicao",
              values_from = "Condicao") %>%
  group_by(`Codigo`) %>%
  summarize(Adequado = sum(!is.na(Adequado)),
            Inadequado = sum(!is.na(Inadequado)),
            `Sem Dados ou Não Aplicável` = sum(!is.na(`Sem Dados ou Não Aplicável`))) %>%
  filter(Codigo %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "Inadequação")) %>%
  mutate(`Sem Dados ou Não Aplicável` = `Sem Dados ou Não Aplicável` + 6020, 
         "%" = Inadequado/22890) ## !!! sempre conferir esse valor

tabela2

tabela3 <- rbind(tabela1, tabela2) %>%
  left_join(tabela) %>%
  mutate("%" = round(`%`, 3)) %>%
  select("Codigo", "Componente", "Inadequado", "%", everything())

tabela3

write.csv2(cadunico_componentes, "resultados/dados1.csv", row.names = FALSE)

write.csv2(tabela3, "resultados/tabela_abordagem_domiciliar.csv", row.names = FALSE)
























