
setwd("~/Desktop/Diadema - QGIS")

# abre as bibliotecas
library(readxl)
library(tidyverse)
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)

# importa os dados
territorial <- read_sf("Assentamentos Precários (união 2016 e 2021).gpkg") %>% st_transform(crs = 4326)
domiciliar <- read_sf("Abordagem Domiciliar (com id assent prec).gpkg") %>% st_transform(crs = 4326)

# tabela territorial 
tabela_territorial <- territorial %>%
  st_drop_geometry() %>%
  mutate(Deficit = Estimativa.Domiclios.Removidos,
         Inadequacao = DOMICILIOS - Deficit) %>%
  summarize(Deficit = sum(Deficit),
            Inadequacao = sum(Inadequacao))

# domiciliar dentro (calculado no QGIS)
domiciliar_dentro <- filter(domiciliar, Dentro.ou.Fora == "Dentro") %>%
#domiciliar_dentro <- st_intersection(domiciliar, territorial) %>%
  #mutate("Dentro ou fora" = "Dentro") #%>%
  rename("Código Familiar" = "codfamiliarfam",
         "Famílias Novas" = "Familias.Novas",
         "Estimativa Domicílios Removidos" = "Estimativa.Domiclios.Removidos",
         "Dentro ou Fora" = "Dentro.ou.Fora",
         #"Urbana/rural" = "Urbana.rural",
         "Déficit ou Inadequação" = "Déficit.ou.Inadequação",
         "Porcentagem Remoção" = "Porcentagem.Remocao",
         "Endereço" = "Address.and.city")

# domiciliar fora
domiciliar_fora <- domiciliar %>%
  filter(!`codfamiliarfam` %in% domiciliar_dentro$`Código Familiar`) %>%
  mutate("Dentro ou Fora" = "Fora") %>%
  mutate(Cascata = case_when(`Déficit.ou.Inadequação` == "Déficit" ~ "1 - Provisão de Moradias",
                             (I01 == "Inadequado" | I02 == "Inadequado" | I03 == "Inadequado" | I04 == "Inadequado") ~ "2 - Melhorias Habitacionais",
                             (I01 != "Inadequado" | I02 != "Inadequado" | I03 != "Inadequado" | I04 != "Inadequado") & (I09 == "Inadequado")  ~ "3 - Políticas de Apoio à Locação",
                             (I05 == "Inadequado" | I06 == "Inadequado" | I07 == "Inadequado" | I08 == "Inadequado") ~ "4 - Melhorias Urbanas"))

write.csv2(domiciliar_fora, "domiciliar_fora.csv", row.names = FALSE)

# agrupa domiciliar por assentamento precário
grupos <- domiciliar_dentro %>%
  st_drop_geometry() %>%
  group_by(COD_AP_DHR, `Déficit ou Inadequação`) %>%
  tally() %>%
  pivot_wider(id_cols = COD_AP_DHR, names_from = `Déficit ou Inadequação`, values_from = n)

# consolida grupos por assentamento precário (domiciliar e territorial)
territorial_grupos <- territorial %>%
  left_join(grupos) %>%
  replace(is.na(.), 0) %>%
  mutate(Domicilios_territorial = DOMICILIOS,
         Domicilios_domiciliar = Adequado + Déficit + Inadequação,
         Deficit_territorial = Estimativa.Domiclios.Removidos,
         Deficit_domiciliar = Déficit,
         D0 = pmax(Deficit_domiciliar, Deficit_territorial),
         I0 = pmax(Domicilios_territorial, Domicilios_domiciliar) - D0)

# tabela hibrida
tabela_hibrida <- tibble(Componente = c("Déficit Híbrido", "Inadequação Híbrido"),
                         Dentro = c(sum(territorial_grupos$D0), sum(territorial_grupos$I0)),
                         Fora = c(as.numeric(table(domiciliar_fora$`Déficit.ou.Inadequação`)[2]), as.numeric(table(domiciliar_fora$`Déficit.ou.Inadequação`)[3]))) %>%
  mutate(Total = Dentro + Fora)

# exporta dados
write.csv2(tabela_territorial,"tabela_abordagem_territorial.csv", row.names = FALSE)
write.csv2 (tabela_hibrida, "tabela_abordagem_hibrida.csv", row.names = FALSE)

### TABELAS FINAIS

df <- bind_rows(domiciliar_dentro, domiciliar_fora)

tabelao <- tibble("Código" = c("Déficit",  
                               "D00", "D01", "D02", "D03", "D04", "D05", "D06", "D07", 
                               "Inadequação",
                               "I00", "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                  "Componente" = c("Total Déficit", 
                                   "Domicílios em assentamentos precários com previsão de remoção", "Domicílio Improvisado", "Material de Parede Inadequado", "Densidade Excessiva em Apartamento ou Lote Condominial", "Coabitação", "Aluguel com Inadequações", "Moradia em Cômodo", "Famílias em Situação de Rua",
                                   "Total Inadequação", 
                                   "Domicílios em assentamentos precários sem previsão de remoção", "Densidade Excessiva", "Ausência de Banheiro", "Material do Piso", "Água Canalizada (Urbano)", "Esgotamento Sanitário", "Abastecimento de Água por Rede Pública (Urbano)", "Energia Elétrica", "Coleta de Lixo (Urbano)", "Ônus Excessivo com Aluguel"))

tabela <- tibble("Código" = c("Déficit", "Inadequação", 
                              "D01", "D02", "D03", "D04", "D05", "D06", "D07", 
                              "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                 "Componente" = c("Total Déficit", "Total Inadequação", 
                                  "Domicílio Improvisado", "Material de Parede Inadequado", "Densidade Excessiva em Apartamento ou Lote Condominial", "Coabitação", "Aluguel com Inadequações", "Moradia em Cômodo", "Famílias em Situação de Rua",
                                  "Densidade Excessiva", "Ausência de Banheiro", "Material do Piso", "Água Canalizada (Urbano)", "Esgotamento Sanitário", "Abastecimento de Água por Rede Pública (Urbano)", "Energia Elétrica", "Coleta de Lixo (Urbano)", "Ônus Excessivo com Aluguel"))


#DENTRO

tabelao_dentro_1 <- domiciliar_dentro%>%
  st_drop_geometry() %>%
  pivot_longer(cols = -c(`Código Familiar`, `Famílias Novas`, `Déficit ou Inadequação`, COD_AP_DHR, DOMICILIOS, `Estimativa Domicílios Removidos`, `Dentro ou Fora`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07")) %>%
  add_row(Código = "D00",
          Inadequado = territorial_grupos$Deficit_territorial) %>%
  add_row(Código = "Déficit",
          Inadequado = sum(territorial_grupos$D0)) %>%
  mutate("%" = Inadequado/sum(territorial_grupos$D0))



# alternativa #############################

domiciliar_dentro <- select(domiciliar_dentro, -Endereço)

library(dplyr)
library(tidyr)

tabelao_dentro_1 <- domiciliar_dentro %>%
  #st_drop_geometry() %>%
  gather(key = "Código", value = "Condição", -`Código Familiar`, -`Famílias Novas`, -`Déficit ou Inadequação`, -COD_AP_DHR, -DOMICILIOS, -`Estimativa Domicílios Removidos`, -`Dentro ou Fora`) %>%
  spread(Condição, Condição) %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07")) %>%
  add_row(Código = "D00",
          Inadequado = territorial_grupos$Deficit_territorial) %>%
  add_row(Código = "Déficit",
          Inadequado = sum(territorial_grupos$D0)) %>%
  mutate("%" = Inadequado / sum(territorial_grupos$D0))

library(dplyr)
library(data.table)

# Converter o dataframe em data.table
domiciliar_dentro_dt <- as.data.table(domiciliar_dentro)

tabelao_dentro_1 <- domiciliar_dentro_dt %>%
  melt(id.vars = c("Código Familiar", "Famílias Novas", "Déficit ou Inadequação", "COD_AP_DHR", "DOMICILIOS", "Estimativa Domicílios Removidos", "Dentro ou Fora"),
       variable.name = "Código",
       value.name = "Condição") %>%
  mutate(index = rowid(`Código Familiar`, `Famílias Novas`, `Déficit ou Inadequação`, COD_AP_DHR, DOMICILIOS, `Estimativa Domicílios Removidos`, `Dentro ou Fora`, `Código`)) %>%
  dcast(index + Código ~ Condição) %>%
  group_by(Código) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07")) %>%
  add_row(Código = "D00",
          Inadequado = territorial_grupos$Deficit_territorial) %>%
  add_row(Código = "Déficit",
          Inadequado = sum(territorial_grupos$D0)) %>%
  mutate("%" = Inadequado / sum(territorial_grupos$D0))




tabelao_dentro_2 <- domiciliar_dentro %>%
  st_drop_geometry() %>%
  filter(`Déficit ou Inadequação` != "Déficit") %>%
  pivot_longer(cols = -c(`Código familiar`, `Famílias novas`, `Déficit ou Inadequação`, COD_AP_DHR, DOMICILIOS, `Estimativa Domicilios Removidos`, `Dentro ou Fora`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09")) %>%
  add_row(Código = "I00",
          Inadequado = territorial_grupos$Domicilios_territorial) %>%
  add_row(Código = "Inadequação",
          Inadequado = sum(territorial_grupos$I0)) %>%
  mutate("%" = Inadequado/sum(territorial_grupos$I0))

tabelao_dentro_3 <- rbind(tabelao_dentro_1, tabelao_dentro_2) %>%
  left_join(tabelao) %>%
  mutate("%" = round(`%`, 3)) %>%
  select("Código", "Componente", "Inadequado", "%", everything())
tabelao_dentro_3

#FORA
tabela1 <- domiciliar_fora %>%
  st_drop_geometry() %>%
  #filter(D07 == "Inadequado") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07", "Déficit")) %>%
  mutate("%" = Inadequado/2404) ## !!! sempre conferir esse valor


tabela1

tabela2 <- domiciliar_fora %>%
  st_drop_geometry() %>%
  filter(`Déficit ou Inadequação` != "Déficit") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Código familiar`, `Urbana/rural`, `Déficit ou Inadequação`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "Inadequação")) %>%
  mutate("%" = Inadequado/4629) ## !!! sempre conferir esse valor

tabela2

tabela3 <- rbind(tabela1, tabela2) %>%
  left_join(tabela) %>%
  mutate("%" = round(`%`, 3)) %>%
  select("Código", "Componente", "Inadequado", "%", everything())
tabela3

# exporta os resultados
write.csv2(tabelao_dentro_3, "resultados/deficit_dentro.csv", row.names = FALSE)
write.csv2(tabela3, "resultados/deficit_fora.csv", row.names = FALSE)


#DEFICIT POR NUCLEO
deficit <- function(nuc){
  tg <- territorial_grupos %>% filter(NUCLEO == nuc)
  dd <- domiciliar_dentro %>% filter(NUCLEO == nuc)
  
  if (nrow(dd) > 0 & "Inadequação" %in% names(table(dd$`Déficit ou Inadequação`))) {
    tb1 <- dd %>%
      st_drop_geometry() %>%
      pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`, NUCLEO, Domicilios, Remocao, `Dentro ou fora`), 
                   names_to = "Código", 
                   values_to = "Condição") %>%
      pivot_wider(names_from = "Condição",
                  values_from = "Condição") %>%
      group_by(`Código`) %>%
      summarize(Inadequado = sum(!is.na(Inadequado)))
  } else {
    tb1 <- tibble(Código = c("D01", "D02", "D03", "D04", "D05", "D06", "D07"),
                  Inadequado = c(0, 0, 0, 0, 0, 0, 0))
  }
  
  tb1 <- tb1 %>%
    filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07")) %>%
    add_row(Código = "D00",
            Inadequado = tg$Deficit_territorial) %>%
    add_row(Código = "Déficit",
            Inadequado = sum(tg$D0))
  
  if (nrow(dd) > 0 & "Inadequação" %in% names(table(dd$`Déficit ou Inadequação`))) {
    tb2 <- dd %>%
      st_drop_geometry() %>%
      filter(`Déficit ou Inadequação` != "Déficit") %>%
      pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`, NUCLEO, Domicilios, Remocao, `Dentro ou fora`), 
                   names_to = "Código", 
                   values_to = "Condição") %>%
      pivot_wider(names_from = "Condição",
                  values_from = "Condição") %>%
      group_by(`Código`) %>%
      summarize(Inadequado = sum(!is.na(Inadequado)))
  } else {
    tb2 <- tibble(Código = c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                  Inadequado = c(0, 0, 0, 0, 0, 0, 0, 0, 0))
  }
  
  tb2 <- tb2 %>%
    filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09")) %>%
    add_row(Código = "I00",
            Inadequado = tg$Domicilios_territorial - filter(tb1, Código == "Déficit")$Inadequado) %>%
    add_row(Código = "Inadequação",
            Inadequado = sum(tg$I0))
  
  tb3 <- rbind(tb1, tb2) %>%
    left_join(tabelao) %>%
    select("Código", "Componente", "Inadequado", everything()) %>%
    replace(is.na(.), 0)
  tb3
  
  write.csv(tb3, paste0("resultados/deficit_nucleos/", nuc, ".csv"), row.names = FALSE)
}

lapply(territorial$NUCLEO, deficit)

deficit("Veraneio Ijal")







