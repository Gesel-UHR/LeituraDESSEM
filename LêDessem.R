library(tidyverse)
ArqRampa <- "rampas.dat"
ArqSaídaMédia <- "SaiMed.csv"
ArqSaídaDetal <- "SaiDet.csv"
  
# Funções -----------------------------------------------------------------
LêRampas <- function(ArqRampa) {
  #Larguras e nomes das colunas 
  widths <- list(begin = c(1L, 5L, 9L, 14L, 18L, 21L, 32L, 38L) - 1,
                 # Pelo manual, o campo Potência é só até a coluna 30, mas tem usina que está usando a 31.
                 end = c(3L, 7L, 11L, 14L, 18L, 31L, 36L, 38L),
                 col_names = c("Número", "Unidade", "Seg", "C", "T", "Potência", "Hora", "Meia-hora"))
  # Carrega o arquivo
  lin <- read_lines(ArqRampa)
  # # Pega os nomes das Usinas
  InicUsina <- c(1, which(c(lin == "&", FALSE) & c(FALSE, lin == "&")))
  IdUsinas <- lin[InicUsina + 1] %>% str_split("-", simplify = TRUE) %>% as_tibble(.name_repair = "unique")
  IdUsinas <- bind_cols(InicUsina + 1, IdUsinas)
  colnames(IdUsinas) <- c("LinhaI", "Número", "Nome")
  IdUsinas <- IdUsinas %>% filter(Nome != "") %>% separate(Número, into = c(NA, "Número")) %>% 
    mutate(Número = as.integer(Número)) %>% mutate(LinhaF = lead(LinhaI) - 1, .after = LinhaI) 
  IdUsinas[nrow(IdUsinas), "LinhaF"] <- length(lin)
  # Carrega os dados
  # Retira a primeira linha (RAMP) e as linhas que começam com "&"
  Rampas <- read_fwf(I(lin %>% str_subset("^&", negate = TRUE) %>% str_subset("RAMP", negate = TRUE)), 
                     col_positions = widths, col_types = "iiiccdii") %>% drop_na(Número)
  # Preenche valores vazios na coluna Meia-hora.
  Rampas <- Rampas %>% replace_na(list(`Meia-hora` = 0))
  # Adiciona linhas com a hora 0.
  Rampas <- bind_rows(Rampas, 
                      Rampas %>% distinct(Número, Unidade, C) %>% 
                        mutate(T = "A", Potência = 0, Hora = 0, `Meia-hora` = 0), 
                      Rampas %>% distinct(Número, Unidade, C) %>% 
                        mutate(T = "D", Hora = 0, `Meia-hora` = 0)
  ) %>% left_join(Rampas %>% group_by(Número, Unidade, C) %>% 
                    summarise(PotMax = max(Potência))) %>% 
    mutate(Potência = coalesce(Potência, PotMax)) %>% select(-PotMax) %>% 
    arrange(Número, Unidade, T, Hora, `Meia-hora`)
  
  Rampas <- Rampas %>% group_by(Número, Unidade, C, T) %>% 
    mutate(Tempo = Hora + `Meia-hora` * 0.5, Rampa = (lead(Potência) - Potência) / (lead(Tempo) - Tempo),
           RampaMédia = (last(Potência) - first(Potência)) / (last(Tempo) - first(Tempo)))
  # Junta tabelas
  left_join(IdUsinas %>% select(Número, Nome), Rampas %>% select(!any_of(c("Hora", "Meia-hora"))))
}

# Rotina principal ----------------------------------------------------------------
Rampas <- LêRampas(ArqRampa)

# Valor único por usina
ValMédios <- Rampas %>% group_by(Número, Nome, Unidade, T) %>% 
  summarise(RampaMédia = abs(mean(RampaMédia) / 60)) %>% # Consolida um valor por unidade 
  group_by(Número, Nome, T) %>% summarise(RampaMédia = round(sum(RampaMédia), 4)) %>% # SOma da rampa de cada unidade
  ungroup() %>% select(Número, Nome, T, RampaMédia) %>% 
  mutate(T = case_when(T == "A" ~ "Max Ramp Up",
                       T == "D" ~ "Max Ramp Down")) %>% 
  pivot_wider(c("Número", "Nome"), names_from = "T", values_from = "RampaMédia")
write_csv2(ValMédios, ArqSaídaMédia)

ValDetalhados <- Rampas %>% group_by(Número, Nome, T, Tempo) %>% drop_na(Rampa) %>% 
  summarise(Potência = sum(Potência), Rampa = sum(Rampa)) %>% # Agregado de todas as unidades
  mutate(Rampa = abs(Rampa / 60)) %>%  # Valores sempre positivos. Divididos por 60 para converter de hora para minuto.
  select(Número, Nome, T, Potência, Rampa) %>% 
  mutate(Rampa = round(Rampa, 4)) %>% 
  group_by(Nome, T) %>% mutate(Band = row_number()) %>%
  filter(Rampa != 0) %>% # A rampa não pode ser 0. 
  pivot_longer(c(Potência, Rampa)) %>% 
  mutate(T = case_when(T == "A" ~ "Up",
                       T == "D" ~ "Down"),
         name = case_when(name == "Potência" ~ paste0("Ramp ", T, " Point"),
                          name == "Rampa" ~ paste0("Max Ramp ", T))) %>% 
  ungroup() %>% select(-T)
write_csv2(ValDetalhados %>% mutate(value = as.character(value) %>% str_replace("\\.", ",")), ArqSaídaDetal)


