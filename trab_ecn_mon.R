#bibliotecas utilizadas
library(knitr)
library(dplyr)
library(tidyquant)
library(readxl)
library(ggplot2)
library(scales)
library(BETS)

#inputs iniciais
first_date <- "2019-01-01"
last_date <- "2021-06-01"

#puxando dados da Selic Meta definida pelo Copom
selic <- BETSget(432, data.frame = T) %>%
  filter(date >= first_date)

#plot Selic
graf_selic <- selic %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value), color = "darkblue", size = 1) +
  geom_line(aes(y = tail(value, 1)), linetype = 2, color = "red") +
  scale_x_date(breaks = "3 months",
               labels = scales::date_format("%Y/%m")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) +
  labs(title = "Meta da taxa Selic definida pelo Copom (% a.a.)",
       x = element_blank(), 
       y = element_blank(), 
       caption = "Fonte: Banco Central do Brasil. Elaboração dos Autores.") + 
  theme_light() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#dados diversos do ipca obtidos da serie 11427 do BCB, em formato .csv
dados_ipca <- read.csv("dados_ipca.csv", sep = ",")

#dados para elaboração do gráfico 2
subdados1 <- dados_ipca %>%
  select(c(data, IPCA.var....a, lim_sup, lim_inf, meta))
colnames(subdados1) <- c("data", "ipca", "lim_sup", "lim_inf", "meta")

#dados para elaboração do gráfico 3
subdados2 <- dados_ipca %>%
  select(c(data, IPCA.var....a, ipca_EX0.var....a, lim_sup, lim_inf))
colnames(subdados2) <- c("data", "ipca", "ipca_ex0","lim_sup", "lim_inf")

#plot ipca1
graf_ipca1 <- subdados1 %>%
  ggplot(aes(x = as.Date(data), group = 1)) +
  geom_line(aes(y = ipca, colour = "IPCA"), size = 0.8) +
  geom_line(aes(y = lim_sup, colour = "Limite superior"), linetype = 2, size = 0.8) +
  geom_line(aes(y = lim_inf, colour = "Limite inferior"), linetype = 2, size = 0.8) +
  geom_line(aes(y = meta, colour = "Meta"), size = 0.8) +
  scale_color_manual(name = element_blank(),
                     values = c("IPCA" = "darkblue", 
                                "Limite superior" = "red",
                                "Limite inferior" = "red",
                                "Meta" = "cyan")) +
  scale_x_date(breaks = "3 months",
               labels = scales::date_format("%Y/%m")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) +
  labs(title = "IPCA, limites e meta (% a.a.)",
       x = element_blank(), 
       y = element_blank(), 
       caption = "Fonte: Banco Central do Brasil. Elaboração dos Autores.") + 
  theme_light() + theme(legend.position = "bottom",
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#plot ipca2
graf_ipca2 <- subdados2 %>%
  ggplot(aes(x = as.Date(data), group = 1)) +
  geom_line(aes(y = ipca, colour = "IPCA"), size = 0.8) +
  geom_line(aes(y = lim_sup, colour = "Limite superior"), linetype = 2, size = 0.8) +
  geom_line(aes(y = lim_inf, colour = "Limite inferior"), linetype = 2, size = 0.8) +
  geom_line(aes(y = ipca_ex0, colour = "IPCA_EX0"), size = 0.8) +
  scale_color_manual(name = element_blank(),
                     values = c("IPCA" = "darkblue", 
                                "Limite superior" = "red",
                                "Limite inferior" = "red",
                                "IPCA_EX0" = "cyan")) +
  scale_x_date(breaks = "3 months",
               labels = scales::date_format("%Y/%m")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) +
  labs(title = "IPCA, IPCA_EX0 e limites (% a.a.)",
       x = element_blank(), 
       y = element_blank(), 
       caption = "Fonte: Banco Central do Brasil. Elaboração dos Autores.") + 
  theme_light() + theme(legend.position = "bottom",
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#dados do hiato do pib obtidos no IFI
url <- 'https://www12.senado.leg.br/ifi/dados/arquivos/estimativas-do-hiato-do-produto-ifi/at_download/file'
download.file(url, destfile = "hiato_pib.xlsx", mode = "wb")

#lendo o arquivo excel
hiato_pib <- read_excel("hiato_pib.xlsx", sheet = 2, skip = 1)
colnames(hiato_pib) <- c("trim", "lim_inf", "hiato", "lim_sup")

#plot hiato pib
graf_hp <- hiato_pib %>%
  ggplot(aes(x = as.Date(trim))) +
  geom_line(aes(y = hiato), color = "darkblue", size = 1) +
  geom_line(y = 0, linetype = 2, color = "red") +
  scale_x_date(breaks = "1 year",
               labels = scales::date_format("%Y")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) +
  labs(title = "Hiato do Produto Brasileiro",
       x = element_blank(), 
       y = "Variação percentual (a.t.)", 
       caption = "Fonte: Instituto Fiscal Independente. Elaboração dos Autores.") + 
  theme_light() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#dados IBC-Br
ibc_br <- BETSget(24363, data.frame = T) %>%
  filter(date >= first_date) %>%
  rename(ibc_br = value)

#plot IBC-Br
graf_ibcbr <- ibc_br %>%
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = ibc_br), color = "darkblue", size = 1) +
  geom_line(y = mean(ibc_br$ibc_br), linetype = 2, color = "red") +
  scale_x_date(breaks = "3 months",
               labels = scales::date_format("%Y/%m")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) +
  labs(title = "Índice de Atividade Econômica do Banco Central",
       x = element_blank(), 
       y = element_blank(), 
       caption = "Fonte: Banco Central do Brasil. Elaboração dos Autores.") + 
  theme_light() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#dados taxa de desocupacao
tx_desemp <- BETSget(24369, data.frame = T) %>%
  filter(date >= first_date) %>%
  rename(tx_desemp = value)

#plot tx de desemprego
graf_txdesemp <- tx_desemp %>%
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = tx_desemp), color = "darkblue", size = 1) +
  geom_line(y = mean(tx_desemp$tx_desemp), linetype = 2, color = "red") +
  scale_x_date(breaks = "2 months",
               labels = scales::date_format("%Y/%m")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) +
  labs(title = "Taxa de Desemprego",
       x = element_blank(), 
       y = "%", 
       caption = "Fonte: Banco Central do Brasil. Elaboração dos Autores.") + 
  theme_light() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))