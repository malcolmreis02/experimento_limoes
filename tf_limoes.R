
##### Biblitotecas

library(readxl) # ler planilha em excel
library(dplyr) # manipulacao do banco
library(ggplot2) # graficos
library(stringr) # manipulacao de strings
library(RColorBrewer) # cores dos graficos
library(ggpubr) # para juntar graficos
library(qqplotr) # para fazer o qqplot e bandas de confianca

##### Limpeza do ambiente

set.seed(123456)
setwd("~/R/projetos/me623_pp/trabalho_final")
rm(list = ls())
options(scipen = 999)

#### Lendo o banco (ja esta em formato longo)

df_limoes <- read_xlsx("experimento_limoes.xlsx") %>%
  mutate(rolou = ifelse(rolou == 1, "Sim", "Nao")) %>%
  mutate_at(vars(rolou, temperatura), as.factor)

df_limoes %>% str(4)

#### Media dos tratamentos

df_limoes %>% 
  group_by(rolou, temperatura) %>%
  summarise(media = sum(volume_ml))

#### Análise gráfica

box_temp <- df_limoes %>%
  mutate(temp_str = str_c("Temperatura: ", temperatura)) %>%
  ggplot(aes(x = rolou, y = volume_ml, fill = temperatura)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ temp_str, scales = "free") +
  stat_summary(aes(group = temperatura), fun = mean, geom = "line", size = 1, color = "black") +
  stat_summary(aes(group = rolou), fun = mean, geom = "point", size = 3, color = "black", show.legend = FALSE) +
  labs(x = "Rolou", y = "Volume em ml") +
  scale_y_continuous(limits = c(47, 54)) +
  scale_fill_manual(values = c("green", "MediumSeaGreen", "darkgreen")) +
  theme_bw()

box_rolou <- df_limoes %>%
  mutate(rolou_str = str_c("Rolou: ", rolou)) %>%
  ggplot(aes(x = temperatura, y = volume_ml, fill = rolou)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ rolou_str, scales = "free") +
  stat_summary(aes(group = rolou), fun = mean, geom = "line", size = 1, color = "black") +
  stat_summary(aes(group = rolou), fun = mean, geom = "point", size = 3, color = "black", show.legend = FALSE) +
  labs(x = "Temperatura", y = "Volume em ml") +
  scale_y_continuous(limits = c(47, 54)) +
  scale_fill_manual(values = c("MediumSeaGreen", "darkgreen")) +
  theme_bw()

  
#### Analise de variancia

## Com interacao (nao rejeitou h0 para a interacao)
fit.int <- lm(volume_ml ~ temperatura*rolou, data = df_limoes)
anova(fit.int)


## Sem interacao (vamos usar esse)
fit.sint <- lm(volume_ml ~ temperatura + rolou, data = df_limoes)
anova(fit.sint)


#### Analise grafica de residuos

res <- data.frame(res = fit.sint$residuals, 
                  rolou = df_limoes$rolou, 
                  temperatura = df_limoes$temperatura, 
                  n = 1:length(fit.sint$residuals))

res_diag <- res %>%
  ggplot(aes(y = res, x = n)) +
  geom_point(col = "darkgreen", size = 2) +
  geom_hline(yintercept = 0, col = "MediumSeaGreen") +
  labs(x = "Ordem dos dados", y = "Resíduos", title = "Resíduos vs Ordem de Coleta dos Dados") +
  theme_bw()

res_qq <- res %>% 
  ggplot(aes(sample = res)) + 
  stat_qq(col = "MediumSeaGreen", size = 2) + 
  stat_qq_line(col = "darkgreen") +
  stat_qq_band(col = "darkgreen", alpha = 0.02, fill = "darkgreen") +
  labs(x = "Resíduos", y = "Quantis Teóricos", title = "Gráfico de Probabilidade Normal") +
  theme_bw()

res.rolou <- res %>%
  ggplot(aes(y = res, x = rolou)) +
  geom_point(col = "darkgreen", size = 2.5) +
  labs(x = "Rolou", y = "Resíduos", title = "Resíduos vs Se rolou o limão") +
  theme_bw()

res.temp <- res %>%
  ggplot(aes(y = res, x = temperatura)) +
  geom_point(col = "darkgreen", size = 2.5) +
  labs(x = "Temperatura", y = "Resíduos", title = "Resíduos vs Temperatura") +
  theme_bw()

ggarrange(res_diag, res_qq)
ggarrange(res.rolou, res.temp)




















