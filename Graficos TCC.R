# carregar pacotes e planilhas #
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)

planilha <- read_excel("C:/Users/julin/Documents/TCC organizado/Planilha para análise dos dados.xlsx")  
head(planilha)

## EPDS e dados não-paramétricos ##

# 1. criar formato para gerar os três gráficos lado a lado
par(mfrow=c(1,3))
# 2. gerar os gráficos
boxplot(L5 ~ (EPDS >= 13), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "L5",
        xlab = "",
        ylab = "Distribuição de L5",
        col = c("rosybrown1", "lightcoral"))
boxplot(M10 ~ (EPDS >= 13), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "M10",
        xlab = "",
        ylab = "Distribuição de M10",
        col = c("rosybrown1", "lightcoral"))
boxplot(RA ~ (EPDS >= 13), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "AR",
        xlab = "",
        ylab = "Distribuição de AR",
        col = c("rosybrown1", "lightcoral"))

## PHQ-9 e dados não-paramétricos ##

# 1. criar formato para gerar os três gráficos lado a lado
par(mfrow=c(1,3))
# 2. gerar os gráficos
boxplot(L5 ~ (PHQ9 >= 10), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "L5 ",
        xlab = "",
        ylab = "Distribuição de L5",
        col = c("rosybrown1", "lightcoral"))
boxplot(M10 ~ (PHQ9 >= 10), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "M10",
        xlab = "",
        ylab = "Distribuição de M10",
        col = c("rosybrown1", "lightcoral"))
boxplot(RA ~ (PHQ9 >= 10), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "AR",
        xlab = "",
        ylab = "Distribuição de AR",
        col = c("rosybrown1", "lightcoral"))


## comparação entre os scores de depressão ##

# Criar colunas com classificação de depressão (ponto de corte: 10 para todos)
planilha$grupo_EPDS <- ifelse(planilha$EPDS >= 13, "Com depressão", "Sem depressão")
planilha$grupo_PHQ9 <- ifelse(planilha$PHQ9 >= 10, "Com depressão", "Sem depressão")
planilha$grupo_EPDSBASE <- ifelse(planilha$EPDSBASE >= 13, "Com depressão", "Sem depressão")
head(planilha)

# EPDS e PHQ-9 #

# 1. Organizar os dados em formato longo 
dados_long <- data.frame(
  ID = rep(planilha$ID, times = 2),
  Teste = rep(c("EPDS", "PHQ-9"), each = nrow(planilha)),
  Grupo = c(planilha$grupo_EPDS, planilha$grupo_PHQ9)
)

# 2. Gráfico de barras lado a lado 
ggplot(dados_long, aes(x = Teste, fill = Grupo)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição dos grupos por teste (EPDS x PHQ-9)",
       y = "Número de indivíduos", x = "") +
  scale_fill_manual(values = c("lightcoral", "rosybrown1")) +
  theme_minimal()

# 3. tabelas de concordância e discordância
tabela_concordancia <- table(planilha$grupo_EPDS, planilha$grupo_PHQ9)
colnames(tabela_concordancia) <- c("PHQ-9: Com depressão", "PHQ-9: Sem depressão")
rownames(tabela_concordancia) <- c("EPDS: Com depressão", "EPDS: Sem depressão")
print(tabela_concordancia)

install.packages("gridExtra")
install.packages("grid")
library("gridExtra")
library("grid")

png("tabela_concordancia.png")
grid.table(tabela_concordancia,
           theme = ttheme_minimal(
             core = list(fg_params = list(col = "black")),  # Cor do texto
             row = list(background = "gray90"),  # Fundo cinza claro nas linhas
             col = list(background = "gray90")   # Fundo cinza claro nas colunas
           ))
dev.off()  

# EPDSBASE  e EPDS #
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# 1. organizar os dados em formato longo com os grupos de depressão
dados_evolucao <- data.frame(
  ID = rep(planilha$ID, times = 2),
  Tempo = rep(c("Linha de base", "6 meses"), each = nrow(planilha)),
  Grupo = c(planilha$grupo_EPDSBASE, planilha$grupo_EPDS)
)

# 2. garantir que os fatores tenham a ordem correta
dados_evolucao$Tempo <- factor(dados_evolucao$Tempo, levels = c("Linha de base", "6 meses"))
dados_evolucao$Grupo <- factor(dados_evolucao$Grupo, levels = c("Sem depressão", "Com depressão"))

# 3. gráfico de barras empilhadas em proporção
ggplot(dados_evolucao, aes(x = Tempo, fill = Grupo)) +
  geom_bar(position = "fill") +  # Mostra proporções (percentual)
  labs(title = "Proporção de grupos de depressão ao longo do tempo (EPDS)",
       y = "Proporção de indivíduos", x = "Momento da avaliação") +
  scale_fill_manual(values = c("rosybrown1", "lightcoral")) +
  theme_minimal()




## SRI Sadeh ##
par(mfrow=c(1,2))
# EPDS
boxplot(SRI_sadeh ~ (EPDS >= 13), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "EPDS",
        xlab = "",
        ylab = "Distribuição de SRI",
        col = c("rosybrown1", "lightcoral"))

# PHQ-9
boxplot(SRI_sadeh ~ (PHQ9 >= 10), data = planilha,
        names = c("Grupo sem depressão", "Grupo com depressão"),
        main = "PHQ-9",
        xlab = "",
        ylab = "Distribuição de SRI",
        col = c("rosybrown1", "lightcoral"))

