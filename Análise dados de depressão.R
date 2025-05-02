# Pacotes necessários
library(readxl)
install.packages("irr")
library(irr) # para o cálculo do Kappa

# Carregar os dados
planilha <- read_excel("C:/Users/julin/Documents/TCC organizado/Planilha para análise dos dados.xlsx")  
head(planilha)

# Criar colunas categóricas: com/sem depressão
planilha$grupo_EPDS <- ifelse(planilha$EPDS >= 13, "Depressed group", "Non-depressed group")
planilha$grupo_PHQ9 <- ifelse(planilha$PHQ9 >= 10, "Depressed group", "Non-depressed group")
planilha$grupo_EPDSBASE <- ifelse(planilha$EPDSBASE >= 13, "Depressed group", "Non-depressed group")
head(planilha)

# Contar quantos indivíduos em cada grupo
cat("Distribuição dos grupos - EPDS:\n")
print(table(planilha$grupo_EPDS))

cat("\nDistribuição dos grupos - PHQ-9:\n")
print(table(planilha$grupo_PHQ9))

cat("\nDistribuição dos grupos - EPDSBASE:\n")
print(table(planilha$grupo_EPDSBASE))

## PHQ-9 e EPDS ##

# 1. Comparar concordância - Tabela de contingência
cat("\nTabela de concordância entre EPDS e PHQ-9:\n")
tabela_concordancia <- table(planilha$grupo_EPDS, planilha$grupo_PHQ9)
print(tabela_concordancia)

# 2. Proporção de concordância simples
concordantes <- sum(planilha$grupo_EPDS == planilha$grupo_PHQ9)
total <- nrow(planilha)
proporcao_concordancia <- concordantes / total
cat("\nProporção de concordância simples:", proporcao_concordancia, "\n")

# 3. Cálculo do índice Kappa (Cohen's Kappa)
kappa_result <- kappa2(data.frame(planilha$grupo_EPDS, planilha$grupo_PHQ9))
cat("\nÍndice Kappa:\n")
print(kappa_result)


## EPDSBASE e EPDS ##

# 1. Comparar concordância - Tabela de contingência
cat("\nTabela de concordância entre EPDS e EPDSBASE:\n")
tabela_concordancia <- table(planilha$grupo_EPDS, planilha$grupo_EPDSBASE)
print(tabela_concordancia)

# 2. Proporção de concordância simples
concordantes <- sum(planilha$grupo_EPDS == planilha$grupo_EPDSBASE)
total <- nrow(planilha)
proporcao_concordancia <- concordantes / total
cat("\nProporção de concordância simples:", proporcao_concordancia, "\n")

# 3. Cálculo do índice Kappa (Cohen's Kappa)
kappa_result <- kappa2(data.frame(planilha$grupo_EPDS, planilha$grupo_EPDSBASE))
cat("\nÍndice Kappa:\n")
print(kappa_result)

#Análises para saber se a diferença foi significativa e qual a diferença#
# 4. Criar nova variável com a diferença dos escores
planilha$dif_EPDS <- planilha$EPDS - planilha$EPDSBASE

# 5. Visualizar um resumo da diferença
summary(planilha$dif_EPDS)

#Wilcoxon para avaliar a significância#
# 6. Teste de normalidade (só pra confirmar)
shapiro.test(planilha$EPDS)
shapiro.test(planilha$EPDSBASE)

# 7. Teste de Wilcoxon para amostras pareadas
wilcox.test(planilha$EPDS, planilha$EPDSBASE, paired = TRUE)


