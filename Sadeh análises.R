# Pacotes necessários
library(readxl)

# Carregar planilha dos dados 
dados_SRI <- read_excel("C:/Users/julin/Documents/TCC organizado/Planilha para análise dos dados.xlsx")  

# Verificar os nomes das colunas
print(colnames(dados_SRI))


## PHQ-9 ##
# 1. Converter os dados para numérico, substituindo vírgulas por pontos
dados_SRI$SRI_sadeh <- as.numeric(dados_SRI$SRI_sadeh)
head(dados_SRI)

# 2. Criar os grupos com base no PHQ-9
grupo_sem_depressao <- dados_SRI$SRI_sadeh[dados_SRI$PHQ9 < 10]  # PHQ-9 < 10 indica menor probabilidade de depressão
grupo_com_depressao <- dados_SRI$SRI_sadeh[dados_SRI$PHQ9 >= 10] # PHQ-9 >= 10 indica maior probabilidade de depressão

# 3. Teste de normalidade de Shapiro-Wilk
shapiro_sem_dep <- shapiro.test(grupo_sem_depressao)
shapiro_com_dep <- shapiro.test(grupo_com_depressao)

# 4. Resultados do teste de normalidade
print(shapiro_sem_dep)
print(shapiro_com_dep)

# 5. Teste de Mann-Whitney (Wilcoxon Rank-Sum Test) para comparar os grupos
mann_whitney <- wilcox.test(grupo_sem_depressao, grupo_com_depressao)

# 6. Resultado do teste de Mann-Whitney
print(mann_whitney)


## EPDS ##
# 1. Converter os dados para numérico, substituindo vírgulas por pontos
dados_SRI$SRI_sadeh <- as.numeric(dados_SRI$SRI_sadeh)

# 2. Criar grupos com base no score EPDS
grupo_sem_depressao1 <- dados_SRI$SRI_sadeh[dados_SRI$EPDS <= 13]
grupo_com_depressao1 <- dados_SRI$SRI_sadeh[dados_SRI$EPDS > 13]

# 3. Teste de normalidade de Shapiro-Wilk
shapiro_sem_dep <- shapiro.test(grupo_sem_depressao1)
shapiro_com_dep <- shapiro.test(grupo_com_depressao1)

# 4. Resultados do teste de normalidade
print(shapiro_sem_dep)
print(shapiro_com_dep)

# 5.  Teste de Levene
install.packages("car")
library(car)
dados_SRI$grupo <- ifelse(dados_SRI$EPDS < 13, "Sem_Depressao", "Com_Depressao")
dados_SRI$grupo <- as.factor(dados_SRI$grupo)  # Converte para fator
leveneTest(SRI_sadeh ~ grupo, data = dados_SRI)

# 6. Escolhero teste que vai usar  
# Teste t de Student (se as variâncias forem iguais)
t.test(SRI_sadeh ~ grupo, data = dados_SRI, var.equal = TRUE)

# Teste t de Welch (se as variâncias forem diferentes)
t.test(M10 ~ grupo, data = dados_SRI, var.equal = FALSE)