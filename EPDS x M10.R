# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
EPDSM10 <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/EPDS x M10.xlsx")

# 3. Verificar os nomes das colunas
print(colnames(EPDSM10))

# 4. Renomeando colunas, se necessário 
colnames(EPDSM10) <- c("M10", "EPDS")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
EPDSM10$M10 <- as.numeric(dados$M10)

# 6. Criar grupos com base no score EPDS
grupo_sem_depressao <- EPDSM10$M10[EPDSM10$EPDS <= 13]
grupo_com_depressao <- EPDSM10$M10[EPDSM10$EPDS > 13]

# 7. Teste de normalidade de Shapiro-Wilk
shapiro_sem_dep <- shapiro.test(grupo_sem_depressao)
shapiro_com_dep <- shapiro.test(grupo_com_depressao)

# 8. Resultados do teste de normalidade
print(shapiro_sem_dep)
print(shapiro_com_dep)

# 9.  Teste de Levene
install.packages("car")
library(car)
EPDSM10$grupo <- ifelse(EPDSM10$EPDS < 13, "Sem_Depressao", "Com_Depressao")
EPDSM10$grupo <- as.factor(EPDSM10$grupo)  # Converte para fator
leveneTest(M10 ~ grupo, data = EPDSM10)

# 10. Escolhero teste que vai usar  
# Teste t de Student (se as variâncias forem iguais)
t.test(M10 ~ grupo, data = EPDSM10, var.equal = TRUE)

# Teste t de Welch (se as variâncias forem diferentes)
t.test(M10 ~ grupo, data = PHQ9M10, var.equal = FALSE)

