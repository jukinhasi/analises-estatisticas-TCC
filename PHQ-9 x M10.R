# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
PHQ9M10 <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/PHQ-9 x M10.xlsx")

# 3. Verificar os nomes das colunas
print(colnames(PHQ9M10))

# 4. Renomeando colunas, se necessário
colnames(PHQ9M10) <- c("M10", "PHQ9")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
PHQ9M10$M10 <- as.numeric(PHQ9M10$M10)

# 6. Criar os grupos com base no PHQ-9
grupo_sem_depressao <- PHQ9M10$M10[PHQ9M10$PHQ9 < 10]  # PHQ-9 < 10 indica menor probabilidade de depressão
grupo_com_depressao <- PHQ9M10$M10[PHQ9M10$PHQ9 >= 10] # PHQ-9 >= 10 indica maior probabilidade de depressão

# 7. Teste de normalidade de Shapiro-Wilk
shapiro_sem_dep <- shapiro.test(grupo_sem_depressao)
shapiro_com_dep <- shapiro.test(grupo_com_depressao)

# 8. Resultados do teste de normalidade
print(shapiro_sem_dep)
print(shapiro_com_dep)

# 9.  Teste de Levene
install.packages("car")
library(car)
PHQ9M10$grupo <- ifelse(PHQ9M10$PHQ9 < 10, "Sem_Depressao", "Com_Depressao")
PHQ9M10$grupo <- as.factor(PHQ9M10$grupo)  # Converte para fator
leveneTest(M10 ~ grupo, data = PHQ9M10)

# 10. Escolhero teste que vai usar  
# Teste t de Student (se as variâncias forem iguais)
t.test(M10 ~ grupo, data = PHQ9M10, var.equal = TRUE)

# Teste t de Welch (se as variâncias forem diferentes)
t.test(M10 ~ grupo, data = PHQ9M10, var.equal = FALSE)


# 11. Boxplot para visualizar a distribuição dos grupos
boxplot(M10 ~ (PHQ9 >= 10), data = PHQ9M10,
        names = c("Non-depressed group", "Depressed group"),
        main = "Distribution of M10 by Depressed and Non-depressed Groups",
        xlab = "",
        ylab = "M10",
        col = c("rosybrown1", "lightcoral"))
