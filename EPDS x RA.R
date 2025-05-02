# 1. Pacotes necessários
install.packages("readxl")  # Execute apenas uma vez, se ainda não tiver
library(readxl)

# 2. Carregar planilha dos dados 
EPDSRA <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/EPDS x RA.xlsx")

# 3. Verificar os nomes das colunas
print(colnames(EPDSRA))  

# 4. Renomeando colunas, se necessário 
colnames(EPDSRA) <- c("RA", "EPDS")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
EPDSRA$RA <- as.numeric(gsub(",", ".", EPDSRA$RA))
EPDSRA$EPDS <- as.numeric(gsub(",", ".", EPDSRA$EPDS))

# 6. Criar grupos com base no score EPDS
grupo_sem_depressao <- EPDSRA$RA[EPDSRA$EPDS <= 13]
grupo_com_depressao <- EPDSRA$RA[EPDSRA$EPDS > 13]

# 7. Teste de normalidade de Shapiro-Wilk
shapiro_sem_dep <- shapiro.test(grupo_sem_depressao)
shapiro_com_dep <- shapiro.test(grupo_com_depressao)

# 8. Resultados do teste de normalidade
print(shapiro_sem_dep)
print(shapiro_com_dep)

# 9. Teste de Mann-Whitney (Wilcoxon Rank-Sum Test) para comparar os grupos
mann_whitney <- wilcox.test(grupo_sem_depressao, grupo_com_depressao)

# 10. Resultado do teste de Mann-Whitney
print(mann_whitney)

# 11. Boxplot para visualizar a distribuição dos grupos
boxplot(RA ~ (EPDS >= 10), data = EPDSRA,
        names = c("Non-depressed group", "Depressed group"),
        main = "Distribution of RA by Depressed and Non-depressed Groups",
        xlab = "",
        ylab = "RA",
        col = c("lightskyblue1", "lightseagreen"))
