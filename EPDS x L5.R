# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
EPDSL5 <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/EPDS x L5.xlsx")

# 3. Verificar os nomes das colunas
print(colnames(EPDSL5))

# 4. Renomeando colunas, se necessário 
colnames(EPDSL5) <- c("L5", "EPDS")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
EPDSL5$L5 <- as.numeric(EPDSL5$L5)

# 6. Criar grupos com base no score EPDS
grupo_sem_depressao <- EPDSL5$L5[EPDSL5$EPDS <= 13]
grupo_com_depressao <- EPDSL5$L5[EPDSL5$EPDS > 13]

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
boxplot(L5 ~ (EPDS >= 10), data = EPDSL5,
        names = c("Non-depressed group", "Depressed group"),
        main = "Distribution of L5 by Depressed and Non-depressed Groups",
        xlab = "",
        ylab = "L5",
        col = c("lightskyblue1", "lightseagreen"))
