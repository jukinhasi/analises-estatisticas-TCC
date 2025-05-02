# 1. Pacotes necessários
library(readxl)

# 2. Carregar planilha dos dados 
PHQ9RA <- read_excel("C:/Users/julin/Documents/FACULDADE/7 período/TCC/Análises em R/Tabelas dos dados/análises específicas (sono X depressão)/PHQ-9 x RA.xlsx")  

# 3. Verificar os nomes das colunas
print(colnames(PHQ9RA))

# 4. Renomeando colunas, se necessário
colnames(PHQ9RA) <- c("RA", "PHQ9")

# 5. Converter os dados para numérico, substituindo vírgulas por pontos
PHQ9RA$RA <- as.numeric(PHQ9RA$RA)

# 6. Criar os grupos com base no PHQ-9
grupo_sem_depressao <- PHQ9RA$RA[PHQ9RA$PHQ9 < 10]   
grupo_com_depressao <- PHQ9RA$RA[PHQ9RA$PHQ9 >= 10]  

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
boxplot(RA ~ (PHQ9 >= 10), data = PHQ9RA,
        names = c("Non-depressed group", "Depressed group"),
        main = "Distribution of AR by Depressed and Non-depressed Groups",
        xlab = "",
        ylab = "Amplitude Relativa (RA)",
        col = c("rosybrown1", "lightcoral"))


# 12. COmparar médias e medianas
mean(grupo_sem_depressao)
mean(grupo_com_depressao)

median(grupo_sem_depressao)
median(grupo_com_depressao)

# 13. Cálculo do tamanho de efeito
resultado <- wilcox.test(grupo_sem_depressao, grupo_com_depressao)

# Cálculo do r de Wilcoxon
r <- abs(qnorm(resultado$p.value / 2)) / sqrt(length(grupo_sem_depressao) + length(grupo_com_depressao))
r

