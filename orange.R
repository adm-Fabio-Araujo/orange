# fonte: https://rpubs.com/andrehofreire/LinearRegressionR#:~:text=A%20regress%C3%A3o%20linear%20%C3%A9%20uma,preditoras%20e%20a%20vari%C3%A1vel%20target.&text=Utilizamos%20a%20fun%C3%A7%C3%A3o%20nativa%20do%20R%20para%20criar%20o%20modelo%20de%20regress%C3%A3o.
# coletando os dados
head(Orange)
cols <- c(colnames(Orange[-1]))
Orange <- Orange[,cols]
str(Orange)
# análise exploratória
par(mfrow = c(1,2), mar = c(4,4,4,4))
hist(x = Orange$age, xlab = "idade da árvore", ylab = "frequência observada", col = "orange")
hist(x = Orange$circumference, xlab = "circunferência da árvore", ylab = "frequência observada", col = "green")
summary(Orange)
# identificação de possíveis outliers e, caso existam, avaliar se os mesmos dever fazer parte do modelo 
boxplot(Orange$age, Orange$circumference, names = c('idade da árvore', 'circunferência da árvore'), 
        col = c('gray', 'red'), main = 'boxplot das laranjeiras')
# criação do modelo
modelo <- lm(Orange$circumference ~ Orange$age)
summary(modelo)
# interpretação do modelo
# A função summary faz o seguinte resumo do modelo em apreço:
# call: mostra as variáveis que foram utilizadas.
# residuals: O resíduo é a diferença entre o valor previsto e o valor real. 
# coeficients: a coluna estimate contém os coeficientes que formam a equação gerada pelo modelo onde:
#   intercept: é o ponto onde a linha cruza o eixo y, dito de outra forma, é o valor de “a” na equação de regressão.
#   “Orange$age”: é o coeficiente angular da equação de regressão, ou “b” na equação de regressão (variável preditora).
#   Std. Error: medida de variabilidade na estimativa do coeficiente angular “a”.
#   t value: valores que são usados para calcular o p-value e os níveis de significância.
#   Pr(>|t|): é o p-value do teste t, o p-value representa a probabilidade que a variável não seja relevante para o modelo
#     normalmente utiliza-se: p-valor < 0,05, a correlação entre as duas variáveis é significativa,
#     caso contrário, a correlação entre as duas variáveis não é significativa.
#     os símbolos apresentados do lado direito do p-value mostram para qual significância os coeficientes são significativos.
#     quanto mais asteriscos, maior a probabilidade de existir relacionamento entre as variáveis.
# a linha Signif. codes mostra os códigos utilizados para a significância, do mais (3 asteriscos) ao menos significante (espaço vazio).
# residual Standar Error: representa o desvio padrão dos resíduos.
# degrees of Freddom: diferença entre o número de observações da amostra e o número de variáveis no modelo.
# multiple R-squared e Adjusted R-squared: definem a representatividade da variável x para prever a variável y, (quanto maior melhor).
# F-statistics: É um teste que compara o desempenho de um modelo com mais parâmetros (variáveis preditoras) com um modelo com menos parâmetros. 
#     o p-value será alto se o modelo com menos parâmetros tiver desempenho melhor que um modelo com mais parâmetros e o p-value será mais baixo 
#     se o seu modelo com mais parâmetros tiver um desempenho melhor que um modelo com menos parâmetros. 
#     em geral um modelo com mais parâmetros tem um desempenho melhor.



