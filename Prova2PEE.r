#xn = 1+1+0 = 2
#yn = 2 + sqrt(2/2) = 3
#zn = 2 + 3 – 1 = 4
X <- c(11.2, 8.6, 11.0, 9.8, 11.0, 14.0, 6.0, 4.0, 12.0, 7.4, 10.8, 2.0)
Y <- c(9.50, 6.58, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 8.04, 3.0)
Z <- c(8.25, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 10.50, 5.25, 7.91, 6.58, 4.0)

a <- data.frame(X = X, Y = Y, Z = Z)
a
#a) A matriz de correlações entre as variações e explicar os resultados.

cor(a)

#b) Escolher as duas variáveis com maior correlação e calcular a equação da reta
#e o coeficiente de determinação de Pearson (R2) e interprete o resultado.

regressao = lm(X ~ Y)
regressao
# y = 1.3354x - 0.9161

summary(regressao)
#Adjusted R-squared:  0.8009  << coeficiente de determinação

#c) Elaborar um teste de hipótese para validar o coeficiente de determinação
#obtido acima e justifique sua resposta.

#d) Verificar se os resíduos são aderentes a distribuição normal.



###################################################

# 2 questao

#Uma fábrica de biscoito menciona o índice de açúcar de seus biscoitos é de
#27 mg. Um laboratório tirou uma amostra e obteve os seguintes valores, em
#mg: 27, 26, 26, 24, 23, 22, 28, 25, 28, 24, 27, 25, 20, 24, 24, 27, 25, 28, 26
#e xx.

amostra <- c(27, 26, 26, 24, 23, 22, 28, 25, 28, 24, 27, 25, 20, 24, 24, 27, 25, 28, 26, 20)

#xx = 2+0+1+1+0+9+2+5 = 20.

#a) O fabricante está correto para o nível de significância de 1%?
t.test(amostra, mu=27) #p-value = 0.001133 < 1%
# não está correto (rejeito a hipótese nula)

#b) O fabricante está correto para o nível de confiança de 94%?
#significancia = 1 - confianca = 6%

t.test(amostra, mu=27) #p-value = 0.001133 < 6%
# não está correto (rejeito a hipótese nula)

#c) Refaça os cálculos do item a e a interpretação considerando as
#hipóteses:

#H0 ≤ 27 e H1 > 27

t.test(amostra, mu = 27, alternative = "greater")
#p-value = 0.9994 (não rejeito a hipotese nula, <= 27)

#d) Refaça os cálculos do item b e a interpretação considerando as
#hipóteses:

#H0 ≥ 27 e H1 < 27

t.test(amostra, mu = 27, alternative = "less")
#p-value = 0.0005667 (rejeito a hipotese nula, >= 27)



###################################################

# 3 questao

#Considere 26 projetos, sendo 13 com a tecnologia old e 13 com a tecnologia
#smart. Os valores, em horas, estão a seguir:

TecAtual <- c(300, 280, 344, 385, 372, 360, 288, 321, 376, 290, 301, 320, 396)
TecNova <- c(274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 273, 305, 400)

# Xta = 201 + 200 -5 = 396
# Ytn = Xta + 4 = 400

#a) Calcular o Intervalo de Confiança com 2% de nível de significância para
#os valores de TecAtual e Tecnova.

t.test(TecAtual, conf = 0.98)
t.test(TecNova, conf = 0.98)

#b) Considerando nível de significância de 4 %, as hipóteses:
#H0 : μ1 − μ2 ≤ 0
#H1 : μ1 − μ2 > 0

#Calcular t, o p-value e interpretar os resultados.

t.test(TecAtual, TecNova, paired = TRUE, alternative = "greater")
#t = 2.8406, p-value = 0.00744

#c) Considerando as hipóteses acima, testar as variâncias e a normalidade
#dos dados. Considerando que os dados são não pareados faça a análise
#e interprete os resultados a 5% de significância.

shapiro.test(TecAtual) #p-value = 0.2102 > 5%, (normalidade)
shapiro.test(TecNova) #p-value = 0.5967 > 5%, (normalidade)

t.test(TecAtual, TecNova, alternative = "greater")
#p-value = 0.01933 < 5%
#testar as variâncias




###################################################

# 4 questao

#Considere os dados da tabela abaixo são as notas de corte para acesso
#ao curso de medicina nas Universidades Federais e Estaduais por região,
#conforme tabela.

regiao <- c("Centro_Oeste", "Centro_Oeste", "Centro_Oeste", "Centro_Oeste",
            "Centro_Oeste", "Centro_Oeste", "Centro_Oeste", "Centro_Oeste",
            "Centro_Oeste", "Centro_Oeste", "Nordeste", "Nordeste", "Nordeste",
            "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Norte", "Norte",
            "Norte", "Norte", "Norte", "Norte", "Norte", "Norte", "Sudeste",
            "Sudeste", "Sudeste", "Sudeste", "Sudeste", "Sudeste", "Sudeste",
            "Sul", "Sul", "Sul", "Sul", "Sul", "Sul", "Sul", "Sul")

nota <- c(776, 791, 786, 758, 776, 790, 797, 790, 792, 770, 794, 794, 790, 806,
          789, 795, 775, 803, 782, 796, 783, 774, 755, 789, 768, 818, 783, 816,
          825, 806, 803, 790, 798, 788, 811, 799, 792, 804, 795, 800)

d <- data.frame(Regiao = regiao, Nota = nota)
d

vetor <- c()
ind = 1
ind2 = 1
for (i in regiao)
{
  vetor[ind] = i
  vetor[ind+1] = nota[ind2]
  ind = ind + 2
  ind2 = ind2 + 1
}
vetor

trat = factor(rep(1:2, times = 40), label = c("A", "B"))
trat

#Calcular:
#a) Verificar e interpretar a homogeneidade das variâncias das regiões;

aggregate(Nota ~ Regiao, d, var)
bartlett.test(Nota ~ Regiao, d)

#b) Verificar e interpretar a normalidade dos dados das regiões;

shapiro.test(nota)

#c) Elaborar e interpretar o quadro de Análise de Variância (ANOVA);

tab12 <- aov(d$Nota ~ d$Regiao)
anova(tab12)

#d) Verificar as divergências entre as médias das regiões e interpretar os
#resultados a 5% e a 1% de significância.
