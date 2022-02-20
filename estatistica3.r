############ 1 questão ############# VARIANCIA CONHECIDA
#Uma fábrica Brasileira de cosméticos, trabalhava com variância de 14 dias. Em uma
#amostra de 120 cosméticos produzidos, foi possível obter um tempo médio de produção
#de 9 dias. Obtenha o intervalo para um nível de confiança de 90%.

#significancia 1-90% = 0.1
alfa = 0.10
#desvio padrao é a raiz quadrada da variancia
desvio = sqrt(14)
media = 9
n = 120

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)
zc

erro = zc * desvio / sqrt(n)
erro = round(erro, 2)
erro

cat( "[", media-erro, ",", media+erro, "]")

############ 2 questão ############# VARIANCIA DESCONHECIDA
#No Hospital Unimed, 10 pacientes do sexo feminino que estavam na sala de espera
#foram sorteadas para realizar a medição da pressão sanguínea arterial, obtendo os
#seguintes resultados: (80, 75, 71, 82, 77, 64, 78, 67, 81, 79). Determine o intervalo de
#confiança para a pressão arterial média feminina com coeficiente de confiança de 98%.

resultados <- c(80, 75, 71, 82, 77, 64, 78, 67, 81, 79)

#significancia 1-98% = 0.02
alfa = 0.02
#DESVIO PADRAO
desvio = sd(resultados)
#MEDIA
media = mean(resultados)
n = length(resultados)

zc = qt(1 - alfa/2, df = n-1)
zc = round(zc, 3)
zc

erro = zc * desvio / sqrt(n)
erro = round(erro, 3)
erro

cat( "[", media-erro, ",", media+erro, "]")
t.test(resultados, conf = 0.98)

############ 3 questão #############
#Um partido deseja estimar a proporção de eleitores favoráveis a um determinado
#candidato a prefeito.Uma amostra piloto de 3500 eleitores revelou que 65% dos
#eleitores são favoráveis a este candidato. Elaborar um intervalo de confiança de 95%.
#(testes de hipótese - bilateral)

prop.test(x = 0.65 * 3500, n = 3500, conf.level = 0.95)

############ 4 questão #############
#De uma população normal com variância 36, tira-se uma amostra aleatória de
#tamanho 20, obtendo-se uma média de 43. Ao nível de significância de 10%, testar as
#hipóteses: H଴ : u = 45, Hଵ : u ≠ 45.
#(testes de hipótese - unilateral à direita)

#variancia 36 -> desvio padrao = 6

desvio = sqrt(36)
tobs = (45 - 43)/desvio/sqrt(20)
tobs
liber = 20 - 1
2*(1-pt(tobs, liber))

#já que é maior que 10% aceitamos a hipotese nula, media = 45


############ 5 questão #############
#Um fabricante de contêineres realizou modificações em sua fabricação para
#aumentar a resistência média, que é de 510 Kg. Ao retirar uma amostra de 15
#contêineres, obteve-se uma média de 550 Kg. Sabendo que o desvio padrão de 25 Kg,
#com um nível de significância de 5%, pode o fabricante afirmar que a resistência média
#dos contêineres aumentou?
#(testes de hipótese com variância desconhecida)

tobs = (550 - 510)/25/sqrt(15)
tobs

#graus de liberdade
liber = 15 - 1

#hipotese nula: u = 510
#hipotese alternativa: u != 510, se quisesse ver maior seria mudando a pt() colocando
# outro parametro

2*(1-pt(tobs, liber)) #fortes evidencias em favor da hipótese nula
#(se manteu a média de 510)
#já que > 0.05, se manteu a média de 510

############ 6 questão #############

#O instituto de engenharia de uma universidade aplica um teste vocacional para os
#calouros. Nos últimos anos tem sido admitida uma nota média de 127. Um teste foi
#realizado no semestre atual, onde foram obtidas as seguintes notas: (125, 124, 125,
#125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124).
#Queremos saber se a média
#do semestre atual foi diferente dos anteriores, então, realize o teste de hipótese,
#admitindo um nível de significância de 5% para efetuar o teste.

#(teste de hipótese para dados pareados)

b <- c(125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124)
mediaATUAL <- mean(b)
mediaANTERIORES <- 127

############ 7 questão #############

#Na disciplina de teoria dos grafos,
#o professor passou como atividade avaliativa, a
#implementação de um algoritmo específico de grafos,
#que deveria ser implementado
#utilizando dois tipos de busca: busca em largura e
#busca em profundidade. O professor
#coletou os dados de tempo de execução (em milissegundos)
#de 15 alunos, para os dois
#métodos de busca.
#Existe diferença na velocidade de execução do algoritmo para os
#dois tipos de busca? Verificar a um nível de 5% de significância.

bfs <- c(32, 27, 38, 35, 33, 29, 25, 19, 31, 24)
dfs <- c(26, 21, 20, 37, 30, 18, 19, 25, 32, 34)

t.test(bfs, dfs, mu = 0, paired = TRUE, conf.level = 0.95)
# Como p_value é maior que 0.05 não rejeitamos H0
# não existe diferença entre as amostras

############ 8 questão #############
#Um pesquisador estudou os efeitos de determinada dieta
#alimentar sobre o aumento do peso corporal em cobaias adultas.
#Coletou seus pesos antes e três meses após a gestão da nova dieta
#e obteve:

#Antes: 54 61 50 74 79 58 55 49 63
#Três meses Depois: 57 66 53 73 82 58 56 53 63

#Considere as hipóteses: H0: μD = μA
#                        H1: μD ≠ μA
#Considere α = 0,05 e justifique sua resposta com relação às
#hipóteses estabelecidas.

antes <- c(54, 61, 50, 74, 79, 58, 55, 49, 63)
depois <- c(57, 66, 53, 73, 82, 58, 56, 53, 63)

t.test(antes, depois, mu = 0, paired = TRUE, conf.level = 0.95)
#Como p−value é menor que α, há evidências amostrais
#suficientes para rejeitar H0

############ 9 questão #############

#Um professor afirma que os alunos vão baixando seus coeficientes
#de rendimento à medida que avançam no curso. Oito alunos foram
#escolhidos aleatoriamente e observado seus rendimentos nos
#semestres anterior e atual.

#Anterior: 89, 84, 96, 82, 74, 92, 85 e 91.
#Atual: 83, 83, 92, 84, 76, 91, 80 e 91.

#Assumindo que os coeficientes são distribuídos normalmente,
#existe evidência suficiente para apoiar a afirmação do professor
#para um nível de significância de 10%?

anterior <- c(89, 84, 96, 82, 74, 92, 85, 91)
atual <- c(83, 83, 92, 84, 76, 91, 80, 91)

t.test(anterior, atual, mu = 0, paired = TRUE, conf.level = 0.90)

# Como p_value é maior que 0.10 não rejeitamos que eles sejam iguais
# não existe diferença entre as amostras, então não existe evidência
# para apoiar a afirmação do professor

############ 10 questão #############

#Uma nova metodologia de desenvolvimento de software
#se propõe a reduzir o tempo de projeto e desenvolvimento de
#sistemas de informação. Assim, foram considerados 24 projetos,
#sendo 12 de tecnologia atual e 12 com a nova proposta. Os
#valores, em horas, estão a seguir:

#TecAtual: 300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 283
#TecNova: 274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 332, 263

#Considerando nível de significância de 2%, as hipóteses:
  
#H0 : μ1 − μ2 ≤ 0
#H1 : μ1 − μ2 > 0

#Calcule t, o p-value e interpretar os resultados.

Tatual <- c(300, 280, 344, 385, 372, 360, 288, 321, 376, 290, 301, 283)
Tnova <- c(274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 332, 263)

t.test(Tatual, Tnova, paired = TRUE, alternative = "greater")
# p-value = 0.01609 < 0.02 (2%) rejeitar a hipótese nula
# (H0 : μ1 − μ2 ≤ 0)
# a média da atual é maior que a nova (H1 : μ1 − μ2 > 0)


############ 11 questão #############
#Um fabricante de cerveja identificou que a qualidade das cervejas depende do
#fornecedor da cevada e que essa qualidade é percebida pelos clientes.
#A tabela a seguir mostra as notas atribuídas pelos clientes aos fornecedores.

#Fornecedor 1 Fornecedor 2 Fornecedor 3 Fornecedor 4
#68.5         76.3         70.6         75.4
#74.0         75.3         75.2         69.9
#67.2         74.0         70.8         72.6
#69.9         71.2         74.7         67.5
#68.0         74.5         72.9         70.4

#Verificar se existe diferença entre os fornecedores, em nível de 5% de significância,
#e em caso afirmativo quais fornecedores são significativamente diferentes.

frnc1 <- c(68.5, 74.0, 67.2, 69.9, 68.0)
frnc2 <- c(76.3, 75.3, 74.0, 71.2, 74.5)
frnc3 <- c(70.6, 75.2, 70.8, 74.7, 72.9)
frnc4 <- c(75.4, 69.9, 72.6, 67.5, 70.4)

vet <- c(frnc1, frnc2, frnc3, frnc4)

ind = 1
vet[1:5]
vet[6:10]
vet[11:15]
vet[16:20]

t.test(frnc1, frnc2, paired = TRUE, mu = 0, conf.level = 0.95) #p-value = 0.02894  X
t.test(frnc1, frnc3, paired = TRUE, mu = 0, conf.level = 0.95) #p-value = 0.01059  X
t.test(frnc1, frnc4, paired = TRUE, mu = 0, conf.level = 0.95) #p-value = 0.4863
t.test(frnc2, frnc3, paired = TRUE, mu = 0, conf.level = 0.95) #p-value = 0.4088
t.test(frnc2, frnc4, paired = TRUE, mu = 0, conf.level = 0.95) #p-value = 0.02166  X
t.test(frnc3, frnc4, paired = TRUE, mu = 0, conf.level = 0.95) #p-value = 0.4909

#alfa: 5%
# se p-valor < 5% rejeitamos a hipotese que eles sejam iguais (são diferentes)
# fornecedor 1 com fornecedor 2
# fornecedor 1 com fornecedor 3
# fornecedor 2 com fornecedor 4     são significativamente diferentes


############ 12 questão #############
#Um fabricante de impressoras com três fábricas deseja examinar se o conhecimento
#em gerenciamento de qualidade é igual nas três fábricas. Uma amostra com notas de
#seis funcionários de cada fábrica estão a seguir:
#  Fábricas

#Atlanta Dallas Seatle
#85       71    59
#75       75    64
#82       73    62
#76       74    69
#71       69    76
#85       82    67

#Verificar a homogeneidade das variâncias, a normalidade da amostra e elaborar o
#quadro de Análise de Variação, interpretar o resultado geral e os resultados entre as
#fábricas. Considere um nível de significância de 5%.

Atlanta <- c(85, 75, 82, 76, 71, 85)
Dallas <- c(71, 75, 73, 74, 69, 82)
Seatle <- c(59, 64, 62, 69, 76, 67)

d <- c()

ind = 1
ind2 = 1
for (i in Atlanta)
{
  d[ind] = i
  d[ind+1] = Dallas[ind2]
  d[ind+2] = Seatle[ind2]
  ind = ind + 3
  ind2 = ind2 + 1
}
d

#Os pressupostos básicos da análise de variância são:
  
# As amostras são aleatórias e independentes;
# As populações têm distribuição normal; e
# As variâncias populacionais são iguais.

#Verificar a homogeneidade das variâncias

trat = factor(rep(1:3, times = 6), label = c("A", "B", "C"))
trat

bartlett.test(d ~ trat)
# p-value = 0.8009 > 5%, admitimos que as variancias são homogeneas


#Verificar a normalidade da amostra

shapiro.test(Atlanta) #p-value = 0.3479 > 5%, (normalidade)
shapiro.test(Dallas) #p-value = 0.4756 > 5%, (normalidade)
shapiro.test(Seatle) #p-value = 0.8819 > 5%, (normalidade)



#Elaborar o quadro de Análise de Variação

tab12 <- aov(Atlanta ~ Dallas)
anova(tab12)  #Pr(>F) = 0.3316 = 33,16% > 5% (as notas não diferem umas das outras)
# de atlanta e dallas
#TukeyHSD(tab12)

tab12 <- aov(Atlanta ~ Seatle)
anova(tab12) #Pr(>F) = 0.09272 = 9,272% > 5%
#(as notas não diferem umas das outras, (Atlanta e Seatle))

tab12 <- aov(Dallas ~ Seatle)
anova(tab12) #Pr(>F) = 0.7883 = 78,83% > 5%
#(as notas não diferem umas das outras, (Dallas e Seatle))

##CONCLUSAO
#o conhecimento em gerenciamento de qualidade é igual nas três fábricas.


############ 13 questão #############
#Deseja-se testar se existem diferenças na quantidade de jacarés em três locais do
#pantanal mato-grossense.

#▪ L1: 13, 11, 14, 12, 12, 10, 10, 15, 10, 13, 11, 12, 11, 10, 14
#▪ L2: 9, 10, 9, 7, 8, 10, 11, 11, 7, 8, 7, 11, 11, 8, 6
#▪ L3: 17, 15, 20, 19, 17, 18, 17, 18, 19, 21, 15, 15, 19, 15, 17

#Verificar a homogeneidade das variâncias, a normalidade da amostra e elaborar o
#quadro de Análise de Variação, interpretar os resultados. Considere um nível de
#significância de 1%.

#Verificar a homogeneidade das variâncias

L1 <- c(13, 11, 14, 12, 12, 10, 10, 15, 10, 13, 11, 12, 11, 10, 14)
L2 <- c(9, 10, 9, 7, 8, 10, 11, 11, 7, 8, 7, 11, 11, 8, 6)
L3 <- c(17, 15, 20, 19, 17, 18, 17, 18, 19, 21, 15, 15, 19, 15, 17)

d <- c()

ind = 1
ind2 = 1
for (i in L1)
{
  d[ind] = i
  d[ind+1] = L2[ind2]
  d[ind+2] = L3[ind2]
  ind = ind + 3
  ind2 = ind2 + 1
}
d

trat = factor(rep(1:3, times = 15), label = c("A", "B", "C"))
trat

bartlett.test(d ~ trat)
# p-value = 0.35803 > 1%, admitimos que as variancias são homogeneas



#Verificar a normalidade da amostra

shapiro.test(L1) #p-value = 0.1366 > 1%, (normalidade)
shapiro.test(L2) #p-value = 0.1034 > 1%, (normalidade)
shapiro.test(L3) #p-value = 0.1846 > 1%, (normalidade)



#Elaborar o quadro de Análise de Variação

tab12 <- aov(L1 ~ L2)
anova(tab12)  #Pr(>F) = 0.8398 = 83,98% > 5%
#(as quantidades não diferem umas das outras) (de L1 e L2)

tab12 <- aov(L1 ~ L3)
anova(tab12) #Pr(>F) = 0.2522 = 25,22% > 5%
#(as quantidades não diferem umas das outras) (de L1 e L3)

tab12 <- aov(L2 ~ L3)
anova(tab12) #Pr(>F) = 0.7564 = 75,64% > 5%
#(as quantidades não diferem umas das outras) (de L2 e L3)

##CONCLUSAO
#não existem diferenças na quantidade de jacarés em três locais do
#pantanal mato-grossense.


############ 14 questão #############
#Um pesquisador deseja verificar se um instrumento para medir a concentração de
#determinada substância no sangue está bem calibrado. Para isto, ele tomou 15
#amostras de concentrações conhecidas (X) e determinou a respectiva concentração
#através do instrumento (Y), obtendo:

#X 2,0 2,0 2,0 4,0 4,0 4,0 6,0 6,0 6,0 8,0 8,0 8,0 10,0 10,0 10,0
#Y 2,1 1,8 1,9 4,5 4,2 4,0 6,2 6,0 6,5 8,2 7,8 7,7 9,6 10,0 10,1

X <- c(2.0, 2.0, 2.0, 4.0, 4.0, 4.0, 6.0, 6.0, 6.0, 8.0, 8.0, 8.0, 10.0, 10.0, 10.0)
Y <- c(2.1, 1.8, 1.9, 4.5, 4.2, 4.0, 6.2, 6.0, 6.5, 8.2, 7.8, 7.7, 9.6, 10.0, 10.1)

Reg <- data.frame(USO = X, CONCENTRAÇÃO = Y)
Reg

plot(Reg)
abline(regressao, col = "red")

#Calcular.
#a) Coeficiente de Correlação e de determinação;

#Hipotese 0: não há correlação
#hipotese 1: há correlação

cor(X, Y) #0.9960838 (99,6% forte correlação) (<<<<<< coeficiente de correlação)
cor.test(X, Y, method = "pearson") #p-value = 4.394e-15 < 1%

#concluindo que há correlação entre X e Y já que p valor < 1%, ou ainda,
#existe uma correlação positiva entre X e Y, e que as
#variáveis são diretamente proporcionais.

regressao = lm(X ~ Y)
regressao  # y = 1.0124x -0.1151

summary(regressao)
#Adjusted R-squared:  0.9916  << coeficiente de determinação


#b) Equação da reta;
regressao  # y = 1.0124x -0.1151

#c) As estimativas dos valores de y para x= 5 e x=12.
predict(regressao)
est1 = regressao$coefficients[1] + regressao$coefficients[2]*5
est2 = regressao$coefficients[1] + regressao$coefficients[2]*12
est1 #4.947071
est2 #12.03409


############ 15 questão #############

#x  y
#14 28
#18 30
#40 45
#41 50
#50 56
#92 94

x <- c(14, 18, 40, 41, 50, 92)
y <- c(28, 30, 45, 50, 56, 94)

regressao = lm(x ~ y)
summary(regressao)
#d) Coeficiente de Correlação e de determinação;

#Adjusted R-squared:  0.9923  << coeficiente de determinação
cor(x, y) #0.9969013 (99,6% forte correlação) (<<<<<< coeficiente de correlação)

#e) Equação da reta;

regressao
# y = 1.164x -16.295

#Calcular:
#f) Elaborar o teste de hipótese e interpretar usando p-value.

cor.test(x, y, method = "pearson") #p-value = 1.439e-05 < 1%

#concluindo que há correlação entre x e y já que p valor < 1%, ou ainda,
#existe uma correlação positiva entre x e y, e que as
#variáveis são diretamente proporcionais.
