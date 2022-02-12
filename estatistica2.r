install.packages("readxl")
library(readxl)

#Importando o dataframe
df_excel <- read_excel(file.choose())
df_excel

#Filtrando as notas
notas <- df_excel$NOTA_ENEN

############## PRIMEIRA ###############
#significancia = x; nivel de confiança = 1 - x

#intervalo de confiança com 5% de nível de significância. (1 - 0.05) = 0.95
t.test(notas, conf = 0.95)

#intervalo de confiança com 1% de nível de significância. (1 - 0.01) = 0.99
t.test(notas, conf = 0.99)

#######################################

df_excel[df_excel$CO_ESCOLA == 27051617, ]

a <- lapply(df_excel, function(x) which(x == 27051617))
v <- a$CO_ESCOLA
v

#colegio 27051617

df_colegio <- data.frame()

for (i in v){
  #print(df_excel[i ,])
  df_colegio <- rbind(df_colegio, df_excel[i ,])
}

#TESTE DE NORMALIDADE
df_colegio
CN <- df_colegio$NU_NOTA_CN
CH <- df_colegio$NU_NOTA_CH
LC <- df_colegio$NU_NOTA_LC
MT <- df_colegio$NU_NOTA_MT

############## SEGUNDA ###############
############## LETRA A ###############
# O p-valor for < 0.05 indica que os dados não apresentam normalidade.
# 5 % de significancia ^^

barplot(CN)
hist(CN)
shapiro.test(CN)
# p-value = 0.2959

barplot(CH)
hist(CH)
shapiro.test(CH)
# p-value = 0.2269

barplot(LC)
hist(LC)
shapiro.test(LC)
# p-value = 0.2981

barplot(MT)
hist(MT)
shapiro.test(MT)
# p-value = 0.03903 (NAO APRESENTA NORMALIDADE)

############## LETRA B ###############

# Transformando o MT (raiz quadrada)
mt <- sqrt(MT)
hist(mt)
shapiro.test(mt)
# p-value = 0.07591 > 0.05 (APRESENTA NORMALIDADE)

############## LETRA C ###############

CN_SEXO <- data.frame(NU_NOTA_CN = CN,
                      TP_SEXO = df_colegio$TP_SEXO)
CH_SEXO <- data.frame(NU_NOTA_CH = CH,
                      TP_SEXO = df_colegio$TP_SEXO)
LC_SEXO <- data.frame(NU_NOTA_LC = LC,
                      TP_SEXO = df_colegio$TP_SEXO)
MT_SEXO <- data.frame(NU_NOTA_MT = mt,
                      TP_SEXO = df_colegio$TP_SEXO)
#CH_SEXO

#                 CN                 #

cn_mulheres <- CN_SEXO[CN_SEXO$TP_SEXO == "Feminino", ]
cn_homens <- CN_SEXO[CN_SEXO$TP_SEXO == "Masculino", ]

notas_CN_mulheres <- cn_mulheres$NU_NOTA_CN
notas_CN_homens <- cn_homens$NU_NOTA_CN

notas_CN_mulheres
notas_CN_homens

#                 CH                 #

ch_mulheres <- CH_SEXO[CH_SEXO$TP_SEXO == "Feminino", ]
ch_homens <- CH_SEXO[CH_SEXO$TP_SEXO == "Masculino", ]

notas_CH_mulheres <- ch_mulheres$NU_NOTA_CH
notas_CH_homens <- ch_homens$NU_NOTA_CH

notas_CH_mulheres
notas_CH_homens

#                 LC                 #

lc_mulheres <- LC_SEXO[LC_SEXO$TP_SEXO == "Feminino", ]
lc_homens <- LC_SEXO[LC_SEXO$TP_SEXO == "Masculino", ]

notas_LC_mulheres <- lc_mulheres$NU_NOTA_LC
notas_LC_homens <- lc_homens$NU_NOTA_LC

notas_LC_mulheres
notas_LC_homens

#                 MT                 #

mt_mulheres <- MT_SEXO[MT_SEXO$TP_SEXO == "Feminino", ]
mt_homens <- MT_SEXO[MT_SEXO$TP_SEXO == "Masculino", ]

notas_MT_mulheres <- mt_mulheres$NU_NOTA_MT
notas_MT_homens <- mt_homens$NU_NOTA_MT

notas_MT_mulheres
notas_MT_homens

# hipotese zero = media dos homens é menor que a de mulheres
# p_valor < 0.05 rejeitamos a hipotese nula

# fazer uma media de p_values para ter uma certeza maior

media_testeCN = 0
media_testeCH = 0
media_testeLC = 0
media_testeMT = 0

for (i in 1:100){
  #randomizar para ser "justo" com as mulheres
  #já que os vetores tem q ser do mesmo tamanho
  
  ind <- sample(1:23, 14, replace=T)
  notas_aleatorias_CN_M <- notas_CN_mulheres[ind]
  ind <- sample(1:23, 14, replace=T)
  notas_aleatorias_CH_M <- notas_CH_mulheres[ind]
  ind <- sample(1:23, 14, replace=T)
  notas_aleatorias_LC_M <- notas_LC_mulheres[ind]
  ind <- sample(1:23, 14, replace=T)
  notas_aleatorias_MT_M <- notas_MT_mulheres[ind]
  
  k <- t.test(notas_CN_homens, notas_aleatorias_CN_M,
              paired = TRUE, alternative = "greater")
  media_testeCN = media_testeCN + k$p.value
  
  k <- t.test(notas_CH_homens, notas_aleatorias_CH_M,
              paired = TRUE, alternative = "greater")
  media_testeCH = media_testeCH + k$p.value
  
  k <- t.test(notas_LC_homens, notas_aleatorias_LC_M,
              paired = TRUE, alternative = "greater")
  media_testeLC = media_testeLC + k$p.value
  
  k <- t.test(notas_MT_homens, notas_aleatorias_MT_M,
              paired = TRUE, alternative = "greater")
  media_testeMT = media_testeMT + k$p.value
}

print(media_testeCN/100) #p-value = 0.5449335
print(media_testeCH/100) #p-value = 0.6825037
print(media_testeLC/100) #p-value = 0.8585995
print(media_testeMT/100) #p-value = 0.08402493

# OU SEJA como p-valor ta em media > 0.05 não rejeitamos a hipotese nula,
# a nota das mulheres é maior que a dos homens

############## LETRA D ###############

boxplot(CN_SEXO$NU_NOTA_CN, notas_CN_homens, notas_CN_mulheres,
        main = "NOTAS CN",
        names = c("notas gerais", "notas homens", "notas mulheres"))

boxplot(CH_SEXO$NU_NOTA_CH, notas_CH_homens, notas_CH_mulheres,
        main = "NOTAS CH",
        names = c("notas gerais", "notas homens", "notas mulheres"))

boxplot(LC_SEXO$NU_NOTA_LC, notas_LC_homens, notas_LC_mulheres,
        main = "NOTAS LC",
        names = c("notas gerais", "notas homens", "notas mulheres"))

boxplot(MT_SEXO$NU_NOTA_MT, notas_MT_homens, notas_MT_mulheres,
        main = "NOTAS MT",
        names = c("notas gerais", "notas homens", "notas mulheres"))

############## LETRA E ###############

#############################################################################
######################         3 Questão         ###########################
#############################################################################

municipios <- unique(df_excel$NO_MUNICIPIO_PROVA)
municipios
notas_municipios <- c()

ind = 1

for (i in municipios){
  # onde tiver um municipio eu pego as notas dele
  nota_municipio <- df_excel[df_excel$NO_MUNICIPIO_PROVA == i, ]$NOTA_ENEN
  notas_municipios[ind] = mean(x = nota_municipio)
  ind = ind + 1
  #print(mean(x = nota_municipio))
}
notas_municipios

mesrregioes <- c("Sertão", "Agreste", "Leste")

sertao <- c("Água Branca", "Delmiro Gouveia",
            "Santana do Ipanema", "São José da Tapera", "Batalha",
            "Olho d'Água das Flores")

agreste <- c("Igaci", "Palmeira dos Índios", "Arapiraca",
             "Girau do Ponciano", "Traipu")

leste <- c("União dos Palmares", "Viçosa", "Atalaia",
           "Porto Calvo", "São Luís do Quitunde",
           "Maceió", "Marechal Deodoro", "Pilar",
           "Rio Largo",
           "Boca da Mata", "Campo Alegre", "Coruripe",
           "São Miguel dos Campos", "Teotônio Vilela",
           "Penedo")

#sertao (6), agreste (5), leste (15) = 26

# VERIFICAR SE PERTENCE A TABELA DOS MUNICIPIOS PARTICIPANTES
for (i in leste)
{
  print(i)
  
  a <- df_municipios_notas$NO_MUNICIPIO_PROVA == i
  for (i in a)
  {
    if (i == TRUE)
    {
      break;
    }
  }
  print(i)
}

################### tabela das medias mesorregioes

medsertao <- 0
medagreste <- 0
medleste <- 0

notassertao <- c()
notasagreste <- c()
notasleste <- c()

df_municipios_notas <- data.frame(NO_MUNICIPIO_PROVA = municipios,
                                  NOTA_ENEN = notas_municipios)
df_municipios_notas

######### SERTAO

cont <- 1
cont2 <- 1

for (i in sertao){
  a <- df_municipios_notas$NO_MUNICIPIO_PROVA == i
  for (b in a)
  {
    if (b == TRUE)
    {
      print(i)
      print(cont)
      
      print(df_municipios_notas[cont, ])
      print(df_municipios_notas[cont, 2])
      medsertao = medsertao + (df_municipios_notas[cont, 2])
      notassertao[cont2] = df_municipios_notas[cont, 2]
      
      cont2 = cont2 + 1
      #print(df_municipios_notas$NO_MUNICIPIO_PROVA)
      break;
    }
    cont = cont + 1
  }
  cont <- 1
  #print(i)
}

length(sertao)
medsertao
medsertao = medsertao / length(sertao)
medsertao
notassertao

#################

######### AGRESTE

cont <- 1
cont2 <- 1

for (i in agreste){
  a <- df_municipios_notas$NO_MUNICIPIO_PROVA == i
  for (b in a)
  {
    if (b == TRUE)
    {
      print(i)
      print(cont)
      
      print(df_municipios_notas[cont, ])
      print(df_municipios_notas[cont, 2])
      medagreste = medagreste + (df_municipios_notas[cont, 2])
      notasagreste[cont2] = df_municipios_notas[cont, 2]
      
      cont2 = cont2 + 1
      break;
    }
    cont = cont + 1
  }
  cont <- 1
}

length(agreste)
medagreste
medagreste = medagreste / length(agreste)
medagreste
notasagreste

NotaAgreste <- data.frame(MUNICIPIO = agreste,
                          MEDIA_NOTA = notasagreste)

NotaAgreste

#################

######### LESTE

cont <- 1
cont2 <- 1

for (i in leste){
  a <- df_municipios_notas$NO_MUNICIPIO_PROVA == i
  for (b in a)
  {
    if (b == TRUE)
    {
      print(i)
      print(cont)
      
      print(df_municipios_notas[cont, ])
      print(df_municipios_notas[cont, 2])
      medleste = medleste + (df_municipios_notas[cont, 2])
      notasleste[cont2] = df_municipios_notas[cont, 2]
      
      cont2 = cont2 + 1
      break;
    }
    cont = cont + 1
  }
  cont <- 1
}

length(leste)
medleste
medleste = medleste / length(leste)
medleste
notasleste

NotaLeste <- data.frame(MUNICIPIO = leste,
                          MEDIA_NOTA = notasleste)

NotaLeste

#################

medias <- c(medsertao, medagreste, medleste)

df_mesorregioes <- data.frame(MESORREGIOES = mesrregioes,
                              MEDIA_NOTA = medias)

df_mesorregioes
