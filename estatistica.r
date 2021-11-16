#https://www.youtube.com/watch?v=-BST5xCFBrQ
file.choose()

install.packages("readxl")
library(readxl)

df_excel <- read_excel(file.choose())

df_excel
boxplot(df_excel$NOTA_ENEN, main = "Notas", col = "green")

#freq absoluta
freq <- table(df_excel$NOTA_ENEN)
# table vai contar quantas vezes aparece cada
freq

#freq absoluta acumulada
freqAc <- cumsum(freq)

#freq Relativa
freqRel <- prop.table(freq)

#freq relativa acumulada
freqRelAc <- cumsum(freqRel)


tabela <- cbind(freq, freqAc, freqRel, freqRelAc)
tabela

#histograma frequencia de notas
hist(df_excel$NOTA_ENEN, ylab = "Qnt Pessoas", xlab = "Notas",
     main = "Frequência de Notas", col = "yellow")

# --------------------------------------------------------------

#notas em quartis
notas.cut <- cut(df_excel$NOTA_ENEN, 
                 breaks = quantile(df_excel$NOTA_ENEN), 
                 right = FALSE, include.lowest = TRUE)

notas_sexo <- table(notas.cut, df_excel$TP_SEXO)

barplot(notas_sexo, ylim = c(0, 12000), legend = TRUE, beside = TRUE,
        col = c("red", "orange", "green", "blue"), 
        ylab = "freq absoluta dos sexos", xlab = "Sexos")

# --------------------------------------------------------------

pie(table(df_excel$TP_SEXO), col = c("violet", "green"), 
    main = "Gráfico dos Sexos")
barplot(table(df_excel$TP_COR_RACA), ylim = c(0, 50000), 
        ylab = "Freq Absoluta", xlab = "Cor", main = "Qnt / cor")


# --------------------------------------------------------------

#escolher treineiros e a escolha da lingua (ingles ou espanhol)
#quem teve treinamento tem a tendência de escolher o que?
# quantidade de pessoas que são treineiros ou não
pie(table(df_excel$IN_TREINEIRO), col = c("violet", "green"),
    main = "qnt Treineiros")

#tbl_TL <- cbind(treineiro, lingua)
tabela_TL <- table(df_excel$TP_LINGUA, df_excel$IN_TREINEIRO)
tabela_TL

barplot(tabela_TL, ylim = c(0, 50000), legend = TRUE, beside = TRUE,
        col = c("green", "cyan"), ylab = "Frequência linguas",
        xlab = "Treineiro")
