##### Capítulo 3: Classificação usando vizinhos mais próximos --------------------

## Exemplo: Classificação de amostras de câncer ----
## Etapa 2: Explorando e preparando os dados ----

# importar o arquivo CSV
wbcd <- read.csv(paste0(getwd(),"/data/wisc_bc_data.csv"), stringsAsFactors = FALSE)

# examinar a estrutura do data frame 'wbcd'
str(wbcd)
#'data.frame':	569 obs. of  32 variables:
#  $ id     : int  87139402 8910251 905520 868871 9012568 906539 925291 87880 862989 89827 ...
#$ diagnosis: chr  "B" "B" "B" "B" ...

# retirar o campo id (é o primeiro campo do data frame)
wbcd <- wbcd[-1]

# tabela de diagnóstico
table(wbcd$diagnosis)

# recodificar o diagnóstico como 'fator'
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benigno", "Maligno"))

# tabela ou proporções com os rótulos mais informativos
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# sumarizar três campos numéricos
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# criar função de normalização
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# teste da função de normalização - o resultado deve ser idêntico
normalize(c(1, 2, 3, 4, 5))

normalize(c(10, 20, 30, 40, 50))

# normalizar os dados no data frame wbcd
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# confirmar se a normalização funcionou!
summary(wbcd_n$area_mean)

# criar dados de treinamento de teste
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# criar rótulos para os dados de treinamento e de testes 

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Etapa 3: Treinando um modelo sobre os dados ----

# carrega a biblioteca "class"
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

## Etapa 4: Avaliando o desempenho do modelo ----

# carrega a biblioteca "gmodels"
library(gmodels)

# Criar a tabulação cruzada do previsto x realizado
##A tabulação cruzada é uma ferramenta que permite comparar a relação entre duas variáveis
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Passo 5: Melhorando o desempenho do modelo ----

# use a função scale () para padronizar o z-score do data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirmar se a transformação foi aplicada corretamente
summary(wbcd_z$area_mean)

# criar dados de treinamentos e de teste 
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# reclassificar os casos de teste
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Criar a tabulação cruzada do previsto x realizado
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# experimentar valores diferentes de k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
