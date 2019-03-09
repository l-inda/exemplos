##### Capítulo 2: Gerenciando e Entendendo os Dados -------------------

##### Estruturas de dados em R --------------------

## Vetores -----

# criar vetores de dados para três pacientes médicos
subject_name <- c("Desconhecido", "Desconhecida", "José da Silva")
temperature <- c(36.7, 37, 38.6)
flu_status <- c(FALSE, FALSE, TRUE)

# acessar o segundo elemento no vetor de temperatura corporal
temperature[2]

## exemplos de acesso a itens de um vetor
# inclui itens no intervalo de 2 a 3
temperature[2:3]

# excluir o item 2 usando o sinal de menos
temperature[-2]

# use um vetor para indicar se deseja incluir item
temperature[c(TRUE, TRUE, FALSE)]

## Fatores -----

# adicionar fator sexo
gender <- factor(c("MASCULINO", "FEMININO", "MASCULINO"))
gender

# adicionar fator de tipo sanguíneo
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

# adicionar fator ordenado
symptoms <- factor(c("AMENA","MODERADA","SEVERA"),
                   levels = c("AMENA", "MODERADA", "SEVERA"),
                   ordered = TRUE)
symptoms

# verificar por sintomas maiores que moderados
symptoms > "MODERADA"

## Listas -----

# exibir informações para um paciente
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

# criar lista para um paciente
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

# exibir o paciente
subject1

## métodos para acessar uma lista

# obter um único valor da lista por posição (retorna uma sub-lista)
subject1[2]

# obter um único valor da lista por posição (retorna um vetor numérico)
subject1[[2]]

# obter um único valor de lista pelo nome
subject1$temperature

# obter vários itens da lista especificando um vetor de nomes
subject1[c("temperature", "flu_status")]

## acessar uma lista como um vetor
# obter valores 2 e 3
subject1[2:3]

## Data frames -----

# criar um data frame a partir dos dados do paciente médico

pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, symptoms, stringsAsFactors = FALSE)

# exibir o data frame
pt_data

## acessando um data frame

# obter uma única coluna
pt_data$subject_name

# obter várias colunas especificando um vetor de nomes
pt_data[c("temperature", "flu_status")]

# este é igual ao anterior, extraindo a temperatura (temperature) e o índice de febre (flu_status)
pt_data[2:3]

# acessando por linha e coluna
pt_data[1, 2]

# acessando várias linhas e várias colunas usando vetores
pt_data[c(1, 3), c(2, 4)]

## Deixar uma linha ou coluna em branco para extrair todas as linhas ou colunas

# coluna 1, todas as linhas
pt_data[, 1]
# coluna 1, todas as colunas
pt_data[1, ]
# todas as linhas e todas as colunas
pt_data[ , ]

# os código seguintes são equivalentes
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5, -6)]

## Matrizes -----

# crie uma matriz 2x2
m <- matrix(c(1, 2, 3, 4), nrow = 2)
m

# equivalente ao comando anterior
m <- matrix(c(1, 2, 3, 4), ncol = 2)
m

# criar uma matriz 2x3
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m

# criar uma matriz 3x2
m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
m

# extrair valores de matrizes
m[1, 1]
m[3, 2]

# extrair linhas
m[1, ]

# extrir colunas
m[, 1]

##### Gerenciando dados com R ------------

## salvando, carregando e removendo estruturas de dados em R

# mostra todas as estruturas de dados na memória
ls()

# remove os objetos m e subject1
rm(m, subject1)
ls()

# limpar todos os objetos
rm(list=ls())

##### Explorando e entendendo dados --------------------

## exemplo de exploração de dados usando os dados de carros usados
usedcars <- read.csv(paste0(getwd(),"/data/usedcars.csv"), stringsAsFactors = FALSE)

# obter a estrutura de dados de carros usados
str(usedcars)

## Explorando variáveis numéricas -----

# sumariza as variáveis numéricas
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# calcular a renda média
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

# a renda mediana
median(c(36000, 44000, 56000))

# valores mínimo / máximo dos preços de carros usados
range(usedcars$price)

# a diferença do intervalo
diff(range(usedcars$price))

# IQR para preços de carros usados
## IQR significa "Interquartile Range" (em português, Variação Interquartil) 
IQR(usedcars$price)

# use quantil para calcular o resumo dos cinco números
quantile(usedcars$price)

# o percentil 99
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintis
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# boxplot de preços de carros usados e suas quilometragens
# nessa versão, gerando um arquivo .png
png(file = paste0(getwd(),"/data/figures/preco-carros-usados.png"))
boxplot(usedcars$price, main="Boxplot dos Preços dos Carros Usados",
      ylab="Preço ($)")
dev.off()

png(file = paste0(getwd(),"/data/figures/quilometragem-carros-usados.png"))
boxplot(usedcars$mileage, main="Boxplot da Quilometragem dos Carros Usados",
      ylab="Odômetro (milhas)")
dev.off()

# histogramas dos preços e da quilometragem dos carros usados 
png(file = paste0(getwd(),"/data/figures/hist-precos-carros-usados.png"))
hist(usedcars$price, main = "Histograma dos Preços dos Carros Usados",
     xlab = "Preço ($)")
dev.off()

png(file = paste0(getwd(),"/data/figures/hist-quilom-carros-usados.png"))
hist(usedcars$mileage, main = "Histograma da Quilometragem dos Carros Usados",
     xlab = "Odômetro (milhas)")
dev.off()

# variância e desvio padrão dos dados de carros usados
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Explorando variáveis numéricas -----

# tabelas unidirecionais para os dados de carros usados
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# calcular proporções da tabela
model_table <- table(usedcars$model)
prop.table(model_table)

# arredondando os dados
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Explorando relações entre variáveis -----

# gráfico de dispersão de preço versus milhagem
png(file = paste0(getwd(),"/data/figures/disp-preco-milhagem-carros-usados.png"))
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Gráfico de Dispersão de Preço vs. Milhagem",
     xlab = "Odômetro do Carro Usado (mi.)",
     ylab = "Preço do carro usado ($)")
dev.off()

# nova variável que indica cores conservadoras
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")

# verificando nossa variável
table(usedcars$conservative)

# Tabela de referência cruzada de cor conservadora por modelo
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)
