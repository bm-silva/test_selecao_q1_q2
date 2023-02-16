##############################
## Questão 1: Faça o código abaixo funcionar corretamente
##############################

#################################
# data
iris_data <- iris[,1:4]
iris_data

#################################
# histograma
hist(iris_data$Petal.Length)

# qqplot
qqnorm(iris_data$Petal.Length, pch = 1, frame = FALSE)
qqline(iris_data$Petal.Length, col = "steelblue", lwd = 2)

## shapiro test 
resultado_shapiro = shapiro.test(iris_data$Petal.Length)

## Explore a variavel "resultado_shapiro" e substitua o "??" pela variavel referente ao p-value
p_valor = resultado_shapiro$p.value # BMS: Foi adicionado resultado_shapiro$p.value

# p-value ? maior que 0.05?
p_valor > 0.05

#################################
# histograma
hist(iris_data$Petal.Width)

# qqplot
qqnorm(iris_data$Petal.Width, pch = 1, frame = FALSE)
qqline(iris_data$Petal.Width, col = "steelblue", lwd = 2) # BMS: Foir alterado as palavras: qqline e width

## shapiro test 
resultado_shapiro = shapiro.test(iris_data$Petal.Width)

## Explore a variavel "resultado_shapiro" e substitua o "??" pela variavel referente ao p-value
p_valor = resultado_shapiro$p.value # BMS: Foi adicionado resultado_shapiro$p.value
  
# p-value ? maior que 0.05?
p_valor > 0.05



##############################
## Questão 2: Leia com atenção e faça o que foi pedido
##############################

# Faça uma função chamada "buscador" que recebe duas váriaveis, uma chamada "frases"
#  e a outra chamada de "palavra". A variavel "frases" é uma lista com várias sequencias
# de strings. A Função "buscador" deve verificar se a variavel "palavra" está presente em cada 
# uma das strings da varivel "frases". Ao final, a função deve retormar uma lista dentro da variavel 
# "resultado" que contem apenas as strings no qual a variavel "palavra" foi encontrada. Por exemplo:

# Se temos os seguintes conteudos paras as variaveis
# frases = c("O ITV esta realizando um processo seletivo", 
#           "O ITV é referencia em vários estudos",
#            "Processo avaliativo de modelos precisa ser feito com cautela")

# palavra = "processo"


# A variavel "resultado" deve retornar o seguinte: c("O ITV esta realizando um processo seletivo",
# "Processo avaliativo de modelos precisa ser feito com cautela")

buscador = function(palavra, frases){
  resultado = frases[grepl(palavra, frases, ignore.case = TRUE)]
  return(as.vector(resultado))
}

# Faça conforme foi pedido:
frases = c("O ITV esta realizando um processo seletivo", 
           "O itv é referencia em vários estudos",
           "R e python são linguagens poderoras",
           "No ITV existe uma grande variedade de projetos")

palavra = "itv"

buscador(palavra, frases)
