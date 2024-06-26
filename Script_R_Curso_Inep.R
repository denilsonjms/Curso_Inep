###############################################################
################# Instalando Pacotes ##########################
###############################################################

if(!require(ltm)) install.packages("ltm", dependencies=TRUE)
if(!require(mirt)) install.packages("mirt", dependencies=TRUE)
if(!require(psych)) install.packages("psych", dependencies=TRUE)
if(!require(ShinyItemAnalysis)) install.packages("ShinyItemAnalysis", dependencies=TRUE)


###############################################################
################# Carregando Pacotes ##########################
###############################################################

library(ltm)
library(mirt)
library(psych)
library(ShinyItemAnalysis)

###############################################################
########################## Figura 1 ###########################
###############################################################

#Usando o conjunto de dados "dataMedical" do pacote "ShinyItemAnalysis"
dados=dataMedical[,1:50]
plot(descript(dados),items=c(49,30,48),type="b",includeFirstLast=TRUE,
xlab = "Escore Total", ylab = "Proporção de acerto (Índice de Dificuldade)",
col = c("blue", "red", "green"))


###############################################################
########################## Figura 2 ###########################
###############################################################

# Definindo os parâmetros a, b e c para um item específico
a <- 2  # Discriminação
b <- 0    # Dificuldade
c <- 0.1  # Acerto ao acaso

# Criando uma função para calcular a CCI
calculate_cci <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Definindo a faixa de habilidades (theta)
theta <- seq(-3, 3, length.out = 100)

# Calculando a probabilidade de resposta correta para a faixa de habilidades
prob <- calculate_cci(theta, a, b, c)

# Plotando a CCI
plot(theta, prob, type = "l", lwd = 3, col = "blue", ylim = c(0, 1),
     xlab = "Habilidade (θ)", ylab = "Probabilidade de Resposta Correta", 
     main = "Curva Característica do Item (CCI)")

# Adicionando uma linha horizontal para o parâmetro c (pseudo-chance level)
abline(h = c, col = "red", lty = 2)
text(x = -2.7, y = c, labels = paste("c =", round(c, 2)), pos = 3, col = "red")

# Adicionando uma linha vertical para o parâmetro b (dificuldade do item)
abline(v = 0:0.55, h=0.55, col = "blue", lty = 2)
text(x = b, y = 0, labels = paste("b =", round(b, 2)), pos = 4, col = "blue")

# Adicionando um ponto e texto no ponto (b, 0.5) para ilustrar b no eixo x
points(x = b, y = 0.55, col = "purple", pch = 19)

text(x = 0.3, y = 0.6, labels = paste("a =", round(a, 2)), pos = 4, col = "green")



###############################################################
########################## Figura 3 ###########################
###############################################################


# Definindo os parâmetros constantes b e c
b <- 0  # Dificuldade
c <- 0.1  # Acerto ao acaso

# Definindo diferentes valores para o parâmetro a (discriminação)
a_values <- c(1, 1.8, 3.5)

# Criando uma função para calcular a CCI
calculate_cci <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Definindo a faixa de habilidades (theta)
theta <- seq(-3, 3, length.out = 100)

# Calculando as probabilidades de resposta correta para cada valor de a
cci_curves <- sapply(a_values, function(a) calculate_cci(theta, a, b, c))

# Plotando as CCIs
plot(theta, cci_curves[,1], type = "l", lwd = 2, col = "blue", ylim = c(0, 1), 
     xlab = "Habilidade (θ)", ylab = "Probabilidade de Resposta Correta", 
     main = "CCIs com diferentes valores de a (discriminação)")
lines(theta, cci_curves[,2], lwd = 2, col = "red")
lines(theta, cci_curves[,3], lwd = 2, col = "green")

# Adicionando uma legenda
legend("bottomright", legend = paste("a =", a_values), col = c("blue", "red", "green"), lwd = 2)



###############################################################
########################## Figura 4 ###########################
###############################################################


# Definindo os parâmetros constantes a e c
a <- 2.5  # Discriminação
c <- 0.15  # Acerto ao acaso

# Definindo diferentes valores para o parâmetro b (dificuldade)
b_values <- c(-1.5, 0, 1.5)

# Criando uma função para calcular a CCI
calculate_cci <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Definindo a faixa de habilidades (theta)
theta <- seq(-3, 3, length.out = 100)

# Calculando as probabilidades de resposta correta para cada valor de b
cci_curves <- sapply(b_values, function(b) calculate_cci(theta, a, b, c))

# Plotando as CCIs
plot(theta, cci_curves[,1], type = "l", lwd = 2, col = "blue", ylim = c(0, 1), 
     xlab = "Habilidade (θ)", ylab = "Probabilidade de Resposta Correta", 
     main = "CCIs com diferentes valores de b (dificuldade)")
lines(theta, cci_curves[,2], lwd = 2, col = "red")
lines(theta, cci_curves[,3], lwd = 2, col = "green")

# Adicionando uma legenda
legend("bottomright", legend = paste("b =", b_values), col = c("blue", "red", "green"), lwd = 2)


###############################################################
########################## Figura 5 ###########################
###############################################################


# Definindo os parâmetros constantes a e b
a <- 1.5  # Discriminação
b <- 0  # Dificuldade

# Definindo diferentes valores para o parâmetro c (acerto ao acaso)
c_values <- c(0.1, 0.2, 0.3)

# Criando uma função para calcular a CCI
calculate_cci <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Definindo a faixa de habilidades (theta)
theta <- seq(-3, 3, length.out = 100)

# Calculando as probabilidades de resposta correta para cada valor de c
cci_curves <- sapply(c_values, function(c) calculate_cci(theta, a, b, c))

# Plotando as CCIs
plot(theta, cci_curves[,1], type = "l", lwd = 2, col = "blue", ylim = c(0, 1), 
     xlab = "Habilidade (θ)", ylab = "Probabilidade de Resposta Correta", 
     main = "CCIs com diferentes valores de c (acerto a acaso)")
lines(theta, cci_curves[,2], lwd = 2, col = "red")
lines(theta, cci_curves[,3], lwd = 2, col = "green")

# Adicionando uma legenda
legend("bottomright", legend = paste("c =", c_values), col = c("blue", "red", "green"), lwd = 2)



###############################################################
########################## Figura 6 ###########################
###############################################################


# Definindo os parâmetros a, b e c para um item específico
a <- 1.5  # Discriminação
b <- 0    # Dificuldade
c <- 0  # Acerto ao acaso

# Criando uma função para calcular a probabilidade de resposta correta no modelo 3PL
calculate_prob_3pl <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Criando uma função para calcular a informação do item no modelo 3PL
calculate_item_information <- function(theta, a, b, c) {
  P_theta <- calculate_prob_3pl(theta, a, b, c)
  return((a^2 * (1 - P_theta) * (P_theta - c)^2) / ((1 - c)^2 * P_theta))
}

# Definindo a faixa de habilidades (theta)
theta <- seq(-3, 3, length.out = 100)

# Calculando a informação do item para a faixa de habilidades
information <- sapply(theta, function(t) calculate_item_information(t, a, b, c))

# Plotando a Curva de Informação do Item
plot(theta, information, type = "l", lwd = 2, col = "blue", ylim = c(0, 0.6),
     xlab = "Habilidade (θ)", ylab = "Informação do Item", 
     main = "Curva de Informação do Item (3PL)")

# Adicionando linhas e textos para destacar os parâmetros
abline(v = b, col = "red", lty = 2) # Linha vertical no ponto de dificuldade b
text(x = b, y = max(information) * 0.9, labels = paste("b =", round(b, 2)), pos = 4, col = "red")

abline(h = 0, col = "black", lty = 2) # Linha horizontal no eixo y
abline(h = max(information), col = "black", lty = 2) # Linha horizontal no pico da informação

# Adicionando pontos de destaque para os parâmetros
points(x = b, y = max(information), col = "purple", pch = 19)
text(x = b, y = max(information), labels = paste("Pico da Informação"), pos = 3, col = "purple")


###############################################################
########################## Figura 7 ###########################
###############################################################


# Definindo os parâmetros a, b e c para vários itens
parameters <- list(
  list(a = 1.5, b = 0, c = 0.2),
  list(a = 1.0, b = -1, c = 0.2),
  list(a = 2.0, b = 1, c = 0.2),
  list(a = 0.5, b = 0.5, c = 0.1)
)

# Criando uma função para calcular a probabilidade de resposta correta no modelo 3PL
calculate_prob_3pl <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Criando uma função para calcular a informação do item no modelo 3PL
calculate_item_information <- function(theta, a, b, c) {
  P_theta <- calculate_prob_3pl(theta, a, b, c)
  return((a^2 * (1 - P_theta) * (P_theta - c)^2) / ((1 - c)^2 * P_theta))
}

# Definindo a faixa de habilidades (theta)
theta <- seq(-3, 3, length.out = 100)

# Plotando as Curvas de Informação dos Itens
plot(NULL, xlim = c(-3, 3), ylim = c(0, 0.7), xlab = "Habilidade (θ)", ylab = "Informação do Item",
     main = "Curvas de Informação dos Itens (3PL)")

# Definindo cores para as diferentes curvas
colors <- c("blue", "red", "green", "purple")

# Iterando sobre os parâmetros e plote cada curva de informação
for (i in 1:length(parameters)) {
  a <- parameters[[i]]$a
  b <- parameters[[i]]$b
  c <- parameters[[i]]$c
  
  information <- sapply(theta, function(t) calculate_item_information(t, a, b, c))
  
  lines(theta, information, lwd = 2, col = colors[i])
  text(x = -3, y = 0.65 - 0.1 * i, labels = paste("a =", a, "b =", b, "c =", c), col = colors[i], pos = 4)
}

# Adicionando uma legenda
legend("topright", legend = paste("Item", 1:length(parameters)), col = colors, lwd = 2)



###############################################################
########################## Figura 8 ###########################
###############################################################


# Definindo os parâmetros
set.seed(123)
n <- 1000 # Número de indivíduos em cada grupo
theta <- rnorm(n) # Habilidade latente
alpha <- 1.5 # Parâmetro de discriminação

# Parâmetros de dificuldade para os dois grupos
beta_ref <- -0.5 # Grupo de referência
beta_focal <- -0.8 # Grupo focal (DIF presente)

# Função para calcular a probabilidade de acerto
p_function <- function(alpha, beta, theta) {
  exp(alpha * (theta - beta)) / (1 + exp(alpha * (theta - beta)))
}

# Valores de theta para as CCIs
theta_values <- seq(-3, 3, by = 0.1)

# Probabilidades para cada grupo ao longo de theta
prob_ref <- p_function(alpha, beta_ref, theta_values)
prob_focal <- p_function(alpha, beta_focal, theta_values)

# Plotando CCIs usando a função plot
plot(theta_values, prob_ref, type = "l", col = "blue", lwd = 2,
     xlab = "Habilidade (θ)", ylab = "Probabilidade de Resposta Correta",
     main = "DIF Uniforme")
lines(theta_values, prob_focal, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Referência", "Focal"), col = c("blue", "red"), lwd = 2, lty = c(1, 2))


###############################################################
########################## Figura 9 ###########################
###############################################################


# Definindo os parâmetros
set.seed(123)
n <- 1000 # Número de indivíduos em cada grupo
theta <- rnorm(n) # Habilidade latente

# Parâmetros de discriminação e dificuldade para os dois grupos
alpha_ref <- 1.5 # Parâmetro de discriminação para o grupo de referência
beta_ref <- -0.5 # Parâmetro de dificuldade para o grupo de referência

alpha_focal <- 1.0 # Parâmetro de discriminação para o grupo focal (DIF não uniforme)
beta_focal <- -0.5 # Parâmetro de dificuldade para o grupo focal

# Função para calcular a probabilidade de acerto
p_function <- function(alpha, beta, theta) {
  exp(alpha * (theta - beta)) / (1 + exp(alpha * (theta - beta)))
}

# Valores de theta para as CCIs
theta_values <- seq(-3, 3, by = 0.1)

# Probabilidades para cada grupo ao longo de theta
prob_ref <- p_function(alpha_ref, beta_ref, theta_values)
prob_focal <- p_function(alpha_focal, beta_focal, theta_values)

# Plotando CCIs usando a função plot
plot(theta_values, prob_ref, type = "l", col = "blue", lwd = 2,
      xlab = "Habilidade (θ)", ylab = "Probabilidade de Resposta Correta",
     main = "DIF Não Uniforme")
lines(theta_values, prob_focal, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Referência", "Focal"), col = c("blue", "red"), lwd = 2, lty = c(1, 2))














