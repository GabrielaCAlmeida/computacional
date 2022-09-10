# entrada de dados ----
library(tidyverse)

dados <- read.csv2("data.csv",
                   col.names = c("lote", "defeituosos")) %>%
  mutate(prop = defeituosos/500)

# metodo EM ----

# Inicializar θA e θB tomando elemento da amostra ao acaso e dividindo por n, e p 
# usando uma distribuição uniforme(0,1). Parar o algoritmo quando, para todo i,
# |γi(k+1)− γi(k) | < 10−9.

#inicializando thetaA e thetaB
set.seed(1234)
thetaA <- sample(dados$prop, 1)
thetaB <- sample(dados$prop, 1)
p <- runif(1)


## prepara dados de iteração com primeiro gamma ----
itera <- dados %>% #gamma k+1
  mutate(gamma1 = p*dbinom(defeituosos, size = 500, prob = thetaB)/
           ((1-p)*dbinom(defeituosos, size = 500, prob = thetaA)+
              p*dbinom(defeituosos, size = 500, prob = thetaB)),
         um_menos_gama1 = 1-gamma1)

itera$dif <- 1 #dif inicial para não travar o while

## inicia iterações ----

contador <- 0

while(any(itera$dif > 10^(-9)) == TRUE){
  
  contador <- contador+1
  
  itera$gamma2 <- itera$gamma1 #guarda o gama k
  
  ## M step ----
  
  thetaA <- sum(itera$defeituosos*(itera$um_menos_gama1))/(500*sum(itera$um_menos_gama1))
  thetaB <- sum(itera$defeituosos*(itera$gamma1))/(500*sum(itera$gamma1))
  p <- sum(itera$gamma1)/500
  
  ## E step ----
  
  itera <- itera %>%
    mutate(gamma1 = p*dbinom(defeituosos, size = 500, prob = thetaB)/
             ((1-p)*dbinom(defeituosos, size = 500, prob = thetaA)+
                p*dbinom(defeituosos, size = 500, prob = thetaB)),
           um_menos_gama1 = 1-gamma1)
  
  itera$dif <- itera$gamma1 - itera$gamma2
}

## funcao de probabilidade ----
Py <- function(y){
  return((1-p)*dbinom(y, size = 500, prob = thetaA)+ p*dbinom(y, size = 500, prob = thetaB))
}

## histograma ----

dados_plot <- data.frame(x = dados$defeituosos, y = Py(dados$defeituosos))

histograma_em <- ggplot(dados, aes(x = defeituosos))+
  geom_histogram(aes(y = ..density..),bins = 25, color = "white")+
  geom_point(data = dados_plot, aes(x = x, y = y))+
  labs(y = "Densidade", x = "Defeituosos")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.05))+
  theme_bw()+
  theme(#panel.grid = element_blank()#,
        # panel.border = element_blank(),
        # axis.line = element_line()
        )


# Amostrador de Gibbs ----
alfaa <- 1
alfab <- 1
betaa <- 1
betab <- 1
alfap <- 1
betap <- 1
B <- 10000
S <- 10000

## inicializando os parametros ----
set.seed(1234)
thetaa <- rbeta(1,1,1)
thetab <- rbeta(1,1,1)
p2 <- rbeta(1,1,1)

## iterações ----

delta <- p2

### delta ----

### atualização dos parametros ----

## media das S ultimas amostras ----