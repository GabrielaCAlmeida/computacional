# funcao geradora de amostras ----
#funcao para facilitar teste de argumento ORDEM na funcao
`%notin%` <- Negate(`%in%`)

gera.amostra <- function(size, ordem, seed){
  #opcoes de argumento
  opcoes <- c("ordenado", "invertido", "aleatorio", "parcial")
  #testa ordem da funcao
  if (ordem %notin% opcoes) { 
    stop("Selecione uma das opcoes possíveis: \nordenado, invertido, aleatorio ou parcial.",
         call. = FALSE)}
  
  if(missing(seed)){message("Argumento não usado: random seed.")} 
  else {set.seed(seed)}
  
  
  #testa tamanho da amostra
  if (size - as.integer(size) != 0){
    warning("Tamanho de amostra será convertido para o \nmenor número inteiro mais próximo.",
            call. = FALSE)
  }
  #converte para realizar os calculos
  size <- as.integer(size)

  #gera amostra
  if (ordem == "ordenado"){
    vec <- 1L:size
} else if (ordem == "invertido"){
    vec <- size:1L
} else if (ordem == "aleatorio"){
    vec <- sample(c(1L:size), size)
} else if (ordem == "parcial") {
  vec <- 1L:size
  amostra <- sample(vec, size = as.integer(size*0.1))
  locais <- sample(as.integer(size*0.1)) 
  antigos_velhos <- matrix(c(amostra, locais), ncol = 2)
  vec[locais] <- antigos_velhos[,1]
  vec[amostra] <- antigos_velhos[,2]
    
  }
  return(list(amostra = vec, ordem = ordem))
}
## exemplos de amostras ----
gera.amostra(15L, "parcial")
gera.amostra(15.2, "aleatorio", seed = 1234)
gera.amostra(10L, "cassildis")

# Metodos de ordenacao ----
# tempo de execução - Sys.time
# média de número de comparações entre elementos do vetor realizadas em 100 repetições
# média de numero de movimentações dos elementos do vetor em 100 repetições


#SE TIVER QUE SER COM A MESMA AMOSTRA, ARGUMENTOS: AMOSTRA, ORDEM, .show_all

## Selecao ----
selecao <- function(amostra, .show_all = FALSE){ 
  
  #amostra <- gera.amostra(size, ordem) - removido
  
  x <- amostra$amostra
  size <- length(amostra$amostra)
  
  # if (length(x) == 1){ - removido. Considerar apenas as amostrar geradas, numéricas
  #   x <- as.character(x)
  #   x <- unlist(strsplit(tolower(x), split = "*"))
  # }
  
  comparacoes <- 0
  movimentacoes <- 0 #copias, reatribuicoes
  t1 <- Sys.time()

  #tamanho do vetor
  n <- length(x)
  #loop de ordenacao
  for(i in 1:(n-1)){ #para i de 1 ao penultimo
    min = i#considere o primeiro como o menor
    for(j in (i+1):n){ #para j do segundo ao ultimo
      if(x[j] < x[min]) {#compara x[j] ao x[i] (considerado minimo)
        min <- j #se x[j] for menor, o indice minimo deve ser j
      }
      comparacoes <- comparacoes+1 #aumenta o contador de comparacoes
    }
    temp <- x[min] #objeto temporario com o menor valor - COPIA
    movimentacoes <- movimentacoes + 1
    
    #objetos originais para comparacao
    xmin_antigo <- x[min]
    xi_antigo <- x[i]
    
    #objetos novos apos reposicionamento - REPOSICIONAMENTOS (talvez)
    x[min] <- x[i] #antigo indice do minimo se torna o maior valor
    x[i] <- temp #antigo indice maior recebe valor minimo, 
                 ## armazenado na variavel temporaria
    
    #testa se houve reposicionamento
    if(xmin_antigo != x[min]){movimentacoes <- movimentacoes + 1}
    if(xi_antigo != x[i]){movimentacoes <- movimentacoes + 1}
  }
  t <- Sys.time()-t1
  
  if(.show_all == FALSE){
    return(data.frame(
      tamanho = size,
      ordenamento = amostra$ordem,
      # objeto_original = amostra,
      # objeto_ordenado = x,
      tempo = t,
      comparacoes = comparacoes,
      movimentacoes = movimentacoes)
    )
  } else {
    return(list(
      tamanho = size,
      ordenamento = amostra$ordem,
      objeto_original = amostra$amostra,
      objeto_ordenado = x,
      tempo = t,
      comparacoes = comparacoes,
      movimentacoes = movimentacoes)
    )
  }
}

### exemplo ----
amostra <- gera.amostra(20, "parcial", seed = 1234)
selecao(amostra)
selecao(amostra, .show_all = T)

## Insercao ----

## QuickSort ----

# Testes de comparação ----

# tamanhos de vetores: 0.5K, 1K, 5K, 10K, 50K
tamanhos <- c(500L, 1000L, 5000L, 10000L, 50000L)
# ordenações possiveis: ordenados, 10% desordenados, aleatorio, e ordem invertida
ordens_simples <- c("ordenado", "invertido")
ordens_medias <- c("parcial", "aleatorio")

## computa tabela de ordenacoes simples ----
comps_simples <- data.frame()

### loop de selecao ----
for(i in tamanhos[1:5]){ #poderia ser otimizado para nao gerar amostras toda vez
    for(j in ordens_simples){
     amostra <- gera.amostra(i,j, seed = 12345)
     comps_simples <- rbind(comps_simples, selecao(amostra))
  }
}
### loop de insercao----
### loop de quicksort----

## computa tabela com multiplas iteracoes nos casos aleatorizados ----
comps_iter <- data.frame()

### loop de selecao ----
for(k in 1:100){
  print(paste("k = ", k))
  for(i in tamanhos[1:5]){ #poderia ser otimizado para nao gerar amostras toda vez
    print(paste("i = ", i))
    for(j in ordens_medias){
      amostra <- gera.amostra(i,j, seed = 12345)
      comps_iter <- rbind(comps_iter, selecao(amostra))
    }
  }
}

### loop de insercao ----
###loop de quicksort ----

## calculo das médias dos casos aleatorizados ----