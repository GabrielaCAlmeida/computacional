# funcao geradora de amostras ----
gera.amostra <- function(size, ordem, seed){
  #funcao para facilitar teste de argumento ORDEM na funcao
  `%notin%` <- Negate(`%in%`)
  
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
    message("Tamanho de amostra será convertido para o \nmenor número inteiro mais próximo.")
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
gera.amostra(15L, "parcial") #uso correto sem seed
gera.amostra(15.2, "aleatorio", seed = 1234) #uso com decimal e com seed
gera.amostra(15L, "aleatorio", seed = 1234) #demonstra que o vetor é igual se o seed é igual
gera.amostra(15L, "aleatorio") #demonstra que o vetor é diferente sem seed fornecido
gera.amostra(10L, "cassildis") #demonstra interrupcao da execucao da funcao

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
      metodo = "selecao",
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
      metodo = "selecao",
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
#FALTA: MOVIMENTACOES, COMPARACOES, RETURNS

insercao <- function(amostra, .show_all = FALSE){
  x <- amostra$amostra
  size <- length(amostra$amostra)
  comparacoes <- 0
  movimentacoes <- 0 #copias, reatribuicoes
  t1 <- Sys.time()
  
  #tamanho do vetor
  n <- length(x)
  #loop de ordenacao
  for(i in 2:n){ # a partir de i = 2
    temp = x[i] # selecione o i-esimo
    movimentacoes <- movimentacoes+1
    j = i-1
    
    while(x[j] > temp && j > 0){ #
      comparacoes <- comparacoes+1
      x[j+1] <- x[j]
      movimentacoes <- movimentacoes+1
      j <- j-1
    }
    comparacoes <- comparacoes+1
    #1 comparacao se x[j] > temp == FALSE
    x[j+1] <- temp
    movimentacoes <- movimentacoes+1
  }
  
  
  t <- Sys.time()-t1
  
  if(.show_all == FALSE){
    return(data.frame(
      metodo = "insercao",
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
      metodo = "insercao",
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
insercao(amostra)
insercao(amostra, .show_all = T)

## QuickSort ----


quicksort_base <- function(vetor, esq = 1, dir = length(vetor), 
                      movimentacoes = 0, comparacoes = 0){
  
  pivot <- vetor[as.integer((esq+dir)/2)] # pega ponto médio como pivo
  i <- esq
  j <- dir
  
  while(i <= j){
    #compara i com j no while(i <= j)
    comparacoes <- comparacoes+1 
    
    # enquanto o pivo for maior que o numero à esquerda
    # avanca com i até achar um numero maior que o pivot
    # compara x[i] com pivot
    while(vetor[i] < pivot){
      i <- i+1 
      comparacoes <- comparacoes+1 
    }
    
    # enquanto o pivo for menor que o numero à direita
    # retrocede com j até achar um numero menor que o pivot
    # compara x[j] com pivot
    while(vetor[j] > pivot){ 
      j <- j-1 
      comparacoes <- comparacoes+1 
    }
    
    #se o índice i for menor que o índice j
    #realiza movimentacoes
    #prossegue com os contadores
    if(i <= j){ 
      temp <- vetor[i] 
      vetor[i] <- vetor[j]
      vetor[j] <- temp
      if(i<j){movimentacoes <- movimentacoes+3}
      i <- i + 1 
      j <- j - 1
    } 
    # #se os indices forem iguais
    # #cruza os apontadores
    # if(i == j){ 
    #   i <- i + 1 
    #   #j <- j - 1
    # }
  }
  
  # prints pra auditoria de um digito que estava escapando do ordenamento
  #print(vetor)
  #print(glue::glue("i = {i}, j = {j}, pivot = {pivot}, es = {esq}, dir = {dir}"))
  
  #quicksort da esquerda
  if(esq < j){
    resultado <- quicksort_base(vetor, esq = esq, dir = j, 
                           movimentacoes = movimentacoes, comparacoes = comparacoes)
    vetor <- resultado$vetor
    movimentacoes <- resultado$movimentacoes
    comparacoes <- resultado$comparacoes
  }
  #quicksort da direita
  if(i < dir){
    resultado <- quicksort_base(vetor, esq = i, dir = dir, 
                           movimentacoes = movimentacoes, comparacoes = comparacoes)
    vetor <- resultado$vetor
    movimentacoes <- resultado$movimentacoes
    comparacoes <- resultado$comparacoes
  }
  return(list(
    vetor = vetor,
    movimentacoes = movimentacoes,
    comparacoes = comparacoes
  ))
}

quick_completa <- function(amostra, .show_all = FALSE){
  
  vetor <- amostra$amostra
  
  t1 <- Sys.time()
  resultados <- quicksort_base(vetor)
  t <- Sys.time()-t1
  
  if(.show_all == FALSE){
    return(data.frame(
      metodo = "quicksort",
      tamanho = length(amostra$amostra),
      ordenamento = amostra$ordem,
      # objeto_original = amostra$amostra,
      # objeto_ordenado = resultados$vetor,
      tempo = t,
      comparacoes = resultados$comparacoes,
      movimentacoes = resultados$movimentacoes)
    )
  } else {
    return(list(
      metodo = "quicksort",
      tamanho = length(amostra$amostra),
      ordenamento = amostra$ordem,
      objeto_original = amostra$amostra,
      objeto_ordenado = resultados$vetor,
      tempo = t,
      comparacoes = resultados$comparacoes,
      movimentacoes = resultados$movimentacoes)
    )
  }
}

### exemplo
amostra <- gera.amostra(20, "parcial", seed = 1234)
quick_completa(amostra)
quick_completa(amostra, .show_all = T)

# Limpeza ----
rm(list=setdiff(ls(), c("gera.amostra", "selecao", "insercao", 
                        "quicksort_base", "quick_completa")))
gc()

# Testes de comparação ----

# tamanhos de vetores: 0.5K, 1K, 5K, 10K, 50K
tamanhos <- c(500L, 1000L, 5000L, 10000L, 50000L)
# ordenações possiveis: ordenados, 10% desordenados, aleatorio, e ordem invertida
ordens_simples <- c("ordenado", "invertido")
ordens_medias <- c("parcial", "aleatorio")


## computa tabela de ordenacoes simples se arquivo nao existe ----
if(!dir.exists("./dados gerados tp1/")){
  dir.create("./dados gerados tp1/")}

if(!file.exists("trabalho pratico 1/dados gerados tp1/comps_simples.Rdata")){

  comps_simples <- data.frame()
  
  ### loop de amostras nao aleatorizadas ----
    
  for(i in tamanhos[1:5]){ #poderia ser otimizado para nao gerar amostras toda vez
      for(j in ordens_simples){
       amostra <- gera.amostra(i,j, seed = 12345)
       comps_simples <- rbind(comps_simples, selecao(amostra), 
                              insercao(amostra), quick_completa(amostra))
    }
  }

  
  ### salva o arquivo, se nao existe ----
  saveRDS(comps_simples, "trabalho pratico 1/dados gerados tp1/comps_simples.Rdata")

} else {comps_simples <- readRDS("trabalho pratico 1/dados gerados tp1/comps_simples.Rdata")}

## computa tabela com multiplas iteracoes nos casos aleatorizados se nao existe ----
if(!file.exists("trabalho pratico 1/dados gerados tp1/comps_iter.Rdata")){

  comps_iter <- data.frame()
  
  ### loop amostras aleatorizadas
  for(k in 1:100){
    #print(paste("k = ", k)) - para auditoria
    for(i in tamanhos[1:5]){ #poderia ser otimizado para nao gerar amostras toda vez
      #print(paste("i = ", i)) - para auditoria
      for(j in ordens_medias){
        amostra <- gera.amostra(i,j, seed = k)
        comps_iter <- rbind(comps_iter, selecao(amostra), 
                            insercao(amostra), quick_completa(amostra))
        #COMO NO ANTERIOR, BOTAR TODOS NO RBIND
      }
    }
  }
  

  
  ### salva o arquivo, se nao existe ----
  saveRDS(comps_iter, "trabalho pratico 1/dados gerados tp1/comps_iter.Rdata")

} else {comps_iter <- readRDS("trabalho pratico 1/dados gerados tp1/comps_iter.Rdata")}

## calculo das médias dos casos aleatorizados ----
library(dplyr)
medias <-comps_iter %>%
  group_by(metodo, tamanho, ordenamento)%>%
  summarise(across(tempo:movimentacoes, mean))

saveRDS(medias, "trabalho pratico 1/dados gerados tp1/medias_iter.Rdata")

# limpeza do ambiente, deixar apenas funcoes e objetos finais

rm(list=setdiff(ls(), c("gera.amostra", "selecao", "insercao", 
                        "quicksort_base", "quick_completa",
                        "comps_iter", "comps_simples", "medias")))
gc()
