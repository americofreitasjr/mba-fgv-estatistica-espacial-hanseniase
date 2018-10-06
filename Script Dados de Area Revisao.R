#-------------------------------------------------------------------------------------#
# ------------------------------- Script: Dados de Area ------------------------------#
#-------------------------------------------------------------------------------------#

#Nao esqueca de iniciar sua pasta de trabalho!!! Faca o R "enxergar" o diretorio
#no qual voce ira trabalhar.

#setwd("/Users/jony/Documents/FGV/MBA/2 Aplicacoes de Estatistica Espacial/Aula/Codigos em R/2 Revisao")

#Carregando o pacote rgdal
library(rgdal)

## Lendo o shapefile com os bairros da cidade de Recife
RJ = readOGR("municipiosrj.shp")

## Plotando o shapefile com todos os bairros da cidade de Recife
plot(RJ, axes = TRUE)

#Carregando o pacote tidyverse
library(tidyverse)

#Lendo o arquivo de dados
#Registro das vendas vendas do produto de interesse
base = read_csv("hanseniase.csv")

#Visualinado a base de dados
base

#Fazendo a relacao dos dois data frames
RJ@data = left_join(RJ@data, base, by = "ID_OBJETO")

#Visualizando o data frame existente em recife acrescido da variavel 
head(RJ@data)

#Carregando o pacote sp e RColorBrewer
library(sp)
library(RColorBrewer)
library(cartography)
library(tmap) 

library(cartogram)

#-------------------------------------------------------------------------------------#
# ----------------- Mapa Cartograma: Numero de casos de hanseniase -------------------#
#-------------------------------------------------------------------------------------#

dados_cartograma = cartogram(RJ,  weight = "Casos", itermax = 5)

## cartogram - constroi cartogramas com base no algoritmo de rubber sheet distortion
#Argumentos:
#shp - um objeto da classe SpatialPolygonsDataFrameclass
#weight - o nome da variavel contida em shp que sera utilizada para o peso
#itermax - numero maximo de interacoes para a transformacao do cartograma

tm_shape(shp = dados_cartograma) +
  tm_polygons(col = "Casos",
              style="quantile", # "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", and "jenks"
              palette = brewer.pal(9, "YlOrRd"), title=c("Numero de casos")) +
  tm_layout(frame=FALSE)

#Argumentos:
#shp - um objeto da classe SpatialPolygonsDataFrameclass
#col - a variavel que deseja plotar
#style - tipo de divisao para criar os intervalos
#palette - paleta de cores
#title - Titulo da legenda
#frame - FALSE faz com que um quadro ao redor da figura nao seja plotado


#-------------------------------------------------------------------------------------#
# ------------------ Mapas coropleticos: Numero de casos de hanseniase  -----------------#
#-------------------------------------------------------------------------------------#


#Plotando o mesmo mapa coropletico, usando funcoes diferentes

#Usando a funcao spplot

#Criando os intervalos com base em quantis
intervalos1=quantile(RJ$Casos, probs = seq(0,1,0.125))
intervalos1[9] = intervalos1[9] + 1

#Plotando o mapa com tons avermelhados
spplot(obj = RJ, zcol = c("Casos"), at = intervalos1, col.regions =brewer.pal(8, "Reds")) #Outras opções de cores: Greens, BrBG, Accent


#Usando a funcao choroLayer 
par(mar = c(0.5,0.5,1.5,0.5))
plot(RJ)  
choroLayer(spdf = RJ, var = "Casos", border = "gray",  
           method = "equal", # "sd", "equal", "quantile", "fisher-jenks","q6" or "geom"  
           nclass = 5, lwd = 0.4, col = brewer.pal(5, "Reds"),  
           legend.pos = "topleft", legend.title.txt = "Numero de Casos", add= TRUE)  


## choroLayer - funcao para plotar dados espaciais com atributos
#Argumentos:
#spdf - um objeto da classe sf
#var - a variavel/atributo que deseja plotar
#border - cor das linhas que definem os poligonos
#method - o metodo que vai definir os intervalos de classes
#nclass - o numero de classes
#lwd - a espessura da linha dos poligonos
#col - vetor com as cores
#legend.pos - posicao da legenda
#legend.title.txt - titulo do texto
#add - para adicionar sobre o grafico que esta plotado


#-------------------------------------------------------------------------------------#
# -------------------- Criando a matriz W baseado em contiguidade  -------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote stringr      
library(spdep)

#W com o criterio queen
W.queen = poly2nb(pl = RJ, row.names = RJ$NOME, queen = TRUE)

## poly2nb - uma das funcoes para criar a matriz W
#Argumentos:
#pl - um objeto da classe SpatialPolygons
#row.names - um vetor indicando os rotulos das sub-regioes
#queen - se TRUE, cria o criterio queen

#Visualizando a quantidade de links criados
summary(W.queen)

#Extraindo as coordenadas
coordenadas = coordinates(obj = RJ)
head(coordenadas)

## coordinates - define coordenadas espaciais para criar um objeto espacial
#Argumentos:
#obj - um objeto da classe SpatialPolygons


#Plotando a estrutura de vizinhanca criada
plot(RJ, border = "grey")
plot(W.queen, coordenadas, add = TRUE, col = "red")

#Visualizando a matriz W com o criterio queen
mat.W.queen <- nb2mat(W.queen, style = "B")
colnames(mat.W.queen) <- rownames(mat.W.queen)
mat.W.queen[1:5,1:5]

#W com o criterio rook
W.rook = poly2nb(pl = RJ, row.names = RJ$NOME, queen = FALSE)

#Visualizando a quantidade de links criados
summary(W.rook)

#Plotando a estrutura de vizinhanca criada
plot(RJ, border = "grey")
plot(W.rook, coordenadas, add = TRUE, col = "yellow")


#Sobrepondo as duas estruturas de vizinhanca
plot(RJ, border = "grey")
plot(W.queen, coordenadas, add = TRUE, col = "red")
plot(W.rook, coordenadas, add = TRUE, col = "yellow")


#-------------------------------------------------------------------------------------#
# --------------------- Criando a matriz W baseado em distancia  ---------------------#
#-------------------------------------------------------------------------------------#

#Extraindo os k vizinhos mais proximos
k1viz = knearneigh(x = coordenadas, k = 1)
k3viz = knearneigh(x = coordenadas, k = 3)

## knearneigh - define os k vizinhos mais proximos
#Argumentos:
#x - objeto do tipo SpatialPoints
#k - numero de vizinhos

#Plotando a estrutura de vizinhanca criada
par(mfrow = c(1,2))
#Vizinhanca considerando o vizinho mais proximo
plot(RJ, border = "grey")
plot(knn2nb(knn = k1viz), coordenadas, add = TRUE, col = "blue")
#Vizinhanca considerando os 3 vizinhos mais proximos
plot(RJ, border = "grey")
plot(knn2nb(knn = k3viz), coordenadas, add = TRUE, col = "blue")
par(mfrow = c(1,1))

## knn2nb - define uma lista de vizinhanca de um objeto knn
#Argumentos:
#knn - um objeto retornado por knearneigh

#-------------------------------------------------------------------------------------#
# ---------------- Definindo os pesos associados aos elementos de W  -----------------#
#-------------------------------------------------------------------------------------#


## Lista de vizinhanca espacial com pesos
recWQW <- nb2listw(neighbours = W.queen, style="W") #outras opcoes: B, C, S e U
recWQW$weights

recWQB <- nb2listw(neighbours = W.queen, style="B") #outras opcoes: B, C, S e U
recWQB$weights

## nb2listw - define uma lista de vizinhanca com pesos espaciais
#Argumentos:
#neighbours - um objeto da classe nb
#style - tipo de peso


#-------------------------------------------------------------------------------------#
# ------------------------ Avaliando variavel x media movel  -------------------------#
#-------------------------------------------------------------------------------------#

#Padronizando os dados
casos_padronizada <- scale(x = base$Casos)
casos_W <- mat.W.queen %*% casos_padronizada

## scale - padroniza um conjunto de valores
#Argumentos:
#x - conjunto de valores a ser padronizado


#Plotando variavel versus media movel
plot(casos_padronizada, casos_W, pch = 19, lwd = 2, cex = 0.9, xlab = "Z", ylab = "WZ")
abline(v=0, h=0)

#-------------------------------------------------------------------------------------#
# ------------------------------ Autocorrelacao global  ------------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o indice de moram considerando uma estrutura de vizinhanca queen com padronizacao pelas linhas
moran.test(x = RJ$Casos,listw = recWQW)

## moran.test - calcula o indice global de moran
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#Calculando o indice de moram considerando uma estrutura de vizinhanca queen com pesos iguais
moran.test(RJ$Casos,recWQB)

#plotando a variavel com a variavel transformada considerando uma estrutura de vizinhanca queen com padronizacao pelas linhas
moran.plot(x = RJ$Casos,listw = recWQW, pch = 21, xlab = "Numero de casos", ylab = "Variavel Transformada ")

## moran.plot - plota a variavel versus os valores transformados em funcao de W
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#-------------------------------------------------------------------------------------#
# ------------------------------ Autocorrelacao local (LISA)  -------------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o Moran Local
moranlocREC = localmoran(x = RJ$Casos,listw = recWQW, na.action=na.exclude,zero.policy=TRUE)

## moranlocREC - calcula o indice local de moran
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#Visualizando as autocorrelacoes locais
head(moranlocREC)

