### FGV/EESP - Mestrado Profissional em Economia e Financas
### Disciplina: Metodos Quantitativos Aplicados         Turma: 2017/1
### Nome dos alunos: 
###Gustavo Falcin   	Codigo: 335723
###Raphael Ferreira  	Codigo: 335740
###Rodolfo Rosina    	Codigo: 335741
###
### Trabalho Final: Forecast de IBOV com Bolsas Asiaticas e dos EUA atraves de Machine-Learning
### Subject email: raphaelccf@hotmail.com;gustavofalcin@yahoo.com.br;rodolfo.rosina@yahoo.com.br

# install.packages("XLConnect")
# install.packages("xlsx")
# install.packages("e1071")
# install.packages("NeuralNetTools")
# install.packages("forecast")
# install.packages("nnet")
# install.packages("dplyr")
# install.packages("neuralnet")
# install.packages("tidyr")
# install.packages("caret")

require(quantmod); 
require(nnet);
require(caret);
require(e1071);
require(XLConnect);
require(dplyr);
require(NeuralNetTools);
require(xts);
require(nnet);
require(NeuralNetTools);
require(nnet);
require(scatterplot3d);
library(xlsx);

source("../lib/MergeArrays.r");
source("../lib/NATreatment.r");

## data de inicio
beginData = "2007-01-02";

#### TICKER DE CADA VARIAVEL CONFORME YAHOO FINANCE
## bolsas utilizadas no modelo:Bovespa, DowJonnes, Nikei, Shangai
getSymbols(c("^BVSP","^DJI","^N225","000001.SS"), from = beginData);

### Obtendo dados dos respectivos fechamentos
BVSP=BVSP$BVSP.Close;
DJI=DJI$DJI.Close;
NIK=N225$N225.Close;
SHAN=`000001.SS`;
SHAN =SHAN$`000001.SS.Close`;


### Fazendo a juncao dos vetores para mesma data
d=Agglutinate(BVSP,DJI,NIK,SHAN);

## Funcoes que tratam o NaN na primeira linha e ao longo da base de dados, repetindo em feriados
## as ultimas cotacoes disponiveis

## tratamento da primeira linha
d = firstLineNA(d);

## tratamento da segunda linha em diante
d = bodyNA(d);

## Fazendo as defasagens de Bovespa e DJI
BVSPl=Lag(d$BVSP,1)
DJIl=Lag(d$DJI,1)

## Incluindo na base com defasagens e datas
d = cbind(d,BVSPl,DJIl)
colnames(d) = c( "Date","BVSP","DJI","NIK","SHAN","BVSP1","DJIl")

## Utilizaremos somente os lags de BVSP e DJI
d=d[-1,-3]

### Estatistica descritiva da base (variaveis)
summary(d)

### Divide a base em treino e teste (80% para treino)
treino =d[1:ceiling(nrow(d)*0.8),]
test = d[-(1:ceiling(nrow(d)*0.8)),]

### Retira da coluna de data da base de treino
treino1=treino[,2:ncol(treino)]

## Bases para teste
x_test = test[ ,3:ncol(treino)]
y_test= test[["BVSP"]]

## Verifica como esta a ultima linha - se nao houver pregao 
tail(x_test)

## Modelo nnet

set.seed(1234)
nn1 = nnet(BVSP~.,data=treino1,size=15,linout=TRUE, rang=0.1,decay=5e-2,maxit=2000)

## Resultado do modelo
summary(nn1)

## Graficos da rede neural
## Apresenta a rede neural: inputs, camada unica, pesos, output
plotnet(nn1);

## Apresenta a importancia relativa de cada variavel na conexao de pesos  
olden(nn1);

## Plota o grafico dos residuos
plot(treino[,1], residuals(nn1),type="l",main="Residuo - treino",xlab="Dias",col="blue",lwd=1);

abline(h=2,col="red")

wts.in=nn1$wts
struct=nn1$n

## Faz a projecao ainda sem ter um dia a frente
prev = predict(nn1,x_test,type="raw",na.omit(x_test))

## Faz a tabela com projetado
final = as.data.frame(cbind(as.Date(test$Date),prev,y_test));
colnames(final)= c("Date","PrevBVSP","RealBVSP");
final[["Date"]] = as.Date(final[["Date"]]);

## Verificaco de existencia de NaN na base final
any(is.na(final));

up = final[,2] + (var(final[,2])^(0.5)/2);
low=final[,2] - (var(final[,2])^(0.5)/2)

## Faz a tabela com observardo
final1=cbind(final,up,low)

## Estatistica descritiva do projetado com observado
## Calcular erro quadratico medio do predict
equadmed = mean(abs(final1[1:ncol(final1)-1,"PrevBVSP"]-final1[1:ncol(final1)-1,"RealBVSP"]))

## Erro quadratico maximo
eqm = max(abs(final1[1:ncol(final1)-1,"PrevBVSP"]-final1[1:ncol(final1)-1,"RealBVSP"]))

## Erro quadratico maximo
eqm2 = min(abs(final1[1:ncol(final1)-1,"PrevBVSP"]-final1[1:ncol(final1)-1,"RealBVSP"]))

## Comentei o grafico em 2D para decidirmos qual apresentar: 2D ou 3D
# plot(final1[,1], final1[,3],type="l",main="BVSP:Previ X Real",xlab="Dias",ylab="Indice",col="blue",lwd=1.5);
# lines(final1[,1], final[,2],col="red",lwd=1.5);
# lines(final1[,1],final1[,4],col="grey");
# lines(final1[,1],final1[,5],col="grey");

## Grafico de BVSP - Previsto versus Realizado
graph3d1= scatterplot3d(x = final1[,1], y = final1[,3],main="BVSP:Previsao X Real", angle = 65,
          color="red", col.grid="lightblue", axis = TRUE, tick.marks = TRUE, xlab = "Dias", ylab = "Indice",
          label.tick.marks = TRUE, grid = TRUE, pch = 10, scale.y=0.75, type = "l", lwd=2);
graph3d1$points3d(x = final1[,1], y = final1[,2], col = "blue", pch = 10, type = "l",lwd=2);
graph3d1$points3d(x = final1[,1], y = final1[,4], col="gray", type = "l", pch = 10,lwd=1);
graph3d1$points3d(x = final1[,1], y = final1[,5],col="gray", type = "l", pch = 10, lwd=1);

#valor final do BVSP projetado 1 dia a frente/ou no mesmo dia
print(final1[nrow(final1),"PrevBVSP"]);

##Criando tabela com as estatisticas
#coeficiente da variacao
CoefVariacao = sd(treino1$BVSP)/mean(treino1$BVSP);

media = mean(abs(residuals(nn1)));
estatisticas  = matrix(data = c(media, CoefVariacao, equadmed,eqm,eqm2, final1[nrow(final1),"PrevBVSP"]), nrow = 1, ncol = 6);
colnames(estatisticas) <- c ("Erro. Qd. Medio Treino", "CoefVariacao Treino", "Erro Qd. Medio Teste", "Max Erro Qd", "Min Erro Qd", "ProjBVSP" );


##Mudando diretorio para salvar a planilha do excel
setwd("../database");
##Criando uma planilha no excel com os resultas e estatisticas
write.xlsx(x = as.data.frame(final1) , file="final1.xlsx",sheetName = "Final",row.names = FALSE);
write.xlsx(x = as.data.frame(estatisticas) , file="final1.xlsx",sheetName = "Estatisticas", append = TRUE,row.names = FALSE);
