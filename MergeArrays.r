require(xts)
require(quantmod) 
require(nnet)
require(caret)
require(e1071)
require(XLConnect)
require(dplyr)

## Fazendo um merge das cotacoes dos ativos para agrupa-los por data, estando cada cotacao de bolsa
## em uma coluna, para facilitar a manipulacao dos dados

Agglutinate = function(BVSP,DJI,NIK,SHAN){
  
  dataframeObject=(merge.xts(BVSP,DJI,NIK,SHAN, all=TRUE, fill=NaN))
  dataframeObject=data.frame(index(dataframeObject),dataframeObject)
  colnames(dataframeObject) = c( "Date","BVSP","DJI","NIK","SHAN")
  
  return(dataframeObject);
}
