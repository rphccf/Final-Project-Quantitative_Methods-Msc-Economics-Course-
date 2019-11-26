require(xts)

## No Loop abaixo e feito o tratamento para a existencia de um NaN na primeira linha, o que prejudicaria o desenvolvimento
## do modelo. A melhor solucao encontrada, que nos fez perder o menor numero de informacoes possiveis, foi excluir a primeira
## linha da base de cotacoes recebidas do YAHOO ate que a primeira linha n?o contenha nenhum NaN. A partir dai, faremos um
## segundo tratamento para NaN em nosso modelo.

firstLineNA = function(d){
  
  iNA =1;
  while(anyNA(d[iNA,])== TRUE){
    d = d[-1,];
  }
  return (d)
}

## A funcao bodyNA percorre os dados baixados do YAHOO ao longo do objeto a partir da segunda linha
## da base de dados, uma vez que a ocorr?ncia de NA na primeira linha foi tratada na function anterior
## e substitui o NaN de uma determinada cota?ao de bolsa pela sua ultima cotacao valida.
## Este tratamento melhorou razoavelmente o fit do modelo, pois caso optassemos por excluir todas as linhas 
## que possuem NaN em alguma das bolsas, perderiamos de 15 a 20% dos dados em uma observacao.

bodyNA = function(d){
  
  ## percorrendo as linhas ate a data atual -1
  for(i in 2:(nrow(d)-1)){
    ## percorrendo as colunas do data frame tal qual uma matriz e substituindo cada ocorrencia de NaN
    for(j in 1:ncol(d)){
      if(is.na(d[i,j])!=FALSE)d[i,j]=d[i-1,j];
    }
  }
  if(is.na(d[nrow(d),3])!=FALSE) d[nrow(d),3] = d[nrow(d)-1,3];
  if(is.na(d[nrow(d),4])!=FALSE) d[nrow(d),4] = d[nrow(d)-1,4];
  if(is.na(d[nrow(d),5])!=FALSE) d[nrow(d),5] = d[nrow(d)-1,5];
  
  return(d);
}
