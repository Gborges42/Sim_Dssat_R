#===============================================#
# Funcao responsavel por escrever um novo arquivo cultivar
writeCultivar = function(cultivarData, simulationDirectory) {
  # Obtendo valores e ajustando eles para apenas 5 digitos (inclusive o separador decimal)
  headerSize = dim(cultivarData)[2]
  values = as.character(cultivarData[,5:headerSize]) |> strsplit("")
  values = sapply(values, function(value) {
    valueLength = length(value)
    valueLength = ifelse(valueLength > 5, 5, valueLength)
    res = paste0(value[1:valueLength], collapse = "")
    return(res)
  })

  # Atualizando valores
  cultivarData = as.data.frame(cultivarData)
  cultivarData[,5:headerSize] = values
  
  # Obtendo arquivo de saida
  outputFile = list.files(simulationDirectory, pattern = ".CUL$", full.names = T)

  # Cria a concatenacao para a linha do cultivar com os espacos corretos
  cultivarConcat = function(values) {
    spaces = rep("%5s", length(values))
    spaces[1:4] = c("%-6s", "%-16s", "%5s", "%6s")
    resp = sprintf(spaces, values)
    resp = paste0(resp, collapse = " ")

    # Retornando linha
    return(resp)
  }

  # Montando cabecalho
  header = names(cultivarData) |> cultivarConcat()

  # Montando linha
  line = cultivarData[1,] |> cultivarConcat()

  # Escrevendo valores no arquivo de saida
  cat(sprintf("%s\n", header), file = outputFile, append = FALSE, sep = "")
  cat(sprintf("%s\n", line), file = outputFile, append = TRUE, sep = "")
}
#===============================================#

#===============================================#
# Funcao responsavel por atualizar o cultivar com novos valores
makeCultivar = function(cultivarFile, multiplyVector, cultivar) {
  
  # Criando conexao com o arquivo cultivar
  con = file(cultivarFile, "r")
  cultivarLines = readLines(con)

  # Encerrando conexao com o arquivo cultivar
  close(con)

  # Extraindo cabecalho do arquivo cultivar
  cultivarHeader = grep(pattern = '@VAR', cultivarLines)
  cultivarHeader = cultivarLines[cultivarHeader]
  cultivarHeader = strsplit(cultivarHeader, " ")[[1]]
  cultivarHeader = cultivarHeader[cultivarHeader != ""]

  # Extraindo linha que contem o cultivar
  findCultivar = grep(pattern = cultivar, cultivarLines)
  findCultivar = cultivarLines[findCultivar]

  # Obtendo linha de cultivar
  cultivarTable = strsplit(findCultivar, "")[[1]]

  # Extraindo posicao do primeiro ponto
  firstPoint.index = which(cultivarTable == ".")[1]

  # Reconstruindo para extracao de valores
  postPoint = paste0(cultivarTable[(firstPoint.index + 1): length(cultivarTable)], collapse = "")

  # Gerando data.table
  cultivarData = fread(text = postPoint)
  
  # Unificando com o resto das colunas
  preData = data.table(cultivar, "RODADA_GA", ".")
  cultivarData = cbind(preData, cultivarData)  

  # Obtendo indices das colunas
  header.index = which(cultivarHeader %in% names(multiplyVector))

  # Adicionando nome das colunas
  colnames(cultivarData) = cultivarHeader
  #======================================================================================================
  # Normalizando os nomes das colunas (removendo "-")
  normalize_names <- function(names_vec) gsub("-", "", names_vec)
  
  names_maior_norm <- normalize_names(names(cultivarData))
  names_menor_norm <- normalize_names(names(multiplyVector))
  
  # Encontrar colunas que existem em ambos após normalização
  colunas_comuns <- intersect(names_maior_norm, names_menor_norm)
  
  # Mapear os nomes originais no dt_maior e dt_menor
  map_maior <- names(cultivarData)[normalize_names(names(cultivarData)) %in% colunas_comuns]
  map_menor <- names(multiplyVector)[normalize_names(names(multiplyVector)) %in% colunas_comuns]
  
  # Atualizar os valores do dt_maior com os do dt_menor (linha a linha)
  for (i in seq_along(map_maior)) {
    # Verificando o número de casas decimais no valor original
    original_value <- cultivarData[[map_maior[i]]]
    decimal_places <- count_decimal_places(original_value)
    
    # Arredondando o novo valor para o mesmo número de casas decimais
    new_value <- multiplyVector[[map_menor[i]]]
    rounded_value <- round(new_value, decimal_places)
    
    # Substituindo o valor no dt_maior com o novo valor arredondado
    cultivarData[[map_maior[i]]] <- rounded_value
  }
  
  # Retornando novo cultivar
  return(cultivarData)
}
#===============================================#

# Funcao para contar as casas decimais
count_decimal_places <- function(x) {
  # Verifica se x é numérico e tem casas decimais
  if (grepl("\\.", as.character(x))) {
    return(nchar(strsplit(as.character(x), "\\.")[[1]][2]))  # Conta as casas após o ponto
  } else {
    return(0)  # Caso o número seja inteiro, retorna 0
  }
}