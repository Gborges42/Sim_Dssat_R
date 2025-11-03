#===============================================#
evaluateDifference = function(evaluateData, calibration, metodo_score) {
  # Obtendo variaveis simuladas de calibracao
  variableSimulated.index = paste0(sprintf("%sS", calibration), collapse = "|") |> grep(names(evaluateData))

  # Obtendo variaveis observadas de calibracao
  variableObserved.index = paste0(sprintf("%sM", calibration), collapse = "|") |> grep(names(evaluateData))

  score.list = lapply(calibration, function(calibration, evaluateData, metodo_score){
    
    metodos_erro <- list(
      rmse = rmse,
      mape = mape
    )

    # Obtendo index variaveis de calibracao
    variable.index = grep(paste0(calibration, collapse = "|"), names(evaluateData))

    # Obtendo variaveis simuladas
    calibrationData = evaluateData[, ..variable.index]
    calibrationData[calibrationData == -99] = NA

    # Fazendo RMSE da variável
    score <- metodos_erro[[metodo_score]](calibrationData)
    return(score)
  }, evaluateData, metodo_score)
  
  # Retorna a média dos RMSE obtidos
  return(mean(unlist(score.list)))
}
#===============================================#

#===============================================#
plantgroDifference = function(plantgroData, tData, calibration) {
  # Gerando regex das variaveis de calibracao
  variableRegex = paste0(c("TRNO", calibration), collapse = "|")
  
  # Obtendo variaveis simuladas de calibracao
  variableSimulated.index = grep(variableRegex, names(plantgroData))

  # Obtendo variaveis observadas de calibracao
  variableObserved.index = grep(variableRegex, names(tData))

  # Obtendo variaveis simuladas
  calibrationDataSimulated = plantgroData[, ..variableSimulated.index]
  calibrationDataSimulated[calibrationDataSimulated == -99] = NA
  calibrationDataSimulated = calibrationDataSimulated[, lapply(.SD, mean, na.rm = TRUE), by = TRNO]

  # Obtendo variaveis observadas
  calibrationDataObserved = tData[, ..variableObserved.index]
  calibrationDataObserved[calibrationDataObserved == -99] = NA
  calibrationDataObserved = calibrationDataObserved[, lapply(.SD, mean, na.rm = TRUE), by = TRNO]

  # Unindo dados e ordenando por "Tratamento"
  calibrationData = merge(calibrationDataObserved, calibrationDataSimulated, by = "TRNO")
  calibrationData = calibrationData[order(TRNO)]

  # Atualizando variaveis simuladas de calibracao
  variableSimulated.index = paste0(sprintf("%s.y", calibration), collapse = "|") |> grep(names(calibrationData))

  # Atuallizando variaveis observadas de calibracao
  variableObserved.index = paste0(sprintf("%s.x", calibration), collapse = "|") |> grep(names(calibrationData))

  # Calculando a diferenca entre Observado e Simulado
  observed = calibrationData[, ..variableObserved.index]
  simulated = calibrationData[, ..variableSimulated.index]
  response = rpe(observed, simulated)

  # Atualizando nomes
  names(response) = gsub(".x|.y", "", names(response))

  # Adicionando tratamento
  response$TN = calibrationData$TRNO

  # Retornando resposta
  return(response)
}
#===============================================#

#===============================================#
# Erro quadrado medio relativo (Para fazer a calibracao do modelo)
rmse <- function(x) {
  y    <- as.numeric(x[[1]])
  yhat <- as.numeric(x[[2]])
  
  # cálculo do RMSE (mesmas unidades de y)
  return(sqrt(mean((y - yhat)^2)))
}
#===============================================#

#===============================================#
# Erro Percentual Absoluto Médio (Para fazer a calibracao do modelo)
mape <- function(x) {
  y     <- as.numeric(x[[1]])
  yhat  <- as.numeric(x[[2]])
  
  # cálculo do MAPE (%)
  return(mean(abs(y - yhat) / abs(y),na.rm=TRUE))
}
#===============================================#

#===============================================#
# Erro percentual relativo (Para normalizar a escala de valores)
rpe = function(measured, simulated) {
  RPE = (simulated - measured) / abs(measured)
  return(RPE)
}
#===============================================#
