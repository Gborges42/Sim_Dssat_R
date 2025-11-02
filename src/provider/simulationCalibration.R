#===============================================#
# Função que dita a simulação da safra com o DSSAT, dado os parâmetros
simulationFunction = function(paramSim, templateId, inputList) {
  # Iniciando timer
  cat(sprintf("==> [%s] Iniciando rodadas ", templateId))
  startTime = Sys.time() |> as.numeric()

  # Avancando log textual (1)
  cat(sprintf(".. "))
  
  # Criando diretorios das simulacoes
  simulation.list = createSimulationDirectories(paramSim, templateId, inputList)
  
  # Iniciando paralelismo caso esteja ativado
  noCores = as.numeric(inputList$simulationCores)

  # Obtendo modelo e calibracao
  model = as.character(inputList$model)
  calibration = as.character(inputList$calibration)

  # Obtendo arquivo de execussao do dssat
  dssatFile = as.character(inputList$dssatFile)

  # Avancando log textual (2)
  cat(sprintf(".. "))

  # Executando Dssat em serie
  run = runDssat(simulation.list, model, dssatFile, calibration)
  
  # Avancando log textual (3)
  cat(sprintf(".. "))
  
  # Encerrando timer
  endTime = Sys.time() |> as.numeric()
  cat(sprintf("Rodadas concluidas em '%s' segundos.\n", round(endTime - startTime, 3)))  
  
  # Retornando valores da run
  return(run)
}
#===============================================#