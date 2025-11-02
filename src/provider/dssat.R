simulacao_dssat = function(df, inputList){

  # Realizando simulação
  res = pmap(df, function(...) {
    # Acessa diretamente as colunas como argumentos
    args <- list(...)
    # Determinando o nome do arquivo
    nomeArquivo <- args$nomeArquivo
    # Determinando os parâmetros
    paramSim = unlist(args[names(args) %in% gsub("-","", inputList$coefficients)])
    
    #==========================================================================#
    # Executando SSE 
    run = simulationFunction(paramSim, gsub(":", "", sprintf("iteration_%s", format(Sys.time(), "%H:%M:%OS3"))), inputList)
    
    # Adicionando nome da simulação
    run$simulacao = nomeArquivo
    return(run)
  })
  
  # Combinando respostas
  res = bind_rows(res)
  fwrite(res, "output/evaluate_dssat.csv")
}
