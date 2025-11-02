#===============================================#
# Lê os limites das variáveis a serem calibradas
load.limites <- function(input){
  
  # Adquirindo limites
  limites <- list()
  for(i in input$limites){
    
    # Separando string em nome e valores
    coef_limit <- strsplit(i, ":")[[1]]
    coef_name <- coef_limit[1]
    limits <- as.numeric(strsplit(coef_limit[2], ";")[[1]])
    
    # Removendo hífen
    coef_name <- gsub("-", "", coef_name)
    
    # Atribuindo os limites à lista com o nome do coeficiente
    limites[[coef_name]] <- limits
  }
  
  return(limites)
}
#===============================================#
calcular_tempo_dec <- function(start_time){
  # Tempo de fim
  end_time <- Sys.time()
  
  # Diferença total em segundos
  time_diff_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Conversão para dias, horas, minutos e segundos
  days <- floor(time_diff_sec / (24 * 3600))
  remainder <- time_diff_sec %% (24 * 3600)
  hours <- floor(remainder / 3600)
  remainder <- remainder %% 3600
  minutes <- floor(remainder / 60)
  seconds <- remainder %% 60
  
  # Mensagem de tempo
  tempo_decorrido = sprintf("")
  if(days != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("D: %s", days), sep = " ")
  }
  if(hours != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("H: %s", hours), sep = " ")
  }
  if(minutes != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("M: %s", minutes), sep = " ")
  }
  if(seconds != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("S: %s", round(seconds)), sep = " ")
  }

  return(tempo_decorrido)
}
#===============================================#

#===============================================#
# Função que salva os resultados da otimização
salvar_resultados_bo <- function(resultado_bo, caminho_arquivo) {
  # Salvando o objeto RDS em completo
  saveRDS(resultado_bo, file = caminho_arquivo)
  
  # Salvando todas as rodadas
  rodadas = resultado_bo$scoreSummary
  fwrite(rodadas, "output/todas_rodadas.csv")
  
  melhor_res = rodadas[Score == max(rodadas$Score)]
  fwrite(melhor_res, "output/melhor_resultado.csv")
}
#===============================================#

#===============================================#
# Função para ler o resultado da otimização como um objeto R
carregar_resultados_bo <- function(caminho_arquivo) {
  # Lendo o objeto RDS em completo
  resultado_bo <- readRDS(file = caminho_arquivo)
  return(resultado_bo)
}
#===============================================#