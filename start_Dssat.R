# Carregando funcoes de inicializacao
source(".//src//loader.R")

# Carregando pacotes
load.packages()

# Compilando funcoes
compile.functions()

# Carregando funcoes compiladas
load.functions()

# Arquivo de Configuração
# ATENÇÃO!!! Ao mudar o arquivo de configurção, será necessário mudar também dentro da função 
# Lendo o arquivo de configuração para adquirir o input
input = config.treatment(".//StartValues_bean.config")
# Lendo arquivo de dados
df = fread("input/parametros_dssat.csv")

# Simulação
simulacao_dssat(df, input)
