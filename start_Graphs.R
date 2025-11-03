# Pacotes
library(tidyverse)
library(patchwork)

# Carregando funcoes de inicializacao
source(".//src//loader.R")

# Carregando pacotes
load.packages()

# Compilando funcoes
compile.functions()

# Carregando funcoes compiladas
load.functions()

# ------------ Entrada do usuário ------------
# Ex.: defina input$calibration em algum lugar do seu app/script
input = config.treatment(".//StartValues_bean.config")
# -------------------------------------------

# Define uma lista de possíveis cores
my_colors <- c("coral", "lightcoral", "cornflowerblue", "cadetblue3" , "darkolivegreen3", "lightgreen" , "darkmagenta", "darkslateblue", "pink", "burlywood", "aquamarine")

# Funções de erro (vetorizadas por linha)
.row_error <- function(y, yhat, metodo) {
  # metodo: "rmse", "mape" ou "mae"
  eps <- .Machine$double.eps
  case_when(
    metodo == "rmse" ~ (y - yhat)^2,                               # erro ao quadrado por linha
    metodo == "mape" ~ abs(y - yhat) / pmax(abs(y), eps),          # APE por linha
    TRUE             ~ abs(y - yhat)                                # MAE por linha
  )
}

# Agregadores coerentes com cada método
.aggregate_error <- function(err_vec, metodo) {
  if (metodo[1] == "rmse") {
    sqrt(mean(err_vec, na.rm = TRUE)) # sqrt da média do SE
  } else {
    mean(err_vec, na.rm = TRUE)       # média de APE ou AE
  }
}

# Lê o CSV
df <- read_csv("output/evaluate_dssat.csv", show_col_types = FALSE)

# Normaliza o método a partir do nome da simulação (case-insensitive)
df <- df %>%
  mutate(
    metodo = case_when(
      str_detect(simulacao, regex("rmse", ignore_case = TRUE)) ~ "rmse",
      str_detect(simulacao, regex("mape", ignore_case = TRUE)) ~ "mape",
      TRUE ~ "mae"
    )
  )
# sobrepõe com definição do arquivo de configuração
df$metodo <- input$metodo_score

# Lista de variáveis permitidas
vars_validas <- c("ADAP","MDAP","PD1P","PDFP","LAIX","SWAD","GWAD","LWAD")

# Valida seleção do usuário
vars_escolhidas <- toupper(input$calibration %||% vars_validas)
vars_escolhidas <- intersect(vars_escolhidas, vars_validas)
if (length(vars_escolhidas) == 0) {
  stop("Nenhuma variável válida em input$calibration. Use: ADAP, MDAP, PD1P, PDFP, LAID, SWAD, GWAD, LWAD.")
}

# Cria colunas de erro por variável, respeitando o método por simulação
for (v in vars_escolhidas) {
  col_s <- paste0(v, "S")
  col_m <- paste0(v, "M")
  if (!all(c(col_s, col_m) %in% names(df))) {
    stop("Colunas esperadas não encontradas: ", col_s, " e/ou ", col_m)
  }
  df[[paste0("erro_", v)]] <- .row_error(
    y    = as.numeric(df[[col_m]]),  # medido (M) como referência
    yhat = as.numeric(df[[col_s]]),  # simulado (S)
    metodo = df$metodo
  )
}

# Erro médio (média das colunas de erro criadas)
cols_erro <- paste0("erro_", vars_escolhidas)
df <- df %>%
  mutate(erro_medio = rowMeans(across(all_of(cols_erro)), na.rm = TRUE))

# ---- Plots dinâmicos ----
make_plot <- function(var, my_colors) {
  ggplot(df, aes(x = simulacao, y = .data[[paste0("erro_", var)]])) +
    geom_boxplot(fill = sample(my_colors,1)) +
    labs(
      title = paste("Erro -", var),
      y = "Erro por observação",
      x = "Simulação"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plots <- lapply(seq_along(vars_escolhidas), function(i) make_plot(vars_escolhidas[i], my_colors))
names(plots) <- vars_escolhidas

# Plot de erro médio
plot_medio <- ggplot(df, aes(x = simulacao, y = erro_medio)) +
  geom_boxplot(fill = sample(my_colors,1)) +
  labs(title = "Erro - Média entre variáveis selecionadas", y = "Erro por observação", x = "Simulação") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Layout: 2 colunas (ajusta automaticamente o nº de linhas)
ncol_layout <- 2
grafico_final <- wrap_plots(plots, ncol = ncol_layout) / plot_medio

# ---- Tabela-resumo por simulação e variável (agregada por método) ----
# Aqui o RMSE vira sqrt(mean(SE)) e MAPE/MAE = mean
long_err <- df %>%
  select(simulacao, metodo, all_of(cols_erro)) %>%
  pivot_longer(cols = starts_with("erro_"),
               names_to = "variavel",
               values_to = "erro") %>%
  mutate(variavel = str_remove(variavel, "^erro_"))

resumo <- long_err %>%
  group_by(simulacao, variavel, metodo) %>%
  summarise(erro_aggregado = .aggregate_error(erro, metodo),
            .groups = "drop")

# Salva a tabela
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
write_csv(resumo, "graphs/resumo_erros_por_simulacao.csv")

# ---- Salvamentos ----
# Salva cada plot individual
invisible(lapply(names(plots), function(v) {
  ggsave(filename = file.path("graphs", paste0("plot_", v, ".png")),
         plot = plots[[v]], width = 6, height = 5, dpi = 300)
}))

# Salva plot do erro médio
ggsave("graphs/plot_medio.png", plot_medio, width = 6, height = 5, dpi = 300)

# Salva grade completa
ggsave("graphs/erros_boxplots.png", grafico_final, width = 12, height = 10, dpi = 300)
