library(tidyverse)
library(forecast)
library(lubridate)

# Carregando os dados
base = read.csv('dengue.csv')

# Preparando os dados
base$Mês.Notificação = as.factor(base$Período)
base1 = base[c(1:204),]
novo = base[c(205:215),]
base_ts = ts(base1$Casos.Prováveis, frequency=12, start=c(2007,1))

# Visualização gráfica com ajuste do eixo Y
plot(base_ts, type = "s", axes = FALSE, ylab = "Casos Prováveis", xlab = "Ano", main = "Série Temporal - Casos de Dengue")

# Adicionando os eixos manualmente
axis(1, at = seq(2007, 2025, by = 1))
axis(2, at = pretty(base_ts), labels = format(pretty(base_ts), big.mark = ".", scientific = FALSE))
box()

# Aplicando o modelo SES (Suavizacão Exponencial Simples)
SES = ses(base_ts)

# Suavizacao exponencial de Holt
HOLT = holt(base_ts)

# Suavizacao exponencial de Holt-Winters
HW_ad = hw(base_ts, seasonal = "additive")

# Comparacao Geral
list(SES, HOLT, HW_ad) %>% map(accuracy)

#Observa-se que o modelo de Holt-Winters apresentou os menores valores de RMSE (42184), indicando o melhor ajuste aos dados históricos.

# Previsao
HWa.predito <- forecast(HW_ad, h = 11)
previsao <- data.frame(
  Previsao = HWa.predito$mean,
  LI = HWa.predito$lower[, 2],
  LS = HWa.predito$upper[, 2]
)

novo <- novo[1:11, ]
novo <- cbind(novo, previsao)
novo$Periodo <- seq(1:11)
ylim_range <- range(novo$Casos.Prováveis, novo$Previsao, novo$LI, novo$LS)

# Grafico da previsao
plot(novo$Periodo, novo$Casos.Prováveis, xlab = "Periodo de Tempo",
     ylab = "Casos prováveis de dengue", ylim = ylim_range)
lines(novo$Periodo, novo$Casos.Prováveis, col = "black", lwd = 2)
lines(novo$Periodo, novo$Previsao, col = "red", lwd = 2)
lines(novo$Periodo, novo$LI, col = "red", lwd = 2, lty = 'dashed')
lines(novo$Periodo, novo$LS, col = "red", lwd = 2, lty = 'dashed')
