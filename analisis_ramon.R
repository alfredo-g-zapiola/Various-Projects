library(data.table)
library(MASS)
library(fitdistrplus)



process_df <- function(df) {
  df <- transpose(df)
  colnames(df) <- df[1, ]
  df <- df[2:dim(df)[1],]
    
  return (df)
}

df1 <- data.frame(read.delim('Downloads/dinero_16_runs.txt', header = T))
df2 <- data.frame(read.delim('Downloads/dinero_17_25.txt', header = T))

df1 <- process_df(df1)
df2 <- process_df(df2)

df <- cbind.data.frame(df1, df2)
time <- 1:158
df <- cbind(time, df)

## dinero ahorrado en un año y medio ##
t <- 52*1.5
distrib_ahorros_t <- as.numeric(df[(df$time == t), which(names(df) %like% "Ramón")])

# obtención del valor crítico
t_975 <- qt(.975, 24)
# cálculo de half width
t_975 * sd(distrib_ahorros_t ) / sqrt(24)
# media
mean(distrib_ahorros_t )
# grafico del intervalo de confianza
p_hist <- function(x){
  h <- hist(x, freq = FALSE, breaks = 12, main="Histograma del dinero en 1 año y medio")
  lines(density(x), col="red")
  x2 <-seq(min(x), max(x), length.out = 40)
  lines(x2 , dnorm(x2, mean(x), sd(x)), col="blue")
  legend("topright", legend =  c("Densidad", "ajuste t student"),
         col=c("red", "blue"), lty = c(1,1), lwd = 2)
}
p_hist(distrib_ahorros_t)

## tiempo en ahorrar para el Ibiza ##
df_aux <- df[, which(names(df) %like% "Meta")]
tiempos <- character()
for (c in 1:ncol(df_aux)){
  tiempos <- c(tiempos, match(1, df_aux[,c]))
  
}
tiempos <- as.numeric(tiempos)
tiempos
hist(tiempos, breaks = 10)
tiempos

# half width
t_975 * sd(tiempos) / sqrt(24)
# media
mean(tiempos)

write.csv(df_aux, "Tabla para obtención del tiempo.csv")
write.csv(df, "Tabla de todos los resultados.csv")
