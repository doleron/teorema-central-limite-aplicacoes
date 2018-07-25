## Introdução - jogando dados

# lançamento de 10000 dados de 6 faces
amostra.1 <- sample(1:6, replace = TRUE, 10000)

hist(amostra.1, breaks = 0:6, prob=TRUE)

amostra.2 <- sample(1:6, replace = TRUE, 10000)

# amostra com soma das amostras anteriores
amostra <- amostra.1 + amostra.2

hist(amostra, breaks = 1:12, main = "10k jogadas de 2 dado2", col = "yellow", prob=TRUE)

amostra.3 <- sample(1:6, replace = TRUE, 10000)

amostra.4 <- sample(1:6, replace = TRUE, 10000)

amostra.5 <- sample(1:6, replace = TRUE, 10000)

amostra <- amostra + amostra.3 + amostra.4 + amostra.5

hist(amostra, breaks = 5:30, main = "10k jogadas de 5 dados", col = "yellow", prob=TRUE)

# Plotando a distribuição normal equivalente
normal.seq <- seq(0, 30, 0.01)
sd.amostra <- sd(amostra)
m.amostra <- mean(amostra)

normal.data <- dnorm(normal.seq, m.amostra, sd.amostra)
lines(normal.seq, normal.data, col = "blue")

## Exemplo pratico - Simulação

quantidade.de.eleitores <- 100000

# vector com votos positivos
votos.candidato.a <- rep(1, 0.7 * quantidade.de.eleitores)
# vector com votos negativos
votos.candidato.b <- rep(0, 0.3 * quantidade.de.eleitores)

eleitores <- append(votos.candidato.a, votos.candidato.b)

#permutar a posicao dos eleitores somente para garantir que a ordem nao afeta o resultado
eleitores <- sample(eleitores)

mean(eleitores)

# distanciamento da proporção de eleitores a partir do tamanho da amostra
amostra <- sample(eleitores, 10)
amostra
mean(amostra)

amostra <- sample(eleitores, 100)
mean(amostra)

amostra <- sample(eleitores, 10000)
mean(amostra)

# vamos ver esse corpotamento graficamente
n <- seq(10, 5000, 10)
medias <- unlist(lapply(n, function(x) mean(sample(eleitores, size=x))))
plot(n, medias, t='l')
abline(h = 0.7, col='green')

# verificando a compatibilidade das médicas com a distribuição normal
medias.100 <- numeric(5000)
for(i in 1:length(medias.100)) {
  medias.100[i] <- mean(sample(eleitores, size=100))
}

sd.medias.100 <- sd(medias.100)
m.medias.100 <- mean(medias.100)
hist(medias.100, prob=TRUE)

normal.seq <- seq(0,1,0.01)
normal.data <- dnorm(normal.seq, m.medias.100, sd.medias.100)
lines(normal.seq, normal.data, col = "blue")

# usando o teste de Shapiro para testar a aderência da amostra à normalidade 
shapiro.test(medias.100)

## Exemplo pratico - Calculando o tamanho ideal da amostra

# Critério de confiança: 95% p_real
# p_estimado - erro <= p_real <= p_estimado + erro
erro <- 0.02
# erro = Z * desvio / sqrt(n) 
# n = Z^2/erro^2 * desvio
# desvio binomial = p(1 - p)

# cenario p = 0.5
Z <- qnorm(0.975)
p <- 0.5 
n <- Z^2/erro^2 * p * (1 - p)
# n é aproximadamente 2400

# cenario p = 0.9
p <- 0.9
n <- Z^2/erro^2 * p * (1 - p)
# n é aproximadamente 864
