

# Aplicación de Monte Carlo para Instrumentos Débiles --------

set.seed(123)
N <- 200   # Número de observaciones
R <- 1000  # Número de simulaciones

install.packages("AER")
library("AER")  # Librería para IV 
alpha <- 1      # Intercepto
sigu  <- 1      # Error de medida en y
sige  <- 0.1    # Monto de error de medida en e
gamma <- 1      # Instrumento fuerte
rho   <- 0.5    # Correlación entre errores
beta  <- 0.2    # Pendiente
theta <- 0.5    # Constante
z <- 5*runif(N) # Instrumento

# Problema de Instrumento Débil ----

gamma <- c(0.3, 0.5, 0.9)
biv <- matrix(NA, R, length(gamma))
for (j in 1:length(gamma)) {
  ga <- gamma[j]
  for (i in 1:R) {
    u <- rnorm(N, sigu)
    x <- alpha + ga*z + rho*u + rnorm(N, 0 , sige)
    y <- theta + beta*x + u
    iv <- ivreg(y ~ x | z)
    biv[i,j] <- iv$coef["x"]
  }
}

d1 <- density(biv[,1])
d2 <- density(biv[,2])
d3 <- density(biv[,3])

plot(d3, main="Distribution of biv", 
        lwd =2, col=1, xlim=range(biv[,3]))
lines(d2, lwd=2, col=2)
lines(d1, lwd=2, col=3)
abline(v=beta, col="black")
legend("topright",
       c("gamma=0.9", "gamma=0.5", "gamma=0.3"),
       lty=c(1,1,1),
       lwd=c(2.5,2.5), col=c("black", "red", "green"))
