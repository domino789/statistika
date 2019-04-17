# Prvni priklad - viz tabule

# Druhy priklad - letadlo

alpha <- 0.05
kvantil <- qnorm(1-alpha/2)
prumer <- 870.3
D <- prumer - kvantil*2.1/sqrt(5)
H <- prumer + kvantil*2.1/sqrt(5)
c(D,H)

# Treti priklad - mouka
# Nacteni dat
load("cviceni8.RData")
x <- data8$pr3$x

n <- length(x)
alpha <- 0.05
prumer <- mean(x)
odchylka <- sd(x)
# IS pro mu
kvantil <- qt(1-alpha/2,n-1)
D <- prumer - kvantil*odchylka/sqrt(n)
H <- prumer + kvantil*odchylka/sqrt(n)
c(D,H)

# IS pro sigma
kvantil1 <- qchisq(1-alpha/2,n-1)
kvantil2 <- qchisq(alpha/2,n-1)
D <- (n-1)*odchylka^2/kvantil1
H <- (n-1)*odchylka^2/kvantil2
c(D,H)

# Ctvrty priklad - selata
# Nacteni dat
load("cviceni8.RData")
x <- data8$pr4$x
y <- data8$pr4$y

alpha <- 0.05
n1 <- length(x)
n2 <- length(y)
prumer_x <- mean(x)
prumer_y <- mean(y)
s1 <- sd(x)
s2 <- sd(y)
s12 <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
kvantil <- qt(1-alpha/2,n1+n2-2)
D <- prumer_x - prumer_y - kvantil*s12*sqrt((n1+n2)/(n1*n2))
H <- prumer_x - prumer_y + kvantil*s12*sqrt((n1+n2)/(n1*n2))
c(D,H)

# Paty priklad - nikl
# Nacteni dat
load("cviceni8.RData")
x <- data8$pr5$x
y <- data8$pr5$y

alpha <- 0.05
n1 <- length(x)
n2 <- length(y)
s1 <- sd(x)
s2 <- sd(y)
kvantil1 <- qf(1-alpha/2,n1-1,n2-1)
kvantil2 <- qf(1-alpha/2,n2-1,n1-1)
D <- (s1/s2)^2/kvantil1
H <- (s1/s2)^2*kvantil2
sqrt(c(D,H))

# Sesty priklad - pneumatiky
# Nacteni dat
load("cviceni8.RData")
x <- data8$pr6$x
y <- data8$pr6$y

z <- x - y
alpha <- 0.05
n <- length(z)
prumer <- mean(z)
odchylka <- sd(z)
kvantil <- qt(1-alpha/2,n-1)
D <- prumer - kvantil*odchylka/sqrt(n)
H <- prumer + kvantil*odchylka/sqrt(n)
c(D,H)

# Sedmy priklad - divky x chlapci
# Nacteni dat
n <- 42
divky <- 16
chlapci <- 26
alpha <- 0.05
# vypocet
prumer <- divky/n 
odchylka <- sqrt(prumer*(1-prumer))
kvantil <- qnorm(1-alpha/2)
D <- prumer - kvantil*odchylka/sqrt(n)
H <- prumer + kvantil*odchylka/sqrt(n)
c(D,H)

#-----------------------------------------------------------------------------------

