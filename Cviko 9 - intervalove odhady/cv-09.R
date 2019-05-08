#	Priklad 1

X <- c (0.31, 0.30, 0.29, 0.32)
n <- length (X)
prumer <- mean (X)
odchylka <- sd (X)
alpha <- 0.05
kvantil <- qchisq (1 - alpha, n - 1)
D <- sqrt (n - 1) * odchylka / sqrt (kvantil)
D


# Priklad 2 

data <- read.csv (file = "intervaly.csv", header = TRUE, sep = ",", dec = ".")
stredy <- (data$dolni + data$horni) / 2
X <- rep (stredy, data$cetnost)
n <- sum (data$cetnost)
prumer <- mean (X)
odchylka <- sd (X)
rozptyl <- var (X)

# a)
alpha <- 0.05
kvantil <- qt (1 - alpha/2, n - 1)
D <- prumer - kvantil * odchylka / sqrt(n)
H <- prumer + kvantil * odchylka / sqrt(n)
c (D, H)

# b)
alpha <- 0.05
kvantil1 <- qchisq (1 - alpha/2, n - 1)
kvantil2 <- qchisq (alpha/2, n - 1)
D <- (n - 1) * rozptyl / kvantil1
H <- (n - 1) * rozptyl / kvantil2
c (D, H)

# Priklad 3 

# a)
alpha <- 0.05
kvantil1 <- qnorm (1 - alpha/2)
odhad <- 48/160

D <- odhad - kvantil1 * sqrt((odhad * (1-odhad))/160) 
H <- odhad + kvantil1 * sqrt((odhad * (1-odhad))/160)
c (D, H)

# b)
odhad * 8000
D <- D*8000
H <- H*8000
c (D,H)

# priklad 4
A <- c (0.14, 0.138, 0.143, 0.142, 0.144, 0.137)
B <- c (0.135, 0.140, 0.142, 0.136, 0.138)
alpha <- 0.05
kvantil <- qnorm (1 - alpha)
prumerA <- mean(A)
prumerB <- mean(B)
rozptylA <- 4e-6
rozptylB <- 9e-6
D <- (prumerA - prumerB) - (kvantil * sqrt((rozptylA/length(A)) + (rozptylB/length(B))))


#priklad 5 
data <- read.csv (file = "selata.csv", header = TRUE, sep = ",", dec = ".")
str(data)
A <- data$prir1
B <- data$prir2
alpha <- 0.05
nA <- length(A)
nB <- length(B)

kvantil1 <- qt (1 - alpha/2, nA + nB)
prumerA <- mean(A)
prumerB <- mean(B)

vyberovyRozptyl1 <- sd(A)^2
vyberovyRozptyl2 <- sd(B)^2
vyberovyRozptyl12 <- sqrt(( ((nA-1)*vyberovyRozptyl1) + ((nB-1)*vyberovyRozptyl2) ) / (nA + nB - 2))

D <- ( (-kvantil1  * vyberovyRozptyl12) / sqrt( (nA*nB)/(nA+nB)) )  + (prumerA - prumerB)
H <- ( (kvantil1  * vyberovyRozptyl12) / sqrt( (nA*nB)/(nA+nB)) )  + (prumerA - prumerB)
c (D, H)
D <- (prumerA - prumerB) - (kvantil1  * vyberovyRozptyl12 * sqrt( (nA+nB)/(nA*nB))) 
H <- (prumerA - prumerB) + (kvantil1  * vyberovyRozptyl12 * sqrt( (nA+nB)/(nA*nB))) 
c (D, H) #nevychádza ale vzorec z prednášky ...why ?

#priklad 6 
A <- c (3.26, 3.26, 3.27, 3.27)
B <- c (3.23, 3.27, 3.29, 3.29)

alpha <- 0.05
nA <- length(A)
nB <- length(B)

kvantil1 <- qf (1 - alpha, nA-1,  nB-1)


vyberovyRozptyl1 <- sd(A)^2
vyberovyRozptyl2 <- sd(B)^2
vyberovyRozptyl12 <- sqrt(( ((nA-1)*vyberovyRozptyl1) + ((nB-1)*vyberovyRozptyl2) ) / (nA + nB - 2))


H <- vyberovyRozptyl1 * kvantil1 / vyberovyRozptyl2
sqrt(H)



# Priklad 7

data <- read.csv (file = "spotreba.csv", header = TRUE, sep = ";", dec = ",")
X <- data$spotreba
n <- length(X)
prumer <- mean (X)
odchylka <- sd (X)
rozptyl <- var (X)

# stredni hodnota
alpha <- 0.05
kvantil <- qt (1 - alpha/2, n - 1)
D <- prumer - kvantil * odchylka / sqrt(n)
H <- prumer + kvantil * odchylka / sqrt(n)
c (D, H)

# rozptyl
alpha <- 0.05
kvantil1 <- qchisq (1 - alpha/2, n - 1)
kvantil2 <- qchisq (alpha/2, n - 1)
D <- (n - 1) * rozptyl / kvantil1
H <- (n - 1) * rozptyl / kvantil2
c (D, H)


# Priklad 8

n <- 25
prumer <- 3118
odchylka <- 357
rozptyl = odchylka^2

alpha <- 0.05
kvantil1 <- qchisq (1 - alpha/2, n - 1)
kvantil2 <- qchisq (alpha/2, n - 1)
D <- sqrt( (n - 1) * rozptyl / kvantil1)
H <- sqrt( (n - 1) * rozptyl / kvantil2)
c (D, H)


# Priklad 9

data <- read.csv (file = "pevnost.csv", header = TRUE, sep = ";", dec = ",")
X <- data$pevnost
n <- length(X)
prumer <- mean (X)
odchylka <- sd (X)
rozptyl <- var (X)

alpha <- 0.05
kvantil <- qchisq (alpha, n - 1)
H <- (n - 1) * rozptyl / kvantil
H

# priklad 10
data <- read.csv (file = "SiO2.csv", header = TRUE, sep = ";", dec = ",")
str(data)
A <- subset(data, metoda == "A")$obsah
B <- subset(data, metoda == "B")$obsah
alpha <- 0.05
nA <- length(A)
nB <- length(B)

kvantil1 <- qt (1 - alpha/2, nA + nB -2)
kvantil2 <- qt (alpha/2, nA + nB -2)
prumerA <- mean(A)
prumerB <- mean(B)

vyberovyRozptyl1 <- sd(A)^2
vyberovyRozptyl2 <- sd(B)^2
vyberovyRozptyl12 <- sqrt(( ((nA-1)*vyberovyRozptyl1) + ((nB-1)*vyberovyRozptyl2) ) / (nA + nB - 2))

#TODO opytat sa preco musi byt prumer prehodeny, nie je A - B 
D <- ( (-kvantil1  * vyberovyRozptyl12) / sqrt( (nA*nB)/(nA+nB)) )  + (prumerB - prumerA)
H <- ( (kvantil1  * vyberovyRozptyl12) / sqrt( (nA*nB)/(nA+nB)) )  + (prumerB - prumerA)

c (D, H)


# Priklad 11
alpha <- 0.05
data <- read.csv (file = "zakaznici.csv", header = TRUE, sep = ";", dec = ",")
str(data)
X <- data$zakaznici
pocet <- length(X)
prumer <- mean(X)
S <- sd(X)

kvantil <- qnorm(1-alpha/2)

D <- (-kvantil * sqrt(prumer) / sqrt(pocet)) + prumer
H <- (kvantil * sqrt(prumer) / sqrt(pocet)) + prumer 
c (D, H)
# Priklad 12 

data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")

# cyklus pomoci funkce "sapply", kde se promenna M (= cislo mesice) postupne meni od 1 do 12
# v kazdem cyklu vratime 4 cisla: cislo mesice, dolni odhad stredni hodnoty, prumer, a horni odhad stredni hodnoty 
matice <- sapply (seq (1, 12), function (M) {
	X <- subset (data, mnth == M)$cnt
	n <- length (X)
	prumer <- mean (X)
	odchylka <- sd (X)
	alpha <- 0.05
	kvantil <- qnorm (1 - alpha / 2)
	D <- prumer - kvantil * odchylka / sqrt (n) 
	H <- prumer + kvantil * odchylka / sqrt (n) 
	# jako posledni prikaz tela funkce v cyklu se uvede vektor cisel, ktera se maji ulozit do matice vysledku
	return (c (mesic = M, D = D, prumer = prumer, H = H))
})

matice 
# prehodime jeste sloupce a radky funkci "t" (= transpozice matice) a vytvorime tabulku 

tabulka <- data.frame (t (matice))
tabulka

plot (tabulka$mesic, tabulka$prumer, type = "b", lwd = 2, col = "red", ylim = c (1500, 6500), xlab = "mesic", ylab = "prumerny pocet zapujcek za den s 95% IS")
lines (tabulka$mesic, tabulka$D, type = "b", lty = 2, col = "green")
lines (tabulka$mesic, tabulka$H, type = "b", lty = 2, col = "blue")

