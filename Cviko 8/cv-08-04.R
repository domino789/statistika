# Working dir must be statistika/data dir !!!

data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")
names (data)
str (data)

# vyber sloupce dat z datove tabulky
X <- data$cnt

# ========================================================================================

n <- length (X)
r <- round (sqrt (n))
r
range (X)
diff (range (X)) / r
#	volime tedy 27 intervalu o delkach 322 

dolni <- seq (from = 21.5, to = 8714, by = 322)
horni <- seq (from = 21.5 + 322, to = 8720, by = 322)
stredy <- (dolni + horni) / 2

tabulka <- data.frame (dolni, horni, stredy)

#	absolutni cetnosti
n.j <- apply (tabulka, 1, function (w) {
	sum (X >= w[1] & X < w[2])
})
sum (n.j)

# cetnosti, relativni, absolutni, kumulativni, cetnostni hustota...

#	pocet pozorovani
n <- sum (n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j)

# cetnostni hustota 
f.j <- p.j/d.j

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n


tabulka <- cbind (tabulka, data.frame (n.j, p.j, N.j, F.j, d.j, f.j))
tabulka

# https://www.rdocumentation.org/packages/graphics/versions/3.5.3/topics/barplot
# https://www.rdocumentation.org/packages/graphics/versions/3.5.3/topics/par
# sloupkovy diagram cetnosti
barplot (tabulka$n.j, names.arg = paste(tabulka$dolni, "-", tabulka$horni), las = 3, xlab = "",
         ylab = "pocet zapujcek", main = "diagram cetnosti")

# sloupkovy diagram relativnich cetnosti
barplot (tabulka$p.j, names.arg = paste(tabulka$dolni, "-", tabulka$horni), las = 3, xlab = "",
         ylab = "diagram relativnich cetnosti")

# sloupkovy diagram cetnostni hustoty
barplot (tabulka$f.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), las = 3, space = 0, xlab = "", 
         ylab = "cetnostni hustota zapujcek", main = "diagram cetnostni hustoty Histogram?", col = "yellow")

#	defaultni histogram v R, zkousejte menit parametr "breaks"
hist (X, breaks = 27, freq = FALSE, col = "yellow", xlab = "pocty zapujcek", ylab = "hodnoty cetnostni hustoty", main = "histogram")

# graf empiricke distribucni funkce
plot (c(0,tabulka$dolni[1],tabulka$horni,9000), c(0,0,tabulka$F.j,1), type = "b", xlab = "pocty zapujcek", ylab = "ECDF", main = "empiricka distribucni funkce", col = "red", pch = 20, lwd = 1.5)

# prumer
prumer <- mean (X)
prumer
# rozptyl 
var (X) * (n-1) / n
rozptyl <- mean (X^2) - prumer^2
rozptyl

# smerodatna odchylka
sqrt (rozptyl)

# vazeny prumer 
vazeny_prumer <- sum (tabulka$stredy * tabulka$n.j) / n
vazeny_prumer

# vazeny rozptyl 
vazeny_rozptyl <- sum (tabulka$stredy^2 * tabulka$n.j) / n - vazeny_prumer^2
vazeny_rozptyl

# vektor poradi a serazeny vzorek
R <- rank (X)
X.sorted <- sort (X)

# median, kvartily, kvartilova odchylka
c.25 <- 0.25 * n
c.50 <- 0.5 * n
c.75 <- 0.75 * n
c (c.25, c.50, c.75)

x.25 <- (X.sorted[c.25] + X.sorted[c.25 + 1]) / 2
x.50 <- (X.sorted[c.50] + X.sorted[c.50 + 1]) / 2
x.75 <- (X.sorted[c.75] + X.sorted[c.75 + 1]) / 2
c (x.25, x.50, x.75)

# kvartilova odchylka (IQR)
q <- x.75 - x.25
q

# hradby boxplotu
c (x.25, x.75) + c (-1, 1) * 1.5 * q
c (x.25, x.75) + c (-1, 1) * 3 * q

# boxplot pomoci stejnojmenne funkce
boxplot (X, horizontal = TRUE, xlab = "pocty zapujcek", ylim = range (X), main = "krabicovy diagram (boxplot)")
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 0.8, add = TRUE)



# Q-Q plot
# tip: viz prednaska 7 a prislusny skript

# prevedeme poradi na faktor, abychom odfiltrovali duplicity
RR <- factor (R)
# zjistime poradi
j <- as.numeric (levels (RR))
# porovnejte
length (RR)
length (j)

# vzorecek pro alpha.j
alpha.j <- (j-.375)/(n+.25)

# prislusne kvantily standardizovaneho normalniho rozdeleni
u.j <-qnorm(alpha.j)

# analogicky odfiltrujeme duplicity ve vzorku
XX <- factor (X)
x <- as.numeric (levels (XX))
# porovnejte
length (XX)
length (j)

# vykreslime body grafu
plot (u.j, x, pch = 20, xlab = "teoreticky kvantil", ylab = "pozorovany kvantil", main = "Q-Q plot")

# prolozime primku
model <- lm (x ~ u.j)
lines (u.j, model$fitted.values, col = 2, lwd = 1.5)

# QQ jednoduseji
qqnorm(x)
qqline(x,col=2)

# N-P plot
# tip: viz prednaska 7

#alpha.j <- #...
u.j <- qnorm(alpha.j)

# vykreslime body grafu
plot (x, u.j, pch = 20, xlab = "pozorovana hodnota", ylab = "ocekavana normalni hodnota", main = "N-P plot")

# prolozime primku
model <- lm (u.j ~ x)
lines (x, model$fitted.values, col = 2, lwd = 1.5)


# P-P plot
# tip: viz prednaska 7

mx <- mean(x)
sx <- sd(x)

z <- sort((x-mx)/sx)

Phi <- pnorm(z)
n <- length(Phi)
F <- (1:n)/n

# vykreslime body grafu
plot (Phi, F, pch = 20, xlab = "teoreticka distribucni funkce", ylab = "empiricka distribucni funkce", main = "P-P plot")

# prikreslime primku y = x
abline (a = 0, b = 1, col = 2, lwd = 1.5)
