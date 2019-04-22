#	Priklad 1

# Working dir must be statistika/data dir!!!
data <- read.csv (file = "znamky.csv", header = TRUE, sep = ",", dec = ".")

#	pocet skupin
r <- nrow (data)

#	absolutni cetnosti
n.j <- data$cetnosti

#	pocet pozorovani
n <- sum (n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j)

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

#	tabulka 
tabulka <- data.frame (znamka = data$znamka, n.j, p.j, N.j, F.j)
tabulka
# sloupce tabulky jsou pojmenovane, lze k nim pristupovat take pomoci jejich nazvu

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = tabulka$znamka, xlab = "znamka", ylab = "pocet pozorovani", main = "sloupkovy diagram")

#	graf cetnostni funkce
plot (tabulka$znamka, tabulka$n.j, type = "p", pch = 20, cex = 2, col = "red", xlab = "znamka", ylab = "hodnoty cetnostni funkce", main = "cetnostni funkce")

# polygon cetnosti
plot (tabulka$znamka, tabulka$n.j, type="b", pch = 20, xlab = "znamka", ylab = "pocet pozorovani", main = "polygon cetnosti", col = "red", cex = 2, lty = 2)

# graf empiricke distribucni funkce
plot (c(0,tabulka$znamka,5), c(0,tabulka$F.j,1), type="s", xlab = "znamka", ylab = "ECDF", main="empiricka distribucni funkce", col = "red", lwd = 2)

X <- rep (tabulka$znamka, tabulka$n.j)
plot (ecdf (X), col = "red", lwd = 2, xlab = "znamka", ylab = "ECDF", main="empiricka distribucni funkce")

# prumer
prumer <- sum (X) / n
prumer
# v R je pro prumer funkce "mean"
mean (X)

# rozpyl a smerodatna odchylka
rozptyl <- mean (X^2) - prumer^2
rozptyl
smerodatna_odchylka <- sqrt (rozptyl)
smerodatna_odchylka
# v R je pro vyberovy rozptyl funkce "var", ale je potreba jej zkorigovat na rozptyl z prednasky
var (X) * (n-1) / n

# krabicovy diagram (boxplot)
boxplot (X, horizontal = TRUE, ylim = range (X), main = "krabicovy diagram (boxplot)")
# pro nazornost muzeme prikreslit i jednotliva pozorovani
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)

#	kvantily a krabicovy diagram
# vektor poradi a serazeny vzorek
R <- rank (X)
R
X.sorted <- sort (X)
X.sorted

# median, kvartily, kvartilova odchylka
c.25 <- 0.25 * n
c.50 <- 0.50 * n
c.75 <- 0.75 * n
c (c.25, c.50, c.75)

# kvantily
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

# porovnejte s nasledujicim
median (X)
quantile (X, c (0.25, 0.5, 0.75))
# 25% kvantil se lisi, R totiz pri vypoctu kvantilu vzdy pouziva prumerovani dvou okolnich hodnot

# boxplot pomoci stejnojmenne funkce
boxplot (X, horizontal = TRUE, ylim = range (X), xlab = "znamka", main = "krabicovy diagram (boxplot)")
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)
# "fousy" v R oznacuji nejmensi a nejvetsi pozorovani, ktera jeste jsou uvnitr vnitrni hradby 



#	=====================================================================================================================================
# Priklad 2

# ... podobne jako v Prikladu 1
data <- read.csv (file = "ocel.csv", header = TRUE, sep = ",", dec = ".")
str (data)

# dopocitejte si stredy a delky jednotlivych intervalu

# stredy intervalu
data$stredy <- (data$mez.d + data$mez.h) / 2
# delky intervalu 
d.j <- data$mez.h - data$mez.d

#	pocet skupin
r <- nrow (data)

#	absolutni cetnosti
n.j <- data$pocet

#	pocet pozorovani
n <- sum (n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j)

# cestnostni hustota 
f.j <- p.j/d.j

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n


#	tabulka 
tabulka <- data.frame (dolni = data$mez.d, horni = data$mez.h, stredy = data$stredy, n.j, p.j, N.j, F.j, f.j)
tabulka
# sloupce tabulky jsou pojmenovane, lze k nim pristupovat take pomoci jejich nazvu

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), xlab = "ocel", ylab = "pocet pozorovani", main = "sloupkovy diagram")

#	graf cetnostni funkce
plot (tabulka$stredy, tabulka$n.j, type = "p", pch = 20, cex = 2, col = "red", xlab = "plasticita", ylab = "hodnoty cetnostni funkce", main = "cetnostni funkce")

# polygon cetnosti
plot (tabulka$stredy, tabulka$n.j, type="b", pch = 20, xlab = "plasticita", ylab = "pocet pozorovani", main = "polygon cetnosti", col = "red", cex = 2, lty = 2)


# sloupkovy diagram relativnich cetnosti
barplot (tabulka$p.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), xlab = "plasticita", ylab = "relativna cestnost", main = "sloupkovy diagram")

# sloupkovy diagram cetnostni hustoty
barplot (tabulka$f.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), space=0,  xlab = "plasticita", ylab = "cetnostni hustota", main = "sloupkovy diagram")

# histogram 
X <- rep (tabulka$stredy, tabulka$n.j)
hist (X, breaks = c(tabulka$dolni[1], tabulka$horni), freq = FALSE, col = "yellow", xlab="mez plasticity", ylab="cetnostni hustota")

# graf empiricke distribucni funkce
plot (c(0,tabulka$dolni[1], tabulka$horni, 200), c(0,0,tabulka$F.j,1), type="b", xlab = "mez plasticity", ylab = "ECDF", main="empiricka distribucni funkce", col = "red", lwd = 2)


X <- rep (tabulka$stredy, tabulka$n.j)
plot (ecdf (X), col = "red", lwd = 2, xlab = "znamka", ylab = "ECDF", main="empiricka distribucni funkce")
 
# rozpyl a smerodatna odchylka
rozptyl <- mean (X^2) - prumer^2
rozptyl
smerodatna_odchylka <- sqrt (rozptyl)
smerodatna_odchylka
# v R je pro vyberovy rozptyl funkce "var", ale je potreba jej zkorigovat na rozptyl z prednasky
var (X) * (n-1) / n


# krabicovy diagram (boxplot)
boxplot (X, horizontal = TRUE, ylim = range (X), main = "krabicovy diagram (boxplot)")
# pro nazornost muzeme prikreslit i jednotliva pozorovani
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)

#	kvantily a krabicovy diagram
# vektor poradi a serazeny vzorek
R <- rank (X)
R
X.sorted <- sort (X)
X.sorted

# median, kvartily, kvartilova odchylka
c.25 <- 0.25 * n
c.50 <- 0.50 * n
c.75 <- 0.75 * n
c (c.25, c.50, c.75)

# kvantily
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

# porovnejte s nasledujicim
median (X)
quantile (X, c (0.25, 0.5, 0.75))
# 25% kvantil se lisi, R totiz pri vypoctu kvantilu vzdy pouziva prumerovani dvou okolnich hodnot

# boxplot pomoci stejnojmenne funkce
boxplot (X, horizontal = TRUE, ylim = range (X), xlab = "znamka", main = "krabicovy diagram (boxplot)")
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)
# "fousy" v R oznacuji nejmensi a nejvetsi pozorovani, ktera jeste jsou uvnitr vnitrni hradby



#	=====================================================================================================================================
# Priklad 3 

puvodni_prumer <- 110
puvodni_rozptyl <- 800
n <- 20

opraveny_prumer <- puvodni_prumer - (85 + 120) / n + (95 + 150) / n
opraveny_prumer

opraveny_rozptyl <- puvodni_rozptyl - (85^2 + 120^2) / n + puvodni_prumer^2 + (95^2 + 150^2) / n - opraveny_prumer^2
opraveny_rozptyl


#	=====================================================================================================================================
# Priklad 4


# ... podobne jako v Prikladu 1
data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")
str (data)
#	pocet skupin
r <- nrow (data)
d.j <- floor(max(data$cnt)/sqrt(r))
d <- z <- split(sort(data$cnt), c(d.j)) 
split(sort(data$cnt),matrix(1:27,27,length(data$cnt))[1:length(data$cnt)])

dolni <- seq(from = min(data$cnt)-0.5, to = max(data$cnt)-0.5, by = d.j)
horni <- seq(from = dolni[2], to = max(data$cnt)+(d.j-1), by = d.j)
stredy <- dolni + (d.j/2)
cetnosti <- rows.per.group  <- aggregate(data$cnt,
                                         by=interval(dolni,horni), sum)
s <- sum(data$cnt > dolni & data$cnt < horni)
cut(data$cnt, b = 0:322,include.lowest = T)
#	absolutni cetnosti
n.j <- d.j

#	pocet pozorovani
n <- sum (n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j)

# cestnostni hustota 
f.j <- p.j/d.j

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n


#	tabulka 
tabulka <- data.frame (dolni = data$mez.d, horni = data$mez.h, stredy = data$stredy, n.j, p.j, N.j, F.j, f.j)
tabulka
# sloupce tabulky jsou pojmenovane, lze k nim pristupovat take pomoci jejich nazvu

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), xlab = "ocel", ylab = "pocet pozorovani", main = "sloupkovy diagram")

#	graf cetnostni funkce
plot (tabulka$stredy, tabulka$n.j, type = "p", pch = 20, cex = 2, col = "red", xlab = "plasticita", ylab = "hodnoty cetnostni funkce", main = "cetnostni funkce")

# polygon cetnosti
plot (tabulka$stredy, tabulka$n.j, type="b", pch = 20, xlab = "plasticita", ylab = "pocet pozorovani", main = "polygon cetnosti", col = "red", cex = 2, lty = 2)


# sloupkovy diagram relativnich cetnosti
barplot (tabulka$p.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), xlab = "plasticita", ylab = "relativna cestnost", main = "sloupkovy diagram")

# sloupkovy diagram cetnostni hustoty
barplot (tabulka$f.j, names.arg = paste(tabulka$dolni, "-",tabulka$horni), space=0,  xlab = "plasticita", ylab = "cetnostni hustota", main = "sloupkovy diagram")

# histogram 
X <- rep (tabulka$stredy, tabulka$n.j)
hist (X, breaks = c(tabulka$dolni[1], tabulka$horni), freq = FALSE, col = "yellow", xlab="mez plasticity", ylab="cetnostni hustota")

# graf empiricke distribucni funkce
plot (c(0,tabulka$dolni[1], tabulka$horni, 200), c(0,0,tabulka$F.j,1), type="b", xlab = "mez plasticity", ylab = "ECDF", main="empiricka distribucni funkce", col = "red", lwd = 2)


X <- rep (tabulka$stredy, tabulka$n.j)
plot (ecdf (X), col = "red", lwd = 2, xlab = "znamka", ylab = "ECDF", main="empiricka distribucni funkce")

# a pridejte je do tabulky 

# krabicovy diagram (boxplot)
boxplot (X, horizontal = TRUE, ylim = range (X), main = "krabicovy diagram (boxplot)")
# pro nazornost muzeme prikreslit i jednotliva pozorovani
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)