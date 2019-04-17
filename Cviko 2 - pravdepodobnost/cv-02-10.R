# Priklad 10

n <- 100000

# nastaveni grafickych parametru, vice viz "?par"
par (mfrow = c(1,1), mar = c (4,4,4,1))
# prikazem "plot" s parametrem type="n" prichystame prazdny souradnicovy system 
plot (c(0,24), c(0,24), type = "n", xlab = "A", ylab = "B")

# funkce "runif" generuje vektor (delky n) nahodnych cisel nezavisle generovanych v zadanem intervalu 
A <- runif (n, 0, 24)
B <- runif (n, 0, 24)

# funkce "cbind" vytvari matici z vektoru (matic) tak, ze je umisti jako sloupce vedle sebe
# funkce "data.frame" vytvari datovou tabulku
# v datove tabulce sloupce odpovidaji jednotlivym promennych, radky odpovidaji jednotlivym merenim (opakovanim pokusu)
V <- data.frame (cbind (A, B))
# logicka formule
V$ok <- (V$B - 1 <= V$A) & (V$A <= V$B + 2) 
V$color <- ifelse (V$ok, "#00cc00", "#ff0000")
V$symbol <- ifelse (V$ok, 4, 4)

# prikaz "points" vykresluje body, jejich souradnice jsou zadany vektory jako prvni dva argumenty
# parametr col nastavuje barvu, parametr pch kreslici symbol, lwd tloustku car
points (V$A, V$B, col = V$color, pch = V$symbol, lwd = 2)
# spocitame a do obrazku vypiseme relativni cetnost
nA <- sum (V$ok)
fA <- nA / n
title (main = paste (nA, "/", n, "=", fA))
