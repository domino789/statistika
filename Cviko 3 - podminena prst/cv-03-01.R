library (prob)

#	Priklad 1

kostky <- rolldie (2)
S <- probspace (kostky)
S

# A = 2 petky
# B = soucet delitelny peti

A <- subset (S, X1 == 5 & X2 == 5)
B <- subset (S, (X1 + X2) %% 5 == 0)
AB <- intersect (A, B)
Prob (AB) / Prob (B)

# Prvni odpovednik
Z <- subset (S, (X1 == 6 | X2 == 6))
V <- subset (S, (X1 + X2) > 10)
ZV <- intersect (Z, V)
Prob(ZV) / Prob(Z)
