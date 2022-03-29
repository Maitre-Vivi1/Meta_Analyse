rm(list = ls())


# Deberdt -----------------------------------------------------------------

N_e = 65
N_c = 68
Df = N_e + N_c -2
MD = -0.82 - 0.99
P = 0.088

# Trouvons la T valeur
t <- qt(P/2,Df,lower.tail = T)
# égal à 0.42
SE = abs(MD/t)
SD = SE / sqrt((1/N_c)+(1/N_e))

# Gureje ------------------------------------------------------------------

# D'après les recommandations du Cochrane
N_e = 33
N_c = 32
Df = N_e + N_c - 2
MD = 4.5-4.9
P = 0.673

# Trouvons la T valeur
t <- qt(0.3365,Df,lower.tail = T)
# égal à 0.42
SE = abs(MD/t)
SD = SE / sqrt((1/N_c)+(1/N_e))


# Stroup ------------------------------------------------------------------

# Imputons la SD du changement à baseline


