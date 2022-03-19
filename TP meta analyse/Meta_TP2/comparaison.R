# Librairies --------------------------------------------------------------

library(readr)
library(metafor)


# Chargement --------------------------------------------------------------

dat_su <- read_table2("dat_su.txt")
dat_sita <- read_table2("dat_sita.txt")


# Modèle ------------------------------------------------------------------
# Fixed -------------------------------------------------------------------



results_sita <- rma.uni(ai=r1, n1i=N1, ci=r0, n2i=N0, measure="OR", data = dat_sita,
                      method = "FE", weighted = TRUE, level = 95)


forest(results_sita, main="Forest plot - Sitamachin", atransf=exp)


results_su <- rma.uni(ai=r1, n1i=N1,
                      ci=r0, n2i=N0, measure="OR",
                      data = dat_su, method = "FE",
                      weighted = TRUE, level = 95)

forest(results_su, main="Forest plot - Sumachin", atransf=exp)



res_itc0 <- c(results_sita$beta-results_su$beta,
              sqrt((results_sita$se)^2+(results_su$se)^2),
              exp(results_sita$beta-results_su$beta))



res_itc <- c(res_itc0,
             exp(res_itc0[1]-qnorm(0.975)*res_itc0[2]),
             exp(res_itc0[1]+qnorm(0.975)*res_itc0[2]))


names(res_itc) <- c("loghr","logse","hr","cll","clu")

res_itc


# Random ------------------------------------------------------------------

results_sita <- rma.uni(ai=r1, n1i=N1, ci=r0, n2i=N0, measure="OR", data = dat_sita,
                        method = "REML", weighted = TRUE, level = 95)


forest(results_sita, main="Forest plot - Sitamachin", atransf=exp)


results_su <- rma.uni(ai=r1, n1i=N1,
                      ci=r0, n2i=N0, measure="OR",
                      data = dat_su, method = "REML",
                      weighted = TRUE, level = 95)

forest(results_su, main="Forest plot - Sumachin", atransf=exp)


res_itc0 <- c(results_sita$beta-results_su$beta,
              sqrt((results_sita$se)^2+(results_su$se)^2),
              exp(results_sita$beta-results_su$beta))



res_itc_re <- c(res_itc0,
             exp(res_itc0[1]-qnorm(0.975)*res_itc0[2]),
             exp(res_itc0[1]+qnorm(0.975)*res_itc0[2]))


names(res_itc_re) <- c("loghr","logse","hr","cll","clu")

res_itc_re

