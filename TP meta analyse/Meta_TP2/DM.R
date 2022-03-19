# Librairies --------------------------------------------------------------

library(readr)
library(metafor)


# Chargement --------------------------------------------------------------

Weight <- read_table2("C:/Users/vivi1/Desktop/Ensai 3A/Meta_Analyse/BDD/Weight.txt")
names(Weight) <- c("s", "nref", "n", "trt", "y", "sdy")


# Fixed -------------------------------------------------------------------

results_trt_2 <- rma.uni(yi=y, vi=sqrt(sdy), n1i = n, n2i = nref, measure="GEN", slab = paste("Étude ", s),
                         data = Weight[which(Weight$trt == 2),], method = "FE", weighted = TRUE, level = 95)
results_trt_3 <- rma.uni(yi=y, vi=sqrt(sdy), n1i = n, n2i = nref, measure="GEN", slab = paste("Étude ", s),
                         data = Weight[which(Weight$trt == 3),], method = "FE", weighted = TRUE, level = 95)
results_trt_4 <- rma.uni(yi=y, vi=sqrt(sdy), n1i = n, n2i = nref, measure="GEN", slab = paste("Étude ", s),
                         data = Weight[which(Weight$trt == 4),], method = "FE", weighted = TRUE, level = 95)

forest(results_trt_2, main="Forest plot - Trt2 vs trt1")
forest(results_trt_3, main="Forest plot - Trt3 vs trt1")
forest(results_trt_4, main="Forest plot - Trt4 vs trt1")

res <- c(results_trt_2$beta-results_trt_3$beta, results_trt_2$beta-results_trt_4$beta, results_trt_3$beta-results_trt_4$beta,
              sqrt((results_trt_2$se)^2+(results_trt_3$se)^2),
              sqrt((results_trt_2$se)^2+(results_trt_4$se)^2),
              sqrt((results_trt_3$se)^2+(results_trt_4$se)^2))

res <- c(res,
         res[1]-qnorm(0.975)*res[4], res[1]+qnorm(0.975)*res[4],
         res[2]-qnorm(0.975)*res[5], res[2]+qnorm(0.975)*res[5],
         res[3]-qnorm(0.975)*res[6], res[3]+qnorm(0.975)*res[6])

names(res) <- c("2-3", "2-4", "3-4", "sd2-3", "sd2-4", "sd3-4", "IC_inf2-3", "IC_sup2-3", "IC_inf2-4", "IC_sup2-4", "IC_inf3-4", "IC_sup3-4")

res

# Random ------------------------------------------------------------------

results_rd_2 <- rma.uni(yi=y, vi=sqrt(sdy), n1i = n, n2i = nref, measure="GEN", slab = paste("Étude ", s),
                         data = Weight[which(Weight$trt == 2),], method = "REML", weighted = TRUE, level = 95)
results_rd_3 <- rma.uni(yi=y, vi=sqrt(sdy), n1i = n, n2i = nref, measure="GEN", slab = paste("Étude ", s),
                         data = Weight[which(Weight$trt == 3),], method = "REML", weighted = TRUE, level = 95)
results_rd_4 <- rma.uni(yi=y, vi=sqrt(sdy), n1i = n, n2i = nref, measure="GEN", slab = paste("Étude ", s),
                         data = Weight[which(Weight$trt == 4),], method = "REML", weighted = TRUE, level = 95)

forest(results_rd_2, main="Forest plot - Trt2 vs trt1")
forest(results_rd_3, main="Forest plot - Trt3 vs trt1")
forest(results_rd_4, main="Forest plot - Trt4 vs trt1")

res2 <- c(results_rd_2$beta-results_rd_3$beta, results_rd_2$beta-results_rd_4$beta, results_rd_3$beta-results_rd_4$beta,
         sqrt((results_rd_2$se)^2+(results_rd_3$se)^2),
         sqrt((results_rd_2$se)^2+(results_rd_4$se)^2),
         sqrt((results_rd_3$se)^2+(results_rd_4$se)^2))

res2 <- c(res2,
         res[1]-qnorm(0.975)*res[4], res[1]+qnorm(0.975)*res[4],
         res[2]-qnorm(0.975)*res[5], res[2]+qnorm(0.975)*res[5],
         res[3]-qnorm(0.975)*res[6], res[3]+qnorm(0.975)*res[6])

names(res2) <- c("2-3", "2-4", "3-4", "sd2-3", "sd2-4", "sd3-4", "IC_inf2-3", "IC_sup2-3", "IC_inf2-4", "IC_sup2-4", "IC_inf3-4", "IC_sup3-4")

res2

