# Librairies --------------------------------------------------------------

library(readr)
library(metafor)


# Chargement --------------------------------------------------------------

Adverse_event <- read_table2("BDD/Adverse_event_regroup.txt")
names(Adverse_event) <- substring(names(Adverse_event), 1, nchar(names(Adverse_event))-2)


# Fixed -------------------------------------------------------------------

results_trt_2 <- rma.uni(ai=r_ae_trt, n1i=n, ci=r_ae_trt_ref, n2i=nref, measure="OR", data = Adverse_event[which(Adverse_event$trt == 2),],
                        method = "FE", weighted = TRUE, level = 95)
results_trt_3 <- rma.uni(ai=r_ae_trt, n1i=n, ci=r_ae_trt_ref, n2i=nref, measure="OR", data = Adverse_event[which(Adverse_event$trt == 3),],
                         method = "FE", weighted = TRUE, level = 95)
results_trt_4 <- rma.uni(ai=r_ae_trt, n1i=n, ci=r_ae_trt_ref, n2i=nref, measure="OR", data = Adverse_event[which(Adverse_event$trt == 4),],
                         method = "FE", weighted = TRUE, level = 95)

forest(results_trt_2, main="Forest plot - Trt2 vs trt1", atransf=exp)
forest(results_trt_3, main="Forest plot - Trt3 vs trt1", atransf=exp)
forest(results_trt_4, main="Forest plot - Trt4 vs trt1", atransf=exp)

results_trt_2$I2
results_trt_3$I2
results_trt_4$I2

res <- c(results_trt_2$beta-results_trt_3$beta, results_trt_2$beta-results_trt_4$beta, results_trt_3$beta-results_trt_4$beta,
         sqrt((results_trt_2$se)^2+(results_trt_3$se)^2),
         sqrt((results_trt_2$se)^2+(results_trt_4$se)^2),
         sqrt((results_trt_3$se)^2+(results_trt_4$se)^2))

res <- c(res,
         exp(res[1]-qnorm(0.975)*res[4]), exp(res[1]+qnorm(0.975)*res[4]),
         exp(res[2]-qnorm(0.975)*res[5]), exp(res[2]+qnorm(0.975)*res[5]),
         exp(res[3]-qnorm(0.975)*res[6]), exp(res[3]+qnorm(0.975)*res[6]))

names(res) <- c("2-3", "2-4", "3-4", "sd2-3", "sd2-4", "sd3-4", "IC_inf2-3", "IC_sup2-3", "IC_inf2-4", "IC_sup2-4", "IC_inf3-4", "IC_sup3-4")

res


# Random ------------------------------------------------------------------

results_rd_2 <- rma.uni(ai=r_ae_trt, n1i=n, ci=r_ae_trt_ref, n2i=nref, measure="OR", data = Adverse_event[which(Adverse_event$trt == 2),],
                         method = "REML", weighted = TRUE, level = 95)
results_rd_3 <- rma.uni(ai=r_ae_trt, n1i=n, ci=r_ae_trt_ref, n2i=nref, measure="OR", data = Adverse_event[which(Adverse_event$trt == 3),],
                         method = "REML", weighted = TRUE, level = 95)
results_rd_4 <- rma.uni(ai=r_ae_trt, n1i=n, ci=r_ae_trt_ref, n2i=nref, measure="OR", data = Adverse_event[which(Adverse_event$trt == 4),],
                         method = "REML", weighted = TRUE, level = 95)

forest(results_rd_2, main="Forest plot - Trt2 vs trt1", atransf=exp)
forest(results_rd_3, main="Forest plot - Trt3 vs trt1", atransf=exp)
forest(results_rd_4, main="Forest plot - Trt4 vs trt1", atransf=exp)

results_rd_2$I2
results_rd_3$I2
results_rd_4$I2

res2 <- c(results_rd_2$beta-results_rd_3$beta, results_rd_2$beta-results_rd_4$beta, results_rd_3$beta-results_rd_4$beta,
         sqrt((results_rd_2$se)^2+(results_rd_3$se)^2),
         sqrt((results_rd_2$se)^2+(results_rd_4$se)^2),
         sqrt((results_rd_3$se)^2+(results_rd_4$se)^2))

res2 <- c(res2,
         exp(res2[1]-qnorm(0.975)*res2[4]), exp(res2[1]+qnorm(0.975)*res2[4]),
         exp(res2[2]-qnorm(0.975)*res2[5]), exp(res2[2]+qnorm(0.975)*res2[5]),
         exp(res2[3]-qnorm(0.975)*res2[6]), exp(res2[3]+qnorm(0.975)*res2[6]))

names(res2) <- c("2-3", "2-4", "3-4", "sd2-3", "sd2-4", "sd3-4", "IC_inf2-3", "IC_sup2-3", "IC_inf2-4", "IC_sup2-4", "IC_inf3-4", "IC_sup3-4")

res2
