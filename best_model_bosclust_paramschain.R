library(ordinalClust)
coclust <- readRDS("Results/bosclust/object12.rds")
kc_group_list <- list("dscrgrp", "vteurmmb","impcntr", "gincdig","ipcrtiva", "likrisk")

# for(kr in 1:12){
#     png(paste0("Images/stability_plot_pi_", kr, ".png"), width = 4800, height = 3200, res = 300)
#     par(mfrow = c(2, 3))

#     for (kc_group in 1:6){
#         toplot <- numeric(150)

#         for(i in 1:150){

#             toadd <- coclust@paramschain[[kc_group]]$pis[kr,1,i]
#             toplot[i] <- toadd
#         }

#         graphics::plot(toplot, type = "l", ylim = c(0,1),
#              col = "hotpink3", main = paste("pi_", kr, kc_group_list[[kc_group]], " values"),
#              ylab = "pi values", xlab = "SEM-Gibbs iterations")
#     }
#     dev.off()
# }

# png(paste0("Images/stability_plot_mu_12", ".png"), width = 4800, height = 3200, res = 300)
# par(mfrow = c(2, 3))

# for (kc_group in 1:6){
#     toplot <- numeric(150)

#     for(i in 1:150){

#         toadd <- coclust@paramschain[[kc_group]]$mus[kr,1,i]
#         toplot[i] <- toadd
#     }

#     graphics::plot(toplot, type = "l", ylim = c(0,7),
#             col = "hotpink3", main = paste("mu_", kr, kc_group_list[[kc_group]], " values"),
#             ylab = "mu values", xlab = "SEM-Gibbs iterations")
# }
# dev.off()

png(paste0("Images/stability_plot_rho", ".png"), width = 4800, height = 3200, res = 300)
par(mfrow = c(4, 3))

for (kc_group in 1:6){
    toplot <- numeric(150)

    for(i in 1:150){

        toadd <- coclust@paramschain[[kc_group]]$mus[kr,1,i]
        toplot[i] <- toadd
    }

    graphics::plot(toplot, type = "l", ylim = c(0,7),
            col = "hotpink3", main = paste("mu_", kr, kc_group_list[[kc_group]], " values"),
            ylab = "mu values", xlab = "SEM-Gibbs iterations")
}
dev.off()


