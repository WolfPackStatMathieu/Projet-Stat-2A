# install.packages("visNetwork")

# can have new features in development version 
# devtools::install_github("datastorm-open/visNetwork")

require(visNetwork, quietly = TRUE)
# minimal example
# nodes <- data.frame(id = 1:3)
# edges <- data.frame(from = c(1), to = c(1,2,3))
# visNetwork(nodes, edges, width = "100%")

# javascrtip api
# visDocumentation()
# vignette("Introduction-to-visNetwork") # with CRAN version
# # shiny example
# shiny::runApp(system.file("shiny", package = "visNetwork"))

#creation des nodes pour les fonctions du fichier surv.R
noms_fonction_surv.R <-c("simul_exp",
                         "Simuler biais un n ech",
                         "Calcul_estim_depuis_df",
                         "Simuler biais taillen",
                         "Calcul biais moyen taillen"
                         ,"plots_scenario_1"
                         ,"biais.selon.taille_echantillon"
                         ,"fonction_generation_taille_mean"
                         ,"tp.surv"
                         , "tps.surv"
                         ,"clep"
                         ,"simul_temps_exp"
                         ,"simul_temp_weibull"
                         ,"simul_temps_alt"
                         )
as.data.frame(noms_fonction_surv.R)
nodes_surv.R <- data.frame(id=1:length(noms_fonction_surv.R),
                           #add labels on nodes
                           label = noms_fonction_surv.R,
                           # control shape of nodes
                           shape = "square",
                           # color
                           color = "darkred",
                           # tooltip (html or character), when the mouse is above
                           title = paste0("<p><b>", 1:5,"</b><br>", noms_fonction_surv.R,
                                          "</p>")
                           )
nodes_surv.R
nodes <- nodes_surv.R
head(nodes)
edges_simul_exp <-data.frame(from= c(1), to = c(53))
edges_simuler_biais_un_n_ech<-data.frame(from= c(2), to = c(10))
edges <- rbind(edges_simul_exp, edges_simuler_biais_un_n_ech)
# edges_Calcul_estim_depuis_df<-data.frame(from= c(3), to = c())
# edges <- rbind(edges_simul_exp, edges_Calcul_estim_depuis_df)
edges_Simuler_biais_taillen<-data.frame(from= c(4), to = c(2))
edges<-rbind(edges, edges_Simuler_biais_taillen)
edges_Calcul_biais_moyen_taillen <- data.frame(from = c(5), to = c(4))
edges<-rbind(edges, edges_Calcul_biais_moyen_taillen)
edges_plots_scenario_1 <- data.frame(from = c(6), to = c(4))
edges<-rbind(edges, edges_plots_scenario_1)
edges_biais.selon.taille_echantillon <- data.frame(from = c(7), to = c(8))
edges<-rbind(edges, edges_biais.selon.taille_echantillon)
edges_fonction_generation_taille_mean <- data.frame(from = c(8), to = c(4))
edges<-rbind(edges, edges_fonction_generation_taille_mean)
edges_tp.surv <- data.frame(from = c(9), to = c(10))
edges<-rbind(edges, edges_tp.surv)
edges_tps.surv <- data.frame(from = c(10), to = c(11))
edges<-rbind(edges, edges_tps.surv)
# edges_clep <- data.frame(from = c(11), to = c())
# edges<-rbind(edges, edges_clep)
# edges_simul_temps_exp <- data.frame(from = c(12), to = c())
# edges<-rbind(edges, edges_simul_temps_exp)
# edges_simul_temp_weibull <- data.frame(from = c(13), to = c())
# edges<-rbind(edges, edges_simul_temp_weibull)
edges_simul_temps_alt <- data.frame(from = c(14), to = c(12))
edges<-rbind(edges, edges_simul_temps_alt)

visNetwork(nodes, edges, height = "500px", width = "100%")







