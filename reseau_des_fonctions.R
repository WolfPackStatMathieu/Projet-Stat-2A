# install.packages("visNetwork")

# can have new features in development version 
# devtools::install_github("datastorm-open/visNetwork")
rm(list=ls())
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

############creation des nodes pour les fonctions du fichier surv.R #####
noms_fonction_surv.R <-c("simul_exp", #surv.R
                         "Simuler_biais_un_n_ech",
                         "Calcul_estim_depuis_df",
                         "Simuler_biais_taillen",
                         "Calcul_biais_moyen_taillen"
                         ,"plots_scenario_1"
                         ,"biais.selon.taille_echantillon"
                         ,"fonction_generation_taille_mean"
                         ,"tp.surv"
                         , "tps.surv"
                         ,"clep"
                         ,"simul_temps_exp"
                         ,"simul_temp_weibull"
                         ,"simul_temps_alt"
                         ,"simul_weibull"
                         ,"Generation_un_ech"
                         ,"fonction_Bern"
                         ,"fonction_KM"
                         ,"fonction_cure"
                         ,"simul_tps_hht" # simul_delai.R
                         ,"fonction_find_lambda"
                         ,"temps_simul1"
                         ,"temps_simul2"
                         ,"estimateur_cure_mult" #estimateurs/estimateur_cure
                         ,"fonction_simul_doses_mean" #generation_doses.R
                         ,"fonction_generation_eqm"
                         ,"function_estim_doses"# "Generation_un_ech" "fonction_Bern" "tp.surv" "tps.surv"
                         ,"fonction_estim_doses_sizen"
                         ,"Realisations_estim_cas_mult"
                         ,"fonction_simul_doses_eqm"
                         ,"get_alpha" #generation_echantillon/fonctions_simulations_competition.R
                         ,"get_expo"
                         ,"hx"
                         ,"one_weibull_comp"
                         ,"get_weibull"
                         ,"get_dataset0"
                         ,"fonction_generation_taille_differente" # generation_taille_differente.R
                         ,"fonction_graph_fonc_size"
                         ,"flexsurvcure"
                         ,"biais_pi"
                         ,"fonction_biais_survie"
                         ,"simul_bernoulli"
                         ,"eqm.selon.taille_echantillon"
                         ,"fonction_generation_taille_eqm"
                         ,"fonction_sapply"
                         ,"function_estim_doses_comp" #generation_comp fonction_Bern
                         ,"generation_comp" #get_alpha
                         ,"generation_comp_mean"  #function_estim_doses_comp
                         ,"evol_biais_comp" #generation_comp_mean evol_n_par_dose
                         
                         )
fonctions <-as.data.frame(noms_fonction_surv.R)
fonctions
nodes_surv.R <- data.frame(id=1:length(noms_fonction_surv.R),
                           #add labels on nodes
                           label = noms_fonction_surv.R,
                           # control shape of nodes
                           shape = "square",
                           group = c("GrA"), 
                           # color
                           color = "darkred",
                           # tooltip (html or character), when the mouse is above
                           title = paste0("<p><b>", 1:5,"</b><br>", noms_fonction_surv.R,
                                          "</p>")
                           ,# smooth
                           smooth = c(TRUE)
                           )
nodes <- nodes_surv.R
head(nodes)

########### Gestion de l'affichage des nodes #########
#fonction generant les graphiques du scenario 1
nodes$shape[nodes$label == "plots_scenario_1"] <-"triangle"
nodes$color[nodes$label == "plots_scenario_1"] <-"blue"
nodes$shape[nodes$label == "biais.selon.taille_echantillon"] <-"triangle"
nodes$color[nodes$label == "biais.selon.taille_echantillon"] <-"blue"
nodes$shape[nodes$label == "eqm.selon.taille_echantillon"] <-"triangle"
nodes$color[nodes$label == "eqm.selon.taille_echantillon"] <-"blue"



# fonctions relevant du package flexsurvcure
nodes$color[nodes$label == "flexsurvcure"] <-"black"
nodes$shape[nodes$label == "Simuler_biais_taillen"] <-"star"


head(nodes)

####### Edges #######
# edges_simul_exp <-data.frame(from= c(1), to = c())#rien


edges_Simuler_biais_un_n_ech<-data.frame(from= c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_un_n_ech"))), 
                                         to = c(which(fonctions$noms_fonction_surv.R %in% c("Generation_un_ech", "fonction_Bern" ,"fonction_KM", "fonction_cure")))) 

# edges_Calcul_estim_depuis_df<-data.frame(from= c(3), to = c())#rien
which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_un_n_ech"))
edges_Simuler_biais_taillen<-data.frame(from= c(4), 
                                        to = c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_un_n_ech")))) 
which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_taillen"))
edges_Calcul_biais_moyen_taillen <- data.frame(from = c(5),
                                               to = c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_taillen"))))
edges_plots_scenario_1 <- data.frame(from = c(6), 
                                     to = c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_taillen"))))

edges_biais.selon.taille_echantillon <- data.frame(from = c(7),
                                                   to = c(which(fonctions$noms_fonction_surv.R %in% c("fonction_generation_taille_mean"))))
edges_fonction_generation_taille_mean <- data.frame(from = c(8), 
                                                    to = c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_taillen")))) #Simuler_biais_taillen
edges_tp.surv <- data.frame(from = c(9),
                            to = c(which(fonctions$noms_fonction_surv.R %in% c("tps.surv")))) #tps.surv
edges_tps.surv <- data.frame(from = c(10),
                             to = c(which(fonctions$noms_fonction_surv.R %in% c("clep")))) #clep
# edges_clep <- data.frame(from = c(11), to = c())#rien
# edges_simul_temps_exp <- data.frame(from = c(12), to = c())#rien
# edges_simul_temp_weibull <- data.frame(from = c(13), to = c())#rien

edges_simul_temps_alt <- data.frame(from = c(14), 
                                    to = c(which(fonctions$noms_fonction_surv.R %in% c("simul_temps_exp")))) #simul_temps_exp
# edges_simul_weibull <- data.frame(from = c(15), to = )
edges_Generation_un_ech <- data.frame(from = c(16), 
                                      to = c(which(fonctions$noms_fonction_surv.R %in% c("simul_weibull")))) #simul_weibull
# edges_fonction_Bern <- data.frame(from = c(17), to = c()) #rien
edges_fonction_KM <- data.frame(from = c(18), 
                                to = c(which(fonctions$noms_fonction_surv.R %in% c("tp.surv")))) #tp.surv
# edges_fonction_cure <- data.frame(from = c(19), to = c())
edges_simul_tps_hht <- data.frame(from = c(20), 
                    to = c(which(fonctions$noms_fonction_surv.R %in% c("fonction_find_lambda", "temps_simul1" ,"temps_simul2")))) #
# edges_fonction_find_lambda <- data.frame(from = c(21), 
#                                          to = c()) # rien
edges_temps_simul1 <- data.frame(from = c(22),
                      to = c(which(fonctions$noms_fonction_surv.R %in% c("simul_weibull")))) #
edges_temps_simul2 <- data.frame(from = c(23),
                                 to = c(which(fonctions$noms_fonction_surv.R %in% c("simul_weibull")))) #
# edges_estimateur_cure_mult <- data.frame(from = c(24), 
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) #rien
edges_fonction_simul_doses_mean <- data.frame(from = c(25),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("fonction_generation_taille_mean")))) #fonction_generation_taille_mean
edges_fonction_generation_eqm <- data.frame(from = c(26),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_taillen", "fonction_sapply")))) #Simuler_biais_taillen fonction_sapply
edges_function_estim_doses <- data.frame(from = c(27),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("Generation_un_ech", "fonction_Bern", "flexsurvcure", "tp.surv"
                                                                        )))) #Generation_un_ech, fonction_Bern, flexsurvcure, tp.surv
edges_fonction_estim_doses_sizen <- data.frame(from = c(28),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("function_estim_doses")))) #function_estim_doses
edges_Realisations_estim_cas_mult <- data.frame(from = c(29),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("function_estim_doses")))) #function_estim_doses
edges_fonction_simul_doses_eqm <- data.frame(from = c(30),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("fonction_generation_eqm")))) #fonction_generation_eqm
# edges_get_alpha <- data.frame(from = c(31),
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) #rien
# edges_get_expo <- data.frame(from = c(32),
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) # rien
# edges_hx <- data.frame(from = c(33),
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) # rien
edges_one_weibull_comp <- data.frame(from = c(34),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("hx")))) # hx
edges_get_weibull <- data.frame(from = c(35),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("one_weibull_comp")))) # one_weibull_comp
edges_get_dataset0 <- data.frame(from = c(36),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("get_expo", "get_weibull")))) #get_expo, get_weibull
edges_fonction_generation_taille_differente <- data.frame(from = c(37),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("biais_pi", "fonction_biais_survie")))) #biais_pi, fonction_biais_survie
edges_fonction_graph_fonc_size <- data.frame(from = c(38),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("fonction_generation_taille_differente")))) #fonction_generation_taille_differente
# edges_flexsurvcure <- data.frame(from = c(39), 
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) # rien
edges_biais_pi <- data.frame(from = c(40),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("simul_bernoulli")))) #simul_bernoulli
# edges_fonction_biais_survie <- data.frame(from = c(41), 
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) #  n'existe plus

# edges_simul_bernoulli <- data.frame(from = c(42),
#                      to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) # rien
edges_eqm.selon.taille_echantillon <- data.frame(from = c(43),
                     to = c(which(fonctions$noms_fonction_surv.R %in% c("fonction_generation_taille_eqm")))) # fonction_generation_taille_eqm
edges_fonction_generation_taille_eqm <- data.frame(from = c(44),
                                                 to = c(which(fonctions$noms_fonction_surv.R %in% c("Simuler_biais_taillen")))) # Simuler_biais_taillen
# edges_fonction_sapply <- data.frame(from = c(45),
#                                                    to = c(which(fonctions$noms_fonction_surv.R %in% c("")))) # rien 
edges_function_estim_doses_comp <- data.frame(from = c(which(fonctions$noms_fonction_surv.R %in% c("function_estim_doses_comp"))),
                                                   to = c(which(fonctions$noms_fonction_surv.R %in% c("function_estim_doses_comp", "fonction_Bern")))) 
edges_generation_comp <- data.frame(from = c(which(fonctions$noms_fonction_surv.R %in% c("generation_comp"))),
                                              to = c(which(fonctions$noms_fonction_surv.R %in% c("get_alpha")))) # # get_alpha

edges_generation_comp_mean <- data.frame(from = c(48),
                                    to = c(which(fonctions$noms_fonction_surv.R %in% 
                                                   c("function_estim_doses_comp")))) # function_estim_doses_comp

edges_evol_biais_comp <- data.frame(from = c(49),
                                         to = c(which(fonctions$noms_fonction_surv.R %in% 
                                                        c("generation_comp_mean")))) #  #generation_comp_mean evol_n_par_dose


###### Liens #########
which(fonctions$noms_fonction_surv.R %in% c(""))
edges <- rbind(#edges_simul_exp, 
               edges_Simuler_biais_un_n_ech
               ,edges_Simuler_biais_taillen
               ,edges_Calcul_biais_moyen_taillen
               ,edges_plots_scenario_1
               ,edges_biais.selon.taille_echantillon
               ,edges_fonction_generation_taille_mean
               ,edges_tp.surv
               ,edges_tps.surv
               #,edges_clep
               #,edges_simul_temps_exp
               #,edges_simul_temp_weibull
               ,edges_simul_temps_alt
               #,edges_simul_weibull
               ,edges_Generation_un_ech
               #,edges_fonction_Bern
               ,edges_fonction_KM
               # ,edges_fonction_cure
               ,edges_simul_tps_hht
               #,edges_fonction_find_lambda
               ,edges_temps_simul1
               ,edges_temps_simul2
               #,edges_estimateur_cure_mult
               ,edges_fonction_simul_doses_mean
               ,edges_fonction_generation_eqm
               ,edges_function_estim_doses
               ,edges_fonction_estim_doses_sizen
               ,edges_Realisations_estim_cas_mult
               ,edges_fonction_simul_doses_eqm
               #,edges_get_alpha
               #,edges_get_expo
               #,edges_hx
               ,edges_one_weibull_comp
               ,edges_get_weibull
               ,edges_get_dataset0
               ,edges_fonction_generation_taille_differente
               ,edges_fonction_graph_fonc_size
               # ,edges_flexsurvcure
               ,edges_biais_pi
               #,edges_fonction_biais_survie
               # ,edges_simul_bernoulli
               ,edges_eqm.selon.taille_echantillon
               ,edges_fonction_generation_taille_eqm
               # ,edges_fonction_sapply
               ,edges_function_estim_doses_comp
               ,edges_generation_comp
               ,edges_generation_comp_mean
               ,edges_evol_biais_comp
               )

edges


visNetwork(nodes, edges, height = "500px", width = "100%",
           main = "Réseau des fonctions utilisées", 
           submain = list(text = "",
                          style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;"), 
           footer = "Fig. réseau des fonctions") %>%
  visEdges(arrows = "to") %>% 
  # visHierarchicalLayout(direction = "LR") #%>%
  visEvents(click = "function(nodes){ Shiny.onInputChange('click', nodes.nodes[0]); if(nodes.nodes[0]){ no_of_edges = this.getConnectedEdges(nodes.nodes[0]); alert('No. of Edges connected to the selected node are :  ' + no_of_edges); } Shiny.onInputChange('edge_connections', no_of_edges); ;}", selectEdge = "function(edges) { Shiny.onInputChange('edge_data', this.body.data.edges._data[edges.edges[0]]); ;}", selectNode = "function(nodes) { no_of_nodes = this.getConnectedNodes(nodes.nodes[0]); no_of_edges_2 = this.getConnectedEdges(no_of_nodes); Shiny.onInputChange('node_data', no_of_edges_2); ;}" )
