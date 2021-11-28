validation <- bind_rows(sim_validation, .id = "Simulation")

rm(list=ls())

load("results/Grazing_C1.RData")
grazing_C1 <- data.frame(matrix(unlist(Grazing_visits), nrow=length(Grazing_visits), byrow=T))
grazing_C1 <- data.frame(t(grazing_C1))

load("results/Grazing_C2.RData")
grazing_C2 <- data.frame(matrix(unlist(Grazing_visits), nrow=length(Grazing_visits), byrow=T))
grazing_C2 <- data.frame(t(grazing_C2))

load("results/Grazing_O1.RData")
grazing_O1 <- data.frame(matrix(unlist(Grazing_visits), nrow=length(Grazing_visits), byrow=T))
grazing_O1 <- data.frame(t(grazing_O1))

load("results/Grazing_O2.RData")
grazing_O2 <- data.frame(matrix(unlist(Grazing_visits), nrow=length(Grazing_visits), byrow=T))
grazing_O2 <- data.frame(t(grazing_O2))


grazing <- bind_rows(grazing_C1,grazing_C2,grazing_O1,grazing_O2)

save(grazing, file = "results/grazing_visits.RData")


