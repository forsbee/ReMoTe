# do parameter sweep to determine optimal ARI 
source("./R/utils.R")

# get optimal ARI for mixing scenario
aris <- seq(0.0001,0.3,0.0001)
incidences.mix <- incidences.nomix <- NULL
for(i in 1:length(aris)){

  tmp <- runSimulation(ari.overall=aris[i],low.prev=0.8,
                       mix=T,
              TB.prog=c(0.04,0.02,0.01),
              prog.old.inf=0.00075,
              pop.age=c(99000, 99000,92000,83000,85000,90000,91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
              median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87))

  incidences.mix[i] <- tmp

  tmp <- runSimulation(ari.overall=aris[i],low.prev=0.8,
                       mix=F,
                       TB.prog=c(0.04,0.02,0.01),
                       prog.old.inf=0.00075,
                       pop.age=c(99000, 99000,92000,83000,85000,90000,91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
                       median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87))
  
  incidences.nomix[i] <- tmp
}  

# now select the ARI that works for our selected prevalences
prevs <- seq(100,1000,100)*1.66
incidence.goal <- prevs*0.5
low.prev  <- 0.8
ARI.overall.mix <- ARI.low <- ARI.high <- ARI.overall.nomix <-
  incidence.achieved.mix <- incidence.achieved.nomix <- NULL

for(i in 1:length(prevs)){
  # get overall ARI for mixing scenario
  index <- which.min(abs(incidences.mix-incidence.goal[i]))
  ARI.overall.mix[i] <- aris[index]*100
  incidence.achieved.mix[i] <- incidences[index]
  ARI.low[i] <- ARI.overall.mix[i]/(low.prev+(1-low.prev)*6)
  ARI.high[i] <- 6*ARI.low[i]
  
  # get ARI for no mixing scenario
  index <- which.min(abs(incidences.nomix-incidence.goal[i]))
  ARI.overall.nomix[i] <- aris[index]*100
  incidence.achieved.nomix[i] <- incidences.nomix[index]
  
}

results <- data.frame(prevs,prevs/1.66,incidence.goal,
                 ARI.overall.nomix,incidence.achieved.nomix,
                 ARI.overall.mix,ARI.low,ARI.high,incidence.achieved.mix)
names(results) <- c("Prev(per100k)","Correct Prev","Target Inc","overall ARI, no mix",
                    "Incidence achieved, no mix","overall ARI, mix","low ARI, mix",
                    "high ARI, mix","Incidence achieved, mix")

write.csv(results,"./Parameters/parSweepAll_2yr.csv")
