# Simulated heterogeneous risk with Dodd ARIs #
###############################################
source("./utils.R")

library(xlsx)

pars <- read.xlsx("../Parameters/Pars_2021_11_5.xlsx",sheetName = "Mixing and Ackley")
prevs <- pars[,1]
low.prev <- pars[1,3]
ari.goal <- c(0.9,1.27,1.63,2,2.36,2.72,3.1,3.46,3.83,4.2)/100
ari.low <- ari.goal/2
ari.high <- 6*ari.low
pop.age=c(99000, 99000,92000,83000,85000,90000, 91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000)
median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87)

sim.data <- list()
for(i in 1:length(pars[,1])){
  
  sim.data[[i]] <- init.popn.mix(ari.high=ari.high[i],
                                 ari.low=ari.low[i],
                                 low.prev=pars[1,3])
} 
num.prev <- length(unique(prevs)); num.age <-length(median.age)
# data frame for plotting with age,prev,total infections
num.inf.age <- data.frame(prevalence=rep(prevs,each=num.age+1),
                          age.group=rep(c(0,median.age),num.prev),
                          total.infections=rep(NA,num.prev*(num.age+1)))

num.inf.mix <-   
  data.frame(prevalence=rep(prevs,each=10),
             num.inf=rep(c(as.character(c(0:8)),"9+"),num.prev),
             count=rep(NA,num.prev*10))

for(i in 1:10){
  
  # total number of infected individuals in each age group by prevalence
  #########################################################################
  tmp.inf <- sim.data[[i]]$all.pop
  tmp.inf <- tmp.inf[,2:ncol(tmp.inf)]
  ### total.infections: (total  infections, age specific number of infections)
  num.inf.age$total.infections[((i-1)*(num.age+1)+1):(i*(num.age+1))] <- round(c(sum(tmp.inf),apply(tmp.inf,1,sum)))
  
  # total number of individuals with 0, 1,...,9+ infections by prevalence
  ##########################################################################
  tmp.inf <- sim.data[[i]]$all.pop
  ### compact together all individuals with 9+ infections
  tmp.inf <- cbind(tmp.inf[,1:9],apply(tmp.inf[,10:ncol(tmp.inf)],1,sum)) 
  num.inf.mix$count[((i-1)*10+1):(10*i)] <- round(apply(tmp.inf,2,sum))
  
}

save(num.inf.age,num.inf.mix,
     file="../Output/Mix_DoddARI_plotData.RData")
