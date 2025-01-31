# run sensitivity analyses and plot the results #
#################################################
source("./utils.R")

prevs <- seq(100,1000,100)

# TB.prog scenarios
TB.prog1 <- c(0.05,0.02,0.01)
TB.prog2 <- c(0.04,0.02,0.01)
TB.prog3 <- c(0.06,0.02,0.01)
TB.prog4 <- c(0.05,0.02,0.005)
TB.prog5 <- c(0.05,0.02,0.0)
TB.prog.all <- rbind(TB.prog1,TB.prog2,TB.prog3,TB.prog4,TB.prog5)

# low.prev scenarios
low.prev.all <- seq(0.6,0.95,0.05)

# ARI ratio
ari.rat <- seq(4,8,1)

# protection from first infection (main analysis uses 60%)
protection <- c(0.4,0.6,0.8)
TB.prog.prot <- cbind(rep(0.05,3),0.02,c(0.01,0.1,0.05))

# low prevalence simulations-vary proportion at low risk of exposure #
######################################################################
low.prev.res <- list()
for(i in 1:length(low.prev.all)){
  low.prev.res[[i]] <- runAllSim(TB.prog=TB.prog1,low.prev=low.prev.all[i],mix=T)
}

# TB prog simulations-vary risk of disease progression #
########################################################
TB.prog.res <- list()
for(i in 1:length(TB.prog.all[,1])){
  TB.prog.res[[i]] <- runAllSim(TB.prog=TB.prog.all[i,],low.prev=0.8,mix=T)
}

# vary ARI.rat #
################
ARI.rat.res <- list()
for(i in 1:length(ari.rat)){
  ARI.rat.res[[i]] <- runAllSim(ARI.rat=ari.rat[i],mix=T)
}

# vary protection from prior infection #
########################################
TB.prot.res <- list()
for(i in 1:length(TB.prog.prot[,1])){
  TB.prot.res[[i]] <- runAllSim(TB.prog=TB.prog.prot[i,],low.prev=0.8,mix=T)
}


# CREATE PLOTS OF RESULTS #
###########################
# change in low prev and incidence
######################################
incidences <- matrix(0,nr=length(low.prev.all),ncol=length(prevs))
aris <- matrix(0,nr=length(low.prev.all),ncol=length(prevs))
for(i in 1:length(low.prev.all)){
  incidences[i,] <- matrix(unlist(low.prev.res[[i]][[1]]),ncol=10)[1,]
  aris[i,] <- low.prev.res[[i]][[2]]
}

plot.dat <- data.frame(low.prev=rep(low.prev.all,10),Prevalence=rep(prevs,each=length(low.prev.all)),
                       ari=c(aris),incidence=c(incidences))

# incidence versus risk plot
ggplot(plot.dat,aes(x=Prevalence,y=incidence,color=low.prev))+geom_point()+
  labs(x="Prevalence",y="Observed incidence")+scale_color_continuous(name="Proportion\n Low Risk")


# ari versus risk plot
ggplot(plot.dat,aes(x=Prevalence,y=ari,color=low.prev))+geom_point()+
  labs(x="Prevalence",y="Actual ARI")+scale_color_continuous(name="Proportion\n Low Risk")

## Change in risk of TB disease assumptions 
#############################################
incidences <- matrix(0,nr=length(TB.prog.all[,1]),ncol=length(prevs))
aris <- matrix(0,nr=length(TB.prog.all[,1]),ncol=length(prevs))
for(i in 1:length(TB.prog.all[,1])){
  incidences[i,] <- matrix(unlist(TB.prog.res[[i]][[1]]),ncol=10)[1,]
  aris[i,] <- TB.prog.res[[i]][[2]]
}

plot.dat.2 <- data.frame(Prevalence=rep(prevs,each=5),Ackley=rep(c("Yes","Yes","Yes","Yes","No"),10),
                         fast=rep(c(0.01,0.01,0.01,0.005,NA),10),Prog1=rep(c(0.05,0.04,0.06,0.05,0.05),10),
                         ari=c(aris),incidence=c(incidences))

# plot of prevalence versus incidence by ackley/no ackley
############################################################
plot.dat.3a <- subset(plot.dat.2,plot.dat.2$fast==0.01)
plot.dat.3b <- subset(plot.dat.2,is.na(plot.dat.2$fast))
plot.dat.3 <- data.frame(rbind(plot.dat.3a,plot.dat.3b))
ggplot(plot.dat.3,aes(x=Prevalence,y=incidence,shape=as.factor(Ackley),color=as.factor(Prog1)))+
  geom_point()+
  labs(y="Incidence",color="TB progression\n with one infection",shape="Waning protection")

plot.dat.4 <- subset(plot.dat.2,plot.dat.2$Prog1==0.05)
ggplot(plot.dat.4,aes(x=Prevalence,y=incidence,shape=as.factor(Ackley),color=as.factor(fast)))+
  geom_point()+
  labs(y="Incidence",color="Progression rate",shape="Waning protection")

# change in ARI ratio between low/high
#########################################
incidences <- matrix(0,nr=length(ari.rat),ncol=length(prevs))
aris <- matrix(0,nr=length(ari.rat),ncol=length(prevs))
for(i in 1:length(ari.rat)){
  incidences[i,] <- matrix(unlist(ARI.rat.res[[i]][[1]]),ncol=10)[1,]
  aris[i,] <- ARI.rat.res[[i]][[2]]
}

plot.dat.ari <- data.frame(ari.rat=rep(ari.rat,10),Prevalence=rep(prevs,each=length(ari.rat)),
                       ari=c(aris),incidence=c(incidences))

# incidence versus risk plot
ggplot(plot.dat.ari,aes(x=Prevalence,y=incidence,color=ari.rat))+geom_point()+
  labs(x="Prevalence",y="Observed incidence")+scale_color_continuous(name="ARI ratio")

# ari versus risk plot
ggplot(plot.dat.ari,aes(x=Prevalence,y=ari,color=ari.rat))+geom_point()+
  labs(x="Prevalence",y="Actual ARI")+scale_color_continuous(name="ARI ratio")

# change in protection from prior infection
#############################################
incidences <- matrix(0,nr=length(TB.prog.prot[,1]),ncol=length(prevs))
aris <- matrix(0,nr=length(TB.prog.prot[,1]),ncol=length(prevs))
for(i in 1:length(TB.prog.prot[,1])){
  incidences[i,] <- matrix(unlist(TB.prot.res[[i]][[1]]),ncol=10)[1,]
  aris[i,] <- TB.prot.res[[i]][[2]]
}

plot.dat.prot <- data.frame(protection=rep(protection,10),Prevalence=rep(prevs,each=length(TB.prog.prot[,1])),
                           ari=c(aris),incidence=c(incidences))

# incidence versus risk plot
ggplot(plot.dat.prot,aes(x=Prevalence,y=incidence,color=protection))+geom_point()+
  labs(x="Prevalence",y="Observed incidence")+scale_color_continuous(name="Protection\nfrom first\ninfection")

# ari versus risk plot-there is no impact on ARI from this.
ggplot(plot.dat.prot,aes(x=Prevalence,y=ari,color=protection))+geom_point()+
  labs(x="Prevalence",y="Actual ARI")+scale_color_continuous(name="Protection\nfrom first\ninfection")
