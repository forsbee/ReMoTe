###################################################
# ReMoTe functions:                               #
# 1. initialize population                        #
# 2. generate infections and disease in one year  #
###################################################
# 7/27/21 this updates v2 of this code to have init.popn for mixing and no mixing populations
library(ggplot2)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(ggpubr)
library(gridExtra)
library(grid)
library(scales)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape)
library(kableExtra)
options(scipen = 999)


##############################################
## FUNCTION 1: Generate Baseline population ##
##############################################
# ari = ari high risk population, low.prev = percentage of population in low risk group
init.popn.mix <- function (ari.high,ari.low,low.prev,
                        pop.age=c(99000, 99000,92000,83000,85000,90000, 91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
                        median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87), stoch="N") {
  # ari.high: ari for high burden group (low risk group starts with no LTBI)
  # low.prev: proportion of population in low risk group; 
  # currently set to proportion uninfected in the population and uniformly distributed across age groups
  # default age groups are based on south africa
  # 4/27/21 functionality for stochasticity based on ARI is not yet implemented
  # 7/28/21: added low risk ARI for mixing populations
  
  num.age <- length(pop.age)
  
  if(stoch=="N"){
    # get number of expected infections by age using the ARI and median age for each age group
    E.La.hi <- ari.high*median.age #changed on 7/21/21
    E.La.lo <- ari.low*median.age 
    
    ## use poisson to get the expected number of people with 1, 2, etc. infections
    # first get the maximum number of infections individuals will have, given the poisson distribution of E.La.hi
    # set up matrix to store this with num.col indicated where P(individual with num.col infections)<0.00001 (~<1 person)
    tmp <- max(E.La.hi)
    max.inf <- min(which(ppois(c(1:1000),tmp)>0.99999)) # max number of infections someone can have
    prob.infection.hi <- matrix(NA, nrow=num.age, ncol=max.inf+1) #needs to be max.inf+1 columns since column 1 corresponds to 0 infections
    prob.infection.lo <- matrix(NA, nrow=num.age, ncol=max.inf+1) 
    
    ## Get the probability of having j infections, where j is the column number of num.infections              
    events<-0:max.inf
    for(i in 1:num.age){
      prob.infection.hi[i,]<-dpois(events,lambda=E.La.hi[i])  
      prob.infection.lo[i,]<-dpois(events,lambda=E.La.lo[i])  
    }
    
    ## Get the initial population
    # specify the population sizes for each
    low.risk.pop <- low.prev*pop.age
    high.risk.pop <- (1-low.prev)*pop.age
    
    pop.names <- c(0:max.inf)
    
    # high risk population: multiply the number of individuals in age group a, by the p(x infections|a)
    init.pop.hi <- as.data.frame(prob.infection.hi*high.risk.pop)
    init.pop.lo <- as.data.frame(prob.infection.lo*low.risk.pop)
    names(init.pop.hi) <- names(init.pop.lo) <- pop.names
    
    combined.pop <- init.pop.hi+init.pop.lo
    
    row.names(combined.pop) <- median.age
    
    return(list(all.pop=combined.pop,low.risk.pop=init.pop.lo,high.risk.pop=init.pop.hi,infect.probs.high=prob.infection.hi))
    
  }
  
  if(stoch=="Y"){# not developed yet, but allows stochasticity from ARI
    ## lognormal distribution of ARI: log(ramdha)~N(mean,sd^2) 
    ari<-matrix(NA, nrow=1000, ncol=1)   
    location <- log(x^2 / sqrt((x*0.1)^2 + x^2))
    shape <- sqrt(log(1 + ((x*0.1)^2 / x^2)))
    ari<- rlnorm(n=1000, location, shape)
    
    ## number of expected infections by age group E(Lai) = (e^ramdha*(median age+1)-1) where ramdha = ARI
    pop_age<-cbind(pop_age)*y
    median_age<-rbind(median_age)
    E_Lai<-matrix(NA, nrow=1000, ncol=18)
    
    for (i in 1:1000) {
      for (j in 1:18) {   
        E_Lai[i,j] <- exp(ari [i]*(median_age[j] + 1))-1
      }}
  }
}

init.popn.nomix <- function (ari,pop.age=c(99000, 99000,92000,83000,85000,90000, 91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
                                 median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87), stoch="N") {
# ari.high: ari for high burden group (low risk group starts with no LTBI)
# low.prev: proportion of population in low risk group; 
  # currently set to proportion uninfected in the population and uniformly distributed across age groups
# default age groups are based on south africa
# 4/27/21 functionality for stochasticity based on ARI is not yet implemented
# 7/28/21: put this one on the shelf, since it only generates cases in high risk group and we want to generate cases in 
  ## both high and low risk groups
  
  
  num.age <- length(pop.age)
  
  if(stoch=="N"){
    # get number of expected infections by age using the ARI and median age for each age group
    E.La.hi <- ari*median.age #changed on 7/21/21
    
    ## use poisson to get the expected number of people with 1, 2, etc. infections
    # first get the maximum number of infections individuals will have, given the poisson distribution of E.La.hi
    # set up matrix to store this with num.col indicated where P(individual with num.col infections)<0.00001 (~<1 person)
    tmp <- max(E.La.hi)
    max.inf <- min(which(ppois(c(1:1000),tmp)>0.99999)) # max number of infections someone can have
    prob.infection.hi <- matrix(NA, nrow=num.age, ncol=max.inf+1) #needs to be max.inf+1 columns since column 1 corresponds to 0 infections

    ## Get the probability of having j infections, where j is the column number of num.infections              
    events<-0:max.inf
    for(i in 1:num.age){
      prob.infection.hi[i,]<-dpois(events,lambda=E.La.hi[i])  
    }
  
    ## Get the initial population
    pop.names <- c(0:max.inf)
    
    # high risk population: multiple the number of individuals in age group a, by the p(x infections|a)
    init.pop.hi <- as.data.frame(prob.infection.hi*pop.age)
    names(init.pop.hi) <- pop.names
    
    combined.pop <- init.pop.hi
    
    row.names(combined.pop) <- median.age
    
    return(list(all.pop=combined.pop,infect.probs.high=prob.infection.hi))

  }
  
  if(stoch=="Y"){# not developed yet, but allows stochasticity from ARI
  ## lognormal distribution of ARI: log(ramdha)~N(mean,sd^2) 
    ari<-matrix(NA, nrow=1000, ncol=1)   
    location <- log(x^2 / sqrt((x*0.1)^2 + x^2))
    shape <- sqrt(log(1 + ((x*0.1)^2 / x^2)))
    ari<- rlnorm(n=1000, location, shape)
    
    ## number of expected infections by age group E(Lai) = (e^ramdha*(median age+1)-1) where ramdha = ARI
    pop_age<-cbind(pop_age)*y
    median_age<-rbind(median_age)
    E_Lai<-matrix(NA, nrow=1000, ncol=18)
    
    for (i in 1:1000) {
      for (j in 1:18) {   
        E_Lai[i,j] <- exp(ari [i]*(median_age[j] + 1))-1
      }}
  }
}

##########################################################################
## FUNCTION 2: GENERATE NEW INFECTIONS AND DISEASE OVER ONE YEAR PERIOD ##
##########################################################################
TB.sim <- function(init.pop,TB.prog,ari.high,ari.low,prog.old.inf=0.00075){
  # init.pop is the output list from init.popn(); 
  #   all.pop is the combined matrix of high and low risk where col# gives number of infections and the row number is the age group
  # TB.prog is information on TB progression rates and is a vector where
  #   TB.prog[i] is prob of progression for an individual with i infections
  # ari.high and ari.low are the ari for high and low risk groups
  # prog.old.inf is the probability of progressing to disease from an old infection
  
  # get the maximum number of infections individuals have
  max.inf <- ncol(init.pop$all.pop)-1
  
  # number of age groups
  num.age <- nrow(init.pop$all.pop)
  
  # generate new infections                                             #
  # we are only concerned with new infections and will only track those #
  #######################################################################
  # set up matrices to store results
  low.risk.pop.t1 <- high.risk.pop.t1 <- matrix(NA,nrow=num.age,ncol=max.inf+2) # need to add another column to allow for new infections

  # first those with the max number of infections previously who develop another infection
  low.risk.pop.t1[,(max.inf+2)] <- ari.low*init.pop$low.risk.pop[,(max.inf+1)]
  high.risk.pop.t1[,(max.inf+2)] <- ari.high*init.pop$high.risk.pop[,(max.inf+1)]
  
  # Then loop through and count new infections only
  #   we loop backwards (though not necessary since we are only counting new infections)
  for(i in (max.inf+1):2){
    low.risk.pop.t1[,i] <- ari.low*init.pop$low.risk.pop[,i-1]
    high.risk.pop.t1[,i] <- ari.high*init.pop$high.risk.pop[,i-1]
  }
  # since we are only tracking new infections there are no individuals with 0 new infections
  low.risk.pop.t1[,1] <- 0
  high.risk.pop.t1[,1] <- 0
  
  # create combined population with all new infections
  new.infections.t1 <- low.risk.pop.t1+high.risk.pop.t1
  
  # generate ATB from old infections with probability prog.old.inf
  ## first get the number of people with an old infection that did not get reinfected; 
  ##  these will only come from the high risk population since low risk have no infections
  old.inf.high <- rowSums(init.pop$high.risk.pop[,2:(max.inf+1)])-rowSums(high.risk.pop.t1)
  
  # store number of new ATB cases as the first column in the ATB matrix that will eventually be created
  ATB.old <- old.inf.high*prog.old.inf

  # Now generate ATB cases from new infections using TB.prog (P(progress|x infections); TB.prog must be max.inf+1 in length)
  TB.prog.mat <- matrix(rep(TB.prog,num.age),nrow=num.age,ncol=length(TB.prog),byrow=T)
  ATB.low <- low.risk.pop.t1[,2:(max.inf+2)]*TB.prog.mat
  ATB.high <- high.risk.pop.t1[,2:(max.inf+2)]*TB.prog.mat
  ATB.all.new <- ATB.low+ATB.high
  
  # total cases by age-includes cases from new and old infections
  num.cases <- rowSums(ATB.all.new) + ATB.old 
  names(num.cases) <- row.names(init.pop$all.pop)

  results.all <- list(init.pop=init.pop, # initial population
                      new.inf.pop.t1=new.infections.t1, # all new infections generate
                      low.pop.t1=low.risk.pop.t1, # new infections from low risk popn
                      high.pop.t1=high.risk.pop.t1,# new infections from high risk popn
                      high.pop.old.t1=old.inf.high, # number of old LTBI in high risk popn by age
                      ATB.new=ATB.all.new, # all new ATB cases from new infections by age and number of previous infections
                      cases.by.age=num.cases, # total number of cases by age-includes old and new infections
                      ATB.low=ATB.low,# low risk new ATB cases by age and number of previous infections
                      ATB.high=ATB.high, # low risk new ATB cases by age and number of previous infections
                      ATB.old=ATB.old #number of cases from old infections in high risk group
                      )
  
  return(results.all)
}

TB.sim.nomix <- function(init.pop,TB.prog,ari,prog.old.inf=0.00075){
  # init.pop is the output list from init.popn(); 
  #   all.pop is the combined matrix of high and low risk where col# gives number of infections and the row number is the age group
  # TB.prog is information on TB progression rates and is a vector where
  #   TB.prog[i] is prob of progression for an individual with i infections
  # ari.high and ari.low are the ari for high and low risk groups
  # prog.old.inf is the probability of progressing to disease from an old infection
  
  # get the maximum number of infections individuals have
  max.inf <- ncol(init.pop$all.pop)-1
  
  # number of age groups
  num.age <- nrow(init.pop$all.pop)
  
  # generate new infections                                             #
  # we are only concerned with new infections and will only track those #
  #######################################################################
  # set up matrices to store results
  new.infections.t1 <- matrix(NA,nrow=num.age,ncol=max.inf+2) # need to add another column to allow for new infections
  
  # first those with the max number of infections previously who develop another infection
  new.infections.t1[,(max.inf+2)] <- ari*init.pop$all.pop[,(max.inf+1)]

  # Then loop through and count new infections only
  #   we loop backwards (though not necessary since we are only counting new infections)
  for(i in (max.inf+1):2){
    new.infections.t1[,i] <- ari*init.pop$all.pop[,i-1]
  }
  # since we are only tracking new infections there are no individuals with 0 new infections
  new.infections.t1[,1] <- 0

  # generate ATB from old infections with probability prog.old.inf
  ## first get the number of people with an old infection that did not get reinfected; 
  old.inf <- rowSums(init.pop$all.pop[,2:(max.inf+1)])-rowSums(new.infections.t1)
  
  # store number of new ATB cases as the first column in the ATB matrix that will eventually be created
  ATB.old <- old.inf*prog.old.inf
  
  # Now generate ATB cases from new infections using TB.prog 
  ## (P(progress|x infections); TB.prog must be max.inf+1 in length)
  TB.prog.mat <- matrix(rep(TB.prog,num.age),nrow=num.age,ncol=length(TB.prog),byrow=T)
  ATB.all.new <- new.infections.t1[,2:(max.inf+2)]*TB.prog.mat

  # total cases by age-includes cases from new and old infections
  num.cases <- rowSums(ATB.all.new) + ATB.old 
  names(num.cases) <- row.names(init.pop$all.pop)
  
  results.all <- list(init.pop=init.pop, # initial population
                      new.inf.pop.t1=new.infections.t1, # all new infections generate
                      pop.old.t1=old.inf, # number of old LTBI by age
                      ATB.new=ATB.all.new, # all new ATB cases from new infections by age and number of previous infections
                      cases.by.age=num.cases, # total number of cases by age-includes old and new infections
                      ATB.old=ATB.old #number of cases from old infections in high risk group
  )
  
  return(results.all)
}


############################################################
## FUNCTION 3: GENERATE SUMMARY STATS FROM SIMULATED DATA ##
############################################################
sum.stats <- function(results.all,pars.tmp,mix=T){
  # results.all is output from TB.sim
  # pars is vector from standard par sheet: "TB.prevalence", "ARI.low.risk", "ARI.high.risk", "percent.low.risk",
  ##  "percent.high.risk", "progression.once.infected" "progression.2x" ..."progression.10x" 
  
  # function generates the following:
  ## -Cases from first infection, low risk
  ## -Cases from reinfection, low risk
  ## -Total cases, low risk
  ## -Cases from first infection, high risk
  ## -Cases from reinfection, high risk
  ## -Total cases, high risk
  ## -Cases from old infections (all high risk group)
  ## -Total cases
  ## -percent cases from first infection
  ## -percent cases from old infection
  ## -Incidence (per 100000)
  ## -Implied duration (=prev/Inc)
  ## -Rt for infection (use true prevalence as denominator and new infections as numerator)
  ## -Rt for disease (use true prevalence as denominator and new ATB as numerator)
  
  max.inf <- ncol(results.all$ATB.new)
  
  if(mix==T){
  
  ATB.low <- results.all$ATB.low; ATB.high <- results.all$ATB.high; ATB.new <- results.all$ATB.new;
  new.infections <- results.all$new.inf.pop.t1
  
  # by age, low risk
  ATB.1.low.age <- ATB.low[,1]
  ATB.mult.low.age <- rowSums(ATB.low[,2:max.inf])
  ATB.all.low.age <- rowSums(ATB.low)
  ATB.low.res <- data.frame(first.inf=ATB.1.low.age,mult.inf=ATB.mult.low.age,all=ATB.all.low.age)
  
  # overall, low risk
  ATB.1.low <- sum(ATB.low[,1])
  ATB.mult.low <- sum(rowSums(ATB.low[,2:max.inf]))
  ATB.all.low <- sum(rowSums(ATB.low))
  
  # by age, high risk
  ATB.1.high.age <- ATB.high[,1]
  ATB.mult.high.age <- rowSums(ATB.high[,2:max.inf])
  ATB.all.high.age <- rowSums(ATB.high)
  ATB.high.res <- data.frame(first.inf=ATB.1.high.age,mult.inf=ATB.mult.high.age,all=ATB.all.high.age)
  
  # overall, high risk
  ATB.1.high <- sum(ATB.high[,1])
  ATB.mult.high <- sum(rowSums(ATB.high[,2:max.inf]))
  ATB.all.high <- sum(rowSums(ATB.high))

  # cases from old infections
  ATB.all.old <- sum(results.all$ATB.old)
    
  # total number of cases                    
  ATB.tot <- sum(results.all$cases.by.age)
  
  # percent total new cases that are coming from first infection
  ## by age
  ATB.1 <- ATB.1.low+ATB.1.high
  perc.1.age <- round(ATB.1/results.all$cases.by.age,3)
  
  ## overall
  perc.1 <- sum(ATB.1)/ATB.tot
  
  # percent of cases from old infections
  perc.old <- sum(results.all$ATB.old)/ATB.tot

  # case summary
  case.results <- as.data.frame(c(ATB.1.low, ATB.mult.low, ATB.all.low, ATB.1.high, ATB.mult.high,ATB.all.high,ATB.all.old,ATB.tot,perc.1,perc.old))
  
  rownames(case.results) <- c("ATB,low,first","ATB,low,mult", "ATB,low,all", "ATB,high,first","ATB,high,mult", "ATB,high,all",
                              "ATB, old","Total ATB","percent first inf","percent old inf")
  
  # True prevalence (=1.66*smear+ prevalence)
  true.prev <- 1.66*pars.tmp[1]
    
  # Incidence
  incidence <- ATB.tot/10
  
  # Implied duration
  duration <- true.prev/incidence
  
  # Rt.inf
  Rt.inf <- sum(new.infections)/true.prev
  
  # Rt.dis
  Rt.dis <- (ATB.tot- ATB.all.old)/true.prev
  
  results <- list(ATB.low=ATB.low.res,ATB.high=ATB.high.res,totals=case.results,true.prev=true.prev,
                  incidence=incidence,duration=duration,Rt.inf=Rt.inf,Rt.dis=Rt.dis)
  
  return(results)
  #####################################################
  }else if(mix==F){
    ATB.new <- results.all$ATB.new;
    new.infections <- results.all$new.inf.pop.t1
    
    # cases from old infections
    ATB.all.old <- sum(results.all$ATB.old)
    
    # total number of cases                    
    ATB.tot <- sum(results.all$cases.by.age)
    
    # percent total new cases that are coming from first infection
    ## by age
    ATB.1 <- sum(ATB.new[,1])
    perc.1.age <- round(ATB.1/results.all$cases.by.age,3)
    
    ## overall
    perc.1 <- sum(ATB.1)/ATB.tot
    
    # percent of cases from old infections
    perc.old <- sum(results.all$ATB.old)/ATB.tot
    
    # case summary
    case.results <- as.data.frame(c(ATB.all.old,ATB.tot,perc.1,perc.old))
    
    rownames(case.results) <- c("ATB, old","Total ATB","percent first inf","percent old inf")
    
    # True prevalence (=1.66*smear+ prevalence)
    true.prev <- 1.66*pars.tmp[1]
    
    # Incidence
    incidence <- ATB.tot/10
    
    # Implied duration
    duration <- true.prev/incidence
    
    # Rt.inf
    Rt.inf <- sum(new.infections)/true.prev
    
    # Rt.dis
    Rt.dis <- (ATB.tot- ATB.all.old)/true.prev
    
    results <- list(totals=case.results,true.prev=true.prev,
                    incidence=incidence,duration=duration,Rt.inf=Rt.inf,Rt.dis=Rt.dis)
    
    return(results)
    
    
  }
}

################################################################
# function to run the overall simulation for input overall ari #
################################################################
runSimulation <- function(ari.overall,low.prev=0.8,
                          mix=T,
              TB.prog=c(0.04,0.02,0.01),
              prog.old.inf=0.00075,
              pop.age=c(99000, 99000,92000,83000,85000,90000,91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
              median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87)){
# TB.prog gives c(prog with one infect,prob with two, step increase after 2)
  
  if(mix==T){
    # first calculate the ARI for high and low risk populations
    ari.low.tmp <- ari.overall/(low.prev+(1-low.prev)*6)
    ari.high.tmp <- 6*ari.low.tmp
  
    # initialize population
     sim.popn <- init.popn.mix(ari.high=ari.high.tmp,
                ari.low=ari.low.tmp,
                low.prev=low.prev)
  
    # get TB progression rates
    max.inf <- ncol(sim.popn$all.pop)-1
  
    if(TB.prog[3]>0){
    TB.prog.sim <- c(TB.prog[1],seq(TB.prog[2],TB.prog[2]+TB.prog[3]*(max.inf-1),TB.prog[3]))
    TB.prog.sim[TB.prog.sim>1] <- 1
    }else{
      TB.prog.sim <- c(TB.prog[1],rep(TB.prog[2],max.inf))
      TB.prog.sim[TB.prog.sim>1] <- 1
    }
    
    # generate new cases of disease
    new.cases <- TB.sim(init.pop=sim.popn,
                      TB.prog=TB.prog.sim,
                      ari.high=ari.high.tmp,
                      ari.low=ari.low.tmp,
                      prog.old.inf)
  
    # get incidence
    ATB.tot <- sum(new.cases$cases.by.age)
    incidence <- ATB.tot/10
    new.infections <- sum(new.cases$new.inf.pop.t1[,2]) # number of people infected for the first time
    num.uninfected <- sum(new.cases$init.pop[[1]][,1]) # number of people never infected
  }else if(mix==F){
      # initialize population
      sim.popn <- init.popn.nomix(ari=ari.overall)
      
      # get TB progression rates
      max.inf <- ncol(sim.popn$all.pop)-1
      
      if(TB.prog[3]>0){
        TB.prog.sim <- c(TB.prog[1],seq(TB.prog[2],TB.prog[2]+TB.prog[3]*(max.inf-1),TB.prog[3]))
        TB.prog.sim[TB.prog.sim>1] <- 1
      }else{
        TB.prog.sim <- c(TB.prog[1],rep(TB.prog[2],max.inf))
        TB.prog.sim[TB.prog.sim>1] <- 1
      }
      
      # generate new cases of disease
      new.cases <- TB.sim.nomix(init.pop=sim.popn,
                          TB.prog=TB.prog.sim,
                          ari=ari.overall,
                          prog.old.inf)
      # get incidence
      ATB.tot <- sum(new.cases$cases.by.age)
      incidence <- ATB.tot/10
      new.infections <- sum(new.cases$new.inf.pop.t1[,2]) # number of people infected for the first time
      num.uninfected <- sum(new.cases$init.pop[[1]][,1]) # number of people never infected at start of simulation
    }
  
  return(list(incidence=incidence,
              new.infections=new.infections,
              num.uninfected=num.uninfected))
  
}

###########################################################
## RUN THE PARAMETER SWEEP WITH TARGET PARAMETERS
###########################################################
runSweep <- function(ari.goal=c(0.9,1.27,1.63,2,2.36,2.72,3.1,3.46,3.83,4.2)/100,
                     mix=T,low.prev=0.8,TB.prog=c(0.04,0.02,0.01),
                     ari.seek.min=0.0001,ari.seek.max=0.07,
                     prevs=seq(100,1000,100)){
  
  # get optimal ARI for mixing scenario
  aris <- seq(ari.seek.min,ari.seek.max,0.0001)
  aris.obs <- NULL
  for(i in 1:length(aris)){
    
    if(mix==T){
    tmp <- runSimulation(ari.overall=aris[i],low.prev=low.prev,
                         mix=T,
                         TB.prog=TB.prog,
                         prog.old.inf=0.00075,
                         pop.age=c(99000, 99000,92000,83000,85000,90000,91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
                         median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87))
    
    aris.obs[i] <- tmp$new.infections/tmp$num.uninfected
    }else if(mix==F){
    tmp <- runSimulation(ari.overall=aris[i],low.prev=low.prev,
                         mix=F,
                         TB.prog=TB.prog,
                         prog.old.inf=0.00075,
                         pop.age=c(99000, 99000,92000,83000,85000,90000,91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
                         median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87))
    
    aris.obs[i] <- tmp$new.infections/tmp$num.uninfected
    }
  }  
  
  # now select the ARI that works for our selected prevalences
  
  ARI.overall <- ARI.low <- ARI.high <- 
    ari.achieved <- NULL
  
  for(i in 1:length(ari.goal)){
    # get overall ARI for mixing scenario
    index <- which.min(abs(aris.obs-ari.goal[i]))
    ARI.overall[i] <- aris[index]*100
    ari.achieved[i] <- aris.obs[index]*100
    
    if(mix==T){
      ARI.low[i] <- ARI.overall[i]/(low.prev+(1-low.prev)*6)
      ARI.high[i] <- 6*ARI.low[i]
    }

  }
  
  results <- data.frame(prevs,prevs*1.66,
                        ARI.overall,
                        ARI.low,ARI.high,ari.achieved)
  names(results) <- c("Prev(per100k)","Correct Prev","overall ARI",
                      "low risk ARI","high risk ARI","ARI achieved")
  
  return(results)
}

##########################################################################
## Function to get ARIs and run entire simulation with those parameters ##
##########################################################################
runAllSim <- function(ari.goal=c(0.9,1.27,1.63,2,2.36,2.72,3.1,3.46,3.83,4.2)/100,
                      mix=T,low.prev=0.8,TB.prog=c(0.04,0.02,0.01),
                      ari.seek.min=0.0001,ari.seek.max=0.07,
                      prevs=seq(100,1000,100),
                      pop.age=c(99000, 99000,92000,83000,85000,90000,91000,79000,63000,54000,44000,37000,30000, 23000,15000,10000,5000,1000),
                      median.age=c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87)){
  

  # First get ARI that will be appropriate for this #
  ## only necessary if mix=T                       ##
  ###################################################
  overall.ARIs <- NULL
  if(mix==T){
    ARI.seek <- runSweep(ari.goal=ari.goal,mix=T,low.prev=low.prev,TB.prog=TB.prog,
                         ari.seek.min=0.0001,ari.seek.max=0.07,
                         prevs=prevs)
    # output is: c("Prev(per100k)","Correct Prev","overall ARI",
    #     "low risk ARI","high risk ARI","ARI achieved")
    overall.ARIs <- ARI.seek[,3] #actual ARIs
    ari.goal <- overall.ARIs/100
  }

  results.sim <- list()
  for(i in 1:length(prevs)){
    
    results.sim[[i]] <- runSimulation(ari.overall=ari.goal[i],low.prev=low.prev,
                                      mix=mix,TB.prog=TB.prog)
    
  }
  
  return(list(results.sim,overall.ARIs))
}

