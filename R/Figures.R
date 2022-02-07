# file to generate figures for the paper #
##########################################
source("./R/utils.R")

num.age <- 18

# homogeneous risk; no protection
load("./Output/noMix_noackley_plotData.RData")
baseline.age.nomix <- num.inf.age
baseline.numinf.nomix <- num.inf.nomix
output.nomix.noprot <- print.nomix

# homogeneous risk; protection
load("./Output/noMix_ackley_plotData.RData")
output.nomix.prot <- print.nomix

# heterogeneous risk; no protection
load("./Output/Mix_noackley_plotData.RData")
baseline.age.mix <- num.inf.age
baseline.numinf.mix <- num.inf.mix
output.mix.noprot <- print.mix

# heterogeneous risk; protection
load("./Output/Mix_ackley_plotData.RData")
output.mix.prot <- print.mix

# heterogeneous risk; unadjust ARI
load("./Output/Mix_DoddARI_plotData.RData")
baseline.age.mix.DoddARI <- num.inf.age
baseline.numinf.mix.DoddARI <- num.inf.mix

# Figure 1: homogeneous mixing baseline conditions
#######################################################
## A) infected individuals by age and prevalence
max.inf <- max(baseline.age.mix$total.infections)
colorCount <- num.age
getPalette <- colorRampPalette(brewer.pal(9,"Paired"))
ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="Total number of infected individuals by age and prevalence",x="TB prevalence",y="Total number baseline infections")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))

## B) number of prior infections by prevalence
ggplot(data=baseline.numinf.nomix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="Distribution of the number of prior infections",x="TB prevalence",y="Total number of individuals")+
  scale_fill_brewer(name="Number of infections",palette="Paired")

# Figure 2: heterogeneous baseline conditions, unadjusted ARI
###############################################################
baseline.age.mix.DoddARI <- num.inf.age
baseline.numinf.mix.DoddARI <- num.inf.mix
## A) infected individuals by age and prevalence
max.inf <- max(baseline.age.mix.DoddARI$total.infections)
ggplot(data=baseline.age.mix.DoddARI[baseline.age.mix.DoddARI$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="Total number of infected individuals by age and prevalence",x="TB prevalence",y="Total number baseline infections")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))

## B) number of prior infections by prevalence
ggplot(data=baseline.numinf.mix.DoddARI,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="Distribution of the number of prior infections",x="TB prevalence",y="Total number of individuals")+
  scale_fill_brewer(name="Number of infections",palette="Paired")

# Figure 3: heterogeneous baseline population with adjusted ARI
#################################################################
## A) infected individuals by age and prevalence
max.inf <- max(baseline.age.mix$total.infections)
ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="Total number of infected individuals by age and prevalence",x="TB prevalence",y="Total number baseline infections")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))

## B) number of prior infections by prevalence
ggplot(data=baseline.numinf.mix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="Distribution of the number of prior infections",x="TB prevalence",y="Total number of individuals")+
  scale_fill_brewer(name="Number of infections",palette="Paired")

# Figure 4: Cases of TB generated over 1 year
#################################################
## A) without mixing, no protection from progression, increasing risk
ggplot(data=output.nomix.noprot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,2100))+
  labs(title="Number of new cases",x="TB prevalence",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

## B) without mixing, protection against progression, increasing risk with infections
ggplot(data=output.nomix.prot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,2100))+
  labs(title="Number of new cases",x="TB prevalence",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

## C) mixing, adjusted ARI, no protection against progression and increasing risk
max.count <- max(sum(output.mix.noprot$count[output.mix.noprot$prevalence==1000]))
ggplot(data=output.mix.noprot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="Number of new cases",x="TB prevalence",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

## D) mixing, adjusted ARI, protection against progression and increasing risk
max.count <- max(sum(output.mix.prot$count[output.mix.prot$prevalence==1000]))
ggplot(data=output.mix.prot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="Number of new cases",x="TB prevalence",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

###########################
# Supplementary figures   #
###########################
# Supp Fig 2: change in obs incidence when risk of progression varies from 0.04-0.06, 
### with and without waning protection

# Supp Fig 3: change in obs incidence when risk of progression varies from 0.04-0.06, 
### with and without waning protection
