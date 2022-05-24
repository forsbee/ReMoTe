# file to generate figures for the paper #
##########################################
source("./utils.R")

library(ggpubr)
library(cowplot)

num.age <- 18

# homogeneous risk; protection
load("../Output/noMix_noackley_plotData.RData")
baseline.age.nomix <- num.inf.age
baseline.numinf.nomix <- num.inf.nomix
output.nomix.prot <- print.nomix

# homogeneous risk; no protection
load("../Output/noMix_ackley_plotData.RData")
output.nomix.noprot <- print.nomix

# heterogeneous risk; protection
load("../Output/Mix_noackley_plotData.RData")
baseline.age.mix <- num.inf.age
baseline.numinf.mix <- num.inf.mix
output.mix.prot <- print.mix

# heterogeneous risk; no protection
load("../Output/Mix_ackley_plotData.RData")
output.mix.noprot <- print.mix

# heterogeneous risk; unadjust ARI
load("../Output/Mix_DoddARI_plotData.RData")
baseline.age.mix.DoddARI <- num.inf.age
baseline.numinf.mix.DoddARI <- num.inf.mix

# Figure 1: homogeneous mixing baseline conditions
#######################################################
## A) infected individuals by age and prevalence
max.inf <- max(baseline.age.mix$total.infections)
colorCount <- num.age
getPalette <- colorRampPalette(brewer.pal(9,"Paired"))

fig1a <- ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="(a)",x="TB prevalence (per 100000)",y="Total infected at baseline")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))

## B) number of prior infections by prevalence
fig1b <- ggplot(data=baseline.numinf.nomix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="(b)",x="TB prevalence (per 100000)",y="Total number of individuals")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")

ggarrange(fig1a,fig1b,nrow=2,ncol=2)

# Figure 2: heterogeneous baseline conditions, unadjusted ARI
###############################################################
baseline.age.mix.DoddARI <- num.inf.age
baseline.numinf.mix.DoddARI <- num.inf.mix
## A) infected individuals by age and prevalence
max.inf <- max(baseline.age.mix.DoddARI$total.infections)
fig2a <- ggplot(data=baseline.age.mix.DoddARI[baseline.age.mix.DoddARI$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="(a)",x="TB prevalence (per 100000)",y="Total infected at baseline")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))

fig2a <- ggplot(data=baseline.age.mix.DoddARI[baseline.age.mix.DoddARI$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="",x="TB prevalence (per 100000)",y="Total infected at baseline")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))


## B) number of prior infections by prevalence
fig2b <- ggplot(data=baseline.numinf.mix.DoddARI,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="",x="TB prevalence (per 100000)",y="Total number of individuals")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")


# Figure 3: heterogeneous baseline population with adjusted ARI
#################################################################
## A) infected individuals by age and prevalence
max.inf <- max(baseline.age.mix$total.infections)
fig3a <- ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="",x="TB prevalence (per 100000)",y="Total infected at baseline")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))

fig3a <- ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="",x="TB prevalence (per 100000)",y="Total infected at baseline")+
  scale_fill_manual(name="Median\nage",values=getPalette(colorCount))

legend1 <- get_legend(fig3a)
  
## B) number of prior infections by prevalence
fig3b <- ggplot(data=baseline.numinf.mix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="(d)",x="TB prevalence (per 100000)",y="Total number of individuals")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")

prow <- ggdraw(
  plot_grid(
    plot_grid(fig2a + theme(legend.position="none") ,
              fig3a +theme(legend.position="none"),
              align = 'vh',
              
              labels = c("a", "b"),
              
              hjust = -1,
              
              nrow = 1,
              axis="1"),
    # as suggested by aosmith you can add additional columns to make legends appear closer
    plot_grid(legend1,ncol=1),
    # I also set the relative widths so the legend takes up less space
    nrow=1, 
    rel_widths = c(5,1)
    )
)

prow

# Figure 4: Cases of TB generated over 1 year
#################################################
## A) without mixing, no protection from progression, increasing risk
max.count <- max(sum(output.mix.noprot$count[output.mix.noprot$prevalence==1000]))
ggplot(data=output.nomix.noprot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="Number of new cases",x="TB prevalence (per 100000)",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

## B) without mixing, protection against progression, increasing risk with infections
ggplot(data=output.nomix.prot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="Number of new cases",x="TB prevalence",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

## C) mixing, adjusted ARI, no protection against progression and increasing risk
ggplot(data=output.mix.noprot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="Number of new cases",x="TB prevalence",y="Number of cases")+
  scale_fill_brewer(name="Source of cases",palette="Paired")

## D) mixing, adjusted ARI, protection against progression and increasing risk
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
