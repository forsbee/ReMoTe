# file to generate figures for the paper #
##########################################
source("./utils.R")

library(ggpubr)
library(cowplot)
library(stringr)

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

# Figure 1: baseline conditions-infected individuals by age and prevalence
############################################################################
### infected individuals by age and prevalence
## A) homogeneous mixing
max.inf <- max(baseline.age.mix$total.infections)
colorCount <- num.age
getPalette <- colorRampPalette(brewer.pal(9,"Paired"))

fig1a <- ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  guides(fill=guide_legend(ncol=2))+
  labs(title="",x="",y="Total infected at baseline")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(axis.text.y=element_text(angle=90,hjust=0.55))+ 
  scale_x_discrete(labels=c("100","","","","500","","","","","1000"))

legend.age <- get_legend(fig1a)

## B) heterogeneous mixing, unadjusted ARI
baseline.age.mix.DoddARI <- num.inf.age
baseline.numinf.mix.DoddARI <- num.inf.mix
fig1b <- ggplot(data=baseline.age.mix.DoddARI[baseline.age.mix.DoddARI$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="",x="",y="")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(axis.text.y=element_blank())+ 
  scale_x_discrete(labels=c("100","","","","500","","","","","1000"))

## C) heterogeneous mixing, adjusted ARI
fig1c <- ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="",x="",y="")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(axis.text.y=element_blank())+ 
  scale_x_discrete(labels=c("100","","","","500","","","","","1000"))

fig1c.alt <- ggplot(data=baseline.age.mix[baseline.age.mix$age.group!=0,],aes(x=as.factor(prevalence),y=total.infections,fill=as.factor(age.group)))+
  geom_bar(stat="identity")+ylim(0,max.inf+500)+
  labs(title="",x="",y="Total infected at baseline")+
  scale_fill_manual(name="Median age",values=getPalette(colorCount))+
  theme(axis.text.y=element_text(angle=90,hjust=0.55))+ 
  scale_x_discrete(labels=c("100","","","","500","","","","","1000"))

## put these three figures in one plot ##
prow <- ggdraw(
  plot_grid(fig1a + theme(legend.position="none") ,
              fig1b +theme(legend.position="none"),
              fig1c.alt +theme(legend.position="none"),
              legend.age,
              
              align = 'vh',
              
              labels = c("a", "b","c",""),
              
              hjust = -1,
              
              nrow = 2,
              axis="1")
)

prow+draw_label("TB prevalence (per 100000)",x=0.5,y=0.02)

ggsave("../Output/Figure1.tiff", width = 5, height = 5, device='tiff', dpi=600)

### bottom row of figure 1: number of prior infections by prevalence

## D) homogeneous mixing
fig1d <- ggplot(data=baseline.numinf.nomix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  guides(fill=guide_legend(ncol=2))+
  labs(title="",x="",y="Total number of individuals")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")+
  theme(axis.text.y=element_text(angle=90))+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))

fig1d <- ggplot(data=baseline.numinf.nomix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  guides(fill=guide_legend(ncol=2))+
  labs(title="",x="",y="Total number of individuals")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")+
  theme(axis.text.y=element_text(angle=90,hjust=0.55))+ 
  scale_y_continuous(breaks=c(0,300000,600000,900000))+
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))

legend2 <- get_legend(fig1d)

## E) heterogeneous mixing, unadjusted ARI
baseline.age.mix.DoddARI <- num.inf.age
baseline.numinf.mix.DoddARI <- num.inf.mix
fig1e <- ggplot(data=baseline.numinf.mix.DoddARI,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="",x="",y="")+
  scale_fill_brewer(name="Number of\ninfections",palette="Paired")+
  theme(axis.text.y=element_blank())+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))

## F) heterogeneous mixing, adjusted ARI
fig1f <- ggplot(data=baseline.numinf.mix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="",x="",y="")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")+
  theme(axis.text.y=element_blank())+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))

fig1f.alt <- ggplot(data=baseline.numinf.mix,aes(x=prevalence,y=count,fill=num.inf))+
  geom_bar(stat="identity")+
  labs(title="",x="",y="Total number of individuals")+
  scale_fill_brewer(name="Number\nof\ninfections",palette="Paired")+
  theme(axis.text.y=element_text(angle=90,hjust=0.55))+ 
  scale_y_continuous(breaks=c(0,300000,600000,900000))+
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))

prow <- ggdraw(
  plot_grid(fig1d + theme(legend.position="none") ,
            fig1e +theme(legend.position="none"),
            fig1f.alt +theme(legend.position="none"),
            legend2,
            
            align = 'vh',
            
            labels = c("a", "b","c",""),
            
            hjust = -1,
            
            nrow = 2,
            axis="1")
)

prow+draw_label("TB prevalence (per 100000)",x=0.5,y=0.02)
ggsave("../Output/Figure2.tiff", width = 5, height = 5, device='tiff', dpi=600)


## Code to put all six figures together ##
##########################################
prow <- ggdraw(
  plot_grid(
  plot_grid(fig1a + theme(legend.position="none") ,
          fig1b +theme(legend.position="none"),
          fig1c +theme(legend.position="none"),
          fig1d +theme(legend.position="none"),
          fig1e +theme(legend.position="none"),
          fig1f +theme(legend.position="none"),
          align = 'vh',
          
          labels = c("a", "b","c","d","e","f"),
          
          hjust = -1,
          
          nrow = 2,
          axis="1"),
plot_grid(legend.age,legend2,nrow=2,ncol=1,align='vh',scale=0.7),
rel_widths = c(6,1),rel_heights = c(1,0.5))
)

prow+draw_label("TB prevalence (per 100000)",x=0.5,y=0.02)


# Figure 2: Cases of TB generated over 1 year
#################################################
max.count <- max(sum(output.mix.noprot$count[output.mix.noprot$prevalence==1000]))
## A) without mixing, protection against progression, increasing risk with infections
fig2a <- ggplot(data=output.nomix.prot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="",x="",y="")+
  scale_fill_brewer(name="Source of cases",palette="Paired")+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))+
  theme(axis.text.y=element_text(angle=90,hjust=0.55))

## B) without mixing, no protection from progression, increasing risk
fig2b <- ggplot(data=output.nomix.noprot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="",x="",y="")+
  theme(axis.text.y=element_blank())+ 
  scale_fill_brewer(name="Source of\ncases",palette="Paired")+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))

legend.src <- get_legend(fig2b)

## C) mixing, adjusted ARI, protection against progression and increasing risk
fig2c <- ggplot(data=output.mix.prot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="",x="",y="")+
  scale_fill_brewer(name="Source of cases",palette="Paired")+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))+
  theme(axis.text.y=element_text(angle=90,hjust=0.55))

## D) mixing, adjusted ARI, no protection against progression and increasing risk
fig2d <- ggplot(data=output.mix.noprot,aes(x=prevalence,y=count,fill=source))+
  geom_bar(stat="identity")+ylim(c(0,max.count))+
  labs(title="",x="",y="")+
  theme(axis.text.y=element_blank())+ 
  scale_fill_brewer(name="Source of cases",palette="Paired")+ 
  scale_x_continuous(breaks=c(100,500,1000),labels=c("100","500","1000"))


prow <- ggdraw(
  plot_grid(
    plot_grid(fig2a + theme(legend.position="none") ,
              fig2b +theme(legend.position="none"),
              fig2c +theme(legend.position="none"),
              fig2d +theme(legend.position="none"),
              align = 'vh',
              labels = c("a", "b","c","d"),
              hjust = -2,
              nrow = 2,
              axis="1"),
    legend.src,
    rel_widths = c(6,1.75),rel_heights = c(1,0.35))
)

prow+draw_label("TB prevalence (per 100000)",x=0.5,y=0.02)+draw_label("Number of cases",x=0.01,y=0.5,angle=90)

ggsave("../Output/Figure3.tiff", width = 5, height = 5, device='tiff', dpi=600)

###########################
# Supplementary figures   #
###########################
# Supp Fig 2: change in obs incidence when risk of progression varies from 0.04-0.06, 
### with and without waning protection

# Supp Fig 3: change in obs incidence when risk of progression varies from 0.04-0.06, 
### with and without waning protection
