setwd("F:/blazar/papers/carbon inequality/20230221/data")
library(gsynth)
library(haven)
library(tidyverse)
library(panelView)
library(cowplot)
library(ggplot2)

raw_data<- read_dta("panel_data.dta")
raw_data <- filter(raw_data, iso3 != "DJI")
panel_data <- filter(raw_data, sptinc992j1 != "NA")
panel_data <- filter(panel_data, trade != "NA")
panel_data <- filter(panel_data, gdppg != "NA")
panel_data <- filter(panel_data, lnenfghg999i != "NA")
panel_data <- filter(panel_data, lnknfghg999i != "NA")

##################################################################################
##Supplementary Figure 1: Carbon inequality of 30 countries over 1990-2019
figdata <- filter(raw_data, (regionpolitical=="European Union")|(iso3=="ISL")|(iso3=="NOR"))
panelview(lnsghg1 ~ euets3, data = figdata, index = c("iso3","year"),xlab= "EU ETS phase 3",
          ylab = "The share of carbon footprint for the top 1% (in Logs)", type = "bivar",
          by.unit = TRUE,  show.id = c(1:30))

##################################################################################
##########GSC. Main results
##The share of carbon footprint for the top 1%
lnsghg1 <- gsynth(lnsghg1 ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data, 
                    index = c("iso3","year"), force = "two-way",
                    CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                    inference = "parametric", nboots = 1000,seed = 2000)
print(lnsghg1)
lnsghg1$est.att
lnsghg1$est.avg

#fig1a. ATT

fig1a<-plot(lnsghg1, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
      ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
      theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))


#fig2a. Counterfactual
fig2a<-plot(lnsghg1, type = "counterfactual", raw = "band", main="",theme.bw = TRUE,
     xlab = "Year", ylab = "The share of carbon footprint\n for the top 1% (in Logs)")+
     scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
     theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
      theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

#fig3. Counterfactual. DEU, GBR, FRA, ITA 
fig3a<-plot(lnsghg1, type = "counterfactual", id = "DEU", main = "",
     xlab = "Year", ylab = "The share of carbon footprint\n for the top 1% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))
fig3b<-plot(lnsghg1, type = "counterfactual", id = "GBR", main = "",
     xlab = "Year", ylab = "The share of carbon footprint\n for the top 1% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))
fig3c<-plot(lnsghg1, type = "counterfactual", id = "FRA", main = "",
     xlab = "Year", ylab = "The share of carbon footprint\n for the top 1% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))
fig3d<-plot(lnsghg1, type = "counterfactual", id = "ITA", main = "",
     xlab = "Year", ylab = "The share of carbon footprint\n for the top 1% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

###fig.3

plot_grid(
  fig3a, fig3b, fig3c, fig3d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

##The share of carbon footprint for the top 10%
lnsghg10 <- gsynth(lnsghg10 ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  inference = "parametric", nboots = 1000,seed = 2000)
print(lnsghg10)
lnsghg10$est.att
lnsghg10$est.avg

#fig1b. ATT
fig1b<-plot(lnsghg10, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

#fig2b. Counterfactual
fig2b<-plot(lnsghg10, type = "counterfactual", raw = "band", main="",theme.bw = TRUE,
            xlab = "Year", ylab = "The share of carbon footprint\n for the top 10% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

#sup_fig3. Counterfactual. DEU, GBR, FRA, ITA 
sup_fig3a<-plot(lnsghg10, type = "counterfactual", id = "DEU", main = "",
            xlab = "Year", ylab = "The share of carbon footprint\n for the top 10% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig3b<-plot(lnsghg10, type = "counterfactual", id = "GBR", main = "",
            xlab = "Year", ylab = "The share of carbon footprint\n for the top 10% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig3c<-plot(lnsghg10, type = "counterfactual", id = "FRA", main = "",
            xlab = "Year", ylab = "The share of carbon footprint\n for the top 10% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig3d<-plot(lnsghg10, type = "counterfactual", id = "ITA", main = "",
            xlab = "Year", ylab = "The share of carbon footprint\n for the top 10% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))

###sup_fig.3

plot_grid(
  sup_fig3a, sup_fig3b, sup_fig3c, sup_fig3d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

##The share of carbon footprint for the middle 40%
lnsghg40 <- gsynth(lnsghg40 ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   inference = "parametric", nboots = 1000,seed = 2000)
print(lnsghg40)
lnsghg40$est.att
lnsghg40$est.avg

#fig1c. ATT
fig1c<-plot(lnsghg40, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
  

#fig2c. Counterfactual
fig2c<-plot(lnsghg40, type = "counterfactual", raw = "band", main="",theme.bw = TRUE,
            xlab = "Year", ylab = "The share of carbon footprint\n for the middle 40% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

#sup_fig4. Counterfactual. DEU, GBR, FRA, ITA 
sup_fig4a<-plot(lnsghg40, type = "counterfactual", id = "DEU", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the middle 40% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig4b<-plot(lnsghg40, type = "counterfactual", id = "GBR", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the middle 40% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig4c<-plot(lnsghg40, type = "counterfactual", id = "FRA", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the middle 40% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig4d<-plot(lnsghg40, type = "counterfactual", id = "ITA", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the middle 40% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))

###sup_fig.4

plot_grid(
  sup_fig4a, sup_fig4b, sup_fig4c, sup_fig4d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

##The share of carbon footprint for the bottom 50%
lnsghg50 <- gsynth(lnsghg50 ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   inference = "parametric", nboots = 1000,seed = 2000)
print(lnsghg50)
lnsghg50$est.att
lnsghg50$est.avg

#fig1d. ATT
fig1d<-plot(lnsghg50, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))

#fig2d. Counterfactual
fig2d<-plot(lnsghg50, type = "counterfactual", raw = "band", main="",theme.bw = TRUE,
            xlab = "Year", ylab = "The share of carbon footprint\n for the bottom 50% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

#sup_fig5. Counterfactual. DEU, GBR, FRA, ITA 
sup_fig5a<-plot(lnsghg50, type = "counterfactual", id = "DEU", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the bottom 50% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig5b<-plot(lnsghg50, type = "counterfactual", id = "GBR", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the bottom 50% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig5c<-plot(lnsghg50, type = "counterfactual", id = "FRA", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the bottom 50% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))
sup_fig5d<-plot(lnsghg50, type = "counterfactual", id = "ITA", main = "",
                xlab = "Year", ylab = "The share of carbon footprint\n for the bottom 50% (in Logs)")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 12))

###sup_fig.5

plot_grid(
  sup_fig5a, sup_fig5b, sup_fig5c, sup_fig5d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

###fig.1

plot_grid(
  fig1a, fig1b, fig1c, fig1d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

###fig.2

plot_grid(
  fig2a, fig2b, fig2c, fig2d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

##The ratio of per capita carbon footprint of the top 1% to bottom 50% 
lnlpghg150 <- gsynth(lnlpghg150 ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   inference = "parametric", nboots = 1000,seed = 2000)
print(lnlpghg150)
lnlpghg150$est.att
lnlpghg150$est.avg

#fig. ATT
sup_fig2a<-plot(lnlpghg150, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))

#fig. Counterfactual
sup_fig2c<-plot(lnlpghg150, type = "counterfactual", raw = "band", main="",theme.bw = TRUE,
            xlab = "Year", ylab = "The ratio of per capita carbon footprint of\n the top 1% to bottom 50%")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

#fig. Counterfactual. DEU, GBR, FRA, ITA 
plot(lnlpghg150, type = "counterfactual", id = "DEU",theme.bw = FALSE, main = "",
     xlab = "(a) DEU", ylab = "The ratio of per capita top 1% carbon footprint to bottom 50% (in Logs)")
plot(lnlpghg150, type = "counterfactual", id = "GBR",theme.bw = FALSE, main = "",
     xlab = "(b) GBR", ylab = "The ratio of per capita top 1% carbon footprint to bottom 50% (in Logs)")
plot(lnlpghg150, type = "counterfactual", id = "FRA",theme.bw = FALSE, main = "",
     xlab = "(c) FRA", ylab = "The ratio of per capita top 1% carbon footprint to bottom 50% (in Logs)")
plot(lnlpghg150, type = "counterfactual", id = "ITA",theme.bw = FALSE, main = "",
     xlab = "(d) ITA", ylab = "The ratio of per capita top 1% carbon footprint to bottom 50% (in Logs)")

##The ratio of per capita carbon footprint of the top 10% to bottom 50%
lnlpghg1050 <- gsynth(lnlpghg1050 ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data, 
                     index = c("iso3","year"), force = "two-way", 
                     CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                     inference = "parametric", nboots = 1000,seed = 2000)
print(lnlpghg1050)
lnlpghg1050$est.att
lnlpghg1050$est.avg

#fig. ATT
sup_fig2b<-plot(lnlpghg1050, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))

#fig. Counterfactual
sup_fig2d<-plot(lnlpghg1050, type = "counterfactual", raw = "band", main="",theme.bw = TRUE,
                xlab = "Year", ylab = "The ratio of per capita carbon footprint of\n the top 10% to bottom 50%")+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 10))+
  theme(axis.text.x=element_text(hjust=0.5))+theme(legend.text =element_text(size = 10))

###sup_fig.2

plot_grid(
  sup_fig2a, sup_fig2b, sup_fig2c, sup_fig2d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

#fig. Counterfactual. DEU, GBR, FRA, ITA 
plot(lnlpghg1050, type = "counterfactual", id = "DEU",theme.bw = FALSE, main = "",
     xlab = "(a) DEU", ylab = "The ratio of per capita top 10% carbon footprint to bottom 50% (in Logs)")
plot(lnlpghg1050, type = "counterfactual", id = "GBR",theme.bw = FALSE, main = "",
     xlab = "(b) GBR", ylab = "The ratio of per capita top 10% carbon footprint to bottom 50% (in Logs)")
plot(lnlpghg1050, type = "counterfactual", id = "FRA",theme.bw = FALSE, main = "",
     xlab = "(c) FRA", ylab = "The ratio of per capita top 10% carbon footprint to bottom 50% (in Logs)")
plot(lnlpghg1050, type = "counterfactual", id = "ITA",theme.bw = FALSE, main = "",
     xlab = "(d) ITA", ylab = "The ratio of per capita top 10% carbon footprint to bottom 50% (in Logs)")



################################################################################
##The impact of EU ETS at different stages
##fig4
phase2_1 <- gsynth(lnsghg1 ~ euets2 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   inference = "parametric", nboots = 1000,seed = 2000)
print(phase2_1)
phase2_1$est.att
phase2_1$est.avg
phase2_1$est.beta
fig4a<-plot(phase2_1, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))


phase2_10<- gsynth(lnsghg10 ~ euets2 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                       index = c("iso3","year"), force = "two-way", 
                       CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                       inference = "parametric", nboots = 1000,seed = 2000)
print(phase2_10)
phase2_10$est.att
phase2_10$est.avg
phase2_10$est.beta
fig4b<-plot(phase2_10, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

phase2_40 <- gsynth(lnsghg40 ~ euets2 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                    index = c("iso3","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                    inference = "parametric", nboots = 1000,seed = 2000)
print(phase2_40)
phase2_40$est.att
phase2_40$est.avg
phase2_40$est.beta
fig4c<-plot(phase2_40, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

phase2_50 <- gsynth(lnsghg50 ~ euets2 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                    index = c("iso3","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                    inference = "parametric", nboots = 1000,seed = 2000)
print(phase2_50)
phase2_50$est.att
phase2_50$est.avg
phase2_50$est.beta
fig4d<-plot(phase2_50, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

###fig4

plot_grid(
  fig4a, fig4b, fig4c, fig4d, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")


################################################################################
##The ratio of per capita top 1% carbon footprint to bottom 50%
OTlnlpghg150 <- gsynth(lnlpghg150 ~ euets2  + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                       index = c("iso3","year"), force = "two-way", 
                       CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                       inference = "parametric", nboots = 1000,seed = 2000)
sup_fig6a<-plot(OTlnlpghg150, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

##The ratio of per capita top 10% carbon footprint to bottom 50%
OTlnlpghg1050 <- gsynth(lnlpghg1050 ~ euets2  + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                        index = c("iso3","year"), force = "two-way", 
                        CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                        inference = "parametric", nboots = 1000,seed = 2000)
sup_fig6b<-plot(OTlnlpghg1050, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
###sup_fig6

plot_grid(
  sup_fig6a, sup_fig6b, labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

################################################################################
################################################################################
raw_data2<- read_dta("panel_euets3.dta")
raw_data2 <- filter(raw_data2, iso3 != "DJI")
panel_data2 <- filter(raw_data2, sptinc992j1 != "NA")
panel_data2 <- filter(panel_data2, trade != "NA")
panel_data2 <- filter(panel_data2, gdppg != "NA")
##5a
lnccm <- gsynth(lnccm ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                 index = c("iso3","year"), force = "two-way", 
                 CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                 min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnccm)
fig5a<-plot(lnccm, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5b
lnbuild <- gsynth(lnbuild ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnbuild)
fig5b<-plot(lnbuild, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5c
lnghg <- gsynth(lnghg ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnghg)
fig5c<-plot(lnghg, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5d
lnccmict <- gsynth(lnccmict ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnccmict)
fig5d<-plot(lnccmict, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5e
lnene <- gsynth(lnene ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnene)
fig5e<-plot(lnene, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5f
lngoods <- gsynth(lngoods ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lngoods)
fig5f<-plot(lngoods, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5g
lntra <- gsynth(lntra ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lntra)
fig5g<-plot(lntra, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##5h
lnwat <- gsynth(lnwat ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnwat)
fig5h<-plot(lnwat, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

plot_grid(
  fig5a, fig5b, fig5c, fig5d, fig5e, fig5f, fig5g, fig5h,labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

###sup_fig7

lnccm <- gsynth(lnccm ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnccm)
sup_fig7a<-plot(lnccm, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnbuild <- gsynth(lnbuild ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnbuild)
sup_fig7b<-plot(lnbuild, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnghg <- gsynth(lnghg ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnghg)
sup_fig7c<-plot(lnghg, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnccmict <- gsynth(lnccmict ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnccmict)
sup_fig7d<-plot(lnccmict, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnene <- gsynth(lnene ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnene)
sup_fig7e<-plot(lnene, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lngoods <- gsynth(lngoods ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lngoods)
sup_fig7f<-plot(lngoods, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lntra <- gsynth(lntra ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lntra)
sup_fig7g<-plot(lntra, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnwat <- gsynth(lnwat ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnwat)
sup_fig7h<-plot(lnwat, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

plot_grid(
  sup_fig7a, sup_fig7b, sup_fig7c, sup_fig7d, sup_fig7e, sup_fig7f, sup_fig7g, sup_fig7h,labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

##6a
lnh_ccm <- gsynth(lnh_ccm ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ccm)
fig6a<-plot(lnh_ccm, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6b
lnh_build <- gsynth(lnh_build ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_build)
fig6b<-plot(lnh_build, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6c
lnh_ghg <- gsynth(lnh_ghg ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ghg)
fig6c<-plot(lnh_ghg, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6d
lnh_ccmict <- gsynth(lnh_ccmict ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ccmict)
fig6d<-plot(lnh_ccmict, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6e
lnh_ene <- gsynth(lnh_ene ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ene)
fig6e<-plot(lnh_ene, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6f
lnh_goods <- gsynth(lnh_goods ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_goods)
fig6f<-plot(lnh_goods, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6g
lnh_tra <- gsynth(lnh_tra ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_tra)
fig6g<-plot(lnh_tra, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))
##6h
lnh_wat <- gsynth(lnh_wat ~ euets2  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_wat)
fig6h<-plot(lnh_wat, type = "gap",main = "",xlab = "Year relative to EU ETS phase 2", 
            ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

plot_grid(
  fig6a, fig6b, fig6c, fig6d, fig6e, fig6f, fig6g, fig6h,labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")

###sup_fig8

lnh_ccm <- gsynth(lnh_ccm ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ccm)
sup_fig8a<-plot(lnh_ccm, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_build <- gsynth(lnh_build ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_build)
sup_fig8b<-plot(lnh_build, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_ghg <- gsynth(lnh_ghg ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ghg)
sup_fig8c<-plot(lnh_ghg, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_ccmict <- gsynth(lnh_ccmict ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ccmict)
sup_fig8d<-plot(lnh_ccmict, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_ene <- gsynth(lnh_ene ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_ene)
sup_fig8e<-plot(lnh_ene, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_goods <- gsynth(lnh_goods ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_goods)
sup_fig8f<-plot(lnh_goods, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_tra <- gsynth(lnh_tra ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_tra)
sup_fig8g<-plot(lnh_tra, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

lnh_wat <- gsynth(lnh_wat ~ euets3  + sptinc992j1 + trade + gdppg , data = panel_data2, 
                index = c("iso3","year"), force = "two-way", 
                CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(lnh_wat)
sup_fig8h<-plot(lnh_wat, type = "gap",main = "",xlab = "Year relative to EU ETS phase 3", 
                ylab = "Treatment effect")+scale_x_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10))+
  theme(axis.title =element_text(size = 12))+theme(axis.text = element_text(size = 12))

plot_grid(
  sup_fig8a, sup_fig8b, sup_fig8c, sup_fig8d, sup_fig8e, sup_fig8f, sup_fig8g, sup_fig8h,labels = "auto", 
  rel_widths = c(0.5, 0.5), 
  align = "h")