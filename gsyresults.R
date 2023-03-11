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

##########GSC. Impact of other climate actions
##The share of carbon footprint for the top 1%
OTlnsghg1 <- gsynth(lnsghg1 ~ euets3 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                  index = c("iso3","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                  inference = "parametric", nboots = 1000,seed = 2000)
print(OTlnsghg1)
OTlnsghg1$est.att
OTlnsghg1$est.avg
OTlnsghg1$est.beta

##The share of carbon footprint for the top 10%
OTlnsghg10 <- gsynth(lnsghg10 ~ euets3 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                    index = c("iso3","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                    inference = "parametric", nboots = 1000,seed = 2000)
print(OTlnsghg10)
OTlnsghg10$est.att
OTlnsghg10$est.avg
OTlnsghg10$est.beta

##The share of carbon footprint for the middle 40%
OTlnsghg40 <- gsynth(lnsghg40 ~ euets3 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                     index = c("iso3","year"), force = "two-way", 
                     CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                     inference = "parametric", nboots = 1000,seed = 2000)
print(OTlnsghg40)
OTlnsghg40$est.att
OTlnsghg40$est.avg
OTlnsghg40$est.beta

##The share of carbon footprint for the bottom 50%
OTlnsghg50 <- gsynth(lnsghg50 ~ euets3 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                     index = c("iso3","year"), force = "two-way", 
                     CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                     inference = "parametric", nboots = 1000,seed = 2000)
print(OTlnsghg50)
OTlnsghg50$est.att
OTlnsghg50$est.avg
OTlnsghg50$est.beta



##The ratio of per capita top 1% carbon footprint to bottom 50%
OTlnlpghg150 <- gsynth(lnlpghg150 ~ euets3  + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                        index = c("iso3","year"), force = "two-way", 
                        CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                        inference = "parametric", nboots = 1000,seed = 2000)
print(OTlnlpghg150)
OTlnlpghg150$est.att
OTlnlpghg150$est.avg
OTlnlpghg150$est.beta

##The ratio of per capita top 10% carbon footprint to bottom 50%
OTlnlpghg1050 <- gsynth(lnlpghg1050 ~ euets3  + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                        index = c("iso3","year"), force = "two-way", 
                        CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                        inference = "parametric", nboots = 1000,seed = 2000)
print(OTlnlpghg1050)
OTlnlpghg1050$est.att
OTlnlpghg1050$est.avg
OTlnlpghg1050$est.beta

################################################################################
##robustness test
##EU ETS phase 1 and 2
phase1 <- gsynth(lnsghg1 ~ euets1 + sptinc992j1 + trade + gdppg , data = panel_data, 
                 index = c("iso3","year"), force = "two-way", 
                 CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                 min.T0 = 8,inference = "parametric", nboots = 1000,seed = 2000)
print(phase1)
phase1$est.att
phase1$est.avg
phase1$est.beta
OTphase1 <- gsynth(lnsghg1 ~ euets1 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                    index = c("iso3","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   min.T0 = 8, inference = "parametric", nboots = 1000,seed = 2000)
print(OTphase1)
OTphase1$est.att
OTphase1$est.avg
OTphase1$est.beta

phase2 <- gsynth(lnsghg1 ~ euets2 + sptinc992j1 + trade + gdppg , data = panel_data, 
                 index = c("iso3","year"), force = "two-way", 
                 CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                 inference = "parametric", nboots = 1000,seed = 2000)
print(phase2)
phase2$est.att
phase2$est.avg
phase2$est.beta
OTphase2 <- gsynth(lnsghg1 ~ euets2 + etspolicy + carbonpolicy + sptinc992j1 + trade + gdppg , data = panel_data, 
                   index = c("iso3","year"), force = "two-way", 
                   CV = TRUE, r = c(0, 6), se = TRUE, parallel = TRUE,
                   inference = "parametric", nboots = 1000,seed = 2000)
print(OTphase2)
OTphase2$est.att
OTphase2$est.avg
OTphase2$est.beta
