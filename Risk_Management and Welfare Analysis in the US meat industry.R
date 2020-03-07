setwd("/Users/borishouenou/Downloads")
data=read.csv("dataw.csv")
library(ggplot2)
library(gridExtra)
library(grid)
#p <-ggplot(data, aes(Welfare.measure, BSE, fill = Scenario))

#dodge <- position_dodge(width=0.9)
#p +
#  geom_col(position = dodge) +
#  geom_errorbar(aes(ymin = L, ymax = U), position = dodge, width = 0.25)+
#  xlab("Welfare Measures") +
#  ylab("Change in Welfare ($ billion)") +
#  ggtitle("Change in Welfare for BSE Shock")+theme_bw()
#  +theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))

  
#grid<-matrix(c(1,1,2,3),nrow=2,ncol=2,byrow = TRUE)
#layout(grid)

g1 <-ggplot(data, aes(Welfare.measure, BSE,fill = Scenario))

dodge <- position_dodge(width=0.9)
g1<-g1 +
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = L, ymax = U), position = dodge, width = 0.25)+
  xlab("Welfare Measures") +
  ylab("$ Billion") +
  ggtitle("BSE Shock")+ theme_bw()+theme(plot.title = element_text(color="black", size=14, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))
g1<-g1 + scale_fill_grey() 

g2 <-ggplot(data, aes(Welfare.measure, RVF,fill = Scenario))
dodge <- position_dodge(width=0.9)
g2<-g2 +
 geom_col(position = dodge) +
  geom_errorbar(aes(ymin = L.1, ymax = U.1), position = dodge, width = 0.25)+
  xlab("Welfare Measures") +
  ylab("$ Billion") +
  ggtitle("RVF Shock") + theme_bw()+theme(plot.title = element_text(color="black", size=14, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))
g2<-g2 + scale_fill_grey()

g3 <-ggplot(data, aes(Welfare.measure, AVI,fill = Scenario))
dodge <- position_dodge(width=0.9)
g3<-g3 +
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = L.2, ymax = U.2), position = dodge, width = 0.25)+
  xlab("Welfare Measures") +
  ylab("$ Billion") +
  ggtitle("HPAI Shock") + theme_bw()+theme(plot.title = element_text(color="black", size=14, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))
g3<-g3 + scale_fill_grey() 



grid.arrange(p1,                                    # bar plot spaning two columns
             p2, p3,                               # box plot and scatter plot
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)))



setwd("/Users/borishouenou/Documents/JSearch")
data1<-read.csv("Table_ConsumerS.csv")
data2<-read.csv("PriceAndQtyChange_BeefShocks.csv")
data3<-read.csv("Changes_BeefShock.csv")
head(data1)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
str(data1)

####Consumer surplus in barplots#####
p1 <-ggplot(data1, aes(Scenario, pc_CS_Beef, fill=Shock))

dodge <- position_dodge(width=0.9)
p1 +
 scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))+
  geom_col(position = dodge) +
  #geom_errorbar(aes(ymin = L, ymax = U), position = dodge, width = 0.25)+
  xlab("Simulation Cases") +
  ylab("Change in per capita Consumer Welfare (USD)") +
  ggtitle("Change in Beef Consumer Welfare Following Beef Sector Shock") +
  lims(fill= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))+
  theme_bw()


p2 <-ggplot(data1, aes(Scenario, pc_CS_Pork, fill=Shock))

dodge <- position_dodge(width=0.9)
p2 +
  scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))+
  geom_col(position = dodge) +
  #geom_errorbar(aes(ymin = L, ymax = U), position = dodge, width = 0.25)+
  xlab("Simulation Cases") +
  ylab("Change in per capita Consumer Welfare (USD)") +
  ggtitle("Change in Pork Consumer Welfare Following Beef Sector Shock") +
  lims(fill= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))+
  theme_bw()


p3 <-ggplot(data1, aes(Scenario, pc_CS_Lamb, fill=Shock))

dodge <- position_dodge(width=0.9)
p3 +
  scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))+
  geom_col(position = dodge) +
  #geom_errorbar(aes(ymin = L, ymax = U), position = dodge, width = 0.25)+
  xlab("Simulation Cases") +
  ylab("Change in per capita Consumer Welfare (USD)") +
  ggtitle("Change in Lamb Consumer Welfare Following Beef Sector Shock") +
  lims(fill= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))+
  theme_bw()+theme(plot.title = element_text(color="black", size=10, face="bold"))

p4 <-ggplot(data1, aes(Scenario, pc_CS_Poultry, fill=Shock))

dodge <- position_dodge(width=0.9)
p4 +
  scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))+
  geom_col(position = dodge) +
  #geom_errorbar(aes(ymin = L, ymax = U), position = dodge, width = 0.25)+
  xlab("Simulation Cases") +
  ylab("Change in per Consumer Welfare (USD)") +
  ggtitle("Change in Poultry Consumer Welfare Following Beef Sector Shock") +
  lims(fill= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))+
  theme_bw()

#####Consumer welfare using line graphs######
g1<-ggplot(data1, aes(x=Scenario, y=pc_CS_Beef, group=Shock)) +
  geom_line(aes(colour=Shock), size=3)+
  geom_point()
g1<-g1+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
g1<-g1+theme_bw()
g1<-g1+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
g1<-g1+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
g1<-g1 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
g1<-g1+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))


g2<-ggplot(data1, aes(x=Scenario, y=pc_CS_Lamb, group=Shock)) +
  geom_line(aes(colour=Shock), size=3)+
  geom_point()
g2<-g2+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
g2<-g2+labs(title="Figure 2: Change in Lamb Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)" ) #Labels
g2<-g2+theme_bw()
g2<-g2+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
g2<-g2 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
g2<-g2+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))

g3<-ggplot(data1, aes(x=Scenario, y=pc_CS_Pork, group=Shock)) +
  geom_line(aes(colour=Shock), size=3)+
  geom_point()
g3<-g3+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
g3<-g3+labs(title="Figure 3: Change in Pork Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)" ) #Labels
g3<-g3+theme_bw()
g3<-g3+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
g3<-g3 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
g3<-g3+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))




g4<-ggplot(data1, aes(x=Scenario, y=pc_CS_Poultry, group=Shock)) +
  geom_line(aes(colour=Shock), size=3)+
  geom_point()
g4<-g4+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
g4<-g4+labs(title="Figure 4: Change in Poultry Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)" ) #Labels
g4<-g4+theme_bw()
g4<-g4+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
g4<-g4 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
g4<-g4+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))




h1<-ggplot(data2, aes(x=Shock, y=Price_Change_Beef  , group=Welfare_Measure)) +
  geom_line(aes(colour=Welfare_Measure), size=3)+
  geom_point()
h1<-h1+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
h1<-h1+theme_bw()
h1<-h1+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
h1<-h1+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
h1<-h1 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
h1<-h1+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))

h2<-ggplot(data3, aes(x=Shock, y=Retail  , group=Change)) +
  geom_line(aes(colour=Change), size=3)+
  geom_point()
h2<-h2+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
h2<-h2+theme_bw()
h2<-h2+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
h2<-h2+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
h2<-h2 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
h2<-h2+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))




h3<-ggplot(data3, aes(x=Shock, y=Slaughter  , group=Change)) +
  geom_line(aes(colour=Change), size=3)+
  geom_point()
h3<-h3+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
h3<-h3+theme_bw()
h3<-h3+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
h3<-h3+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
h3<-h3 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
h3<-h3+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))



h4<-ggplot(data3, aes(x=Shock, y=Export  , group=Change)) +
  geom_line(aes(colour=Change), size=3)+
  geom_point()
h4<-h4+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
h4<-h4+theme_bw()
h4<-h4+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
h4<-h4+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
h4<-h4 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
h4<-h4+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))

h5<-ggplot(data3, aes(x=Shock, y=Wholesale  , group=Change)) +
  geom_line(aes(colour=Change), size=3)+
  geom_point()
h5<-h5+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
h5<-h5+theme_bw()
h5<-h5+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
h5<-h5+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
h5<-h5 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
h5<-h5+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))



h6<-ggplot(data3, aes(x=Shock, y=Feeder  , group=Change)) +
  geom_line(aes(colour=Change), size=3)+
  geom_point()
h6<-h6+ geom_hline(yintercept=0, linetype="dashed", color = "blue")
h6<-h6+theme_bw()
h6<-h6+labs(title="Figure 1: Change in Beef Consumer Welfare Following Beef Sector Shock",x="Simulation Cases", y="Change in per capita Consumer Welfare (in USD)") 
h6<-h6+lims(colour= c("Farm_Supply", "Retail_Demand_Contraction", "Trade_Ban"))
h6<-h6 + scale_x_discrete(limits=c("Point_Estimate_Baseline", "NIP", "SO", "MO", "LO"))
h6<-h6+theme(plot.title = element_text(color="black", size=20, face="bold"), axis.title = element_text(color="black", size = 14, face="bold"), legend.text = element_text(color="black", size = 14, face="bold"), legend.title = element_text(color="black", size = 12, face="bold"))
####################################################Times series results#######
setwd("/Users/borishouenou/Documents/JSearch")# Loading data
tb<-read.csv("Tb_shock_beef.csv")
rd<-read.csv("Rd_shock_beef.csv")
fs<-read.csv("Fs_shock_beef.csv")
tbp<-subset(tb, SEGMENT!="pc_CS")
rdp<-subset(rd,SEGMENT!="pc_CS" )
fsp<-subset(fs,SEGMENT!="pc_CS" )
tbc<-subset(tb, SEGMENT=="pc_CS")
rdc<-subset(rd,SEGMENT=="pc_CS" )
fsc<-subset(fs,SEGMENT=="pc_CS" )
tbc$Shock<-"Trade_Ban"
rdc$Shock<-"Retail_Demand_Shock"
fsc$Shock<-"Farm_Supply_Shock"
dc<-rbind(tbc, rdc, fsc)
tbp$BEEF<-tbp$BEEF/1000000 # data manipulation, rescaling figures in millions
tbp$PORK<-tbp$PORK/1000000
tbp$LAMB<-tbp$LAMB/1000000
tbp$POULTRY<-tbp$POULTRY/1000000
rdp$BEEF<-rdp$BEEF/1000000 # data manipulation, rescaling figures in millions
rdp$PORK<-rdp$PORK/1000000
rdp$LAMB<-rdp$LAMB/1000000
rdp$POULTRY<-rdp$POULTRY/1000000
fsp$BEEF<-fsp$BEEF/1000000 # data manipulation, rescaling figures in millions
fsp$PORK<-fsp$PORK/1000000
fsp$LAMB<-fsp$LAMB/1000000
fsp$POULTRY<-fsp$POULTRY/1000000
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(plotly)
library(reshape2) # for melt


######Producer Surplus following beef sector shock######
b1<-qplot(YEAR, BEEF, data = tbp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
b1<-b1+labs(title="Producer Surplus (Beef)",x="Year", y="$ Million") 
b1
b2<-qplot(YEAR, BEEF, data = rdp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
b2<-b2+labs(title="Producer Surplus (Beef)",x="Year", y="$ Million") 
b2
b3<-qplot(YEAR, BEEF, data = fsp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
b3<-b3+labs(title="Producer Surplus (Beef)",x="Year", y="$ Million") 
b3

p1<-qplot(YEAR, PORK, data = tbp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
p1<-p1+labs(title="Producer Surplus (Pork)",x="Year", y="$ Million") 
p1
p2<-qplot(YEAR, PORK, data = rdp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
p2<-p2+labs(title="Producer Surplus (Pork)",x="Year", y="$ Million") 
p2
p3<-qplot(YEAR, PORK, data = fsp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
p3<-p3+labs(title="Producer Surplus (Pork)",x="Year", y="$ Million") 
p3

l1<-qplot(YEAR, LAMB, data = tbp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
l1<-l1+labs(title="Producer Surplus (Lamb)",x="Year", y="$ Million") 
l1
l2<-qplot(YEAR, LAMB, data = rdp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
l2<-l2+labs(title="Producer Surplus (Lamb)",x="Year", y="$ Million") 
l2
l3<-qplot(YEAR, LAMB, data = fsp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
l3<-l3+labs(title="Producer Surplus (Lamb)",x="Year", y="$ Million") 
l3

po1<-qplot(YEAR, POULTRY, data = tbp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
po1<-po1+labs(title="Producer Surplus (Poultry)",x="Year", y="$ Million") 
po1
po2<-qplot(YEAR, POULTRY, data = rdp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
po2<-po2+labs(title="Producer Surplus (Poultry)",x="Year", y="$ Million") 
po2
po3<-qplot(YEAR, POULTRY, data = fsp, geom = "line", linetype = SCENARIO) +
  facet_grid(SEGMENT ~ ., scale = "free_y") +
  theme_bw()
po3<-po3+labs(title="Producer Surplus (Poultry)",x="Year", y="$ Million") 
po3

############Consumer surplus per shock and species#############
c1<-qplot(YEAR, BEEF, data = dc, geom = "line", linetype = SCENARIO) +
  facet_grid(Shock ~ ., scale = "free_y") +
  theme_bw()
c1<-c1+labs(title="Consumer Surplus (Beef)",x="Year", y="$") 
c1

c2<-qplot(YEAR, PORK, data = dc, geom = "line", linetype = SCENARIO) +
  facet_grid(Shock ~ ., scale = "free_y") +
  theme_bw()
c2<-c2+labs(title="Consumer Surplus (Pork)",x="Year", y="$") 
c2

c3<-qplot(YEAR, LAMB, data = dc, geom = "line", linetype = SCENARIO) +
  facet_grid(Shock ~ ., scale = "free_y") +
  theme_bw()
c3<-c3+labs(title="Consumer Surplus (Lamb)",x="Year", y="$") 
c3

c4<-qplot(YEAR, POULTRY, data = dc, geom = "line", linetype = SCENARIO) +
  facet_grid(Shock ~ ., scale = "free_y") +
  theme_bw()
c4<-c4+labs(title=" Consumer Surplus (Poultry) ",x="Year", y="$") 
c4
ggplotly()
#grid.arrange(c1, c2, nrow = 1)
#ggarrange(c1, c2, widths = 1:2)
#####Arranging the plots in grids####
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }

grid_arrange_shared_legend(c1, c2)
grid_arrange_shared_legend(c3, c4)
#grid_arrange_shared_legend(c1, c2, c3, c4, ncol=2, nrow = 2)
grid_arrange_shared_legend(b1, p1, ncol=2, nrow = 1)
grid_arrange_shared_legend(l1, po1, ncol=2, nrow = 1)

grid_arrange_shared_legend(b2, p2, ncol=2, nrow = 1)
grid_arrange_shared_legend(l2, po2, ncol=2, nrow = 1)

grid_arrange_shared_legend(b3, p3, ncol=2, nrow = 1)
grid_arrange_shared_legend(l3, po3, ncol=2, nrow = 1)

grid_arrange_shared_legend(g1, g2,g3, ncol=1, nrow = 3)

##########Scenarios Analysis-Time series graphs#####
setwd("/Users/borishouenou/Documents/JSearch")# Loading data
Ag<-read.csv("Scenario-results-aggregated.csv")
Disag<-read.csv("Scenario_results_disaggregated.csv")
head(Ag)
head(Disag)
Disagp<-Disag 
#Disagp<-subset(Disag, SEGMENT!="PER_CAPITA_CS")
Disagp$BEEF<-Disagp$BEEF/1000000 # data manipulation, rescaling figures in millions
Disagp$PORK<-Disagp$PORK/1000000
Disagp$LAMB<-Disagp$LAMB/1000000 # data manipulation, rescaling figures in millions
Disagp$POULTRY<-Disagp$POULTRY/1000000
DisagpRetail<-subset(Disagp, SEGMENT=="RETAIL")
DisagpWholesale<-subset(Disagp, SEGMENT=="WHOLESALE")
DisagpExports<-subset(Disagp, SEGMENT=="EXPORTS")
DisagpFeeder<-subset(Disagp, SEGMENT=="FEEDER")
DisagpSlaughter<-subset(Disagp, SEGMENT=="SLAUGHTER")
DisagpPcConsumer<-subset(Disagp, SEGMENT=="PER CAPITA CONSUMER SURPLUS")
head(DisagpPcConsumer)
Ag$Total_CS<-Ag$TOTAL_CS/1000000
Ag$Total_PS<-Ag$INDUSTRY/1000000
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(plotly)
library(reshape2) # for melt
D1<-qplot(YEAR, BEEF, data =DisagpRetail ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D1<-D1+labs(title="Producer Surplus (Beef_retail)",x="Year", y="$ Million") 
D1

D2<-qplot(YEAR, PORK, data =DisagpRetail ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D2<-D2+labs(title="Producer Surplus (Pork_retail)",x="Year", y="$ Million") 
D2

D3<-qplot(YEAR, LAMB, data =DisagpRetail ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D3<-D3+labs(title="Producer Surplus (Lamb_retail)",x="Year", y="$ Million") 
D3

D4<-qplot(YEAR, POULTRY, data =DisagpRetail ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D4<-D4+labs(title="Producer Surplus (Poultry_retail)",x="Year", y="$ Million") 
D4


D5<-qplot(YEAR, BEEF, data =DisagpWholesale ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D5<-D5+labs(title="Producer Surplus (Beef_wholesale)",x="Year", y="$ Million") 
D5

D6<-qplot(YEAR, PORK, data =DisagpWholesale ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D6<-D6+labs(title="Producer Surplus (Pork_wholesale)",x="Year", y="$ Million") 
D6

D7<-qplot(YEAR, LAMB, data =DisagpWholesale ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D7<-D7+labs(title="Producer Surplus (Lamb_wholesale)",x="Year", y="$ Million") 
D7

D8<-qplot(YEAR, POULTRY, data =DisagpWholesale ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D8<-D8+labs(title="Producer Surplus (Poultry_wholesale)",x="Year", y="$ Million") 
D8


D9<-qplot(YEAR, POULTRY, data =DisagpExports ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D9<-D9+labs(title="Producer Surplus (Poultry_Exports)",x="Year", y="$ Million") 
D9

D10<-qplot(YEAR, PORK, data =DisagpExports ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D10<-D10+labs(title="Producer Surplus (Pork_Exports)",x="Year", y="$ Million") 
D10

D11<-qplot(YEAR, LAMB, data =DisagpExports ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D11<-D11+labs(title="Producer Surplus (Lamb_Exports)",x="Year", y="$ Million") 
D11

D12<-qplot(YEAR, BEEF, data =DisagpExports ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D12<-D12+labs(title="Producer Surplus (Beef_Exports)",x="Year", y="$ Million") 
D12


D13<-qplot(YEAR, BEEF, data =DisagpFeeder ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D13<-D13+labs(title="Producer Surplus (Beef_Feeder)",x="Year", y="$ Million") 
D13

D14<-qplot(YEAR, PORK, data =DisagpFeeder ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D14<-D14+labs(title="Producer Surplus (Pork_Feeder)",x="Year", y="$ Million") 
D14

D15<-qplot(YEAR, LAMB, data =DisagpFeeder ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D15<-D15+labs(title="Producer Surplus (Lamb_Feeder)",x="Year", y="$ Million") 
D15

D16<-qplot(YEAR, POULTRY, data =DisagpFeeder ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D16<-D16+labs(title="Producer Surplus (Poultry_Feeder)",x="Year", y="$ Million") 
D16


D17<-qplot(YEAR, POULTRY, data =DisagpPcConsumer ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D17<-D17+labs(title="Per Capita Consumer Surplus (Poultry)",x="Year", y="$") 
D17


D18<-qplot(YEAR, LAMB, data =DisagpPcConsumer ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D18<-D18+labs(title="Per Capita Consumer Surplus (Lamb)",x="Year", y="$") 
D18

D19<-qplot(YEAR, PORK, data =DisagpPcConsumer ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D19<-D19+labs(title="Per Capita Consumer Surplus (Pork)",x="Year", y="$") 
D19


D20<-qplot(YEAR, BEEF, data =DisagpPcConsumer ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D20<-D20+labs(title="Per Capita Consumer Surplus (Beef)",x="Year", y="$") 
D20

grid_arrange_shared_legend(D20,D19, D18, D17,ncol=4, nrow = 1)
grid_arrange_shared_legend(D1, D2, D3, D4,ncol=2, nrow = 2)
grid_arrange_shared_legend(D5, D6, D7, D8,ncol=2, nrow = 2)
grid_arrange_shared_legend(D12, D10, D11, D9,ncol=2, nrow = 2)
grid_arrange_shared_legend(D13, D14, D15, D16,ncol=2, nrow = 2)


D21<-qplot(YEAR, PER_CAPITA_CS, data =Ag ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D21<-D21+labs(title="Per Capita Consumer Surplus",x="Year", y="$ Million") 
D21

D22<-qplot(YEAR, Total_PS, data =Ag ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D22<-D22+labs(title=" Total Producer Surplus",x="Year", y="$ Million") 
D22

D23<-qplot(YEAR, Total_CS, data =Ag ,geom = "line", linetype = SCENARIO) +
  facet_grid(DISEASE ~ ., scale = "free_y") +
  theme_bw()
D23<-D23+labs(title=" Total Consumer Surplus",x="Year", y="$ Million") 
D23


grid_arrange_shared_legend(D21,D23, D22,ncol=3, nrow = 1)
