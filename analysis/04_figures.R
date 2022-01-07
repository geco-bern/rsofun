# This script plots the outputs from the model simulations

# load packages

# Read model outputs

# DBH Mortality gs-leuning 
ea1sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp1gl_out_annual_tile.csv")
ea1sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp1gl_out_annual_cohorts.csv")
ea1sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp2gl_out_annual_tile.csv")
ea1sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp2gl_out_annual_cohorts.csv")
ea1sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp3gl_out_annual_tile.csv")
ea1sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp3gl_out_annual_cohorts.csv")

ea2sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp1gl_out_annual_tile.csv")
ea2sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp1gl_out_annual_cohorts.csv")
ea2sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp2gl_out_annual_tile.csv")
ea2sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp2gl_out_annual_cohorts.csv")
ea2sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp3gl_out_annual_tile.csv")
ea2sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp3gl_out_annual_cohorts.csv")

ea3sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp1gl_out_annual_tile.csv")
ea3sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp1gl_out_annual_cohorts.csv")
ea3sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp2gl_out_annual_tile.csv")
ea3sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp2gl_out_annual_cohorts.csv")
ea3sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp3gl_out_annual_tile.csv")
ea3sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp3gl_out_annual_cohorts.csv")

ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, color='+30'),alpha=.7)

# GR Mortality gs-leuning 
ea1sa1GRp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp1gl_out_annual_tile.csv")
ea1sa1GRp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp1gl_out_annual_cohorts.csv")
ea1sa1GRp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp2gl_out_annual_tile.csv")
ea1sa1GRp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp2gl_out_annual_cohorts.csv")
ea1sa1GRp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp3gl_out_annual_tile.csv")
ea1sa1GRp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp3gl_out_annual_cohorts.csv")

ea2sa1GRp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp1gl_out_annual_tile.csv")
ea2sa1GRp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp1gl_out_annual_cohorts.csv")
ea2sa1GRp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp2gl_out_annual_tile.csv")
ea2sa1GRp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp2gl_out_annual_cohorts.csv")
ea2sa1GRp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp3gl_out_annual_tile.csv")
ea2sa1GRp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp3gl_out_annual_cohorts.csv")

ea3sa1GRp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp1gl_out_annual_tile.csv")
ea3sa1GRp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp1gl_out_annual_cohorts.csv")
ea3sa1GRp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp2gl_out_annual_tile.csv")
ea3sa1GRp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp2gl_out_annual_cohorts.csv")
ea3sa1GRp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp3gl_out_annual_tile.csv")
ea3sa1GRp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp3gl_out_annual_cohorts.csv")

ggplot() + 
  geom_line(data=ea1sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, color='+30'),alpha=.7)

# Exploring relationships and plotting

# a) Mortality formulations

# DBH
scaleFUN <- function(x) sprintf("%.2f", x)

fig1a_dbh <- ggplot(data.frame(x = c(0, 1.06)), aes(x)) + 
  stat_function(fun = ~ 0.12*.x ^ 1.5, col="#009E73") +  
  stat_function(fun = ~ 0.12*.x ^ 2.0, col="#0072B2") + 
  stat_function(fun = ~ 0.12*.x ^ 2.7, col="#D55E00") + 
  labs(x='DBH', y='m') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1.1), breaks=seq(0,1.1,0.25)) +
  scale_y_continuous(limits=c(0,0.13), breaks=seq(0,0.10,0.05)) #+
#annotate("text", x = 1, y = 0.03, label = expression(paste("y = 0.1", x^1.5)),col="#009E73") +
#annotate("text", x = 1, y = 0.02, label = expression(paste("y = 0.1", x^2.3)),col="#0072B2") +
#annotate("text", x = 1, y = 0.01, label = expression(paste("y = 0.1", x^3.5)),col="#D55E00")

fig1a_dbh

# GR
fig1a_gr <- ggplot(data.frame(x = c(0, 20)), aes(x)) + 
  stat_function(fun = ~ 0.12/(1+(exp(-0.5*(.x-10)))), col="#009E73") +
  stat_function(fun = ~ 0.12/(1+(exp(-0.8*(.x-10)))), col="#0072B2") +
  stat_function(fun = ~ 0.12/(1+(exp(-1.4*(.x-10)))), col="#D55E00") +
  labs(x='GR', y='m') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,20), breaks=seq(0,20,5)) +
  scale_y_continuous(limits=c(0,0.13),breaks=seq(0,0.15,0.05))

fig1a_gr

# b) Stand develop: Stand biomass vs. time

# DBH
fig1b_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dotdash","solid")) +
  labs(x = "Year", y = "B") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))

fig1b_dbh

# GR
fig1b_gr <- ggplot() + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dotdash","solid")) +
  labs(x = "Year", y = "B") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))

fig1b_gr

# c) Growth (NPP)

# DBH
fig1c_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dotdash","solid")) +
  labs(x = "Year", y = "G") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))

fig1c_dbh

# GR
fig1c_gr <- ggplot() + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dotdash","solid")) +
  labs(x = "Year", y = "G") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))

fig1c_gr

# d) Mortality (Both mortality and biomass turnover)

# DBH
fig1d_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dotdash","solid")) +
  labs(x = "Year", y = "M") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))

fig1d_dbh

# GR
fig1d_gr <- ggplot() + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dotdash","solid")) +
  labs(x = "Year", y = "M") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))

fig1d_gr

# e) Relative change biomass (plantC) vs. NPP

#DBH
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dB0_30)

fig1e_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dB0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dB0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dB0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dB0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dB0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dB0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dB/B") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.1)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.1)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 

fig1e_dbh

# GR
# GR p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dB0_30)

# GR p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dB0_30)

# GR p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dB0_30)

fig1e_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dB0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dB0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dB0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dB0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dB0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dB0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dB/B") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0.0,0.5)) + 
  scale_y_continuous(limits = c(0.0,0.5)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed")

fig1e_gr

# f) Relative change mortality vs. NPP

# DBH
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig1f_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dM/M") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.5)) + 
  scale_y_continuous(limits = c(0,0.5)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed")

fig1f_dbh

# GR
# GR 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# GR p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# GR p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  #NPP or A_NPP
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) #plantC or A_Biomass
M15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig1f_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dM/M") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.5)) + 
  scale_y_continuous(limits = c(0,0.5)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed")

fig1f_gr

# g) Relative change k vs. NPP

# DBH
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig1g_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dk/k") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(0,0.16), breaks = seq(0,0.15,0.05)) + 
  geom_hline(yintercept =  0.0, linetype="dashed")

fig1g_dbh

# GR
# GR 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# GR 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# NSC 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig1g_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dk/k") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(0,0.030), breaks = seq(0,0.030,0.005)) +
  geom_hline(yintercept =  0.0, linetype="dashed")

fig1g_gr

# g*) Carbon Turnover time vs. time
# Plot c_turnover_time or plantC/NPP

# DBH
fig1gg_dbh <- ggplot() +  
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x1', linetype='Control'),alpha=.7) + 
  geom_line(data=ea1sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x2', linetype='Control'),alpha=.7) +
  geom_line(data=ea1sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x3', linetype='Control'),alpha=.7) +
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid")) +
  labs(x = "Year", y = expression(paste("Carbon turnover time ( ", tau, " , ", yr, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(0,32), breaks = seq(0,30,10))

fig1gg_dbh

# GR
fig1gg_gr <- ggplot() + 
  geom_line(data=ea1sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x1', linetype='Control'),alpha=.7) + 
  geom_line(data=ea1sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x2', linetype='Control'),alpha=.7) +
  geom_line(data=ea1sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x3', linetype='Control'),alpha=.7) +
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid")) +
  labs(x = "Year", y = expression(paste("Carbon turnover time ( ", tau, " , ", yr, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,10))

fig1gg_gr

# h) Relative longevity vs. growth rates

# DBH
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig1h_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dL/L") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = seq(-0.25,0,0.05)) +
  geom_hline(yintercept =  0.0, linetype="dashed")

fig1h_dbh

# GR
# GR p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# GR p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig1h_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dG/G", y="dL/L") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(-0.05,0), breaks = seq(-0.05,0,0.01)) +
  geom_hline(yintercept =  0.0, linetype="dashed")

fig1h_gr

# i) Self-thinning relationship: From annual_tile_output: QMD and Density12

# DBH
fig2a_dbh <- ggplot() + 
  geom_point(data= subset(ea1sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#009E73",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#009E73",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#009E73",
              method = "lm",fullrange = T) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Quadratic Mean Diameter (QMD)", y = "Stand density (N)",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + ggtitle("a)") + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2a_dbh

fig2b_dbh <- ggplot() + 
  geom_point(data= subset(ea1sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#0072B2",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#0072B2",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#0072B2",
              method = "lm",fullrange = T) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Quadratic Mean Diameter (QMD)", y = "Stand density (N)",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + ggtitle("b)") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2b_dbh

fig2c_dbh <- ggplot() + 
  geom_point(data= subset(ea1sa1DBHp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1DBHp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1DBHp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1DBHp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#D55E00",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1DBHp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#D55E00",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1DBHp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#D55E00",
              method = "lm",fullrange = T) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Quadratic Mean Diameter (QMD)", y = "Stand density (N)",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + ggtitle("c)") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2c_dbh













# Arrange plots
ff1 <- fig1a_dbh + fig1b_dbh + fig1c_dbh + fig1d_dbh + fig1e_dbh + fig1f_dbh + fig1g_dbh + fig1h_dbh + 
  fig1a_gr + fig1b_gr + fig1c_gr + fig1d_gr + fig1e_gr + fig1f_gr + fig1g_gr + fig1h_gr + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff1
ggsave("~/rsofun/manuscript/figures/fig_1.png", width = 12, height = 11, dpi=300)

