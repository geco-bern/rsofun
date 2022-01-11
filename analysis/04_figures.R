# This script plots the outputs from the model simulations

# load packages

# Read model outputs ####

# DBH Mortality gs-leuning 
ea1sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea1sa1DBHp1gl_out_annual_tile.csv")
ea1sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea1sa1DBHp1gl_out_annual_cohorts.csv")
ea1sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea1sa1DBHp2gl_out_annual_tile.csv")
ea1sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea1sa1DBHp2gl_out_annual_cohorts.csv")
ea1sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea1sa1DBHp3gl_out_annual_tile.csv")
ea1sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea1sa1DBHp3gl_out_annual_cohorts.csv")

ea2sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea2sa1DBHp1gl_out_annual_tile.csv")
ea2sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea2sa1DBHp1gl_out_annual_cohorts.csv")
ea2sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea2sa1DBHp2gl_out_annual_tile.csv")
ea2sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea2sa1DBHp2gl_out_annual_cohorts.csv")
ea2sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea2sa1DBHp3gl_out_annual_tile.csv")
ea2sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea2sa1DBHp3gl_out_annual_cohorts.csv")

ea3sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea3sa1DBHp1gl_out_annual_tile.csv")
ea3sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea3sa1DBHp1gl_out_annual_cohorts.csv")
ea3sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea3sa1DBHp2gl_out_annual_tile.csv")
ea3sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea3sa1DBHp2gl_out_annual_cohorts.csv")
ea3sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea3sa1DBHp3gl_out_annual_tile.csv")
ea3sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/outputs_lm3ppa/old_outputs/ea3sa1DBHp3gl_out_annual_cohorts.csv")

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
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='+30'),alpha=.7)

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
  geom_line(data=ea1sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='+30'),alpha=.7)

# Exploring relationships and plotting

# Mortality formulations ####

# DBH
scaleFUN <- function(x) sprintf("%.2f", x)

fig1a_dbh <- ggplot(data.frame(x = c(0, 1.06)), aes(x)) + 
  stat_function(fun = ~ 0.12*.x ^ 1.5, col="#009E73") +  
  stat_function(fun = ~ 0.12*.x ^ 2.5, col="#0072B2") + 
  stat_function(fun = ~ 0.12*.x ^ 5.0, col="#D55E00") + 
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

# Stand develop: Stand biomass vs. time ####

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
                        values = c("dashed","solid")) +
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
                        values = c("dashed","solid")) +
  labs(x = "Year", y = "B") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))

fig1b_gr

# Growth (NPP) ####

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
                        values = c("dashed","solid")) +
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
                        values = c("dashed","solid")) +
  labs(x = "Year", y = "G") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))

fig1c_gr

# Mortality (Both mortality and biomass turnover) ####

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
                        values = c("dashed","solid")) +
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
                        values = c("dashed","solid")) +
  labs(x = "Year", y = "M") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))

fig1d_gr

# Relative change biomass (plantC) vs. NPP ####

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

DBHp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

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

DBHp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

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

DBHp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig1e_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, color='x3', shape='0-30%'),size=3) + 
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

GRp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

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

GRp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

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

GRp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig1e_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, color='x3', shape='0-30%'),size=3) + 
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

# Relative change mortality vs. NPP ####

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

# Relative change k vs. NPP ####

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

# Carbon Turnover time vs. time ####
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

# Relative longevity vs. growth rates ####

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

# Distribution of tree sizes ####

# DBH
figdistr_dbh <- ggplot() +  
  geom_smooth(data=ea1sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x1', linetype='Control'),se=F) + 
  geom_smooth(data=ea1sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x2', linetype='Control'),se=F) +
  geom_smooth(data=ea1sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x3', linetype='Control'),se=F) +
  geom_smooth(data=ea2sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x1', linetype='+15%'),se=F) + 
  geom_smooth(data=ea2sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x2', linetype='+15%'),se=F) +
  geom_smooth(data=ea2sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x3', linetype='+15%'),se=F) +
  geom_smooth(data=ea3sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x1', linetype='+30%'),se=F) + 
  geom_smooth(data=ea3sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x2', linetype='+30%'),se=F) +
  geom_smooth(data=ea3sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x3', linetype='+30%'),se=F) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid")) +
  labs(x = "DBH (cm)", y = "Log(N)") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) #+ 
#scale_y_continuous(limits = c(-4.5,11.2), breaks = seq(0,10,5))
figdistr_dbh

# GR
figdistr_gr <- ggplot() +  
  geom_smooth(data=ea1sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x1', linetype='Control'),se=F) + 
  geom_smooth(data=ea1sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x2', linetype='Control'),se=F) +
  geom_smooth(data=ea1sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x3', linetype='Control'),se=F) +
  geom_smooth(data=ea2sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x1', linetype='+15%'),se=F) + 
  geom_smooth(data=ea2sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x2', linetype='+15%'),se=F) +
  geom_smooth(data=ea2sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x3', linetype='+15%'),se=F) +
  geom_smooth(data=ea3sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x1', linetype='+30%'),se=F) + 
  geom_smooth(data=ea3sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x2', linetype='+30%'),se=F) +
  geom_smooth(data=ea3sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='x3', linetype='+30%'),se=F) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid")) +
  labs(x = "DBH (cm)", y = "Log(N)") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
figdistr_gr

# Self-thinning relationship
# 1) From annual_tile_output: QMD and Density12 ####

# DBH
# DBH1
fig2a_dbh <- ggplot() + 
  geom_point(data= subset(ea1sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#009E73",
              method = "lm",fullrange = T,size=0.6) + 
  geom_smooth(data= subset(ea2sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  geom_smooth(data= subset(ea3sa1DBHp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
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
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4.5,6.5,.5))
fig2a_dbh

# DBH2
fig2b_dbh <- ggplot() + 
  geom_point(data= subset(ea1sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#0072B2",
              method = "lm",fullrange = T,size=0.6) + 
  geom_smooth(data= subset(ea2sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#0072B2",
              method = "lm",fullrange = T,size=0.6) +
  geom_smooth(data= subset(ea3sa1DBHp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#0072B2",
              method = "lm",fullrange = T,size=0.6) +
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
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4.5,6.5,.5))
fig2b_dbh

# DBH3
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
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4.5,6.5,.5))
fig2c_dbh

# GR
# GR1
fig2g_gr <- ggplot() + 
  geom_point(data= subset(ea1sa1GRp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1GRp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1GRp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1GRp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#009E73",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1GRp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#009E73",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1GRp1gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#009E73",
              method = "lm",fullrange = T) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Quadratic Mean Diameter (QMD)", y = "Stand density (N)",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4.5,6.5,.5))
fig2g_gr

# GR2
fig2h_gr <- ggplot() + 
  geom_point(data= subset(ea1sa1GRp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1GRp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1GRp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1GRp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#0072B2",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1GRp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#0072B2",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1GRp2gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#0072B2",
              method = "lm",fullrange = T) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Quadratic Mean Diameter (QMD)", y = "Stand density (N)",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11))  #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2h_gr

# GR3
fig2i_gr <- ggplot() + 
  geom_point(data= subset(ea1sa1GRp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea2sa1GRp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_point(data=subset(ea3sa1GRp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="grey") + 
  geom_smooth(data= subset(ea1sa1GRp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#D55E00",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1GRp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#D55E00",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1GRp3gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#D55E00",
              method = "lm",fullrange = T) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Quadratic Mean Diameter (QMD)", y = "Stand density (N)",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11))  #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2i_gr

# 2) Plotted as for empirical data ####

# DBH

# DBH1 ####
ea1sa1DBHp1gl_out_annual_tile <- ea1sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1DBHp1gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1DBHp1gl_out_annual_tile$NPP * 10000

data_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea1p1$QMD_bins))
max(data_DBH_ea1p1$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea1p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p1 <- data_DBH_ea1p1 %>% left_join(quantileX)
data_DBH_ea1p1Den <- data_DBH_ea1p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p1Rest <- data_DBH_ea1p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1DBHp1gl_out_annual_tile <- ea2sa1DBHp1gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1DBHp1gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1DBHp1gl_out_annual_tile$NPP * 10000

data_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea2p1$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea2p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p1 <- data_DBH_ea2p1 %>% left_join(quantileX)
data_DBH_ea2p1Den <- data_DBH_ea2p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p1Rest <- data_DBH_ea2p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1DBHp1gl_out_annual_tile <- ea3sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1DBHp1gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1DBHp1gl_out_annual_tile$NPP * 10000

data_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea3p1$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea3p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p1 <- data_DBH_ea3p1 %>% left_join(quantileX)
data_DBH_ea3p1Den <- data_DBH_ea3p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p1Rest <- data_DBH_ea3p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_DBH_p1 <- rbind(data_DBH_ea1p1, data_DBH_ea2p1, data_DBH_ea3p1) 
data_DBH_p1Den <- rbind(data_DBH_ea1p1Den, data_DBH_ea2p1Den, data_DBH_ea3p1Den) 
data_DBH_p1Rest <- rbind(data_DBH_ea1p1Rest, data_DBH_ea2p1Rest, data_DBH_ea3p1Rest) 

data_DBH_p1Den <- data_DBH_p1Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_DBH_p1Rest <- data_DBH_p1Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_DBH_p1, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p1Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_DBH_p1Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_DBH_p1Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDNoInter))
fig2aLUE_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                           terms = c("logQMD","LUE"),title = "",
                           axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                           colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aLUE_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH1 <- as.data.frame(pred)

fig2aLUE_dbh1 <- ggplot() + 
  geom_point(data = data_DBH_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH1, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6) +
  labs(x = "QMD", y = "N",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2aLUE_dbh1

# as NPP change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(NPP_kg_ha_year), data = data_DBH_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(NPP_kg_ha_year), data = data_DBH_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
max(data_DBH_p1Den$NPP_kg_ha_year,na.rm=T)
hist(data_DBH_p1Den$NPP_kg_ha_year)
hist_NPP_dbh <- ggplot(data_DBH_p1Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D",binwidth=30) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year",breaks = seq(10000,30000,1000)) +
  scale_y_continuous("Frequency")
hist_NPP_dbh

fig2aNPP_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                           terms = c("logQMD","NPP_kg_ha_year[15000,17500,20000]"),title = "",
                           axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of NPP",
                           colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aNPP_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "NPP_kg_ha_year[15000,17500,20000]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2aNPP_dbh1 <- ggplot() + 
  geom_point(data = data_DBH_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size =0.6) +
  labs(x = "QMD", y = "N",
       color  = "NPP", linetype = "NPP") + 
  scale_linetype_manual("NPP", breaks = c("15000","17500", "20000"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2aNPP_dbh1

# as NPP Residuals change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(Res_NPP), data = data_DBH_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(Res_NPP), data = data_DBH_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
# Analysis of residuals
ResG <- residuals(Fit_QMDInter)
FittedG <- fitted(Fit_QMDInter)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(data_DBH_p1Den$logDensity12 ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
hist(data_DBH_p1Den$Res_NPP)
hist_Res_dbh <- ggplot(data_DBH_p1Den, aes(x=Res_NPP)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_Res_dbh

fig2aRes_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                           terms = c("logQMD","Res_NPP[-0.2, 0, 0.2]"),title = "",
                           axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Res_NPP",
                           colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aRes_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "Res_NPP[-0.2, 0, 0.2]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2aRes_dbh1 <- ggplot() + 
  geom_point(data = data_DBH_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="grey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  labs(x = "QMD", y = "N",
       color  = "Res_NPP", linetype = "Res_NPP") + 
  scale_linetype_manual("Res_NPP", breaks = c("-0.2","0", "0.2"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2aRes_dbh1

# DBH2 ####
ea1sa1DBHp2gl_out_annual_tile <- ea1sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1DBHp2gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1DBHp2gl_out_annual_tile$NPP * 10000

data_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea1p2$QMD_bins))
max(data_DBH_ea1p2$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea1p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p2 <- data_DBH_ea1p2 %>% left_join(quantileX)
data_DBH_ea1p2Den <- data_DBH_ea1p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p2Rest <- data_DBH_ea1p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1DBHp2gl_out_annual_tile <- ea2sa1DBHp2gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1DBHp2gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1DBHp2gl_out_annual_tile$NPP * 10000

data_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea2p2$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea2p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p2 <- data_DBH_ea2p2 %>% left_join(quantileX)
data_DBH_ea2p2Den <- data_DBH_ea2p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p2Rest <- data_DBH_ea2p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1DBHp2gl_out_annual_tile <- ea3sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1DBHp2gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1DBHp2gl_out_annual_tile$NPP * 10000

data_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea3p2$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea3p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p2 <- data_DBH_ea3p2 %>% left_join(quantileX)
data_DBH_ea3p2Den <- data_DBH_ea3p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p2Rest <- data_DBH_ea3p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_DBH_p2 <- rbind(data_DBH_ea1p2, data_DBH_ea2p2, data_DBH_ea3p2) 
data_DBH_p2Den <- rbind(data_DBH_ea1p2Den, data_DBH_ea2p2Den, data_DBH_ea3p2Den) 
data_DBH_p2Rest <- rbind(data_DBH_ea1p2Rest, data_DBH_ea2p2Rest, data_DBH_ea3p2Rest) 

data_DBH_p2Den <- data_DBH_p2Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_DBH_p2Rest <- data_DBH_p2Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_DBH_p2, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p2Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_DBH_p2Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_DBH_p2Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
fig2bLUE_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bLUE_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH2 <- as.data.frame(pred)

fig2bLUE_dbh2 <- ggplot() + 
  geom_point(data = data_DBH_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH2, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6) +
  labs(x = "QMD", y = "N",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2bLUE_dbh2

# as NPP change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(NPP_kg_ha_year), data = data_DBH_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(NPP_kg_ha_year), data = data_DBH_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
max(data_DBH_p2Den$NPP_kg_ha_year,na.rm=T)
hist(data_DBH_p2Den$NPP_kg_ha_year)
hist_NPP <- ggplot(data_DBH_p2Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D",binwidth=30) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year",breaks = seq(10000,30000,1000)) +
  scale_y_continuous("Frequency")
hist_NPP

fig2bNPP_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","NPP_kg_ha_year[15000,17500,20000]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bNPP_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "NPP_kg_ha_year[15000,17500,20000]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2bNPP_dbh2 <- ggplot() + 
  geom_point(data = data_DBH_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size =0.6) +
  labs(x = "QMD", y = "N",
       color  = "NPP", linetype = "NPP") + 
  scale_linetype_manual("NPP", breaks = c("15000","17500", "20000"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2bNPP_dbh2

# as NPP Residuals change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(Res_NPP), data = data_DBH_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(Res_NPP), data = data_DBH_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
# Analysis of residuals
ResG <- residuals(Fit_QMDInter)
FittedG <- fitted(Fit_QMDInter)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(data_DBH_p2Den$logDensity12 ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
hist(data_DBH_p2Den$Res_NPP)
hist_Res <- ggplot(data_DBH_p2Den, aes(x=Res_NPP)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_Res

fig2bRes_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","Res_NPP[-0.2, 0, 0.2]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Res_NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bRes_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "Res_NPP[-0.2, 0, 0.2]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2bRes_dbh2 <- ggplot() + 
  geom_point(data = data_DBH_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  labs(x = "QMD", y = "N",
       color  = "Res_NPP", linetype = "Res_NPP") + 
  scale_linetype_manual("Res_NPP", breaks = c("-0.2","0", "0.2"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2bRes_dbh2

# DBH3 ####
ea1sa1DBHp3gl_out_annual_tile <- ea1sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1DBHp3gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1DBHp3gl_out_annual_tile$NPP * 10000

data_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea1p3$QMD_bins))
max(data_DBH_ea1p3$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea1p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p3 <- data_DBH_ea1p3 %>% left_join(quantileX)
data_DBH_ea1p3Den <- data_DBH_ea1p3 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p3Rest <- data_DBH_ea1p3 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1DBHp3gl_out_annual_tile <- ea2sa1DBHp3gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1DBHp3gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1DBHp3gl_out_annual_tile$NPP * 10000

data_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea2p3$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea2p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p3 <- data_DBH_ea2p3 %>% left_join(quantileX)
data_DBH_ea2p3Den <- data_DBH_ea2p3 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p3Rest <- data_DBH_ea2p3 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1DBHp3gl_out_annual_tile <- ea3sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1DBHp3gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1DBHp3gl_out_annual_tile$NPP * 10000

data_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea3p3$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea3p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p3 <- data_DBH_ea3p3 %>% left_join(quantileX)
data_DBH_ea3p3Den <- data_DBH_ea3p3 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p3Rest <- data_DBH_ea3p3 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_DBH_p3 <- rbind(data_DBH_ea1p3, data_DBH_ea2p3, data_DBH_ea3p3) 
data_DBH_p3Den <- rbind(data_DBH_ea1p3Den, data_DBH_ea2p3Den, data_DBH_ea3p3Den) 
data_DBH_p3Rest <- rbind(data_DBH_ea1p3Rest, data_DBH_ea2p3Rest, data_DBH_ea3p3Rest) 

data_DBH_p3Den <- data_DBH_p3Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_DBH_p3Rest <- data_DBH_p3Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_DBH_p3, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p3Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_DBH_p3Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_DBH_p3Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
fig2cLUE_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cLUE_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH3 <- as.data.frame(pred)

fig2cLUE_dbh3 <- ggplot() + 
  geom_point(data = data_DBH_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH3, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6) +
  labs(x = "QMD", y = "N",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2cLUE_dbh3

# as NPP change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(NPP_kg_ha_year), data = data_DBH_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(NPP_kg_ha_year), data = data_DBH_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
max(data_DBH_p3Den$NPP_kg_ha_year,na.rm=T)
hist(data_DBH_p3Den$NPP_kg_ha_year)
hist_NPP <- ggplot(data_DBH_p3Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D",binwidth=30) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year",breaks = seq(10000,30000,1000)) +
  scale_y_continuous("Frequency")
hist_NPP

fig2cNPP_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","NPP_kg_ha_year[15000,17500,20000]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cNPP_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "NPP_kg_ha_year[15000,17500,20000]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2cNPP_dbh3 <- ggplot() + 
  geom_point(data = data_DBH_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size =0.6) +
  labs(x = "QMD", y = "N",
       color  = "NPP", linetype = "NPP") + 
  scale_linetype_manual("NPP", breaks = c("15000","17500", "20000"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2cNPP_dbh3

# as NPP Residuals change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(Res_NPP), data = data_DBH_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(Res_NPP), data = data_DBH_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
# Analysis of residuals
ResG <- residuals(Fit_QMDInter)
FittedG <- fitted(Fit_QMDInter)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(data_DBH_p3Den$logDensity12 ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
hist(data_DBH_p3Den$Res_NPP)
hist_Res <- ggplot(data_DBH_p3Den, aes(x=Res_NPP)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_Res

fig2cRes_dbh_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","Res_NPP[-0.2, 0, 0.2]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Res_NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cRes_dbh_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "Res_NPP[-0.2, 0, 0.2]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2cRes_dbh3 <- ggplot() + 
  geom_point(data = data_DBH_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  labs(x = "QMD", y = "N",
       color  = "Res_NPP", linetype = "Res_NPP") + 
  scale_linetype_manual("Res_NPP", breaks = c("-0.2","0", "0.2"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2cRes_dbh3

# GR

# GR1 ####
ea1sa1GRp1gl_out_annual_tile <- ea1sa1GRp1gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1GRp1gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1GRp1gl_out_annual_tile$NPP * 10000

data_GR_ea1p1 <- ea1sa1GRp1gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea1p1$QMD_bins))
max(data_GR_ea1p1$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea1p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p1 <- data_GR_ea1p1 %>% left_join(quantileX)
data_GR_ea1p1Den <- data_GR_ea1p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p1Rest <- data_GR_ea1p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1GRp1gl_out_annual_tile <- ea2sa1GRp1gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1GRp1gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1GRp1gl_out_annual_tile$NPP * 10000

data_GR_ea2p1 <- ea2sa1GRp1gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea2p1$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea2p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p1 <- data_GR_ea2p1 %>% left_join(quantileX)
data_GR_ea2p1Den <- data_GR_ea2p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p1Rest <- data_GR_ea2p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1GRp1gl_out_annual_tile <- ea3sa1GRp1gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1GRp1gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1GRp1gl_out_annual_tile$NPP * 10000

data_GR_ea3p1 <- ea3sa1GRp1gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea3p1$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea3p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p1 <- data_GR_ea3p1 %>% left_join(quantileX)
data_GR_ea3p1Den <- data_GR_ea3p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p1Rest <- data_GR_ea3p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_GR_p1 <- rbind(data_GR_ea1p1, data_GR_ea2p1, data_GR_ea3p1) 
data_GR_p1Den <- rbind(data_GR_ea1p1Den, data_GR_ea2p1Den, data_GR_ea3p1Den) 
data_GR_p1Rest <- rbind(data_GR_ea1p1Rest, data_GR_ea2p1Rest, data_GR_ea3p1Rest) 

data_GR_p1Den <- data_GR_p1Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_GR_p1Rest <- data_GR_p1Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_GR_p1, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p1Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_GR_p1Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_GR_p1Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_GR_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDNoInter))
fig2aLUE_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aLUE_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR1 <- as.data.frame(pred)

fig2aLUE_gr1 <- ggplot() + 
  geom_point(data = data_GR_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR1, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6) +
  labs(x = "QMD", y = "N",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2aLUE_gr1

# as NPP change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(NPP_kg_ha_year), data = data_GR_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(NPP_kg_ha_year), data = data_GR_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
max(data_GR_p1Den$NPP_kg_ha_year,na.rm=T)
hist(data_GR_p1Den$NPP_kg_ha_year)
hist_NPP_gr <- ggplot(data_GR_p1Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D",binwidth=30) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year",breaks = seq(15000,30000,1000)) +
  scale_y_continuous("Frequency")
hist_NPP_gr

fig2aNPP_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","NPP_kg_ha_year[17000,20000,23000]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aNPP_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "NPP_kg_ha_year[17000,20000,23000]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2aNPP_gr1 <- ggplot() + 
  geom_point(data = data_GR_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size =0.6) +
  labs(x = "QMD", y = "N",
       color  = "NPP", linetype = "NPP") + 
  scale_linetype_manual("NPP", breaks = c("17000","20000", "23000"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2aNPP_gr1

# as NPP Residuals change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(Res_NPP), data = data_GR_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(Res_NPP), data = data_GR_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
# Analysis of residuals
ResG <- residuals(Fit_QMDInter)
FittedG <- fitted(Fit_QMDInter)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(data_GR_p1Den$logDensity12 ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
hist(data_GR_p1Den$Res_NPP)
hist_Res_gr <- ggplot(data_GR_p1Den, aes(x=Res_NPP)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_Res_gr

fig2aRes_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","Res_NPP[-0.2, 0, 0.2]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Res_NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aRes_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "Res_NPP[-0.2, 0, 0.2]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2aRes_gr1 <- ggplot() + 
  geom_point(data = data_GR_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="grey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  labs(x = "QMD", y = "N",
       color  = "Res_NPP", linetype = "Res_NPP") + 
  scale_linetype_manual("Res_NPP", breaks = c("-0.2","0", "0.2"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2aRes_gr1

# GR2 ####
ea1sa1GRp2gl_out_annual_tile <- ea1sa1GRp2gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1GRp2gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1GRp2gl_out_annual_tile$NPP * 10000

data_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea1p2$QMD_bins))
max(data_GR_ea1p2$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea1p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p2 <- data_GR_ea1p2 %>% left_join(quantileX)
data_GR_ea1p2Den <- data_GR_ea1p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p2Rest <- data_GR_ea1p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1GRp2gl_out_annual_tile <- ea2sa1GRp2gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1GRp2gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1GRp2gl_out_annual_tile$NPP * 10000

data_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea2p2$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea2p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p2 <- data_GR_ea2p2 %>% left_join(quantileX)
data_GR_ea2p2Den <- data_GR_ea2p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p2Rest <- data_GR_ea2p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1GRp2gl_out_annual_tile <- ea3sa1GRp2gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1GRp2gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1GRp2gl_out_annual_tile$NPP * 10000

data_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea3p2$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea3p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p2 <- data_GR_ea3p2 %>% left_join(quantileX)
data_GR_ea3p2Den <- data_GR_ea3p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p2Rest <- data_GR_ea3p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_GR_p2 <- rbind(data_GR_ea1p2, data_GR_ea2p2, data_GR_ea3p2) 
data_GR_p2Den <- rbind(data_GR_ea1p2Den, data_GR_ea2p2Den, data_GR_ea3p2Den) 
data_GR_p2Rest <- rbind(data_GR_ea1p2Rest, data_GR_ea2p2Rest, data_GR_ea3p2Rest) 

data_GR_p2Den <- data_GR_p2Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_GR_p2Rest <- data_GR_p2Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_GR_p2, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p2Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_GR_p2Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_GR_p2Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_GR_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
fig2bLUE_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bLUE_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR2 <- as.data.frame(pred)

fig2bLUE_gr2 <- ggplot() + 
  geom_point(data = data_GR_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR2, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6) +
  labs(x = "QMD", y = "N",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2bLUE_gr2

# as NPP change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(NPP_kg_ha_year), data = data_GR_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(NPP_kg_ha_year), data = data_GR_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
max(data_GR_p2Den$NPP_kg_ha_year,na.rm=T)
hist(data_GR_p2Den$NPP_kg_ha_year)
hist_NPP <- ggplot(data_GR_p2Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D",binwidth=30) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_NPP

fig2bNPP_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","NPP_kg_ha_year[17000,20000,23000]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bNPP_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "NPP_kg_ha_year[17000,20000,23000]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2bNPP_gr2 <- ggplot() + 
  geom_point(data = data_GR_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size =0.6) +
  labs(x = "QMD", y = "N",
       color  = "NPP", linetype = "NPP") + 
  scale_linetype_manual("NPP", breaks = c("17000","20000", "23000"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2bNPP_gr2

# as NPP Residuals change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(Res_NPP), data = data_GR_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(Res_NPP), data = data_GR_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
# Analysis of residuals
ResG <- residuals(Fit_QMDInter)
FittedG <- fitted(Fit_QMDInter)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(data_GR_p2Den$logDensity12 ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
hist(data_GR_p2Den$Res_NPP)
hist_Res <- ggplot(data_GR_p2Den, aes(x=Res_NPP)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_Res

fig2bRes_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","Res_NPP[-0.2, 0, 0.2]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Res_NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bRes_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "Res_NPP[-0.2, 0, 0.2]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2bRes_gr2 <- ggplot() + 
  geom_point(data = data_GR_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  labs(x = "QMD", y = "N",
       color  = "Res_NPP", linetype = "Res_NPP") + 
  scale_linetype_manual("Res_NPP", breaks = c("-0.2","0", "0.2"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2bRes_gr2

# GR3 ####
ea1sa1GRp3gl_out_annual_tile <- ea1sa1GRp3gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1GRp3gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1GRp3gl_out_annual_tile$NPP * 10000

data_GR_ea1p3 <- ea1sa1GRp3gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea1p3$QMD_bins))
max(data_GR_ea1p3$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea1p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p3 <- data_GR_ea1p3 %>% left_join(quantileX)
data_GR_ea1p3Den <- data_GR_ea1p3 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p3Rest <- data_GR_ea1p3 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1GRp3gl_out_annual_tile <- ea2sa1GRp3gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1GRp3gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1GRp3gl_out_annual_tile$NPP * 10000

data_GR_ea2p3 <- ea2sa1GRp3gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea2p3$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea2p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p3 <- data_GR_ea2p3 %>% left_join(quantileX)
data_GR_ea2p3Den <- data_GR_ea2p3 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p3Rest <- data_GR_ea2p3 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1GRp3gl_out_annual_tile <- ea3sa1GRp3gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1GRp3gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1GRp3gl_out_annual_tile$NPP * 10000

data_GR_ea3p3 <- ea3sa1GRp3gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea3p3$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea3p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p3 <- data_GR_ea3p3 %>% left_join(quantileX)
data_GR_ea3p3Den <- data_GR_ea3p3 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p3Rest <- data_GR_ea3p3 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_GR_p3 <- rbind(data_GR_ea1p3, data_GR_ea2p3, data_GR_ea3p3) 
data_GR_p3Den <- rbind(data_GR_ea1p3Den, data_GR_ea2p3Den, data_GR_ea3p3Den) 
data_GR_p3Rest <- rbind(data_GR_ea1p3Rest, data_GR_ea2p3Rest, data_GR_ea3p3Rest) 

data_GR_p3Den <- data_GR_p3Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_GR_p3Rest <- data_GR_p3Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_GR_p3, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p3Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_GR_p3Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_GR_p3Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_GR_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
fig2cLUE_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cLUE_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR3 <- as.data.frame(pred)

fig2cLUE_gr3 <- ggplot() + 
  geom_point(data = data_GR_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR3, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6) +
  labs(x = "QMD", y = "N",
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2cLUE_gr3

# as NPP change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(NPP_kg_ha_year), data = data_GR_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(NPP_kg_ha_year), data = data_GR_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
max(data_GR_p3Den$NPP_kg_ha_year,na.rm=T)
hist(data_GR_p3Den$NPP_kg_ha_year)
hist_NPP <- ggplot(data_GR_p3Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D",binwidth=30) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_NPP

fig2cNPP_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","NPP_kg_ha_year[17000,20000,23000]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cNPP_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "NPP_kg_ha_year[17000,20000,23000]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2cNPP_gr3 <- ggplot() + 
  geom_point(data = data_GR_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size =0.6) +
  labs(x = "QMD", y = "N",
       color  = "NPP", linetype = "NPP") + 
  scale_linetype_manual("NPP", breaks = c("17000","20000", "23000"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2cNPP_gr3

# as NPP Residuals change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * scale(Res_NPP), data = data_GR_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + scale(Res_NPP), data = data_GR_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
# Analysis of residuals
ResG <- residuals(Fit_QMDInter)
FittedG <- fitted(Fit_QMDInter)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(data_GR_p3Den$logDensity12 ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
hist(data_GR_p3Den$Res_NPP)
hist_Res <- ggplot(data_GR_p3Den, aes(x=Res_NPP)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),legend.title = element_text(size = 11),
        plot.margin = unit(c(.1,1,1,.5), "cm")) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year") +
  scale_y_continuous("Frequency")
hist_Res

fig2cRes_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","Res_NPP[-0.2, 0, 0.2]"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Res_NPP",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cRes_gr_a

pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "Res_NPP[-0.2, 0, 0.2]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig2cRes_gr3 <- ggplot() + 
  geom_point(data = data_GR_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddata, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size=0.6) +
  labs(x = "QMD", y = "N",
       color  = "Res_NPP", linetype = "Res_NPP") + 
  scale_linetype_manual("Res_NPP", breaks = c("-0.2","0", "0.2"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     legend.key.size = unit(1, 'cm'),
                     plot.title = element_text(size = 11)) + guides(color = "none") #+ 
#scale_x_continuous(limits = c(3.4,4.1),breaks = seq(3.5,4,0.1)) + 
#scale_y_continuous(limits = c(4.5,6.5),breaks = seq(4,7,1))
fig2cRes_gr3



# 3) Link model and observations ####

# DBH
# DBH1
# Upward shift of the STL in the model
preddataLUE_DBH1
preddataLUE_DBH1_agg <- preddataLUE_DBH1 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_DBH1_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH1_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp1gl_RelChange_B_NPP_0_15 <- DBHp1gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
DBHp1gl_RelChange_B_NPP_0_30 <- DBHp1gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# DBH2
# Upward shift of the STL in the model
preddataLUE_DBH2
preddataLUE_DBH_agg2 <- preddataLUE_DBH2 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_DBH_agg2 %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH_agg2 %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp2gl_RelChange_B_NPP_0_15 <- DBHp2gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
DBHp2gl_RelChange_B_NPP_0_30 <- DBHp2gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# DBH3
# Upward shift of the STL in the model
preddataLUE_DBH3
preddataLUE_DBH_agg3 <- preddataLUE_DBH3 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_DBH_agg3 %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH_agg3 %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp3gl_RelChange_B_NPP_0_15 <- DBHp3gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
DBHp3gl_RelChange_B_NPP_0_30 <- DBHp3gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# Upward shift of the STL from observations
load("~/rsofun/data/inputs/preddataRes.RData")
preddata
preddata_agg <- preddata %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddata_agg %>%
  filter(group==1.5) %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddata_agg %>%
  filter(group==0) %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

load("~/rsofun/data/inputs/preddataGrowth.RData")
preddata
preddata_agg <- preddata %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddata_agg %>%
  filter(group==21000) %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddata_agg %>%
  filter(group==18500) %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

fig4_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dlnB/dlnG", y="dN/N") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  scale_x_continuous(limits = c(0.5,0.7),breaks=seq(0.5,0.7,0.2)) + 
  scale_y_continuous(limits = c(0,0.2),breaks=seq(0,0.2,0.05)) +
  geom_hline(yintercept = N15, linetype="dashed") +
  geom_hline(yintercept = N30, linetype="dashed") +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=N15,ymax=N30),fill="grey", alpha=0.2)
fig4_dbh

# GR ####
# GR1
# Upward shift of the STL in the model ####
preddataLUE_GR1
preddataLUE_GR1_agg <- preddataLUE_GR1 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_GR1_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR1_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP ####
GRp1gl_RelChange_B_NPP_0_15 <- GRp1gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp1gl_RelChange_B_NPP_0_30 <- GRp1gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR2
# Upward shift of the STL in the model ####
preddataLUE_GR2
preddataLUE_GR_agg2 <- preddataLUE_GR2 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_GR_agg2 %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR_agg2 %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP ####
GRp2gl_RelChange_B_NPP_0_15 <- GRp2gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp2gl_RelChange_B_NPP_0_30 <- GRp2gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR3
# Upward shift of the STL in the model ####
preddataLUE_GR3
preddataLUE_GR_agg3 <- preddataLUE_GR3 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_GR_agg3 %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR_agg3 %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP ####
GRp3gl_RelChange_B_NPP_0_15 <- GRp3gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp3gl_RelChange_B_NPP_0_30 <- GRp3gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# Upward shift of the STL from observations ####
load("~/rsofun/data/inputs/preddataRes.RData")
preddata
preddata_agg <- preddata %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddata_agg %>%
  filter(group==1.5) %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddata_agg %>%
  filter(group==0) %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

load("~/rsofun/data/inputs/preddataGrowth.RData")
preddata
preddata_agg <- preddata %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddata_agg %>%
  filter(group==21000) %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddata_agg %>%
  filter(group==18500) %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

fig4_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  labs(x="dlnB/dlnG", y="dN/N") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  #scale_x_continuous(limits = c(0.5,0.7),breaks=seq(0.5,0.7,0.2)) + 
  scale_y_continuous(limits = c(0,0.2),breaks=seq(0,0.2,0.05)) +
  geom_hline(yintercept = N15, linetype="dashed") +
  geom_hline(yintercept = N30, linetype="dashed") +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=N15,ymax=N30),fill="grey", alpha=0.2)
fig4_gr








# Figure 1 ####
ff1 <- fig1a_dbh + fig1b_dbh + fig1c_dbh + fig1d_dbh + fig1e_dbh + fig1f_dbh + fig1g_dbh + fig1h_dbh + 
  fig1a_gr + fig1b_gr + fig1c_gr + fig1d_gr + fig1e_gr + fig1f_gr + fig1g_gr + fig1h_gr + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff1
ggsave("~/rsofun/manuscript/figures/fig_1_Rev1.png", width = 12, height = 11, dpi=300)

# Figure 2 ####
ff2LUE <- fig2aLUE_dbh1 + fig2bLUE_dbh2 + fig2cLUE_dbh3 + 
  fig2aLUE_gr1 + fig2bLUE_gr2 + fig2cLUE_gr3 + 
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff2LUE
ggsave("~/rsofun/manuscript/figures/fig2LUE.png", width = 12, height = 9, dpi=300)

ff2NPP <- fig2aNPP_dbh1 + fig2bNPP_dbh2 + fig2cNPP_dbh3 + 
  fig2aNPP_gr1 + fig2bNPP_gr2 + fig2cNPP_gr3 +
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff2NPP
ggsave("~/rsofun/manuscript/figures/fig2NPP.png", width = 12, height = 9, dpi=300)

ff2Res <- fig2aRes_dbh + fig2bRes_dbh + fig2cRes_dbh + 
  fig2aRes_gr1 + fig2bRes_gr2 + fig2cRes_gr3 +
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff2Res
ggsave("~/rsofun/manuscript/figures/fig2Res.png", width = 12, height = 9, dpi=300) 

ffhist <- hist_NPP_dbh + hist_NPP_gr +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffhist
ggsave("~/rsofun/manuscript/figures/figHistMod.png", width = 12, height = 6, dpi=300)

# Figure 4 ####
ff4 <- fig4_dbh + fig4_gr + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff4
ggsave("~/rsofun/manuscript/figures/ff4.png", width = 12, height = 6, dpi=300)


# Figure S1 ####
ffs1 <- figdistr_dbh + figdistr_gr +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffs1
ggsave("~/rsofun/manuscript/figures/fig_S1.png", width = 9, height = 5, dpi=300)

# Figure S2 ####
ffs2 <- fig1gg_dbh + fig1gg_gr + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffs2
ggsave("~/rsofun/vignettes_add/fig_S2.png", width = 9, height = 5, dpi=300)
