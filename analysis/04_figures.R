# This script plots the outputs from the model simulations

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(ggeffects)
library(patchwork)

# Read model outputs ####

# DBH Mortality gs-leuning 
ea1sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp1gl_out_annual_tile.csv")
ea1sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp1gl_out_annual_cohorts.csv")
ea1sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp2gl_out_annual_tile.csv")
ea1sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp2gl_out_annual_cohorts.csv")
ea1sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp3gl_out_annual_tile.csv")
ea1sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp3gl_out_annual_cohorts.csv")

ea1sa1DBHp4gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp4gl_out_annual_tile.csv")
ea1sa1DBHp4gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1DBHp4gl_out_annual_cohorts.csv")

ea2sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp1gl_out_annual_tile.csv")
ea2sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp1gl_out_annual_cohorts.csv")
ea2sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp2gl_out_annual_tile.csv")
ea2sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp2gl_out_annual_cohorts.csv")
ea2sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp3gl_out_annual_tile.csv")
ea2sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp3gl_out_annual_cohorts.csv")

ea2sa1DBHp4gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp4gl_out_annual_tile.csv")
ea2sa1DBHp4gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1DBHp4gl_out_annual_cohorts.csv")

ea3sa1DBHp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp1gl_out_annual_tile.csv")
ea3sa1DBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp1gl_out_annual_cohorts.csv")
ea3sa1DBHp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp2gl_out_annual_tile.csv")
ea3sa1DBHp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp2gl_out_annual_cohorts.csv")
ea3sa1DBHp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp3gl_out_annual_tile.csv")
ea3sa1DBHp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp3gl_out_annual_cohorts.csv")

ea3sa1DBHp4gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp4gl_out_annual_tile.csv")
ea3sa1DBHp4gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1DBHp4gl_out_annual_cohorts.csv")

ggplot() + 
  geom_line(data=ea1sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, color='+30'),alpha=.7)

ggplot() + 
  geom_line(data=ea1sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC, color='+30'),alpha=.7)

# GR Mortality gs-leuning 
ea1sa1GRp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp1gl_out_annual_tile.csv")
ea1sa1GRp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp1gl_out_annual_cohorts.csv")
ea1sa1GRp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp2gl_out_annual_tile.csv")
ea1sa1GRp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp2gl_out_annual_cohorts.csv")
ea1sa1GRp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp3gl_out_annual_tile.csv")
ea1sa1GRp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp3gl_out_annual_cohorts.csv")

ea1sa1GRp4gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea1sa1GRp4gl_out_annual_tile.csv")
ea1sa1GRp4gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea1sa1GRp4gl_out_annual_cohorts.csv")

ea2sa1GRp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp1gl_out_annual_tile.csv")
ea2sa1GRp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp1gl_out_annual_cohorts.csv")
ea2sa1GRp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp2gl_out_annual_tile.csv")
ea2sa1GRp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp2gl_out_annual_cohorts.csv")
ea2sa1GRp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp3gl_out_annual_tile.csv")
ea2sa1GRp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp3gl_out_annual_cohorts.csv")

ea2sa1GRp4gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea2sa1GRp4gl_out_annual_tile.csv")
ea2sa1GRp4gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea2sa1GRp4gl_out_annual_cohorts.csv")

ea3sa1GRp1gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp1gl_out_annual_tile.csv")
ea3sa1GRp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp1gl_out_annual_cohorts.csv")
ea3sa1GRp2gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp2gl_out_annual_tile.csv")
ea3sa1GRp2gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp2gl_out_annual_cohorts.csv")
ea3sa1GRp3gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp3gl_out_annual_tile.csv")
ea3sa1GRp3gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp3gl_out_annual_cohorts.csv")

ea3sa1GRp4gl_out_annual_tile    <- read.csv("~/rsofun/data/outputs/ea3sa1GRp4gl_out_annual_tile.csv")
ea3sa1GRp4gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/ea3sa1GRp4gl_out_annual_cohorts.csv")

ggplot() + 
  geom_line(data=ea1sa1GRp1gl_out_annual_tile, aes(x=year, y=MaxAge, color='Control'),alpha=.7) + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=MaxAge, color='+15'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=MaxAge, color='+30'),alpha=.7) 

# Exploring relationships and plotting

# Mortality formulations ####

# DBH
scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data.frame(x = c(0, 0.5)), aes(x)) + 
  stat_function(fun = ~ 0.12*.x ^ 1.5, aes(colour = "r1")) + 
  stat_function(fun = ~ 0.12*.x ^ 2.5, aes(colour = "r2")) + 
  stat_function(fun = ~ 0.12*.x ^ 5.0, aes(colour = "r3")) 

fig1a_dbh <- ggplot(data.frame(x = c(0, 1.2)), aes(x)) + 
  stat_function(fun = ~ 0.12*.x ^ 1.5, aes(colour = "r1")) + 
  stat_function(fun = ~ 0.12*.x ^ 2.5, aes(colour = "r2")) + 
  stat_function(fun = ~ 0.12*.x ^ 5.0, aes(colour = "r3")) +
  stat_function(fun = ~ 0.12*.x ^ 20.0, aes(colour = "r4")) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3", "r4"), 
                     values = c("#009E73", "#0072B2", "#D55E00", "#D55E80"),
                     labels =c(expression(paste(italic("r")[italic("S1")])),expression(paste(italic("r")[italic("S2")])),
                               expression(paste(italic("r")[italic("S3")])),
                               expression(paste(italic("r")[italic("S4")])))) +
  labs(x='DBH', y='m',title="Mortality rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_text(size = 8.5),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.17, .78),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.7, 'lines'),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1.2), breaks=seq(0,1.2,0.25)) +
  scale_y_continuous(limits=c(0,0.15), breaks=seq(0,0.15,0.05)) #+
#annotate("text", x = 1, y = 0.03, label = expression(paste("y = 0.1", x^1.5)),col="#009E73") +
#annotate("text", x = 1, y = 0.02, label = expression(paste("y = 0.1", x^2.3)),col="#0072B2") +
#annotate("text", x = 1, y = 0.01, label = expression(paste("y = 0.1", x^3.5)),col="#D55E00")
fig1a_dbh

# GR
fig1a_gr <- ggplot(data.frame(x = c(0, 20)), aes(x)) + 
  stat_function(fun = ~ 0.12/(1+(exp(-0.5*(.x-10)))), aes(colour = "r1")) +
  stat_function(fun = ~ 0.12/(1+(exp(-0.8*(.x-10)))), aes(colour = "r2")) +
  stat_function(fun = ~ 0.12/(1+(exp(-1.4*(.x-10)))), aes(colour = "r3")) +
  stat_function(fun = ~ 0.12/(1+(exp(-5*(.x-10)))), aes(colour = "r4")) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")])))) +
  labs(x='GR', y='m',title="Mortality rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_text(size = 8.5),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.17, .78),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.7, 'lines'),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,20), breaks=seq(0,20,5)) +
  scale_y_continuous(limits=c(0,0.13),breaks=seq(0,0.15,0.05))
fig1a_gr

# Stand develop: Stand biomass vs. time ####

# DBH
fig1b_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#009E73", alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#0072B2",alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E00",alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#009E73",alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#0072B2",alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E00",alpha=.7) +
  geom_line(data=ea2sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E80",alpha=.7) +
  geom_line(data=ea3sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E80",alpha=.7) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "B",title="Total biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_text(size = 8.5),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.20, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.8, 'lines'),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1b_dbh

# Changes in biomass
B_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea1p1
B_DBH_ea2p1
B_DBH_ea3p1
B_DBH_ea2p1/B_DBH_ea1p1
B_DBH_ea3p1/B_DBH_ea1p1

B_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p2/B_DBH_ea1p2
B_DBH_ea3p2/B_DBH_ea1p2

B_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p3/B_DBH_ea1p3
B_DBH_ea3p3/B_DBH_ea1p3

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
  labs(x = "t", y = "B",title="Total biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1b_gr

# Changes in biomass
B_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2/B_GR_ea1p2
B_GR_ea3p2/B_GR_ea1p2

B_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2/B_GR_ea1p2
B_GR_ea3p2/B_GR_ea1p2

B_GR_ea1p3 <- ea1sa1GRp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea2p3 <- ea2sa1GRp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea3p3 <- ea3sa1GRp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(plantC))
B_GR_ea2p3/B_GR_ea1p3
B_GR_ea3p3/B_GR_ea1p3

# Growth (NPP) ####

# DBH
fig1c_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+30%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp4gl_out_annual_tile, aes(x=year, y=NPP, color='x4', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp4gl_out_annual_tile, aes(x=year, y=NPP, color='x4', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid")) +
  labs(x = "t", y = "G",title="Net Primary Productivity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))
fig1c_dbh

# Changes in growth
G_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p1/G_DBH_ea1p1
G_DBH_ea3p1/G_DBH_ea1p1

G_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p2/G_DBH_ea1p2
G_DBH_ea3p2/G_DBH_ea1p2

G_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p3/G_DBH_ea1p3
G_DBH_ea3p3/G_DBH_ea1p3

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
  labs(x = "t", y = "G",title="Net Primary Productivity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))
fig1c_gr

# Changes in growth
G_GR_ea1p1 <- ea1sa1GRp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea2p1 <- ea2sa1GRp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea3p1 <- ea3sa1GRp1gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea2p1/G_GR_ea1p1
G_GR_ea3p1/G_GR_ea1p1

G_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea2p2/G_GR_ea1p2
G_GR_ea3p2/G_GR_ea1p2

G_GR_ea1p3 <- ea1sa1GRp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea2p3 <- ea2sa1GRp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea3p3 <- ea3sa1GRp3gl_out_annual_tile %>% filter(year>=700) %>% summarise(meanB=mean(NPP))
G_GR_ea2p3/G_GR_ea1p3
G_GR_ea3p3/G_GR_ea1p3

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
  labs(x = "t", y = "M",title="Tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
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
  labs(x = "t", y = "M",title="Tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
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

# DBH p4
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp4gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp4gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp4gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig1e_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#009E73",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp4gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#D55E80",size=3) + 
  geom_point(data=DBHp4gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#D55E80",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_text(size = 8.5),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.20, .8),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.9, 'lines'),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
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

# GR p4
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp4gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp4gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp4gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

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
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +  
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

# DBH p4
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp4gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp4gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp4gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig1f_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x3', shape='0-30%'),size=3) +
  geom_point(data=DBHp4gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x4', shape='0-15%'),size=3) + 
  geom_point(data=DBHp4gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x4', shape='0-30%'),size=3) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3", "x4"), 
                     values = c("#009E73", "#0072B2", "#D55E00","#D55E80")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dM, M)),title = "Changes in tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +  
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
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dM, M)),title = "Changes in tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +  
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

# DBH 4
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp4gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp4gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp4gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp4gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig1g_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x3', shape='0-30%'),size=3) +
  #geom_point(data=DBHp4gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x4', shape='0-15%'),size=3) + 
  #geom_point(data=DBHp4gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x4', shape='0-30%'),size=3) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)), title = "Changes in C turnover rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
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
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)),title = "Changes in C turnover rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(0,0.16), breaks = seq(0,0.15,0.05)) + 
  geom_hline(yintercept =  0.0, linetype="dashed")

fig1g_gr

# Carbon Turnover time vs. time ####
# Plot c_turnover_time or plantC/NPP

# DBH
fig1gg_dbh <- ggplot() +  
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='Control'),alpha=.7) + 
  geom_line(data=ea1sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='Control'),alpha=.7) +
  geom_line(data=ea1sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='Control'),alpha=.7) +
  #geom_line(data=ea1sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r4', linetype='Control'),alpha=.7) +
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+15%'),alpha=.7) +
  #geom_line(data=ea2sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r4', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+30%'),alpha=.7) +
  #geom_line(data=ea3sa1DBHp4gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r4', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("S1")])),expression(paste(italic("r")[italic("S2")])),
                               expression(paste(italic("r")[italic("S3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Year", y = expression(paste(tau)), 
       title = "Carbon turnover time with size-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.28, .93),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(1.2, 'lines'),
                     legend.spacing = unit(.1,"cm"),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(10,35), breaks = seq(10,35,10))
fig1gg_dbh

# GR
fig1gg_gr <- ggplot() + 
  geom_line(data=ea1sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='Control'),alpha=.7) + 
  geom_line(data=ea1sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='Control'),alpha=.7) +
  geom_line(data=ea1sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='Control'),alpha=.7) +
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Year", y = expression(paste(tau)), 
       title = "Carbon turnover time with size-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.28, .93),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(1.2, 'lines'),
                     legend.spacing = unit(.1,"cm"),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(10,35), breaks = seq(10,35,10))
fig1gg_gr

# Relative longevity vs. growth rates ####
# longevity is maximum age at tile-level, plot mean across multiple years after spinup

# DBH
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

#L30 <- ea3sa1DBHp1gl_out_annual_cohorts %>% dplyr::filter(year>=1500) %>% summarise(L30=max(age))
#L15 <- ea2sa1DBHp1gl_out_annual_cohorts %>% dplyr::filter(year>=1500) %>% summarise(L15=max(age))
#L0  <- ea1sa1DBHp1gl_out_annual_cohorts%>% dplyr::filter(year>=1500)  %>% summarise(L0=max(age))
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

L30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

#L30 <- ea3sa1DBHp2gl_out_annual_cohorts %>% dplyr::filter(year>=1500) %>% summarise(L30=max(age))
#L15 <- ea2sa1DBHp2gl_out_annual_cohorts %>% dplyr::filter(year>=1500) %>% summarise(L15=max(age))
#L0  <- ea1sa1DBHp2gl_out_annual_cohorts%>% dplyr::filter(year>=1500)  %>% summarise(L0=max(age))
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

L30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=1300&year<=1500) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

#L30 <- ea3sa3DBHp1gl_out_annual_cohorts %>% dplyr::filter(year>=1500) %>% summarise(L30=max(age))
#L15 <- ea2sa3DBHp1gl_out_annual_cohorts %>% dplyr::filter(year>=1500) %>% summarise(L15=max(age))
#L0  <- ea1sa3DBHp1gl_out_annual_cohorts%>% dplyr::filter(year>=1500)  %>% summarise(L0=max(age))
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
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dL, L)),title = "Changes in tree longevity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = seq(-0.25,0,0.05)) +
  geom_hline(yintercept =  0.0, linetype="dashed")
fig1h_dbh

# DBH p1
ea1sa1DBHp1gl_out_annual_tile <- ea1sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 
ea2sa1DBHp1gl_out_annual_tile <- ea2sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 
ea3sa1DBHp1gl_out_annual_tile <- ea3sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 

# DBH p2
ea1sa1DBHp2gl_out_annual_tile <- ea1sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 
ea2sa1DBHp2gl_out_annual_tile <- ea2sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 
ea3sa1DBHp2gl_out_annual_tile <- ea3sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 

# DBH p3
ea1sa1DBHp3gl_out_annual_tile <- ea1sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 
ea2sa1DBHp3gl_out_annual_tile <- ea2sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 
ea3sa1DBHp3gl_out_annual_tile <- ea3sa1DBHp1gl_out_annual_tile %>% mutate(MaxDBH_MaxAge = MaxDBH/MaxAge) 

# GR
# GR p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=700&year<=1500) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L0=max(MaxAge))
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

L30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L0=max(MaxAge))
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

L30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=1100&year<=1500) %>% summarise(L0=max(MaxAge))
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
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dL, L)),title = "Changes in tree longevity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = seq(-0.25,0,0.05)) +
  geom_hline(yintercept =  0.0, linetype="dashed")
fig1h_gr

# Distribution of tree sizes ####

load("~/forest_Swiss/data/aggData_preds.RData")
figdistr_data <- ggplot() +  
  #geom_smooth(data=aggData_preds, aes(x=DBH, y=log(Density)),se=F,col="blue",size=.5) + 
  geom_smooth(data=aggData_preds, aes(x=DBH, y=predict0, linetype='Control'),se=F,col="grey",size=.5) + 
  geom_smooth(data=aggData_preds, aes(x=DBH, y=predict15, linetype='+15%'),se=F,col="grey",size=.5) +
  geom_smooth(data=aggData_preds, aes(x=DBH, y=predict30, linetype='+30%'),se=F,col="grey",size=.5) +
  scale_linetype_manual("Growth enhacement", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.3))) +
  labs(x = "Diameter (cm)", y = "Ln N", title = "Size distributions from growth enhancement predictions") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.65, .90),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(1.2, 'lines'),
                     legend.spacing = unit(.1,"cm"),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  coord_cartesian(xlim = c(10,95),ylim = c(0,8)) +
  scale_y_continuous(breaks = seq(0,8,2)) + 
  scale_x_continuous(breaks = seq(10,100,20)) 
figdistr_data

# DBH
figdistr_dbh <- ggplot() +  
  geom_smooth(data=ea1sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='Control'),se=F,size=.5) + 
  geom_smooth(data=ea1sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea1sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='Control'),se=F,size=.5) +
  #geom_smooth(data=ea1sa1DBHp4gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r4', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea2sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+15%'),se=F,size=.5) + 
  geom_smooth(data=ea2sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea2sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+15%'),se=F,size=.5) +
  #geom_smooth(data=ea2sa1DBHp4gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r4', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+30%'),se=F,size=.5) + 
  geom_smooth(data=ea3sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+30%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+30%'),se=F,size=.5) +
  #geom_smooth(data=ea3sa1DBHp4gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r4', linetype='+30%'),se=F,size=.5) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3", "r4"), 
                     values = c("#009E73", "#0072B2", "#D55E00", "#E69F00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("S1")])),expression(paste(italic("r")[italic("S2")])),
                               expression(paste(italic("r")[italic("S3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Diameter (cm)", y = "Ln N", title = "Size distributions with size-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.28, .93),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(1.2, 'lines'),
                     legend.spacing = unit(.1,"cm"),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  coord_cartesian(xlim = c(25,150),ylim = c(0,6)) +
  scale_y_continuous(breaks = seq(0,6,2)) + 
  scale_x_continuous(breaks = seq(25,150,25)) 
figdistr_dbh

# GR
figdistr_gr <- ggplot() +  
  geom_smooth(data=ea1sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='Control'),se=F,size=.5) + 
  geom_smooth(data=ea1sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea1sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='Control'),se=F,size=.5) +
  #geom_smooth(data=ea1sa1GRp4gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r4', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea2sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+15%'),se=F,size=.5) + 
  geom_smooth(data=ea2sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea2sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+15%'),se=F,size=.5) +
  #geom_smooth(data=ea2sa1GRp4gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r4', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+30%'),se=F,size=.5) + 
  geom_smooth(data=ea3sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+30%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+30%'),se=F,size=.5) +
  #geom_smooth(data=ea3sa1GRp4gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r4', linetype='+30%'),se=F,size=.5) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3", "r4"), 
                     values = c("#009E73", "#0072B2", "#D55E00", "#E69F00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Diameter (cm)", y = "Ln N", title = "Size distributions with growth rate-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.28, .93),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(1.2, 'lines'),
                     legend.spacing = unit(.1,"cm"),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  coord_cartesian(xlim = c(25,300),ylim = c(0,6)) +
  scale_y_continuous(breaks = seq(0,6,2)) + 
  scale_x_continuous(breaks = seq(25,300,25)) 
figdistr_gr

# Self-thinning relationship  ####
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

# DBH4
fig2d_dbh <- ggplot() + 
  geom_point(data= subset(ea1sa1DBHp4gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="red") + 
  geom_point(data=subset(ea2sa1DBHp4gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="blue") + 
  geom_point(data=subset(ea3sa1DBHp4gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12)),alpha=.5,size=1,col="green") + 
  geom_smooth(data= subset(ea1sa1DBHp4gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='Control'),col="#D55E00",
              method = "lm",fullrange = T) + 
  geom_smooth(data= subset(ea2sa1DBHp4gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+15%'),col="#D55E00",
              method = "lm",fullrange = T) +
  geom_smooth(data= subset(ea3sa1DBHp4gl_out_annual_tile, year>=750), aes(x=log(QMD), y=log(Density12), linetype='+30%'),col="#D55E00",
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
fig2d_dbh


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

data_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
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
mean(data_DBH_ea1p1Den$NPP_kg_ha_year)

ea2sa1DBHp1gl_out_annual_tile <- ea2sa1DBHp1gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1DBHp1gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1DBHp1gl_out_annual_tile$NPP * 10000

data_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea2p1$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea2p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p1 <- data_DBH_ea2p1 %>% left_join(quantileX)
data_DBH_ea2p1Den <- data_DBH_ea2p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p1Rest <- data_DBH_ea2p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)
mean(data_DBH_ea2p1Den$NPP_kg_ha_year)

ea3sa1DBHp1gl_out_annual_tile <- ea3sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1DBHp1gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1DBHp1gl_out_annual_tile$NPP * 10000

data_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea3p1$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea3p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p1 <- data_DBH_ea3p1 %>% left_join(quantileX)
data_DBH_ea3p1Den <- data_DBH_ea3p1 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p1Rest <- data_DBH_ea3p1 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)
mean(data_DBH_ea3p1Den$NPP_kg_ha_year)

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

hist(data_DBH_p1Den$NPP_kg_ha_year)

### STL model
#### as LUE change
data_DBH_p1Den <- data_DBH_p1Den %>% mutate(LUE=as.factor(LUE))
data_DBH_p1Den$LUE <- relevel(data_DBH_p1Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p1Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p1Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)

Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p1Den, na.action = "na.exclude")
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDNoInter))
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
           terms = c("logQMD","LUE"))
fig2aLUE_dbh_a <- plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
                           terms = c("logQMD","LUE"),title = "",
                           axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                           colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2aLUE_dbh_a

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH1 <- as.data.frame(pred)

fig2aLUE_dbh1 <- ggplot() + 
  geom_point(data = data_DBH_p1Rest, aes(x = logQMD, y = logDensity12,col=LUE), alpha=0.2, size = 1 ,inherit.aes = FALSE) +
  geom_point(data = data_DBH_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="black", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH1, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size-dependent mortality - ", italic("r")[italic("S1")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", 
                        breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10),axis.title = element_text(size = 10),
          legend.text = element_text(size = 9),legend.title = element_text(size = 9),
          plot.title = element_text(size = 10),
          legend.key = element_rect(fill = NA, color = NA),
          legend.position = c(.15, .19),
          legend.direction="vertical",
          legend.margin = margin(2, 2, 2, 2),
          legend.box.background = element_rect(color="black",size=0.2),
          legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5))
fig2aLUE_dbh1

hist(data_DBH_p1Den$NPP_kg_ha_year)
hist_DBH_p1Den <- ggplot(data_DBH_p1Den, aes(x=NPP_kg_ha_year)) + geom_histogram(color="darkgrey", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       axis.text = element_text(size = 8),axis.title = element_text(size = 8)) + ggtitle("") +
  scale_x_continuous("NPP_kg_ha_year", limits=c(14000,20500), breaks = c(14500,17250,20000)) +
  scale_y_continuous("Frequency")
hist_DBH_p1Den

# DBH2 ####
ea1sa1DBHp2gl_out_annual_tile <- ea1sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1DBHp2gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1DBHp2gl_out_annual_tile$NPP * 10000

data_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
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
mean(data_DBH_ea1p2Den$NPP_kg_ha_year)

ea2sa1DBHp2gl_out_annual_tile <- ea2sa1DBHp2gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1DBHp2gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1DBHp2gl_out_annual_tile$NPP * 10000

data_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea2p2$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea2p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p2 <- data_DBH_ea2p2 %>% left_join(quantileX)
data_DBH_ea2p2Den <- data_DBH_ea2p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p2Rest <- data_DBH_ea2p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)
mean(data_DBH_ea2p2Den$NPP_kg_ha_year)

ea3sa1DBHp2gl_out_annual_tile <- ea3sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1DBHp2gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1DBHp2gl_out_annual_tile$NPP * 10000

data_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea3p2$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea3p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p2 <- data_DBH_ea3p2 %>% left_join(quantileX)
data_DBH_ea3p2Den <- data_DBH_ea3p2 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p2Rest <- data_DBH_ea3p2 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)
mean(data_DBH_ea3p2Den$NPP_kg_ha_year)

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
data_DBH_p2Den <- data_DBH_p2Den %>% mutate(LUE=as.factor(LUE))
data_DBH_p2Den$LUE <- relevel(data_DBH_p2Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
hist(data_DBH_p2Den$NPP)
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
           terms = c("logQMD","LUE"))
fig2bLUE_dbh_a <- plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2bLUE_dbh_a

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH2 <- as.data.frame(pred)

fig2bLUE_dbh2 <- ggplot() + 
  geom_point(data = data_DBH_p2Rest, aes(x = logQMD, y = logDensity12,col=LUE), alpha=0.2, size = 1, inherit.aes = FALSE) +
  geom_point(data = data_DBH_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="black", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH2, aes(x=x, y=predicted, linetype=group),col="#0072B2",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size-dependent mortality - ", italic("r")[italic("S2")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") + 
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5))
fig2bLUE_dbh2

# DBH3 ####
ea1sa1DBHp3gl_out_annual_tile <- ea1sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1DBHp3gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1DBHp3gl_out_annual_tile$NPP * 10000

data_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
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

data_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
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

data_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
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
data_DBH_p3Den <- data_DBH_p3Den %>% mutate(LUE=as.factor(LUE))
data_DBH_p3Den$LUE <- relevel(data_DBH_p3Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
fig2cLUE_dbh_a <- plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p3Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cLUE_dbh_a

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH3 <- as.data.frame(pred)

fig2cLUE_dbh3 <- ggplot() + 
  geom_point(data = data_DBH_p3Rest, aes(x = logQMD, y = logDensity12,col=LUE), alpha=0.2, size = 1, inherit.aes = FALSE) +
  geom_point(data = data_DBH_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="black", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH3, aes(x=x, y=predicted, linetype=group),col="#D55E00",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size-dependent mortality - ", italic("r")[italic("S3")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = "none",
                      legend.direction="vertical",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.background = element_rect(color="black",size=0.2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") + 
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5))
fig2cLUE_dbh3

# DBH4 ####
ea1sa1DBHp4gl_out_annual_tile <- ea1sa1DBHp4gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1DBHp4gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1DBHp4gl_out_annual_tile$NPP * 10000

data_DBH_ea1p4 <- ea1sa1DBHp4gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea1p4$QMD_bins))
max(data_DBH_ea1p4$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea1p4 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p4 <- data_DBH_ea1p4 %>% left_join(quantileX)
data_DBH_ea1p4Den <- data_DBH_ea1p4 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea1p4Rest <- data_DBH_ea1p4 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1DBHp4gl_out_annual_tile <- ea2sa1DBHp4gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1DBHp4gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1DBHp4gl_out_annual_tile$NPP * 10000

data_DBH_ea2p4 <- ea2sa1DBHp4gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea2p4$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea2p4 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p4 <- data_DBH_ea2p4 %>% left_join(quantileX)
data_DBH_ea2p4Den <- data_DBH_ea2p4 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea2p4Rest <- data_DBH_ea2p4 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1DBHp4gl_out_annual_tile <- ea3sa1DBHp4gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1DBHp4gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1DBHp4gl_out_annual_tile$NPP * 10000

data_DBH_ea3p4 <- ea3sa1DBHp4gl_out_annual_tile %>% filter(year>=700) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_DBH_ea3p4$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_DBH_ea3p4 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p4 <- data_DBH_ea3p4 %>% left_join(quantileX)
data_DBH_ea3p4Den <- data_DBH_ea3p4 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_DBH_ea3p4Rest <- data_DBH_ea3p4 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_DBH_p4 <- rbind(data_DBH_ea1p4, data_DBH_ea2p4, data_DBH_ea3p4) 
data_DBH_p4Den <- rbind(data_DBH_ea1p4Den, data_DBH_ea2p4Den, data_DBH_ea3p4Den) 
data_DBH_p4Rest <- rbind(data_DBH_ea1p4Rest, data_DBH_ea2p4Rest, data_DBH_ea3p4Rest) 

data_DBH_p4Den <- data_DBH_p4Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_DBH_p4Rest <- data_DBH_p4Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_DBH_p4, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p4Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_DBH_p4Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_DBH_p4Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
data_DBH_p4Den <- data_DBH_p4Den %>% mutate(LUE=as.factor(LUE))
data_DBH_p4Den$LUE <- relevel(data_DBH_p4Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p4Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p4Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDNoInter))
fig2cLUE_dbh_a <- plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
                             terms = c("logQMD","LUE"),title = "",
                             axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                             colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_DBH_p4Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cLUE_dbh_a

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH4 <- as.data.frame(pred)

fig2cLUE_dbh4 <- ggplot() + 
  geom_point(data = data_DBH_p4Rest, aes(x = logQMD, y = logDensity12,col=LUE), alpha=0.2, size = 1, inherit.aes = FALSE) +
  geom_point(data = data_DBH_p4Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="black", inherit.aes = FALSE) +
  geom_smooth(data = preddataLUE_DBH4, aes(x=x, y=predicted, linetype=group),col="#D55E00",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size-dependent mortality - ", italic("r")[italic("S4")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") + 
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5))
fig2cLUE_dbh4

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
data_GR_p1Den <- data_GR_p1Den %>% mutate(LUE=as.factor(LUE))
data_GR_p1Den$LUE <- relevel(data_GR_p1Den$LUE, ref = "Control")
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

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR1 <- as.data.frame(pred)

fig2aLUE_gr1 <- ggplot() + 
  geom_point(data = data_GR_p1Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR1, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate-dependent mortality - ", italic("r")[italic("GR1")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") + 
scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig2aLUE_gr1

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
data_GR_p2Den <- data_GR_p2Den %>% mutate(LUE=as.factor(LUE))
data_GR_p2Den$LUE <- relevel(data_GR_p2Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_GR_p2Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p2Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
           terms = c("logQMD","LUE"))
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

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR2 <- as.data.frame(pred)

fig2bLUE_gr2 <- ggplot() + 
  geom_point(data = data_GR_p2Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR2, aes(x=x, y=predicted, linetype=group),col="#0072B2",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate-dependent mortality - ", italic("r")[italic("GR2")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") + 
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig2bLUE_gr2

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
data_GR_p3Den <- data_GR_p3Den %>% mutate(LUE=as.factor(LUE))
data_GR_p3Den$LUE <- relevel(data_GR_p3Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_GR_p3Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p3Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
           terms = c("logQMD","LUE"))
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

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR3 <- as.data.frame(pred)

fig2cLUE_gr3 <- ggplot() + 
  geom_point(data = data_GR_p3Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR3, aes(x=x, y=predicted, linetype=group),col="#D55E00",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate-dependent mortality - ", italic("r")[italic("GR3")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") +  
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig2cLUE_gr3

# GR4 ####
ea1sa1GRp4gl_out_annual_tile <- ea1sa1GRp4gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "Control")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea1sa1GRp4gl_out_annual_tile$NPP_kg_ha_year <- ea1sa1GRp4gl_out_annual_tile$NPP * 10000

data_GR_ea1p4 <- ea1sa1GRp4gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea1p4$QMD_bins))
max(data_GR_ea1p4$NPP_kg_ha_year)

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea1p4 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p4 <- data_GR_ea1p4 %>% left_join(quantileX)
data_GR_ea1p4Den <- data_GR_ea1p4 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea1p4Rest <- data_GR_ea1p4 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea2sa1GRp4gl_out_annual_tile <- ea2sa1GRp4gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "+15%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea2sa1GRp4gl_out_annual_tile$NPP_kg_ha_year <- ea2sa1GRp4gl_out_annual_tile$NPP * 10000

data_GR_ea2p4 <- ea2sa1GRp4gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea2p4$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea2p4 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p4 <- data_GR_ea2p4 %>% left_join(quantileX)
data_GR_ea2p4Den <- data_GR_ea2p4 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea2p4Rest <- data_GR_ea2p4 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

ea3sa1GRp4gl_out_annual_tile <- ea3sa1GRp4gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "+30%")

# Convert NPP from KgC/m2/yr to KgC/ha/yr
ea3sa1GRp4gl_out_annual_tile$NPP_kg_ha_year <- ea3sa1GRp4gl_out_annual_tile$NPP * 10000

data_GR_ea3p4 <- ea3sa1GRp4gl_out_annual_tile %>% filter(year>=750) %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(data_GR_ea3p4$QMD_bins))

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- data_GR_ea3p4 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
max(quantileX$`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p4 <- data_GR_ea3p4 %>% left_join(quantileX)
data_GR_ea3p4Den <- data_GR_ea3p4 %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
data_GR_ea3p4Rest <- data_GR_ea3p4 %>% filter(Density12<`quantile(Density12, c(valueQuantile))`)

data_GR_p4 <- rbind(data_GR_ea1p4, data_GR_ea2p4, data_GR_ea3p4) 
data_GR_p4Den <- rbind(data_GR_ea1p4Den, data_GR_ea2p4Den, data_GR_ea3p4Den) 
data_GR_p4Rest <- rbind(data_GR_ea1p4Rest, data_GR_ea2p4Rest, data_GR_ea3p4Rest) 

data_GR_p4Den <- data_GR_p4Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))
data_GR_p4Rest <- data_GR_p4Rest %>% mutate(logDensity12=log(Density12),logQMD=log(QMD))

ggplot() + 
  geom_point(data = data_GR_p4, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p4Den, aes(x = log(QMD), y = log(Density12)), color="red") 

FitRes = lm(log(NPP_kg_ha_year) ~ QMD, data = data_GR_p4Den, na.action = "na.exclude")
summary(FitRes)
Res_NPP <- residuals(FitRes)
Fitted_NPP <- fitted(FitRes)
data_GR_p4Den$Res_NPP <- Res_NPP

### STL model
#### as LUE change
data_GR_p4Den <- data_GR_p4Den %>% mutate(LUE=as.factor(LUE))
data_GR_p4Den$LUE <- relevel(data_GR_p4Den$LUE, ref = "Control")
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_GR_p4Den, na.action = "na.exclude")
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p4Den, na.action = "na.exclude")
AICc(Fit_QMDInter,Fit_QMDNoInter)
summary(Fit_QMDInter)
summary(Fit_QMDNoInter)
plot(allEffects(Fit_QMDInter))
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,
           terms = c("logQMD","LUE"))
fig2cLUE_gr_a <- plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,
                            terms = c("logQMD","LUE"),title = "",
                            axis.title = c("Quadratic Mean Diameter (QMD)","Stand density (N)"),legend.title = "Level of LUE",
                            colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  #geom_point(data = data_GR_p4Rest, aes(x = logQMD, y = logDensity12), alpha=0.2, size = 1,shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) 
fig2cLUE_gr_a

pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR4 <- as.data.frame(pred)

fig2cLUE_gr4 <- ggplot() + 
  geom_point(data = data_GR_p4Den, aes(x = logQMD, y = logDensity12), alpha=0.3, size = 1,col="darkgrey", inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR4, aes(x=x, y=predicted, linetype=group),col="#D55E00",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate-dependent mortality - ", italic("r")[italic("GR3")])),
       color  = "Level of LUE", linetype = "Level of LUE") + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") +  
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig2cLUE_gr4

# 3) Link model and observations ####

# DBH ####
ea1sa1DBHp1gl_out_annual_tile <- ea1sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "Control")
ea2sa1DBHp1gl_out_annual_tile <- ea2sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+15%")
ea3sa1DBHp1gl_out_annual_tile <- ea3sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+30%")
data_DBH_p1 <- bind_rows(ea1sa1DBHp1gl_out_annual_tile, ea2sa1DBHp1gl_out_annual_tile, ea3sa1DBHp1gl_out_annual_tile) %>% 
  mutate(logDensity12=log(Density12),logQMD=log(QMD)) %>% mutate(LUE=as.factor(LUE)) %>% filter(year>=750)
data_DBH_p1$LUE <- relevel(data_DBH_p1$LUE, ref = "Control")
ggplot() + 
  geom_point(data = data_DBH_p1, aes(x = log(QMD), y = log(Density12)), color="blue") 

### STL model all data
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p1, na.action = "na.exclude")
summary(Fit_QMDInter)
plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p1, na.action = "na.exclude")
summary(Fit_QMDNoInter)
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
AICc(Fit_QMDInter,Fit_QMDNoInter)
pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p1_inter <- as.data.frame(pred)
pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p1_Nointer <- as.data.frame(pred)

# Upward shift of the STL in the model
pred_data_DBH_p1_Nointer
pred_data_DBH_p1_inter
preddataLUE_DBH1
pred_data_DBH_p1 <- preddataLUE_DBH1 %>% mutate(e_predicted=exp(predicted))
preddataLUE_DBH1_agg <- pred_data_DBH_p1 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_DBH1_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH1_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp1gl_RelChange_B_NPP_0_30 <- DBHp1gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp1gl_RelChange_B_NPP_0_15 <- DBHp1gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# DBH2
ea1sa1DBHp2gl_out_annual_tile <- ea1sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "Control")
ea2sa1DBHp2gl_out_annual_tile <- ea2sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+15%")
ea3sa1DBHp2gl_out_annual_tile <- ea3sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+30%")
data_DBH_p2 <- bind_rows(ea1sa1DBHp2gl_out_annual_tile, ea2sa1DBHp2gl_out_annual_tile, ea3sa1DBHp2gl_out_annual_tile) %>% 
  mutate(logDensity12=log(Density12),logQMD=log(QMD)) %>% mutate(LUE=as.factor(LUE)) %>% filter(year>=750)
data_DBH_p2$LUE <- relevel(data_DBH_p2$LUE, ref = "Control")
ggplot() + 
  geom_point(data = data_DBH_p2, aes(x = log(QMD), y = log(Density12)), color="blue") 

### STL model all data
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p2, na.action = "na.exclude")
summary(Fit_QMDInter)
plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p2, na.action = "na.exclude")
summary(Fit_QMDNoInter)
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
AICc(Fit_QMDInter,Fit_QMDNoInter)
pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p2_inter <- as.data.frame(pred)
pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p2_Nointer <- as.data.frame(pred)

# Upward shift of the STL in the model
pred_data_DBH_p2_Nointer
pred_data_DBH_p2_inter
preddataLUE_DBH2
pred_data_DBH_p2 <- preddataLUE_DBH2 %>% mutate(e_predicted=exp(predicted))
preddataLUE_DBH2_agg <- pred_data_DBH_p2 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_DBH2_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH2_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp2gl_RelChange_B_NPP_0_30 <- DBHp2gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp2gl_RelChange_B_NPP_0_15 <- DBHp2gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# DBH3
ea1sa1DBHp3gl_out_annual_tile <- ea1sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "Control")
ea2sa1DBHp3gl_out_annual_tile <- ea2sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+15%")
ea3sa1DBHp3gl_out_annual_tile <- ea3sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+30%")
data_DBH_p3 <- bind_rows(ea1sa1DBHp3gl_out_annual_tile, ea2sa1DBHp3gl_out_annual_tile, ea3sa1DBHp3gl_out_annual_tile) %>% 
  mutate(logDensity12=log(Density12),logQMD=log(QMD)) %>% mutate(LUE=as.factor(LUE)) %>% filter(year>=750)
data_DBH_p3$LUE <- relevel(data_DBH_p3$LUE, ref = "Control")
ggplot() + 
  geom_point(data = data_DBH_p3, aes(x = log(QMD), y = log(Density12)), color="blue") 

### STL model all data
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p3, na.action = "na.exclude")
summary(Fit_QMDInter)
plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p3, na.action = "na.exclude")
summary(Fit_QMDNoInter)
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
AICc(Fit_QMDInter,Fit_QMDNoInter)
pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p3_inter <- as.data.frame(pred)
pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p3_Nointer <- as.data.frame(pred)

# Upward shift of the STL in the model
pred_data_DBH_p3_Nointer
pred_data_DBH_p3_inter
preddataLUE_DBH3
pred_data_DBH_p3 <- preddataLUE_DBH3 %>% mutate(e_predicted=exp(predicted))
preddataLUE_DBH3_agg <- pred_data_DBH_p3 %>% group_by(x) %>%  
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_DBH3_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH3_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp3gl_RelChange_B_NPP_0_30 <- DBHp3gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp3gl_RelChange_B_NPP_0_15 <- DBHp3gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# DBH4
ea1sa1DBHp4gl_out_annual_tile <- ea1sa1DBHp4gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "Control")
ea2sa1DBHp4gl_out_annual_tile <- ea2sa1DBHp4gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "+15%")
ea3sa1DBHp4gl_out_annual_tile <- ea3sa1DBHp4gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p4") %>% 
  mutate(LUE = "+30%")
data_DBH_p4 <- bind_rows(ea1sa1DBHp4gl_out_annual_tile, ea2sa1DBHp4gl_out_annual_tile, ea3sa1DBHp4gl_out_annual_tile) %>% 
  mutate(logDensity12=log(Density12),logQMD=log(QMD)) %>% mutate(LUE=as.factor(LUE)) %>% filter(year>=500)
data_DBH_p4$LUE <- relevel(data_DBH_p4$LUE, ref = "Control")
ggplot() + 
  geom_point(data = data_DBH_p4, aes(x = log(QMD), y = log(Density12)), color="blue") 

### STL model all data
#### as LUE change
Fit_QMDInter = lm(logDensity12 ~ scale(logQMD) * LUE, data = data_DBH_p4, na.action = "na.exclude")
summary(Fit_QMDInter)
plot_model(Fit_QMDInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
Fit_QMDNoInter = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p4, na.action = "na.exclude")
summary(Fit_QMDNoInter)
plot_model(Fit_QMDNoInter, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))
AICc(Fit_QMDInter,Fit_QMDNoInter)
pred <- ggpredict(Fit_QMDInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p4_inter <- as.data.frame(pred)
pred <- ggpredict(Fit_QMDNoInter, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
pred_data_DBH_p4_Nointer <- as.data.frame(pred)

# Upward shift of the STL in the model
pred_data_DBH_p4_Nointer
pred_data_DBH_p4_inter
preddataLUE_DBH4
pred_data_DBH_p4 <- preddataLUE_DBH4 %>% mutate(e_predicted=exp(predicted))
preddataLUE_DBH4_agg <- pred_data_DBH_p4 %>% group_by(x) %>%  
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_DBH4_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH4_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp4gl_RelChange_B_NPP_0_30 <- DBHp4gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp4gl_RelChange_B_NPP_0_15 <- DBHp4gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# Upward shift of the STL from observations when increasing Growth 15 and 30%
load("~/forestNFI_Swiss/data/aggData_preds.RData")
aggData_QMDbinsDen
preddata_agg <- aggData_QMDbinsDen %>% 
  mutate(STL_15=(predict15-predict0)) %>%
  mutate(STL_30=(predict30-predict0)) %>%
  mutate(STL_15Rel=(e_predict15-e_predict0)/e_predict0) %>%
  mutate(STL_30Rel=(e_predict30-e_predict0)/e_predict0) %>%
  dplyr::select(STL_15,STL_30,STL_15Rel,STL_30Rel)

N30 <- preddata_agg %>%
  summarise(STL_30=mean(STL_30,na.rm=T)) %>% pull()

N15 <- preddata_agg %>%
  summarise(STL_15=mean(STL_15,na.rm=T)) %>% pull()

fig4_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r1', shape='+15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r1', shape='+30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r2', shape='+15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r2', shape='+30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r3', shape='+15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r3', shape='+30%'),size=3) + 
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("S1")])),expression(paste(italic("r")[italic("S2")])),
                               expression(paste(italic("r")[italic("S3")]))),
                    guide = guide_legend(override.aes = list(size=1.6),order = 1)) +
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color ="black",size=1.6))) +
  labs(x = expression(frac(dlnB, dlnG)), y = expression(frac(dN, N)),title = "Relative shifts in the STL for size-dependent mortality") + 
  theme_bw() + #guides(color = guide_legend(order = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_text(size = 8.5),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.20, .94),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.6, 'lines'),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0.5,0.8),breaks=seq(0.5,0.8,0.1)) + 
  scale_y_continuous(limits = c(-0.001,0.2),breaks=seq(0,0.2,0.05)) +
  geom_hline(yintercept = N15, linetype="dashed") +
  geom_hline(yintercept = N30, linetype="dashed") +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=N15,ymax=N30),fill="grey", alpha=0.2)
fig4_dbh

# GR ####
# GR1
# Upward shift of the STL in the model
preddataLUE_GR1
preddataLUE_GR1 <- preddataLUE_GR1 %>% mutate(e_predicted=exp(predicted))
preddataLUE_GR1_agg <- preddataLUE_GR1 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_GR1_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR1_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp1gl_RelChange_B_NPP_0_15 <- GRp1gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp1gl_RelChange_B_NPP_0_30 <- GRp1gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR2
# Upward shift of the STL in the model
preddataLUE_GR2
preddataLUE_GR2 <- preddataLUE_GR2 %>% mutate(e_predicted=exp(predicted))
preddataLUE_GR2_agg <- preddataLUE_GR2 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_GR2_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR2_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp2gl_RelChange_B_NPP_0_15 <- GRp2gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp2gl_RelChange_B_NPP_0_30 <- GRp2gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR3
# Upward shift of the STL in the model
preddataLUE_GR3
preddataLUE_GR3 <- preddataLUE_GR3 %>% mutate(e_predicted=exp(predicted))
preddataLUE_GR3_agg <- preddataLUE_GR3 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_GR3_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR3_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp3gl_RelChange_B_NPP_0_15 <- GRp3gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp3gl_RelChange_B_NPP_0_30 <- GRp3gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR4
# Upward shift of the STL in the model
preddataLUE_GR4
preddataLUE_GR4 <- preddataLUE_GR4 %>% mutate(e_predicted=exp(predicted))
preddataLUE_GR4_agg <- preddataLUE_GR4 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) %>%
  mutate(STL_15Rel=(e_predicted-lag(e_predicted))/lag(e_predicted)) %>%
  mutate(STL_30Rel=(e_predicted-lag(lag(e_predicted)))/lag(lag(e_predicted))) 

N30 <- preddataLUE_GR4_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR4_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp4gl_RelChange_B_NPP_0_15 <- GRp4gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp4gl_RelChange_B_NPP_0_30 <- GRp4gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# Upward shift of the STL from observations when increasing Growth 15 and 30%
load("~/forest_Swiss/data/aggData_preds.RData")
aggData_QMDbinsDen
preddata_agg <- aggData_QMDbinsDen %>% 
  mutate(STL_15=(predict15-predict0)) %>%
  mutate(STL_30=(predict30-predict0)) %>%
  mutate(STL_15Rel=(e_predict15-e_predict0)/e_predict0) %>%
  mutate(STL_30Rel=(e_predict30-e_predict0)/e_predict0) %>%
  dplyr::select(STL_15,STL_30,STL_15Rel,STL_30Rel)

N30 <- preddata_agg %>%
  summarise(STL_30=mean(STL_30,na.rm=T)) %>% pull()

N15 <- preddata_agg %>%
  summarise(STL_15=mean(STL_15,na.rm=T)) %>% pull()

fig4_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r1', shape='+15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r1', shape='+30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r2', shape='+15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r2', shape='+30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r3', shape='+15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r3', shape='+30%'),size=3) + 
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")]))),
                     guide = guide_legend(override.aes = list(size=1.6),order = 1)) +
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color ="black",size=1.6))) +
  labs(x = expression(frac(dlnB, dlnG)), y = expression(frac(dN, N)),title = "Relative shifts in the STL for growth rate mortality") + 
  theme_bw() + #guides(color = guide_legend(order = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_text(size = 8.5),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.20, .94),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.6, 'lines'),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0.85,0.905),breaks=seq(0.85,0.90,0.01)) + 
  scale_y_continuous(limits = c(-0.001,0.2),breaks=seq(0,0.2,0.05)) +
  geom_hline(yintercept = N15, linetype="dashed") +
  geom_hline(yintercept = N30, linetype="dashed") +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=N15,ymax=N30),fill="grey", alpha=0.2)
fig4_gr

# Figure 1 ####
ff1 <- fig1a_dbh + fig1b_dbh + fig1c_dbh + fig1d_dbh + fig1e_dbh + fig1f_dbh + fig1g_dbh + fig1h_dbh + 
  fig1a_gr + fig1b_gr + fig1c_gr + fig1d_gr + fig1e_gr + fig1f_gr + fig1g_gr + fig1h_gr + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A') #+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff1
ggsave("~/rsofun/manuscript/figures/fig_1.png", width = 12, height = 11, dpi=300)

# Figure 2 ####
ff2LUE <- fig2aLUE_dbh1 + fig2bLUE_dbh2 + fig2cLUE_dbh3 + fig2cLUE_dbh4
  fig2aLUE_gr1 + fig2bLUE_gr2 + fig2cLUE_gr3 + 
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'A') #+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff2LUE
ggsave("~/rsofun/manuscript/figures/fig_2.png", width = 12, height = 8, dpi=300)

# Figure 4 ####
ff4 <- fig4_dbh + fig4_gr + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') #+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff4
ggsave("~/rsofun/manuscript/figures/fig_4.png", width = 12, height = 6, dpi=300)

# Figure S1 ####
ffs1 <- figdistr_dbh + figdistr_gr +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') #+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffs1
ggsave("~/rsofun/manuscript/figures/fig_S1.png", width = 12, height = 6, dpi=300)

# Figure S2 ####
ffs2 <- fig1gg_dbh + fig1gg_gr + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') #+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffs2
ggsave("~/rsofun/manuscript/figures/fig_S2.png", width = 12, height = 6, dpi=300)
