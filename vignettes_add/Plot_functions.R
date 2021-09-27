
######## DBH function ######## 
# Understory 
# Weng et al 2015

ggplot(data.frame(x = c(0, 0.5)), aes(x)) + stat_function(fun = ~ 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x))) +
  labs(x='DBH', y='Mortality rate') +theme_bw()

ggplot(data.frame(x = c(0, 0.5)), aes(x)) + 
  stat_function(fun = ~ 0.1*0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x))) +
  stat_function(fun = ~ 1* 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x)), col="blue") +
  stat_function(fun = ~ 2* 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x))) +
  labs(x='DBH (m)', y='Deathrate') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  labs(title= "Understory mortality as function of size")

# Canopy
# Weng et al 2015
# See that maximum value of DBH expected is 1 m or 100 cm

# Option 1 NOW in FORTRAN
ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = ~ 0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1)))) +
  labs(x='DBH', y='Mortality rate') +theme_bw()

ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = ~ .x ^ 1.5) +
  stat_function(fun = ~ .x ^ 2.5) +
  stat_function(fun = ~ .x ^ 5) +
  labs(x='DBH', y='Mortality rate') +theme_bw()

ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = ~ 1*0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1))),col="#F8766D") +
  stat_function(fun = ~ 0.1*0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1)))) +
  stat_function(fun = ~ 2*0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1)))) +
  labs(x='DBH (m)', y='Deathrate') 

# Option 2
ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = ~ 1*(1*exp(2*(.x-2)))/(1+exp(2*(.x-2)))) +
  labs(x='DBH', y='Mortality rate') +theme_bw()

ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = ~ 1.8*(1*exp(1*2*(.x-2)))/(1+exp(1*2*(.x-2))),col="#F8766D") +
  stat_function(fun = ~ 1.8*(1*exp(0.1*2*(.x-2)))/(1+exp(0.1*2*(.x-2)))) +
  stat_function(fun = ~ 1.8*(1*exp(2*2*(.x-2)))/(1+exp(2*2*(.x-2)))) +
  labs(x='DBH (m)', y='Deathrate') 

fig1a_dbh <- ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = ~ 0.1006894*0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1))),col="#F8766D") +
  stat_function(fun = ~ 0.1107583*0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1))),col="grey") +
  stat_function(fun = ~ 0.09062046*0.1*(1*exp(2*(.x-1)))/(1+exp(2*(.x-1))),col="grey") +
  labs(x='DBH (m)', y='Mortality rate (m)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  #labs(title= "a) Mortality as function of tree size") +
  labs(title= "a)")

# Shape of the curve
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = ~ 0.01*(1+5*exp(3*(.x-2)))/(1+exp(3*(.x-2)))) +
  stat_function(fun = ~ 0.01*(1+5*exp(4*(.x-2)))/(1+exp(4*(.x-2))),col="#F8766D") +
  stat_function(fun = ~ 0.01*(1+5*exp(5*(.x-2)))/(1+exp(5*(.x-2)))) +
  stat_function(fun = ~ 0.01*(1+5*exp(6*(.x-2)))/(1+exp(6*(.x-2)))) +
  stat_function(fun = ~ 0.01*(1+5*exp(7*(.x-2)))/(1+exp(7*(.x-2)))) +
  labs(x='DBH', y='Mortality rate') +theme_bw()

######## Carbon starvation function ######## 
# NOW IN FORTRAN!
# See that maximum value of cc%nsc/cc%bl_max expected is 5

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  stat_function(fun = ~ 0.05*(exp(-3*.x)/(0.01+exp(-3*.x)))) +
  stat_function(fun = ~ 0.05*(exp(-3*.x)/(0.02+exp(-3*.x)))) +
  stat_function(fun = ~ 0.05*(exp(-3*.x)/(0.03+exp(-3*.x))), col = "red") +
  stat_function(fun = ~ 0.05*(exp(-3*.x)/(0.1+exp(-3*.x))), col = "red") +
  labs(x='NSC', y='Mortality rate') +theme_bw()

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  stat_function(fun = ~ 1*0.05*(exp(-3*.x)/(0.01+exp(-3*.x))),col="#F8766D")+
  stat_function(fun = ~ 0.1*0.05*(exp(-3*.x)/(0.01+exp(-3*.x))))+
  stat_function(fun = ~ 2*0.05*(exp(-3*.x)/(0.01+exp(-3*.x))))+
  labs(x='DBH (m)', y='Deathrate') 

fig1a_nsc <- ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  stat_function(fun = ~ 1.4958614*0.05*(exp(-3*.x)/(0.01+exp(-3*.x))),col="#F8766D")+
  stat_function(fun = ~ 1.645448*0.05*(exp(-3*.x)/(0.01+exp(-3*.x))),col="grey")+
  stat_function(fun = ~ 1.346275*0.05*(exp(-3*.x)/(0.01+exp(-3*.x))),col="grey")+
  labs(x='nsc/bl_max', y='Mortality rate (m)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  labs(title= "a)")

######## Growth rate function ######## 
#  NOW IN FORTRAN!
# See that maximum value of cc%bsw+cc%bHW-cc%ABG_ys is 12

ggplot(data.frame(x = c(0, 12)), aes(x)) + stat_function(fun = ~ 0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6)))) +
  labs(x='GR', y='Mortality rate (m)') +theme_bw()

ggplot(data.frame(x = c(0, 12)), aes(x)) + 
  stat_function(fun = ~ 1*0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6))),col="#F8766D") +
  stat_function(fun = ~ 0.1*0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6)))) +
  stat_function(fun = ~ 2*0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6)))) +
  labs(x='DBH (m)', y='Deathrate') 

fig1a_gr <- ggplot(data.frame(x = c(0, 12)), aes(x)) + 
  stat_function(fun = ~ 0.1510497*0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6))),col="#F8766D") +
  stat_function(fun = ~ 0.1661547*0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6))),col="grey") +
  stat_function(fun = ~ 0.1359447*0.05*(1*exp(1*(.x-6)))/(1+exp(1*(.x-6))),col="grey") +
  labs(x='Biomass growth (Kg C/year)', y='Mortality rate (m)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  labs(title= "a)")

#ggplot(data.frame(x = c(0, 12)), aes(x)) + stat_function(fun = ~ 0.002*(1+5*exp(1*(.x-6)))/(1+exp(1*(.x-6)))) +
#  labs(x='GR', y='Mortality rate') +theme_bw()

