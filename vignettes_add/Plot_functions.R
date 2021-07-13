
######## DBH function ######## 
# Understory 
# Weng et al 2015
ggplot(data.frame(x = c(0, 0.5)), aes(x)) + 
  #stat_function(fun = ~ 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x)),col="blue") +
  stat_function(fun = ~ 0.81* 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x)), col="blue") +
  stat_function(fun = ~ 0.1* 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x))) +
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
ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  #stat_function(fun = ~ 0.01*(1+5*exp(4*(.x-2)))/(1+exp(4*(.x-2)))) +
  #stat_function(fun = ~ 0.17* 0.01*(1+5*exp(4*(.x-2)))/(1+exp(4*(.x-2))), col="blue") +
  #stat_function(fun = ~ 0.1* 0.01*(1+5*exp(4*(.x-2)))/(1+exp(4*(.x-2)))) +
  stat_function(fun = ~ 2* 0.01*(1+5*exp(4*(.x-2)))/(1+exp(4*(.x-2)))) +
  labs(x='DBH (m)', y='Deathrate') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom",
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 10),legend.title = element_text(size = 11),
                     plot.title = element_text(size = 11)) + 
  labs(title= "Canopy mortality as function of size")


######## Carbon starvation function ######## 
# NOW IN FORTRAN!
# See that maximum value of cc%nsc/cc%bl_max expected is 5

ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 0.01*(exp(-2*.x)/(0.01+exp(-2*.x))))+
  labs(x='NSC', y='Deathrate') +theme_bw()


######## Growth rate function ######## 
#  NOW IN FORTRAN!
# See that maximum value of cc%dbh-cc%DBH_ys is 0.002
# See that maximum value of cc%dbh-cc%DBH_ys is 0.002

ggplot(data.frame(x = c(0, 0.002)), aes(x)) + stat_function(fun = ~ 0.01*(2*exp(60*(.x)))/(1+exp(60*(.x)))) +
  labs(x='Growth', y='Deathrate')




