
######## DBH function ######## 
# Understory 
# Weng et al 2015
ggplot(data.frame(x = c(0, 0.5)), aes(x)) + stat_function(fun = ~ 0.075*(1+9*exp(-60*.x))/(1+exp(-60*.x)))+
  labs(x='DBH (m)', y='Deathrate')

# Canopy
# Weng et al 2015
# See that maximum value of DBH expected is 1 m or 100 cm
ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = ~ 0.01*(1+5*exp(4*(.x-2)))/(1+exp(4*(.x-2)))) +
  labs(x='DBH (m)', y='Deathrate')


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




