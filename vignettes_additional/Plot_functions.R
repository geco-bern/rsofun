
######## DBH function ######## 
# Understory 
# Weng et al 2015
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.08*(1+9*exp(-60*.x))/(1+exp(-60*.x)))+
  labs(x='DBH (m)', y='Deathrate')
# Modified from Ensheng to reach 0! NOW IN FORTRAN!
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.08*(9*exp(-40*.x))/(1+exp(-40*.x)))+
  labs(x='DBH (m)', y='Deathrate') + theme_bw()
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ (0.72*exp(-40*.x))/(1+exp(-40*.x)))+
  labs(x='DBH (m)', y='Deathrate') + theme_bw()
# Sum this one to control infinite growing! NOW IN FORTRAN!
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 0.01*exp(0.6*.x))+
  labs(x='DBH (m)', y='Deathrate') + theme_bw()

# Canopy
# Weng et al 2015
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.02*(1+5*exp(4*.x-2))/(1+exp(4*.x-2)))+
  labs(x='DBH (m)', y='Deathrate')
# Modified from Ensheng to avoid asymptotes! NOW IN FORTRAN!
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 0.01*exp(0.6*.x))+
  labs(x='DBH (m)', y='Deathrate') + theme_bw()

######## Carbon starvation function ######## 
#  NOW IN FORTRAN!
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ (exp(-2.3*.x+7)/(1+exp(-1*.x+7))))+
  labs(x='NSC', y='Deathrate') +theme_bw()

ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-20*.x+7)/(1+exp(-20*.x+7))))+
  labs(x='NSC/leaf mass', y='Deathrate')+theme_bw()

######## Growth rate function ######## 
#  NOW IN FORTRAN!
ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = ~ 1*.x)+
  labs(x='Growth', y='Deathrate') + theme_bw()
ggplot(data.frame(x = c(0, 10)), aes(x)) + stat_function(fun = ~ 0.2*.x)+
  labs(x='Growth', y='Deathrate') + theme_bw()

ggplot(data.frame(x = c(0, .5)), aes(x)) + stat_function(fun = ~ 2*.x)+
  labs(x='dVol', y='Deathrate')
ggplot(data.frame(x = c(0, 12)), aes(x)) + stat_function(fun = ~ 0.08*.x)+
  labs(x='dDBH', y='Deathrate')
# Linear fc
ggplot(data.frame(x = c(0, 180)), aes(x)) + stat_function(fun = ~ 0.5*.x)+
  labs(x='Vol', y='Deathrate')
# Previous forms
ggplot(data.frame(x = c(0, 10)), aes(x)) + stat_function(fun = ~ 0.01*(4*exp(4*(.x)))/(1+exp(4*(.x))))+
  labs(x='Vol', y='Deathrate')
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.1/(1+exp((-5)*(.x-1))))+
  labs(x='DBH (m)', y='Deathrate')
ggplot(data.frame(x = c(0, 80)), aes(x)) + stat_function(fun = ~ 1/(1+exp((-.1)*(.x-40))))+
  labs(x='dVol', y='Deathrate')


