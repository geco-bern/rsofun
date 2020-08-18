
######## DBH function ######## 
# Understory 
# Weng et al 2015
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.08*(1+9*exp(-60*.x))/(1+exp(-60*.x)))+
  labs(x='DBH (m)', y='Deathrate')
# Modified from Ensheng to reach 0!
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.08*(9*exp(-40*.x))/(1+exp(-40*.x)))+
  labs(x='DBH (m)', y='Deathrate')
# Other options
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.04*(1+20*exp(-10*.x))/(1+exp(-10*.x)))+
  labs(x='DBH (m)', y='Deathrate')
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.4*((.x-1)**8))+
  labs(x='DBH (m)', y='Deathrate')

# Canopy
# Weng et al 2015
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.02*(1+5*exp(4*.x-2))/(1+exp(4*.x-2)))+
  labs(x='DBH (m)', y='Deathrate')
# Other options
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.1/(1+exp((-10)*(.x-1))))+
  labs(x='DBH (m)', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1/exp((-1)*(.x-2)))+
  labs(x='DBH (m)', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 0.01*exp(0.6*.x))+
  labs(x='DBH (m)', y='Deathrate')

######## Carbon starvation function ######## 
# Previous forms
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 0.03*(exp(-0.9*.x+5)/(0.01+exp(-0.9*.x+5))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 7)), aes(x)) + stat_function(fun = ~ 1*(exp(-2*.x+7)/(1+exp(-2*.x+7))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 0.3*(exp(-1.5*.x+6)/(1+exp(-1.5*.x+6))))+
  labs(x='NSC/leaf mass', y='Deathrate')
# New one
ggplot(data.frame(x = c(0, 8)), aes(x)) + stat_function(fun = ~ 1*(exp(-2*.x+8)/(1+exp(-2*.x+8))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-1.5*.x+5)/(1+exp(-1.5*.x+5))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-5.5*.x+5)/(1+exp(-5.5*.x+5))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-1.5*.x+5)/(1+exp(-1.1*.x+5))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-1.3*.x+5)/(1+exp(-1.3*.x+5))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-1.3*.x+5)/(1+exp(-1*.x+5))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 10)), aes(x)) + stat_function(fun = ~ 1*(exp(-1.4*.x+10)/(1+exp(-1*.x+10))))+
  labs(x='NSC/leaf mass', y='Deathrate')
ggplot(data.frame(x = c(0, 5)), aes(x)) + stat_function(fun = ~ 1*(exp(-2.5*.x+7)/(10+exp(-2.5*.x+7))))+
  labs(x='NSC/leaf mass', y='Deathrate')

######## Growth rate function ######## 
# Previous forms
ggplot(data.frame(x = c(0, 10)), aes(x)) + stat_function(fun = ~ 0.01*(4*exp(4*(.x)))/(1+exp(4*(.x))))+
  labs(x='Vol', y='Deathrate')
ggplot(data.frame(x = c(0, 2)), aes(x)) + stat_function(fun = ~ 0.1/(1+exp((-5)*(.x-1))))+
  labs(x='DBH (m)', y='Deathrate')
ggplot(data.frame(x = c(0, 80)), aes(x)) + stat_function(fun = ~ 0.6/(1+exp((-.1)*(.x-30))))+
  labs(x='Vol', y='Deathrate')
# Linear fc
ggplot(data.frame(x = c(0, 180)), aes(x)) + stat_function(fun = ~ 0.01*.x)+
  labs(x='Vol', y='Deathrate')
