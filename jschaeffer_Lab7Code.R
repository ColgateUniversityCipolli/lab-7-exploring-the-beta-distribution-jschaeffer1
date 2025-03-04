library(tidyverse)

######################################################
#####     TASK ONE: Describing distribution     ######
######################################################

#Making a function that allows alpha and beta values to change

distribution.func = function(alpha, beta){
  
  mean = alpha/(alpha+beta)
  sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))
  
  distribute.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
           norm.pdf = dnorm(x,                                    # Gaussian distribution with
                            mean = mean,            # same mean and variance
                            sd = sd))
  
  return(distribute.dat)
  #Returning our created tibble
}

alpha = 2
beta = 5

mean = alpha/(alpha+beta)
sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))

###Making a tibble for each parameter
distrib.tibble1 = distribution.func(2,5) #Alpha = 2, Beta = 5
distrib.tibble2 = distribution.func(5,5) #Alpha = 5, Beta = 5
distrib.tibble3 = distribution.func(5,2) #Alpha = 5, Beta = 2
distrib.tibble4 = distribution.func(0.50,0.50) #Alpha = 0.5, Beta = 0.5

####################
###Making a plot for each of the 4 tibbles
####################

plot1 = ggplot(data= distrib.tibble1)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom

plot2 = ggplot(data= distrib.tibble2)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom

plot3 = ggplot(data= distrib.tibble3)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom

plot4 = ggplot(data= distrib.tibble4)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.5,0.5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom

#######################
#Making function for mean, variance, skewness and kurtosis
######################

mean.calc








