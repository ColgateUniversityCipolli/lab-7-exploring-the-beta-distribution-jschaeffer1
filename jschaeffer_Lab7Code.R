library(tidyverse)
library(ggplot2)
library(patchwork)
library(nleqslv)

######################################################
#####     TASK ONE: Describing distribution     ######
######################################################

#######################
#Making function for mean, variance, skewness and kurtosis
######################
#Calculating mean
mean.calc = function(alpha, beta){
  mean = alpha/(alpha+beta)
  return(mean)
}

#Calculating Variance
variance.calc = function(alpha, beta){
  sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))
  return(sd)
}

#Calculating Skewness
skew.calc = function(alpha, beta){
  skew = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta))
  return(skew)
}

#Calculating Kurtosis
kurt.calc = function(alpha, beta){
  kurt.num = 6*((alpha-beta)^2*(alpha+beta+1)-alpha*beta*(alpha+beta+2))
  kurt.denum = alpha*beta*(alpha+beta+2)*(alpha+beta+3)
  kurt = kurt.num/kurt.denum
  return(kurt)
}


#Making a function to calculate distribution tibble
distribution.func = function(alpha, beta){
  
  mean = mean.calc(alpha, beta)
  sd = variance.calc(alpha, beta)
  
  distribute.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
           norm.pdf = dnorm(x,                                    # Gaussian distribution with
                            mean = mean,            # same mean and variance
                            sd = sd))
  
  return(distribute.dat)
  #Returning our created tibble
}


###Making a tibble for each parameter and getting values
#Alpha = 2, Beta = 5
distrib.tibble1 = distribution.func(2,5)
mean1 = mean.calc(2,5)
sd1 = variance.calc(2,5)
skew1 = skew.calc(2,5)
kurt1 = kurt.calc(2,5)


#Alpha = 5, Beta = 5
distrib.tibble2 = distribution.func(5,5)
mean2 = mean.calc(5,5)
sd2 = variance.calc(5,5)
skew2 = skew.calc(5,5)
kurt2 = kurt.calc(5,5)


#Alpha = 5, Beta = 2
distrib.tibble3 = distribution.func(5,2)
mean3 = mean.calc(5,2)
sd3 = variance.calc(5,2)
skew3 = skew.calc(5,2)
kurt3 = kurt.calc(5,2)


#Alpha = 0.5, Beta = 0.5
distrib.tibble4 = distribution.func(0.5,0.5)
mean4 = mean.calc(0.5,0.5)
sd4 = variance.calc(0.5,0.5)
skew4 = skew.calc(0.5,0.5)
kurt4 = kurt.calc(0.5,0.5)

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



################################################
###               TASK 2                    ####
################################################


beta.moment = function(alpha, beta, k, centered) {
  if (centered) { #Calculating the centered value
    #calculating E(X)
    integrand = function(x) {x*dbeta(x,alpha,beta)}
    EX = integrate(integrand, 0, 1)$value
    
    #Calculating centered value
    integrand = function(x) {((x-EX)^k)*dbeta(x,alpha,beta)}
    result = integrate(integrand, 0, 1)
  }
  else{ #Calculating uncentered
    integrand = function(x) {x^k * dbeta(x,alpha,beta)}
    result = integrate(integrand, 0, 1)
  }
  return(result)
}

###Testing function
#Testing Mean
beta.moment(2,5,1,F)
mean1

#Testing Variance
beta.moment(2,5,2,T)
sd1^2

#Testing Skew
beta.moment(2,5,3,T)$value/(beta.moment(2,5,2,T)$value^(3/2))
skew1

#Testing Kurtosis
(beta.moment(2,5,4,T)$value/(beta.moment(2,5,2,T)$value)^2)-3
kurt1

###############################################
#####               TASK THREE            #####
###############################################
set.seed(7272) # Set seed so we all get the same results.

sample_func = function(n, alpha, beta){
  
  sample.size <- n # Specify sample details
  beta.sample <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
  return(beta.sample)
}

####Making density plots
#Alpha = 2, Beta = 5
sample1 = sample_func(500, 2, 5)

density_plot1 = ggplot() + 
  geom_histogram(aes(sample1, y=after_stat(density))) +
  geom_density(aes(sample1)) + 
  geom_hline(yintercept=0)


#Alpha = 5, Beta = 5
sample2 = sample_func(500, 5, 5)

density_plot2 = ggplot() + 
  geom_histogram(aes(sample2, y=after_stat(density))) +
  geom_density(aes(sample2)) + 
  geom_hline(yintercept=0)


#Alpha = 5, Beta = 2
sample3 = sample_func(500,5,2)

density_plot3 = ggplot() + 
  geom_histogram(aes(sample3, y=after_stat(density))) +
  geom_density(aes(sample3)) +
  geom_hline(yintercept=0)


#Alpha = 0.5, Beta = 0.5
sample4 = sample_func(500,0.5,0.5)

density_plot4 = ggplot() + 
  geom_histogram(aes(sample4, y=after_stat(density))) +
  geom_density(aes(sample4)) +
  geom_hline(yintercept=0)


#summary1 %>%
#  summarize(mean = mean(sample1),
 #           variance = var(sample1),
 #           skewness = )


##############################################
###             TASK 6                    ####
##############################################
death.data = read_csv("death.data.test.csv") #Pulling data


#Keeping only data from 2022
death.data <- death.data %>%
  select(1:2, `2022`) %>%
  mutate(`2022` = `2022` / 1000) #Convering to rate out of 1


##############################################
####            TASK 7                    ####
##############################################

##############
# CACLULATING MOM
##############
MOM.beta <- function(data, par){
  alpha <- par[1]
  beta <- par[2]
  
  EX1 <- alpha/(alpha+beta)
  EX2 <- alpha*(alpha+1)/((alpha+beta+1)*(alpha+beta))
  
  m1 <- mean(data, na.rm = T)
  m2 <- mean(data^2, na.rm = T)
  
  return( c(EX1 - m1, EX2 - m2) )
}

#Using function to find actual alpha and beta values
moms<- nleqslv(x = c(5, 1000),
                fn = MOM.beta,
                data=death.data$`2022`)

alpha.hat.mom = moms$x[1]
beta.hat.mom = moms$x[2]


#####################
# CALCULATING MLE
#####################

llbeta <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dbeta(x=data, shape1=alpha, shape2=beta)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

#Using function to find actual alpha/beta values
mles <- optim(par = c(5,1000),
               fn = llbeta,
               data=death.data$`2022`,
               neg=T)
alpha.hat.mle <- mles$par[1]
beta.hat.mle <- mles$par[2]

#################
# PLOTTING
#################
#Getting data to plot from MOM and MLE
ggdat.beta <- tibble(x=seq(0,0.025, length.out=1000))|>
  mutate(mom.pdf = dbeta(x=x, shape1=alpha.hat.mom, shape2 = beta.hat.mom),
         mle.pdf = dbeta(x=x, shape1=alpha.hat.mle, shape2 = beta.hat.mle))


###Plotting the graph
mlemom.plot = ggplot(death.data, aes(x = `2022`, y=after_stat(density))) + #Plotting sample data
  geom_histogram(bins = 30) + #Histogram plot
  geom_hline(yintercept=0)+ #Making y intercept line
  theme_bw() +
  geom_line(data=ggdat.beta,
            aes(x=x, y=mom.pdf, color="MOM")) + #Calculating MOM line
  geom_line(data=ggdat.beta,
            aes(x=x, y=mle.pdf, color="MLE")) + #Calculating MLE line
  ylab("Deaths per Person in 2022") +
  xlab("Density")

############################################
######            TASK 8              ######
############################################

#Initializing values
alpha = 8
beta = 950
n = 266
estimates.data = data.frame(iteration = numeric(),
                            alpha.mom = numeric(),
                            beta.mom = numeric(),
                            alpha.mle = numeric(),
                            beta.mle = numeric())

#view(estimates.data)
for (i in 1:1000){
  set.seed(7272+i) #Setting seed
  
  sample = rbeta(n, alpha, beta)
  moms<- nleqslv(x = c(5, 1000),
                 fn = MOM.beta,
                 data=sample)
  
  mles <- optim(par = c(5,1000),
                fn = llbeta,
                data=sample,
                neg=T)
  
  new.row = data.frame(i, moms$x[1], moms$x[2], mles$par[1], mles$par[2])
  
  #Adding values into dataframe
  estimates.data[i,1] = i
  estimates.data[i,2] = moms$x[1]
  estimates.data[i,3] = moms$x[2]
  estimates.data[i,4] = mles$par[1]
  estimates.data[i,5] = mles$par[2]
  
  #Plotting values in a geom_density 2x2
  #Calculating Graph for the mom alpha estimates
  momA = ggplot(estimates.data, aes(x = alpha.mom)) +
    geom_density() +
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Alpha (MOM estimate)") +
    ylab("Density")
  
  #Calculating graph for mom beta estimates
  momB = ggplot(estimates.data, aes(x = beta.mom)) +
    geom_density() +
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Beta (MOM estimate)") +
    ylab("Density")
  
  #Graph for mle alpha estimates
  mleA = ggplot(estimates.data, aes(x = alpha.mle)) +
    geom_density() +
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Alpha (MLE estimate)") +
    ylab("Density")
  
  #Graph for mle beta estimates
  mleB = ggplot(estimates.data, aes(x = beta.mle)) +
    geom_density() +
    geom_hline(yintercept=0) +
    theme_bw() +
    xlab("Beta (MLE estimate)") +
    ylab("Density")
  
  #Making graphs into 2x2 grid using patchwork
  estimate.density = (momA | momB) / (mleA | mleB)
  
  estimates.data |>
    summarize(
      bias = mean(alpha.mom)-alpha,
      precision = 1/var(alpha.mom),
      mse = var(alpha.mom) + bias^2
    )
  (bias <- mean(theta.hats)- theta)
  (precision <- 1/var(theta.hats))
  (mse <- var(theta.hats) + bias^2)
  
  
  
  
}














