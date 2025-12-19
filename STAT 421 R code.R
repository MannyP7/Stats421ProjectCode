### SCENARIO 1| PART B ###

# parameter space Θ(0)
theta=0:494

# likelihood values
likelihood_values=choose(496-theta,2)/choose(496,2)

#Creating the plot
plot(theta, likelihood_values,
     pch = 1, col = "black",cex=0.3,
     xlab = expression(theta),
     ylab = "Likelihood",
     main = "Likelihood Function for " * theta ~ "Given " ~ X[1] == 0)

#Identifying MLE
theta_hat1=theta[which.max(likelihood_values)]
theta_hat1


### SCENARIO 1 | PART D ###

# parameter space Θ(0,2)
theta2=2:492

# likelihood values (with denominators kept)
likelihood_values2=(choose(496-theta2,2)/choose(496,2))*(choose(theta2,2)*choose(494-theta2,2)/choose(494,4))

# creating plot
plot(theta2, likelihood_values2,
     pch = 1, col = "black", cex = 0.3,
     xlab = expression(theta),
     ylab = "Likelihood",
     main = expression("Likelihood Function for " * theta ~ "Given " ~ X[1] == 0 ~ "and" ~ X[2] == 2))

# find MLE
theta_hat2=theta2[which.max(likelihood_values2)]
theta_hat2


### SCENARIO 1 | PART E ###

######################## n=4 #############################

## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=496
theta_o=331
n=4      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

############################# n = 5 ################################

## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=496
theta_o=331
n=5      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

############################# n = 6 ################################

## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=496
theta_o=331
n=6      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

############################# N1 = 750 ################################


## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=750
theta_o=331
n=4      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

############################# N1=1000 ################################

## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=1000
theta_o=331
n=4      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

############################# theta_o=360 ################################

## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=496
theta_o=360
n=4      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

############################# theta_o=390 ################################

## Monte Carlo Simulation

# S1
set.seed(4212025)
N1=496
theta_o=390
n=4      
R=1000
K=1

# S2
theta_hat1_vector=numeric(R)
set.seed(4212025)
for (r in 1:R){
  
  sample_x=rhyper(1,m=theta_o,n=N1-theta_o,k=n)
  
  theta_values=sample_x:(N1-(n-sample_x))
  
  likelihood_values=choose(theta_values,sample_x)*choose(N1-theta_values,n-sample_x)/choose(N1,n)
  
  
  theta_hat1_vector[r]=theta_values[which.max(likelihood_values)]
}
theta_hat1_vector


# S3
theta_hat3_vector=numeric(R)
install.packages("extraDistr")
library(extraDistr)

set.seed(4212025)
for (r in 1:R){
  sample_y=rnhyper(1,n=N1-theta_o,m=theta_o,r = 1)
  
  theta_values=1:N1
  likelihood_values3=choose(N1- sample_y,theta_values-1)/choose(N1,theta_values)
  likelihood_values3[is.nan(likelihood_values3)]=0
  
  theta_hat3_vector[r]=theta_values[which.max(likelihood_values3)]
}

theta_hat3_vector

#S4

bias1=mean(theta_hat1_vector)-theta_o
bias3=mean(theta_hat3_vector)-theta_o

var1=var(theta_hat1_vector)
var3=var(theta_hat3_vector)

rmse1=sqrt(var1+bias1^2)
rmse3= sqrt(var3+bias3^2)

results=data.frame(
  Estimator=c("ThetaHat1", "ThetaHat3"),
  Bias=c(bias1, bias3),
  Variance=c(var1, var3),
  RMSE=c(rmse1, rmse3)
)

results

