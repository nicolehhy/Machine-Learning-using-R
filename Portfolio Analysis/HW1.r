# Homework Assignment 1:Functions and Matrix Algebra in R
# Name:
# ID:
# Date:02/09/2019


# 1 #
# Q1(a) equally weighted portfolio
expected_r = matrix(c(0.08,0.101,0.075,-0.01),nrow=4)
equal_w = matrix(c(1/4,1/4,1/4,1/4),nrow=4)
expected_r = t(equal_w)%*%expected_r

C =matrix(c(1.000,0.770,0.670,-0.070,0.770,1.000,0.600,-0.110,0.670,
            0.600,1.000,-0.050,-0.070,-0.110,-0.050,1.000),nrow=4,byrow = T)
sig1 = c(0.175,0.212,0.195,0.142)
sig1_transpose = t(sig1)  # t(vector) transposes vector or matrix
sig = sig1 %*% sig1_transpose #sig1 [2x1] and sig1_t [1x2]->[2x2]
V=sig*C

standard_d = sqrt(t(equal_w)%*%V%*%equal_w)
expected_r
standard_d

# Q1(b) weight for having maximum sharpe ratio
R = matrix(c(0.08,0.101,0.075,-0.01),nrow=4)
I = matrix(c(1,1,1,1),nrow=4)
Rf = 0.025
A = solve(V) %*% (R-Rf)
A1 = t(I)%*%solve(V)%*%(R-Rf)
w = A%*%solve(A1)
w

# Q1(c)  expected return for having maximum sharpe ratio
ER = t(w)%*%R
ER

# Q1(d)  expected standard deviation for having maximum sharpe ratio
SD = matrix(c(0.175,0.212,0.195,0.142),nrow=4)
SD_p =  sqrt(t(w)%*%V%*%w)
SD_p

# Q1(e)  SD and ER for having minimum sharpe ratio
Wmin1 = solve(V)%*%I
Wmin2 = t(I)%*%solve(V)%*%I
Wmin = Wmin1%*%solve(Wmin2)
ERmin = t(Wmin)%*%R
SDmin = sqrt(t(Wmin)%*%V%*%Wmin)
ERmin
SDmin

# Q2 Create a Function for valuing either a delayed perpetuity or a delayed annuity.
# The inputs that had been selected were impermissible if in fact you input a value of g > r
Value <- function(N,M,C,g,r){
  P <- NULL
  V <- NULL
  if(N<0){
    P = C/((1+r)^M*(r-g))
    return(P)
  }
  else{
    V = (1-((1+g)/(1+r))^N)*C/(((1+r)^M)*(r-g))
  }
  return(V)
}

# Q3 Using the R Function required in problem 2, graph the value 
# here we assume that M=5;g=0;N=20;C=100 and r increases from 0.01 to 0.1
value_g0 <- list()
for(i in 1:19){
  r <- c()
  r[i] <- 0.01+(i-1)*0.005
  M=5;g=0;N=20;C=100
  value_g0[[i]] <- Value(N,M,C,g,r[i])
}
value_g0

# here we assume that M=10;g=0;N=20;C=100 and r increases from 0.01 to 0.1
value_g1 <- list()
for(i in 1:19){
  r <- c()
  r[i] <- 0.01+(i-1)*0.005
  M=10;g=0;N=20;C=100
  value_g1[[i]] <- Value(N,M,C,g,r[i])
}
value_g1

rate <- c()
for (i in 1:18) {
  rate[i] <- 0.01+(i-1)*0.005
}
# here we assume that M=5;g=0;N=20;C=100 and r increases from 0.01 to 0.1
data0 <- data.frame(value = unlist(value_g0),interest_rate = rate)
ggplot(data0, aes(x = interest_rate, y = value)) +
  geom_line() + labs(title = "Relation between present_value and interest when M=5")
data1 <- data.frame(value = unlist(value_g1),interest_rate = rate)
ggplot(data1, aes(x = interest_rate, y = value)) +
  geom_line() +labs(title = "Relation between present_value and interest when M=10")

data0 <- data.frame(value = unlist(value_g0),interest_rate = rate)
plot(as.vector(data0$interest_rate),as.numeric(data0$value),type = "l" ,main = "Relation between present_value and interest",
     xlab = "interest rate",ylab = "present_value",col="red", yaxt ="n")
data1 <- data.frame(value = unlist(value_g1),interest_rate = rate)
plot(as.vector(data1$interest_rate),as.vector(data1$value),type = "l" ,main = "Relation between present_value and interest",
     xlab = "interest rate",ylab = "present_value",col="blue")

plot(as.vector(data1$interest_rate),as.vector(data1$value),type = "l" ,main = "Relation between present_value and interest",
     xlab = "interest rate",ylab = "present_value",col="blue")