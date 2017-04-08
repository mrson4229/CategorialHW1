library("ggplot2")
library("epitools")

# 1
#(1)

eq1 = function(x){dnorm(x,170,2)} #likelihood function for mu1
eq1.1<-function(x){log(eq1(x))} #loglikelihood function for mu1

# Graph of the likelihood function 
# ggplot(data.frame(x=c(160, 180)), aes(x=x)) +
#   stat_function(fun=eq1, geom="line") + 
#   xlab("x") +
#   ylab("y")

eq2 = function(x){dnorm(x,172,2)} #likelihood function for mu2
eq2.1 = function(x){log(eq2(x))} #loglikelihood function for mu2

eq3 = function(x){dnorm(x,177,2)} #likelihood function for mu3
eq3.1 = function(x){log(eq3(x))} #loglikelihood function for mu3

mu<-seq(160,180,0.1)

(L1=max(eq1.1(mu))*max(eq2.1(mu))*max(eq3.1(mu)))


# (2)

#likelihood function for mu1=mu2=mu3=mu
likelifun2<- function(x){
(8*pi)^(-1.5)*exp(-0.125*((170-x)^2+(172-x)^2 + (177-x)^2))
  }

# Graph of the likelihood function 
ggplot(data.frame(x=c(160, 180)), aes(x=x)) +
  stat_function(fun=likelifun2, geom="line") +
  xlab("x") +
  ylab("y")

max(likelifun2(mu))

(L2=log(max(likelifun2(mu))))

# (3)
(L3=log(likelifun2(168)))

# (4)

L1>L2
L2>L3

2*(L1-L2)
# pearson chisqaure statistics
# 
2*(L2-L3)


#2
#(1)
eq1 = function(x){dnorm(x,170,2)} #likelihood function for mu1
eq1.1<-function(x){log(eq1(x))} #loglikelihood function for mu1

# Graph of the likelihood function 
# ggplot(data.frame(x=c(160, 180)), aes(x=x)) +
#   stat_function(fun=eq1, geom="line") + 
#   xlab("x") +
#   ylab("y")

eq2 = function(x){dnorm(x,172,2)} #likelihood function for mu2
eq2.1 = function(x){log(eq2(x))} #loglikelihood function for mu2

eq3 = function(x){dnorm(x,177,2)} #likelihood function for mu3
eq3.1 = function(x){log(eq3(x))} #loglikelihood function for mu3

mu<-seq(150,180,0.1)

(L1.2<-max(eq1.1(mu))*max(eq2.1(mu))*max(eq3.1(mu)))


#(2)
dat=as.data.frame(matrix(c(0.5,1,1.5,170,172,177),byrow = FALSE, nrow = 3))
names(dat)=c('x','y')
fit<-lm(y ~ x, data=dat)
summary(fit)
166+dat$x*7
(L2.2=log((8*pi)^(-1.5)*exp(-0.125*((170-169.5)^2+(172-173.0)^2 + (177-176.5)^2))))

#(3)
163+dat$x*6
(L3.2=log((8*pi)^(-1.5)*exp(-0.125*((170-166)^2+(172-169)^2 + (177-172)^2))))

#(4)
L1.2>L2.2
L2.2>L3.2

2*(L1.2-L2.2)
2*(L2.2-L3.2)
#similar to pearson chisquare statistics

#(5)
eq1 = function(x){dnorm(x,170,sqrt(10))} #likelihood function for mu1
eq1.2<-function(x){log(eq1(x))} #loglikelihood function for mu1
eq2 = function(x){dnorm(x,172,sqrt(10))} #likelihood function for mu2
eq2.2 = function(x){log(eq2(x))} #loglikelihood function for mu2
eq3 = function(x){dnorm(x,177,sqrt(10))} #likelihood function for mu3
eq3.2 = function(x){log(eq3(x))} #loglikelihood function for mu3
(L1.3<-log(max(eq1(mu))^3))

(L2.3=log((20*pi)^(-1.5)*exp(-0.05*((170-169.5)^2+(172-173.0)^2 + (177-176.5)^2))))

(L3.3=log((20*pi)^(-1.5)*exp(-0.05*((170-166)^2+(172-169)^2 + (177-172)^2))))

L1.3>L2.3
L2.3>L3.3

2*(L1.3-L2.3)
2*(L2.3-L3.3)
#doent affect the order of three stats. but due to the increase of population variation,
#the deviance decreased.

#3
#(1)
cont<-matrix(c(22,18,26,14),byrow=TRUE,nrow=2)
row.names(cont)<-c('Male','Female')
colnames(cont)<-c('High','Low')
chisq.test(cont,correct = FALSE)
#행과열이 통계적으로 독립인가

#(2)
prop.test(cont)

#(3)
#install.packages('epitools')
# library(epitools)
riskratio.wald(cont,correction = FALSE)

estm<- (22/40)/(26/40)
log(estm)
se_log<-sqrt((18/40)/22 + (14/40)/26)
#95% ci for log estimate
lower<-log(estm)-1.96*se_log ; upper<-log(estm)+1.96*se_log
#95% ci for estimate(RR)
exp(lower);exp(upper)


#(4) 
#oddsratio.small(cont,correction=FALSE)
ore<-(22*14)/(18*26)
ase_log<-sqrt(1/22 + 1/26 + 1/18 + 1/14)
lo<-log(ore)-ase_log ; up<-log(ore)+ase_log
exp(lo);exp(up)

#(5)
a<-matrix(nrow = 80,ncol = 2)
colnames(a)<-c('Male','High')
a[1:40,1]<-1
a[1:22,2]<-1
a[41:66,2]<-1
for (k in 1:80)  {
  for (j in 1:2) {
    if (is.na(a[k,j])) a[k,j]=0
  }
}

df<-data.frame(a)
cor(df)
cor.test(df$Male,df$High)

#(6)
# 검정은 유효하지 않다.켄달 타우로 
# 부부간의 학력이 유사한 짝표본과
# 상이한 짝표본의 수를 비교해본다.
