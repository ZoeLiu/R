getwd()
setwd("/Users/ZoeL/Google Drive/Big Data Analytics/Lecture2/hw2")

'''
Problem 1: Boxplot of Binomial distribution
'''
# create a dataframe containing the distribution for the probabilities
n <- 60
dist <- c()
prob <- c()
nk <- c()
for (p in c(0.3,0.5,0.8)){
  
  for (k in 0:n)
  {
    dist <- c(dist, dbinom(k,n,p)) 
    prob <- c(prob, p)
    nk <- c(nk,k)
  }  
}
dist
prob
dist.df <- data.frame(nk,prob=factor(prob),dist)
str(dist.df)

#plot the binomial distribution 
library(ggplot2)
ggplot(dist.df,aes(x=nk,y=dist,color=prob)) + 
      scale_x_discrete(breaks=seq(0,60,10)) + xlab("Number of Successful Trials") +
      scale_y_continuous(limits=c(0,0.2),breaks=seq(0,0.2,0.05)) + ylab("Binomial Distribution") +
      geom_point()


## summary statistics of distributions
mean <- tapply(dist.df$dist,dist.df$prob,mean)
sd <- tapply(dist.df$dist,dist.df$prob,sd)
quantile <- tapply(dist.df$dist,dist.df$prob,quantile)

#quantile data only contains 25%, 50% and 75%:
q <- cbind(quantile$'0.3'[2:4],quantile$'0.5'[2:4],quantile$'0.8'[2:4])

## create a list of statistics as input for 'bxp' to generate the boxplot
#initiate an empty list
boxstat <- list()
#statistics input: mean-sd, 1st quantile, median, 3rd quantile, mean+sd
boxstat$stats <- rbind(mean-sd,q,mean+sd)
rownames(boxstat$stats) <- c("mean-sd","q1","median","q3","mean+sd")

#other statistics
boxstat$n <- c(61,61,61)
boxstat$group <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
boxstat$names <- c("0.3","0.5","0.8")
boxstat

## generate boxplot using bxp, taking statistics list as input
bxp(boxstat)
# add the 'mean' diamond to the plot
points(mean,col="red",pch=5)



'''
Problem 2: Old Faithful data: scatter plot
'''
head(faithful)
attach(faithful)
plot(eruptions,waiting,xlab="Eruption Durations",ylab="Time Waited")
model <- lm(waiting~eruptions,data=faithful)
model
#add the line with the intercept and coefficient from model variable
abline(33.47,10.73,col="blue",lwd=8)
#pass model variable directly to abline
abline(model,col="red",lwd=5)



'''
Problem 3: Old Faithful data: boxplot for short and long type
'''
#create type data
faithful$type <- ifelse(eruptions < 3.1,'short','long')

#boxplot of waiting by type
boxplot(waiting~type,faithful,xlab='Type of eruption durations',ylab="Time waited",
        main="Waiting time distribution by duration type")
#boxplot of duration by type
boxplot(eruptions~type,faithful,xlab='Type of eruption durations',ylab="Eruption durations",
        main="Eruption durations distribution by duration type")
detach(faithful)


'''
Problem 4: Generate Uniform Distribution
'''
#create a random variable containing 500000 random numbers drawn from U[-1,2]
ru <- runif(500000,min=-1,max=2)
#generate the histogram with 20 breaks
bk <- seq(-1,2,by=0.15)
ru.hist <- hist(ru,breaks=bk)
hist(ru,breaks=bk)

## cumulative distribution 
# cut the random variable and find the frequency for the 20 breaks:
# method 1 (modify the histogram list and re-generate histogram)
ru.hist$counts <- cumsum(ru.hist$counts)/length(ru)
plot(ru.hist,
     xlab='Random numbers drawn from U[-1,2]', 
     ylab="Relative cumulative distribution", 
     main="Histogram of relative cumulative distribution")

# method 2 (use barplot):
ru.cut <- cut(ru,bk,right=FALSE)
ru.freq <- table(ru.cut)
ru.cumfreq <- cumsum(ru.freq)
ru.cumrelfreq <- ru.cumfreq/length(ru)
barplot(ru.cumrelfreq,
        xlab='Random numbers drawn from U[-1,2]', 
        ylab="Relative cumulative distribution", 
        main="Histogram of relative cumulative distribution")



'''
Problem 5: Write function to create matrix and present distributions
'''
## matrix generation function
getUnifMatrix <- function(nrow,ncol){
    mat <- matrix(,nrow,ncol)  #initialize matrix
    for (col in 1:ncol){
      set.seed(col)
      mat[,col] <- runif(nrow,min=-1,max=2)  #populate column with uniform distribution
    }
    return (mat)
}
#call getUnifMatrix function to create the matrix with 40 columns and 100 rows
unifMat <- getUnifMatrix(100,40)

## present distribution of any two columns
#choose the column index
index1 <- 19
index2 <- 40
#create data cut and summarize probabilities(from frequencies)
bk <- seq(-1,2,by=0.06)  #50 breaks
y1 <- table(cut(unifMat[,index1],bk,right=FALSE))/100
y2 <- table(cut(unifMat[,index2],bk,right=FALSE))/100
#take x as the middle point of each range break
start_midpt <- ((-1)+(-0.94))/2
end_midpt <- (1.94 + 2)/2
x <- seq(start_midpt,end_midpt,by=0.06)

## Plot density function
#plot the first column data
plot(x,y1,type='l',col="red",
     xlab="random values in [-1,2]",ylab="Probability",yaxt="n",
     main=paste("Probability density of random vectors",
               "\ngenerated from Uniform distribution"))
axis(side=2,at=y1)
#add the seconde column data
lines(x,y2,col="blue")

## Plot cumulative distribution
#plot the first column data
plot(x,cumsum(y1),type='l',col="red",
     xlab="random values in [-1,2]",ylab="Probability",
     main=paste("Cumulative distribution of random vectors",
                "\ngenerated from Uniform distribution"))
#add the seconde column data
lines(x,cumsum(y2),col="blue")


'''
Problem 6: Gaussian curve
'''
#add a sum column to matrix in P5
newcol <- apply(unifMat,1,sum)
unifMat_add <- cbind(unifMat,newcol)
hist(newcol,prob=T,ylim=c(0,0.1))

#calculate the mean and sd for Gaussian distribution
#note: this is to find mean and sd of sum of X~U[-1,2]
N <- 40
#mean and sd of U[-1,2]
unif.mean <- ((-1) + 2)/2
unif.sd <- (2-(-1))^2/12
#mean and sd of sum of X~U[-1,2]
gaussian.mean <-  N*unif.mean
gaussian.sd <- sqrt(N)*unif.sd

#plot the Gaussian curve using the mean and sd calculated
dx <- seq(0,35,length=100)
y  <- dnorm(dx,mean=gaussian.mean,sd=gaussian.sd)
lines(dx,y,lwd=2,col="red")





