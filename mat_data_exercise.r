'''
Problem 1

Set up the matrix'''
# Generate the random vector
set.seed(123)
rnd.vec <- runif(16)
# Populate the vector into 4x4 square matrix B
B <- matrix (rnd.vec, nrow=4, byrow=T)
# Generate the unit matrix
unitm <- diag(4)
# Modify matrix B by adding the unit matrix
B <- B + unitm

#Note: generateMatrix(20,100) function can also be used. sapply()

'''Calculations'''
#1. Inverse matrix of B
BINV <- solve(B)
#2. Demonstrate B*BINV is a unit matrix
B %*% BINV  
#note: the result printed out by above is a unit matrix

#3. Eigen values of matrix B and BINV
eB <- eigen(B)
eBINV <- eigen(BINV)
eB$values
eBINV$values

#4. Eigen values of transpose of B
eTB <- eigen(t(B))
eTB$values

#5. Demonstrate that matrix B multiplied by one of its eigen vectors from right results in a vector which is equal to the product of the corresponding eigen value and that same vector
idx <- 4
leftside <- B %*% eB$vectors[,idx]
rightside <- eB$values[idx]*eB$vectors[,idx]
leftside
rightside
#note: leftside and rightside is equal


'''
Problem 2
'''
# Create x1 coordinate and x2
x1 <- seq(0,10,length=20)
set.seed(123)
x2 <- x1*2.3 - 1.2 + rnorm(20)
# Scatter plot of (x1,x2)
plot(x1,x2, xlim=c(0,20), ylim=c(0,20),col="blue")
abline(a=-1.2,b=2.3)
'''
Problem 3: covariance matrix
'''
# Calculate average of x1, x2, and covariance
n <-20
avgx1 <- mean(x1)
avgx2 <- mean(x2) 
diff1 <- x1 - avgx1
diff2 <- x2 - avgx2
cov <- sum(diff1*diff2)/(n-1)

# Calculate variance
var1 <- sum(diff1^2)/(n-1)
var2 <- sum(diff2^2)/(n-1)

# Covariance matrix
cm <- matrix(c(var1,cov,cov,var2), nrow=2)
# Find eigen vectors of covariance matrix. Demonstrate that one of them is parallel with the line in problem 2 and the other is normal to that line
# draw vectors:
slope1 <- eigen(cm)$vectors[2,1]/eigen(cm)$vectors[1,1]
#slope1 equals to 2.3, which is parallel to the line in problem 2
slope2 <- eigen(cm)$vectors[2,2]/eigen(cm)$vectors[1,2]
slope2*slope1  #since slope2*2.3 equals to -1, it is a normal line of v1, i.e. of line in problem 2
abline(0,slope1,col='red')
abline(0,slope2,col='red')

'''
Problem 4: Smokers data
'''
smokers <- read.delim("Smokers.txt",header=TRUE,sep="")
class(smokers)
mode(smokers)
str(smokers)
labels(smokers)[[1]]
labels(smokers)[[2]]

attach(smokers)
smokers$GDPPerCapita <- as.numeric(gsub(",","",GDPPerCapita))
#droplevels(GDPPerCapita)
smokers[,2]  #PercentSmokes
smokers[,3]  #GDPPerCapita

# Scatter plot
plot.new()
plot(smokers[,3],smokers[,2],xlab="GDP Per Capita",ylab="Percent Smokes", main="Relationship between % Smokers and GDP Per Capita")

# Histogram
#bp <- c(0,2000,5000,10000,16000,30000,50000) #set up the break point
bp <- c(0,1025,4035,12475,40000)
hist(GDPPerCapita,breaks=bp,ylim=c(0,20), ylab='Number of Countries', xlab='GDP Per Capita',freq=TRUE,main='Number of Countries by GDP Per Capita',col.main='purple')

# Pie Chart
#create country category by GDP
smokers$category <- ifelse(GDPPerCapita<=1025,'Low-income', 
                 ifelse(GDPPerCapita <= 4035, 'Lower middle income',
                 ifelse(GDPPerCapita <= 12475, 'Upper middle income',
                 ifelse(GDPPerCapita <= 40000, 'High-income'))))
str(smokers)
income_freq <- table(smokers$category)
pie(income_freq)


