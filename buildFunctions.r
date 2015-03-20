set.seed(100)   #set the seed for randomization
?rpois   #rpois(n,lambda)
d <- rpois(25,8)
d

# Basic function:
GetMeanAndSE <- function(x,level=0.95){
	#Add error msg
	if (level <= 0 || level >= 1) {
		stop('The level argument must be >0 and <1')
			}
	#Add warning msg		
	if (level <0.5) {
		warning('The confidence levels are often close to 1, e.g. 0.95')	
	}
	m <- mean(x)
	n <- length(x)
	SE <- sd(x) / sqrt(n)
	upper <- 1- (1-level)/2
	ci <- m + c(-1,1)*qt(upper,n-1)*SE
	#return(c(m,SE,ci))   #it would be more useful to return the list and give each element in the list a name
	return(list(mean=m,se=SE,confidence.interval=ci))
}

GetMeanAndSE(d,0.05)


# Pass Extra Argument using Ellipsis
rev <- read.delim("revenue.txt", header=FALSE)
head(rev)

RevSummary <- function(x,...){
	if (!is.matrix(x) && !is.data.frame(x)){
		stop("x must be a matrix or data frame")
	}
	
#	ellipsis.args <- list(...)
	rev.per.company <- colMeans(x,...)
	rev.per.day <- rowMeans(x,...)
	return(list(rev.per.company=rev.per.company,rev.per.day=rev.per.day))
}

RevSummary(rev,na.rm=TRUE,dims=1)

