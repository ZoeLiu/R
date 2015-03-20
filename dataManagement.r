######## Work Space management ########
a <- 1
b <- 2
x <- 1:10
y <- "okay"
z <- TRUE
ls()
rm(a)
rm(list=ls())    #remove everything from workspace


######### Data type conversion (Variable Casting) ##########
x
as.numeric(x)
as.character(x)
x <- as.factor(x)
x
# convert factor to numeric: always convert to character first and then change char to numeric
x <- as.numeric(as.character(x))
as.matrix(x)
as.data.frame(x)
as.list(x)

######  Create a list of results and unlist #####
results <- list()
results$gp1 <- c(40,22,16)
results$gp2 <- c(25,15,9)
results$gp3 <- c(32,35,12)
results$gp4 <- c(28,19,15)
results

ur <- unlist(results)
ur
mr <- matrix(ur,4,3,byrow=TRUE)
um2 <- as.matrix(results)    # it won't work as expected
um2
dr <- as.data.frame(results) 
# will return
gp1 gp2 gp3 gp4
1  40  25  32  28
2  22  15  35  19
3  16   9  12  15

dr <- as.data.frame(mr)
colnames(dr) <- c("n","mean","sd")
dr
dr$n


#########  Apply() and Aggregate Function  #############
getwd()
rev <- read.delim("revenue.txt",header=FALSE)
colnames(rev) <- c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","g1","g2")
#1. First way to sum
apply(rev,2,sum)   #apply the sum function for each column. can write a loop for each column and store the result, but will be inefficient
apply(rev,1,sum,na.rm=TRUE)

#2. Second way to sum
colSums(rev)
rowSums(rev)

#3. Summary by group
 tapply(rev$c5,rev$g1,mean)   #single group
 tapply(rev$c5,rev[,c("g1","g2")],mean)   #multiple group
 tapply(rev$c5,rev[,c("g1","g2")],length)  #check the counts within each group
 
 x <- by(rev$c5,rev[,c("g1","g2")],mean)   #by statement will return a vector
 c(x)
 
#4. Summary using Aggregate 
aggregate(c5 ~ g1 + g2, rev, mean)
#output:
  g1 g2       c5
1  1  1 68.26114
2  2  1 11.68229
3  3  1 33.94074
4  5  1 47.85908
5  1  2 65.59992
6  2  2 47.57083
7  3  2 86.04406
8  4  2 58.41800
9  5  2 50.74578

#compare to the output from tapply (cross-tab)
   g2
g1         1        2
  1 68.26114 65.59992
  2 11.68229 47.57083
  3 33.94074 86.04406
  4       NA 58.41800
  5 47.85908 50.74578

#Aggregate on all variables
aggregate(. ~ g1 + g2, rev, mean) 


########  Other functions: With, Within ################
stock <- read.delim("Dummy_Stock.txt")
head(stock)

# With: create new variables as different vector independent of the dataset
change <- with(stock,Close-Open)
head(change)

#Within: create new variables within the dataset
stock_new <- within(stock,{
	          price.change <-Close-Open
	          total.change <- price.change*volume
	          })
head(stock_new)
