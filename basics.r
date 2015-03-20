3 + 4
5 * 5

getwd()
setwd("/Users/ZoeL/Documents/R_Exercise")
sqrt(16)
factorial(4)
4 * 3 * 2 * 1

x <- 4
x * x
sqrt(x)

my.height <- 5*12 + 8
my.height


########  Vectors  ##############
c(2,3,5,7,10,11)
v <- c(2,3,4,5,6,7,7,7,78,8,9,0,12,14,334,256)
v[3]
v[100]
look.at <- 1:3
v[look.at]
length(v)
head(v)
tail(v,8)

v <- c("Gangnum Style","Charlie bit my finger","Evolution of dance","Rickroll","Lady Gaga")
length(v)

TRUE
FALSE
c(TRUE,FALSE,FALSE)

under.10 <- nchar(v)<10
under.10 
#will return [1] FALSE FALSE FALSE  TRUE  TRUE
v[under.10]  
#will return "Rickroll"  "Lady Gaga"


##########   Vector Arithmetic ##############
v <- c(10,20,30,40,50,60)
v + 1   
#will return [1] 11 21 31 41 51 61

2*v
w <- 2:7
v*w
#this will apply element-wise multiplication and return 20  60 120 200 300 420

#multiply vectors of different lengths:
w <- c(5,10)
v*w  #R will recycle the vector w in order to multiply all elements in v. will return [1]  50 200 150 400 250 600

w <- seq(5,20,5)   #sequence function: start 5, end 20, step 5
w
v*w  #will return warning msg: In v * w : longer object length is not a multiple of shorter object length
v^2
sqrt(v)
p <-2:7
v^p


############ Matrix ###################
v <- 1:12
v
matrix(v,nrow=3,ncol=4)
matrix(v,4)  #default would be nrow=3
matrix(v,ncol=4)

#don't have to have the vector to create matrix
matrix(0,3,4)
matrix(11:13,3,4) 
#note that the matrix write down the columns, so it will return:
 #  [,1] [,2] [,3] [,4]
#[1,]   11   11   11   11
#[2,]   12   12   12   12
#[3,]   13   13   13   13

#if you want it to write by rows, then do
m <- matrix(11:13,3,4,byrow=TRUE)
dim(m)
length(m)
head(m)   #first 6 rows of the matrix. in this case, it will return the full matrix
tail(m)
head(m,1)
tail(m,1)
head(m,-2)

m <- matrix(1:12,3,4)
m
m[2,4]
m[2,]
m[,4]
m[1:2,4]
m[1:2,3:4]
m[5]  #this will return '5'. however use rows and columns indication is the preferred method
m[2,2]  #this is equivalent to m[5]


#########   Functions in R ############
c()   #concatenation
length(v)
head(v)
tail(v)
dim(m)

letters #Built-in functions from a to z
letters[5:10]

#look for functions
?seq



