getwd()

setwd("/Users/ZoeL/Documents/Programming/Datasets")
list.files()

state <- read.csv("states.csv")
dim(state)

head(state)
length(state)  #return number of variables

###### Data Frames  - the same as Matrix
state[,1]
state[2:4,5:9]

names(state)   #to extract the variable names
state$name   #print out all observations for variable name
state$population

typeof(state$population)
sapply(state,class)  #check the variable type

#### Data checks #######
summary(state)
str(state)   #structure of the data (like proc contents in SAS)
table(state$dst)   #run counts

new_population <- as.numeric(as.character(state$population))   #this won't work since the numbers contain comma ,. will need to use gsub before the conversion
new_population

#### Get basic statistics via variable Type conversion: #########
?gsub
options(digits=2)
state$population <- as.numeric(gsub("\\,","",state$population)) 
population.mean <- mean(state$population)
format (population.mean,nsmall=4)  #format the print of means
population.mean
population.std <- sd(state$population)
population.std

#####  Subset a dataset  ################
high.pop <-subset(state, population > population.mean)
high.pop
#####  Subset the dataset with selected columns
subset(state,population>population.mean,select=c("name","capital"))

####  Factor variables  #################
## Note: you can always convert factor variable into character variable when you don't know how to deal with it. And R and convert char back to factor when it is appropriate with warnings
state$dst
#it will show Levels: NO YES (which means it is a factor)
as.character(state$dst)
#after change to char, the data will print out with quotation marks, like "YES" "YES" "NO"  "YES" 



##### Objects in data frame  (Date object)
stock <- read.csv("http://faculty.washington.edu/ezivot/econ424/sbuxPrices.csv")
str(stock)
head(stock,3)
tail(stock,3)
summary(stock)
head(stock$Date,3)

#-----Format Notation in R -------
01/31/2013   %m/%d/%Y
20130131      %Y%m%d
Jan.1, 2013   %b.%d,%Y
January 1,13  %B,%d,%y

?strptime   #Date-time conversion functions to and from character
#change the date format
stock$Date <- as.Date(stock$Date,"%m/%d/%Y")
head(stock$Date,3)
plot(stock$Date,stock$Adj.Close,type="l")



###########  If Statement #########
# create an open column
stock$open <-sample(1:2,181,replace=T)
head(stock$open)
str(stock)

if (stock$open[1]>stock$Adj.Close[1]) {
	status <- "down"
} else if (stock$open[1]<stock$Adj.Close[1]) {
	status <- "up"
} else {
	status <- "flat"
}
status
head(stock)

cond <- stock$Adj.Close > stock$open
cond
head(cond)
x <- data.frame(a=c(1,2,3), b=c(3,2,1))
cond <- x$a>x$b
status <- ifelse(cond,"up","flat")
cond <- x$a<x$b
status <- ifelse(cond,"down",status)
table(status)   #check the counts

### element wise comparisons
x <- sample(-1:13,10)
y <- sample(-1:13,10)
all(x>0)
any(x==y)
x > 0 & y > 0
x > 0 | y > 0
?xor 
x > 0 && y > 0  #only on first element
x > 0 || y > 0  #only on first element

### which function: return which indices are true
which(stock$Adj.Close>9)

###########  For loops #########
x <- c()
  for (i in 1:10){
  	  x <- append(x,i^2)
  	
  }
x 


str(state)
table(state$time.zone.1)
state$population <- as.numeric(gsub("\\,","",state$population))
state$time.zone.1 <- as.character(state$time.zone.1)
the.zones <- unique(state$time.zone.1)

low <- c()
high <-c()

for (zone in the.zones){
	look.at <- state$time.zone.1 == zone
    state$population[look.at]		
	low <- append(low,min(state$population[look.at]))
	high <-append(high,max(state$time.population[look.at]))
}
low
high
which(state$time.zone.1 == "AKST (UTC-09) ")


low <- c()
high <-c()
pos <- c()
for (zone in the.zones){
	look.at <- state$time.zone.1 == zone
  #  look.at
   # pos <- append(pos,which(state$time.zone.1 == zone))
	low <- append(low,min(state$population[look.at]))
	high <-append(high,max(state$time.population[look.at]))
}
low
high

########### MISSING Data
stateM <- read.csv("states_wMISSING.csv")
stateM$population <- as.numeric(gsub("\\,","",stateM$population))
str(stateM)
# check the number of missings
sum(is.na(stateM$population))
# strip the missing data in calculation of means, max, min etc.
mean(stateM$population,na.rm=TRUE) 


################# List #################
stock2 <- read.delim("Dummy_Stock.txt")
stock2 <- read.table("Dummy_Stock.txt",header=TRUE,sep="\t")
head(stock2)
table(stock2$Company)

the.tickers <- unique(stock2$Company)
stock.summary <- list()
for (ticker in the.tickers){
	these <- stock2$Company == ticker
	stock.summary[[ticker]] <- summary(stock2[these,])
}
stock.summary[[5]]









