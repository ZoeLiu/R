##########################################################################################
# This analysis used hierachical clustering and NLP to categorize the services 
##########################################################################################

# setup
library(stringr)
library(ClustOfVar)
library(ggdendro)
library(ggplot2)
#install.packages('ClustOfVar')
#install.packages('tm')
library(tm)
library(Matrix)

# read in data
stylist <- read.csv('stylist_info.csv', stringsAsFactors = F)
app <- read.csv('appointment_data.csv',stringsAsFactors = F)
app2 <- merge(x=app, y=stylist, by=c("provider_id","service_name"))
table(app2$cost.x==app2$cost.y)
table(app2$cost.x>app2$cost.y)
#str(stylist)
#summary(stylist)
# length(unique(stylist$service_name))
# length(unique(app$service_name))
# length(is.element(unique(app$service_name),unique(stylist$service_name)))
# length(intersect(unique(stylist$service_name),unique(app$service_name)))

#str(app)
#summary(app)

########################  Pre- Processing of words ###########################################################
# category name adjustment
#table(grep("color balancing", tolower(stylist$service_name), value=T))
# remove 'and' where it does not mean two services' connections
service_name <- gsub("color balancing -mid-lengths and ends", "color balancing",tolower(app2$service_name))
service_name <- gsub("wigs and units","wigs units", service_name)

# mis-spelling and other adjustment
service_name <- gsub("kid's cut \\(12 & under\\)", "child's cut", service_name)
service_name <- gsub("haurcut|hair cut","haircut", service_name)
service_name <- gsub("\\Wcut|hc"," haircut", service_name)
service_name <- gsub("^cut","haircut", service_name)
service_name <- gsub("haircuts","haircut", service_name)
service_name <- gsub("high lighting|highlightd|hilite|hi-lites|hi-lights|hilights|highligt|hilight|highlightt|highligh\\W|highlights|high lights","highlight", service_name)
service_name <- gsub("partical|parcial","partial", service_name)
service_name <- gsub("up do|updo's|up-do style|up-do|up - do, or style|updo hair styling|up styleupdo\\/upstyle|updos|updos set","up-do's", service_name)
service_name <- gsub("blowdry|bowdry","blow dry", service_name)
service_name <- gsub("allover color","all over color",service_name)
service_name <- gsub("blowout", "blow out", service_name)
service_name <- gsub("conditioning treatment|conditioning|cond. treatment|condition","cond.treatment", service_name)
service_name <- gsub("complementary, blow dry styl","complementary blow dry styling", service_name)
service_name <- gsub("ironwork","iron work", service_name)
service_name <- gsub("child\\W|kid","children", service_name)
service_name <- gsub("touch up|touch-up","touchup", service_name)
service_name <- gsub("re touch|re-touch","touchup", service_name)
service_name <- gsub("up style","upstyle", service_name)
service_name <- gsub("flatiron","flat iron", service_name)
service_name <- gsub("make up|make-up","makeup", service_name)
service_name <- gsub("colour","color",service_name)
service_name <- gsub("at home","home", service_name)
service_name <- gsub("1/2","half", service_name)
service_name <- gsub("balyágé|balyage technique","balyage", service_name)

# remove the part 'w/o xxx, no xxx'
service_name <- gsub("w/o haircut","", service_name)
service_name <- gsub("(no hair added)|(no haircut)|(no exts.)","", service_name)

# replace "w/","&","/","," with "and"
service_name <- gsub("&|\\,|w/| with |\\/|\\+| n "," and ", service_name)

# remove punctuations
service_name <- gsub("'s|'s"," ",service_name)
service_name <- gsub("[[:punct:]]"," ",service_name)
#tail(service_name,50)


# More stems on words
service_name <- gsub("woman","women",service_name)
service_name <- gsub("womens","women",service_name)
service_name <- gsub("man|gentlemen","men",service_name)
service_name <- gsub("mens","men",service_name)
service_name <- gsub("childrens","children", service_name)
service_name <- gsub("children 13 yr|children up to 13","children haircut",service_name)
service_name <- gsub("waxing","wax", service_name)
service_name <- gsub("tinting","tint", service_name)
service_name <- gsub("styling","style", service_name)
service_name <- gsub("trims","trim", service_name)
service_name <- gsub("arms","arm", service_name)
service_name <- gsub("legs","leg", service_name)

# remove extra white spaces, leading and trailing spaces
service_name <- gsub("\\s+"," ",service_name)
service_name <- gsub("^\\s+","",service_name)
service_name <- gsub("\\s+$","",service_name)

# create a vector of unique service names 
unique_names <-as.data.frame(unique(service_name))

#service_name <- wordStem(service_name,"english")

########################  Use all available data to categorize services  ######################3########
# serv <- as.data.frame(unique(cbind(stylist[,c("service_group_id","provider_id","cost","duration_minutes")], 
#                                    service_name)), stringsAsFactors = F)
# 
# serv$service_group_id <- as.factor(serv$service_group_id)
# serv$provider_id <- as.factor(serv$provider_id)
# 
# X.quanti <- serv[,c("cost","duration_minutes")]
# X.quali <- serv[,c("provider_id","service_group_id")]
# tree <- hclustvar(X.quanti,X.quali)
# str(X.quali)
# plot(tree)
# part_hier <- cutreevar(tree,3)
# str(part_hier)
# 
# part_hier$var$"cluster1"
# hc <-hclust(dist(X.quanti))
# plot(hc)


###### Create service group ID distance matrix
# create dataframe that contains unique combination of service names and service group id
#library(reshape2)
serv <- as.data.frame(unique(cbind(app2$service_group_id, service_name)), stringsAsFactors = F)
names(serv)[1] <- c('service_group_id')

# sort the data frame by service_name
serv_sort <- serv[order(serv$service_name),]
# remove the service name with blanks
serv_sort <- serv_sort[!serv_sort$service_name=="",]
#length(serv_sort$service_group_id)

A1 <- model.matrix(service_name ~ service_group_id, serv_sort)
# dim(A)
# A[2,"service_group_id65920"]
# A[3,"service_group_id68906"]

serv_id_wide <- data.frame(cbind(serv_sort, A1[,2:ncol(A1)]))
serv_id_wide$service_group_id <- NULL
serv_id_sums <- aggregate( .~ service_name, data=serv_id_wide, sum)


###### Create provider ID distance matrix
serv2 <- as.data.frame(unique(cbind(app2$provider_id, service_name)), stringsAsFactors = F)
names(serv2)[1] <- c('provider_id')

# sort the data frame by service_name
serv_sort2 <- serv2[order(serv2$service_name),]
# remove the service name with blanks
serv_sort2 <- serv_sort2[!serv_sort2$service_name=="",]

A2 <- model.matrix(service_name ~ provider_id, serv_sort2)
# dim(A)
# A[2,"service_group_id65920"]
# A[3,"service_group_id68906"]

serv_id_wide2 <- data.frame(cbind(serv_sort2, A2[,2:ncol(A2)]))
serv_id_wide2$provider_id <- NULL
serv_id_sums2 <- aggregate( .~ service_name, data=serv_id_wide2, sum)

serv_id_sums_all <- merge(serv_id_sums,serv_id_sums2, by="service_name")
rownames(serv_id_sums_all) <- gsub(" ",".",serv_id_sums_all$service_name)


########################## Find most requent terms in category names 
########################## Using Term-Document Matix ########################################
corp_serv <- Corpus(VectorSource(serv_id_sums$service_name))
dtm <- DocumentTermMatrix(corp_serv)
#most_freq_30 <- findFreqTerms(dtm,30) #occur at least 30 times
most_freq_30 <- findFreqTerms(dtm,30) #occur at least 5 times
#most_freq_30 <- most_freq_30[-1]

#create binary variable for each frequent terms
for (i in 1:length(most_freq_30)){
  #print (most_freq_30[i])
     serv_id_sums_all[, most_freq_30[i]] <- as.integer(grepl(most_freq_30[i],serv_id_sums_all$service_name))
}

hc.complete <- hclust(dist(serv_id_sums_all[,-1]), method="complete")
#plot(hc.complete)

# View dendrogram
dhc <- as.dendrogram(hc.complete)
# Rectangular lines
ddata <- dendro_data(dhc, type = "rectangle")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))
p

# attach label
serv_id_sums_all$label <- cutree(hc.complete,30)
name.wLabels <- data.frame(serv_id_sums_all[,c('service_name','label')]) 

d <- cut(as.dendrogram(hc.complete), h=3.6)
d
plot(d$lower[[6]])
plot(d$lower[[20]])
plot(d$upper)
plot(d$lower[[30]])


# library(NbClust)
# hc <- NbClust(serv_id_sums2[,-1], distance = "euclidean", min.nc=2, max.nc=100,
#               method = "complete", index = "ch")
# #hc$All.index
# hc$Best.nc
# hc$Best.partition

# add label back to original data
app2$adj.service_name <- service_name
app3 <- merge(app2, serv_id_sums_all[,c('service_name','label')], 
              by.x="adj.service_name", by.y="service_name", all.x=T)
tab <-prop.table(table(app3$label))
write.csv(tab,'category_distribution.csv')

# average cost by service
avgcost <- aggregate(cost.y ~ adj.service_name, data=app3, FUN=mean)
write.csv(avgcost,'average cost per service.csv')

tail(names(serv_id_sums),30)
names(serv_id_sums[,431:456])
str(hc.complete)








###### Use service group ID to categorize services
# create dataframe that contains unique combination of service names and service group id
#library(reshape2)
serv <- as.data.frame(unique(cbind(stylist$service_group_id, service_name)), stringsAsFactors = F)
names(serv)[1] <- c('service_group_id')

# sort the data frame by service_name
serv_sort <- serv[order(serv$service_name),]
# remove the service name with blanks
serv_sort <- serv_sort[!serv_sort$service_name==" ",]
#length(serv_sort$service_group_id)

A1 <- model.matrix(service_name ~ service_group_id, serv_sort)
# dim(A)
# A[2,"service_group_id65920"]
# A[3,"service_group_id68906"]

serv_id_wide <- data.frame(cbind(serv_sort, A1[,2:ncol(A1)]))
serv_id_wide$service_group_id <- NULL
serv_id_sums <- aggregate( .~ service_name, data=serv_id_wide, sum)

# length(unique(serv_sort$service_name))
# serv_id_sums[2,"service_group_id65920"]
# serv_id_sums[2,"service_group_id68906"]

hc.complete <- hclust(dist(serv_id_sums[,-1]), method="complete")
plot(hc.complete)
serv_id_sums$label <- cutree(hc.complete,10)
d <- cut(as.dendrogram(hc.complete), h=8)
d
plot(d$upper)
hc <- hclustvar(X.quali=serv$service_group_id)

hc <- NbClust(x, distance = "euclidean", min.nc=2, max.nc=150,
        method = "complete", index = "ch")
res$All.index
hc$Best.nc
res$Best.partition



name.wLabels <- serv_id_sums[,c('service_name','label')]
km <- kmeans(dist(serv_id_sums[,-1]),3)
name.wLabels <- as.data.frame(cbind(serv_id_sums[,c('service_name')],km$cluster))



################## Use provider ID to categorize services  #########################################
# create dataframe that contains unique combination of service names and service group id
#library(reshape2)
serv <- as.data.frame(unique(cbind(stylist$provider_id, service_name)), stringsAsFactors = F)
names(serv)[1] <- c('provider_id')

# sort the data frame by service_name
serv_sort <- serv[order(serv$service_name),]
# remove the service name with blanks
serv_sort <- serv_sort[!serv_sort$service_name=="",]
head(unique(serv_sort$service_name))
#length(serv_sort$provider_id)

A2 <- model.matrix(service_name ~ provider_id, serv_sort)
# dim(A)
# A[2,"provider_id65920"]
# A[3,"provider_id68906"]

serv_id_wide <- data.frame(cbind(serv_sort, A2[,2:ncol(A2)]))
serv_id_wide$provider_id <- NULL
serv_id_sums <- aggregate( .~ service_name, data=serv_id_wide, sum)

# length(unique(serv_sort$service_name))
# serv_id_sums[2,"provider_id65920"]
# serv_id_sums[2,"provider_id68906"]

hc.complete <- hclust(dist(serv_id_sums[,-1]), method="complete")
serv_id_sums$label <- cutree(hc.complete,5)

name.wLabels <- serv_id_sums[,c('service_name','label')]
km <- kmeans(dist(serv_id_sums[,-1]),3)
name.wLabels <- as.data.frame(cbind(serv_id_sums[,c('service_name')],km$cluster))


####################### Use both provider id and service group ID to categorize services  ###################
# create dataframe that contains unique combination of service names and service group id
#library(reshape2)
serv <- as.data.frame(unique(cbind(stylist[,c('service_group_id','provider_id')], service_name), stringsAsFactors = F))

# sort the data frame by service_name
serv_sort <- serv[order(serv$service_name),]
# remove the service name with blanks
serv_sort <- serv_sort[!serv_sort$service_name=="",]
#length(serv_sort$service_group_id)

A1 <- model.matrix(service_name ~ service_group_id, serv_sort)
A2 <- model.matrix(service_name ~ provider_id, serv_sort)
# dim(A)
# A[2,"service_group_id65920"]
# A[3,"service_group_id68906"]

serv_id_wide <- data.frame(cbind(serv_sort, A[,2:ncol(A)]))
#serv_id_wide$service_group_id <- NULL
#serv_id_sums <- aggregate( .~ service_name, data=serv_id_wide, sum)

# length(unique(serv_sort$service_name))
# serv_id_sums[2,"service_group_id65920"]
# serv_id_sums[2,"service_group_id68906"]

hc.complete <- hclust(dist(serv_id_wide[,1:3]), method="complete")
serv_id_wide$label <- cutree(hc.complete,3)

name.wLabels <- serv_id_wide[,c('service_name','label')]
km <- kmeans(dist(serv_id_sums[,-1]),3)
name.wLabels <- as.data.frame(cbind(serv_id_sums[,c('service_name')],km$cluster))




##############################################################################################


table(grep("brazilians",service_name, value=T))


head(service_name,50)

# check each service_group_id is unique within each provider
length(unique(stylist$provider_id))
length(unique(stylist$service_group_id))
dim(unique(stylist[,c('provider_id','service_group_id')]))

table(grep("kid", service_name,value=T))





########################## Create Term-Document Matix ########################################
## Create corpus
corp_serv <- Corpus(VectorSource(unique_names))
## Strip white spaces 
new_serv <- tm_map(corp_serv, stripWhitespace)
## Stem words
new_serv <-tm_map(new_serv, stemDocument)
dtm <- DocumentTermMatrix(new_serv)
#inspect(new_serv)
writeLines(as.character(new_serv[[1010]]))
#inspect(dtm)
#findFreqTerms(dtm,5)

######################### Hierachical Clustering ##########################################
x <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dimnames=dimnames(dtm))
hc.complete <- hclust(dist(x), method="complete")
hc.average <- hclust(dist(x), method="average")
hc.single <- hclust(dist(x), method="single")
#plot(hc)

labels <- cutree(hc.complete,15)
labels <- cutree(hc.average,3)
labels <- cutree(hc.single,3)
name.wLabels <- as.data.frame(cbind(unique_names,labels))




table(grep("[0-9]", service_name,value=T))
# table(grep("up, up & away", tolower(stylist$service_name),value=T))
# table(grep("away", service_name,value=T))
table(grep("up", service_name,value=T))






# remove all "w/o haircut"
#table(grep('w/o', tolower(stylist$service_name), value=T))

#table(grep('w/o', service_name, value=T))
#table(grep("balyage", service_name, value=T))

split1 <- str_split_fixed(service_name,"&| and |w/|| with |\\+",2)
split2 <- str_split_fixed(split1[,2], "&| and |w/| with |\\+",2)
catname1 <- str_trim(split1[,1])
catname2 <- str_trim(split2[,1])
catname3 <- str_trim(split2[,2])

table(catname3)
table(catname2)
table(catname1)


check <-grep("&|w/|\\+|and", catname3)
check2 <- grep("and", catname2)

grep("Color")


head(catname1,50)
head(catname2,50)
head(catname3,20)
head(stylist$service_name,50)


####  Natural Language Processing
install.packages("RTextTools")
install.packages("RSiteCatalyst")
library(RTextTools)
library(RSiteCatalyst)
str(service_name)

## category based on original service name
dtm <- create_matrix(tolower(stylist$service_name), stemWords=TRUE, removeStopwords=FALSE, minWordLength=1,removePunctuation= TRUE)
kmeans3 <- kmeans(dtm,3)
kw_with_cluster <- as.data.frame(cbind(stylist$service_name, kmeans3$cluster))
names(kw_with_cluster) <- c("service","kmeans3")
cluster1 <- subset(kw_with_cluster, subset=kmeans3==1)
cluster2 <- subset(kw_with_cluster, subset=kmeans3==2)
cluster3 <- subset(kw_with_cluster, subset=kmeans3==3)

kw_with_cluster <- as.data.frame(cbind(kw_with_cluster, kmeans3$cluster))

cost.df <- data.frame()
for (i in 1:100){
  
  kmeans <- kmeans(x=dtm, centers=i, iter.max=100)
  cost.df <- rbind(cost.df, cbind(i, kmeans$tot.withinss))
  
}
names(cost.df) <- c("cluster", "cost")
plot(cost.df$cluster, cost.df$cost)

kw_with_cluster <- as.data.frame(cbind(kw_with_cluster, kmeans$cluster))


# using hierachical clustering
install.packages('Matrix')
library(Matrix)
mdtm <- sparseMatrix( i=dtm$i, j=dtm$j, x=dtm$v, dimnames = dimnames(x) ) 
hc.complete <- hclust(dtm, method="complete")


## category based on splitted service name
services <- as.data.frame(t(cbind(t(catname1), t(catname2), t(catname3))))
# remove blank rows
services <- services[!services$V1=="",]
names(kmeans3)
kmeans3$centers

str(services)
dtm2 <- create_matrix(services, stemWords=TRUE, removeStopwords=FALSE, minWordLength=1,removePunctuation= TRUE)
cat_kmeans3 <- kmeans(dtm2,3)

head(table(catname1[,1]), 100)

catname1 <- strsplit(stylist$service_name,"and|w/")
do.call(rbind,catname1)


# nums <- seq(1:length(serv_sort$service_group_id))
# dummy.names <- paste0("service",nums)


# for (i in as.numeric(serv_sort$service_group_id)){
#   serv_sort[,c(dummy.names[i])] <-0
#   if (as.numeric(serv_sort$service_group_id)==i) {
#        serv_sort[,c(dummy.names[i])] <-1
# }
# }
#serv_id_wide <- reshape(data=serv_sort, v.names="count",timevar="service_group_id", idvar="service_name", direction="wide")

