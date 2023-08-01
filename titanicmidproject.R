1+5
 titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic
str(titanic)
head(titanic)
names(titanic)
summary(titanic)

find_mode <-function(x){
  u<-unique(x)
  tab <-tabulate(match(x,u))
  u[tab==max(tab)]
}
########################  

########no need
titanic$age <- as.numeric(as.character(titanic$age))
titanic

##### need sapply to see attributes type...
sapply(titanic, class)



########################################

###############....................mean,median of age.............
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

age_mean <- mean(titanic$age, na.rm = TRUE)
age_median <- median(titanic$age, na.rm = TRUE)
age_mode <- find_mode(titanic$age,na)

print(age_mean)
print(age_median)
print(age_mode)

##########################.................mean,median,mode of sibsp
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}


sibsp_mean <- mean(titanic$sibsp, na.rm = TRUE)
sibsp_median <- median(titanic$sibsp, na.rm = TRUE)
sibsp_mode <- find_mode(titanic$sibsp)

print(sibsp_mean)
print(sibsp_median)
print(sibsp_mode)
##########.................mean,median,mode of parch
parch_mean <- mean(titanic$parch , na.rm = TRUE)
parch_median <- median(titanic$parch , na.rm = TRUE)
parch_mode <- find_mode(titanic$parch )

print(parch_mean)
print(parch_median)
print(parch_mode)
####....
fare_mean <- mean(titanic$fare  , na.rm = TRUE)
fare_median <- median(titanic$fare  , na.rm = TRUE)
fare_mode <- find_mode(titanic$fare  )

print(fare_mean)
print(fare_median)
print(fare_mode)
###,,,,
survived_mean <- mean(titanic$survived  , na.rm = TRUE)
survived_median <- median(titanic$survived  , na.rm = TRUE)
survived_mode <- find_mode(titanic$survived  )

print(survived_mean)
print(survived_median)
print(survived_mode)
##############.............. Measure of Spread (range and standard Deviation) 

age_range= range(titanic$age)
print(age_range)

age_range <- range(titanic$age, na.rm = TRUE)
print(age_range)

age_sd <- sd(titanic$age, na.rm = TRUE)
print(age_sd)

##
sibsp_range <- range(titanic$sibsp, na.rm = TRUE)
print(sibsp_range)

sibsp_sd <- sd(titanic$sibsp, na.rm = TRUE)
print(sibsp_sd)

##
parch_range <- range(titanic$parch, na.rm = TRUE)
print(parch_range)

parch_sd <- sd(titanic$parch, na.rm = TRUE)
print(parch_sd)
##
fare_range <- range(titanic$fare, na.rm = TRUE)
print(fare_range)

fare_sd <- sd(titanic$fare, na.rm = TRUE)
print(fare_sd)

##
survived_range <- range(titanic$survived, na.rm = TRUE)
print(survived_range)

survived_sd <- sd(titanic$fsurvived, na.rm = TRUE)
print(survived_sd)


##############find the missing value for all attributes..................................................

number_of_missing_value=colSums(is.na(titanic))
number_of_missing_value 
##
titanic[!complete.cases(titanic),]
##
missing_gender=which(is.na(titanic$gender))
missing_gender
missing_age=which(is.na(titanic$age))
missing_age
###
############### Handle gender attributes invalid value

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(titanic$gender)
most_frequent_gender

##
titanic$gender[10]<-most_frequent_gender
print(titanic)

##
titanic$gender[is.na(titanic$gender)]<-most_frequent_gender
print(titanic)
####
###############...........Detect the outlier as a missing value. 
sort(titanic$age)
###
summary(titanic)
###
titanic_outlier=subset(titanic, age<=19)
titanic_outlier
###
titanic_outlier_location=which(titanic$age<19)
titanic_outlier_location
###
titanic$age[titanic_outlier_location]<-NA
print(titanic)

####
titanic$who<-factor(titanic$who,levels=c("man","woman","child"),labels=c(1,2,3))
print(titanic$who)
#################...........missing value hamdeling
#### 1st rule
remove_missing<-na.omit(titanic)
remove_missing
remove_missing <- na.omit(titanic)
print(remove_missing)
########2nd rule
age_mean=mean(titanic$age,na.rm=T)
recover_missing_age_mean=titanic$age[is.na(titanic$age)]<-age_mean
recover_missing_age_mean
#
age_mean=mean(titanic$age,na.rm=T)
recover_missing_age_mean=titanic$age[is.na(titanic$age)]<-age_mean
recover_missing_age_mean




###########################......again
print(titanic)
########
##################################################################................again
setwd("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project")
titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic

###
number_of_missing_value=colSums(is.na(titanic))
number_of_missing_value 
##
titanic[!complete.cases(titanic),]
##
missing_gender=which(is.na(titanic$gender))
missing_gender
missing_age=which(is.na(titanic$age))
missing_age
###
############### Handle gender attributes invalid value

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(titanic$gender)
most_frequent_gender
####
titanic$gender[10]<-most_frequent_gender
print(titanic)

##
titanic$gender[is.na(titanic$gender)]<-most_frequent_gender
print(titanic)
#####
sort(titanic$age)
###
summary(titanic)
##
###
titanic_outlier=subset(titanic, age<=19)
titanic_outlier
###
titanic_outlier_location=which(titanic$age<19)
titanic_outlier_location
###
titanic$age[titanic_outlier_location]<-NA
print(titanic)

#####
titanic$who<-factor(titanic$who,levels=c("man","woman","child"),labels=c(1,2,3))
print(titanic$who)
##
#################...........missing value handeling ....fst remove missing value
#### 1st rule>>.........................................................omit 
remove_missing<-na.omit(titanic)
remove_missing
remove_missing <- na.omit(titanic)
print(remove_missing)
########2nd rule
age_mean=mean(titanic$age,na.rm=T)
recover_missing_age_mean=titanic$age[is.na(titanic$age)]<-age_mean
recover_missing_age_mean

#########################################for finding missing value (ii. Recover missing values with the mean value.)..........

##################################################################................again
setwd("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project")
titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic
##
number_of_missing_value=colSums(is.na(titanic))
number_of_missing_value 
##
titanic[!complete.cases(titanic),]
##
missing_gender=which(is.na(titanic$gender))
missing_gender
missing_age=which(is.na(titanic$age))
missing_age
###
############### Handle gender attributes invalid value......................

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(titanic$gender)
most_frequent_gender
####
titanic$gender[10]<-most_frequent_gender
print(titanic)

##
titanic$gender[is.na(titanic$gender)]<-most_frequent_gender
print(titanic)
#####
sort(titanic$age)
###
summary(titanic)
##
titanic_outlier=subset(titanic, age<=19)
titanic_outlier
###
titanic_outlier_location=which(titanic$age<19)
titanic_outlier_location
###
titanic$age[titanic_outlier_location]<-NA
print(titanic)

#####
titanic$who<-factor(titanic$who,levels=c("man","woman","child"),labels=c(1,2,3))
print(titanic$who)
##
########2nd rule
age_mean=mean(titanic$age,na.rm=T)
recover_missing_age_mean=titanic$age[is.na(titanic$age)]<-age_mean
recover_missing_age_mean
print(titanic)
#####  ....normaliza................
min_max_norm <- function(x) { (x - min(x)) / (max(x) - min(x)) }
titanic <- as.data.frame(lapply(titanic[3:5], min_max_norm))
titanic
##
titanic $sibsp = as.numeric(format(round(titanic $sibsp ,0)))
titanic $parch = as.numeric(format(round(titanic $parch,0)))
titanic $fare = as.numeric(format(round(titanic $fare,0)))
titanic

####################
setwd("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project")
titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic

##
min_max_norm <- function(x) { (x - min(x)) / (max(x) - min(x)) }
titanic <- as.data.frame(lapply(titanic[3:5], min_max_norm))
titanic
###
setwd("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project")
titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic

titanic $sibsp = as.numeric(format(round(titanic $sibsp ,0)))
titanic $parch = as.numeric(format(round(titanic $parch,0)))
titanic $fare = as.numeric(format(round(titanic $fare,0)))

print (titanic)


####################.............................................................
titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic
###### for noicy value:::: noynaaa
noise_value <- sd(titanic$fare)
str(titanic)
noise_value <- sd(titanic$fare)


#####....................
# Set the mean and standard deviation




####################.............
mean_val <- 33.32837
sd_val <- 45.7735

# Generate random data from a normal distribution
age_data <- rnorm(1000, mean = mean_val, sd = sd_val)

# Create a histogram
hist(age_data, breaks = 30,
     main = "Histogram of Age",
     xlab = "Age", ylab = "Frequency",
     col = "blue", border = "white")


####.........age range ,age sd, mean histo
mean_val <- 33.32837
sd_val <- 45.7735
age_range <- c(0, 83, 455)

# Generate random data within the specified age range
age_data <- runif(1000, min = age_range[1], max = age_range[2])

# Create a histogram
hist(age_data, breaks = age_range[3],
     main = "Histogram of Age",
     xlab = "Age", ylab = "Frequency",
     col = "blue", border = "white")





####............................................normal histogram , scatter

hist(titanic$age)
barplot(table(titanic$survived))
plot(titanic$age, titanic$fare)

###################################
titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic

##

####.........age range ,age sd, mean histo
mean_val <- 33.32837
sd_val <- 45.7735
age_range <- c((0.83), 455)

# Generate random data within the specified age range
age_data <- runif(1000, min = age_range[1], max = age_range[2])

# Create a histogram
hist(age_data,
     main = "Histogram of Age",
     xlab = "Age", ylab = "Frequency",
     col = "blue", border = "white")
######...for fare
mean_val <- 26.58762
sd_val <- 34.82165
fare_range <- c(0, 263)

# Generate random data within the specified fare range
fare_data <- runif(1000, min = fare_range[1], max = fare_range[2])

# Create a histogram
hist(fare_data,
     main = "Histogram of Fare",
     xlab = "Fare", ylab = "Frequency",
     col = "blue", border = "white")
#####.....for survived
mean_val <- 0.344
survived_range <- c(0, 1)

# Generate random data within the specified survival range
survived_data <- sample(survived_range, 1000, replace = TRUE, prob = c(1 - mean_val, mean_val))

# Create a histogram
hist(survived_data, breaks = c(survived_range, survived_range[2] + 1),
     main = "Histogram of Survived",
     xlab = "Survived", ylab = "Frequency",
     col = "blue", border = "white")





####.........................


mean_val <- 0.392
sd_val <- 0.8252637
parch_range <- c(0, 5)

parch_data <- runif(1000, min = parch_range[1], max = parch_range[2])

hist(parch_data,
     main = "Histogram of Parch",
     xlab = "Parch", ylab = "Frequency",
     col = "blue", border = "white")

####################################..............................

#.........for ................Annotate 


titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic

#####Annotate............

titanic$class<-factor(titanic$class,levels=c("First","Second","Third"),labels=c(1,2,3))
print(titanic$class)

print(titanic)

titanic$who<-factor(titanic$who,levels=c("man","woman","child"),labels=c(1,2,3))
print(titanic$who)

print(titanic)


#############################    missing value ..............

titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic
number_of_missing_value=colSums(is.na(titanic))
number_of_missing_value
#

titanic[!complete.cases(titanic),]
#
missing_gender=which(is.na(titanic$gender))
missing_gender
missing_age=which(is.na(titanic$age))
missing_age
##
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(titanic$gender)
most_frequent_gender

titanic$gender[10]<-most_frequent_gender
print(titanic)
##
titanic$gender[is.na(titanic$gender)]<-most_frequent_gender
print(titanic)

##
sort(titanic$age)

summary(titanic)

#
titanic_outlier=subset(titanic, age<=19)
titanic_outlier

##
titanic_outlier_location=which(titanic$age<19)
titanic_outlier_location
##
titanic$age[titanic_outlier_location]<-NA
print(titanic)

###......................i.Delete the rows with missing values

remove_missing <- na.omit(titanic)
print(remove_missing)

####.............................ii) Recover missing values with the mean value.


titanic <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/mid project/titanic.csv")
titanic
number_of_missing_value=colSums(is.na(titanic))
number_of_missing_value
#

titanic[!complete.cases(titanic),]
#
missing_gender=which(is.na(titanic$gender))
missing_gender
missing_age=which(is.na(titanic$age))
missing_age
##
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
most_frequent_gender=find_mode(titanic$gender)
most_frequent_gender

titanic$gender[10]<-most_frequent_gender
print(titanic)
##
titanic$gender[is.na(titanic$gender)]<-most_frequent_gender
print(titanic)

##
sort(titanic$age)

summary(titanic)

#
titanic_outlier=subset(titanic, age<=19)
titanic_outlier

##
titanic_outlier_location=which(titanic$age<19)
titanic_outlier_location
##
titanic$age[titanic_outlier_location]<-NA
print(titanic)
###.....................................
age_mean=mean(titanic$age,na.rm=T)
recover_missing_age_mean=titanic$age[is.na(titanic$age)]<-age_mean
recover_missing_age_mean

print(titanic)

####.........




















