# ITC 255
# to import data following command used
library(readxl)
myData2 <- read_excel("myData2.xlsx")
View(myData2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# w1P1
# 3 control structure in R sequencing, selection and loop
# 1: sequencing: write the order of the action
x=3
if(x>0)
  print(x)
#2: selection: select a specific action based on condition 
x=6
if(x>0){
  print("positive")
}else {
  print("negative")
}
# or we can add more condition 
x=-34
if(x>0){
  print("positive")
}else if(x<0){
  print("negative")
}else{
  print("not a valid number ")
}
#loop:perform an action repeatadly 
x = c(2,3,4,5)
for (i in 1:4){
  x[i] = x[i]+12
}
x
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#w1P2
#FunctionS
mysum = function(x){
  sm=0
  for (i in 1:length(x)) {
    sm=sm+x[i]
  }
  return(sm)
}
mysum(x)
# function for mean
myMean = function(x){
  sm=0
  n=length(x)
  for (i in 1:length(x)) {
    sm=sm+x[i]
  }
  return(cat("the mean is:", sm/n))
}
myMean(x)
# Max function 
myMax = function(x){
  max=x[1]
  for (i in 2:length(x)) {
    if(x[i]>max) 
      max=x[i]
    print(max)
  }
}
myMax(x)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2live ssession, DFT=disterbuted frequency table
#we can collect data in three type: yes or no, give as the range or give the exact information
#to summarize the data we use  DFT and its in three type: absolute frequency, relative f, epirrical commulative frequency
# descriptive method is generally applied to collect data in one of several ways and summarize them
#univariate descriptive method: DFT, graphs, numerical method
#Dft for ql and qnl
# to find the DFT first we need to find absolute frequency, relative f, epirrical commulative frequency
#1: absolute frequency 
absFreq = table(myData2$`Marital status`) 
absFreq
#2: relative frequency shows the data in percentage
relFreq = round(prop.table(absFreq), 2) # comma 2 means number after round
relFreq
#emoirical commulative frequency = shows the sum of all frequency 
#it is like the second frequency is the of first with second 
# for example the cumFreq for Maried Single is 0.4 for first becuase there wasnt any other variable so its the same as reletive freq but for second one 
# 0.4 + with 0.6 whic becomes one and in total = cumFreq for both is 0.4  1.0
cumFreq= cumsum(relFreq)
cumFreq
# now we can create DFT table from three absFreq, relFreq and cumFreq
dftMaritalStatus = cbind(absFreq, relFreq, cumFreq)
dftMaritalStatus
#disterbution shows how total of abservation disterbuted or devided between two variable
#funtion for DFT
DFTcatagoricalVar = function(x){
  absFreq = table(x)
  relFreq =round(prop.table(absFreq), 3)
  cumFreq = cumsum(relFreq)
  fdtX = cbind(absFreq, relFreq, cumFreq)
  return(fdtX)
}

DFTcatagoricalVar(myData2$`Gender(M/F)`)
# to create DFT for qnt variable we have to first catagorize them then we can make DFT
catagAge = c()
for(i in 1:length(myData2$Age)){
  if(myData2$Age[i]>30){
    catagAge[i]="old"
  }else if(myData2$Age[i]<30 && myData2$Age>18){
    catagAge[i]="avarage"
  }else{
    catagAge[i]="young"
  }
}
amountAge = cbind(myData2$Age, catagAge)
View(amountAge)
DFTcatagoricalVar(catagAge)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#w2P4
#graph:ql pie and bar chart : for qnt = density chart and histogram
graphDFT = DFTcatagoricalVar(myData2$`Education Level`)[,2] # first colum of ftd table
pie(graphDFT,
    col = "red",
    main = "degree disterbution")
# to plot histogram 
hist(myData2$Age,
     col = "pink",
     main = "histo of age")
# density plot
plot(density(myData2$Age),
     col="blue",
     main = "density of age")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#w2P6
#numerical measures 
mean(myData2$Age)
median(myData2$Age)
range(myData2$Age)
sd(myData2$Age) # the avarage distance of each observation from the mean
var(myData2$Age) # squure of sd
mad(myData2$Age) # mean absolute deviation here we use the absolute 
quantile(myData2$Age)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#live session mean, center and qountile
#25% people paide either 2 or less then 2 doller as tips 
#first qountile means that the above
quantile(myData2$Age, 0.8) # shows 80% off all data 
1-quantile(myData2$Age, 0.8) # means that 80% from right side is older then 29 years 
boxplot(myData2$Age,
        horizontal = T,
        col = "red")
# quantile function 
myQnt= function(x,q){
  pr= quantile(x,q)
  return(pr)
}
# creating boxplot
boxplot.stats(myData2$Age)# with outlier 
quantile(myData2$Age, .9)
# to remove the outlier 
AgeWithoutOutlier = myData2$Age[myData2$Age<36]
boxplot(AgeWithoutOutlier, horizontal = T)
# ECDF commulative frequency function = sum of disterbution
# the difference between quantile and ecdf is that ecdf give us the amount and quantile give us percentage
plot(ecdf(myData2$`Weight (in Kg)`),
     col = "blue",
     main="ecdf of wieght",
     xlab = "wieght")
ecdf(myData2$`Weight (in Kg)`)(50)
quantile(myData2$`Weight (in Kg)`, .2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# w3 P2 dplyr
library(dplyr)
str(myData2)
# filter used to create sub data variable
myDataFemaleSet= filter(myData2, `Gender(M/F)`=="F", `Marital status` =="Single" )
head(myDataFemaleSet) # it created another set of data from female who is single

# we can use the filter with and or and not logical operator
unique(myData2$`Gender(M/F)`)
maleGender = filter(myData2, `Gender(M/F)`=="M" & Age =="30")
maleGender
dim(maleGender)

# %in% is used to a specific condtion 
unique(myData2$Age)
age20_30 = filter(myData2, Age %in% c(30,40) )
age20_30

# we can use  %in% in none numerical data as well
unique(myData2$`Education Level`)
mydataEdu = filter(myData2, `Education Level`%in% c("bachlor", "PHD"))
mydataEdu

# we can use <> as well
Age1 =filter(myData2, Age>20)
Age1

# w3P1 bivariate 
# joint table, two qualitative variable
jnGenderMaritalStatus = table(myData2$`Gender(M/F)`, myData2$`Marital status`)
jnGenderMaritalStatus
addmargins(jnGenderMaritalStatus)

# one qnt and one ql
groupGender = group_by(myData2, `Gender(M/F)`)
summarise(groupGender, mean(Age), sd(Age), min(Age), max(Age))


# for two qnt variabe we use the smooth function
scatter.smooth(myData2$Age, myData2$`Hieght (in cm)`)
abline(v=40, col="blue") # v shows the age 
cor(myData2$Age, myData2$`Hieght (in cm)`) #coreletion

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#live session 4
femaleGender = filter(myData2, `Gender(M/F)`!="M" & Age !="30")
femaleGender # means that the gender male and age 30 excluded 

# arrange 
names(myData2)
head(arrange(myData2, desc(`Hieght (in cm)`)))
decending = arrange(myData2, -desc(`Gender(M/F)`))
decending
View(decending)

# select used for creating another data set in colunm 
sameData = select(myData2, c(`Gender(M/F)`,`Hieght (in cm)`, Age))
sameData

dataSome1 = select(myData2, Name, `Gender(M/F)`, everything())
dataSome1

dataSome2 = select(myData2,`Gender(M/F)`, everything())
dataSome2
specificData= select(myData2, Age:`Education Level`)
specificData

specificData2= select(myData2, -(Age:`Education Level`)) # it exclude from age to edu level
specificData2

#rename can help us to rename the colum
dataEdit = rename(myData2,eduLevel =`Education Level` )
dataEdit

#mutate function create another colunm
sumAgeWeight = mutate(myData2, SHW = Age+`Weight (in Kg)`)
sumAgeWeight

#pull function can pull out one variable
genderOnly = pull(myData2, `Gender(M/F)`)
genderOnly

#sample_n select some rows rendomly 
sampleN = sample_n(myData2, 5) # 5 from how many number 
sampleN

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#week 5live session


