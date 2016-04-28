# If any package is not installed in your computer, please use 
## install.packages("package_name") 
# to instALL THE package

# load("alumni_26April.RData")
# save("alumni_26April.RData") ## Save your workspace

## Load necessary packages ##
library(data.table)
library(survival)
library(ggplot2)
library(ggfortify)
library(OIsurv)
library(fmsb)

dataset1 <- read.csv("OriginalAlumniData.csv")

## Number of students of same year who are members

dataset1$Gdate <- as.Date(strptime(dataset1$CnPrAl_Date_graduated,"%m/%d/%Y"))
dataset1$Gmonth <- as.factor(format(dataset1$Gdate, "%m"))
dataset1$Gyear <- format(dataset1$Gdate, "%Y")

dataset1$Mdate <- as.Date(strptime(dataset1$CnMem_1_01_Date_Joined,"%m/%d/%Y"))
dataset1$Mmonth <- as.factor(format(dataset1$Mdate, "%m"))
dataset1$Myear <- format(dataset1$Mdate, "%Y")

dataset1$degree_full <- paste(dataset1$CnPrAl_Degree, dataset1$CnPrAlAttrCat_1_01_Description, sep='_')
dataset1$stateF <- "others"
dataset1$stateF[dataset1$CnAdrPrf_State == "AZ"] <- "Arizona"
dataset1$stateF[dataset1$CnAdrPrf_State == "CA"] <- "California"
dataset1$stateF <- as.factor(dataset1$stateF)
dataset1$state <- relevel(dataset1$stateF, ref = "Arizona")

dataset1$Ethnicity <- "others"
dataset1$Ethnicity[dataset1$CnBio_Ethnicity == "White or Caucasian"] <- "Caucasian"
dataset1$Ethnicity[dataset1$CnBio_Ethnicity == "Hispanic/Latino"] <- "Hispanic"
dataset1$Ethnicity <- factor(dataset1$Ethnicity)
dataset1$Ethnicity <- relevel(dataset1$Ethnicity, ref = "Caucasian")

dataset1$Age <- dataset1$CnBio_Age
dataset1$Age[is.na(dataset1$Age)]  <- mean(dataset1$Age,na.rm=T)

dataset1$one <- 1

dataset1[is.na(dataset1$Mdate),c("Same_day")] <- 0	
dataset1[is.na(dataset1$Mdate),c("Same_classroom_same_day")] <- 0 

dataset1$surv <- as.Date(dataset1$Mdate)
dataset1$surv[is.na(dataset1$Mdate)] <-  as.Date(strptime("04/01/2016","%m/%d/%Y"))

dataset1$dtm <- dataset1$surv - dataset1$Gdate   ## Days to membership (with censor data)

dataset1$cens[!is.na(dataset1$Mdate)] <- 1
dataset1$cens[is.na(dataset1$Mdate)] <- 0

dataset1$group <- dataset1$CnMem_1_01_Standing
dataset1$group[dataset1$CnMem_1_01_Standing == "Lapsed"] <- "Dropped"
dataset1$group <- factor(dataset1$group)
levels(dataset1$group)

dataset1$Gyear <- as.numeric(dataset1$Gyear)

dataset1$Gmonth[dataset1$Gmonth=="01"] <- "12"
dataset1$Gmonth[dataset1$Gmonth=="08"] <- "05"
dataset1$Gmonth[dataset1$Gmonth=="06"] <- "05"
dataset1$Gmonth <- factor(dataset1$Gmonth)
levels(dataset1$Gmonth) <- c("Fall","Spring")
dataset1$GraduationPeriod <- dataset1$Gmonth 

dataset1$Type_membership <- factor(dataset1$Type_membership, levels = c("Annual","Dropped","Life"))

dataset1$time <- as.numeric(dataset1$dtm)
dataset1$Gender <- dataset1$CnBio_Gender

summary(dataset1$Membership_desc)
dataset1$Membership_desc[dataset1$Membership_desc==""] <- NA
dataset1$Membership_desc <- relevel(dataset1$Membership_desc,ref = "Paid")

dataset1 <- dataset1[!is.na(dataset1$time),]

dataset1 <- dataset1[order(dataset1$Mdate),]

dat_dt <- data.table(dataset1)
dat_dt[, Same_city_members := cumsum(one)-1, by=list(CnAdrPrf_City)]
dat_dt[, Same_degree_year_members := cumsum(one)-1, by=list(degree_full,Gyear)]

write.csv(dat_dt,"ProcessedData.csv",row.names=FALSE,na="")
