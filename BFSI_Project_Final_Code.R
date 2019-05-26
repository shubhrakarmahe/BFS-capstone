###########################################################################################
###########################################################################################
#-----------------------------------------------------------------------------------------#
#CredX is a leading credit card provider that gets thousands of credit card applicants
#every year. But in the past few years, it has experienced an increase in credit loss. The
#CEO believes that the best strategy to mitigate credit risk is to 'acquire the right
#customers'.
###########################################################################################
#                               Understanding the data
###########################################################################################
#There are two data sets in this project - demographic and credit bureau data.
#Demographic/application data: This is obtained from the information provided by the
#applicants at the time of credit card application. It contains customer-level information
# on age, gender, income, marital status, etc.
#Credit bureau: This is taken from the credit bureau and contains variables such as 'number
# of times 30 DPD or worse in last 3/6/12 months', 'outstanding balance', 'number of
# trades', etc.
###########################################################################################
#                               Data cleaning and preparation
###########################################################################################
#Create a master file with all the relevant variables and conduct the necessary data
#quality checks and cleaning. In credit risk analytics, the weight of evidence (WOE) (and,
#equivalently, information value analysis) is often used to identify the important
# variables.
###########################################################################################
#                                     Model building
###########################################################################################
#Demographic data model: Model on demographic data
#Model using both demographic and credit bureau data : Model on both the data set
###########################################################################################
#                                   Model evaluation
###########################################################################################
#Evaluate the models using relevant metrics and report the results. As a part of model
#validation, predict the likelihood of default for the rejected candidates and assess
# whether the results correspond to your expectations.
###########################################################################################
#                               Application scorecard
###########################################################################################
#Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400
#doubling every 20 points.
###########################################################################################
#                     Assessing the financial benefit of your project
###########################################################################################
#assess and explain the potential financial benefit of your project to the management
#of the bank.
###########################################################################################
###########################################################################################


###########################################################################################
###########################################################################################
#                                   Intall & Load libraries
###########################################################################################
###########################################################################################
load.libraries <-
  c(
    'readr',
    'dplyr',
    'ggplot2',
    'caret',
    'kernlab',
    'gridExtra',
    'dummies',
    'caTools',
    'MASS',
    'car',
    'e1071',
    'randomForest',
    'grid',
    'Information',
    'highr',
    'corrplot',
    'cowplot',
    'rattle',
    'gbm',
    'unbalanced'
  )
install.libs <-
  load.libraries[!load.libraries %in% installed.packages()]

for (lib in install.libs)
  install.packages(lib, dependencies = TRUE)

sapply(load.libraries, require, character = TRUE)

###########################################################################################
###########################################################################################
#                                   Data loding
###########################################################################################
###########################################################################################

#Reading demograpic data  and credit bureau data.

demographic_df <-
  read.csv(
    file = 'Demographic data.csv',
    header = T,
    stringsAsFactors = T,
    na.strings = c("NA")
  )
creditbureau_df <-
  read.csv(
    file = 'Credit Bureau data.csv',
    header = T,
    stringsAsFactors = T,
    na.strings = c("NA")
  )

nrow(demographic_df)
#71295
nrow(creditbureau_df)
#71295
###########################################################################################
###########################################################################################
#                           Data Cleaning and Basic processing
###########################################################################################
###########################################################################################
## Duplicate Check
sum(duplicated(demographic_df$Application.ID))
###3
sum(duplicated(creditbureau_df$Application.ID))
###3
length(unique(tolower(demographic_df$Application.ID)))
##71292
length(unique(tolower(creditbureau_df$Application.ID)))
##71292

#finding duplicate id from both data sets
demographic_df[duplicated(demographic_df$Application.ID), ]
creditbureau_df[duplicated(creditbureau_df$Application.ID), ]
### 0 rows: No duplicates left


#Removing the duplicate  application id
demographic_df <-
  demographic_df[-which(duplicated(demographic_df$Application.ID) == T),]
creditbureau_df <-
  creditbureau_df[-which(duplicated(creditbureau_df$Application.ID) == T),]

#uniquie id's
nrow(demographic_df)
#71292
nrow(creditbureau_df)
#71292
###########################################################################################
# merging  the two datasets by common attributes
combined_df <- merge(
  x = unique(demographic_df)
  ,
  y = unique(creditbureau_df)
  ,
  by = c("Application.ID", "Performance.Tag")
)

nrow(combined_df)
#71292
###########################################################################################
master_data1 <- combined_df
# Dropping the ID column
combined_df <- combined_df[, -1]

#Duplicate rows in data
sum(duplicated(combined_df))
##0
###########################################################################################
#Checking distribution of Performance Tag
table(combined_df$Performance.Tag)
#69867 rows, indicates presence of NAs
sum(is.na(combined_df$Performance.Tag))
###########################################################################################
# Creating validation set with the rows where performance tag=na, assuming that these are
#people who have not been issued cards this data will be used for scorecard validation
rejected_data <-
  combined_df[which(is.na(combined_df$Performance.Tag)),]
nrow(rejected_data)
###########################################################################################

###########################################################################################
###########################################################################################
#                                         EDA
###########################################################################################
###########################################################################################

combined_df_1 <-
  combined_df[!is.na(combined_df$Performance.Tag) == TRUE, ]

str(combined_df_1)

# summary master data
summary(combined_df_1)
str(combined_df_1)

###########################################################################################
#                              Missing Value Treatment
###########################################################################################
missing_val_count <-
  sapply(combined_df_1, function(x)
    sum(is.na(x)))

missing_val_count
# 1 NA value  in "No.of.trades.opened.in.last.6.months" column
# 3 NAs  in - "No.of.dependents" column
# 272 NAs in Outstanding.Balance column
# 272 NAs in Presence.of.open.home.loan  column
# 1023 NAs in Avgas.CC.Utilization.in.last.12.months column

###########################################################################################
# Missing values are replaced by median value
combined_df_1$No.of.dependents[which(is.na(combined_df_1$No.of.dependents) ==
    1)] <-
  median(combined_df_1$No.of.dependents, na.rm = T)
combined_df_1$No.of.trades.opened.in.last.6.months[which(is.na(combined_df_1$No.of.trades.opened.in.last.6.months) ==
    1)] = median(combined_df_1$No.of.trades.opened.in.last.6.months, na.rm = T)
combined_df_1$Presence.of.open.home.loan[which(is.na(combined_df_1$Presence.of.open.home.loan) ==
    1)] = median(combined_df_1$Presence.of.open.home.loan, na.rm = T)
combined_df_1$Outstanding.Balance[which(is.na(combined_df_1$Outstanding.Balance) ==
    1)] = median(combined_df_1$Outstanding.Balance, na.rm = T)

#NA value in Avgas.CC.Utilization.in.last.12.months indicates no usage of CC by user.
#assigning  0 value  to these avg-cc-utilization values.
combined_df_1$Avgas.CC.Utilization.in.last.12.months[which(is.na(combined_df_1$Avgas.CC.Utilization.in.last.12.months) ==
    1)] = 0

###########################################################################################
#                                     checking empty values
###########################################################################################
val_empty_count <-
  sapply(combined_df_1, function(x)
    sum(x == " " | x == ""))

val_empty_count

#Gender has 2 empty values
#Marital.Status..at.the.time.of.application. has  6  empty values
#Education has  118  empty values
#Profession has  13  empty values
#Type.of.residence has  8  empty values
###########################################################################################
# Replacing the empty values with  mode
combined_df_1$Gender[which(combined_df_1$Gender == " " |
    combined_df_1$Gender == " ")] <- 'M'
combined_df_1$Marital.Status..at.the.time.of.application.[which(
  combined_df_1$Marital.Status..at.the.time.of.application. == " " |
    combined_df_1$Marital.Status..at.the.time.of.application. == " "
)] <- 'Married'
combined_df_1$Education[which(combined_df_1$Education == " " |
    combined_df_1$Education == " ")] <-
  'Professional'
combined_df_1$Profession[which(combined_df_1$Profession == " " |
    combined_df_1$Profession == " ")] <-
  'SAL'
combined_df_1$Type.of.residence[which(combined_df_1$Type.of.residence ==
    " " |
    combined_df_1$Type.of.residence == " ")] <- 'Rented'

###########################################################################################
#                                  outliers treatment
###########################################################################################
Outlier <- function(data) {
  lowerq = quantile(data, probs = seq(0, 1, 0.10))[3]
  upperq = quantile(data, probs = seq(0, 1, 0.10))[9]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  #  extreme outlier indices are present
  result <-
    which(data > extreme.threshold.upper |
        data < extreme.threshold.lower)
}

CurrentCompany_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.months.in.current.company), ]
CCutilization_outliers <-
  combined_df_1[Outlier(combined_df_1$Avgas.CC.Utilization.in.last.12.months), ]
Tradesin6mon_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.trades.opened.in.last.6.months), ]
Tradesin12mon_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.trades.opened.in.last.12.months), ]
PLTradesin6mon_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.PL.trades.opened.in.last.6.months), ]
PLTradesin12mon_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.PL.trades.opened.in.last.12.months), ]
Inquiresin6mon_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), ]
Inquiresin12mon_outliers <-
  combined_df_1[Outlier(combined_df_1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), ]
TotalTrades__outliers <-
  combined_df_1[Outlier(combined_df_1$Total.No.of.Trades), ]

CurrentCompany_outliers$No.of.months.in.current.company
PLTradesin6mon_outliers$No.of.PL.trades.opened.in.last.6.months
TotalTrades__outliers$Total.No.of.Trades

###########################################################################################
#                                 Handling Invalid data
###########################################################################################

###########################################################################################
#age
###########################################################################################
#Invalid values for age column : 0, -3
invalidage <- which(combined_df_1$Age < 10)
#populating median values
combined_df_1$Age[invalidage] <-
  median(combined_df_1$Age, na.rm = T)

###########################################################################################
#Income
###########################################################################################
#Income has zero values
invalidincome <- which(combined_df_1$Income < 0)

#populating median values
combined_df_1$Income[invalidincome] <- 0

###########################################################################################
#Card Usage
###########################################################################################
# Considering NA values with zero.
combined_df_1$Avgas.CC.Utilization.in.last.12.months[is.na(combined_df_1$Avgas.CC.Utilization.in.last.12.months)] <-
  0
###########################################################################################
#Dependents
###########################################################################################
# Substituting 0 for NA values
combined_df_1$No.of.dependents[is.na(combined_df_1$No.of.dependents)] <-
  0
###########################################################################################
#Presence.of.open.home.loan
###########################################################################################
#Substituting 0 for NA values
combined_df_1$Presence.of.open.home.loan[is.na(combined_df_1$Presence.of.open.home.loan)] <-
  0
###########################################################################################
#Outstanding balance
############################################################################################
# Substituting 0 for NA values
combined_df_1$Outstanding.Balance[is.na(combined_df_1$Outstanding.Balance)] <-
  0

###########################################################################################
#                                   Univariate Analysis
###########################################################################################
str(combined_df_1)

###########################################################################################
#                               Box plaots and histograms
###########################################################################################

ggplot(combined_df_1, aes(Age)) + geom_histogram()
boxplot(
  combined_df_1$Age,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders are of late 30 to early 50 age range.


hist(combined_df_1$Income, xlab = "Income")
boxplot(
  combined_df_1$Income,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders are in 15 to 40 income range.

hist(combined_df_1$No.of.months.in.current.company,
  xlab = "No.of.months.in.current.company")
boxplot(
  combined_df_1$No.of.months.in.current.company,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
#Most of the card holders are with new job holders with 0-5 yr experience.

summary(combined_df_1$No.of.times.90.DPD.or.worse.in.last.6.months)
hist(combined_df_1$No.of.times.90.DPD.or.worse.in.last.6.months,
  xlab = "No.of.times.90.DPD.or.worse.in.last.6.months")
boxplot(
  combined_df_1$No.of.times.90.DPD.or.worse.in.last.6.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)

# Most of the card holders are have no  overdues
##repeating offenders population size is almost nil.

summary(combined_df_1$No.of.times.60.DPD.or.worse.in.last.6.months)
hist(combined_df_1$No.of.times.60.DPD.or.worse.in.last.6.months,
  xlab = "No.of.times.60.DPD.or.worse.in.last.6.months")
boxplot(
  combined_df_1$No.of.times.60.DPD.or.worse.in.last.6.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders are have no  overdues

summary(combined_df_1$No.of.times.30.DPD.or.worse.in.last.6.months)
hist(combined_df_1$No.of.times.30.DPD.or.worse.in.last.6.months,
  xlab = "No.of.times.30.DPD.or.worse.in.last.6.months")
boxplot(
  combined_df_1$No.of.times.30.DPD.or.worse.in.last.6.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)


summary(combined_df_1$No.of.times.90.DPD.or.worse.in.last.12.months)
hist(combined_df_1$No.of.times.90.DPD.or.worse.in.last.12.months,
  xlab = "No.of.times.90.DPD.or.worse.in.last.12.months")
boxplot(
  combined_df_1$No.of.times.90.DPD.or.worse.in.last.12.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)


summary(combined_df_1$No.of.times.60.DPD.or.worse.in.last.12.months)
hist(combined_df_1$No.of.times.60.DPD.or.worse.in.last.12.months,
  xlab = "No.of.times.60.DPD.or.worse.in.last.12.months")
boxplot(
  combined_df_1$No.of.times.60.DPD.or.worse.in.last.12.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)


summary(combined_df_1$No.of.times.30.DPD.or.worse.in.last.12.months)
hist(combined_df_1$No.of.times.30.DPD.or.worse.in.last.12.months,
  xlab = "No.of.times.30.DPD.or.worse.in.last.12.months")
boxplot(
  combined_df_1$No.of.times.30.DPD.or.worse.in.last.12.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)

summary(combined_df_1$Avgas.CC.Utilization.in.last.12.months)
hist(combined_df_1$Avgas.CC.Utilization.in.last.12.months,
  xlab = "avg cc utilization")
boxplot(
  combined_df_1$Avgas.CC.Utilization.in.last.12.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
#Most of the card holders are using only 20% of card upper limit



summary(combined_df_1$No.of.trades.opened.in.last.6.months)
hist(combined_df_1$No.of.trades.opened.in.last.6.months,
  xlab = "No.of.trades.opened.in.last.6.months")
boxplot(
  combined_df_1$No.of.trades.opened.in.last.6.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders have opened 0-4  opened in last 6 months.


summary(combined_df_1$No.of.trades.opened.in.last.12.months)
hist(combined_df_1$No.of.trades.opened.in.last.12.months,
  xlab = "No.of.trades.opened.in.last.12.months")
boxplot(
  combined_df_1$No.of.trades.opened.in.last.12.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders have opened 0-10 trades  in last 12 months.

summary(combined_df_1$No.of.PL.trades.opened.in.last.6.months)
hist(combined_df_1$No.of.PL.trades.opened.in.last.6.months,
  xlab = "No.of.PL.trades.opened.in.last.6.months")
boxplot(
  combined_df_1$No.of.PL.trades.opened.in.last.6.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders have opened 0-3 PL  in last 12 months.

summary(combined_df_1$No.of.PL.trades.opened.in.last.12.months)
hist(combined_df_1$No.of.PL.trades.opened.in.last.12.months,
  xlab = "No.of.PL.trades.opened.in.last.12.months")
boxplot(
  combined_df_1$No.of.PL.trades.opened.in.last.12.months,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders  have opened 0-6 trades in last 12 months




summary(combined_df_1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
hist(combined_df_1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
boxplot(
  combined_df_1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders  opened 0-4 trades in last 6 months.


summary(combined_df_1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
hist(
  combined_df_1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  xlab = "Autoloans-6mon"
)
boxplot(
  combined_df_1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)
# Most of the card holders  have opened 0-5 trades  in last 12 months


str(combined_df_1)

summary(combined_df_1$Total.No.of.Trades)
hist(combined_df_1$Total.No.of.Trades, xlab = "Total.No.of.Trades")
boxplot(
  combined_df_1$Total.No.of.Trades,
  names = "No of Trades",
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)

summary(combined_df_1$Outstanding.Balance)
hist(combined_df_1$Outstanding.Balance, xlab = "Outstanding.Balance")
boxplot(
  combined_df_1$Outstanding.Balance,
  names = "Outstanding Balance",
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21,
  horizontal = T
)

###########################################################################################
#                            Bar Charts and Density plots
###########################################################################################
performance_df <-
  data.frame(prop.table(table(combined_df_1$Performance.Tag) * 100))
ggplot(performance_df, aes(x = reorder(Var1, -Freq), Freq)) + geom_bar(stat =
    "identity", fill = 'orange') + xlab("Performance.Tag") + ylab("Frequency")

gender_df <-
  data.frame(prop.table(table(combined_df_1$Gender) * 100))
ggplot(gender_df, aes(x = reorder(Var1, -Freq), Freq)) + geom_bar(stat =
    "identity", fill = "orange") + xlab("Gender") + ylab("Frequency")

Education_df <-
  data.frame(prop.table(table(combined_df_1$Education) * 100))
ggplot(Education_df, aes(x = reorder(Var1, -Freq), Freq)) + geom_bar(stat =
    "identity", fill = "orange") + xlab("Education") + ylab("Frequency")

Profession_df <-
  data.frame(prop.table(table(combined_df_1$Profession) * 100))
ggplot(Profession_df, aes(x = reorder(Var1, -Freq), Freq)) + geom_bar(stat =
    "identity", fill = "orange") + xlab("Profession") + ylab("Frequency")

ms_df <-
  data.frame(prop.table(
    table(combined_df_1$Marital.Status..at.the.time.of.application.) * 100
  ))
ggplot(ms_df, aes(x = reorder(Var1, -Freq), Freq)) + geom_bar(stat =
    "identity", fill = "orange") + xlab("Marital.Status") + ylab("Frequency")


res_df <-
  data.frame(prop.table(table(combined_df_1$Type.of.residence) * 100))
ggplot(res_df, aes(x = reorder(Var1, -Freq), Freq)) + geom_bar(stat =
    "identity", fill = "orange") + xlab("Type.of.residence") + ylab("Frequency")

ggplot(combined_df_1[which(combined_df_1$Performance.Tag == 1), ] , aes(x = Income)) +
  geom_density(col = "brown", fill =
      "orange") + xlab("Income") +
  ggtitle("Income Distribution of Defaulters") +
  theme(plot.title = element_text(hjust = 0.5))
# Most of the card holders are in 0-200000 range.
# Most of the card holders have started to repay their loans.

ggplot(combined_df_1[which(combined_df_1$Performance.Tag == 1), ] ,
  aes(x = Avgas.CC.Utilization.in.last.12.months)) + geom_density(col = "brown", fill =
      "orange")
###########################################################################################
#Function to plot data
###########################################################################################
plot_data <- function(cat_var, var_name) {
  a <- aggregate(Performance.Tag ~ cat_var, combined_df_1, mean)
  count <- data.frame(table(cat_var))
  count <- count[, -1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <-
    c(var_name, "response_rate", "No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response,
    aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity', fill =
        "red") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_data(combined_df_1$Gender, "Gender")
plot_data(combined_df_1$Age, "Age")
plot_data(combined_df_1$Marital.Status..at.the.time.of.application.,
  "Marital Status")
plot_data(combined_df_1$No.of.dependents, "No of Dependents")
prop.table(table(combined_df_1$Marital.Status..at.the.time.of.application.))

############################################################################################
#                                      WOE /IV analysis
############################################################################################
##  deriving WOE /IV values for all columns

IV <-
  create_infotables(
    data = combined_df_1,
    y = "Performance.Tag",
    bins = 10,
    parallel = T
  )

IV$Tables$Age

head(IV)

IV_Value = data.frame(IV$Summary)

grid.table(IV$Summary[seq(from = 1, to = 20, by = 1),], rows = NULL)

IV_plot <- IV$Summary[order(-IV$Summary$IV), ]
IV_plot$Variable <-
  factor(IV_plot$Variable, levels = IV_plot$Variable[order(-IV_plot$IV)])
ggplot(IV_plot, aes(x = Variable, y = IV)) +
  geom_bar(
    width = .35,
    stat = "identity",
    color = "blue",
    fill = "red"
  ) +
  ggtitle("Woe- Important variables based on Information value analysis") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

IV_plot


#None of the demographic variables are important predictors
#Following are significant predictors i.e. IV>=0.1:
#                          Avgas.CC.Utilization.in.last.12.months 2.993517e-01
#                           No.of.trades.opened.in.last.12.months 2.979723e-01
#                        No.of.PL.trades.opened.in.last.12.months 2.958971e-01
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 2.954176e-01
#                                             Outstanding.Balance 2.427494e-01
#                    No.of.times.30.DPD.or.worse.in.last.6.months 2.415512e-01
#                                              Total.No.of.Trades 2.366296e-01
#                         No.of.PL.trades.opened.in.last.6.months 2.197272e-01
#                   No.of.times.90.DPD.or.worse.in.last.12.months 2.138633e-01
#                    No.of.times.60.DPD.or.worse.in.last.6.months 2.058259e-01
#  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 2.051762e-01
#                   No.of.times.30.DPD.or.worse.in.last.12.months 1.982410e-01
#                            No.of.trades.opened.in.last.6.months 1.860197e-01
#                   No.of.times.60.DPD.or.worse.in.last.12.months 1.854889e-01
#                    No.of.times.90.DPD.or.worse.in.last.6.months 1.601060e-01

master_test <- combined_df_1

# Change the levels to "good " and "bad"
master_test$Performance.Tag <-
  ifelse(master_test$Performance.Tag == 1, "bad", "good")

# Rechange the levels to "1" for good customer and "0" for bad customers
#(This change is only for the IV package)
master_test$Performance.Tag <-
  ifelse(master_test$Performance.Tag == "good", 1, 0)

IV_new <-
  create_infotables(
    data = combined_df_1,
    y = "Performance.Tag",
    bins = 10,
    parallel = T
  )

IV_new$Tables$Age
IV_new$Summary

head(IV_new)

IV_Value = data.frame(IV_new$Summary)


IV_dataframe <- IV_new$Summary
str(IV_dataframe)

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for (i in 1:nrow(IV_dataframe)) {
  if (IV_dataframe$IV[i] < 0.02) {
    IV_dataframe$feedback[i] = "Useless"
    
  } else if (IV_dataframe$IV[i] >= 0.02 &
      IV_dataframe$IV[i] < 0.1) {
    IV_dataframe$feedback[i] = "Weak"
    
  } else if (IV_dataframe$IV[i] >= 0.1 &
      IV_dataframe$IV[i] < 0.28) {
    IV_dataframe$feedback[i] = "Medium"
    
  } else if (IV_dataframe$IV[i] >= 0.29 &
      IV_dataframe$IV[i] < 0.5) {
    IV_dataframe$feedback[i] = "Strong"
    
  } else if (IV_dataframe$IV[i] > 0.1 & IV_dataframe$IV[i] < 0.3) {
    IV_dataframe$feedback[i] = "Suspicious"
  }
}

str(IV_dataframe)
IV_dataframe$var <- as.factor(IV_dataframe$Variable)
IV_dataframe$predictor <- as.factor(IV_dataframe$feedback)
IV_dataframe
## Extract "Strong" and "Medium" variables
imp_vars <-
  which(IV_dataframe$feedback == "Strong" |
      IV_dataframe$feedback == "Medium")
df1 <- IV_dataframe[imp_vars, 1]
imp <- which(colnames(master_test) %in% df1)
str(master_test)

IV_Value <-
  create_infotables(
    data = master_test,
    y = "Performance.Tag",
    bins = 10,
    parallel = T
  )

IV_Value$Tables$Age
IV_Value$Summary

woe_table <- data.frame(IV_Value$Summary)

knitr::kable(head(IV_Value$Summary))


plot_infotables(IV_Value, IV_Value$Summary$Variable[1:9], show_values = TRUE)
plot_infotables(IV_Value, IV_Value$Summary$Variable[10:18], show_values = TRUE)
plot_infotables(IV_Value, IV_Value$Summary$Variable[19:27], show_values = TRUE)

############################################################################################
############################################################################################

dependent_data <- c("Performance.Tag")

fact_data <-
  c(
    "Gender",
    "Marital.Status..at.the.time.of.application."
    ,
    "Education",
    "Profession",
    "Type.of.residence"
  )

numeric_data <-
  c(
    'Age',
    'Income',
    'No.of.months.in.current.residence',
    'No.of.months.in.current.company'
    ,
    'Total.No.of.Trades',
    'Outstanding.Balance',
    'Avgas.CC.Utilization.in.last.12.months'
    ,
    'No.of.times.90.DPD.or.worse.in.last.6.months',
    'No.of.times.60.DPD.or.worse.in.last.6.months',
    'No.of.times.30.DPD.or.worse.in.last.6.months'
    ,
    'No.of.times.90.DPD.or.worse.in.last.12.months',
    'No.of.times.60.DPD.or.worse.in.last.12.months',
    'No.of.times.30.DPD.or.worse.in.last.12.months'
    ,
    'No.of.trades.opened.in.last.6.months',
    'No.of.trades.opened.in.last.12.months'
    ,
    'No.of.PL.trades.opened.in.last.6.months',
    'No.of.PL.trades.opened.in.last.6.months'
    ,
    'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
    ,
    'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'
    ,
    'No.of.PL.trades.opened.in.last.12.months',
    'Presence.of.open.home.loan',
    'Presence.of.open.auto.loan'
  )

############################################################################################
#                                Correlation analysis
############################################################################################

correlation_df <- combined_df_1[, numeric_data]

correlation_index <- cor(correlation_df)

corrplot(
  correlation_index,
  type = "upper",
  tl.pos = "td",
  method = "circle",
  tl.cex = 0.7,
  tl.col = 'blue',
  order = "hclust",
  diag = FALSE
)


# correlations from the plot-

#Income is  Negatively correlated with all other attributes
#"Total.No.of.Trades" is Positively correlated with   "Outstanding.Balance"
#Outstanding.Balance" is Positively correlated with  "Avgas.CC.Utilization.in.last.12.months"
#Trades,Inquiries and DPD values over different periods of time are correlated, which is expected
#"Avgas.CC.Utilization.in.last.12.months" is Positively correlated with
#"No.of.times.90.DPD.or.worse.in.last.6.months"

############################################################################################
#                              Multi-variate analysis
############################################################################################

############################################################################################
#                                    Demogrphic Data
############################################################################################
# Barcharts for categorical variables
#

plot_theme <-
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  ))

#Extract subset of defaulters only
filtered_data <-
  dplyr::filter(combined_df_1, combined_df_1$Performance.Tag == 1)

#Histograms for continuous variables
plot_grid(
  ggplot(combined_df_1, aes(Age)) + geom_histogram(binwidth = 2),
  ggplot(combined_df_1, aes(Income)) + geom_histogram(binwidth = 2)
)

plot_grid(
  ggplot(filtered_data, aes(Age, fill = "indianred2")) + geom_histogram(binwidth = 2),
  ggplot(filtered_data, aes(Income, fill = "indianred2")) + geom_histogram(binwidth = 2)
)

plot_grid(
  ggplot(combined_df_1, aes(x = Education, fill = "orange")) + geom_bar() +
    plot_theme + theme(legend.position = "none"),
  ggplot(combined_df_1, aes(x = Profession, fill = "orange")) + geom_bar() +
    plot_theme + theme(legend.position = "none"),
  ggplot(
    combined_df_1,
    aes(x = Marital.Status..at.the.time.of.application., fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(combined_df_1, aes(x = Type.of.residence, fill = "orange")) +
    geom_bar() + plot_theme + theme(legend.position = "none")
)

plot_grid(
  ggplot(filtered_data, aes(x = Education, fill = "orange")) + geom_bar() +
    plot_theme + theme(legend.position = "none"),
  ggplot(filtered_data, aes(x = Profession, fill = "orange")) + geom_bar() +
    plot_theme + theme(legend.position = "none"),
  ggplot(
    filtered_data,
    aes(x = Marital.Status..at.the.time.of.application., fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(filtered_data, aes(x = Type.of.residence, fill = "orange")) +
    geom_bar() + plot_theme + theme(legend.position = "none")
)

############################################################################################
#                                 Credit History
############################################################################################

plot_grid(
  ggplot(
    combined_df_1,
    aes(x = Presence.of.open.home.loan, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    combined_df_1,
    aes(x = Presence.of.open.auto.loan, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(combined_df_1, aes(x = Total.No.of.Trades, fill = "orange")) +
    geom_bar() + plot_theme + theme(legend.position = "none")
)

plot_grid(
  ggplot(
    combined_df_1,
    aes(x = No.of.times.90.DPD.or.worse.in.last.6.months, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    combined_df_1,
    aes(x = Avgas.CC.Utilization.in.last.12.months, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    combined_df_1,
    aes(x = No.of.trades.opened.in.last.6.months, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    combined_df_1,
    aes(x = No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., fill =
        "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none")
)

plot_grid(
  ggplot(
    filtered_data,
    aes(x = Presence.of.open.home.loan, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    filtered_data,
    aes(x = Presence.of.open.auto.loan, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(filtered_data, aes(x = Total.No.of.Trades, fill = "orange")) +
    geom_bar() + plot_theme + theme(legend.position = "none")
)

ggplot(
  combined_df_1,
  aes(
    x = Avgas.CC.Utilization.in.last.12.months,
    y = No.of.PL.trades.opened.in.last.12.months,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')
## No of PL-trades opened is relatively high for default users.

plot_grid(
  ggplot(
    filtered_data,
    aes(x = No.of.times.90.DPD.or.worse.in.last.6.months, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    filtered_data,
    aes(x = Avgas.CC.Utilization.in.last.12.months, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    filtered_data,
    aes(x = No.of.trades.opened.in.last.6.months, fill = "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none"),
  ggplot(
    filtered_data,
    aes(x = No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., fill =
        "orange")
  ) + geom_bar() + plot_theme + theme(legend.position = "none")
)

ggplot(
  data = combined_df_1,
  aes(
    x = No.of.times.90.DPD.or.worse.in.last.6.months,
    y = Avgas.CC.Utilization.in.last.12.months,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')


ggplot(
  data = combined_df_1,
  aes(
    x = Total.No.of.Trades,
    y = Outstanding.Balance,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')


ggplot(
  data = combined_df_1,
  aes(
    x = Income,
    y = Outstanding.Balance,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean')
# Outstanding balance is pretty higher for default users

ggplot(
  combined_df_1,
  aes(
    x = Income,
    y = Avgas.CC.Utilization.in.last.12.months,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')

#avg-cc-usage decreases with increasing income for the card holders


ggplot(
  data = combined_df_1,
  aes(
    x = Income,
    y = No.of.times.90.DPD.or.worse.in.last.6.months,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')

# no of inquiries decresae with increase in income for non defaulters,
#no of inquiries is large with increase in income for defaulters

ggplot(
  data = combined_df_1,
  aes(
    x = Income,
    y = No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')

# Income per no of dependants is low comapared to non-defaulters to defaulters,

ggplot(
  data = combined_df_1,
  aes(
    x = No.of.dependents,
    y = Income,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')


ggplot(
  data = combined_df_1,
  aes(
    x = No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
    y = Total.No.of.Trades ,
    group = Performance.Tag,
    color = Performance.Tag
  )
) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  geom_point(stat = 'summary', fun.y = 'mean')

# total no of trades is higher for default users.

###########################################################################################
#                       Scaling numeric columns & creating dummies
###########################################################################################
table(combined_df_1$Performance.Tag)
prop.table(table(combined_df_1$Performance.Tag))
# 4% of data is in default category in the current data set
# This data is highly skewed

# data before scaling
table(combined_df_1$Performance.Tag)

#creating a copy to be used later during decision tree/Random forest processing.
random_df <- combined_df_1

combined_scaled_df <-
  data.frame(sapply(combined_df_1[numeric_data], scale))

str(combined_scaled_df)

combined_dummy_df <- combined_df_1[fact_data]

str(combined_dummy_df)
# creating dummy variables for factor attributes
dummies <-
  data.frame(sapply(combined_dummy_df, function(x)
    data.frame(
      model.matrix( ~ x - 1, data = combined_dummy_df)
    )[, -1]))

# combining all relevant columns to build final training data to be used in model building
final_data <-
  cbind(combined_df_1[dependent_data], combined_scaled_df, dummies)

###########################################################################################
###########################################################################################
#                           Create Model for Prediction
###########################################################################################
###########################################################################################


###########################################################################################
#                               DEMOGRAPHIC MODEL
###########################################################################################
#THIS MODEL USES DEMOGRAPHIC VARIABLES ONLY

demographic_data <- final_data[, c(1:5, 24:40)]

###########################################################################################
##  deriving WOE /IV values for all demographic columns
###########################################################################################
IV_demo <-
  create_infotables(
    data = demographic_data,
    y = "Performance.Tag",
    bins = 10,
    parallel = T
  )


head(IV_demo)

IV_Demo_Value = data.frame(IV_demo$Summary)

grid.table(IV_demo$Summary[seq(from = 1, to = 21, by = 1),], rows = NULL)

IV_Demo_plot <- IV_demo$Summary[order(-IV_demo$Summary$IV), ]
IV_Demo_plot$Variable <-
  factor(IV_Demo_plot$Variable, levels = IV_Demo_plot$Variable[order(-IV_Demo_plot$IV)])
ggplot(IV_Demo_plot, aes(x = Variable, y = IV)) +
  geom_bar(
    width = .35,
    stat = "identity",
    color = "red",
    fill = "red"
  ) +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

IV_Demo_plot

plot_infotables(IV_demo, IV_demo$Summary$Variable[1:9], show_values = TRUE)
plot_infotables(IV_demo, IV_demo$Summary$Variable[10:18], show_values = TRUE)
plot_infotables(IV_demo, IV_demo$Summary$Variable[19:21], show_values = TRUE)

knitr::kable(head(IV_demo$Summary))
###########################################################################################
#                  Sampling and mdoel building based on demographic data
###########################################################################################

set.seed(10)

split_indices_demo <-
  sample.split(demographic_data, SplitRatio = 0.7)

sampling_data_demo_train <- demographic_data[split_indices_demo,]

sampling_data_demo_test <- demographic_data[!split_indices_demo,]
# Logistic Model for Demographic Data

logit_demo_1 <-
  glm(Performance.Tag ~ ., family = "binomial", data = sampling_data_demo_train)

summary(logit_demo_1)

# We observe most of the variables are insignificant,so will use StepAIC algorithm to remove them
# Takes about 4 mins to execute
logit_demo_2 <- stepAIC(logit_demo_1, direction = "both")

logit_demo_3 <-
  glm(
    formula = Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Education.xOthers + Profession.xSE +
      Type.of.residence.xCompany.provided + Type.of.residence.xOwned +
      Type.of.residence.xRented,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_3)
vif(logit_demo_3)

# Removing Education.xOthers because p-value > 0.05
logit_demo_4 <-
  glm(
    formula = Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company +  Profession.xSE +
      Type.of.residence.xCompany.provided + Type.of.residence.xOwned +
      Type.of.residence.xRented,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_4)
vif(logit_demo_4)

# Removing variable Type.of.residence.xRented as VIF is high
logit_demo_5 <-
  glm(
    formula = Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Profession.xSE + Type.of.residence.xCompany.provided +
      Type.of.residence.xOwned ,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_5)
vif(logit_demo_5)

# All variables have VIF<2, but looking at p-values removing Type.of.residence.xCompany.provided
logit_demo_6 <-
  glm(
    formula = Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Profession.xSE +
      Type.of.residence.xOwned,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_6)

# Removing variable Type.of.residence.xOwned as p-value > 0.05
logit_demo_7 <-
  glm(
    formula = Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Profession.xSE ,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_7)

# Removing No.of.months.in.current.residence as it has low significance
logit_demo_8 <-
  glm(
    formula = Performance.Tag ~ Income  + No.of.months.in.current.company +
      Profession.xSE,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_8)

# Removing Profession.xSE
logit_demo_9 <-
  glm(
    formula = Performance.Tag ~ Income  + No.of.months.in.current.company ,
    family = "binomial",
    data = sampling_data_demo_train
  )

summary(logit_demo_9)

final_demo_model <- logit_demo_9
# We observe that out of all the demographic variables, only Income and No.of.months.in.
# current.company
# are significant variables
###########################################################################################
#                               Demographic Model Evaluation
###########################################################################################
# Predicting probabilities of response for the test data
sampling_data_demo_test_pred <- predict(final_demo_model,
  newdata = sampling_data_demo_test[, -1],
  type = "response")

summary(sampling_data_demo_test_pred)
# min - 0.024 max - 0.06

# Let's use the probability cutoff of 50%
sampling_data_demo_test_pred_default <-
  as.factor(ifelse(sampling_data_demo_test_pred >= 0.50, 1, 0))
sampling_data_demo_test_default <-
  as.factor(ifelse(sampling_data_demo_test$Performance.Tag == 1, 1, 0))

confusion_matrix_default <-
  confusionMatrix(sampling_data_demo_test_pred_default,
    sampling_data_demo_test_default,
    positive = "1")
confusion_matrix_default
###########################################################################################
confusion_matrix_default
###########################################################################################
#
#            Reference
#Prediction     0     1
#         0 21288   941
#         1     0     0

# Accuracy : 0.9577
#Sensitivity : 0.0000
#Specificity : 1.0000

#---------------------------------------------------------
###########################################################################################
# Let's find out the optimal probalility cutoff
###########################################################################################
performance_fun <- function(cutoff)
{
  predicted_default <-
    as.factor(ifelse(sampling_data_demo_test_pred >= cutoff, 1, 0))
  conf <-
    confusionMatrix(sampling_data_demo_test_pred_default,
      sampling_data_demo_test_default)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.024 to 0.060 for plotting and initializing a
# matrix of 1000 X 3.

s = seq(.024, .060, length = 1000)
OUT = matrix(0, 1000, 3)
for (i in 1:100)
{
  OUT[i, ] = performance_fun(s[i])
}


plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
grid(50, lwd = 2)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "orange", lwd = 2)
lines(s, OUT[, 3], col = "blue", lwd = 2)
box()
legend(
  "topright",
  legend = c("Sensitivity", "Specificity", "Accuracy"),
  col = c(2, "darkred", 4, "darkorange"),
  cex = 0.8,
  lwd = c(4, 4, 4, 4),
  text.font = 14,
  y.intersp = 0.8
)

cutoff <- s[which(abs(OUT[, 1] - OUT[, 2]) < 0.02)]
# Let's pick cutoff as 0.06
data_demo_test_pred_actual <-
  as.factor(ifelse(sampling_data_demo_test_pred >= 0.06, 1, 0))
confusion_matrix_actual <-
  confusionMatrix(data_demo_test_pred_actual,
    sampling_data_demo_test_default,
    positive = "1")
confusion_matrix_actual


# accuracy = 0.953
# specificity = 0.994
# sensitivity = 0.013

# Sensitivity is very low for this model and is not business viable and hence we
# will be building our model
# on our final dataset combining both demographic and credit bureau data


###########################################################################################
#                              Final Data for model building
###########################################################################################

final_data$Performance.Tag <- as.factor(final_data$Performance.Tag)

str(final_data)

#Final data set for Model building

###########################################################################################
#                         Creating test and training data
###########################################################################################
set.seed(100)

split_indices <- sample.split(final_data, SplitRatio = 0.7)

sampling_data <- final_data[split_indices,]

test_df <- final_data[!split_indices,]

###########################################################################################
#                       synthetic minority oversampling technique
###########################################################################################
#install.packages("ROSE")
library(ROSE)

train_df <-
  ROSE(Performance.Tag ~ ., data = sampling_data, seed = 1)$data

table(train_df$Performance.Tag)

###########################################################################################
#                              Logistic Regression
###########################################################################################

log_model_1 <-
  glm(Performance.Tag ~ ., family = "binomial", data = train_df)

summary(log_model_1)

###########################################################################################
# Using stepwise algorithm for removing insignificant variables from the model
###########################################################################################
log_model_2 <- stepAIC(log_model_1, direction = "both")


log_model_3 <-
  glm(
    Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months +
      No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months +
      No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
      No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months +
      No.of.PL.trades.opened.in.last.6.months.1 + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan +
      Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried +
      Education.xOthers + Profession.xSE + Type.of.residence.xCompany.provided +
      Type.of.residence.xOwned
    ,
    family = "binomial",
    data = train_df
  )

summary(log_model_3)

vif(log_model_3)

sort(vif(log_model_3), decreasing = T)

# All correlations are <2, hence looking at significance values for variable selection
#removing marital status=married due to low significance
log_model_4 <-
  glm(
    Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months +
      No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months +
      No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
      No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months +
      No.of.PL.trades.opened.in.last.6.months.1 + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan +
      Presence.of.open.auto.loan +
      Education.xOthers + Profession.xSE + Type.of.residence.xCompany.provided +
      Type.of.residence.xOwned
    ,
    family = "binomial",
    data = train_df
  )
summary(log_model_4)
vif(log_model_4)


#removing number of trades opened in last 6 months due to low significance
log_model_5 <-
  glm(
    Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months +
      No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months +
      No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
      No.of.PL.trades.opened.in.last.6.months +
      No.of.PL.trades.opened.in.last.6.months.1 + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan +
      Presence.of.open.auto.loan +
      Education.xOthers + Profession.xSE + Type.of.residence.xCompany.provided +
      Type.of.residence.xOwned
    ,
    family = "binomial",
    data = train_df
  )
summary(log_model_5)
vif(log_model_5)

#removing type of residence (company provided) due to low significance

log_model_6 <-
  glm(
    Performance.Tag ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months +
      No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months +
      No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
      No.of.PL.trades.opened.in.last.6.months +
      No.of.PL.trades.opened.in.last.6.months.1 + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan +
      Presence.of.open.auto.loan +
      Education.xOthers + Profession.xSE  +
      Type.of.residence.xOwned
    ,
    family = "binomial",
    data = train_df
  )
summary(log_model_6)
vif(log_model_6)

## significance is  high now for existing attributes.
#Lets take this model as final Logistic Regression  model .
final_log_model <- log_model_6

###########################################################################################
###########################################################################################
#                   Logistic Regression Model Evaluation
###########################################################################################
###########################################################################################

#predicted probabilities  for test_df data
test_df_pred = predict(final_log_model, type = "response", newdata = test_df[, -1])


# Using the probability cutoff at  50% for logistic reg..
test_df_pred_default <-
  as.factor(ifelse(test_df_pred >= 0.50, 1, 0))
test_df_default <-
  as.factor(ifelse(test_df$Performance.Tag == 1, 1, 0))


confusion_matrix_logit <-
  confusionMatrix(test_df_pred_default, test_df_default, positive = "1")

confusion_matrix_logit

#0  12092   347
#1  7946    573

#Accuracy : 0.6043
#Sensitivity : 0.62283
#Specificity : 0.60345

###########################################################################################
# Finding  the optimal probalility cutoff
###########################################################################################
performance_fun <- function(cutoff)
{
  predicted_default <- as.factor(ifelse(test_df_pred >= cutoff, 1, 0))
  conf <- confusionMatrix(predicted_default, test_df_default)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01, .95, length = 100)
OUT = matrix(0, 100, 3)
for (i in 1:100)
{
  OUT[i, ] = performance_fun(s[i])
}


plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
grid(50, lwd = 2)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "orange", lwd = 2)
lines(s, OUT[, 3], col = "blue", lwd = 2)
box()
legend(
  "topright",
  legend = c("Sensitivity", "Specificity", "Accuracy"),
  col = c(2, "orange", "blue"),
  cex = 0.8,
  lwd = c(4, 4, 4, 4),
  text.font = 14,
  y.intersp = 0.8
)

test_df_pred_actual <-
  as.factor(ifelse(test_df_pred >= 0.502, 1, 0))

confusion_matact <-
  confusionMatrix(test_df_pred_actual, test_df_default, positive = "1")
confusion_matact

# for 50.2% optimal threshold,
# accuracy = 60.9% ,
# specificity = 61.7% ,
# sensitivity = 60.9%
# cutoff value is 0.502 for final model.



###########################################################################################
#                            KS -statistic - Test Data
###########################################################################################
library(ROCR)

prediction_object <-
  ROCR::prediction(as.numeric(test_df_pred_actual),
    as.numeric(test_df_default))
prediction_object
perf_msr_test <- ROCR::performance(prediction_object, "tpr", "fpr")

ks_test <- attr(perf_msr_test, "y.values")[[1]] -
  (attr(perf_msr_test, "x.values")[[1]])

max(ks_test)   ## 0.2263842

#KS-statistic is 22.6%

###########################################################################################
#                                   ROC Curve
###########################################################################################
ROC_area <- ROCR::performance(prediction_object, measure = "auc")
ROC_area <- ROC_area@y.values[[1]]
ROC_area
##Area under curve is : 0.6131921

pred_df <-
  data.frame(fpr = unlist(perf_msr_test@x.values),
    tpr = unlist(perf_msr_test@y.values))

ggplot(pred_df , aes(x = fpr, y = tpr)) +
  geom_line(colour = "blue") +
  geom_line(data = data.frame(), aes(x = c(0, 1), y = c(0, 1)), colour =
      "grey") +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    title = "ROC Curve for Logistic Regression Model",
    caption = "pred_df_logistic"
  ) +
  theme(axis.text.x = element_text(hjust = 1)) +
  annotate(
    "text",
    x = 0.4,
    y = 0.00,
    hjust = 0,
    vjust = 0,
    size = 5,
    label = paste("AUC =", round(ROC_area, 3))
  )

gini <- (ROC_area * 2) - 1
gini
#0.2263842
###########################################################################################
#                               Lift and Gain charts
###########################################################################################
library(dplyr)

lift <- function(labels , pred_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(pred_prob))
    pred_prob <- as.integer(as.character(pred_prob))
  helper = data.frame(cbind(labels , pred_prob))
  helper[, "bucket"] = ntile(-helper[, "pred_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
      totalresp = sum(., na.rm = TRUE))) %>%
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}

lift_info = lift(test_df_default, test_df_pred, groups = 10)

print(lift_info)
write.csv(lift_info, "lift.csv", row.names = FALSE)

### Gain Chart
ggplot(lift_info, aes(x = bucket)) +
  labs(x = "Decile", y = "Gain (%)") +
  geom_point(
    data = lift_info,
    aes(x = bucket, y = Gain),
    color = '#FF6666',
    group = 1,
    size = 2,
    shape = 21,
    stroke = 2.5
  ) +
  geom_line(
    data = lift_info,
    aes(x = bucket, y = Gain),
    color = '#07843b',
    size = 1,
    group = 1
  ) +
  theme(panel.grid.minor = element_line(colour = "blue", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    breaks = seq(20, 100, 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  ggtitle("Gain Chart")


### Lift Chart
ggplot(lift_info, aes(x = bucket)) +
  labs(x = "Decile", y = "Lift") +
  geom_point(
    data = lift_info,
    aes(x = bucket, y = Cumlift),
    color = '#07843b',
    group = 1,
    size = 2,
    shape = 21,
    stroke = 2.5
  ) +
  geom_line(
    data = lift_info,
    aes(x = bucket, y = Cumlift),
    color = '#FF6666',
    size = 1,
    group = 1
  ) +
  theme(panel.grid.minor = element_line(colour = "blue", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0.4, 4, 0.4)) +
  ggtitle("Lift Chart")

###########################################################################################
#                       Rejected Data validation using logistic model
###########################################################################################
#Rejected data EDA and feature engineering

str(rejected_data)

scaling_for_rejects <- rejected_data[numeric_data]

rejectedapplicants_scaled_data <-
  data.frame(sapply(scaling_for_rejects, scale))
str(rejectedapplicants_scaled_data)

rejects_for_dummies <- rejected_data[fact_data]


# dummy variables for factor attributes
rejectedapplicants_dummies <-
  data.frame(sapply(rejects_for_dummies, function(x)
    data.frame(
      model.matrix( ~ x - 1, data = rejects_for_dummies)
    )[, -1]))

# combining all related columns to build final training data

rejected_final_df <-
  cbind(rejectedapplicants_scaled_data, rejectedapplicants_dummies)
str(rejected_final_df)

eda_val_missing_count <-
  sapply(rejected_final_df, function(x)
    sum(is.na(x)))

eda_val_missing_count

rejected_final_df$Avgas.CC.Utilization.in.last.12.months[which(is.na(rejected_final_df$Avgas.CC.Utilization.in.last.12.months) ==
    1)] = 0

#Predicting defaulters in rejected applicants using logistic model
rejected_final_df$perdict_default  <-
  predict(final_log_model, type = "response", newdata = rejected_final_df)
rejected_final_df$default_status  <-
  ifelse(rejected_final_df$perdict_default >= .502, 1, 0)

table(rejected_final_df$default_status)

#Using this model, 36% of the rejected applicants would be defaulters

#########################################################################################
#                         Building  model for Decision tree
#########################################################################################
library(rpart)
library(rpart.plot)
library(kernlab)
library(readr)

# Splitting the bank data in 70:30 ratio
set.seed(101)
random_df$Performance.Tag <-
  as.factor(ifelse(random_df$Performance.Tag == 0, "no", "yes"))


train_dt <- train_df
test_dt <- test_df
train_dt$Performance.Tag <-
  as.factor(ifelse(train_dt$Performance.Tag == 0, "no", "yes"))
test_dt$Performance.Tag <-
  as.factor(ifelse(test_dt$Performance.Tag == 0, "no", "yes"))


table(train_dt$Performance.Tag)
# no   yes
# 24451  24458

#check  distribution of classes
prop.table(table(train_dt$Performance.Tag))
# no        yes
# 0.4999284 0.5000716

#Building a decision tree with default hyperparameters
decision_tree_model <-
  rpart(Performance.Tag ~ .,
    data = train_dt,
    method = "class")

# decision tree1
prp(decision_tree_model)
#  predictions on the test set
decision_tree_pred <-
  predict(decision_tree_model, test_dt, type = "class")
decision_tree_pred
# evaluating the results
confusionMatrix(decision_tree_pred, test_dt$Performance.Tag, positive = "yes")

# Accuracy : 0.6658
# Sensitivity : 0.56957
# Specificity : 0.67023

###########################################################################################
#               Change the algorithm to "information gain"
###########################################################################################
decision_tree_model <-
  rpart(
    Performance.Tag ~ .,
    data = train_dt,
    method = "class",
    parms = list(split = "information")
  )

#  decision tree2
prp(decision_tree_model)

#  predictions on the test set
decision_tree_pred <-
  predict(decision_tree_model, test_dt, type = "class")

# evaluating the results
confusionMatrix(decision_tree_pred, test_dt$Performance.Tag, positive = "yes")

# Accuracy : 0.6658
# Sensitivity : 0.56957
# Specificity : 0.67023

###########################################################################################
#                             Tuning the hyperparameters
###########################################################################################
decision_tree_model <-
  rpart(
    Performance.Tag ~ .,
    data = train_dt,
    method = "class",
    control = rpart.control(
      minsplit = 100,
      minbucket = 100,
      cp = 0.001
    )
  )

# decision tree3
prp(decision_tree_model)

# predictions on the test set
decision_tree_pred <-
  predict(decision_tree_model, test_dt, type = "class")



# evaluating the results
confusionMatrix(decision_tree_pred, test_dt$Performance.Tag, positive = "yes")

# Accuracy : 0.7433
# Sensitivity : 0.42609
# Specificity : 0.75786

###########################################################################################
#                       Cross test to choose CP
###########################################################################################
# setting the number of folds in cross validation test to 5
library(foreach)
registerDoSEQ()
decision_tree.control = trainControl(method = "cv", number = 5)

# setting the search space for CP
decision_tree.grid = expand.grid(cp = seq(0, 0.02, 0.001))

# train model
decision_tree_model <- caret::train(
  Performance.Tag ~ .,
  data = train_dt,
  method = "rpart",
  metric = "Accuracy",
  trControl = decision_tree.control,
  tuneGrid = decision_tree.grid,
  control = rpart.control(minsplit = 50,
    minbucket = 20)
)



# cross validated model results
decision_tree_model$results

#  best value of hyperparameter
decision_tree_model$bestTune

# makeing predictions on test set
decision_tree_pred <-
  predict(decision_tree_model, test_dt, type = "raw")
table(test_dt$Performance.Tag)
confusionMatrix(decision_tree_pred, test_dt$Performance.Tag, positive = "yes")
#Accuracy : 0.744
# Sensitivity : 0.426
# Specificity : 0.758


#In terms of probability
decision_tree_pred <-
  predict(decision_tree_model, test_dt[, -1], type = "prob")
predicted_default <-
  as.factor(ifelse(decision_tree_pred[, 2] >= .5, "yes", "no"))
table(predicted_default)
confusionMatrix(predicted_default, test_dt$Performance.Tag, positive = "yes")

###########################################################################################
#                     finding out the optimal probalility cutoff
###########################################################################################

performance_fun <- function(cutoff)
{
  predicted_default <-
    as.factor(ifelse(decision_tree_pred[, 2] >= cutoff, "yes", "no"))
  conf <-
    confusionMatrix(predicted_default, test_dt$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01, .95, length = 100)
OUT = matrix(0, 100, 3)
for (i in 1:100)
{
  OUT[i, ] = performance_fun(s[i])
}

plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
grid(50, lwd = 2)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "orange", lwd = 2)
lines(s, OUT[, 3], col = "blue", lwd = 2)
box()
legend(
  "topright",
  legend = c("Sensitivity", "Specificity", "Accuracy"),
  col = c(2, "orange",  "blue"),
  cex = 0.5,
  lwd = c(4, 4, 4, 4),
  text.font = 20,
  y.intersp = 1
)
test_df_pred_actual <-
  as.factor(ifelse(decision_tree_pred[, 2] >= .3, "yes", "no"))
table(test_df_pred_actual)
test_df_pred <- (ifelse(test_df_pred_actual == "yes", 1, 0))

confusion_matrixdt <-
  confusionMatrix(test_df_pred_actual, test_dt$Performance.Tag, positive = "yes")

confusion_matrixdt

# Accuracy : 0.6389
# Sensitivity : 0.6022
# Specificity : 0.6406

###########################################################################################
#                        KS -statistic - Decision Tree - Test Data
###########################################################################################

library(ROCR)
test_df_default <-
  as.factor(ifelse(test_dt$Performance.Tag == "yes", 1, 0))
prediction_object <-
  prediction(as.numeric(test_df_pred_actual),
    as.numeric(test_df_default))

perf_msr_test <- performance(prediction_object, "tpr", "fpr")

ks_test <- attr(perf_msr_test, "y.values")[[1]] -
  (attr(perf_msr_test, "x.values")[[1]])

max(ks_test)  #0.2427568


#KS-statistic is 24%

#ROC Curve

ROC_area <- performance(prediction_object, measure = "auc")
ROC_area <- ROC_area@y.values[[1]]
ROC_area

# Area under curve is : 0.62

pred_df <-
  data.frame(fpr = unlist(perf_msr_test@x.values),
    tpr = unlist(perf_msr_test@y.values))

ggplot(pred_df , aes(x = fpr, y = tpr)) +
  geom_line(colour = "blue") +
  geom_line(data = data.frame(), aes(x = c(0, 1), y = c(0, 1)), colour =
      "grey") +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    title = "ROC Curve for Decision Tree Model",
    caption = "pred_df_decision_tree"
  ) +
  theme(axis.text.x = element_text(hjust = 1)) +
  annotate(
    "text",
    x = 0.4,
    y = 0.00,
    hjust = 0,
    vjust = 0,
    size = 5,
    label = paste("AUC =", round(ROC_area, 3))
  )

gini <- (ROC_area * 2) - 1
gini
#0.2427568

library(dplyr)

lift <- function(labels , pred_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(pred_prob))
    pred_prob <- as.integer(as.character(pred_prob))
  helper = data.frame(cbind(labels , pred_prob))
  helper[, "bucket"] = ntile(-helper[, "pred_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
      totalresp = sum(., na.rm = TRUE))) %>%
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}
lift_info_dstree = lift(test_df_default, test_df_pred, groups = 10)

print(lift_info_dstree)
write.csv(lift_info_dstree, "lift_dstree.csv", row.names = FALSE)

### Gain Chart
ggplot(lift_info_dstree, aes(x = bucket)) +
  labs(x = "Decile", y = "Gain (%)") +
  geom_point(
    data = lift_info_dstree,
    aes(x = bucket, y = Gain),
    color = '#FF6666',
    group = 1,
    size = 2,
    shape = 21,
    stroke = 2.5
  ) +
  geom_line(
    data = lift_info_dstree,
    aes(x = bucket, y = Gain),
    color = '#07843b',
    size = 1,
    group = 1
  ) +
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    breaks = seq(20, 100, 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  ggtitle("Gain Chart")


### Lift Chart
ggplot(lift_info_dstree, aes(x = bucket)) +
  labs(x = "Decile", y = "Lift") +
  geom_point(
    data = lift_info_dstree,
    aes(x = bucket, y = Cumlift),
    color = '#07843b',
    group = 1,
    size = 2,
    shape = 21,
    stroke = 2.5
  ) +
  geom_line(
    data = lift_info_dstree,
    aes(x = bucket, y = Cumlift),
    color = '#FF6666',
    size = 1,
    group = 1
  ) +
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0.4, 4, 0.4)) +
  ggtitle("Lift Chart")

###########################################################################################
#                   validating decision tree model using rejected data
###########################################################################################

#Predicting defaulters in rejected applicants using decision tree
rejected_final_df$perdict_default_dctree  <-
  predict(decision_tree_model, rejected_final_df, type = "prob")
rejected_final_df$default_status_dctree  <-
  ifelse(rejected_final_df$perdict_default_dctree >= .3, 1, 0)

table(rejected_final_df$default_status_dctree)

#Using this model, 83% of the rejected applicants would be defaulters


#########################################################################################
# Building a random Forest model
# library(randomForest)
#########################################################################################

set.seed(100)
rf_model <- randomForest(
  Performance.Tag ~ .,
  data = train_dt,
  proximity = F,
  do.trace = F,
  mtry = 5,
  ntree = 501
)
ncol(train_dt)
summary(rf_model)

# making predictions on the test set
tree.predict <- predict(rf_model, test_dt, type = "class")
# evaluating the results
confusionMatrix(tree.predict, test_dt$Performance.Tag, positive = "yes")


# Prediction    no   yes
#         no  18178   746
#         yes  1860   174

#Metrics based on ntree=1000

#Accuracy .8757
#Sensitivity .1891
#Specificity .9072

#metrics based on ntree=501


#Prediction    no   yes
#no         18154   741
#yes        1884   179

#Accuracy .8739
#Sensitivity .1978
#Specificity .9049



rdmfor_pred_mod <- predict(rf_model, test_dt, type = "prob")
#Plotting variable importance
varImpPlot(rf_model)

rf_subset <- train_dt[c(1, 8:13, 16:19, 30, 38)]

#########################################################################################
#                         Parameter tuning for Random Forest
#########################################################################################
control <- trainControl(method = "cv", number = 5)
tunegrid <- expand.grid(.mtry = c(1:3))
set.seed(100)

rf_random <-
  train(
    Performance.Tag ~ .,
    data = rf_subset,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tunegrid,
    trControl = control
  )

print(rf_random)
plot(rf_random)

rdmfor_pred_mod <- predict(rf_random, test_dt, type = "raw")
rdmfor_pred_mod1 <- predict(rf_random, test_dt, type = "prob")
confusionMatrix(rdmfor_pred_mod, test_dt$Performance.Tag, positive = "yes")

#Reference
#Prediction    no     yes
#no           18008   739
#yes          2030    181

#Accuracy .8679
#Sensitivity .1967
#Specificity .8987



# the optimal cutoff value for probalility with synthetic data
#Cutoff for randomforest : assigning yes or no
performance_fun_rf <- function(cutoff)
{
  pred_response <-
    as.factor(ifelse(rdmfor_pred_mod1[, 2] >= cutoff, "yes", "no"))
  conf <-
    confusionMatrix(pred_response, test_dt$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc)))
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99
s = seq(.01, .99, length = 100)

OUT_rf = matrix(0, 100, 3)

# calculating  the sensitivity, specificity and accuracy for different cutoff values

for (i in 1:100)
{
  OUT_rf[i, ] = performance_fun_rf(s[i])
}

OUT_rf
# plot cutoffs

plot(
  s,
  OUT_rf[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT_rf[, 2], col = "orange", lwd = 2)
lines(s, OUT_rf[, 3], col = "blue", lwd = 2)
box()
legend(
  0,
  .50,
  col = c(2, "orange", "blue"),
  lwd = c(1, 1, 1),
  c("Sensitivity", "Specificity", "Accuracy")
)

rdmfor_cutoff <- s[which(abs(OUT_rf[, 1] - OUT_rf[, 2]) < 0.05)]
rdmfor_cutoff
#Of the three cutoffs,  0.21 has best sensitivity and accuracy.
## The cut off is  low.


test_df_pred <- ifelse(rdmfor_pred_mod1[, 2] >= 0.2, 1, 0)
test_df_pred_actual <- ifelse(test_df_pred == "1", "yes", "no")
test_df_pred_actual <- as.factor(test_df_pred_actual)

rdmfor_conf <-
  confusionMatrix(test_df_pred_actual, test_dt$Performance.Tag, positive = "yes")
rdmfor_conf

# Accuracy : 0.635
# Sensitivity :0.6152
# Specificity :0.6358

###########################################################################################
#                 KS - statistic -Random Forest using Test Data
###########################################################################################
library(ROCR)
test_df_default <-
  as.factor(ifelse(test_dt$Performance.Tag == "yes", 1, 0))
prediction_object <-
  prediction(as.numeric(test_df_pred_actual),
    as.numeric(test_df_default))

perf_msr_test <- performance(prediction_object, "tpr", "fpr")

ks_test <- attr(perf_msr_test, "y.values")[[1]] -
  (attr(perf_msr_test, "x.values")[[1]])

max(ks_test)  #0.2511092


#KS-statistic is 25%

#ROC Curve

ROC_area <- performance(prediction_object, measure = "auc")
ROC_area <- ROC_area@y.values[[1]]
ROC_area
#0.6255546

# Area under curve is : 62%

pred_df <-
  data.frame(fpr = unlist(perf_msr_test@x.values),
    tpr = unlist(perf_msr_test@y.values))

ggplot(pred_df , aes(x = fpr, y = tpr)) +
  geom_line(colour = "blue") +
  geom_line(data = data.frame(), aes(x = c(0, 1), y = c(0, 1)), colour =
      "grey") +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    title = "ROC Curve for Random Forest",
    caption = "pred_df_randomForest"
  ) +
  theme(axis.text.x = element_text(hjust = 1)) +
  annotate(
    "text",
    x = 0.4,
    y = 0.00,
    hjust = 0,
    vjust = 0,
    size = 5,
    label = paste("AUC =", round(ROC_area, 3))
  )

gini <- (ROC_area * 2) - 1
gini
#0.2511092
library(dplyr)

lift <- function(labels , pred_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(pred_prob))
    pred_prob <- as.integer((pred_prob))
  helper = data.frame(cbind(labels , pred_prob))
  helper[, "bucket"] = ntile(-helper[, "pred_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
      totalresp = sum(., na.rm = TRUE))) %>%
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}

lift_info_rf = lift(test_df_default, factor(test_df_pred), groups = 10)

print(lift_info_rf)
write.csv(lift_info_rf, "lift_rf.csv", row.names = FALSE)

### Gain Chart
ggplot(lift_info_rf, aes(x = bucket)) +
  labs(x = "Decile", y = "Gain (%)") +
  geom_point(
    data = lift_info_rf,
    aes(x = bucket, y = Gain),
    color = '#FF6666',
    group = 1,
    size = 2,
    shape = 21,
    stroke = 2.5
  ) +
  geom_line(
    data = lift_info_rf,
    aes(x = bucket, y = Gain),
    color = '#07843b',
    size = 1,
    group = 1
  ) +
  theme(panel.grid.minor = element_line(colour = "red", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    breaks = seq(20, 100, 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  ggtitle("Gain Chart")


### Lift Chart
ggplot(lift_info_rf, aes(x = bucket)) +
  labs(x = "Decile", y = "Lift") +
  geom_point(
    data = lift_info_rf,
    aes(x = bucket, y = Cumlift),
    color = '#07843b',
    group = 1,
    size = 2,
    shape = 21,
    stroke = 2.5
  ) +
  geom_line(
    data = lift_info_rf,
    aes(x = bucket, y = Cumlift),
    color = '#FF6666',
    size = 1,
    group = 1
  ) +
  theme(panel.grid.minor = element_line(colour = "red", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0.4, 4, 0.4)) +
  ggtitle("Lift Chart")


###########################################################################################
#               validating random forest model using rejected data
###########################################################################################

#Predicting defaulters in rejected applicants using random forest
rejected_final_df$perdict_default_rf  <-
  predict(rf_random, rejected_final_df, type = "prob")
rejected_final_df$default_status_rf  <-
  ifelse(rejected_final_df$perdict_default_rf >= .2, 1, 0)

table(rejected_final_df$default_status_rf)

#Using this model, 95% of the rejected applicants would be defaulters

#########################################################################################
#                                         GBM
#########################################################################################
library(unbalanced)
set.seed(100)
options("scipen" = 1000)
trainindices = sample(1:nrow(sampling_data), 0.7 * nrow(sampling_data))
traindata <- sampling_data[trainindices,]
test <- sampling_data[-trainindices,]
nrow(traindata)

output <- traindata[, c(1)]
input <- traindata[, -c(1)]
output <- as.factor(output)

nrow(input)
nrow(output)
data <-
  ubBalance(
    X = input,
    Y = output,
    type = "ubSMOTE",
    percOver = 300,
    percUnder = 150,
    verbose = TRUE
  )

#Balanced Data#
balancedData <- cbind(data$X, data$Y)
nrow(data)
nrow(balancedData)
View(balancedData)
table(balancedData$`data$Y`)

gbmfit <-
  caret::train(`data$Y` ~ .,
    data = balancedData,
    method = "gbm",
    verbose = FALSE)

summary(gbmfit)
test$score_Y = predict(gbmfit, newdata = test, type = "prob")[, 2]

test$score_Y = ifelse(test$score_Y > 0.5, 1, 0)

pred_GBM <- ROCR::prediction(test$score_Y, test$Performance.Tag)
pred_GBM

model_perf_GBM <- ROCR::performance(pred_GBM, "tpr", "fpr")
model_perf_GBM

auc.tmp_GBM <- ROCR::performance(pred_GBM, "auc")
AUC_GBM <- as.numeric(auc.tmp_GBM@y.values)
auc.tmp_GBM
AUC_GBM

trellis.par.set(caretTheme())
plot(gbmfit)

#########################################################################################
#                                     Parameter tuning
#########################################################################################
gbmGrid <-  expand.grid(
  interaction.depth = c(1, 2, 3),
  n.trees = (1:30) * 50,
  shrinkage = 0.1,
  n.minobsinnode = 20
)
fitcontrol <-
  caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 1,
    classProbs = TRUE,
    verbose = FALSE
  )

levels(balancedData$`data$Y`) <-
  make.names(levels(factor(balancedData$`data$Y`)))

gbmFit3 <- caret::train(
  `data$Y` ~ .,
  data = balancedData,
  method = "gbm",
  trControl = fitcontrol,
  verbose = FALSE,
  tuneGrid = gbmGrid,
  ## Specify which metric to optimize
  metric = "Accuracy"
)
summary(gbmFit3)
print(gbmFit3)
plot(gbmFit3)
gbmFit3$results
gbmFit3$bestTune

test$score_Y = predict(gbmFit3, newdata = test, type = "prob")[, 2]


test$score_Y = ifelse(test$score_Y > 0.5, 1, 0)
conf <-
  confusionMatrix(as.factor(test$score_Y), test$Performance.Tag, positive =
      "1")
conf
#Accuracy .9354
#Sensitivity .976
#Specificity .05




pred_GBM_fit3 <-
  ROCR::prediction(test$score_Y, test$Performance.Tag)
pred_GBM_fit3

model_perf_GBM_fit3 <-
  ROCR::performance(pred_GBM_fit3, "tpr", "fpr")
model_perf_GBM_fit3

auc.tmp_GBM <- ROCR::performance(pred_GBM_fit3, "auc")
AUC_GBM <- as.numeric(auc.tmp_GBM@y.values)
auc.tmp_GBM
AUC_GBM



conf <-
  confusionMatrix(as.factor(test$score_Y), test$Performance.Tag)
conf


#########################################################################################
#                                 Linear SVM
#SVM needs all numeric attribubtes .so reusing the SMOTE balanced train_df dataset.
#########################################################################################

# library(plyr)
# library(caret)
# library(kernlab)
# library(readr)
# library(caret)
# library(caTools)
# #install.packages("foreach")
# library(foreach)
# registerDoSEQ()
#
# train_df_svm <- train_df
# test_df_svm <- test_df
#
# nrow(train_df_svm)
# table(train_df_svm$Performance.Tag)
# str(train_df_svm)
# nrow(test_df_svm)
# table(test_df_svm$Performance.Tag)
# str(test_df_svm)
#
# #Since it would take much time for modeling on the  whole train_df data,
# #So we are taking 50% sample of the data and building the model
#
# train_df_svm_sample = sample(2:nrow(train_df_svm), 0.5 * nrow(train_df_svm))
# train_df_svm = train_df_svm[train_df_svm_sample, ]
#
#
# #  Model Building
#
# #########################################################################################
# # 4.1 Linear model - SVM  at Cost(C) = 1
# #########################################################################################
#
# svm_model_1 <-
#   ksvm(Performance.Tag ~ .,
#     data = train_df_svm,
#     scale = TRUE,
#     C = 1)
# svm_linear <- predict(svm_model_1, test_df_svm)
# confusionMatrix(svm_linear, test_df_svm$Performance.Tag)
#
# #Accuracy : 0.8867
# #Sensitivity : 0.9205
# #Specificity : 0.1500
#
# #########################################################################################
# # 4.2 Linear model - SVM  at Cost(C) = 10
# #########################################################################################
#
# # Model with C =10. ( an overfit model)
#
# svm_model_2 <-
#   ksvm(Performance.Tag ~ .,
#     data = train_df_svm,
#     scale = TRUE,
#     C = 10)
# svm_linear <- predict(svm_model_2, test_df_svm)
# confusionMatrix(svm_linear, test_df_svm$Performance.Tag)
#
# # Accuracy : 0.9425
# #Sensitivity : 0.9843
# # Specificity : 0.03152
##########################################################################################
# ##########################   Using Linear Kernel###############################
##########################################################################################
# svm_linear_model <-
#   ksvm(Performance.Tag ~ .,
#     data = train_df_svm,
#     scale = FALSE,
#     kernel = "vanilladot")
# svm_linear_pred <- predict(svm_linear_model, test_df_svm)
#
# #confusion matrix - Linear Kernel
# confusionMatrix(svm_linear_pred, test_df_svm$Performance.Tag)
#
# #  Accuracy : 0.5654
# # Sensitivity : 0.5604
# # Specificity : 0.6723
## #########################################################################################
# ############   Hyperparameter tuning and Cross Validation #####################
## #########################################################################################
#
# #traincontrol function Controls the computational nuances of the train_df function.
# # method =  CV means  Cross Validation.
# # Number = 5 implies Number of folds in CV.
#
# trainControl <- trainControl(method = "cv", number = 5)
#
# # Metric <- "Accuracy" implies the Evaluation metric is Accuracy.
# metric <- "Accuracy"
#
# #### Hyperparameter tuning and Cross Validation  with Linear SVM , for C = 1 to 5 ####
# #Expand.grid functions takes set of hyperparameters, that are passed to our model.
# # making a grid of C values.
# grid <- expand.grid(C = seq(1, 5, by = 1))
#
# #  5-fold cross validation
# svm_fit_linear <-
#   caret::train(
#     Performance.Tag ~ .,
#     data = train_df_svm,
#     method = "svmLinear",
#     metric = metric,
#     tuneGrid = grid,
#     trControl = trainControl
#   )
# # execution time - 11 hours
#
# #  cross validation result
# print(svm_fit_linear)
# # The final value for C = 4.
#
# # Plotting "svm_fit" results
# plot(svm_fit_linear)
#
# # Valdiating the model after cross validation on test_df_svm data
# svm_fit_linear_eval <- predict(svm_fit_linear, test_df_svm)
# confusionMatrix(svm_fit_linear_eval, test_df_svm$Performance.Tag)
#
# # Accuracy : 0.5655
# # Sensitivity : 0.56049
# # Specificity : 0.67391
#
# #########################################################################################
# #                                   Using Polynomial Kernel
# #########################################################################################
# # Using Polynomial Kernel with degree=2
# svm_poly_model <-
#   ksvm(
#     Performance.Tag ~ .,
#     data = train_df_svm,
#     scale = FALSE,
#     kernel = "polydot",
#     kpar = list(degree = 2)
#   )
# # Predicting model results
# svm_poly_eval <- predict(svm_poly_model, test_df_svm)
# #confusion matrix - RBF Kernel
# confusionMatrix(svm_poly_eval, test_df_svm$Performance.Tag)
#
# trainControl <-
#   trainControl(method = "cv",
#     number = 5,
#     returnResamp = "all")
# # train_df function takes Target ~ Prediction, Data, Method = Algorithm
# # trcontrol = the  traincontrol method.
# svm_fit_poly <-
#   train(
#     Performance.Tag ~ .,
#     data = train_df_svm,
#     method = "svmPoly",
#     trControl = trainControl,
#     preProc = c("center", "scale")
#   )
# print(svm_fit_poly)
#
#
# plot(svm_fit_poly)
#
# # Predicting the model results
# svm_fit_poly_eval <- predict(svm_fit_poly, test_df_svm)
# #confusion matrix - RBF Kernel
# confusionMatrix(svm_fit_poly_eval, test_df_svm$Performance.Tag)
#
# #########################################################################################
# #                                 Using RBF Kernel
# #########################################################################################
# RBF_Model <-
#   ksvm(Performance.Tag ~ .,
#     data = train_df_svm,
#     scale = FALSE,
#     kernel = "rbfdot")
# RBF_Model_pred <- predict(RBF_Model, test_df_svm)
#
# #confusion matrix - RBF Kernel
# confusionMatrix(RBF_Model_pred, test_df_svm$Performance.Tag)
#
# #Accuracy : 0.8872
# #Sensitivity : 0.92100
# #Specificity : 0.15000
#
# ####   Hyperparameter tuning and Cross Validation  for  Radial SVM  ####
# trainControl <- trainControl(method = "cv", number = 2)
# grid <-
#   expand.grid(.sigma = c(0.025, 0.05, 0.075, 0.1),
#     .C = c(0.1, 0.5, 1, 2, 3, 4, 5))
#
# # trcontrol = the traincontrol method.
#
# svm_fit <-
#   train(
#     Performance.Tag ~ .,
#     data = train_df_svm,
#     method = "svmRadial",
#     metric = metric,
#     tuneGrid = grid,
#     trControl = trainControl
#   )
#
# print(svm_fit)
# # execution time - 10 hours
#
# plot(svm_fit)
# # The final values used for the model were sigma = 0.025 and C= 4.
#
# # Valdiating the model after cross validation on test_df_svm data
# svm_cv_eval <- predict(svm_fit, test_df_svm)
# confusionMatrix(svm_cv_eval, test_df_svm$Performance.Tag)
#
# # Accuracy : 0.9394
# # Sensitivity : 0.98044
# # Specificity : 0.04457

# CONCLUSIONS : As SVM is taking up a lot of time to execute so we are commenting out the
#               code and the results of the same are mentioned in comments as well.

#########################################################################################
#########################################################################################
#                                    credit score calculation
#########################################################################################
#########################################################################################
## Building an application scorecard  
##
str(final_data)
final_data$perdict_default  <-
  rdmfor_pred_mod1 <-
  predict(rf_random, final_data[, -1], type = "prob")

final_data$perdict_default <- final_data$perdict_default[, 2]
final_data$perdict_default

#predict(final_log_model, type = "response", newdata = final_data[, -1])
final_data$predict_NonDefault <- 1 - final_data$perdict_default
final_data$odds <-
  log(final_data$predict_NonDefault / final_data$perdict_default)

Offset_scorecard = 400
PDO_scorecard = 20
log_odds_scorecard = 10
Factor_scorecard = PDO_scorecard / log(2)
Factor_scorecard  #28.8539

final_data$Score = ceiling(Offset_scorecard + (Factor_scorecard * final_data$odds))

str(final_data$Score)
summary(final_data$Score)
## min - 336 ; max - 528


quantile(final_data$Score, seq(0, 1, 0.1))

score_group <-
  ifelse(
    final_data$score >= 336 & final_data$core <= 445,
    "low",
    ifelse(final_data$Score > 445 &
        final_data$Score <= 494, "medium", "high")
  )


#Plotting histogram for scores we see maximum frequency score~510.This is further
#validated by the mode,
#However 510 is in the last 25% of the scores and we will end up rejecting most applicants
#Hence we choose cutoff=445, which is fourth decile

ggplot(final_data, aes(Score)) + geom_histogram()

#Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(final_data$Score)
#510

##  score cut off  set to 445

cutoff_scorecard = 445

defaults_lessthan_cutoff <-
  length(which(
    final_data$Performance.Tag == 1 &
      final_data$Score < cutoff_scorecard
  ))
defaults_total_no <- length(which(final_data$Performance.Tag == 1))

pc_defaults_lessthan_cutoff <-
  ceiling((defaults_lessthan_cutoff / defaults_total_no) * 100)

pc_defaults_lessthan_cutoff
#66


ggplot(final_data, aes(x = Score, color = Performance.Tag)) + geom_bar() +
  geom_vline(aes(xintercept = cutoff_scorecard)) + labs(x = "Score", y = "Count", title =
      "Score Distribution for all applicants") + annotate(
        "text",
        x = 350,
        y = 4000,
        colour = "black",
        hjust = 0,
        vjust = 0,
        size = 7,
        label =
          paste(
            "Defaults covered under 445 cut off : " ,
            pc_defaults_lessthan_cutoff,
            "%"
          )
      )
#########################################################################################
#                            Predicting score for rejected Data
#########################################################################################
str(rejected_data)

rejected_final_df$perdict_default  <-
  rejected_final_df$perdict_default[, 2]

rejected_final_df$predict_NonDefault <-
  1 - rejected_final_df$perdict_default
rejected_final_df$odds <-
  log(rejected_final_df$predict_NonDefault / rejected_final_df$perdict_default)

rejected_final_df$Score = ceiling(Offset_scorecard + (Factor_scorecard *
    rejected_final_df$odds))

summary(rejected_final_df$Score)
cutoff_score = 445
length(which(rejected_final_df$Score < cutoff_score)) / nrow(rejected_final_df) #0.8063158

correct_rejections_by_scorecard <-
  length(which(rejected_final_df$Score < cutoff_score)) / nrow(rejected_final_df) *
  100
correct_rejections_by_scorecard
ggplot(rejected_final_df, aes(x = Score)) + geom_bar() + geom_vline(aes(xintercept = cutoff_score, col =
    "blue")) + labs(x = "Score", y = "Count", title = "Score Distribution of Actual Rejected applications") +
  annotate(
    "text",
    x = 380,
    y = 1,
    colour = "white",
    hjust = 0,
    vjust = 0,
    size = 7,
    label =
      paste(
        "Corect rejections by score card% =",
        correct_rejections_by_scorecard
      )
  )

###########################################################################################
##                            Model using WOe Data
###########################################################################################

#install.packages("woeBinning")
library(woeBinning)
#install.packages("scorecard")
library(scorecard)
# woe binning ------
bins = woebin(final_data, "Performance.Tag")
data_woe = woebin_ply(final_data, bins)

#modelling using woe dataframe
logit_model_woe = glm(Performance.Tag ~ ., family = binomial(), data = data_woe)

logit_model_woe_2 <- stepAIC(logit_model_woe, direction = "both")
logit_model_woe_2 <- stepAIC(logit_model_woe, direction = "both")

logit_model_woe_3 <-
  glm(
    Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe +
      Avgas.CC.Utilization.in.last.12.months_woe + No.of.times.90.DPD.or.worse.in.last.6.months_woe +
      No.of.times.30.DPD.or.worse.in.last.6.months_woe + No.of.times.90.DPD.or.worse.in.last.12.months_woe +
      No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe +
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe +
      Profession.xSE_woe
    ,
    family = "binomial",
    data = data_woe
  )

summary(logit_model_woe_3)

logit_model_woe_4 <-
  glm(
    Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe +
      Avgas.CC.Utilization.in.last.12.months_woe  +
      No.of.times.90.DPD.or.worse.in.last.12.months_woe +
      No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe +
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe
    ,
    family = "binomial",
    data = data_woe
  )

summary(logit_model_woe_4)
install.packages("car")
library(car)
vif(logit_model_woe_4)



card = scorecard(
  bins,
  logit_model_woe_4,
  points0 = 400,
  odds0 = 1 / 9,
  pdo = 20
)

# credit score
final_data$score = scorecard_ply(final_data, card)

summary(final_data$score)
## min = 393 , max = 464

final_df_ordered_by_score <-
  final_data[order(final_data$score, decreasing = TRUE)]

#test_score = scorecard_ply(test_df, card, print_step=0)

final_df_ordered_by_score$color_code <-
  cut(
    final_df_ordered_by_score$score,
    breaks = c(-Inf, 400, 430, 460, Inf),
    labels = c("low", "low-medium", "medium-high", "high")
  )

uni_data_totals <-
  data.frame(table(final_df_ordered_by_score$color_code))

final_df_ordered_by_score$color_code <-
  as.character(final_df_ordered_by_score$color_code)
final_df_ordered_by_score$color_code  <-
  factor(
    final_df_ordered_by_score$color_code ,
    levels = unique(final_df_ordered_by_score$color_code)
  )

ggplot(final_df_ordered_by_score, aes(color_code, ..count..)) +
  geom_bar(aes(fill = Performance.Tag), position = "dodge")

###########################################################################################
#                             Random forest using woe data
###########################################################################################

set.seed(100)
rf_model_woe <- randomForest(
  Performance.Tag ~ .,
  data = data_woe,
  proximity = F,
  do.trace = F,
  mtry = 5,
  ntree = 501
)
ncol(train_dt)
summary(rf_model)

# making predictions on the test set
tree.predict <- predict(rf_model_woe, data_woe, type = "class")
# evaluating the results
confusionMatrix(tree.predict, data_woe$Performance.Tag, positive = "1")

#Results are not very prmising, hence following regular approach

#########################################################################################
#########################################################################################
#                            financial benefit analysis
#########################################################################################
#########################################################################################
approval_rate <-
  (nrow(combined_df) - nrow(rejected_data)) / nrow(combined_df) * 100

approval_rate
#Original approval rate is 98%


default_users_outstanding <-
  combined_df_1$Outstanding.Balance[which(combined_df_1$Performance.Tag ==
      1)]

current_credit_loss <- sum(default_users_outstanding)

current_credit_loss
#3711178158~3.7B

#Calculating % of defaulters originally
default_users_ID <-
  master_data1$Application.ID[which(master_data1$Performance.Tag == 1)]
pc_defaulters_original <-
  length(default_users_ID) / (nrow(combined_df) - nrow(rejected_data))
pc_defaulters_original

#original defaulters-4.2%

#After applying model and derinving score
#Creating subset of defaulters and outstanding amount
temp1 <-
  final_data$Outstanding.Balance[which(final_data$Performance.Tag == 1)]
temp2 <- final_data$Score[which(final_data$Performance.Tag == 1)]

outstanding <-
  cbind(
    default_users_ID,
    default_users_outstanding,
    scale(default_users_outstanding),
    temp1,
    temp2
  )


defaults_morethan_445 <-
  data.frame(subset(outstanding, temp2 >= 445))
defaulters_morethan_445 <-
  data.frame(subset(default_users_ID, temp2 >= 445))

pc_default_with_model <-
  nrow(defaulters_morethan_445) / nrow(data.frame(subset(final_data, Score >=
      445))) * 100
pc_default_with_model
#2.41%
#hence % of defaulters fall from 4.2% to 1.66% after applying model

sum(defaults_morethan_445$default_users_outstanding)

# New credit loss :1.3B

nrow(data.frame(subset(final_data, Score >= 445))) / nrow(final_data)
# New approval rate :   0.6022156
