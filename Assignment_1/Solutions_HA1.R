#Setup ----
#Clear Environment Variables
rm(list = ls())

#Setup working directory to path where file is present
setwd('E:/Data_Science_Masters_Program/Statistical_Learning/Home_Assignment_1')
filename = 'ANES2016.XLSX'
library(readxl)
library(leaps)
library(class)

#Load excel file into data frame and analyse structure and summary
df <- data.frame(read_excel(filename))
str(df)
summary(df)

# Question 1.A ----

# Make a copy of the actual data frame to work on, with retaining a copy of original
df_trump <- df

# Since Trump and Hillary are identically structured columns, 
# and Trump needs to be transformed, Hillary is also converted in same format
# to understand their relation
df_trump$Trump <- ifelse(df_trump$Trump > 0, ifelse(df_trump$Trump > 3, 1,0),NA)
df_trump$Hillary <- ifelse(df_trump$Hillary > 0, ifelse(df_trump$Hillary > 3, 1,0),NA)
df_trump <- na.omit(df_trump)
nrow(df_trump[df_trump$Trump != df_trump$Hillary,]) 
# 3062 out of 4017 rows have opposite values in Trump and Hillary columns
# This shows higher relation of Hillary on Trump 

# Transform and categorize Trump as required
df_trump$Trump <- ifelse(df_trump$Trump == 1, 'Conservative','Liberal')
df_trump$Trump <- factor(df_trump$Trump)
table(df_trump$Trump)

# Get list of all columns
all_cols <- colnames(df_trump)

# Understanding the unique values in each column
for (col in all_cols[2:18]) { print(col); print(table(df_trump[col])) }

# All the columns excluding below needs to be filtered with rows having values less than 0
cols <- all_cols[!all_cols %in% c('ID','Hillary','Trump','Partner','SpouseEdu')]

# Function to drop rows
drop_rows <- function(dft,col,val) { return(dft[dft[col] >= val,]) }

# Loop to drop rows for 13 columns having valus less than 0
for (col in cols) {df_trump <- drop_rows(df_trump,col,0)}

# Loop to drop rows for 2 columns having values less than -1, 
# As -1 has a significant number of occurrences and gives important information
for (col in c('Partner','SpouseEdu')) {df_trump <- drop_rows(df_trump,col,-1)}

# Function to summarize the GLM computed for each field with trump as response
check_cols <- all_cols[!all_cols %in% c('ID','Trump')]
check_relation <- function(col) {
  glm_trump <- glm(formula = reformulate(termlabels = col, response = 'Trump'), 
                   data = df_trump, family = 'binomial')
  print(summary(glm_trump))
}

# Loop to perform model summerization 
for (col in check_cols) {check_relation(col)}

# Model to analyze the parameters considering interactions with and within features
glm_all <- glm(Trump~ Media+FamSize+Age+Hillary+Partner+Education+SpouseEdu+ 
            Employment+Birthplace+GBirth+Dependent+Housing+Income+ 
            Education2+PartyID+Marital, data = df_trump, family = 'binomial')
summary(glm_all)

# Model to analyze the parameters considering interactions with and within the 15 features
glm_all2 <- glm(Trump~ Media+FamSize+Age+Partner+Education+SpouseEdu+ 
                 Employment+Birthplace+GBirth+Dependent+Housing+Income+ 
                 Education2+PartyID+Marital, data = df_trump, family = 'binomial')
summary(glm_all2)

# Question 1.B ----

# Make a copy of the actual data frame to work on, with retaining a copy of original
df_party <- df

# Function to drop rows
drop_rows <- function(dfp,col,val) { return(dfp[dfp[col] >= val,]) }

cols_list <- colnames(df_party)
cols_list_filter <- cols_list[!cols_list %in% c('ID','Partner','SpouseEdu')]
#cols_list_factor <- cols_list[!cols_list %in% c('ID','Age','SpouseEdu')]

for (col in cols_list[2:18]) { print(col); print(table(df_party[col])) }

# Loop to drop rows for 13 columns having valus less than 0
for (col in cols_list_filter) {df_party <- drop_rows(df_party,col,0)}

# Loop to drop rows for 2 columns having values less than -1, 
# As -1 has a significant number of occurrences and gives important information
for (col in c('Partner','SpouseEdu')) {df_party <- drop_rows(df_party,col,-1)}

df_party <- na.omit(df_party)
sum(is.na(df_party))

# Drop ID Column
df_party <- df_party[2:18]

# Categorizing response variable
df_party$PartyID <- factor(df_party$PartyID)

# Create Train and Test Data for Validation
set.seed(7)
dt = sort(sample(nrow(df_party), nrow(df_party)*.75))
train<-df_party[dt,]
test<-df_party[-dt,]
Y<-test$PartyID

# Multinomial Logistic Model  with all the variables
party_multinom <- multinom(PartyID ~ ., data = train)
predict_fit <- predict(party_multinom,newdata = test)
table(predict_fit,Y)
mean(Y == predict_fit)
summary(party_multinom)

# Best Subset Selection
party_best_subset_full <- regsubsets(PartyID~.,train,nvmax=16)
pbs_summary <- summary(party_best_subset_full)
names(pbs_summary)
plot(pbs_summary$rss, xlab="No of Variables", ylab ="rss")
plot(pbs_summary$cp, xlab="No of Variables", ylab ="Cp")
pbs_cp_rss <- which.min(pbs_summary$rss)
pbs_cp_min <- which.min(pbs_summary$cp)
coef(party_best_subset_full,pbs_cp_rss)
coef(party_best_subset_full,pbs_cp_min)
best_columns <- names(coef(party_best_subset_full,pbs_cp_min))
best_columns <- best_columns[2:length(best_columns)]
train_bs <- train[c('PartyID',best_columns)]
test_bs <- test[c('PartyID',best_columns)]

# Multinomial Logistic Model with best subset selected columns
party_multinom_bs <- multinom(PartyID ~ ., data = train_bs)
predict_fit_bs <- predict(party_multinom_bs,newdata = test_bs)
table(predict_fit_bs,Y)
mean(Y == predict_fit_bs)
summary(party_multinom_bs)

# KNN Model with best subset selected columns
set.seed(7)
party_knn <- knn(train = train_bs,test = test_bs, cl=train_bs$PartyID,k=5)
round(prop.table(xtabs(~Y+party_knn),1),4)
mean(party_knn==test_bs$PartyID)

party_knn2 <- knn(train=train_bs,test=test_bs,cl=train_bs$PartyID,k=10)
round(prop.table(xtabs(~Y+party_knn2),1),4)
mean(party_knn2==test_bs$PartyID)