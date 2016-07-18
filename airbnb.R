age_gender_bkts <- read.csv("age_gender_bkts.csv", h=T)
age_gender_bucks <- read.csv("age_gender_bucks.csv", h=T)
countries <- read.csv("countries.csv", h=T)
sessions <- read.csv("sessions.csv", h=T)
sessions.nona <- na.omit(sessions)
test <- read.csv("test_users.csv", h=T)
train <- read.csv("train_users.csv", h=T)
train.joined <- read.csv("train_joined2.csv", h=T)
test.joined <- read.csv("test_joined2.csv", h=T)
train[,"id"] <- as.character(train[,"id"])
train[,"date_account_created"] <- as.character(train[,"date_account_created"])
train[,"date_first_booking"] <- as.character(train[,"date_first_booking"])
mysubmish <- read.csv("my_submish.csv",h=T)
ben.submish <- read.csv("submit4.csv", h=T)

for (i in 2:(length(test))) {
  if (is.factor(test[,i])) {
    train.levels <- levels(train[,i])
    test.levels <- levels(test[,i])
    all.levels <- c(train.levels, test.levels)
    levels(train[,i]) <- all.levels
    levels(test[,i]) <- all.levels
  }
}

str(age_gender_bkts)
str(countries)
str(sessions)
str(train)


test <- read.csv('test_users.csv', header=T)
test.joined <- read.csv('test_joined.csv', header=T)
test.joined.no.na <- na.omit(test.joined)
train <- read.csv('train_users.csv', header=T)
train.no.na <- na.omit(train)
train.joined <- read.csv('train_joined.csv', header=T)
train.joined.no.na <- na.omit(train.joined)
train.ndf <- train.joined[train.joined$country_destination == 'NDF',]
train.ndf <- read.csv('train_ndf.csv', header=T)
sessions <- read.csv("sessions_expanded.csv", header=T)

# explore ndf
table(train.ndf$age, train.ndf$num_sessions)

length(which(train$country_destination == 'NDF')) / nrow(train)
length(which(train$country_destination == 'US')) / nrow(train)
length(which(train$country_destination == 'other')) / nrow(train)
length(which(train$country_destination == 'FR')) / nrow(train)
length(which(train$country_destination == 'CA')) / nrow(train)
length(which(train$country_destination == 'GB')) / nrow(train)
length(which(train$country_destination == 'ES')) / nrow(train)
length(which(train$country_destination == 'IT')) / nrow(train)
length(which(train$country_destination == 'PT')) / nrow(train)
length(which(train$country_destination == 'NL')) / nrow(train)
length(which(train$country_destination == 'DE')) / nrow(train)
length(which(train$country_destination == 'AU')) / nrow(train)

length(which(train$country_destination == 'NDF')) / nrow(train)
length(which(train$country_destination == 'US')) / nrow(train)
length(which(train$country_destination == 'other')) / nrow(train)
length(which(train$country_destination == 'FR')) / nrow(train)
length(which(train$country_destination == 'CA')) / nrow(train)
length(which(train$country_destination == 'GB')) / nrow(train)
length(which(train$country_destination == 'ES')) / nrow(train)
length(which(train$country_destination == 'IT')) / nrow(train)
length(which(train$country_destination == 'PT')) / nrow(train)
length(which(train$country_destination == 'NL')) / nrow(train)
length(which(train$country_destination == 'DE')) / nrow(train)
length(which(train$country_destination == 'AU')) / nrow(train)

# NDF, US, other, FR, IT, GB, ES

# balance NDF and US destinations
train.ndf <- subset(train.joined, country_destination == 'NDF')
nrow(train.ndf)  # 18989 
train.ndf.indices <- sample(1:nrow(train.ndf), 5000)
train.ndf.samp <- train.ndf[train.ndf.indices,]
train.us <- subset(train.joined, country_destination == 'US')
nrow(train.us)  # 8599
train.us.indices <- sample(1:nrow(train.us), 5000)  
train.us.samp <- train.us[train.us.indices,]

mean(na.omit(train.us$age)) # 42.98
length(which(train.us$gender == 'MALE')) # 2541  
length(which(train.us$gender == 'FEMALE')) # 3032
length(which(train.us$gender == '-unknown-')) # 3018
length(which(train.us$gender == 'OTHER')) # 8

length(which(train.us$gender == 'MALE')) / nrow(train.us) # .2954  
length(which(train.us$gender == 'FEMALE')) / nrow(train.us) # .3525
length(which(train.us$gender == '-unknown-')) / nrow(train.us) # .3509
length(which(train.us$gender == 'OTHER')) / nrow(train.us) # .0009

# run a rf on train data for country destination us
train.joined$destination_us <- as.factor(ifelse(train.joined$country_destination == 'US', 1, 0))

train.indices <- sample(nrow(train.joined), floor(.75*nrow(train.joined)))
train.sub <- train.joined[train.indices,]
test.sub <- train.joined[-train.indices,]

# impute missing vals then run rf
library(randomForest)

# train.joined.no.na$destination_us <- as.factor(ifelse(train.joined.no.na$country_destination == 'US', 1, 0))
train.joined.no.na$destination_ndf <- as.factor(ifelse(train.joined.no.na$country_destination == 'NDF', 1, 0))
train.no.na <- train.joined.no.na
train.no.na$date_account_created <- as.Date(train.joined.no.na$date_account_created)
train.no.na$date_first_booking <- as.Date(train.joined.no.na$date_first_booking)
nrow(train.no.na)
train.no.na <- na.omit(train.no.na)
nrow(train.joined.no.na)

train.indices <- sample(nrow(train.joined.no.na), floor(.75*nrow(train.joined.no.na)))
train.sub <- train.joined.no.na[train.indices,]
test.sub <- train.joined.no.na[-train.indices,]
n <- length(which(train.sub$destination_ndf == 0))
train.sub.ndf <- which(train.sub$destination_ndf == 1)
balance.indices <- sample(train.sub.ndf, n)
balance.indices <- c(balance.indices, which(train.sub$destination_ndf == 0))
train.sub.balanced <- train.sub[balance.indices,]

use_cols <- c("destination_us", "sum_seconds", "num_sessions", 
              "num_actions", "signup_app", "first_device_type", 
              "first_browser", "gender", "age","date_account_created",
              "date_first_booking", "timestmp_first_active", "signup_method",
              "affiliate_channel", "affiliate_provider", "first_affiliate_tracked"
              )

use_cols <- c("destination_ndf", "sum_seconds", "num_sessions", "num_actions",
              "signup_app", "first_device_type", "first_browser",
              "gender", "age", "signup_method", "affiliate_channel",
              "first_affiliate_tracked")

rf.fit <- randomForest(formula=destination_ndf ~ ., data=train.sub[,use_cols], maxnodes=7, ntree=5000)

rf.pred.vals <- predict(rf.fit, test.sub[,use_cols[-1]])

table(rf.pred.vals, test.sub$destination_ndf)

confusionMatrix(test.sub$destination_ndf, rf.pred.vals, positive="1")

# glm to see se associated with each covariate

log.fit <- glm(formula=destination_ndf ~ ., data=train.sub[,use_cols], family="binomial")
summary(log.fit)
pred.vals <- predict(log.fit, test.sub[,use_cols[-1]], type="response") # test first_browser has new levels. remove first_browser

use_cols <- c("destination_ndf", "sum_seconds", "num_sessions", "num_actions",
              "signup_app", "first_device_type", "gender", "age", 
              "signup_method", "affiliate_channel",
              "first_affiliate_tracked")

use_cols <- c("destination_ndf", "num_actions", "gender", "signup_method")

log.fit <- glm(formula=destination_ndf ~ ., data=train.sub[,use_cols], family="binomial")
summary(log.fit)
pred.probs <- predict(log.fit, test.sub[,use_cols[-1]], type="response")

log.pred.vals <- rep(0, length(pred.probs))
log.pred.vals[which(pred.probs > .5)] <- 1

table(log.pred.vals, test.sub$destination_ndf)
confusionMatrix(test.sub$destination_ndf, log.pred.vals, positive="1")

# impute train values
train$destination_ndf <- as.factor(ifelse(train$country_destination == 'NDF', 1, 0))

train$date_first_booking[which(as.character(train$date_first_booking) == "")] <- NA
train$date_account_created[which(as.character(train$date_account_created) == "")] <- NA
train$date_first_booking <- as.Date(as.character(train$date_first_booking))
train$date_account_booking <- as.Date(as.character(train$date_account_created))

# svm
library(e1071)

?svm
svm.fit <- svm(destination_ndf ~ ., data=train.sub[,use_cols])
svm.pred <- predict(svm.fit, test.sub[,use_cols][-1], type="response")
table(svm.pred, test.sub$destination_ndf)
confusionMatrix(test.sub$destination_ndf, svm.pred, positive="1")


# inspect which test rows have "" date_first_booking
train.date.1st.booking.na <- train[which(is.na(train$date_first_booking)),]
summary(train.date.1st.booking.na$destination_ndf)

# just guess ndf for all test rows with empty string date_first_booking
# then just guess country US for all other rows

id <- test$id
is.ndf <- ifelse(test$date_first_booking == "", 'NDF', 'US')

my_submish <- data.frame(id=id, country=is.ndf)
head(my_submish)
write.csv(my_submish, "my_submish.csv", row.names=F) # .88 

# determine which rows going to US
us <- subset(train.joined, country_destination != 'NDF')
us$destination_us <- as.factor(ifelse(us$country_destination == 'US', 1, 0))

# > length(which(train.us$destination_us == 1))
# [1] 8599
# > length(which(train.us$destination_us == 0))
# [1] 3614

# let's balance the data to determine who is going to US destinations
us.nona <- na.omit(us)
summary(us)
library(randomForest)
library(e1071)
dont.use <- c("country_destination", "id", "date_first_booking", "date_account_created")

train.indices <- sample(nrow(us.nona), floor(.75*nrow(us.nona)))
train.us <- us.nona[train.indices,]
test.us <- us.nona[-train.indices,]

rf.us.fit <- randomForest(formula=destination_us ~ .,
                          data=train.us[, -match(dont.use, names(train.us))], 
                          maxnodes=17, 
                          ntree=1000)

us.pred.vals <- predict(rf.us.fit, test.us[,-match(c("destination_us", dont.use), names(test.us))])

table(test.us$destination_us, us.pred.vals)

confusionMatrix(test.us$destination_us, us.pred.vals, positive="1")


us.impute <- rfImpute(x=us[,-match(c(dont.use, "destination_us"), names(us))],
                      y=us$destination_us,
                      iter=5, ntree=300)

train.indices <- sample(nrow(us.impute), floor(.75*nrow(us.impute)))
train.us.impute <- us.impute[train.indices,]
test.us.impute <- us.impute[-train.indices,]

rf.us.impute.fit <- randomForest(formula=destination_us ~ .,
                          data=train.us.impute, 
                          maxnodes=17, 
                          ntree=1000)

us.impute.pred.vals <- predict(rf.us.impute.fit, test.us.impute[,-match("destination_us", names(test.us.impute))])

table(test.us.impute$destination_us, us.impute.pred.vals)

confusionMatrix(test.us.impute$destination_us, us.impute.pred.vals, positive="1")

# logistic on imputed df

log.us.fit <- glm(destination_us ~ ., data=train.us.impute, family="binomial")
# remove rows with new languages :(
test.us.impute.lang <- test.us.impute[which(!(test.us.impute$language %in% c("el", "hu"))),]
log.probs.us <- predict(log.us.fit, test.us.impute.lang[,-match("destination_us", names(test.us.impute.lang))], type="response")
# 

# naive bayes
nb.fit <- naiveBayes(destination_us ~ ., data=train.us.impute)
nb.pred <- predict(nb.fit, test.us.impute[,match("destination_us", names(test.us.impute))], type="class")
table(test.us.impute$destination_us, nb.pred)
confusionMatrix(test.us.impute$destination_us, nb.pred)

nb.fit <- naiveBayes(destination_us ~ ., data=train.us)
nb.pred <- predict(nb.fit, test.us[,match("destination_us", names(test.us))], type="class")
table(test.us$destination_us, nb.pred)
confusionMatrix(test.us$destination_us, nb.pred)



table(train.us$destination_us, train.us$gender)
table(train.us$destination_us, sort(train.us$age))
table(train.us$destination_us, train.us$signup_method)
table(train.us$destination_us, train.us$signup_app)
table(train.us$destination_us, train.us$signup_flow)
table(train.us$destination_us, train.us$language)
table(train.us$destination_us, train.us$affiliate_channel)
table(train.us$destination_us, train.us$affiliate_provider)
table(train.us$destination_us, train.us$first_affiliate_tracked)
table(train.us$destination_us, train.us$first_device_type)
table(train.us$destination_us, train.us$first_browser)
table(train.us$destination_us, train.us$sum_seconds)
table(train.us$destination_us, train.us$num_sessions)
table(train.us$destination_us, train.us$num_actions)


head(sessions)


####################################################################
library(randomForest)

ndfs <- ifelse(as.character(test$date_first_booking == ""), 'NDF', '')
test$country <- ndfs

train.us <- train.joined[train.joined$country_destination != 'NDF',]
train.us$country_destination <- factor(train.us$country_destination)
train.us <- train.us[,-match(c("id", "date_first_booking", "date_account_created"), names(train.us))]
nrow(train.us) # 12213
train.us.impute <- rfImpute(country_destination ~ ., data=train.us)

write.csv(train.us.impute, "train_us_impute.csv")

head(train.us.impute)
vars <- c("timestmp_first_active", "gender",
          "age", "signup_method", "signup_flow", "language",
          "affiliate_channel", "affiliate_provider", "first_affiliate_tracked",
          "signup_app", "first_device_type", "first_browser", "sum_seconds",
          "num_sessions", "num_actions", "num_action_detail")

train.us.impute$destination_us <- as.factor(ifelse(train.us.impute$country_destination == 'US', 1, 0))
train.us.impute$destination_other <- as.factor(ifelse(train.us.impute$country_destination == 'other', 1, 0))
train.us.impute$destination_fr <- as.factor(ifelse(train.us.impute$country_destination == 'FR', 1, 0))
train.us.impute$destination_it <- as.factor(ifelse(train.us.impute$country_destination == 'IT', 1, 0))
train.us.impute$destination_gb <- as.factor(ifelse(train.us.impute$country_destination == 'GB', 1, 0))
train.us.impute$destination_es <- as.factor(ifelse(train.us.impute$country_destination == 'ES', 1, 0))
train.us.impute$destination_nl <- as.factor(ifelse(train.us.impute$country_destination == 'NL', 1, 0))
train.us.impute$destination_au <- as.factor(ifelse(train.us.impute$country_destination == 'AU', 1, 0))
train.us.impute$destination_de <- as.factor(ifelse(train.us.impute$country_destination == 'DE', 1, 0))
train.us.impute$destination_ca <- as.factor(ifelse(train.us.impute$country_destination == 'CA', 1, 0))
train.us.impute$destination_pt <- as.factor(ifelse(train.us.impute$country_destination == 'PT', 1, 0))

train.indices <- sample(nrow(train.us.impute), floor(.75*nrow(train.us.impute)))
train.sub <- train.us.impute[train.indices,]
test.sub <- train.us.impute[-train.indices,]

train.sub.impute <- rfImpute(country_destination ~ ., data=train.sub)

us.fit <- randomForest(destination_us ~ ., 
                       data=train.sub[,c(vars, "destination_us")], 
                       maxnodes=17,
                       ntrees=1000)
us.probs <- predict(us.fit, test.sub[,!(vars %in% c("destination_us"))], type="prob")

other.fit <- randomForest(destination_other ~ ., 
                       data=train.sub[,c(vars, "destination_other")], 
                       maxnodes=17,
                       ntrees=1000)
other.probs <- predict(other.fit, test.sub[,!(vars %in% c("destination_other"))], type="prob")

fr.fit <- randomForest(destination_fr ~ ., 
                       data=train.sub[,c(vars, "destination_fr")], 
                       maxnodes=17,
                       ntrees=1000)
fr.probs <- predict(fr.fit, test.sub[,!(vars %in% c("destination_fr"))], type="prob")

it.fit <- randomForest(destination_it ~ ., 
                       data=train.sub[,c(vars, "destination_it")], 
                       maxnodes=17,
                       ntrees=1000)
it.probs <- predict(it.fit, test.sub[,!(vars %in% c("destination_it"))], type="prob")

gb.fit <- randomForest(destination_gb ~ ., 
                       data=train.sub[,c(vars, "destination_gb")], 
                       maxnodes=17,
                       ntrees=1000)
gb.probs <- predict(gb.fit, test.sub[,!(vars %in% c("destination_gb"))], type="prob")

es.fit <- randomForest(destination_es ~ ., 
                       data=train.sub[,c(vars, "destination_es")], 
                       maxnodes=17,
                       ntrees=1000)
es.probs <- predict(es.fit, test.sub[,!(vars %in% c("destination_es"))], type="prob")

nl.fit <- randomForest(destination_nl ~ ., 
                       data=train.sub[,c(vars, "destination_nl")], 
                       maxnodes=17,
                       ntrees=1000)
nl.probs <- predict(nl.fit, test.sub[,!(vars %in% c("destination_nl"))], type="prob")

au.fit <- randomForest(destination_au ~ ., 
                       data=train.sub[,c(vars, "destination_au")], 
                       maxnodes=17,
                       ntrees=1000)
au.probs <- predict(us.fit, test.sub[,!(vars %in% c("destination_au"))], type="prob")

de.fit <- randomForest(destination_us ~ ., 
                       data=train.sub[,c(vars, "destination_us")], 
                       maxnodes=17,
                       ntrees=1000)
de.probs <- predict(us.fit, test.sub[,!(vars %in% c("destination_us"))], type="prob")

ca.fit <- randomForest(destination_us ~ ., 
                       data=train.sub[,c(vars, "destination_us")], 
                       maxnodes=17,
                       ntrees=1000)
ca.probs <- predict(us.fit, test.sub[,!(vars %in% c("destination_us"))], type="prob")

pt.fit <- randomForest(destination_us ~ ., 
                       data=train.sub[,c(vars, "destination_us")], 
                       maxnodes=17,
                       ntrees=1000)
pt.probs <- predict(us.fit, test.sub[,!(vars %in% c("destination_us"))], type="prob")