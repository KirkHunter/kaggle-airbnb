
train <- read.csv("train_users.csv", h=T)
test <- read.csv("test_users.csv", h=T)

train.joined <- read.csv("train_joined2.csv", h=T)
test.joined <- read.csv("test_joined2.csv", h=T)

train.joined$timestamp_first_active <- train.joined$timestmp_first_active
test.joined$timestamp_first_active <- test.joined$timestmp_first_active
################################################################################
# rbind test and train tables to unionize factor levels

test$type <- "test"
train$type <- "train"
test.joined$type <- "test"
train.joined$type <- "train"
country.destination <- train$country_destination
country.destination.joined <- train.joined$country_destination

temp.union <- rbind(train[,-match("country_destination", names(train))], test)
temp.joined.union <- rbind(train.joined[-match("country_destination", names(train))], test.joined)

train <- temp.union[which(temp.union$type == "train"),]
test <- temp.union[which(temp.union$type == "test"),]
train.joined <- temp.joined.union[which(temp.joined.union$type == "train"),] 
test.joined <- temp.joined.union[which(temp.joined.union$type == "test"),]

train$country_destination <- factor(country.destination)
train.joined$country_destination <- factor(country.destination.joined)

################################################################################

# separate test observations with date_first_booking = ""
test.ndf.indices <- which(test$date_first_booking == "")
test.ndf.ids <- test[test.ndf.indices, "id"]
length(test.ndf.ids) # 27151

# test joined ids that are not ndf
ids <- test$id
test.non.ndf.ids <- setdiff(ids, test.ndf.ids)
test.joined.ids <- test.non.ndf.ids[which(test.non.ndf.ids %in% test.joined$id)]
length(test.joined.ids) # 16458

# test that are not ndf and not in joined
test.else.ids <- setdiff(test.non.ndf.ids, test.joined.ids)
length(test.else.ids) # 64

################################################################################
# subset test and train sets into those observations with date_first_booking != ""

train.joined.ndf.indices <- which(train.joined$country_destination == 'NDF')
train.joined <- train.joined[-train.joined.ndf.indices,]

test.joined.ndf.indices <- which(test.joined$date_first_booking == "")
test.joined <- test.joined[-test.joined.ndf.indices,]

train.joined$country_destination <- factor(train.joined$country_destination)

################################################################################

# create custom binary response variables for each of the 12 country destinations

train.joined$destination_us <- as.factor(ifelse(train.joined$country_destination == 'US', 1, 0))
train.joined$destination_other <- as.factor(ifelse(train.joined$country_destination == 'other', 1, 0))
train.joined$destination_fr <- as.factor(ifelse(train.joined$country_destination == 'FR', 1, 0))
train.joined$destination_it <- as.factor(ifelse(train.joined$country_destination == 'IT', 1, 0))
train.joined$destination_gb <- as.factor(ifelse(train.joined$country_destination == 'GB', 1, 0))
train.joined$destination_es <- as.factor(ifelse(train.joined$country_destination == 'ES', 1, 0))
train.joined$destination_nl <- as.factor(ifelse(train.joined$country_destination == 'NL', 1, 0))
train.joined$destination_au <- as.factor(ifelse(train.joined$country_destination == 'AU', 1, 0))
train.joined$destination_de <- as.factor(ifelse(train.joined$country_destination == 'DE', 1, 0))
train.joined$destination_ca <- as.factor(ifelse(train.joined$country_destination == 'CA', 1, 0))
train.joined$destination_pt <- as.factor(ifelse(train.joined$country_destination == 'PT', 1, 0))

train$destination_us <- as.factor(ifelse(train$country_destination == 'US', 1, 0))
train$destination_other <- as.factor(ifelse(train$country_destination == 'other', 1, 0))
train$destination_fr <- as.factor(ifelse(train$country_destination == 'FR', 1, 0))
train$destination_it <- as.factor(ifelse(train$country_destination == 'IT', 1, 0))
train$destination_gb <- as.factor(ifelse(train$country_destination == 'GB', 1, 0))
train$destination_es <- as.factor(ifelse(train$country_destination == 'ES', 1, 0))
train$destination_nl <- as.factor(ifelse(train$country_destination == 'NL', 1, 0))
train$destination_au <- as.factor(ifelse(train$country_destination == 'AU', 1, 0))
train$destination_de <- as.factor(ifelse(train$country_destination == 'DE', 1, 0))
train$destination_ca <- as.factor(ifelse(train$country_destination == 'CA', 1, 0))
train$destination_pt <- as.factor(ifelse(train$country_destination == 'PT', 1, 0))

# get rid of ndf observations from train set
train.ndf <- train[which(train$country_destination == 'NDF'),]
train <- train[which(train$country_destination != 'NDF'),]

################################################################################
# set date variables to date and create date numeric variables
train.joined$date_first_booking <- as.Date(train.joined$date_first_booking)
train.joined$date_account_created <- as.Date(train.joined$date_account_created)
train$date_first_booking <- as.Date(train$date_first_booking)
train$date_account_created <- as.Date(train$date_account_created)
test.joined$date_first_booking <- as.Date(test.joined$date_account_created)
test.joined$date_account_created <- as.Date(test.joined$date_account_created)
test$date_first_booking <- as.Date(test$date_account_created)
test$date_account_created <- as.Date(test$date_account_created)

# convert dates to int
train.joined$numeric_date_first_booking <- as.numeric(train.joined$date_first_booking)
train.joined$numeric_date_account_created <- as.numeric(train.joined$date_account_created)
train$numeric_date_first_booking <- as.numeric(train$date_first_booking)
train$numeric_date_account_created <- as.numeric(train$date_account_created)
test.joined$numeric_date_first_booking <- as.numeric(test.joined$date_first_booking)
test.joined$numeric_date_account_created <- as.numeric(test.joined$date_account_created)
test$numeric_date_first_booking <- as.numeric(test$date_first_booking)
test$numeric_date_account_created <- as.numeric(test$date_account_created)

################################################################################

# norwegian.id <- which(test.joined$language == 'no')
# norwegian <- test.joined[norwegian.id,]
# norwegian.guess <- data.frame(id=rep(norwegian$id, 5), country=c('US', 'other', 'FR', 'IT', 'ES'))
# 
# test.joined <- test.joined[-norwegian.id,]

################################################################################
# create a factor variable that indicates whether first device type is iphone
# since this variable is significant for italy
# create several other custom covariates

train.joined$first_device_iphone <- factor(ifelse(train.joined$first_device_type == 'iPhone', 1, 0))
train$first_device_iphone <- factor(ifelse(train$first_device_type == 'iPhone', 1, 0))
test.joined$first_device_iphone <- factor(ifelse(test.joined$first_device_type == 'iPhone', 1, 0))
test$first_device_iphone <- factor(ifelse(test$first_device_type == 'iPhone', 1, 0))

# is_male is significant for 'other'
train.joined$is_male <- factor(ifelse(train.joined$gender == 'MALE', 1, 0))
train$is_male <- factor(ifelse(train$gender == 'MALE', 1, 0))
test.joined$is_male <- factor(ifelse(test.joined$gender == 'MALE', 1, 0))
test$is_male <- factor(ifelse(test$gender == 'MALE', 1, 0))

# is_female for great britain
train.joined$is_female <- factor(ifelse(train.joined$gender == 'FEMALE', 1, 0))
train$is_female <- factor(ifelse(train$gender == 'FEMALE', 1, 0))
test.joined$is_female <- factor(ifelse(test.joined$gender == 'FEMALE', 1, 0))
test$is_female <- factor(ifelse(test$gender == 'FEMALE', 1, 0))

# is_signup app web for US
train.joined$is_signup_app_web <- factor(ifelse(train.joined$signup_app == 'Web', 1, 0))
train$is_signup_app_web <- factor(ifelse(train$signup_app == 'Web', 1, 0))
test.joined$is_signup_app_web <- factor(ifelse(test.joined$signup_app == 'Web', 1, 0))
test$is_signup_app_web <- factor(ifelse(test$signup_app == 'Web', 1, 0))

# spain affiliate channel is 'content'
train.joined$affiliate_channel_content <- factor(ifelse(train.joined$affiliate_channel == 'content', 1, 0))
train$affiliate_channel_content <- factor(ifelse(train$affiliate_channel == 'content', 1, 0))
test.joined$affiliate_channel_content <- factor(ifelse(test.joined$affiliate_channel == 'content', 1, 0))
test$affiliate_channel_content <- factor(ifelse(test$affiliate_channel == 'content', 1, 0))

# gender is 'other' canada
train.joined$gender_is_other <- factor(ifelse(train.joined$gender == 'OTHER', 1, 0))
train$gender_is_other <- factor(ifelse(train$gender == 'OTHER', 1, 0))
test.joined$gender_is_other <- factor(ifelse(test.joined$gender == 'OTHER', 1, 0))
test$gender_is_other <- factor(ifelse(test$gender == 'OTHER', 1, 0))

# signup app moweb for 'other'
train.joined$signup_app_moweb <- factor(ifelse(train.joined$signup_app == 'Moweb', 1, 0))
train$signup_app_moweb <- factor(ifelse(train$signup_app == 'Moweb', 1, 0))
test.joined$signup_app_moweb <- factor(ifelse(test.joined$signup_app == 'Moweb', 1, 0))
test$signup_app_moweb <- factor(ifelse(test$signup_app == 'Moweb', 1, 0))

# train affiliate channel sem-brand
train$affiliate_channel_sembrand <- factor(ifelse(train$affiliate_channel == 'sem-brand', 1, 0))
train.joined$affiliate_channel_sembrand <- factor(ifelse(train.joined$affiliate_channel == 'sem-brand', 1, 0))
test.joined$affiliate_channel_sembrand <- factor(ifelse(test.joined$affiliate_channel == 'sem-brand', 1, 0))
test$affiliate_channel_sembrand <- factor(ifelse(test$affiliate_channel == 'sem-brand', 1, 0))

################################################################################

# signup method is significant. But the presence of signup method google 
# in the test is breaking our model. Let's subset the test set further into those
# who signed up through google, and those who did not.

google.indices <- which(test.joined$signup_method == 'google')
length(google.indices) # 100
test.joined.google <- test.joined[google.indices,]
test.joined.non.google <- test.joined[-google.indices,]

test.joined.non.google$signup_method <- factor(test.joined.non.google$signup_method)

################################################################################
# run logistic on non google test set where age is not NA

train.age.indices <- which(!(is.na(train.joined$age)))
train.joined.age <- train.joined[train.age.indices,]
train.age <- train[which(!(is.na(train$age))),]
test.age.indices <- which(!(is.na(test.joined.non.google$age)))
test.joined.age <- test.joined.non.google[test.age.indices,]

j.train <- train.joined
j.train.joined.age <- train.joined.age
j.train.age <- train.age
j.test <- test.joined.age

vars <- c("num_sessions", "num_actions", "num_action_detail", 
          "is_signup_app_web")
us.fit <- glm(destination_us ~ ., 
              data=j.train[,c("destination_us", vars)],
              family="binomial")
summary(us.fit)
us.pred <- predict(us.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "is_male")
other.fit <- glm(destination_other ~ ., 
                 data=j.train[,c("destination_other", vars)],
                 family="binomial")
summary(other.fit)
other.pred <- predict(other.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "num_actions", "numeric_date_first_booking")
fr.fit <- glm(destination_fr ~ ., 
              data=j.train[,c("destination_fr", vars)],
              family="binomial")
summary(fr.fit)
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

vars <- c("is_female", "is_signup_app_web", "signup_flow",
          "timestamp_first_active")
it.fit <- glm(destination_it ~ ., 
              data=train[,c("destination_it", vars)],
              family="binomial")
summary(it.fit)
it.pred <- predict(it.fit, j.test[,vars], type="response")

vars <- c("is_signup_app_web", "is_female")
gb.fit <- glm(destination_gb ~ ., 
              data=train[,c("destination_gb", vars)],
              family="binomial")
summary(gb.fit)
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

vars <- c("is_male", "is_signup_app_web", "numeric_date_first_booking")
de.fit <- glm(destination_de ~ ., 
              data=train[,c("destination_de", vars)],
              family="binomial")
summary(de.fit)
de.pred <- predict(de.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "affiliate_channel_content")
es.fit <- glm(destination_es ~ ., 
              data=j.train[,c("destination_es", vars)],
              family="binomial")
summary(es.fit)
es.pred <- predict(es.fit, j.test[,vars], type="response")

vars <- c("is_male", "numeric_date_account_created", "first_device_iphone")
nl.fit <- glm(destination_nl ~ ., 
              data=train[,c("destination_nl", vars)],
              family="binomial")
summary(nl.fit)
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

vars <- c("is_female", "affiliate_channel_content",
          "numeric_date_account_created", "timestamp_first_active")
au.fit <- glm(destination_au ~ ., 
              data=train[,c("destination_au", vars)],
              family="binomial")
summary(au.fit)
au.pred <- predict(au.fit, j.test[,vars], type="response")

vars <- c("is_female", "numeric_date_first_booking",
          "first_device_iphone")
ca.fit <- glm(destination_ca ~ ., 
              data=train[,c("destination_ca", vars)],
              family="binomial")
summary(ca.fit)
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

vars <- c("affiliate_channel_sembrand")
pt.fit <- glm(destination_pt ~ ., 
              data=train[,c("destination_pt", vars)],
              family="binomial") 
summary(pt.fit)
pt.pred <- predict(pt.fit, data.frame(affiliate_channel_sembrand=j.test[,vars]), type="response")
# pt.pred <- rep(0.002205668, length(ca.pred))

country.probs <- data.frame(US=us.pred,
                            other=other.pred,
                            FR=fr.pred,
                            IT=it.pred,
                            GB=gb.pred,
                            ES=es.pred,
                            NL=nl.pred,
                            AU=au.pred,
                            DE=de.pred,
                            CA=ca.pred,
                            PT=pt.pred)

head(country.probs)

get_five_countries <- function(i) {
  return(names(country.probs[i,][order(country.probs[i,], decreasing=T)][,1:5]))
}

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

# predictions for test.joined set
j.submish.age <- data.frame(id=j.ids, country=countries)
# j.submish <- data.frame(id=j.ids, country=countries)
################################################################################
# run logistic on non google test set where age is NA

j.train <- train.joined
j.test <- test.joined.non.google[-test.age.indices,]

vars <- c("num_sessions", "num_actions", "num_action_detail", 
          "is_signup_app_web")
us.fit <- glm(destination_us ~ ., 
              data=j.train[,c("destination_us", vars)],
              family="binomial")
summary(us.fit)
us.pred <- predict(us.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "is_male")
other.fit <- glm(destination_other ~ ., 
                 data=j.train[,c("destination_other", vars)],
                 family="binomial")
summary(other.fit)
other.pred <- predict(other.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "num_actions", "numeric_date_first_booking", 
          "first_device_iphone")
fr.fit <- glm(destination_fr ~ ., 
              data=j.train[,c("destination_fr", vars)],
              family="binomial")
summary(fr.fit)
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

vars <- c("first_device_iphone", "is_female")
it.fit <- glm(destination_it ~ ., 
              data=j.train.age[,c("destination_it", vars)],
              family="binomial")
summary(it.fit)
it.pred <- predict(it.fit, j.test[,vars], type="response")

vars <- c("is_signup_app_web", "is_female")
gb.fit <- glm(destination_gb ~ ., 
              data=j.train[,c("destination_gb", vars)],
              family="binomial")
summary(gb.fit)
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

vars <- c("is_male", "num_actions")
de.fit <- glm(destination_de ~ ., 
              data=j.train[,c("destination_de", vars)],
              family="binomial")
summary(de.fit)
de.pred <- predict(de.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "affiliate_channel_content")
es.fit <- glm(destination_es ~ ., 
              data=j.train[,c("destination_es", vars)],
              family="binomial")
summary(es.fit)
es.pred <- predict(es.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "signup_app_moweb")
nl.fit <- glm(destination_nl ~ ., 
              data=j.train.age[,c("destination_nl", vars)],
              family="binomial")
summary(nl.fit)
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

vars <- c("is_male", "affiliate_channel_content")
au.fit <- glm(destination_au ~ ., 
              data=j.train[,c("destination_au", vars)],
              family="binomial")
summary(au.fit)
au.pred <- predict(au.fit, j.test[,vars], type="response")

vars <- c("affiliate_channel_content", "numeric_date_first_booking")
ca.fit <- glm(destination_ca ~ ., 
              data=j.train[,c("destination_ca", vars)],
              family="binomial")
summary(ca.fit)
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "num_action_detail")
pt.fit <- glm(destination_pt ~ ., 
              data=j.train.age[,c("destination_pt", vars)],
              family="binomial") 
summary(pt.fit)
pt.pred <- predict(pt.fit, j.test[,vars], type="response")

country.probs <- data.frame(US=us.pred,
                            other=other.pred,
                            FR=fr.pred,
                            IT=it.pred,
                            GB=gb.pred,
                            ES=es.pred,
                            NL=nl.pred,
                            AU=au.pred,
                            DE=de.pred,
                            CA=ca.pred,
                            PT=pt.pred)

head(country.probs)

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

# predictions for test.joined set
j.submish.age.na <- data.frame(id=j.ids, country=countries)

################################################################################
# run logistic on google test set where age is not NA

google.age.indices <- which(!(is.na(test.joined.google$age)))
test.joined.google.age <- test.joined.google[google.age.indices,]


j.train <- train.joined
j.train.age <- train.joined.age
j.test <- test.joined.google.age

vars <- c("num_sessions", "num_actions", "num_action_detail", 
          "is_signup_app_web")
us.fit <- glm(destination_us ~ ., 
              data=j.train[,c("destination_us", vars)],
              family="binomial")
summary(us.fit)
us.pred <- predict(us.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "is_male")
other.fit <- glm(destination_other ~ ., 
                 data=j.train[,c("destination_other", vars)],
                 family="binomial")
summary(other.fit)
other.pred <- predict(other.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "num_actions", "numeric_date_first_booking", 
          "first_device_iphone")
fr.fit <- glm(destination_fr ~ ., 
              data=j.train[,c("destination_fr", vars)],
              family="binomial")
summary(fr.fit)
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

vars <- c("age", "first_device_iphone")
it.fit <- glm(destination_it ~ ., 
              data=j.train.age[,c("destination_it", vars)],
              family="binomial")
summary(it.fit)
it.pred <- predict(it.fit, j.test[,vars], type="response")

vars <- c("is_signup_app_web", "is_female")
gb.fit <- glm(destination_gb ~ ., 
              data=j.train[,c("destination_gb", vars)],
              family="binomial")
summary(gb.fit)
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

vars <- c("is_male", "num_actions")
de.fit <- glm(destination_de ~ ., 
              data=j.train[,c("destination_de", vars)],
              family="binomial")
summary(de.fit)
de.pred <- predict(de.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "affiliate_channel_content")
es.fit <- glm(destination_es ~ ., 
              data=j.train[,c("destination_es", vars)],
              family="binomial")
summary(es.fit)
es.pred <- predict(es.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "age")
nl.fit <- glm(destination_nl ~ ., 
              data=j.train.age[,c("destination_nl", vars)],
              family="binomial")
summary(nl.fit)
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

vars <- c("is_male", "affiliate_channel_content")
au.fit <- glm(destination_au ~ ., 
              data=j.train[,c("destination_au", vars)],
              family="binomial")
summary(au.fit)
au.pred <- predict(au.fit, j.test[,vars], type="response")

vars <- c("affiliate_channel_content", "numeric_date_first_booking")
ca.fit <- glm(destination_ca ~ ., 
              data=j.train[,c("destination_ca", vars)],
              family="binomial")
summary(ca.fit)
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

vars <- c("age", "num_sessions")
pt.fit <- glm(destination_pt ~ ., 
              data=j.train.age[,c("destination_pt", vars)],
              family="binomial") 
summary(pt.fit)
pt.pred <- predict(pt.fit, j.test[,vars], type="response")


country.probs <- data.frame(US=us.pred,
                            other=other.pred,
                            FR=fr.pred,
                            IT=it.pred,
                            GB=gb.pred,
                            ES=es.pred,
                            NL=nl.pred,
                            AU=au.pred,
                            DE=de.pred,
                            CA=ca.pred,
                            PT=pt.pred)

head(country.probs)

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

# predictions for test.joined set
j.submish.google.age <- data.frame(id=j.ids, country=countries)

################################################################################
# run logistic on google test set where age is NA


j.train <- train.joined
j.test <- test.joined.google[-google.age.indices,]

vars <- c("num_sessions", "num_actions", "num_action_detail", 
          "is_signup_app_web")
us.fit <- glm(destination_us ~ ., 
              data=j.train[,c("destination_us", vars)],
              family="binomial")
summary(us.fit)
us.pred <- predict(us.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "is_male")
other.fit <- glm(destination_other ~ ., 
                 data=j.train[,c("destination_other", vars)],
                 family="binomial")
summary(other.fit)
other.pred <- predict(other.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "num_actions", "numeric_date_first_booking", 
          "first_device_iphone")
fr.fit <- glm(destination_fr ~ ., 
              data=j.train[,c("destination_fr", vars)],
              family="binomial")
summary(fr.fit)
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

vars <- c("first_device_iphone", "is_female")
it.fit <- glm(destination_it ~ ., 
              data=j.train.age[,c("destination_it", vars)],
              family="binomial")
summary(it.fit)
it.pred <- predict(it.fit, j.test[,vars], type="response")

vars <- c("is_signup_app_web", "is_female")
gb.fit <- glm(destination_gb ~ ., 
              data=j.train[,c("destination_gb", vars)],
              family="binomial")
summary(gb.fit)
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

vars <- c("is_male", "num_actions")
de.fit <- glm(destination_de ~ ., 
              data=j.train[,c("destination_de", vars)],
              family="binomial")
summary(de.fit)
de.pred <- predict(de.fit, j.test[,vars], type="response")

vars <- c("num_actions", "num_action_detail", "affiliate_channel_content")
es.fit <- glm(destination_es ~ ., 
              data=j.train[,c("destination_es", vars)],
              family="binomial")
summary(es.fit)
es.pred <- predict(es.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "signup_app_moweb")
nl.fit <- glm(destination_nl ~ ., 
              data=j.train.age[,c("destination_nl", vars)],
              family="binomial")
summary(nl.fit)
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

vars <- c("is_male", "affiliate_channel_content")
au.fit <- glm(destination_au ~ ., 
              data=j.train[,c("destination_au", vars)],
              family="binomial")
summary(au.fit)
au.pred <- predict(au.fit, j.test[,vars], type="response")

vars <- c("affiliate_channel_content", "numeric_date_first_booking")
ca.fit <- glm(destination_ca ~ ., 
              data=j.train[,c("destination_ca", vars)],
              family="binomial")
summary(ca.fit)
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

vars <- c("num_sessions", "num_action_detail")
pt.fit <- glm(destination_pt ~ ., 
              data=j.train.age[,c("destination_pt", vars)],
              family="binomial") 
summary(pt.fit)
pt.pred <- predict(pt.fit, j.test[,vars], type="response")

country.probs <- data.frame(US=us.pred,
                            other=other.pred,
                            FR=fr.pred,
                            IT=it.pred,
                            GB=gb.pred,
                            ES=es.pred,
                            NL=nl.pred,
                            AU=au.pred,
                            DE=de.pred,
                            CA=ca.pred,
                            PT=pt.pred)

head(country.probs)

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

# predictions for test.joined set
j.submish.google.age.na <- data.frame(id=j.ids, country=countries)

################################################################################
# predictions for all other ids

j.test <- test[which(test$id %in% test.else.ids),]

vars <- c("is_signup_app_web", "affiliate_channel")
us.fit <- glm(destination_us ~ ., 
              data=train[,c("destination_us", vars)],
              family="binomial")
summary(us.fit)
us.pred <- predict(us.fit, j.test[,vars], type="response")

vars <- c("is_male", "numeric_date_first_booking", "affiliate_channel",
          "signup_app")
other.fit <- glm(destination_other ~ ., 
                 data=train[,c("destination_other", vars)],
                 family="binomial")
summary(other.fit)
other.pred <- predict(other.fit, j.test[,vars], type="response")

vars <- c("numeric_date_first_booking", "signup_flow", 
          "timestamp_first_active", "gender", "affiliate_channel",
          "signup_app")
fr.fit <- glm(destination_fr ~ ., 
              data=train[,c("destination_fr", vars)],
              family="binomial")
summary(fr.fit)
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

vars <- c("is_female", "is_signup_app_web", "signup_flow",
          "timestamp_first_active")
it.fit <- glm(destination_it ~ ., 
              data=train[,c("destination_it", vars)],
              family="binomial")
summary(it.fit)
it.pred <- predict(it.fit, j.test[,vars], type="response")

vars <- c("is_signup_app_web", "is_female")
gb.fit <- glm(destination_gb ~ ., 
              data=train[,c("destination_gb", vars)],
              family="binomial")
summary(gb.fit)
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

vars <- c("is_male", "is_signup_app_web", "numeric_date_first_booking")
de.fit <- glm(destination_de ~ ., 
              data=train[,c("destination_de", vars)],
              family="binomial")
summary(de.fit)
de.pred <- predict(de.fit, j.test[,vars], type="response")

vars <- c("is_signup_app_web", "affiliate_channel_content",
          "numeric_date_first_booking")
es.fit <- glm(destination_es ~ ., 
              data=train[,c("destination_es", vars)],
              family="binomial")
summary(es.fit)
es.pred <- predict(es.fit, j.test[,vars], type="response")

vars <- c("is_male", "numeric_date_account_created", "first_device_iphone")
nl.fit <- glm(destination_nl ~ ., 
              data=train[,c("destination_nl", vars)],
              family="binomial")
summary(nl.fit)
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

vars <- c("affiliate_channel_content",
          "numeric_date_account_created", "timestamp_first_active")
au.fit <- glm(destination_au ~ ., 
              data=train[,c("destination_au", vars)],
              family="binomial")
summary(au.fit)
au.pred <- predict(au.fit, j.test[,vars], type="response")

vars <- c("numeric_date_first_booking",
          "first_device_iphone")
ca.fit <- glm(destination_ca ~ ., 
              data=train[,c("destination_ca", vars)],
              family="binomial")
summary(ca.fit)
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

vars <- c("affiliate_channel_sembrand")
pt.fit <- glm(destination_pt ~ ., 
              data=train[,c("destination_pt", vars)],
              family="binomial") 
summary(pt.fit)
pt.pred <- predict(pt.fit, data.frame(affiliate_channel_sembrand=j.test[,vars]), type="response")
# pt.pred <- rep(0.002205668, length(ca.pred))

country.probs <- data.frame(US=us.pred,
                            other=other.pred,
                            FR=fr.pred,
                            IT=it.pred,
                            GB=gb.pred,
                            ES=es.pred,
                            NL=nl.pred,
                            AU=au.pred,
                            DE=de.pred,
                            CA=ca.pred,
                            PT=pt.pred)

head(country.probs)

get_five_countries <- function(i) {
  return(names(country.probs[i,][order(country.probs[i,], decreasing=T)][,1:5]))
}

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

# predictions for test.joined set
j.submish.else <- data.frame(id=j.ids, country=countries)

################################################################################
# predictions for test.ndf
ndf.submish <- data.frame(id=test.ndf.ids, country=rep('NDF', length(test.ndf.ids)))

################################################################################
# final submish
final.submish <- rbind(j.submish.age, 
                       j.submish.age.na,
                       j.submish.google.age,
                       j.submish.google.age.na,
                       ndf.submish, 
                       j.submish.else)

nrow(j.submish.age) / 5            # 12870
nrow(j.submish.age.na) / 5         # 3488
nrow(j.submish.google.age) / 5     # 84
nrow(j.submish.google.age.na) / 5  # 16
nrow(ndf.submish)                  # 27151   
nrow(else.submish) / 5             # 64

final.submish <- rbind(j.submish,
                       ndf.submish, 
                       else.submish)

write.csv(final.submish, "submish_29.csv", row.names=F)




