dcg <- function(five_guesses, actual) {
  if (any(five_guesses %in% actual)) {
    i <- which(five_guesses %in% actual)
    return(1 / (log(i + 1, base=2)))
  } else {
    return(0)
  }
}

calculate_dcg <- function(id, submish.df, actual) {
  return(dcg(as.vector(submish.df[which(submish.df$id == id), "country"]), actual))
}

submish.sub <- submish[1:10,]
actual <- data.frame(id=unique(submish.sub$id), country=c('FR', 'US'))
calculate_ndcg <- function(submish, actual) {
  ids <- actual[, "id"]
  actual.countries <- actual[,"country"]
  n <- length(ids)
  return(sum(sapply(1:n, function(i) calculate_dcg(ids[i], submish, actual.countries[i]))) / n)
}


################################################################################
# maximize ndcg

train <- read.csv("train_users.csv", h=T)
ids <- train$id

# 3 subsets: those with date_first_booking = "", those in the joined train set,
# and those not in the joined set.

train.ndf <- train[which(train$date_first_booking == ""), c("id", "country_destination")]
nrow(train.ndf) # 99152


train.joined <- read.csv("train_joined2.csv", h=T)
train.joined <- train.joined[which(train.joined$date_first_booking != ""),]
train.joined.ids <- train.joined$id
length(train.joined.ids) # 12213


train.else <- setdiff(ids, train.ndf$id)
train.else <- setdiff(train.else, train.joined.ids)
length(train.else) # 59874
train.else <- train[which(train$id %in% train.else),]

################################################################################
# train joined

train.joined$country_destination <- factor(train.joined$country_destination)

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

length(which(is.na(train.joined$timestmp_first_active)))
length(which(is.na(train.joined$language)))
length(which(is.na(train.joined$sum_seconds)))
length(which(is.na(train.joined$num_actions)))
length(which(is.na(train.joined$num_sessions)))
length(which(is.na(train.joined$num_action_detail)))

train.isna <- which(is.na(train.joined$sum_seconds))
train.joined <- train.joined[-train.isna,]

train.indices <- sample(nrow(train.joined), floor(.75*nrow(train.joined)))
j.train <- train.joined[train.indices,]
j.test <- train.joined[-train.indices,]
j.actual <- j.test[,c("id", "country_destination")]
names(j.actual) <- c("id", "country")

vars <- c("timestmp_first_active", "signup_method", 
          "signup_flow", "affiliate_channel",
          "affiliate_provider", "first_affiliate_tracked",
          "signup_app", "first_device_type", "sum_seconds", 
          "num_sessions", "num_actions", "num_action_detail")

vars <- c("sum_seconds", "num_sessions", "num_actions", "num_action_detail")

rf.fit <- randomForest(destination_us ~ ., 
                       data=j.train[,c("destination_us", vars)],
                       maxnodes=17, ntrees=1001)
rf.pred <- predict(rf.fit, j.test[,vars], type="response")

log.fit <- glm(destination_us ~ ., 
               data=j.train[,c("destination_us", vars)],
               family="binomial")
log.pred <- predict(log.fit, j.test[,vars], type="response")

svm.fit <- svm(destination_us ~ ., 
               data=j.train[,c("destination_us", vars)])
svm.pred <- predict(svm.fit, j.test[,vars], type="response")


# perform logistic regressions for each country
us.fit <- glm(destination_us ~ ., 
               data=j.train[,c("destination_us", vars)],
               family="binomial")
us.pred <- predict(us.fit, j.test[,vars], type="response")

other.fit <- glm(destination_other ~ ., 
              data=j.train[,c("destination_other", vars)],
              family="binomial")
other.pred <- predict(other.fit, j.test[,vars], type="response")

fr.fit <- glm(destination_fr ~ ., 
              data=j.train[,c("destination_fr", vars)],
              family="binomial")
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

it.fit <- glm(destination_it ~ ., 
              data=j.train[,c("destination_it", vars)],
              family="binomial")
it.pred <- predict(it.fit, j.test[,vars], type="response")

gb.fit <- glm(destination_gb ~ ., 
              data=j.train[,c("destination_gb", vars)],
              family="binomial")
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

de.fit <- glm(destination_de ~ ., 
              data=j.train[,c("destination_de", vars)],
              family="binomial")
de.pred <- predict(de.fit, j.test[,vars], type="response")

es.fit <- glm(destination_es ~ ., 
              data=j.train[,c("destination_es", vars)],
              family="binomial")
es.pred <- predict(es.fit, j.test[,vars], type="response")

nl.fit <- glm(destination_nl ~ ., 
              data=j.train[,c("destination_nl", vars)],
              family="binomial")
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

au.fit <- glm(destination_au ~ ., 
              data=j.train[,c("destination_au", vars)],
              family="binomial")
au.pred <- predict(au.fit, j.test[,vars], type="response")

ca.fit <- glm(destination_ca ~ ., 
              data=j.train[,c("destination_ca", vars)],
              family="binomial")
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

pt.fit <- glm(destination_pt ~ ., 
              data=j.train[,c("destination_pt", vars)],
              family="binomial")
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

get_five_countries <- function(i) {
  return(names(country.probs[i,][order(country.probs[i,], decreasing=T)][,1:5]))
}

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

submish <- data.frame(id=j.ids, country=countries)

calculate_ndcg(submish, j.actual)  # .839



# using rf
us.rf.fit <- randomForest(destination_us ~ ., 
                       data=j.train[,c(vars, "destination_us")], 
                       maxnodes=17,
                       ntrees=1001)
us.rf.probs <- predict(us.rf.fit, j.test[,vars], type="prob")

other.rf.fit <- randomForest(destination_other ~ ., 
                          data=j.train[,c(vars, "destination_other")], 
                          maxnodes=17,
                          ntrees=1001)
other.rf.probs <- predict(other.rf.fit, j.test[,vars], type="prob")

fr.rf.fit <- randomForest(destination_fr ~ ., 
                       data=j.train[,c(vars, "destination_fr")], 
                       maxnodes=17,
                       ntrees=1001)
fr.rf.probs <- predict(fr.rf.fit, j.test[,vars], type="prob")

it.rf.fit <- randomForest(destination_it ~ ., 
                       data=j.train[,c(vars, "destination_it")], 
                       maxnodes=17,
                       ntrees=1001)
it.rf.probs <- predict(it.rf.fit, j.test[,vars], type="prob")

gb.rf.fit <- randomForest(destination_gb ~ ., 
                       data=j.train[,c(vars, "destination_gb")], 
                       maxnodes=17,
                       ntrees=1001)
gb.rf.probs <- predict(gb.rf.fit, j.test[,vars], type="prob")

es.rf.fit <- randomForest(destination_es ~ ., 
                       data=j.train[,c(vars, "destination_es")], 
                       maxnodes=17,
                       ntrees=1001)
es.rf.probs <- predict(es.rf.fit, j.test[,vars], type="prob")

nl.rf.fit <- randomForest(destination_nl ~ ., 
                       data=j.train[,c(vars, "destination_nl")], 
                       maxnodes=17,
                       ntrees=1001)
nl.rf.probs <- predict(nl.rf.fit, j.test[,vars], type="prob")

au.rf.fit <- randomForest(destination_au ~ ., 
                       data=j.train[,c(vars, "destination_au")], 
                       maxnodes=17,
                       ntrees=1001)
au.rf.probs <- predict(au.rf.fit, j.test[,vars], type="prob")

de.rf.fit <- randomForest(destination_de ~ ., 
                       data=j.train[,c(vars, "destination_de")], 
                       maxnodes=17,
                       ntrees=1001)
de.rf.probs <- predict(de.rf.fit, j.test[,vars], type="prob")

ca.rf.fit <- randomForest(destination_ca ~ ., 
                       data=j.train[,c(vars, "destination_ca")], 
                       maxnodes=17,
                       ntrees=1001)
ca.rf.probs <- predict(ca.rf.fit, j.test[,vars], type="prob")

pt.rf.fit <- randomForest(destination_pt ~ ., 
                       data=j.train[,c(vars, "destination_pt")], 
                       maxnodes=17,
                       ntrees=1001)
pt.rf.probs <- predict(pt.rf.fit, j.test[,vars], type="prob")


country.probs <- data.frame(US=us.rf.probs[,"1"],
                            other=other.rf.probs[,"1"],
                            FR=fr.rf.probs[,"1"],
                            IT=it.rf.probs[,"1"],
                            GB=gb.rf.probs[,"1"],
                            ES=es.rf.probs[,"1"],
                            NL=nl.rf.probs[,"1"],
                            AU=au.rf.probs[,"1"],
                            DE=de.rf.probs[,"1"],
                            CA=ca.rf.probs[,"1"],
                            PT=pt.rf.probs[,"1"])

j.ids <- rep(j.test$id, each=5)
rf.countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

rf.submish <- data.frame(id=j.ids, country=rf.countries)

calculate_ndcg(rf.submish, j.actual)  # .8264


train.joined$country_destination <- factor(train.joined$country_destination)

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

length(which(is.na(train.joined$timestmp_first_active)))
length(which(is.na(train.joined$language)))
length(which(is.na(train.joined$sum_seconds)))
length(which(is.na(train.joined$num_actions)))
length(which(is.na(train.joined$num_sessions)))
length(which(is.na(train.joined$num_action_detail)))

train.isna <- which(is.na(train.joined$sum_seconds))
train.joined <- train.joined[-train.isna,]

train.indices <- sample(nrow(train.joined), floor(.75*nrow(train.joined)))
j.train <- train.joined[train.indices,]
j.test <- train.joined[-train.indices,]
j.actual <- j.test[,c("id", "country_destination")]
names(j.actual) <- c("id", "country")

vars <- c("timestmp_first_active", "signup_method", 
          "signup_flow", "affiliate_channel",
          "affiliate_provider", "first_affiliate_tracked",
          "signup_app", "first_device_type", "sum_seconds", 
          "num_sessions", "num_actions", "num_action_detail")

rf.fit <- randomForest(destination_us ~ ., 
                       data=j.train[,c("destination_us", vars)],
                       maxnodes=17, ntrees=1001)
rf.pred <- predict(rf.fit, j.test[,vars], type="response")

log.fit <- glm(destination_us ~ ., 
               data=j.train[,c("destination_us", vars)],
               family="binomial")
log.pred <- predict(log.fit, j.test[,vars], type="response")

svm.fit <- svm(destination_us ~ ., 
               data=j.train[,c("destination_us", vars)])
svm.pred <- predict(svm.fit, j.test[,vars], type="response")


# perform logistic regressions for each country
us.fit <- glm(destination_us ~ ., 
              data=j.train[,c("destination_us", vars)],
              family="binomial")
us.pred <- predict(us.fit, j.test[,vars], type="response")

other.fit <- glm(destination_other ~ ., 
                 data=j.train[,c("destination_other", vars)],
                 family="binomial")
other.pred <- predict(other.fit, j.test[,vars], type="response")

fr.fit <- glm(destination_fr ~ ., 
              data=j.train[,c("destination_fr", vars)],
              family="binomial")
fr.pred <- predict(fr.fit, j.test[,vars], type="response")

it.fit <- glm(destination_it ~ ., 
              data=j.train[,c("destination_it", vars)],
              family="binomial")
it.pred <- predict(it.fit, j.test[,vars], type="response")

gb.fit <- glm(destination_gb ~ ., 
              data=j.train[,c("destination_gb", vars)],
              family="binomial")
gb.pred <- predict(gb.fit, j.test[,vars], type="response")

de.fit <- glm(destination_de ~ ., 
              data=j.train[,c("destination_de", vars)],
              family="binomial")
de.pred <- predict(de.fit, j.test[,vars], type="response")

es.fit <- glm(destination_es ~ ., 
              data=j.train[,c("destination_es", vars)],
              family="binomial")
es.pred <- predict(es.fit, j.test[,vars], type="response")

nl.fit <- glm(destination_nl ~ ., 
              data=j.train[,c("destination_nl", vars)],
              family="binomial")
nl.pred <- predict(nl.fit, j.test[,vars], type="response")

au.fit <- glm(destination_au ~ ., 
              data=j.train[,c("destination_au", vars)],
              family="binomial")
au.pred <- predict(au.fit, j.test[,vars], type="response")

ca.fit <- glm(destination_ca ~ ., 
              data=j.train[,c("destination_ca", vars)],
              family="binomial")
ca.pred <- predict(ca.fit, j.test[,vars], type="response")

pt.fit <- glm(destination_pt ~ ., 
              data=j.train[,c("destination_pt", vars)],
              family="binomial")
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

get_five_countries <- function(i) {
  return(names(country.probs[i,][order(country.probs[i,], decreasing=T)][,1:5]))
}

j.ids <- rep(j.test$id, each=5)
countries <- as.vector(sapply(1:nrow(j.test), function(i) get_five_countries(i)))

submish <- data.frame(id=j.ids, country=countries)

calculate_ndcg(submish, j.actual) 

################################################################################
# train else

length(which(is.na(train.else$age))) # 14734
length(which(train.else$first_browser == '-unknown-')) # 4778
length(which(train.else$first_browser == '-unknown-'))











