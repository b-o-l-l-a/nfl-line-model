

#####


#### Logistic Regression based on single regression models
logistic.predictor.df <- read.csv("single_logistic_regressors_continuous.csv")
logistic.predictor.list <- logistic.predictor.df[logistic.predictor.df["p.val"] < .10, "predictor"]
logistic.predictor.list <- setdiff(logistic.predictor.list, exclusion.patterns)
logistic.predictor.list <- logistic.predictor.list[!logistic.predictor.list %in% setdiff(logistic.predictor.list, names(train.df))]
logistic.predictor.list <- c(logistic.predictor.list, "line", "home_favorite","over_under", "favorite_tercile", "underdog_tercile", "home_tercile", "away_tercile", "part.of.season")
logisitic.lhs <- paste(response.col, "~")
logistic.rhs <- paste(logistic.predictor.list, collapse="+")
logistic.full.formula <- as.formula(paste(logisitic.lhs, logistic.rhs))
logistic.full.model <- glm(formula = logistic.full.formula, family = binomial(link = "logit"), 
                           data = train.df)
coef.df <- as.data.frame(coef(summary(logistic.full.model)))
odds.ratio <- exp(coefficients(logistic.full.model))
odds.df <- as.data.frame(odds.ratio)
logistic.full.model.df <- merge(coef.df, odds.df, by=0)
write.csv(logistic.full.model.df, "full_logistic_model.csv")

reduced.model.df <- read.csv("full_logistic_model.csv")
reduced.model.predictors <- reduced.model.df[reduced.model.df["Pr...z.."] < .05, "Row.names"]
addtl.predictors <- setdiff(reduced.model.predictors, names(train.df))
reduced.model.predictors <- as.character(reduced.model.predictors[!reduced.model.predictors %in% addtl.predictors])
reduced.model.predictors <- c(reduced.model.predictors, c("away_tercile", "home_tercile", "favorite_tercile"))
reduced.model.predictors <- as.character(reduced.model.predictors[!reduced.model.predictors %in% c("dog_QB_avg_threeyr_all_pros", "fave_DT_avg_prev4wks_sacks", "fave_QB_avg_prev2wks_pass_comp")])
logistic.reduced.formula <- paste(paste(response.col, "~"), paste(reduced.model.predictors, collapse="+"))
logistic.reduced.model <- glm(formula = logistic.reduced.formula, family = binomial(link = "logit"), 
                              data = train.df)
coef.df <- as.data.frame(coef(summary(logistic.reduced.model)))
odds.ratio <- exp(coefficients(logistic.reduced.model))
odds.df <- as.data.frame(odds.ratio)
logistic.reduced.model.df <- merge(coef.df, odds.df, by=0)
write.csv(logistic.reduced.model.df, "reduced_logistic_model.csv")

logistic.stepwise.model <- step(logistic.reduced.model, direction = "both")
#rf.pred.train.prob <- predict(class.rand.forest, train.df[, (names(train.df) %in% logistic.predictors)], type = "prob")

logistic.stepwise.train.pred <- predict(logistic.stepwise.model, train.df[, !(names(train.df) %in% exclusion.patterns)], type = "response")
logistic.stepwise.test.pred <- predict(logistic.stepwise.model, newdata = test.df[, !(names(test.df) %in% exclusion.patterns)], type = "response")

train.df["logistic.stepwise.pred"] <-  logistic.stepwise.train.pred
test.df["logistic.stepwise.pred"] <- logistic.stepwise.test.pred

##### Bucketed variables #####
predictors <- c(
  "fave_G_avg_career_gs_two_buckets",
  "underdog_tercile",
  "favorite_tercile",
  "away_tercile",
  "home_tercile",
  "fave_QB_avg_threeyr_all_pros_binary",
  "dog_WR_avg_threeyr_rec_binary",
  "dog_RB_avg_prev6wks_rec_binary",
  "fave_QB_prev4wks_pass_int_perc",
  "dog_QB_avg_prev2wks_pass_yds",
  "dog_QB_avg_prev4wks_pass_int"
)

logisitic.lhs <- paste(response.col, "~")
logistic.rhs <- paste(predictors, collapse="+")
logistic.formula <- as.formula(paste(logisitic.lhs, logistic.rhs))
logistic.model <- glm(formula = logistic.formula, family = binomial(link = "logit"), 
                           data = train.df)


