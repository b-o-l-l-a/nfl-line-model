library(ROCR)

setwd("C:/Users/greg.bolla/Desktop/Personal/Football Spreads/new_player_stats")
full.df <- read.csv("final_df.csv")
cat("================================", "\n", "Num Rows before eliminating pushes","\n","================================")
nrow(full.df)

full.df <- na.omit(full.df)
cat("================================", "\n", "Num Rows after eliminating pushes","\n","================================")
nrow(full.df)

set.seed(2017)
full.df[,"cover."] <- ifelse(full.df[,"cover."] == TRUE, 1, 0)
train <- sample(1:nrow(full.df), (nrow(full.df) * .8))

position.groups <- c("C", "G", "T", "RB", "TE", "WR", "QB", "DT", "DE", "LB", "DB")
time.horizons <- c("career", "prevyear", "twoyear", "threeyr", "prev2wks", "prev4wks", "prev6wks")

##### Binning Variables ####
# favorite
favorite.cover.first.tercile <- c("Green Bay Packers", "New England Patriots", "Seattle Seahawks")
favorite.no.cover.first.tercile <- c("Miami Dolphins", "Cleveland Browns", "Oakland Raiders")

favorite.cover.second.tercile <- c("San Francisco 49ers", "Minnesota Vikings", "Indianapolis Colts")  
favorite.no.cover.second.tercile <- c("Jacksonville Jaguars", "Los Angeles Rams", "Detroit Lions", "Tampa Bay Buccaneers", "Tennessee Titans", "Los Angeles Chargers", "Washington Redskins", "Chicago Bears", "Baltimore Ravens")
full.df[,"favorite_tercile"] <- ifelse(
  full.df[,"favorite"] %in% favorite.cover.first.tercile, "cover - first tercile",
  ifelse(full.df[,"favorite"] %in% favorite.no.cover.first.tercile, "no cover - first tercile",
         ifelse(full.df[,"favorite"] %in% favorite.cover.second.tercile, "cover - second tercile",
                ifelse(full.df[,"favorite"] %in% favorite.no.cover.second.tercile, "no cover - second tercile", "other")
         )
  )
)
full.df[,"favorite_tercile"] <- factor(full.df[,"favorite_tercile"])
full.df[,"favorite_tercile"] <- relevel(full.df[,"favorite_tercile"], "other")

# underdog
underdog.cover.first.tercile <- c("New England Patriots", "Pittsburgh Steelers", "Indianapolis Colts", "Cincinnati Bengals")
underdog.no.cover.first.tercile <- c("Buffalo Bills", "Tennessee Titans", "New York Giants", "Los Angeles Rams")

underdog.cover.second.tercile <- c("Atlanta Falcons", "Dallas Cowboys", "Kansas City Chiefs", "New Orleans Saints", "Washington Redskins", "Minnesota Vikings", "Green Bay Packers", "Baltimore Ravens", "Miami Dolphins", "New York Jets")
underdog.no.cover.second.tercile <- c("Chicago Bears", "Cleveland Browns", "Philadelphia Eagles", "Denver Broncos", "Houston Texans", "San Francisco 49ers")

full.df[,"underdog_tercile"] <- ifelse(
  full.df[,"underdog"] %in% underdog.cover.first.tercile, "cover - first tercile",
  ifelse(full.df[,"underdog"] %in% underdog.no.cover.first.tercile, "no cover - first tercile",
         ifelse(full.df[,"underdog"] %in% underdog.cover.second.tercile, "cover - second tercile",
                ifelse(full.df[,"underdog"] %in% underdog.no.cover.second.tercile, "no cover - second tercile", "other")
         )
  )
)
full.df[,"underdog_tercile"] <- factor(full.df[,"underdog_tercile"])
full.df[,"underdog_tercile"] <- relevel(full.df[,"underdog_tercile"], "other")        

# home
home.cover.first.tercile <- c()
home.no.cover.first.tercile <- c("Miami Dolphins", "Indianapolis Colts", "Kansas City Chiefs")

home.cover.second.tercile <- c("New England Patriots", "Minnesota Vikings", "Tampa Bay Buccaneers", "Los Angeles Chargers", "Los Angeles Rams", "Philadelphia Eagles", "Seattle Seahawks", "Oakland Raiders", "Tennessee Titans", "Green Bay Packers")
home.no.cover.second.tercile <- c("New York Jets", "Cincinnati Bengals", "Washington Redskins", "Atlanta Falcons", "Arizona Cardinals", "Cleveland Browns")

full.df[,"home_tercile"] <- ifelse(
  full.df[,"home"] %in% home.cover.first.tercile, "cover - first tercile",
  ifelse(full.df[,"home"] %in% home.no.cover.first.tercile, "no cover - first tercile",
         ifelse(full.df[,"home"] %in% home.cover.second.tercile, "cover - second tercile",
                ifelse(full.df[,"home"] %in% home.no.cover.second.tercile, "no cover - second tercile", "other")
         )
  )
)
full.df[,"home_tercile"] <- factor(full.df[,"home_tercile"])
full.df[,"home_tercile"] <- relevel(full.df[,"home_tercile"], "other")   

# away
away.cover.first.tercile <- c("Buffalo Bills", "San Francisco 49ers", "New York Giants", "Green Bay Packers")
away.no.cover.first.tercile <- c("Los Angeles Chargers", "Tampa Bay Buccaneers", "Oakland Raiders", "Pittsburgh Steelers")

away.cover.second.tercile <- c("Chicago Bears", "Seattle Seahawks", "Indianapolis Colts", "Denver Broncos", "Arizona Cardinals", "Houston Texans", "Cleveland Browns")
away.no.cover.second.tercile <- c("Minnesota Vikings", "Detroit Lions", "Baltimore Ravens")

full.df[,"away_tercile"] <- ifelse(
  full.df[,"away"] %in% away.cover.first.tercile, "cover - first tercile",
  ifelse(full.df[,"away"] %in% away.no.cover.first.tercile, "no cover - first tercile",
         ifelse(full.df[,"away"] %in% away.cover.second.tercile, "cover - second tercile",
                ifelse(full.df[,"away"] %in% away.no.cover.second.tercile, "no cover - second tercile", "other")
         )
  )
)
full.df[,"away_tercile"] <- factor(full.df[,"away_tercile"])
full.df[,"away_tercile"] <- relevel(full.df[,"away_tercile"], "other")   

# season
early.season <- c("1", "2", "3", "4", "5", "6")
mid.season <- c("7", "8", "9", "10", "11", "12")
late.season <- c("13", "14", "15", "16", "17")

full.df[,"part.of.season"] <- ifelse(
  full.df[,"week"] %in% early.season, "early",
  ifelse(full.df[,"week"] %in% mid.season, "mid", "late")  
)

full.df[,"part.of.season"] <- factor(full.df[,"part.of.season"])
full.df[,"part.of.season"] <- relevel(full.df[,"part.of.season"], "mid") 

# OL games / gs
ol.positions <- c("C", "G", "T")
for (p in 1:length(ol.positions)) {
  ol.pos <- ol.positions[p]
  gs.column.suffix <- paste(ol.pos, "_avg_career_gs", sep="")
  fave.col <- paste("fave_", gs.column.suffix, sep="")
  dog.col <- paste("dog_", gs.column.suffix, sep="")
  full.vec <- c(full.df[[fave.col]], full.df[[dog.col]])
  
  first.quartile <- summary(full.vec)["1st Qu."]
  median <- summary(full.vec)["Median"]
  third.quartile <- summary(full.vec)["3rd Qu."]
  
  full.df[,paste(dog.col, "_two_buckets", sep="")] <- ifelse(
    full.df[,dog.col] < median, "below_avg", "above_avg")
  full.df[,paste(fave.col, "_two_buckets",  sep="")] <- ifelse(
    full.df[,fave.col] < median, "below_avg", "above_avg")
  
  full.df[,paste(dog.col, "_four_buckets", sep="")] <- ifelse(
    full.df[,dog.col] < first.quartile, "quartile 1",
      ifelse(full.df[,dog.col] < median, "quartile 2",
        ifelse(full.df[,dog.col] < third.quartile, "quartile 3", 
               "quartile 4")
      )
    )
  
  full.df[,paste(fave.col, "_four_buckets", sep="")] <- ifelse(
    full.df[,fave.col] < first.quartile, "quartile 1",
    ifelse(full.df[,fave.col] < median, "quartile 2",
           ifelse(full.df[,fave.col] < third.quartile, "quartile 3", 
                  "quartile 4")
    )
  )
}

# Fave LB
full.df[,"fave_LB_avg_prev6wks_games_binary"] <- ifelse(
  full.df[,"fave_LB_avg_prev6wks_games"] == 6, TRUE, FALSE 
)

# Dog WR recs
wr.recs.full.vec <- c(full.df$dog_WR_avg_threeyr_rec, full.df$fave_WR_avg_threeyr_rec)
wr.recs.median <- median(wr.recs.full.vec)
full.df[,"dog_WR_avg_threeyr_rec_binary"] <- ifelse(
  full.df[,"dog_WR_avg_threeyr_rec"] < wr.recs.median, "below median", "above median"
)

# Dog RB recs
rb.recs.full.vec <- c(full.df$fave_RB_avg_prev6wks_rec, full.df$dog_RB_avg_prev6wks_rec)
rb.recs.mean <- mean(rb.recs.full.vec)
full.df[,"dog_RB_avg_prev6wks_rec_binary"] <- ifelse(
  full.df[,"dog_RB_avg_prev6wks_rec"] < rb.recs.mean, "below mean", "above mean"
)

# Fave 3rd down conversions
third.down.conv.full.vec <- c(full.df$dog_avg_4wks_def_third_down_conv_allowed, full.df$fave_avg_4wks_def_third_down_conv_allowed)
third.down.conv.median <- median(third.down.conv.full.vec)
full.df[,"dog_avg_4wks_def_third_down_conv_allowed_binary"] <- ifelse(
  full.df[,"dog_avg_4wks_def_third_down_conv_allowed"] < third.down.conv.median, "below median", "above median"
)

# Fave QB All-Pro
full.df[,"fave_QB_avg_threeyr_all_pros_binary"] <- ifelse(
  full.df[,"fave_QB_avg_threeyr_all_pros"] > 0, TRUE, FALSE
)
#for (p in 1:length(position.groups)) {
#  for (t in 1:length(time.horizons)) {
#    position <- position.groups[p]
#    time.horizon <- time.horizons[t]
#    games.column.suffix <- paste(position, "_avg_", time.horizon, "_games", sep="")
#    fave.col <- paste("fave_", games.column.suffix, sep="")
#    dog.col <- paste("dog_", games.column.suffix, sep="")
#  }
#}

#dog.vec <- as.vector(full.df["dog_G_avg_career_gs"])
#fave.vec <- as.vector(full.df["fave_G_avg_career_gs"]) 

#full.vec <- c(full.df$dog_G_avg_career_gs, full.df$fave_G_avg_career_gs)
#first.quartile <- summary(full.vec)["1st Qu."]
#median <- summary(full.vec)["Median"]
#third.quartile <- summary(full.vec)["3rd Qu."]

#full.df[,"dog_G_avg_career_gs_bucketed"] <- ifelse(
#  full.df[,"dog_G_avg_career_gs"] < first.quartile, "below_avg",
#  ifelse(full.df[,"dog_G_avg_career_gs"] > third.quartile, "above_avg", "avg")
#)

##### Plot / EDA #####
columns.to.explore <- c ("fave_G_avg_career_gs", 
                         "fave_G_avg_career_games", 
                         "fave_G_avg_yrs_career",
                         "dog_WR_avg_threeyr_games", 
                         "dog_WR_avg_twoyear_games")
##### Prep train/test #####
test.df <- full.df[-train,]
train.df <- full.df[train,]
response.col <- "cover."

# Need to exclude any variables in the data set that are unknown prior to the game
# or are irrelevant to prediction
exclusion.patterns <- c(
  "detail_link",
  "over_under",
  "cover.",
  "margin",
  "over.",
  "winner",
  "pts_winner",
  "loser",
  "pts_loser",
  "game_date",
  "gametime",
  "winner_home",
  "pts_favorite",
  "pts_underdog"
)

# Cast columns to appropriate data type
#####
addtl.categorical.vars <- c("year", "week")
addtl.boolean.vars <- c("over.", "cover.")

for(i in 1:length(addtl.categorical.vars)) {
  var <- addtl.categorical.vars[i]
  full.df[,var] <- as.factor(full.df[,var])
}

for(i in 1:length(addtl.boolean.vars)) {
  var <- addtl.boolean.vars[i]
  full.df[,var] <- as.logical(full.df[,var])
}
#####
# get list of all potential predictors
##### 
continuous.predictors <- setdiff(names(full.df[, sapply(full.df, is.numeric)]), exclusion.patterns)
categorical.predictors <- setdiff(names(full.df[, !sapply(full.df, is.numeric)]), exclusion.patterns)

null.model <- glm(formula=as.formula(paste(response.col, "1", sep="~")), family = binomial(link = "logit"), 
                  data = train.df)

#### 
# For continuous variables, the best univariable analysis involves fitting a univariate logistic
#  regression model to obtain: 
#    1) the estimated coefficient, 
#    2) the estimated standard error, the 
#    3) the likelihood ratio test compared to a null model
#    4) the univariable Wald statistic (aka z-test)
#    5) two-sample t-test between the two groups of the response variable
#    6) odds ratio of the coefficient (i.e., how much does the likelihood of the response increase with a one unit increase of the predictor)

continuous.predictor.df <- data.frame(predictor                = character(),
                                      coef = numeric(),
                                      std.err = numeric(),
                                      wald = numeric(),
                                      odds = numeric(),
                                      p.val                    = numeric()
)

for (i in 1:length(continuous.predictors)) {
  predictor <- continuous.predictors[i]
  print(paste("calculating for: ", predictor))
  
  single.log.formula <- as.formula(paste(response.col, predictor, sep = "~"))
  single.log.reg <- glm(formula = single.log.formula, family = binomial(link = "logit"), 
                        data = train.df)
  
  p.val <- coef(summary(single.log.reg))[predictor, "Pr(>|z|)"]
  coef  <- coef(summary(single.log.reg))[predictor, "Estimate"]
  std.err  <- coef(summary(single.log.reg))[predictor, "Std. Error"]
  wald <- coef(summary(single.log.reg))[predictor,"z value"]
  odds <- exp(coefficients(single.log.reg))[predictor]
  
  
  continuous.predictor.df <- rbind(continuous.predictor.df, 
                                   data.frame(
                                     predictor                = predictor,
                                     coef = coef,
                                     std.err = std.err,
                                     wald = wald,
                                     odds = odds,
                                     p.val                    = p.val
                                   )
  )          
}
write.csv(continuous.predictor.df, "single_logistic_regressors_continuous.csv")


#####
# EDA on categorical variables
##### 
# will use this to make odds ratio easier to interpret
reference.constant <- sum(train.df[,"cover."]) / nrow(train.df)
#reference.constant <- sum(as.numeric(train.df[,"cover."]) -1 ) / nrow(train.df)

categorical.predictor.df <- data.frame(
  predictor.val = character(),
  coef      = numeric(),
  std.err   = numeric(),
  wald      = numeric(),
  p.val     = numeric(),
  odds.ratio    = numeric(),
  reference.value = character(),
  reference.perc = character(),
  predictor = character()
)
for (i in 1:length(categorical.predictors)) {
  predictor <- categorical.predictors[i]
  train.df[,predictor] <- factor(train.df[,predictor])
  contingency.table <- table(train.df[,predictor], train.df[,response.col])
  contingency.table.mat <- as.data.frame.matrix(contingency.table)
  contingency.table.mat[,"perc"] <- contingency.table.mat[,"1"] / (contingency.table.mat[,"1"] + contingency.table.mat[,"0"])
  #contingency.table.mat[,"perc"] <- contingency.table.mat[,"TRUE"] / (contingency.table.mat[,"TRUE"] + contingency.table.mat[,"FALSE"])
  reference.row <- which(abs(contingency.table.mat[,"perc"]-reference.constant)==min(abs(contingency.table.mat[,"perc"]-reference.constant)))
  reference.col <- rownames(contingency.table.mat)[reference.row]
  reference.perc <- contingency.table.mat[reference.col, "perc"]
  
  train.df[,predictor] <- relevel(train.df[,predictor], reference.col)
  single.log.formula <- as.formula(paste(response.col, predictor, sep = "~"))
  single.log.reg <- glm(formula = single.log.formula, family = binomial(link = "logit"), 
                        data = train.df)
  
  coef.df <- as.data.frame(coef(summary(single.log.reg)))
  odds.ratio <- exp(coefficients(single.log.reg))
  odds.df <- as.data.frame(odds.ratio)
  merged.df <- merge(coef.df, odds.df, by=0)
  merged.df[1, "Row.names"] <- paste(predictor, "intercept")
  colnames(merged.df) <- c("predictor.val", "coef", "std.err", "wald", "p.val", "odds.ratio")
  
  merged.df[,"reference.value"] <- reference.col
  merged.df[,"reference.perc"] <- reference.perc
  merged.df[,"predictor"] <- predictor
  categorical.predictor.df <- rbind(categorical.predictor.df, merged.df)
}
write.csv(categorical.predictor.df, "single_logistic_regressors_categorical.csv")
