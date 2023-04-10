library(clusterSim)
library(caret)
library(DALEX)
library(fastDummies)
library(gridExtra)
library(outliers)
library(psych)
library(stats)

outliers_grubbs <- function(dataset, alpha)
{
  for (i in 1:ncol(dataset))
  {
    if (is.numeric(dataset[,i])==TRUE)
    {
      while (grubbs.test(as.numeric(dataset[,i]))$p.value < alpha)
      {
        p1 <- grubbs.test(as.numeric(dataset[,i]))$alternative
        p2 <- readr::parse_number(p1)
        dataset <- dataset[-which(dataset[,i] %in% p2), ]
      }
    }
  }
  return(dataset)
}

seed = 307
set.seed(seed)

dataset <- read.csv('penguins.csv')
dataset$species <- as.factor(dataset$species)
dataset$island <- as.factor(dataset$island)
dataset$sex <- as.factor(dataset$sex)
str(dataset)

colnames(dataset) <- c("species", "island", "bill_length", "bill_depth", "flipper_length", "body_mass", "sex")

# _______________________________________________________________________
# -------------------------- Missing data -------------------------------

colSums(is.na(dataset))
is.na(dataset)

dataset_without_na <- dataset[complete.cases(dataset),]
dataset_with_na <- dataset[-as.numeric(row.names(dataset_without_na)),]

set.seed(seed)
model_knn_na <- train(sex ~ .,
                      data = dataset_without_na,
                      method = "knn",
                      preProcess = c('center', 'scale'),
                      tuneGrid = expand.grid(k = c(5, 7, 9, 11, 13, 15, 17)),
                      trControl = trainControl(method = "cv", 
                                               number = 10,
                                               classProbs = TRUE,
                                               savePredictions = TRUE))
results_knn_na <- predict(model_knn_na, dataset_with_na)
dataset_with_na$sex <- results_knn_na
dataset <- rbind(dataset_with_na, dataset_without_na)
colSums(is.na(dataset))

# _______________________________________________________________________
# -------------------------- Outliers -----------------------------------

outliers_grubbs <- function(dataset, alpha)
{
  for (i in 1:ncol(dataset))
  {
    if (is.numeric(dataset[,i])==TRUE)
    {
      while (grubbs.test(as.numeric(dataset[,i]))$p.value < alpha)
      {
        p1 <- grubbs.test(as.numeric(dataset[,i]))$alternative
        p2 <- readr::parse_number(p1)
        dataset <- dataset[-which(dataset[,i] %in% p2), ]
      }
    }
  }
  return(dataset)
}

dataset = outliers_grubbs(dataset, 0.05) # no outliers based on Grubbs test

df = dataset[,3:6]
df <- data.Normalization(df, type="n1")
df['ThreeSigma'] = rep(0, nrow(dataset))
for (i in 3:7){
  above3 <- as.integer(as.logical(df[,i] > 3))
  below3 <- as.integer(as.logical(df[,i] < -3))
  dataset['ThreeSigma'] = dataset['ThreeSigma'] + above3 + below3
}
sum(df['ThreeSigma']>0) # no outliers based on Three Sigma Rule of Thumb

# _______________________________________________________________________
# -------------------------- Stats --------------------------------------

# descriptive statistics
describe(dataset)

# balance of a dataset
table(dataset$species)

ggplot(dataset, aes(x=species, y=bill_length))+
  geom_boxplot(color="blue4") +
  theme_bw()
ggplot(dataset, aes(x=species, y=bill_depth))+
  geom_boxplot(color="blue4") +
  theme_bw()
ggplot(dataset, aes(x=species, y=flipper_length))+
  geom_boxplot(color="blue4") +
  theme_bw()
ggplot(dataset, aes(x=species, y=body_mass))+
  geom_boxplot(color="blue4") +
  theme_bw()

table(dataset$species, dataset$sex)
table(dataset$species, dataset$island)

# _______________________________________________________________________
# -------------------------- Train and test data ------------------------
# transforming categorical variables
dataset_1 <- dummy_cols(dataset, select_columns = c('island', 'sex'), remove_first_dummy = TRUE)
dataset_1 <- dataset_1[, !names(dataset_1) %in% c('island', 'sex')]

dataset_1$island_Dream <- as.factor(dataset_1$island_Dream)
dataset_1$island_Torgersen <- as.factor(dataset_1$island_Torgersen)
dataset_1$sex_male <- as.factor(dataset_1$sex_male)

index  <- sample(nrow(dataset_1), .75*nrow(dataset_1), replace = F)
train_set <- dataset_1[index,]
test_set <- dataset_1[-index,]

# _______________________________________________________________________
# -------------------------- KNN ----------------------------------------
set.seed(seed)
model_knn <- caret::train(species ~ .,
                          data = train_set,
                          method = "knn",
                          preProcess = c('center', 'scale'),
                          tuneGrid = expand.grid(k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)),
                          trControl = trainControl(method = "cv", 
                                                   number = 10,
                                                   classProbs = TRUE,
                                                   sampling = 'up',
                                                   savePredictions = TRUE))
model_knn$bestTune
confusionMatrix(data=predict(model_knn, test_set), reference = test_set$species, mode = "everything")

# _______________________________________________________________________
# -------------------------- SVM ----------------------------------------
set.seed(seed)
model_svm <- caret::train(species ~ .,
                          data = train_set,
                          method = "svmLinear",
                          preProcess = c('center', 'scale'),
                          tuneGrid = expand.grid(C = seq(0.1, 2, length=20)),
                          trControl = trainControl(method = "cv", 
                                                   number = 10,
                                                   classProbs = TRUE,
                                                   sampling = 'up',
                                                   savePredictions = TRUE))
confusionMatrix(data=predict(model_svm, test_set), reference = test_set$species, mode = "everything")

# _______________________________________________________________________
# -------------------------- RF -----------------------------------------
set.seed(seed)
model_tree <- caret::train(species ~ .,
                           data = train_set,
                           method = "rpart",
                           tuneGrid = expand.grid(cp = seq(0.005, 0.2, 0.005)),
                           trControl = trainControl(method = "cv", 
                                                    number = 10,
                                                    classProbs = TRUE,
                                                    sampling = 'up',
                                                    savePredictions = TRUE))
confusionMatrix(data=predict(model_tree, test_set), reference = test_set$species, mode = "everything")
model_tree$bestTune


#########################################################################
#########################################################################

# -----------------------------------------------------------------------
# ------------ PREDICTED VARIABLE: SEX ----------------------------------
# -----------------------------------------------------------------------

# _______________________________________________________________________
# -------------------------- Stats --------------------------------------

# balance of a dataset
table(dataset$sex)
table(dataset$sex, dataset$species)
table(dataset$sex, dataset$island)

ggplot(dataset, aes(x=sex, y=bill_length))+
  geom_boxplot(color="blue4") +
  theme_bw()
ggplot(dataset, aes(x=sex, y=bill_depth))+
  geom_boxplot(color="blue4") +
  theme_bw()
ggplot(dataset, aes(x=sex, y=flipper_length))+
  geom_boxplot(color="blue4") +
  theme_bw()
ggplot(dataset, aes(x=sex, y=body_mass))+
  geom_boxplot(color="blue4") +
  theme_bw()

dataset_2 <- dummy_cols(dataset, select_columns = c('island', 'species'), remove_first_dummy = TRUE)
dataset_2 <- dataset_2[, !names(dataset_2) %in% c('island', 'species')]
names(dataset_2)[names(dataset_2) == 'sex_male'] <- 'sex' # 1 - male

dataset_2$island_Dream <- as.factor(dataset_2$island_Dream)
dataset_2$island_Torgersen <- as.factor(dataset_2$island_Torgersen)
dataset_2$species_Chinstrap <- as.factor(dataset_2$species_Chinstrap)
dataset_2$species_Gentoo <- as.factor(dataset_2$species_Gentoo)

train_set_2 <- dataset_2[index,]
test_set_2 <- dataset_2[-index,]

# _______________________________________________________________________
# -------------------------- KNN 2 --------------------------------------

set.seed(seed)
model_knn_2 <- caret::train(sex ~ .,
                            data = train_set_2,
                            method = "knn",
                            preProcess = c('center', 'scale'),
                            tuneGrid = expand.grid(k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)),
                            trControl = trainControl(method = "cv", 
                                                     number = 10,
                                                     classProbs = TRUE,
                                                     sampling = 'up',
                                                     savePredictions = TRUE))
model_knn_2$bestTune
confusionMatrix(data=predict(model_knn_2, test_set_2), reference = test_set_2$sex, mode = "everything")

# _______________________________________________________________________
# -------------------------- SVM 2 --------------------------------------
set.seed(seed)
model_svm_2 <- caret::train(sex ~ .,
                            data = train_set_2,
                            method = "svmLinear",
                            preProcess = c('center', 'scale'),
                            tuneGrid = expand.grid(C = seq(0.1, 2, length=20)),
                            trControl = trainControl(method = "cv", 
                                                     number = 10,
                                                     classProbs = TRUE,
                                                     sampling = 'up',
                                                     savePredictions = TRUE))
confusionMatrix(data=predict(model_svm_2, test_set_2), reference = test_set_2$sex, mode = "everything")

# _______________________________________________________________________
# -------------------------- RF 2 -----------------------------------------
set.seed(seed)
model_tree_2 <- caret::train(sex ~ .,
                             data = train_set_2,
                             method = "rpart",
                             tuneGrid = expand.grid(cp = seq(0.005, 0.5, 0.005)),
                             trControl = trainControl(method = "cv", 
                                                      number = 10,
                                                      classProbs = TRUE,
                                                      sampling = 'up',
                                                      savePredictions = TRUE))
confusionMatrix(data=predict(model_tree_2, test_set_2), reference = test_set_2$sex, mode = "everything")
model_tree$bestTune

# _______________________________________________________________________
# -------------------------- GLM 2 --------------------------------------

train_set_3 <- dataset[index,]
test_set_3 <- dataset[-index,]

set.seed(seed)
model_glm_2 <- caret::train(sex ~ .,
                            data = train_set_3,
                            method = "glm",
                            trControl = trainControl(method = "cv", 
                                                     number = 10,
                                                     classProbs = TRUE,
                                                     sampling = 'up',
                                                     savePredictions = TRUE))
confusionMatrix(data=predict(model_glm_2, test_set_3), reference = test_set_3$sex, mode = "everything")

# #######################################################################
# _______________________________________________________________________
# -------------------------- PCP ----------------------------------------

explain_glm <- DALEX::explain(model = model_glm_2,
                              data = train_set_3[, !names(train_set_3) %in% c('sex')],
                              y = train_set_3$sex,
                              type = "classification",
                              label = "Logistic regression")

pcp <- predict_profile(explainer = explain_glm, new_observation = train_set_3[13,])
plot(pcp, variables = c("bill_length", "bill_depth", "flipper_length", "body_mass"))
plot(pcp, variables = c("species", "island"), variable_type = "categorical", categorical_type = "bars")


# _______________________________________________________________________
# -------------------------- PDP ----------------------------------------

pdp <- model_profile(explainer = explain_glm, 
                     variables = c("bill_length", "bill_depth", "flipper_length", "body_mass"))
plot(pdp)
pdp <- model_profile(explainer = explain_glm, 
                     variables = c("species", "island"), variable_type = "categorical")
plot(pdp)

# _______________________________________________________________________
# -------------------------- BD -----------------------------------------

predict(model_glm_2, train_set_3[13,], type="prob")

bd1 <- predict_parts(explainer = explain_glm,
                     new_observation = train_set_3[13,],
                     type = "break_down_interactions")
plot(bd1)


# _______________________________________________________________________
# -------------------------- SHAP ---------------------------------------

shap <- predict_parts(explainer = explain_glm, 
                      new_observation = train_set_3[13,], 
                      type = "shap")
p1 <- plot(shap)
p2 <- plot(shap, show_boxplots = FALSE) 
grid.arrange(p1, p2, nrow = 1)

