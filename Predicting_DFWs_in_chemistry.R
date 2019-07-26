library(tidyverse)
library(caret)

# read in data
df1 <- read_csv("CH115_and_CH117_courses_7_5_19.csv")

# quick summary
df1 %>%
  glimpse() %>%
  summary()

# glance at data missingness
df1 %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(var.name, num_nulls) %>%
  arrange(-num_nulls)

# a few visuals -----------------------------------------------------------

# seperate numeric and categorical features 

df1.numeric <- df1 %>%
  select(TEST_SCORE, ACT_MATH_TEST_SCORE, SAT_MATH_TEST_SCORE, HIGH_SCHOOL_GPA, HIGH_SCHOOL_RANK_PERCENTILE, 
         CURRENT_TOTAL_CREDITS)

df1.categorical <- df1 %>%
  select(-TEST_SCORE, -ACT_MATH_TEST_SCORE, -SAT_MATH_TEST_SCORE, -HIGH_SCHOOL_GPA, -HIGH_SCHOOL_RANK_PERCENTILE, 
         -CURRENT_TOTAL_CREDITS)

# grades distribution 
ggplot(data = df1, aes(FINAL_GRADE)) + 
  geom_bar() + 
  labs(title = 'Distribution of Grades: CH115 and CH117, 2012-2019', 
       x = 'Grade', 
       y = 'Count') +
  theme(plot.title = element_text(hjust = 0.5))

# distributions of numeric variables
df.numdist <- df1.numeric %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# distributions of categorical variables
df.catdist <- df1.categorical %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat = 'count')

# data pre-processing ---------------------------------------------------------

# numeric variables 

# 1-impute median for current_total_credits
# 2-impute median for high_school_gpa (hsgpa = 0 is actually NA)
# 3-impute median for high_school_rank_percentile
# 4-impute median for test_score

# glance at variables before transformation
df1.numeric %>%
  select(CURRENT_TOTAL_CREDITS, HIGH_SCHOOL_GPA, 
         HIGH_SCHOOL_RANK_PERCENTILE, TEST_SCORE) %>%
  glimpse() %>%
  summary() 

# median impute
medimpute_vars <- preProcess(df1.numeric, method = c("medianImpute"))

# Use predict to transform data
medimpute_df_pred <- predict(medimpute_vars, df1)

# impute median for high_school_gpa = 0
medimpute_df_pred <- medimpute_df_pred %>%
  mutate(HIGH_SCHOOL_GPA = ifelse(HIGH_SCHOOL_GPA == 0, 
                                  median(HIGH_SCHOOL_GPA), 
                                  HIGH_SCHOOL_GPA))

# glance at variables after transformations
medimpute_df_pred %>%
  select(CURRENT_TOTAL_CREDITS, HIGH_SCHOOL_GPA, 
         HIGH_SCHOOL_RANK_PERCENTILE, TEST_SCORE) %>%
  glimpse() %>%
  summary() 

medimpute_df_pred %>%
  select(CURRENT_TOTAL_CREDITS, HIGH_SCHOOL_GPA, 
         HIGH_SCHOOL_RANK_PERCENTILE, TEST_SCORE) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

df2 <- medimpute_df_pred

# categorical variables 

# glance at variables before transformation
df1.categorical %>%
  glimpse() %>%
  summary() 

# change person_uid to factor
# one_hot encoding for academic period
# drop course_reference_number
# mutate repeat_course_ind
# change instructor_id to factor and one_hot encode
# drop instructor_last_name and instructor_first_name
# change begin_time to factor and one hot encode
# change end_time to factor and one hot encode
# change meeting_days to factor and one hot encode
# change meeting hours to factor and one hot encode
# change college to factor and one hot encode
# change major to factor and one hot encode
# change gender to factor and one hot encode
# change housing_ind to factor 
# change ipeds_desc to factor and one hot encode
# mutate institution_1 to 1 if not null 
# change student_class_first to factor and one hot encode

df1.categorical <- df1.categorical %>%
  mutate(PERSON_UID = as.factor(PERSON_UID)) %>%
  mutate(ACADEMIC_PERIOD = as.factor(ACADEMIC_PERIOD)) %>%
  select(-COURSE_REFERENCE_NUMBER) %>%
  mutate(REPEAT_COURSE_IND = ifelse(is.na(REPEAT_COURSE_IND), 0, 1)) %>%
  mutate(INSTRUCTOR_ID = as.factor(INSTRUCTOR_ID)) %>%
  select(-INSTRUCTOR_LAST_NAME, -INSTRUCTOR_FIRST_NAME) %>%
  mutate(BEGIN_TIME = as.factor(BEGIN_TIME)) %>%
  mutate(END_TIME = as.factor(END_TIME)) %>%
  mutate(MEETING_DAYS = as.factor(MEETING_DAYS)) %>%
  mutate(MEETING_HOURS = as.factor(MEETING_HOURS)) %>%
  mutate(COLLEGE = as.factor(COLLEGE)) %>%
  select(-MAJOR) %>%
  mutate(MAJOR_DESC = as.factor(MAJOR_DESC)) %>%
  mutate(GENDER = as.factor(GENDER)) %>%
  mutate(HOUSING_IND = ifelse(HOUSING_IND == 'Y', 1, 0)) %>%
  mutate(IPEDSR10_DESC = as.factor(IPEDSR10_DESC)) %>%
  mutate(INSTITUTION_1 = ifelse(is.na(INSTITUTION_1), 0, 1)) %>%
  mutate(STUDENT_CLASS_FIRST = as.factor(STUDENT_CLASS_FIRST))

# note: ACADEMIC_PERIOD and MEETING_HOURS not coerced to factor
df1.categorical$ACADEMIC_PERIOD <- as.factor(df1.categorical$ACADEMIC_PERIOD)
df1.categorical$MEETING_HOURS <- as.factor(df1.categorical$MEETING_HOURS)

df2$ACADEMIC_PERIOD <- as.factor(df2$ACADEMIC_PERIOD)
df2$MEETING_HOURS <- as.factor(df2$MEETING_HOURS)

df1.categorical %>%
  select(ACADEMIC_PERIOD, MEETING_HOURS) %>%
  glimpse() %>%
  str() %>%
  summary()

# one hot encoding
df.onehot <- df1.categorical %>%
  select(ACADEMIC_PERIOD, INSTRUCTOR_ID, 
         BEGIN_TIME, END_TIME, MEETING_DAYS, MEETING_HOURS,
         COLLEGE, MAJOR_DESC, GENDER, IPEDSR10_DESC, 
         STUDENT_CLASS_FIRST) 
df.onehot <- dummyVars( ~ ., data = df.onehot, fullRank=T)
df.onehot <- data.frame(predict(df.onehot, df2))

df3 <- df2 %>%
  select(FINAL_GRADE, CURRENT_TOTAL_CREDITS, HIGH_SCHOOL_GPA, 
         HIGH_SCHOOL_RANK_PERCENTILE, TEST_SCORE) %>%
  bind_cols(df.onehot) 

# look at dataset 
df3 %>%
  glimpse() 

# check for missing values 
df.missing <- df3 %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(var.name, num_nulls) %>%
  arrange(-num_nulls)

# drop rows with missing values - students who did not take the course
# for credit 
df3 <- df3 %>%
  filter(complete.cases(.))

# look at near zero variance predictors 

# identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(df3, names = TRUE, 
                           freqCut = 19, uniqueCut = 10)

# get all column names and remove 
all_cols <- names(df3)

# remove 
df4 <- df3[ , setdiff(all_cols, remove_cols)]

# check 
df4 %>%
  glimpse() 

# note: revisit without removing nzvs if time allows

# check for multicollinearity
df.vifcheck <- df4  %>%
  select(CURRENT_TOTAL_CREDITS, HIGH_SCHOOL_GPA, 
         HIGH_SCHOOL_RANK_PERCENTILE, TEST_SCORE)
# 
# # lm model 
# m.vifcheck <- lm(FINAL_GRADE ~ ., data = df.vifcheck)
# 
# library(olsrr)
# output.vifcheck <- ols_vif_tol(m.vifcheck)
# output.vifcheck <- output.vifcheck %>%
#   filter(VIF > 4)

# look at correlations
library(Hmisc)
df.vifcheck <- df4 %>%
  keep(is.numeric) %>%
  as.matrix() %>%
  rcorr(type = "spearman")  # note: firstgen is a factor

# plot of correlations
library(corrplot)
corrplot(df.vifcheck$r, type = "upper" )

# create classification output variable 
df5 <- df4 %>%
  mutate(FINAL_GRADE = ifelse(FINAL_GRADE %in% c('A', 'B', 'C'), 0, 
                              ifelse(FINAL_GRADE %in% c('D', 'F', 'W'), 1, 99))) %>%
  filter(!FINAL_GRADE == 99)

df6 <- df5 %>%
  select(-starts_with('END'))

# create 'yes' or 'no' output variable 
df7 <- df6 %>%
  mutate(FINAL_GRADE = ifelse(FINAL_GRADE == 1, 'Yes', 'No')) %>%
  mutate(FINAL_GRADE = as.factor(FINAL_GRADE))

df8 <- df7 %>%
  select(-HIGH_SCHOOL_RANK_PERCENTILE) %>%
  mutate(gpa.quart = ntile(HIGH_SCHOOL_GPA, 4)) %>%
  mutate(testscore.quart = ntile(TEST_SCORE, 4)) %>%
  select(-HIGH_SCHOOL_GPA, -TEST_SCORE) %>%
  mutate(gpa.quart = as.factor(gpa.quart)) %>%
  mutate(testscore.quart = as.factor(testscore.quart))

# one hot encode quartile variables 
df.onehot.2 <- df8 %>%
  select(gpa.quart, testscore.quart) %>%
  mutate(gpa.quart = as.factor(gpa.quart)) %>%
  mutate(testscore.quart = as.factor(testscore.quart))

df.onehot.2 <- dummyVars( ~ ., data = df.onehot.2, fullRank=T)
df.onehot.2 <- data.frame(predict(df.onehot.2, df8))

df9 <- df8 %>%
  select(-gpa.quart, -testscore.quart) %>%
  bind_cols(df.onehot.2) 

df9 %>%
  glimpse()

df.10 <- df9 %>%
  mutate(crt.tot.cred = ntile(CURRENT_TOTAL_CREDITS, 4)) %>%
  select(-CURRENT_TOTAL_CREDITS)

df.10$crt.tot.cred <- as.factor(df.10$crt.tot.cred)

df.onehot.3 <- df.10 %>%
  select(crt.tot.cred) %>%
  mutate(crt.tot.cred = as.factor(crt.tot.cred))

df.onehot.3 <- dummyVars( ~ ., data = df.onehot.3, fullRank=T)
df.onehot.3 <- data.frame(predict(df.onehot.3, df.10))

df.10 %>% glimpse()

df.11 <- df.10 %>%
  select(-crt.tot.cred) %>%
  bind_cols(df.onehot.3) 

df.11 %>%
  glimpse()

df.11 <- df.11 %>%
  select(-starts_with('ACADEMIC'))

# train and test data ------------------------------------------------------

# filter courses from fall 2018 and spring 2019 and set aside for testing 

train_1<- df7 %>%
  filter(!ACADEMIC_PERIOD.201850 == 1) %>%
  filter(!ACADEMIC_PERIOD.201930 == 1)

train_1 %>%
  select(ACADEMIC_PERIOD.201850, ACADEMIC_PERIOD.201930) %>%
  glimpse() %>%
  summary()

test_a <- df7 %>%
  filter(ACADEMIC_PERIOD.201850 == '1')
test_b <- df7 %>%
  filter(ACADEMIC_PERIOD.201930 == '1')
test_1 <- test_a %>%
  bind_rows(test_b)

test_1 %>%
  select(ACADEMIC_PERIOD.201850, ACADEMIC_PERIOD.201930) %>%
  glimpse() %>%
  summary()

train_2 <- df.10 %>%
  filter(!ACADEMIC_PERIOD.201850 == 1) %>%
  filter(!ACADEMIC_PERIOD.201930 == 1)

test_a.2 <- df.10 %>%
  filter(ACADEMIC_PERIOD.201850 == '1')
test_b.2 <- df.10 %>%
  filter(ACADEMIC_PERIOD.201930 == '1')

test_2 <- test_a.2 %>%
  bind_rows(test_b.2)

test_2 %>%
  select(ACADEMIC_PERIOD.201850, ACADEMIC_PERIOD.201930) %>%
  glimpse() %>%
  summary()

# initial models ------------------------------------------------------------

# glmnet using grid search 
myControl <- trainControl(
  method='cv',
  number=10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE,
  savePredictions = TRUE,
  search = "grid"
)

# glmnet 
m.glmnet.5 <- train(
  FINAL_GRADE ~ .,
  data = train_1,
  method = "glmnet",
  trControl = myControl, 
  tuneLength = 20
)

print(m.glmnet.5)
plot(m.glmnet.5)
res.glmnet.5 <- max(m.glmnet.5[["results"]][["ROC"]])

# random forest using grid search 
m.rf.4 <- train(
  FINAL_GRADE ~ .,
  data = train_1,
  method = "ranger",
  trControl = myControl, 
  tuneLength = 20, 
  importance = 'impurity'
)

print(m.rf.4)
plot(m.rf.4)
res.rf.4 <- max(m.rf.4[["results"]][["ROC"]])
plot(varImp(m.rf.4))

# glmnet predictions
pred <- predict(m.glmnet.5, newdata = test_1, type = 'raw')
cm.glm1 <- confusionMatrix(pred, test_1$FINAL_GRADE, positive = 'Yes')

# rf predictions
pred <- predict(m.rf.4, newdata = test_1, type = 'raw')
cm.rf1 <- confusionMatrix(pred, test_1$FINAL_GRADE, positive = 'Yes')

# not very good; look at training on unbalanced data

# train on unbalanced data ---------------------------------------------------

set.seed(7)
down_train1 <- downSample(x = train_1[, -ncol(train_1)],
                          y = train_1$FINAL_GRADE)
down_train1 <- as.data.frame(down_train1)
down_train1 <- down_train1 %>%
  select(-Class)
table(train_1$FINAL_GRADE) 
table(down_train1$FINAL_GRADE)

down_train1 %>% glimpse()

# glmnet
m.glmnet.6 <- train(
  FINAL_GRADE ~ .,
  data = down_train1,
  method = "glmnet",
  trControl = myControl, 
  tuneLength = 20
)

print(m.glmnet.6)
plot(m.glmnet.6)
res.glmnet.6 <- max(m.glmnet.6[["results"]][["ROC"]])
plot(varImp(m.glmnet.6))

# glmnet predictions
pred <- predict(m.glmnet.6, newdata = test_1)
cm.glmnet2 <- confusionMatrix(pred, test_1$FINAL_GRADE, positive = 'Yes')

# rf trained on balanced data 
m.rf.7 <- train(
  FINAL_GRADE ~ .,
  data = down_train1,
  method = "ranger",
  trControl = myControl, 
  tuneLength = 20,
  importance = 'impurity'
)

print(m.rf.7)
plot(m.rf.7)
res.rf.7 <- max(m.rf.7[["results"]][["ROC"]])
plot(varImp(m.rf.7))

# glmnet predictions
pred <- predict(m.rf.7, newdata = test_1)
cm.rf2 <- confusionMatrix(pred, test_1$FINAL_GRADE, positive = 'Yes')

# gbm on balanced data 
m.gbm.4 <- train(
  FINAL_GRADE ~ .,
  data = down_train1,
  method = "gbm",
  trControl = myControl, 
  tuneLength = 20
)

print(m.gbm.4)
plot(m.gbm.4)
res.gbm.4 <- max(m.gbm.4[["results"]][["ROC"]])

# gbm predictions
pred <- predict(m.gbm.4, newdata = test_1)
cm.gbm1 <- confusionMatrix(pred, test_1$FINAL_GRADE, positive = 'Yes')
summary(m.gbm.4)

# try to get more parsimonious model -----------------------------------------

set.seed(8)
down_train2 <- downSample(x = train_2[, -ncol(train_2)],
                          y = train_2$FINAL_GRADE)
down_train2 <- as.data.frame(down_train2)
down_train2 <- down_train2 %>%
  select(-Class)
table(train_2$FINAL_GRADE) 
table(down_train2$FINAL_GRADE)

# remove academic period variables 
down_train2 <- down_train2 %>%
  select(-starts_with('ACADEMIC'))

# glmnet
m.glmnet.7 <- train(
  FINAL_GRADE ~ .,
  data = down_train2,
  method = "glmnet",
  trControl = myControl, 
  tuneLength = 20
)

print(m.glmnet.7)
plot(m.glmnet.7)
res.glmnet.7 <- max(m.glmnet.7[["results"]][["ROC"]])
plot(varImp(m.glmnet.7))

# glmnet predictions
pred <- predict(m.glmnet.7, newdata = test_2)
cm.glmnet3 <- confusionMatrix(pred, test_2$FINAL_GRADE, positive = 'Yes')

# rf
m.rf.8 <- train(
  FINAL_GRADE ~ .,
  data = down_train2,
  method = "glmnet",
  trControl = myControl, 
  tuneLength = 20
)

print(m.rf.8)
plot(m.rf.8)
res.rf.8 <- max(m.rf.8[["results"]][["ROC"]])
plot(varImp(m.rf.8))

pred <- predict(m.rf.8, newdata = test_2)
cm.rf3 <- confusionMatrix(pred, test_2$FINAL_GRADE, positive = 'Yes')

# gbm
m.gbm.5 <- train(
  FINAL_GRADE ~ .,
  data = down_train2,
  method = "gbm",
  trControl = myControl, 
  tuneLength = 20
)

print(m.gbm.5)
plot(m.gbm.5)
res.gbm.5 <- max(m.gbm.5[["results"]][["ROC"]])
summary(m.gbm.5)

pred <- predict(m.gbm.5, newdata = test_2)
cm.gbm2 <- confusionMatrix(pred, test_2$FINAL_GRADE, positive = 'Yes')

# visualize results: focus on highest sensitivity ----------------------------

library(broom)
m1 <- tidy(cm.glmnet2)
m2 <- tidy(cm.rf2)
m3 <- tidy(cm.gbm1)
m4 <- tidy(cm.glmnet3)
m5 <- tidy(cm.rf3)
m6 <- tidy(cm.gbm2)

m1r <- m1 %>%
  select(term, estimate) %>%
  spread(term, estimate)
m2r <- m2 %>%
  select(term, estimate) %>%
  spread(term, estimate)
m3r <- m3 %>%
  select(term, estimate) %>%
  spread(term, estimate)
m4r <- m4 %>%
  select(term, estimate) %>%
  spread(term, estimate)
m5r <- m5 %>%
  select(term, estimate) %>%
  spread(term, estimate)
m6r <- m6 %>%
  select(term, estimate) %>%
  spread(term, estimate)

df.results <- m1r %>%
  bind_rows(m2r) %>%
  bind_rows(m3r) %>%
  bind_rows(m4r) %>%
  bind_rows(m5r) %>%
  bind_rows(m6r)

rnames <- c('glmnet1', 'rf1', 'gbm1', 'glmnet2', 'rf2', 'gbm2')
df.results$mnames <- rnames

# quick visual 
ggplot(df.results, aes(mnames, sensitivity)) +
  geom_point()

# model: parsimonious glmnet on balanced data ---------------------------

# next steps: max lift/cumulative gain 



