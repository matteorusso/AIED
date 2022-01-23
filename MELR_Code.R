library(readr)
library(dplyr)
library(lme4)
library(nlme)
library(cvAUC)

#### DATA PREPROCESSING #####

train <- read_csv("Downloads/AIED-main/data/new_train_MELR.csv", 
                  col_types = cols(X1 = col_skip(), username = col_character()))

#categorical features as factor type
train$course_id <- as.factor(train$course_id)
train$username <- as.factor(train$username)
train$gender <- as.factor(train$gender)
train$education <- as.factor(train$education)
train$cat <- as.factor(train$cat)
train$Uni <- as.factor(train$Uni)
train$start_month <- as.factor(train$start_month)
train$repeated <- as.factor(train$repeated)

#standart scale non categorical values
train <- train %>% mutate_at(c('w1_click', 'w2_click', 'w3_click', 'w4_click', 'w5_click', 'w1_close', 'w2_close', 'w3_close', 'w4_close', 'w5_close', 'w1_forum', 'w2_forum', 'w3_forum', 'w4_forum', 'w5_forum', 'w1_problem', 'w2_problem', 'w3_problem', 'w4_problem', 'w5_problem', 'w1_session_numbers', 'w2_session_numbers', 'w3_session_numbers', 'w4_session_numbers', 'w5_session_numbers', 'w1_time_spent', 'w2_time_spent', 'w3_time_spent', 'w4_time_spent', 'w5_time_spent', 'w1_video', 'w2_video', 'w3_video', 'w4_video', 'w5_video', "age", 'simultaneous_courses', 'previous_completion', 'previous_courses'), ~(scale(.) %>% as.vector))

test <- read_csv("Downloads/AIED-main/data/new_test_MELR.csv", 
                 col_types = cols(X1 = col_skip(), username = col_character()))

#categorical features as factor type
test$course_id <- as.factor(test$course_id)
test$username <- as.factor(test$username)
test$gender <- as.factor(test$gender)
test$education <- as.factor(test$education)
test$cat <- as.factor(test$cat)
test$start_month <- as.factor(test$start_month)
test$repeated <- as.factor(test$repeated)
test$Uni <- as.factor(test$Uni)

#standart scale non categorical values
test <- test %>% mutate_at(c('w1_click', 'w2_click', 'w3_click', 'w4_click', 'w5_click', 'w1_close', 'w2_close', 'w3_close', 'w4_close', 'w5_close', 'w1_forum', 'w2_forum', 'w3_forum', 'w4_forum', 'w5_forum', 'w1_problem', 'w2_problem', 'w3_problem', 'w4_problem', 'w5_problem', 'w1_session_numbers', 'w2_session_numbers', 'w3_session_numbers', 'w4_session_numbers', 'w5_session_numbers', 'w1_time_spent', 'w2_time_spent', 'w3_time_spent', 'w4_time_spent', 'w5_time_spent', 'w1_video', 'w2_video', 'w3_video', 'w4_video', 'w5_video', "age", 'simultaneous_courses', 'previous_completion', 'previous_courses'), ~(scale(.) %>% as.vector))


#### MIXED EFFECTS LOGISTIC REGRESSIONS ####

# option for regression to have categorical regression coefficients with the restriction to sum to 0
# therefore the intercept of our models correspond to the overall means of the data
options(contrasts = c("contr.sum", "contr.poly"))

# MELR with user specific features per week
results_user1 <- glmer(w1_y ~ gender + education+age + simultaneous_courses + previous_completion*previous_courses +  (1 | username) + (1 | course_id), data = train[(train$w1_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_user2 <- glmer(w2_y ~ gender + education+age + simultaneous_courses + previous_completion*previous_courses +  (1 | username) + (1 | course_id), data = train[(train$w2_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_user3 <- glmer(w3_y ~ gender + education+age + simultaneous_courses + previous_completion*previous_courses +  (1 | username) + (1 | course_id), data = train[(train$w3_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_user4 <- glmer(w4_y ~ gender + education+age + simultaneous_courses + previous_completion*previous_courses +  (1 | username) + (1 | course_id), data = train[(train$w4_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_user5 <- glmer(w5_y ~ gender + education+age + simultaneous_courses + previous_completion*previous_courses +  (1 | username) + (1 | course_id), data = train[(train$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
summary(results_user1)
p.adjust(summary(results_user1)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_user2)
p.adjust(summary(results_user2)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_user3)
p.adjust(summary(results_user3)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_user4)
p.adjust(summary(results_user4)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_user5)
p.adjust(summary(results_user5)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values

# MELR with activity specific features per week
results_activity1 <- glmer(w1_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + (1 | username) + (1 | course_id), data = train[(train$w1_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_activity2 <- glmer(w2_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + (1 | username) + (1 | course_id), data = train[(train$w2_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_activity3 <- glmer(w3_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + (1 | username) + (1 | course_id), data = train[(train$w3_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_activity4 <- glmer(w4_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + (1 | username) + (1 | course_id), data = train[(train$w4_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_activity5 <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + w5_click + w5_close + w5_forum + w5_problem + w5_session_numbers + w5_time_spent + w5_video + (1 | username) + (1 | course_id), data = train[(train$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
summary(results_activity1)
p.adjust(summary(results_activity1)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_activity2)
p.adjust(summary(results_activity2)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_activity3)
p.adjust(summary(results_activity3)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_activity4)
p.adjust(summary(results_activity4)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_activity5)
p.adjust(summary(results_activity5)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values

# MELR with course specific features per week
results_course1 <- glmer(w1_y ~ cat + Uni + start_month + repeated +  (1 | username) + (1 | course_id), data = train[(train$w1_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_course2 <- glmer(w2_y ~ cat + Uni + start_month + repeated +  (1 | username) + (1 | course_id), data = train[(train$w2_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_course3 <- glmer(w3_y ~ cat + Uni + start_month + repeated +  (1 | username) + (1 | course_id), data = train[(train$w3_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_course4 <- glmer(w4_y ~ cat + Uni + start_month + repeated +  (1 | username) + (1 | course_id), data = train[(train$w4_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_course5 <- glmer(w5_y ~ cat + Uni + start_month + repeated +  (1 | username) + (1 | course_id), data = train[(train$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
summary(results_course1)
p.adjust(summary(results_course1)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_course2)
p.adjust(summary(results_course2)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_course3)
p.adjust(summary(results_course3)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_course4)
p.adjust(summary(results_course4)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_course5)
p.adjust(summary(results_course5)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values

# MELR with combined significant features per week
# starting point for those models was to include all significant features from the above models at week t
# we then exluded any features that are not significant in conjuction anymore
# we tested this with the drop1(results, test="Chisq", trace = TRUE) function
results_all1 <- glmer(w1_y ~ previous_completion*previous_courses + w1_click + w1_close + w1_forum +  w1_session_numbers + w1_time_spent + (1 | username) + (1 | course_id), data = train[(train$w1_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_all2 <- glmer(w2_y ~ previous_completion+previous_courses + w1_close + w1_forum + w1_session_numbers + w1_time_spent + w2_click + w2_close + w2_session_numbers + w2_time_spent + start_month + (1 | username) + (1 | course_id), data = train[(train$w2_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_all3 <- glmer(w3_y ~ previous_completion + w1_session_numbers + w1_time_spent + w2_click + w2_close + w2_session_numbers + w2_time_spent + w3_click + w3_close + w3_session_numbers + w3_time_spent + cat + start_month + (1 | username) + (1 | course_id), data = train[(train$w3_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_all4 <- glmer(w4_y ~ previous_completion + w1_session_numbers + w1_time_spent + w2_click + w2_close + w2_forum + w2_session_numbers + w2_time_spent + w3_session_numbers + w3_time_spent + w4_close + w4_session_numbers + w4_time_spent + (1 | username) + (1 | course_id), data = train[(train$w4_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
results_all5 <- glmer(w5_y ~ previous_completion + w1_time_spent + w2_time_spent + w3_session_numbers + w3_time_spent + w4_session_numbers + w4_time_spent + w5_close + w5_session_numbers + w5_time_spent + start_month + (1 | username) + (1 | course_id), data = train[(train$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
summary(results_all1)
p.adjust(summary(results_all1)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_all2)
p.adjust(summary(results_all2)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_all3)
p.adjust(summary(results_all3)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_all4)
p.adjust(summary(results_all4)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values
summary(results_all5)
p.adjust(summary(results_all5)$coefficients[,4] , method = "bonferroni") #for bonferroni correction of p-values



#### AUC CALCULATIONS OF MODELS ####

# AUCs of user feature models
AUC(predict(results_user1, test[(test$w1_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w1_has_activity != 0),]$w1_y, label.ordering = NULL)
AUC(predict(results_user2, test[(test$w2_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w2_has_activity != 0),]$w2_y, label.ordering = NULL)
AUC(predict(results_user3, test[(test$w3_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w3_has_activity != 0),]$w3_y, label.ordering = NULL)
AUC(predict(results_user4, test[(test$w4_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w4_has_activity != 0),]$w4_y, label.ordering = NULL)
AUC(predict(results_user5, test[(test$w5_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w5_has_activity != 0),]$w5_y, label.ordering = NULL)

# AUCs of activity feature models
AUC(predict(results_activity1, test[(test$w1_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w1_has_activity != 0),]$w1_y, label.ordering = NULL)
AUC(predict(results_activity2, test[(test$w2_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w2_has_activity != 0),]$w2_y, label.ordering = NULL)
AUC(predict(results_activity3, test[(test$w3_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w3_has_activity != 0),]$w3_y, label.ordering = NULL)
AUC(predict(results_activity4, test[(test$w4_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w4_has_activity != 0),]$w4_y, label.ordering = NULL)
AUC(predict(results_activity5, test[(test$w5_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w5_has_activity != 0),]$w5_y, label.ordering = NULL)

# AUCs of course feature models
AUC(predict(results_course1, test[(test$w1_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w1_has_activity != 0),]$w1_y, label.ordering = NULL)
AUC(predict(results_course2, test[(test$w2_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w2_has_activity != 0),]$w2_y, label.ordering = NULL)
AUC(predict(results_course3, test[(test$w3_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w3_has_activity != 0),]$w3_y, label.ordering = NULL)
AUC(predict(results_course4, test[(test$w4_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w4_has_activity != 0),]$w4_y, label.ordering = NULL)
AUC(predict(results_course5, test[(test$w5_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w5_has_activity != 0),]$w5_y, label.ordering = NULL)

# AUCs of all significant feature models
AUC(predict(results_all1, test[(test$w1_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w1_has_activity != 0),]$w1_y, label.ordering = NULL)
AUC(predict(results_all2, test[(test$w2_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w2_has_activity != 0),]$w2_y, label.ordering = NULL)
AUC(predict(results_all3, test[(test$w3_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w3_has_activity != 0),]$w3_y, label.ordering = NULL)
AUC(predict(results_all4, test[(test$w4_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w4_has_activity != 0),]$w4_y, label.ordering = NULL)
AUC(predict(results_all5, test[(test$w5_has_activity != 0),], type="response", allow.new.levels = TRUE), test[(test$w5_has_activity != 0),]$w5_y, label.ordering = NULL)
