library(readr)
train <- read_csv("Downloads/AIED-main/data/new_train.csv", 
                  col_types = cols(X1 = col_skip(), username = col_character()))
View(train)

train$course_id <- as.factor(train$course_id)

train$username <- as.factor(train$username)

results <- glmer(y ~ week + click + close + forum + problem + session_numbers + time_spent + video + (1 | username) + (1 | course_id), data = train, family = binomial(link = "logit"))

library(dplyr)
train <- train %>% mutate_at(c("time_spent", "video", "click", "close", "forum", "problem", "session_numbers", "week"), ~(scale(.) %>% as.vector))

control = glmerControl(tolPwrss=1e-3)

, nAGQ = 0)


test <- read_csv("Downloads/AIED-main/data/new_test.csv", 
                  col_types = cols(X1 = col_skip(), username = col_character()))

test$course_id <- as.factor(test$course_id)

test$username <- as.factor(test$username)

test <- test %>% mutate_at(c("time_spent", "video", "click", "close", "forum", "problem", "session_numbers", "week"), ~(scale(.) %>% as.vector))

mean(test$y - predict(results_2, test, type="response", allow.new.levels = TRUE))

predict(results_2, newdata = test, type = "response")

percent(test$y == round(predict(results_2, test, type="response", allow.new.levels = TRUE)))

AUC(predict(results1, test, type="response", allow.new.levels = TRUE), test$y, label.ordering = NULL)


train_plus <- read_csv("Downloads/AIED-main/data/new_train_user_course.csv", 
                       col_types = cols(X1 = col_skip(), username = col_character()))

train_plus$course_id <- as.factor(train_plus$course_id)

train_plus$username <- as.factor(train_plus$username)

train_plus <- train_plus %>% mutate_at(c("time_spent", "video", "click", "close", "forum", "problem", "session_numbers", "week", "age"), ~(scale(.) %>% as.vector))


test_plus <- read_csv("Downloads/AIED-main/data/new_test_user_course.csv", col_types = cols(X1 = col_skip(), username = col_character()))

test_plus$course_id <- as.factor(test_plus$course_id)

test_plus$username <- as.factor(test_plus$username)

test_plus <- test_plus %>% mutate_at(c("time_spent", "video", "click", "close", "forum", "problem", "session_numbers", "week", "age"), ~(scale(.) %>% as.vector))

results_plus <- glmer(y ~ week + click + close + forum + problem + session_numbers + time_spent + video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_plus, family = binomial(link = "logit"), nAGQ = 0)

AUC(predict(results_plus, test_plus, type="response", allow.new.levels = TRUE), test_plus$y, label.ordering = NULL)


### Analysis with weekly data

train_week <- read_csv("Downloads/AIED-main/data/new_train_user_course_wide.csv", 
                       col_types = cols(X1 = col_skip(), username = col_character()))

train_week$course_id <- as.factor(train_week$course_id)

train_week$username <- as.factor(train_week$username)

train_week <- train_week %>% mutate_at(c('w1_click', 'w2_click', 'w3_click', 'w4_click', 'w5_click', 'w1_close', 'w2_close', 'w3_close', 'w4_close', 'w5_close', 'w1_forum', 'w2_forum', 'w3_forum', 'w4_forum', 'w5_forum', 'w1_problem', 'w2_problem', 'w3_problem', 'w4_problem', 'w5_problem', 'w1_session_numbers', 'w2_session_numbers', 'w3_session_numbers', 'w4_session_numbers', 'w5_session_numbers', 'w1_time_spent', 'w2_time_spent', 'w3_time_spent', 'w4_time_spent', 'w5_time_spent', 'w1_video', 'w2_video', 'w3_video', 'w4_video', 'w5_video', "age"), ~(scale(.) %>% as.vector))

test_week <- read_csv("Downloads/AIED-main/data/new_test_user_course_wide.csv", col_types = cols(X1 = col_skip(), username = col_character()))

test_week$course_id <- as.factor(test_week$course_id)

test_week$username <- as.factor(test_week$username)

test_week <- test_week %>% mutate_at(c('w1_click', 'w2_click', 'w3_click', 'w4_click', 'w5_click', 'w1_close', 'w2_close', 'w3_close', 'w4_close', 'w5_close', 'w1_forum', 'w2_forum', 'w3_forum', 'w4_forum', 'w5_forum', 'w1_problem', 'w2_problem', 'w3_problem', 'w4_problem', 'w5_problem', 'w1_session_numbers', 'w2_session_numbers', 'w3_session_numbers', 'w4_session_numbers', 'w5_session_numbers', 'w1_time_spent', 'w2_time_spent', 'w3_time_spent', 'w4_time_spent', 'w5_time_spent', 'w1_video', 'w2_video', 'w3_video', 'w4_video', 'w5_video', "age"), ~(scale(.) %>% as.vector))

# activity in next week prediction (without exclusion)
results_w1 <- glmer(w1_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w1, test_week, type="response", allow.new.levels = TRUE), test_week$w1_y, label.ordering = NULL)

results_w2 <- glmer(w2_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w2, test_week, type="response", allow.new.levels = TRUE), test_week$w2_y, label.ordering = NULL)

results_w3 <- glmer(w3_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w3, test_week, type="response", allow.new.levels = TRUE), test_week$w3_y, label.ordering = NULL)

results_w4 <- glmer(w4_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w4, test_week, type="response", allow.new.levels = TRUE), test_week$w4_y, label.ordering = NULL)

results_w5 <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum  + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + w5_click + w5_close + w5_forum + w5_problem + w5_session_numbers + w5_time_spent + w5_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w5, test_week, type="response", allow.new.levels = TRUE), test_week$w5_y, label.ordering = NULL)

#completion prediction
results_w1c <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w1c, test_week, type="response", allow.new.levels = TRUE), test_week$w5_y, label.ordering = NULL)

results_w2c <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w2c, test_week, type="response", allow.new.levels = TRUE), test_week$w5_y, label.ordering = NULL)

results_w3c <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w3c, test_week, type="response", allow.new.levels = TRUE), test_week$w5_y, label.ordering = NULL)

results_w4c <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w4c, test_week, type="response", allow.new.levels = TRUE), test_week$w5_y, label.ordering = NULL)

results_w5c <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + w5_click + w5_close + w5_forum + w5_problem + w5_session_numbers + w5_time_spent + w5_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week, family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w5c, test_week, type="response", allow.new.levels = TRUE), test_week$w5_y, label.ordering = NULL)

#completion prediction with users with previous activity only
results_w1cu <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w1_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w1cu, test_week[(test_week$w1_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w1_has_activity != 0),]$w5_y, label.ordering = NULL)

results_w2cu <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w2_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w2cu, test_week[(test_week$w2_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w2_has_activity != 0),]$w5_y, label.ordering = NULL)

results_w3cu <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w3_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w3cu, test_week[(test_week$w3_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w3_has_activity != 0),]$w5_y, label.ordering = NULL)

results_w4cu <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w4_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w4cu, test_week[(test_week$w4_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w4_has_activity != 0),]$w5_y, label.ordering = NULL)

results_w5cu <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + w5_click + w5_close + w5_forum + w5_problem + w5_session_numbers + w5_time_spent + w5_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w5cu, test_week[(test_week$w5_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w5_has_activity != 0),]$w5_y, label.ordering = NULL)

#activity in next week prediction with users with previous activity only
results_w1u <- glmer(w1_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w1_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w1u, test_week[(test_week$w1_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w1_has_activity != 0),]$w1_y, label.ordering = NULL)

results_w2u <- glmer(w2_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w2_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w2u, test_week[(test_week$w2_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w2_has_activity != 0),]$w2_y, label.ordering = NULL)

results_w3u <- glmer(w3_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w3_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w3u, test_week[(test_week$w3_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w3_has_activity != 0),]$w3_y, label.ordering = NULL)

results_w4u <- glmer(w4_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w4_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w4u, test_week[(test_week$w4_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w4_has_activity != 0),]$w4_y, label.ordering = NULL)

results_w5u <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + w5_click + w5_close + w5_forum + w5_problem + w5_session_numbers + w5_time_spent + w5_video + female + education_Associate + education_Bachelor + education_Doctorate + education_High + education_Master + education_Middle + category_art + category_biology + category_business + category_chemistry + category_computer + category_economics + category_electrical + category_engineering + category_foreign_language + category_history + category_literature + category_math + category_medicine + category_philosophy + category_physics + category_social_science + age + (1 | username) + (1 | course_id), data = train_week[(train_week$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)
AUC(predict(results_w5u, test_week[(test_week$w5_has_activity != 0),], type="response", allow.new.levels = TRUE), test_week[(test_week$w5_has_activity != 0),]$w5_y, label.ordering = NULL)

stepcAIC(results_w5cu, direction = "backward", trace = TRUE)

#### DATA PREPROCESSING #####

train <- read_csv("Downloads/AIED-main/data/new_train_user_course_wide_compactcat.csv", 
                      col_types = cols(X1 = col_skip(), username = col_character()))

#categorical features as factor type
train$course_id <- as.factor(train$course_id)
train$username <- as.factor(train$username)
train$gender <- as.factor(train$gender)
train$education <- as.factor(train$education)
train$University <- as.factor(train$University)
train$category <- as.factor(train$category)
train$cat <- as.factor(train$cat)
train$Uni <- as.factor(train$Uni)
train$start_month <- as.factor(train$start_month)
train$repeated <- as.factor(train$repeated)

#standart scale non categorical values
train <- train %>% mutate_at(c('w1_click', 'w2_click', 'w3_click', 'w4_click', 'w5_click', 'w1_close', 'w2_close', 'w3_close', 'w4_close', 'w5_close', 'w1_forum', 'w2_forum', 'w3_forum', 'w4_forum', 'w5_forum', 'w1_problem', 'w2_problem', 'w3_problem', 'w4_problem', 'w5_problem', 'w1_session_numbers', 'w2_session_numbers', 'w3_session_numbers', 'w4_session_numbers', 'w5_session_numbers', 'w1_time_spent', 'w2_time_spent', 'w3_time_spent', 'w4_time_spent', 'w5_time_spent', 'w1_video', 'w2_video', 'w3_video', 'w4_video', 'w5_video', "age", 'simultaneous_courses', 'previous_completion', 'previous_courses'), ~(scale(.) %>% as.vector))

test <- read_csv("Downloads/AIED-main/data/new_test_user_course_wide_compactcat.csv", 
                  col_types = cols(X1 = col_skip(), username = col_character()))

#categorical features as factor type
test$course_id <- as.factor(test$course_id)
test$username <- as.factor(test$username)
test$gender <- as.factor(test$gender)
test$education <- as.factor(test$education)
test$University <- as.factor(test$University)
test$category <- as.factor(test$category)
test$cat <- as.factor(test$cat)
test$start_month <- as.factor(test$start_month)
test$repeated <- as.factor(test$repeated)
test$Uni <- as.factor(test$Uni)

#standart scale non categorical values
test <- test %>% mutate_at(c('w1_click', 'w2_click', 'w3_click', 'w4_click', 'w5_click', 'w1_close', 'w2_close', 'w3_close', 'w4_close', 'w5_close', 'w1_forum', 'w2_forum', 'w3_forum', 'w4_forum', 'w5_forum', 'w1_problem', 'w2_problem', 'w3_problem', 'w4_problem', 'w5_problem', 'w1_session_numbers', 'w2_session_numbers', 'w3_session_numbers', 'w4_session_numbers', 'w5_session_numbers', 'w1_time_spent', 'w2_time_spent', 'w3_time_spent', 'w4_time_spent', 'w5_time_spent', 'w1_video', 'w2_video', 'w3_video', 'w4_video', 'w5_video', "age", 'simultaneous_courses', 'previous_completion', 'previous_courses'), ~(scale(.) %>% as.vector))

results5 <- glmer(w5_y ~ w1_click + w1_close + w1_forum + w1_problem + w1_session_numbers + w1_time_spent + w1_video + w2_click + w2_close + w2_forum + w2_problem + w2_session_numbers + w2_time_spent + w2_video + w3_click + w3_close + w3_forum + w3_problem + w3_session_numbers + w3_time_spent + w3_video + w4_click + w4_close + w4_forum + w4_problem + w4_session_numbers + w4_time_spent + w4_video + w5_click + w5_close + w5_forum + w5_problem + w5_session_numbers + w5_time_spent + w5_video + gender + education + category  + age + University + start_month + simultaneous_courses + previous_completion*previous_courses +  (1 | username) + (1 | course_id) + (1 | course_id:username), data = train[(train$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)


#### MIXED EFFECTS LOGISTIC REGRESSIONS ####

# option for regression to have categorical regression coefficients with the restriction to sum to 0
# therefore the intercept of our models correspond to the overall means of the data
options(contrasts = c("contr.sum", "contr.poly"))

results_course5 <- glmer(w5_y ~ category + University + start_month + repeated +  (1 | username) + (1 | course_id) + (1 | course_id:username), data = train[(train$w5_has_activity != 0),], family = binomial(link = "logit"), nAGQ = 0)






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
