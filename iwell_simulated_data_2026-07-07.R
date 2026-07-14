#######################################
#### iWell Tutorials
#### Data Simulation
#### Varsha Venkatesh
#### 2026-07-07
#######################################

# workspace setup
library("dplyr")
library("faux")
library("summarytools")
library("missMethods")

# setting seed
set.seed(2004)

### using sample() to simulate data

# loading in data dictionary
dict <- read.csv("Data/iwell_data_dictionary_2026-07-14.csv")

# create empty data frame and populate with id variable based on N (N = 426)
dat <- data.frame(participant_id = c(1:426))
head(dat)

### simulate individual items based on properties of interest

# age
dat$age <- sample(18:22, size = 426, replace = TRUE, prob = c(.50, .30, .10, .05, .05))
table(dat$age)

# gender
dat$gender <- sample(1:3, size = 426, replace = TRUE, prob = c(.61, .37, .02))
table(dat$gender)

# recode gender
dat$gender <- as.factor(dat$gender)

dat$gender <-  recode(dat$gender,
                      "1" = "Male",
                      "2" = "Female",
                      "3" = "Nonbinary")

# ethnicity
dat$ethnicity <- sample(1:7, size = 426, replace = TRUE, prob = c(.58, .14, .11, .07, .05, .01, .04))
table(dat$ethnicity)

# recode ethnicity
dat$ethnicity <-  recode(dat$ethnicity,
                         "1" = "White",
                         "2" = "Asian/Pacific Islander",
                         "3" = "Black/African American",
                         "4" = "Hispanic/Latine",
                         "5" = "Multiracial",
                         "6" = "Native American",
                         "7" = "Other",
)

# transfer
dat$transfer <- sample(0:1, size = 426, replace = TRUE, prob = c(.70, .30))
table(dat$transfer)

# international
dat$international <- sample(0:1, size = 426, replace = TRUE, prob = c(.94, .06))
table(dat$international)

# us born
dat$us_born <- sample(0:1, size = 426, replace = TRUE, prob = c(.05, .95))
table(dat$us_born)

# parent ed 1
dat$parent_ed_1 <- sample(1:9, size = 426, replace = TRUE, prob = c(.01, .01, .01, .06, .29, .24, .25, .10, .03))
table(dat$parent_ed_1)

# recode parent ed 1
dat$parent_ed_1 <- recode(dat$parent_ed_1,
                          "1" = "No school",
                          "2" = "Elementary school",
                          "3" = "Middle school/Junior high",
                          "4" = "Some high school",
                          "5" = "High school diploma",
                          "6" = "Associates degree or equivalent (2 year degree)",
                          "7" = "Bachelor's degree (4 year degree)",
                          "8" = "Master's degree",
                          "9" = "Doctorate or professional degree")

# parent ed 2
dat$parent_ed_2 <- sample(1:9, size = 426, replace = TRUE, prob = c(.01, .01, .01, .06, .29, .24, .25, .10, .03))
table(dat$parent_ed_2)

# recode parent ed 2
dat$parent_ed_2 <- recode(dat$parent_ed_2,
                          "1" = "No school",
                          "2" = "Elementary school",
                          "3" = "Middle school/Junior high",
                          "4" = "Some high school",
                          "5" = "High school diploma",
                          "6" = "Associates degree or equivalent (2 year degree)",
                          "7" = "Bachelor's degree (4 year degree)",
                          "8" = "Master's degree",
                          "9" = "Doctorate or professional degree")

# major sem 1
dat$major_sem_1 <- sample(1:5, size = 426, replace = TRUE, prob = c(.36, .23, .19, .13, .09))
table(dat$major_sem_1)

dat$major_sem_1 <-  recode(dat$major_sem_1,
                               "1" = "Computer Science",
                               "2" = "Mechanical Engineering",
                               "3" = "Biology",
                               "4" = "Biomedical Engineering",
                               "5" = "Chemical Engineering")

# major sem 2
# setting parameters for randomization
num_students <- 426
change_prob <- 0.35 # 35% of students change their major
majors <- c("Computer Science", "Mechanical Engineering", "Biology", "Biomedical Engineering", "Chemical Engineering")

dat$major_sem_2 <- NA

# randomly choosing who changes major
change_status <- rbinom(n = num_students, size = 1, prob = change_prob)

# assigning new majors to those who change
for (i in 1:num_students) {
  if (change_status[i] == 1) {
    # Exclude the original major so they are forced to pick a new one
    available_majors <- setdiff(majors, dat$major_sem_1[i])
    dat$major_sem_2[i] <- sample(available_majors, 1)
  } else {
    # Keep the same major if they did not change
    dat$major_sem_2[i] <- dat$major_sem_1[i]
  }
}

# major sem 3
# setting parameters for randomization
change_prob_2 <- 0.15 # 15% of students change their major

dat$major_sem_3 <- NA

# randomly choosing who changes major
change_status_2 <- rbinom(n = num_students, size = 1, prob = change_prob_2)

# assigning new majors to those who change
for (i in 1:num_students) {
  if (change_status_2[i] == 1) {
    # Exclude the original major so they are forced to pick a new one
    available_majors <- setdiff(majors, dat$major_sem_2[i])
    dat$major_sem_3[i] <- sample(available_majors, 1)
  } else {
    # Keep the same major if they did not change
    dat$major_sem_3[i] <- dat$major_sem_2[i]
  }
}

# major sem 4
# setting parameters for randomization
change_prob_3 <- 0.05 # 5% of students change their major

dat$major_sem_4 <- NA

# randomly choosing who changes major
change_status_3 <- rbinom(n = num_students, size = 1, prob = change_prob_3)

# assigning new majors to those who change
for (i in 1:num_students) {
  if (change_status_3[i] == 1) {
    # Exclude the original major so they are forced to pick a new one
    available_majors <- setdiff(majors, dat$major_sem_3[i])
    dat$major_sem_4[i] <- sample(available_majors, 1)
  } else {
    # Keep the same major if they did not change
    dat$major_sem_4[i] <- dat$major_sem_3[i]
  }
}

# term gpas
possible_gpas <- seq(2.0, 4.0, by = 0.01)
dat$gpa_sem_1 <- sample(possible_gpas, size = 426, replace = TRUE)
dat$gpa_sem_2 <- sample(possible_gpas, size = 426, replace = TRUE)
dat$gpa_sem_3 <- sample(possible_gpas, size = 426, replace = TRUE)
dat$gpa_sem_4 <- sample(possible_gpas, size = 426, replace = TRUE)