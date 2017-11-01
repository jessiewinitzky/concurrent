library(tidyverse)

concurrent <- read_csv("concurrent_20171101.csv")

#missing values
sum(is.na(concurrent$PIDM)) #none
sum(is.na(concurrent$BANNER_ID)) #none
sum(is.na(concurrent$GENDER)) #none
sum(is.na(concurrent$ETHNICITY)) #0
sum(is.na(concurrent$HIGH_SCHOOL)) #0
sum(is.na(concurrent$HS_GRAD_YEAR)) #0
sum(is.na(concurrent$ENROLLED_AFTER_HS_GRAD)) #0
sum(is.na(concurrent$AGE_ON_FIRST_DAY)) #0
sum(is.na(concurrent$ADDRESS_ZIP)) #1,858 (4.5%)
sum(is.na(concurrent$TERM_CODE)) #0
sum(is.na(concurrent$CRN))#0
sum(is.na(concurrent$COURSE_SUBJECT)) #0
sum(is.na(concurrent$COURSE_NUMBER)) #0
sum(is.na(concurrent$COURSE_SECTION)) #0

summary(concurrent)
ce_tbl <- tbl_df(concurrent)
ce_tbl
ce <- concurrent
rm(concurrent)
