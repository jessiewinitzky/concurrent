library(tidyverse)

concurrent <- read_csv("concurrent_20171101.csv")
concurrent2 <- read_csv("20171110_concurrent.csv")
clearinghouse <- read_csv("20171113 clearinghouse data.csv")

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

sum(is.na(concurrent2$PIDM)) #none
sum(is.na(concurrent2$BANNER_ID)) #none
sum(is.na(concurrent2$GENDER)) #none
sum(is.na(concurrent2$ETHNICITY)) #0
sum(is.na(concurrent2$HIGH_SCHOOL)) #0
sum(is.na(concurrent2$HS_GRAD_YEAR)) #0
sum(is.na(concurrent2$ENROLLED_AFTER_HS_GRAD)) #0
sum(is.na(concurrent2$AGE_ON_FIRST_DAY)) #0
sum(is.na(concurrent2$ADDRESS_ZIP)) #1,858 (4.5%)
sum(is.na(concurrent2$TERM_CODE)) #0
sum(is.na(concurrent2$CRN))#0
sum(is.na(concurrent2$COURSE_SUBJECT)) #0
sum(is.na(concurrent2$COURSE_NUMBER)) #0
sum(is.na(concurrent2$COURSE_SECTION)) #0
sum(is.na(concurrent2$FINAL_GRADE)) #0

summary(concurrent)
ce_tbl <- tbl_df(concurrent)
ce_tbl
ce <- concurrent
rm(concurrent)

summary(concurrent2)
ce_tbl2 <- tbl_df(concurrent2)
ce_tbl
ce2 <- concurrent2
rm(concurrent2)

summary(clearinghouse)
ch <- tbl_df(clearinghouse)
glimpse(ch)

######THE 5 VERBS#####
#select -- removes columns -- works with variables
#mutate -- create new variables -- works with variables
#filter -- removes rows -- works with observations
#arrange -- reorders rows -- works with observations
#summarise -- calculates summary statistics -- works with groups

glimpse(ce_tbl)

####TO-DO####
#summarize: collapse student observations & count how many courses students take, along with whether they enrolled at SLCC
#mutate: new variable combining course subject and course number, summarize, whether they enrolled at SLCC
#mutate: create new variable numerizing grade
#join clearinhouse data

#unique observations
n_distinct(ce_tbl$PIDM)
#11781 distinct studens
#41671/11781 = 3.54, so students took an average of 3.5 concurrent courses. That seems like a lot!

#paste course subject and course number together
ce_tbl$class <- paste(ce_tbl$COURSE_SUBJECT, ce_tbl$COURSE_NUMBER)

ce_grouped <- ce_tbl %>%
  group_by(PIDM) %>%
  summarise(num_courses = n())

ce_grouped %>% summarize(min(num_courses), max(num_courses))
course_count <- ce_grouped %>% group_by(num_courses) %>% summarise(student_count = n())

ce_gr_join <- left_join(ce_grouped, ce_tbl, by = "PIDM")

ggplot(course_count, aes(num_courses, student_count)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  labs(x = "Number of courses", y = "Student count") +
  theme_minimal()

ggplot(course_count, aes(num_courses, student_count)) +
  geom_histogram(fill = "#00A8E1") +
  labs(x = "Number of courses", y = "Student count") +
  theme_minimal()

###join data####
ch$PIDM <- ch$`Requester Return Field`
cech <- left_join(ce_tbl, ch, by = "PIDM")

####new variable numerating grade####
ce_tbl <- ce_tbl %>% mutate(grade_num = ifelse(FINAL_GRADE == "A", 4, ifelse(FINAL_GRADE == "A-", 3.7, ifelse(FINAL_GRADE == "B+", 3.3, 
                              ifelse(FINAL_GRADE == "B", 3, ifelse(FINAL_GRADE == "B-", 2.7, ifelse(FINAL_GRADE == "C+", 2.3,
                              ifelse(FINAL_GRADE == "C", 2, ifelse(FINAL_GRADE == "C-", 1.7, ifelse(FINAL_GRADE == "D+", 1.3,
                              ifelse(FINAL_GRADE == "D", 1, ifelse(FINAL_GRADE == "D-", .7, ifelse(FINAL_GRADE == "E", 0,
                              ifelse(FINAL_GRADE == "W", 0, ifelse(FINAL_GRADE == "I", 0, NA)))))))))))))))
ce_tbl$grade_cum <- NULL

ce_gr_join <- ce_gr_join %>% mutate(grade_num = ifelse(FINAL_GRADE == "A", 4, ifelse(FINAL_GRADE == "A-", 3.7, ifelse(FINAL_GRADE == "B+", 3.3, 
                                        ifelse(FINAL_GRADE == "B", 3, ifelse(FINAL_GRADE == "B-", 2.7, ifelse(FINAL_GRADE == "C+", 2.3,
                                        ifelse(FINAL_GRADE == "C", 2, ifelse(FINAL_GRADE == "C-", 1.7, ifelse(FINAL_GRADE == "D+", 1.3,
                                        ifelse(FINAL_GRADE == "D", 1, ifelse(FINAL_GRADE == "D-", .7, ifelse(FINAL_GRADE == "E", 0,
                                        ifelse(FINAL_GRADE == "W", 0, ifelse(FINAL_GRADE == "I", 0, NA)))))))))))))))

cech <- cech %>% mutate(grade_num = ifelse(FINAL_GRADE == "A", 4, ifelse(FINAL_GRADE == "A-", 3.7, ifelse(FINAL_GRADE == "B+", 3.3, 
                                                ifelse(FINAL_GRADE == "B", 3, ifelse(FINAL_GRADE == "B-", 2.7, ifelse(FINAL_GRADE == "C+", 2.3,
                                                ifelse(FINAL_GRADE == "C", 2, ifelse(FINAL_GRADE == "C-", 1.7, ifelse(FINAL_GRADE == "D+", 1.3,
                                                ifelse(FINAL_GRADE == "D", 1, ifelse(FINAL_GRADE == "D-", .7, ifelse(FINAL_GRADE == "E", 0,
                                                ifelse(FINAL_GRADE == "W", 0, ifelse(FINAL_GRADE == "I", 0, NA)))))))))))))))

#new variable: numerating enroll after high school grad
ce_gr_join <- ce_gr_join %>% mutate(enrollafter_num = ifelse(ENROLLED_AFTER_HS_GRAD == "Y", 1,0))
ce_gr_join_summer <- ce_gr_join %>% filter(TERM_CODE == 201230)
#LEFT OFF HERE                                           TERM_CODE == 201330 & TERM_CODE == 201430 & TERM_CODE == 201530 & TERM_CODE == 201630)

#group by s number
ce_gr_join %>% group_by(TERM_CODE, PIDM) %>%
  summarize(n())

#group by semester, then PIDM: gives the number of courses taken per semester by each student
#filter term > 201140 because we don't have full data for earlier semesters
bysem_bypidm <- ce_gr_join %>% group_by(TERM_CODE, PIDM) %>%
  filter(TERM_CODE > 201140) %>%
  summarize(num_courses_per_sem = n(), enroll_after = max(enrollafter_num))
bysem_bypidm %>% summarize(mean(num_courses_per_sem))
bysem_bypidm %>% summarize(max(num_courses_per_sem))

#group by pidm: gives the total number of courses taken
by_pidm <- ce_gr_join %>%
  group_by(PIDM) %>%
  summarize(total_courses = n(), enroll_after = max(enrollafter_num))
by_pidm %>% summarise(mean(total_courses))
by_pidm %>% summarise(max(total_courses))

enrolled_table1 <- ce_gr_join %>% group_by(PIDM) %>%
#  filter(TERM_CODE > 201140) %>%
  summarize(courses = max(num_courses), slcc = max(enrollafter_num))

enrolled_table2 <- ce_gr_join %>% group_by(PIDM) %>%
  filter(TERM_CODE > 201140) %>%
  summarize(courses = max(num_courses), slcc = max(enrollafter_num))

enrolled_table1 %>% summarize(mean(courses))
enrolled_table2 %>% summarize(mean(courses))

enrolled_table %>% group_by(slcc) %>% 
  summarize(mean(courses))
enrolled_table <- enrolled_table %>% mutate(slcc_yn = ifelse(slcc == 1, "Y", "N"))

###How many take concurrent overall?####
enrolled_table %>% ggplot(aes(slcc, courses)) +
  geom_point()
enroll_summary <- enrolled_table %>% group_by(courses) %>%
  summarize(mean(slcc))

barchart1 <- by_pidm %>% ggplot(aes(total_courses)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Distribution of number of total number of concurrent courses taken", 
       x = "Total number of concurrent courses taken", y = "Count",
       subtitle = "The majority of concurrent students took 1-2 classes")
barchart1

densityplot1 <- by_pidm %>% ggplot(aes(total_courses)) +
  geom_density(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Density plot of total number of concurrent courses taken", x = "Total number of concurrent courses taken", y = "Density",
       subtitle = "More than half of all concurrent students took 1-2 classes overall")
densityplot1

###How many per term?####
#group by term code
bysem_bypidm$TERM_CODE <- as.factor(bysem_bypidm$TERM_CODE)

glimpse(bysem_bypidm)

by_semester <- by_semester %>% mutate(course_year = ifelse(TERM_CODE == 201220, 2012, ifelse(TERM_CODE == 201230, 2012, ifelse(TERM_CODE == 201240, 2012,
                                      ifelse(TERM_CODE == 201320, 2013, ifelse(TERM_CODE == 201330, 2013, ifelse(TERM_CODE == 201340, 2013,
                                      ifelse(TERM_CODE == 201420, 2014, ifelse(TERM_CODE == 201430, 2014, ifelse(TERM_CODE == 201440, 2014,
                                      ifelse(TERM_CODE == 201520, 2015, ifelse(TERM_CODE == 201530, 2015, ifelse(TERM_CODE == 201540, 2015,
                                      ifelse(TERM_CODE == 201620, 2016, ifelse(TERM_CODE == 201630, 2016, ifelse(TERM_CODE == 201640, 2016,
                                                                                                                 2017))))))))))))))))
by_semester <- by_semester %>% mutate(course_sem = ifelse(TERM_CODE == 201220, "Spring", ifelse(TERM_CODE == 201230, "Summer", ifelse(TERM_CODE == 201240, "Fall",
                                                    ifelse(TERM_CODE == 201320, "Spring", ifelse(TERM_CODE == 201330, "Summer", ifelse(TERM_CODE == 201340, "Fall",
                                                    ifelse(TERM_CODE == 201420, "Spring", ifelse(TERM_CODE == 201430, "Summer", ifelse(TERM_CODE == 201440, "Fall",
                                                    ifelse(TERM_CODE == 201520, "Spring", ifelse(TERM_CODE == 201530, "Summer", ifelse(TERM_CODE == 201540, "Fall",
                                                    ifelse(TERM_CODE == 201620, "Spring", ifelse(TERM_CODE == 201630, "Summer", ifelse(TERM_CODE == 201640, "Fall",
                                                    "Spring"))))))))))))))))

by_semester <- by_semester %>% mutate(course_year_sem = paste(course_sem, course_year))

barchart2 <- by_semester %>% arrange(TERM_CODE) %>%
  ggplot(aes(course_year, count)) +
  geom_bar(fill = "#00A8E1", stat = "identity") +
  theme_minimal() +
  labs(title = "Number of concurrent students enrolled by semester", x = "Year", y = "Count",
       subtitle = "Concurrent enrollments appear to be declining,\n and are least common during summer semester") +
  facet_wrap(~course_sem) +
  geom_text(aes(label = count, vjust = -1
                #, size = 3
                ))
#don't know why size = 3 isn't shrinking the size??? It worked earlier!
barchart2

####Average course load per semester####
bysem_bypidm <- bysem_bypidm %>% mutate(course_year = ifelse(TERM_CODE == 201220, 2012, ifelse(TERM_CODE == 201230, 2012, ifelse(TERM_CODE == 201240, 2012,
                                                      ifelse(TERM_CODE == 201320, 2013, ifelse(TERM_CODE == 201330, 2013, ifelse(TERM_CODE == 201340, 2013,
                                                      ifelse(TERM_CODE == 201420, 2014, ifelse(TERM_CODE == 201430, 2014, ifelse(TERM_CODE == 201440, 2014,
                                                      ifelse(TERM_CODE == 201520, 2015, ifelse(TERM_CODE == 201530, 2015, ifelse(TERM_CODE == 201540, 2015,
                                                      ifelse(TERM_CODE == 201620, 2016, ifelse(TERM_CODE == 201630, 2016, ifelse(TERM_CODE == 201640, 2016,
                                                                                                                                                                                                                                                                                                                                                                                                                                             2017))))))))))))))))
bysem_bypidm <- bysem_bypidm %>% mutate(course_sem = ifelse(TERM_CODE == 201220, "Spring", ifelse(TERM_CODE == 201230, "Summer", ifelse(TERM_CODE == 201240, "Fall",
                                                     ifelse(TERM_CODE == 201320, "Spring", ifelse(TERM_CODE == 201330, "Summer", ifelse(TERM_CODE == 201340, "Fall",
                                                     ifelse(TERM_CODE == 201420, "Spring", ifelse(TERM_CODE == 201430, "Summer", ifelse(TERM_CODE == 201440, "Fall",
                                                     ifelse(TERM_CODE == 201520, "Spring", ifelse(TERM_CODE == 201530, "Summer", ifelse(TERM_CODE == 201540, "Fall",
                                                     ifelse(TERM_CODE == 201620, "Spring", ifelse(TERM_CODE == 201630, "Summer", ifelse(TERM_CODE == 201640, "Fall",
                                                     "Spring"))))))))))))))))

meancourse_persem <- bysem_bypidm %>% 
  ungroup() %>%
  group_by(TERM_CODE, course_year, course_sem) %>%
  summarize(mean_courses = mean(num_courses_per_sem))

barchart3 <- meancourse_persem %>% 
  arrange(TERM_CODE) %>%
  ggplot(aes(course_year, round(mean_courses, 1))) +
  geom_bar(fill = "#00A8E1", stat = "identity") +
  theme_minimal() +
  labs(title = "Average number of concurrent courses taken per semester", x = "Year", y = "Average number of courses taken",
       subtitle = "In the fall and spring, concurrent students take, on average, 1-2 courses. \n Students enrolled in the summer tend to take more.") +
  facet_wrap(~course_sem)+
  geom_text(aes(label = round(mean_courses, 1)), vjust = -1)
#                , size = 3
#don't know why size = 3 isn't shrinking the size??? It worked earlier!
barchart3
chart_persem <- barchart3

###this boxplot is not helpful
bysem_bypidm$course_year <- as.factor(bysem_bypidm$course_year)
boxplot2 <- bysem_bypidm %>% 
  group_by(TERM_CODE) %>%
  arrange(TERM_CODE) %>%
  ggplot(aes(x = course_year, y = num_courses_per_sem)) +
  geom_boxplot()
#  theme_minimal() +
#  labs(title = "Average number of concurrent courses taken per semester", x = "Year", y = "Average number of courses taken",
#       subtitle = "In the fall and spring, concurrent students take, on average, 1-2 courses. \n Students enrolled in the summer tend to take more.") +
#  facet_wrap(~course_sem)
#                , size = 3
#don't know why size = 3 isn't shrinking the size??? It worked earlier!

####Median course load per semester####
barchart4 <- by_semester %>% arrange(TERM_CODE) %>%
  ggplot(aes(course_year, median_courses)) +
  geom_bar(fill = "#00A8E1", stat = "identity") +
  theme_minimal() +
  labs(title = "Median number of concurrent courses taken per semester", x = "Year", y = "Count",
       subtitle = "In the fall and spring, concurrent students take, on average, 4-6 courses. \n Students enrolled in the summer take significantly more.") +
  facet_wrap(~course_sem) +
  geom_text(aes(label = round(median_courses,1), vjust = -1
                #                , size = 3
  ))
#don't know why size = 3 isn't shrinking the size??? It worked earlier!

####Concurrent & likelihood to attend SLCC####
#overall
enrolled_table <- enrolled_table %>% mutate(slcc_yn = ifelse(slcc == 1, "Yes", "No"))
boxplot1 <- enrolled_table %>% ggplot(aes(slcc_yn, courses)) +
  geom_boxplot(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Relationship between concurrent course-taking\n and enrolling at SLCC as a college student", 
       x = "Attended SLCC after HS graduation?", y = "Number of concurrent courses taken", 
       subtitle = "Students who took more concurrent classes were more likely to enroll at SLCC")
boxplot_enrollafter <- boxplot1
chart_countpersem <- barchart2
chart_numcoursespersem <- chart_persem

enrolled_table %>%
  group_by(slcc_yn) %>%
  summarise(mean_courses = mean(courses), med_courses = median(courses))

#how many ce students attended after graduating?
chart_enrollcount <- enrolled_table %>%
  group_by(slcc_yn) %>%
  summarise(count = n()) %>%
  ggplot(aes(slcc_yn, count)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after taking concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Count", 
       subtitle = "The majority of students attended SLCC after graduating high school") +
  geom_text(aes(label = count, vjust = -1))

percent_table <- enrolled_table %>%
  group_by(slcc_yn)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

#what percent of ce students attended SLCC after graduating?
chart_enrollpercent <- percent_table %>%
  ggplot(aes(slcc_yn, freq)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after taking concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "The majority of students attended SLCC after graduating high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1)
  
#per term