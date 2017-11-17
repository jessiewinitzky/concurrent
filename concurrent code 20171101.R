library(tidyverse)
library(rmarkdown)

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
  
####model: does taking more classes make attending slcc more likely?####
mod <- glm(slcc ~ courses, data = enrolled_table, family = binomial(link = "logit"))
summary(mod)

#Call:
#  glm(formula = slcc ~ courses, data = enrolled_table)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7732   0.2268   0.2273   0.2283   0.2374  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.7737094  0.0059419 130.213   <2e-16 ***
#  courses     -0.0005056  0.0012756  -0.396    0.692    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1760866)

#Null deviance: 2074.2  on 11780  degrees of freedom
#Residual deviance: 2074.1  on 11779  degrees of freedom
#AIC: 12976

#Number of Fisher Scoring iterations: 2

#### add covariates
ce_summarize <- ce_gr_join %>% group_by(PIDM) %>%
  summarize(num_courses = max(num_courses), Snum = max(BANNER_ID), gender = max(GENDER), ethnicity = max(ETHNICITY), HS = max(HIGH_SCHOOL),
            grade_num = mean(grade_num), gradyear = max(HS_GRAD_YEAR), enrollafter = max(ENROLLED_AFTER_HS_GRAD), 
            enrollafter_num = max(enrollafter_num),age = min(AGE_ON_FIRST_DAY), zip = max(ADDRESS_ZIP), min_term = min(TERM_CODE), max_term = max(TERM_CODE))

mod_cov <- glm(enrollafter_num ~ num_courses + grade_num + age + gradyear + gender + ethnicity, 
               data = ce_summarize, family = binomial(link = "logit"))
summary(mod_cov)

#create female variable
ce_summarize <- ce_summarize %>% mutate(female = ifelse(gender == "Female", 1, 0))

#create hispanic & white variables
ce_summarize <- ce_summarize %>% mutate(white = ifelse(ethnicity == "White", 1, 0))
ce_summarize <- ce_summarize %>% mutate(hispanic = ifelse(ethnicity == "Hispanic", 1, 0))

mod_cov2 <- glm(enrollafter_num ~ num_courses + grade_num + age + gradyear + female + hispanic + white,
                data = ce_summarize, family = binomial(link = "logit"))
summary(mod_cov2)

#Call:
#  glm(formula = enrollafter_num ~ num_courses + grade_num + age + 
#        gradyear + female + hispanic + white, family = binomial(link = "logit"), 
#      data = ce_summarize)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1651   0.5708   0.6924   0.7481   0.9372  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 222.628233  30.460222   7.309 2.70e-13 ***
#  num_courses  -0.015503   0.008513  -1.821   0.0686 .  
#grade_num     0.044371   0.021527   2.061   0.0393 *  
#  age           0.039905   0.032806   1.216   0.2238    
#gradyear     -0.110349   0.015127  -7.295 2.99e-13 ***
#  female       -0.013787   0.045232  -0.305   0.7605    
#hispanic      0.466620   0.084074   5.550 2.86e-08 ***
#  white         0.069520   0.067712   1.027   0.3046    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 12515  on 11621  degrees of freedom
#Residual deviance: 12416  on 11614  degrees of freedom
#(159 observations deleted due to missingness)
#AIC: 12432
#
#Number of Fisher Scoring iterations: 4

#need data viz of % of students who went to slcc at each number of courses
#Count
ce_summarize %>% group_by(enrollafter, num_courses) %>%
  summarize(count = n()) %>%
  ggplot(aes(enrollafter, count)) +
  geom_col(fill = "#00A8E1") +
  facet_wrap(~num_courses) +
  theme_minimal() +
  labs(x = "Enrolled at SLCC after taking concurrent?", y = "Count")

#Percent
percent_table_courses <- ce_summarize %>%
  group_by(num_courses, enrollafter)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

percent_table_courses %>%
  ggplot(aes(enrollafter, freq)) +
  geom_col(fill = "#00A8E1") +
  facet_wrap(~num_courses) +
  theme_minimal() +
  labs(x = "Enrolled at SLCC after taking concurrent?", y = "Percent") +
  geom_text(aes(label = n), vjust = -1)

#collapse num_courses >15
ce_summarize <- ce_summarize %>% 
  mutate(num_courses_coll = ifelse(num_courses > 15, 16, num_courses))

percent_courses_coll <- ce_summarize %>%
  group_by(num_courses_coll, enrollafter)%>%
  summarize(n = n(), num_courses = min(num_courses)) %>%
  mutate(freq = n/sum(n)) 

percent_courses_coll$num_courses <- as.integer(percent_courses_coll$num_courses)

chart_pccourses <- percent_courses_coll %>% 
  arrange(num_courses) %>%
  ggplot(aes(enrollafter, freq)) +
  geom_col(fill = "#00A8E1") +
  facet_wrap(~num_courses_coll) +
  theme_minimal() +
  labs(x = "Enrolled at SLCC after taking concurrent?", y = "Proportion", 
       title = "Proportion of concurrent students who enrolled at SLCC,\nby total number of concurrent courses taken",
       subtitle = "Number of courses taken was not associated with likelihood of enrolling, \nthough those who took more than 12 courses were less likely to enroll. \nThis may be because they had completed an Associate's degree by high school graduation.",
       caption = "*The figure above each bar represents the count of students in that category") +
  geom_text(aes(label = n), vjust = -.5) +
  ylim(0, 1)

####joining clearinghouse data####
rm(cech)
ce_tojoin <- ce_gr_join %>%
  group_by(PIDM) %>%
  summarize(num_ce_courses = max(num_courses), max_ce_term = max(TERM_CODE), HS_GRAD_YEAR = max(HS_GRAD_YEAR), 
            ce_gpa = mean(grade_num), enrollafter = max(enrollafter_num))

cech <- left_join(ch, ce_tojoin, by = "PIDM")

####which institutions?####
cech_colleges <- cech %>% group_by(PIDM) %>%
  summarize(college = max(`College Name`), n = n()) %>%
  group_by(college) %>%
  summarize(n = n())

cech_colleges2 <- cech %>% group_by(PIDM, `College Name`) %>% summarize(n()) %>%
  group_by(`College Name`) %>% summarize(n())


####filter out ce enrollments from clearinghouse####
slcc_cech <- cech %>% filter(`College Name` == "SALT LAKE COMMUNITY COLLEGE")
slcc_cech_begindate <- slcc_cech %>% group_by(`Enrollment Begin`) %>% summarize(n = n() )
slcc_cech <- slcc_cech %>% mutate(term_code = ifelse(`Enrollment Begin` > 20120501 & `Enrollment Begin` < 20120731, "201230", 0))

#create term code from begin date
slcc_cech <- slcc_cech %>% mutate(term_code = ifelse(`Enrollment Begin` %in% 20120501:20120731, 201230,
                                              ifelse(`Enrollment Begin` %in% 20120801:20121231, 201240,
                                              ifelse(`Enrollment Begin` %in% 20130101:20130430, 201320,
                                              ifelse(`Enrollment Begin` %in% 20130501:20130731, 201330,
                                              ifelse(`Enrollment Begin` %in% 20130801:20131231, 201340,
                                              ifelse(`Enrollment Begin` %in% 20140101:20140430, 201420,
                                              ifelse(`Enrollment Begin` %in% 20140501:20140731, 201430,
                                              ifelse(`Enrollment Begin` %in% 20140801:20141231, 201440,
                                              ifelse(`Enrollment Begin` %in% 20150101:20150430, 201520,
                                              ifelse(`Enrollment Begin` %in% 20150501:20150731, 201530,
                                              ifelse(`Enrollment Begin` %in% 20150801:20151231, 201540,
                                              ifelse(`Enrollment Begin` %in% 20160101:20160430, 201620,
                                              ifelse(`Enrollment Begin` %in% 20160501:20160731, 201630,
                                              ifelse(`Enrollment Begin` %in% 20160801:20161231, 201640,
                                              ifelse(`Enrollment Begin` %in% 20170101:20170430, 201720,
                                              ifelse(`Enrollment Begin` %in% 20170501:20170731, 201730,
                                              ifelse(`Enrollment Begin` %in% 20170801:20171231, 201740,
                                                     NA))))))))))))))))))
slcc_cech %>% group_by(term_code) %>% summarize(n = n())

#concurrent student = if max ce term code is >= term code
slcc_cech_no_ce <- slcc_cech %>%
  filter(term_code > max_ce_term)

#concurrent enrollment rows found in cech dataset
slcc_cech_only_ce <- slcc_cech %>%
  filter(term_code <= max_ce_term)

slcc_cech_only_ce %>% summarise(n())
slcc_cech_only_ce <- slcc_cech_only_ce %>% rename(term_code = TERM_CODE)

#anti join to slcc_cech
#this dataset contains every college semester for every student who attended slcc
slcc_cech_noce <- anti_join(slcc_cech, slcc_cech_only_ce, by = c("PIDM", "term_code"))

#anti join slcc_noce to cech
#first create term code variable for cech
cech <- cech %>% mutate(term_code = ifelse(`Enrollment Begin` %in% 20120501:20120731, 201230,
                                    ifelse(`Enrollment Begin` %in% 20120801:20121231, 201240,
                                    ifelse(`Enrollment Begin` %in% 20130101:20130430, 201320,
                                    ifelse(`Enrollment Begin` %in% 20130501:20130731, 201330,
                                    ifelse(`Enrollment Begin` %in% 20130801:20131231, 201340,
                                    ifelse(`Enrollment Begin` %in% 20140101:20140430, 201420,
                                    ifelse(`Enrollment Begin` %in% 20140501:20140731, 201430,
                                    ifelse(`Enrollment Begin` %in% 20140801:20141231, 201440,
                                    ifelse(`Enrollment Begin` %in% 20150101:20150430, 201520,
                                    ifelse(`Enrollment Begin` %in% 20150501:20150731, 201530,
                                    ifelse(`Enrollment Begin` %in% 20150801:20151231, 201540,
                                    ifelse(`Enrollment Begin` %in% 20160101:20160430, 201620,
                                    ifelse(`Enrollment Begin` %in% 20160501:20160731, 201630,
                                    ifelse(`Enrollment Begin` %in% 20160801:20161231, 201640,
                                    ifelse(`Enrollment Begin` %in% 20170101:20170430, 201720,
                                    ifelse(`Enrollment Begin` %in% 20170501:20170731, 201730,
                                    ifelse(`Enrollment Begin` %in% 20170801:20171231, 201740,
                                           NA))))))))))))))))))
#then anti join slcc noce to cech
cech_noce <- anti_join(cech, slcc_cech_only_ce, by = c("PIDM", "term_code"))

####which colleges after high school?####

#first college attended after ce
#locate first class by term_code
first_coll <- cech_noce %>%
  group_by(PIDM) %>% 
  arrange(term_code) %>%
  slice(1) %>%
  ungroup

first_coll_sum <- first_coll %>% mutate(`College Name` = ifelse(is.na(`College Name`), "NO COLLEGE", `College Name`)) %>%
  group_by(`College Name`) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(desc(count)) %>%
  slice(1:10)

#reorder so most frequent colleges appear first
first_coll_sum$`College Name` <- factor(first_coll_sum$`College Name`, level = first_coll_sum$`College Name`[order(first_coll_sum$count)])

#plot of first institution attended after HS graduation
chart_firstcoll <- first_coll_sum %>%
  ggplot(aes(`College Name`, percent)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  labs(title = "First institution attended after high school graduation",
       subtitle = "For more than 60% of concurrent students, SLCC was the first college they attended after high school.",
       x = "Institution", y = "Percent",
       caption = "*The figure next to each bar represents the count of students who attended that institution") +
  geom_text(aes(label = count), hjust = -0.25) +
  ylim(0, 100)

#ever attended slcc after graduation
college_ever <- cech_noce %>%
  group_by(PIDM, `College Name`) %>%
  summarize(count = 1) %>%
  group_by(`College Name`) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  mutate(freq = count/11235*100) %>%
  slice(1:10) %>%
  mutate(`College Name` = ifelse(is.na(`College Name`), "NO COLLEGE", `College Name`))

#reorder so most frequent colleges appear first
college_ever$`College Name` <- factor(college_ever$`College Name`, level = college_ever$`College Name`[order(college_ever$count)])

#plot
college_ever %>%
  ggplot(aes(`College Name`, freq))+
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Institutions ever attended after high school graduation",
       subtitle = "Nearly 80% of concurrent students attended SLCC after graduating from high school.",
       x = "Institution", y = "Percent",
       caption = "*The figure next to each bar represents the count of students who attended that institution") +
  geom_text(aes(label = count), hjust = -0.25) +
  ylim(0, 100)


####what ce courses do students take?####
chart_commoncourses <- ce_gr_join %>% group_by(class) %>%
  summarize(count=n(), average_grade = mean(grade_num, na.rm=TRUE)) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(class, count), count)) +
  geom_col(fill = "#00A8E1") +
  coord_flip() +
  labs(title = "Most frequent concurrent courses taken",
       x = "Course",
       y = "Count",
       subtitle = "Math and English are the most popular concurrent classes") +
  geom_text(aes(label = count), hjust = -.25) +
  ylim(0,4000) +
  theme_minimal()
#MA 1100 = Medical Terminology
#FIN 1050 = Personal Finance
#FHS 2400 = Marriage and family relations

first_coll_sum$`College Name` <- factor(first_coll_sum$`College Name`, level = first_coll_sum$`College Name`[order(first_coll_sum$count)])

#students who retake courses: 187 of them
ce_gr_join %>% group_by(PIDM, class) %>%
  summarise(count = n(),
            mean_grade = mean(grade_num, na.rm = TRUE), 
            min_grade = min(grade_num, na.rm = TRUE), 
            max_grade = max(grade_num, na.rm = TRUE),
            firstterm = min(TERM_CODE),
            lastterm = max(TERM_CODE)) %>%
  filter(count > 1) %>%
  arrange(desc(count))
#41,483 students overall; 187 retook a concurrent course
# this is 0.04% of students. I'm not going to worry about them.

#most common subject?