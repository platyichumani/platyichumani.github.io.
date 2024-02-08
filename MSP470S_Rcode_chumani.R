setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages("tidyverse")
library("tidyverse")

load("academic_records_2023-09-23.RData")
load("enrollments_residence_curriculum_2023-09-23.RData")
load("appdat.RData")

gc()

names(acadat)

resdat2 <- resdat %>% 
  mutate(DATE_IN = as.Date(DATE_IN, format = "%d/%B/%y"),
    YEAR = year(DATE_IN),
    STAYED_IN_RES = "Yes") %>% 
  select(c(IDEN, YEAR, STAYED_IN_RES)) %>% 
  distinct()

# First semester subjects only
acajoin <- left_join(x = acadat,
                     y = resdat2,
                     by = join_by(IDEN == IDEN,
                                  YEAR == YEAR)) %>% 
  mutate(STAYED_IN_RES = if_else(is.na(STAYED_IN_RES), "No", "Yes")) %>% 
  filter(YEAR >= 2018,
         PERIOD_OF_STUDY <= 4)


#Replace NA with zero
na.omit(acajoin)

#Convert acajoin to csv file
write.csv(acajoin, file = "acajoin.csv")

acajoin %>% 
  filter(ACADEMIC_BLOCK_CODE %in% c("0", "1")) %>%
  mutate(FirstRegDate = case_when(
    YEAR == 2018 ~ as.Date("2018-01-08"),
    YEAR == 2019 & (TRANSACTION_DATE < "2019-01-14" | 
        PERIOD_OF_STUDY > 1) ~ as.Date("2019-01-07"),
    YEAR == 2019 ~ as.Date("2019-01-14"),
    YEAR == 2020 & (TRANSACTION_DATE < "2020-01-13" | 
                      PERIOD_OF_STUDY > 1) ~ as.Date("2020-01-06"),
    YEAR == 2020 ~ as.Date("2020-01-13"),
    YEAR == 2022 & (TRANSACTION_DATE < "2022-01-31" | 
                      PERIOD_OF_STUDY > 1) ~ as.Date("2022-01-10"),
    YEAR == 2022 ~ as.Date("2022-01-31"),
    YEAR == 2021 & (TRANSACTION_DATE < "2022-03-08" | 
                      PERIOD_OF_STUDY > 1) ~ as.Date("2021-01-11"),
    YEAR == 2021 ~ as.Date("2021-03-08")),
  RegDelayWeeks = interval(start = ymd(FirstRegDate), 
                      end = ymd(TRANSACTION_DATE)) %/% weeks(1),
  RegDelayDays = interval(start = ymd(FirstRegDate), 
                           end = ymd(TRANSACTION_DATE)) %/% days(1)) %>% 
  filter(between(RegDelayDays, 0, 105)) -> 
  acajoin1

#Replace NA with zero
na.omit(acajoin1)

#Convert acajoin1 to csv file
write.csv(acajoin1, file = "acajoin1.csv")

acajoin1 %>% 
  drop_na(PASS_FAIL) %>%
  ggplot(mapping = aes(x = RegDelayWeeks, fill = PASS_FAIL)) + 
  geom_bar(stat = "count", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(fill = "Subject Outcome",
       x = "Delay in Registering (Weeks)",
       y = "Pass Rate (Percent)",
       title = "Subject Pass Rate by Registration Delay, 2018-2022")

# Check for correlation between registration delay and final mark
cor.test(acajoin1$FINAL_MARK, 
         acajoin1$RegDelayDays, na.rm = TRUE)


acajoin %>% 
  drop_na(PASS_FAIL) %>% 
  ggplot(mapping = aes(x = STAYED_IN_RES, fill = PASS_FAIL)) + 
  geom_bar(stat = "count", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Stayed in Residence",
       y = "Pass Rate (%)",
       title = "Subject Pass Rate by Residence Status, 2018-2022")

acajoin.lm <- acajoin1 %>%
  drop_na(PASS_FAIL) %>%
  mutate(PASS_FAIL = factor(PASS_FAIL) %>% relevel(ref = "F"))

mylogistic <- glm(PASS_FAIL ~ STAYED_IN_RES + RegDelayWeeks, 
                  data = acajoin.lm,
                  family = binomial(link = "logit"))

install.packages("broom")
library("broom")
results <- tidy(mylogistic)

exp(results$estimate[2])
# 1.408232
# Odds of passing are 40.8% higher for a student staying in residence 
#  compared to a student not staying in residence

1 / exp(results$estimate[3])
# 1.11731
# odds of passing decrease by 11.7% for every additional week 
# of delay in registering

appdat2 <- appdat %>% 
  drop_na(ENGLISH, `LIFE ORIENTATION`, ENGLISH_TYPE) %>% 
  select(IDEN, ENGLISH, `LIFE ORIENTATION`, ENGLISH_TYPE) %>% 
  arrange(IDEN, desc(ENGLISH), desc(`LIFE ORIENTATION`)) %>%
  distinct(IDEN, .keep_all = TRUE)
  

appjoin <- left_join(x = acadat,
                     y = appdat2,
                     by = join_by(IDEN == IDEN)) %>% 
  filter(YEAR >= 2018, PERIOD_OF_STUDY <= 4)


#Replace NA with zero
na.omit(appjoin)

#Convert appjoin to csv file
write.csv(appjoin, file = "appjoin.csv")

appjoin %>% 
  distinct(IDEN, .keep_all = TRUE) %>%
  ggplot(mapping = aes(x = ENGLISH)) + 
  geom_histogram(colour = "black", fill = "cyan", binwidth = 5) + 
  labs(x = "NSC English Mark",
       y = "Number of Students",
       title = "NSC English Marks of Undergraduate Students, 2018-2022")

appjoin %>% 
  distinct(IDEN, .keep_all = TRUE) %>%
  ggplot(mapping = aes(x = `LIFE ORIENTATION`)) + 
  geom_histogram(colour = "black", fill = "cyan", binwidth = 5) + 
  labs(x = "NSC Life Orientation Mark",
       y = "Number of Students",
       title = "NSC Life Orientation Marks of Undergraduate Students, 2018-2022")

appjoin %>% 
  drop_na(PASS_FAIL, ENGLISH) %>% 
  ggplot(mapping = aes(x = cut(ENGLISH, 
                        breaks = c(0, seq(50, 100, 10)), 
                        include.lowest = TRUE), 
                       fill = PASS_FAIL)) + 
  geom_bar(stat = "count", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "NSC English Mark",
       y = "Pass Rate (%)",
       title = "Subject Pass Rate by NSC English Mark, 2018-2022")

appjoin %>% 
  drop_na(PASS_FAIL, ENGLISH_TYPE) %>% 
  ggplot(mapping = aes(x = ENGLISH_TYPE, 
                       fill = PASS_FAIL)) + 
  geom_bar(stat = "count", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "NSC English Type",
       y = "Pass Rate (%)",
       title = "Subject Pass Rate by NSC English Type, 2018-2022")

appjoin %>% 
  drop_na(PASS_FAIL, `LIFE ORIENTATION`) %>% 
  ggplot(mapping = aes(x = cut(`LIFE ORIENTATION`, 
                               breaks = c(0, seq(40, 100, 10)), 
                               include.lowest = TRUE), 
                       fill = PASS_FAIL)) + 
  geom_bar(stat = "count", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "NSC Life Orientation Mark",
       y = "Pass Rate (%)",
       title = "Subject Pass Rate by NSC Life Orientation Mark, 2018-2022")

appjoin.lm <- appjoin %>%
  drop_na(PASS_FAIL) %>%
  mutate(PASS_FAIL = factor(PASS_FAIL) %>% relevel(ref = "F"))

mylogistic2 <- glm(PASS_FAIL ~ ENGLISH + ENGLISH_TYPE + `LIFE ORIENTATION`, 
                  data = appjoin.lm,
                  family = binomial(link = "logit"))

library(broom)
results2 <- tidy(mylogistic2)

exp(results2$estimate[2])
# 1.012635
# For every additional 1% in NSC English mark,
# odds of passing a CPUT subject are estimated to increase by 1.26%

exp(results2$estimate[4])
# 1.01523
# For every additional 1% in NSC Life Orientation mark,
# odds of passing a CPUT subject are estimated to increase by 1.52%

exp(results2$estimate[3])
# 1.446236
# Odds of passing a CPUT subject are 44.6% higher for a student 
#  who did English Home Language in NSC, than for a student 
#  who did English First Additional Language in NSC