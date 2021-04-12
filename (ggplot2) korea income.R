library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(readxl)

korea <- read_csv("Korea Income and Welfare.csv")
job_code <- read_excel("job_code_translated.xlsx")

korea <- left_join(korea, job_code, by = c('occupation' = 'job_code'))

str(korea)
summary(korea)
glimpse(korea)

# Missing value

sort(colSums(is.na(korea)), decreasing = TRUE)

korea <- rename(korea, job_group = "...3")

korea <- korea %>%
  filter(!is.na(occupation))

miss <- korea %>%
  select(job_group, occupation, job_title) %>%
  filter(is.na(job_group))

# Job Missing value
korea[korea$occupation %in% c(111,112),]$job_group <- "Senior public and corporate positions"
korea[korea$occupation %in% c(132,133,134,135,139),]$job_group <- "Professional service manager"
korea[korea$occupation %in% c(141,149),]$job_group <- "Construction, electricity and production-related managerial positions"
korea[korea$occupation %in% c(151,152,153,159),]$job_group <- "Sales and customer service management positions"
korea[korea$occupation %in% c(211,212,213),]$job_group <- "Scientific experts and related positions"
korea[korea$occupation %in% c(221,222,223,224),]$job_group <- "Information and communication specialists and technical positions"
korea[korea$occupation %in% c(231,232,233,234,235,236,237,239),]$job_group <- "Engineering professionals and technical positions"
korea[korea$occupation %in% c(241,242,243,244,245,246,247,248),]$job_group <- "Health, social welfare and religious jobs"
korea[korea$occupation %in% c(251,252,253,254,259),]$job_group <- "Education professionals and related positions"
korea[korea$occupation %in% c(261,262),]$job_group <- "Legal and administrative professions"
korea[korea$occupation %in% c(271,272,273,274),]$job_group <- "Management and financial experts and related positions"
korea[korea$occupation %in% c(281,282,283,284,285,286,289),]$job_group <- "Culture, arts and sports experts and related positions"
korea[korea$occupation %in% c(311,312,313,314),]$job_group <- "Office jobs related to management and accounting"
korea[korea$occupation %in% c(391,392,399),]$job_group <- "Consultation, guidance, statistics and other office jobs"
korea[korea$occupation %in% c(411,412),]$job_group <- "Police, fire and security related service positions"                                                                                                                                        
korea[korea$occupation %in% c(421,422,423,429),]$job_group <- "Cosmetology, wedding and medical assistant services"
korea[korea$occupation %in% c(431,432),]$job_group <- "Transportation and leisure service jobs"
korea[korea$occupation %in% c(441,442),]$job_group <- "Cooking and Food Service Jobs"
korea[korea$occupation %in% c(521,522),]$job_group <- "Store salesperson"
korea[korea$occupation %in% c(611,612,613),]$job_group <- "Agricultural and livestock skilled workers"
korea[korea$occupation %in% c(721,722),]$job_group <- "Textile, clothing and leather related craftsmen"
korea[korea$occupation %in% c(741,742,743),]$job_group <- "Skills related to wood, furniture, musical instruments and signboards"
korea[korea$occupation %in% c(751,752,753,761,762),]$job_group <- "Transportation and machinery related trades"
korea[korea$occupation %in% c(771,772,773,774),]$job_group <- "Construction and mining related craftsmen"
korea[korea$occupation %in% c(791,792,799),]$job_group <- "Other Skills Related Jobs"
korea[korea$occupation %in% c(811,812,819),]$job_group<- "Food processing related machine operator"
korea[korea$occupation %in% c(821,822,823),]$job_group <- "Textile and footwear related machinery workers"
korea[korea$occupation %in% c(831,832),]$job_group <- "Chemical-related machine operator"
korea[korea$occupation %in% c(841,842,843),]$job_group <- "Metal and non-metal related machine operators"
korea[korea$occupation %in% c(851,852,853,854,855),]$job_group <- "Machine manufacturing and related machine operators"
korea[korea$occupation %in% c(861,862,863,864),]$job_group <- "Electrical and electronic related mechanical engineering jobs"
korea[korea$occupation %in% c(871,872,873,874,875,876),]$job_group <- "Driving and transport related jobs"
korea[korea$occupation %in% c(881,882),]$job_group <- "Machine operator related to water supply, sewage and recycling treatment"
korea[korea$occupation %in% c(891,892,899),]$job_group <- "Wood, printing and other machine-manipulation"
korea[korea$occupation %in% c(921,922),]$job_group <- "Transportation related simple labor"
korea[korea$occupation %in% c(941,942),]$job_group <- "Simple labor jobs related to cleaning and security"
korea[korea$occupation %in% c(951,952,953),]$job_group <- "Simple labor jobs related to housework, food and sales"
korea[korea$occupation %in% c(991,992,999),]$job_group <- "Agriculture, forestry and fishing and other services simple labor"
korea[korea$occupation %in% c(1011,1012),]$job_group <- "soldier"

korea <- korea %>%
  filter(!is.na(job_group))

# income 2008
korea08 <- korea %>%
  filter(income > 0 &  income < 40000 & year == 2008)

ggplot(data = korea08[korea08$income < 35000,], aes(x = income)) +
  geom_histogram(bins = 30, fill = "Tomato") +
  scale_x_continuous(breaks = seq(0,40000,10000)) +
  geom_vline(aes(xintercept = mean(income, na.rm = T)), linetype = 'dashed', color = "Red") +
  geom_text(aes(x = 30000, y = 100, label = "Average = 3908.4"), size = 7) +
  labs(title = "Distribution of Korean's Income 2008 ",
       x = "Income")

ggplot(data = korea08[korea08$income < 36000,], aes(x = income)) +
  geom_histogram(bins = 30, fill = "sky blue") +
  geom_vline(aes(xintercept = mean(income[gender == 1], na.rm = T)), linetype = 'dashed', color = "Dark Blue") +
  scale_x_continuous(breaks = seq(0,40000,10000)) +
  scale_fill_discrete(name = element_text("Gender")) +
  labs(title = "Distribution of Man's Income 2008",
       x = "Income") +
  geom_text(aes(x = 30000, y = 100, label = "Average = 4,304,8"), size = 7) +
  theme(legend.position = c(0.9,0.9))

ggplot(data = korea08[korea08$income < 36000,], aes(x = income)) +
  geom_histogram(bins = 30, fill = "Light Pink") +
  geom_vline(aes(xintercept = mean(income[gender == 2], na.rm = T)), linetype = 'dashed', color = "Deep Pink") +
  scale_x_continuous(breaks = seq(0,40000,10000)) +
  scale_fill_discrete(name = element_text("Gender")) +
  labs(title = "Distribution of Woman's Income 2008",
       x = "Income") +
  geom_text(aes(x = 30000, y = 100, label = "Average = 2089,9"), size = 7) +
  theme(legend.position = c(0.9,0.9))

subset08 <- korea08 %>%
  group_by(job_group) %>%
  summarise(mean.income = mean(income)) %>%
  mutate(top10 = ifelse(mean.income > 6353., "top10", "others")) %>%
  arrange(-mean.income)
  
test08 <- korea08 %>%
  group_by(job_group) %>%
  mutate(mean.income = mean(income)) %>%
  arrange(-mean.income)

ggplot(data = subset08[1:10,], aes(x = reorder(job_group,mean.income), y = mean.income)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Income by job",
       x = "Job", y = "Income") +
  geom_text(aes(x= 10, y = 15500, label = "# 1"), size = 5, col = "White") +
  geom_text(aes(x= 9, y = 5900, label = "# 2"), size = 5, col = "White") +
  geom_text(aes(x= 8, y = 5500, label = "# 3"), size = 5, col = "White") +
  geom_text(aes(x= 7, y = 5500, label = "# 4"), size = 5, col = "White") +
  geom_text(aes(x= 6, y = 5400, label = "# 5"), size = 5, col = "White") +
  geom_text(aes(x= 5, y = 5300, label = "# 6"), size = 5, col = "White") +
  geom_text(aes(x= 4, y = 5150, label = "# 7"), size = 5, col = "White") +
  geom_text(aes(x= 3, y = 5100, label = "# 8"), size = 5, col = "White") +
  geom_text(aes(x= 2, y = 5100, label = "# 9"), size = 5, col = "White") +
  geom_text(aes(x= 1, y = 4800, label = "# 10"), size = 5, col = "White") +
  coord_flip()

ggplot(data = test08[1:259,], aes(x = reorder(job, mean.income), y = mean.income, fill = factor(-gender))) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Income by Job (Count by Job)",
       x = "Job", y = "Sum of Income") +
  scale_fill_discrete(name = "Gender") +
  theme(legend.position = "none") +
  coord_flip()

ggplot(data = subset08[40:50,], aes(x = reorder(job_group,mean.income), y = mean.income)) +
  geom_bar(stat = "identity") +
  labs(title = "Bottom 10 Income by job",
       x = "Job", y = "Income") +
  coord_flip()

ggplot(data = test08[2426:4030,], aes(x = reorder(job_group, mean.income), y = mean.income, fill = factor(-gender))) +
  geom_bar(stat = "identity") +
  labs(title = "Bottom 10 Income by Job (Count by Job)",
       x = "Job", y = "Sum of Income") +
  scale_fill_discrete(name = "Gender") +
  theme(legend.position = "none") +
  coord_flip()

ggplot(data = korea08, aes(x = ))

 
# income 18, 

korea18 <- korea %>%
  filter(income > 0 & income < 67000 & year == 2018)

ggplot(data = korea18[korea18$income < 67000,], aes(x = income)) +
  geom_histogram(bins = 30, fill = "Tomato") +
  scale_x_continuous(breaks = seq(0,70000,10000)) +
  labs(title = "Distribution of Korean's Income 2018 ",
       x = "Income")

ggplot(data = korea18[korea18$income < 67000,], aes(x = income)) +
  geom_histogram(bins = 30, fill = "sky blue") +
  geom_vline(aes(xintercept = mean(income[gender == 1], na.rm = T)), linetype = 'dashed', color = "Dark Blue") +
  scale_x_continuous(breaks = seq(0,70000,10000)) +
  scale_fill_discrete(name = element_text("Gender")) +
  labs(title = "Distribution of Man's Income 2018",
       x = "Income") +
  geom_text(aes(x = 30000, y = 100, label = "Average = 6470.8"), size = 7) +
  theme(legend.position = c(0.9,0.9))

ggplot(data = korea08[korea08$income < 67000,], aes(x = income)) +
  geom_histogram(bins = 30, fill = "Light Pink") +
  geom_vline(aes(xintercept = mean(income[gender == 2], na.rm = T)), linetype = 'dashed', color = "Deep Pink") +
  scale_x_continuous(breaks = seq(0,70000,10000)) +
  scale_fill_discrete(name = element_text("Gender")) +
  labs(title = "Distribution of Woman's Income 2008",
       x = "Income") +
  geom_text(aes(x = 30000, y = 100, label = "Average = 2943,9"), size = 7) +
  theme(legend.position = c(0.9,0.9))

subset18 <- korea18 %>%
  group_by(job_group) %>%
  summarise(mean.income = mean(income)) %>%
  arrange(-mean.income)

test18 <- korea18 %>%
  group_by(job_group) %>%
  mutate(mean.income = mean(income)) %>%
  arrange(-mean.income)

ggplot(data = subset18[1:10,], aes(x = reorder(job,mean.income), y = mean.income)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Income by Job Group",
       x = "Job", y = "Income") +
  geom_text(aes(x= 10, y = 10000, label = "# 1"), size = 5, col = "White") +
  geom_text(aes(x= 9, y = 9000, label = "# 2"), size = 5, col = "White") +
  geom_text(aes(x= 8, y = 8800, label = "# 3"), size = 5, col = "White") +
  geom_text(aes(x= 7, y = 8500, label = "# 4"), size = 5, col = "White") +
  geom_text(aes(x= 6, y = 8400, label = "# 5"), size = 5, col = "White") +
  geom_text(aes(x= 5, y = 7900, label = "# 6"), size = 5, col = "White") +
  geom_text(aes(x= 4, y = 7800, label = "# 7"), size = 5, col = "White") +
  geom_text(aes(x= 3, y = 7500, label = "# 8"), size = 5, col = "White") +
  geom_text(aes(x= 2, y = 7300, label = "# 9"), size = 5, col = "White") +
  geom_text(aes(x= 1, y = 6800, label = "# 10"), size = 5, col = "White") +
  coord_flip()

ggplot(data = test18[1:355,], aes(x = reorder(job, mean.income), y = mean.income, fill = factor(-gender))) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Income by Job (Count by Job)",
       x = "Job", y = "Sum of Income") +
  scale_fill_discrete(name = "Gender") +
  theme(legend.position = "none") +
  coord_flip()

ggplot(data = subset18[37:46,], aes(x = reorder(job,mean.income), y = mean.income)) +
  geom_bar(stat = "identity") +
  labs(title = "Bottom 10 Income by Job Group",
       x = "Job", y = "Income") +
  geom_text(aes(x= 10, y = 4400, label = "# 1"), size = 5, col = "White") +
  geom_text(aes(x= 9, y = 4300, label = "# 2"), size = 5, col = "White") +
  geom_text(aes(x= 8, y = 4200, label = "# 3"), size = 5, col = "White") +
  geom_text(aes(x= 7, y = 3600, label = "# 4"), size = 5, col = "White") +
  geom_text(aes(x= 6, y = 3550, label = "# 5"), size = 5, col = "White") +
  geom_text(aes(x= 5, y = 2500, label = "# 6"), size = 5, col = "White") +
  geom_text(aes(x= 4, y = 2400, label = "# 7"), size = 5, col = "White") +
  geom_text(aes(x= 3, y = 2300, label = "# 8"), size = 5, col = "White") +
  geom_text(aes(x= 2, y = 2100, label = "# 9"), size = 5, col = "White") +
  geom_text(aes(x= 1, y = 1700, label = "# 10"), size = 5, col = "White") +
  coord_flip()

ggplot(data = test18[2341:3885,], aes(x = reorder(job, mean.income), y = mean.income, fill = factor(-gender))) +
  geom_bar(stat = "identity") +
  labs(title = "Bottom 10 Income by Job (Count by Job)",
       x = "Job", y = "Sum of Income") +
  scale_fill_discrete(name = "Gender") +
  theme(legend.position = "none") +
  coord_flip()

proportion18 <- subset18 %>%
  group_by(top10) %>%
  summarise(total = sum(mean.income))

pie18 <- ggplot(data = proportion18, aes(x = "", y = total, fill = top10)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = round(100*(total / sum(total)),3)), position = position_stack(vjust = 0.5), size = 5) +
  geom_text(aes(label = round(total, 3)), position = position_stack(vjust = 0.40), size = 5) +
  scale_y_continuous(breaks = seq(0,300000,50000)) +
  theme_void() +
  labs(title = "Piechart of Sum of Income")

pie18 + scale_fill_manual(values=c("#999999", "#E69F00"))

subset18 <- subset18 %>%
  mutate(top10 = ifelse(mean.income > 8170, "top10", "others"),
         p = mean.income / sum(mean.income))

100 * prop.table(subset18$mean.income)

subset18 %>%
  summarise(sum = sum(mean.income))
