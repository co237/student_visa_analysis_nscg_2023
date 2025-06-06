# Exploring the labor market outcomes of former student visa holders

## Load Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(matrixStats)

## Load data

nscg_23 <- read.csv("pcg23Public/epcg23.csv")

## Add new variables

nscg_23 <- nscg_23 %>%
  mutate(First_visa_type = case_when(
    FNVSATP ==1 ~ "Green card",
    FNVSATP ==2 ~ "Work visa",
    FNVSATP ==3 ~ "Student visa",
    FNVSATP ==4 ~ "Dependent visa",
    FNVSATP==5 ~ "Other temp",
    FNVSATP=="E" ~ "Error",
    FNVSATP=="L" ~ "Native",
    FNVSATP=="M" ~ "Missing"),
    MRDegType = case_when(
      MRDG ==1 ~ "BA",
      MRDG == 2 ~ "MA",
      MRDG == 3 ~ "PhD",
      MRDG == 4 ~ "Prof"),
    Current_ctzn_status = case_when(
      CTZN ==1 ~ "native",
      CTZN ==2 ~ "naturalized",
      CTZN ==3 ~ "green card",
      CTZN ==4 ~ "temporary"),
    Entrepreneur = ifelse(EMTP==12 | EMTP==13, "Entrepreneur", "Not an entrepreneur")
  )

nscg_23 <- nscg_23 %>%
  mutate(Industry_group = case_when(
    INDCODE>0 & INDCODE<491 ~ "Agriculture",
    INDCODE>1069 & INDCODE<3991 ~ "Manufacturing",
    INDCODE >4069 & INDCODE <4591 ~ "Wholesale Trade",
    INDCODE >4669 & INDCODE <5792 ~ "Retail Trade",
    INDCODE> 6069 & INDCODE <6391 ~ "Trans/WH, Util",
    INDCODE>6470 & INDCODE <6782 ~ "Information",
    INDCODE >6870 & INDCODE<7191 ~ "FIRE",
    INDCODE >7269 & INDCODE <7991 ~ "Prof services",
    INDCODE >7859 & INDCODE<8471 ~ "Ed and HC",
    INDCODE >8560 & INDCODE<8691 ~ "Accom & Food",
    INDCODE >8769 & INDCODE <9291 ~ "Other Services",
    INDCODE >9369 & INDCODE <9591 ~ "Public Admin",
    INDCODE >9669 & INDCODE <9891 ~ "Military",
    INDCODE > 9892 ~ "NA/Missing"
  ))

## Analysis

### Headline salary comparison

nscg_23 %>%
  filter(First_visa_type=="Student visa" | First_visa_type=="Native",
         # Filter out non-missing salaries
         SALARY!= 9999998,
         # Filter for working 50+ weeks/year
         WKSLYR >49,
         # Filter for working 35 hours/week
         HRSWK>34) %>%
  group_by(First_visa_type) %>%
  summarise(Median_salary = weightedMedian(SALARY, w = WTSURVY, na.rm = TRUE))


### Salary by age bucket (decade)

student_visa_comp_age <- nscg_23 %>%
  filter(SALARY!= 9999998,
         First_visa_type=="Student visa" | First_visa_type=="Native",
         WKSLYR >49,
         HRSWK>34) %>%
  mutate(Age_bucket = case_when(
    AGE < 30               ~ "20s",
    AGE >= 30 & AGE < 40   ~ "30s",
    AGE >= 40 & AGE < 50   ~ "40s",
    AGE >= 50 & AGE < 60   ~ "50s",
    AGE >= 60              ~ "60+"))%>%
  group_by(Age_bucket, First_visa_type) %>%
  summarise(Median_salary = weightedMedian(SALARY, w = WTSURVY, na.rm = TRUE)) %>%
  pivot_wider(names_from = First_visa_type, values_from = Median_salary)

### Salary by degree level

student_visa_comp_deglvl <- nscg_23 %>%
  filter(SALARY!= 9999998,
         First_visa_type=="Student visa" | First_visa_type=="Native",
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(First_visa_type, MRDegType) %>%
  summarise(Median_salary = weightedMedian(SALARY, w = WTSURVY, na.rm = TRUE)) %>%
  pivot_wider(names_from = MRDegType, values_from = Median_salary)
  
## Degree level frequency

nscg_23 %>%
  filter(First_visa_type == "Student visa"| First_visa_type=="Native",
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(MRDegType, First_visa_type) %>%
  summarise(total = sum(WTSURVY)) %>%
  pivot_wider(names_from = First_visa_type, values_from = total) %>%
  ungroup() %>%
  mutate(nativeshare = Native/sum(Native)*100,
         Visa_share = `Student visa`/sum(`Student visa`)*100) %>%
  select(-Native, -`Student visa`)


### Salary by industry

student_visa_comp_industry<- nscg_23 %>%
  filter(SALARY!= 9999998,
         First_visa_type=="Student visa" | First_visa_type=="Native",
         Industry_group != "NA/Missing",
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(First_visa_type, Industry_group)%>%
  summarise(Median_salary = weightedMedian(SALARY, w = WTSURVY, na.rm = TRUE)) %>%
  #Filter out groups w/ estimated < 50,000 international grads
  filter(!Industry_group %in% c("Agriculture","Military","Wholesale Trade")) %>%
  pivot_wider(names_from = Industry_group, values_from = Median_salary)
  
### Salary by country of birth (top ten largest senders)

student_visa_comp_country <- nscg_23 %>%
  filter(SALARY!= 9999998,
         First_visa_type=="Student visa",
         BTHST_TOGA>99,
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(BTHST_TOGA) %>%
  summarise(Median_salary = weightedMedian(SALARY, w = WTSURVY, na.rm = TRUE),
            total_grads = sum(WTSURVY)) %>%
  arrange(desc(total_grads)) %>%
  slice_head(n = 10)

## Compare research rate at principal jobs

student_visa_comp_research <- nscg_23 %>%
  filter(First_visa_type=="Student visa" | First_visa_type=="Native",
         WKSLYR >49,
         HRSWK>34,
         ACTRES != "L") %>%
  group_by(First_visa_type, ACTRES) %>%
  summarise(total = sum(WTSURVY)) %>%
  pivot_wider(names_from = ACTRES, values_from = total) %>%
  mutate(Reserch_rate = Y/(Y+N))

### Entrepreneur status

#### By first visa status 

nscg_23 %>%
  filter(First_visa_type == "Student visa"| First_visa_type=="Native",
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(First_visa_type, Entrepreneur)%>%
  summarize(People = sum(WTSURVY)) %>%
  pivot_wider(names_from = Entrepreneur, values_from = People) %>%
  mutate(Entrepreneur_rate = Entrepreneur/`Not an entrepreneur`) %>%
  ggplot(aes(x = First_visa_type, y = Entrepreneur_rate)) + 
  geom_col() +
  scale_y_continuous(labels = percent_format()) +  
  geom_text(aes(label = percent(Entrepreneur_rate)),  
            vjust = -0.5, size = 5) +
  coord_cartesian(ylim = c(0, .25))+
  labs(y = "Entrepreneur rate", 
       x = "Visa status",
       title = "Self-employment rate by first visa type") +
  theme_minimal()

#### By first and current visa status 

nscg_23 %>%
  filter(First_visa_type == "Student visa"| First_visa_type=="Native",
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(First_visa_type, Entrepreneur, Current_ctzn_status) %>%
  summarize(People = sum(WTSURVY)) %>%
  pivot_wider(names_from = Entrepreneur, values_from = People) %>%
  mutate(Entrepreneur_rate = Entrepreneur/`Not an entrepreneur`) %>%
  filter((First_visa_type=="Native" & Current_ctzn_status=="native") | (First_visa_type=="Student visa" & Current_ctzn_status !="native")) %>%
  mutate(status = paste(First_visa_type, Current_ctzn_status, sep = ", ")) %>%
  ggplot(aes(x = status, y = Entrepreneur_rate)) + 
  geom_col() +
  scale_y_continuous(labels = percent_format()) +  
  geom_text(aes(label = percent(Entrepreneur_rate)),  
            vjust = -0.5, size = 5) +
  coord_cartesian(ylim = c(0, .25))+
  labs(y = "Entrepreneur rate", 
       x = "Visa status",
       title = "Self-employment rate by first and current visa type") +
  theme_minimal()
  
### Current status of student visa arrivals

nscg_23 %>%
  filter(First_visa_type =="Student visa",
         WKSLYR >49,
         HRSWK>34) %>%
  group_by(Current_ctzn_status) %>%
  summarise(total = sum (WTSURVY))





