library(RSocrata)
library(tidyverse)
library(readr)

#https://data.virginia.gov/Public-Safety/Community-Policing-Data-July-1-2020-to-June-30-202/2c96-texw

# access data with SODA api - takes a LONG time 3M rows 
#df <- read.socrata(
#  "https://data.virginia.gov/resource/2c96-texw.json"
#)

# OR download csv 
df <- read_csv("~/Desktop/Community_Policing_Data_July_1__2020_to_June_30__2023.csv") %>%
  janitor::clean_names()

codes <- readxl::read_excel("~/Desktop/codes.xlsx") %>%
  janitor::clean_names() %>%
  select(statute, heading, subhead, descrpt1, descrpt2, viewkey, sentence)

# dates - 1280 NA's
df %>% filter(is.na(stop_date)) %>%
  with(table(agency_name)) %>%
  sort() # Rockingham (600), Scott County (300), and Eastville (200) have lots of NA dates 

clean <- df %>%
  filter(!is.na(stop_date)) %>%
  mutate(stop_date = mdy(stop_date))

clean %>%
  ggplot(aes(stop_date)) +
  geom_histogram()

summary(clean$stop_date) # earliest stop is 1930 and latest is 2027

clean <- clean %>%
  filter(stop_date >= as.Date("2020-07-01") & stop_date <= as.Date("2023-06-30"))

# ages - 47 NAs
clean %>%
  ggplot(aes(age)) +
  geom_histogram() # distribution is right skewed

clean %>% filter(is.na(age)) %>%
  with(table(agency_name)) %>%
  sort() # no specific locations are especially high with NAs (> 6)

youth <- clean %>% filter(age < 18) # 122,738 under 18

baby <- clean %>% filter(age == 0) # 62,162 under 1

teen <- clean %>% filter(age > 0 & age < 18)

baby %>% 
  with(table(agency_name)) %>%
  sort() # every county arrests babies 

baby %>%
  with(table(reason_for_stop)) # babies get mostly traffic & equipment violations

clean %>% with(table(reason_for_stop))

# let's just drop all underage arrests for now: 
clean <- clean %>% filter(age >= 18)


# jurisdiction - cville

table(clean$jurisdiction) %>% sort() #6 missing and 9 unknown

cville <- clean %>% 
  filter(jurisdiction == "CHARLOTTESVILLE")

cville %>% with(table(age))

cville %>%
  ggplot(aes(age)) +
  geom_histogram() #right skewed with a peak around 20/21 years old 

cville %>%
  ggplot(aes(stop_date)) +
  geom_histogram() #left skewed with peaks around May/June

cville %>% with(table(agency_name)) #CPD with a few UVA PD

cville %>% with(table(reason_for_stop)) #mostly traffic violations?

cville_join <- left_join(cville, codes, by = c("specific_violation" = "statute"))

cville_join %>% with(table(heading))







         