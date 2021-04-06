# tidy scrape data

library(tidyverse)
library(lubridate)
library(magrittr)

#load data
PRH <- read.csv("reviews_raw_PRH.csv") %>%
  mutate(company = "PRH") %>%
  distinct()

BBC <- read.csv("reviews_raw_BBC.csv") %>%
  mutate(company = "BBC") %>%
  distinct()

Pearson <- read.csv("reviews_raw_Pearson.csv") %>%
  mutate(company = "Pearson") %>%
  distinct()

Netflix <- read.csv("reviews_raw_Netflix.csv") %>%
  mutate(company = "Netflix") %>%
  distinct()

gd <- rbind(PRH, BBC, Pearson, Netflix)


#tidy data
gd %<>%
  separate(review.sub.raw, c("raw1", "raw2", "raw3", "raw4", "raw5", "raw6"),"(?=Career Opportunities|Compensation and Benefits|Culture &amp; Values|Life Balance|Senior Management)") %>%
  mutate(raw2 = ifelse(is.na(raw2), "no data", raw2)) %>% 
  pivot_longer(c(raw2, raw3, raw4, raw5, raw6), values_drop_na = TRUE) %>% 
  mutate(
    name = str_extract(value, ".*(?=<)"),
    value = as.numeric(str_extract(value, "(?<=title=.)\\d"))
  ) %>%  #ggplot(aes(value)) + geom_bar() + facet_wrap(~name)
  pivot_wider(names_from = name, values_from = value) %>% 
  transmute(
    company = company,
    date = dmy(review.date),
    headline = review.sum,
    rating = review.rating,
    career_opportunities = `Career Opportunities`,
    compensation_benefits = `Compensation and Benefits`,
    culture_values = `Culture &amp; Values`,
    work_life_balance = `Life Balance`,
    senior_management = `Senior Management`,
    status = ifelse(str_detect(review.title, "^Current"), "current",
                    ifelse(str_detect(review.title, "^Former"), "former", NA)),
    job_title = ifelse(!str_detect(review.title, "^Current|^Former"), review.title, 
                       ifelse(str_detect(review.title, "Anonymous"), NA,
                              str_extract(review.title, "(?<= - ).*"))),
    job_title = str_replace_all(job_title, "[0-9]+", ""),
    job_title = ifelse(job_title == "", NA, job_title),
    location = ifelse(str_detect(review.location.raw, "authorLocation"), str_extract(review.location.raw, '(?<="authorLocation">).*(?=</span></span></span>)'), NA),
    location_clean = ifelse(str_detect(location, "England$|Ireland$"), "UKI", NA),
    recommend = ifelse(str_detect(review.recommend.raw, "span>Recommends|span>Doesn't Recommend"), str_extract(review.recommend.raw, "(?<=span>)Recommends|(?<=span>)Doesn't Recommend"), NA),
    #outlook = ,
    ceo = ifelse(str_detect(review.recommend.raw, "span>Approves of CEO|span>No Opinion of CEO|span>Disapproves of CEO"), str_extract(review.recommend.raw, "(?<=span>)Approves of CEO|(?<=span>)No Opinion of CEO|(?<=span>)Disapproves of CEO"), NA),
    work_history = review.main,
    pros = review.pros,
    cons = review.cons,
    helpful = str_extract(review.helpful, "[0-9]{1,3}"),
    helpful = ifelse(is.na(helpful), 0, helpful)
  ) 

write.csv(gd, "reviews_clean.csv", row.names = FALSE)



