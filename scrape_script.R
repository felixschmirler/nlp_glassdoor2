#Scraping Glassdoor

#source("scrape_function.R")
library(rvest)
library(tidyverse)
library(RSelenium)
library(lubridate)
library(magrittr)


#open session
#shell('docker run -d -p 4445:4444 selenium/standalone-chrome')
#remDr <- remoteDriver(remoteServerAddr = "localhost",
#                                 port = 4445L,
#                                 browserName = "chrome")

shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

Sys.sleep(3)
remDr$open()
Sys.sleep(3)

#login to Glassdoor
remDr$navigate("https://www.glassdoor.co.uk/profile/login_input.htm?userOriginHook=HEADER_SIGNIN_LINK")
remDr$findElement("id", "userEmail")$sendKeysToElement(list("user_email"))
remDr$findElement("id", "userPassword")$sendKeysToElement(list("user_password"))
remDr$findElement("css", "button.gd-ui-button")$clickElement()

#create function to scrape individual glassdoor pages
scrape_gd <- function(current) {
  tibble(
    review.date = read_html(current) %>% 
      html_nodes(".date.subtle.small, .featuredFlag") %>% 
      html_text(),
    review.sum = read_html(current) %>% 
      html_nodes(".summary:not([class*='toggleBodyOff'])") %>% 
      html_text() %>% 
      str_replace_all('\\\"', ""),
    review.rating = read_html(current) %>%
      html_nodes(".gdStars.gdRatings.subRatings__SubRatingsStyles__gdStars") %>%
      html_text() %>%
      str_extract("^."),
    review.location.raw = read_html(current) %>%
      html_nodes(".author.minor") %>%
      as.character(.),
    review.info.raw = read_html(current) %>%
      html_nodes(".authorInfo") %>%
      html_text(),
    review.title = read_html(current) %>%
      html_nodes(".authorJobTitle") %>%
      html_text(),
    review.main = read_html(current) %>%
      html_nodes(".mainText") %>%
      html_text(),
    review.pros = read_html(current) %>%
      html_nodes(".v2__EIReviewDetailsV2__fullWidth") %>%
      html_text() %>% 
      enframe(name = NULL) %>% 
      filter(str_detect(value, "^Pros")) %>%
      transmute(
        `pros` = str_extract(value, "(?<=Pros).*")
      ) %>%
      .[[1]],
    review.cons = read_html(current) %>%
      html_nodes(".v2__EIReviewDetailsV2__fullWidth") %>%
      html_text() %>% 
      enframe(name = NULL) %>% 
      filter(str_detect(value, "^Cons")) %>%
      transmute(
        `cons` = str_extract(value, "(?<=Cons).*")
      ) %>% 
      .[[1]],
    review.helpful = read_html(current) %>%
      html_nodes(".gd-ui-button.css-glrvaa") %>%
      html_text(),
    review.recommend.raw = read_html(current) %>%
      html_nodes(".col-sm-11.pl-sm-lg.mx-0") %>%
      as.character(.), 
    review.sub.raw = read_html(current) %>%
      html_nodes(".gdStars.gdRatings.subRatings__SubRatingsStyles__gdStars") %>%
      as.character(.) 
  ) 
}

#test scrape page 3
#remDr$navigate(paste0("https://www.glassdoor.co.uk/Reviews/Penguin-Random-House-Reviews-E743403_P", "3", ".htm?sort.sortType=RD&sort.ascending=false"))
#remDr$executeScript("window.scrollTo(0, 1080);")
#current <- remDr$getPageSource()[[1]]
#scrape_gd(current) %>% view()
#remDr$getCurrentUrl()
#remDr$screenshot(display = TRUE)

#create an empty data frame to loop through glassdor pages
reviews <- tibble(
  review.date = character(), 
  review.sum = character(), 
  review.rating = character(), 
  review.location.raw = character(),
  review.info.raw = character(),
  review.title = character(), 
  review.main = character(), 
  review.pros = character(), 
  review.cons = character(), 
  review.helpful = character(),
  review.recommend.raw = character(),
  review.sub.raw = character()
  )

#scraping glasdoor pages
company <- readline(prompt="Please enter the name of the company you want to scrape reviews from (PRH, BBC or Pearson): ")
Sys.sleep(2)

if (company == "PRH") {
remDr$navigate(paste0("https://www.glassdoor.co.uk/Reviews/Penguin-Random-House-Reviews-E743403_P", 1, ".htm?sort.sortType=RD&sort.ascending=false"))
} else if (company == "BBC") {
  remDr$navigate(paste0("https://www.glassdoor.co.uk/Reviews/BBC-Reviews-E5847.htm?sort.sortType=RD&sort.ascending=false"))
} else if (company == "Pearson") {
  remDr$navigate(paste0("https://www.glassdoor.co.uk/Reviews/Pearson-Reviews-E3322_P1.htm?sort.sortType=RD&sort.ascending=false"))
} else if (company == "Netflix") {
  remDr$navigate(paste0("https://www.glassdoor.co.uk/Reviews/Netflix-Reviews-E11891.htm?sort.sortType=RD&sort.ascending=false"))
}

Sys.sleep(2)

#loop to scrape all reviews
while (remDr$findElements("css", ".pagination__ArrowStyle__nextArrow") %>% length() > 0) {
  current <- remDr$getPageSource()[[1]]
  reviews <- rbind(reviews, scrape_gd(current))
  remDr$findElement("css", ".pagination__ArrowStyle__nextArrow")$sendKeysToElement(list(key="enter"))
  Sys.sleep(sample(seq(1, 2, by = 0.01), 1))
  if (remDr$findElements("css", ".pagination__ArrowStyle__nextArrow") %>% length() == 0) {
    remDr$refresh()
    Sys.sleep(sample(seq(2, 3, by = 0.01), 1))
  }
}

write.csv(reviews, "reviews_raw.csv")

#close session 
remDr$close()
shell('docker ps')
temp <- enframe(shell('docker ps', intern = TRUE))[[2,2]] #extract the name of the currennt session to stop it in the next line of code
shell(paste('docker stop', word(temp,-1), sep = " "))
shell('docker ps')


#load data
PRH <- read.csv("PRH_reviews_raw.csv") %>%
  mutate(company = "PRH")

BBC <- read.csv("BBC_reviews_raw.csv") %>%
  mutate(company = "BBC")

Pearson <- read.csv("Pearson_reviews_raw.csv") %>%
  mutate(company = "Pearson")

Netflix <- read.csv("Netflix_reviews_raw.csv") %>%
  mutate(company = "Pearson")

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

write.csv(gd, "reviews_clean.csv")



