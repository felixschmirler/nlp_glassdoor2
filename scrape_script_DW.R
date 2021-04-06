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
#LoadCredentials
user <- read_file("User.txt")
pw <- read_file("PW.txt")

#login to Glassdoor
remDr$navigate("https://www.glassdoor.co.uk/profile/login_input.htm?userOriginHook=HEADER_SIGNIN_LINK")
remDr$screenshot(display=TRUE)
remDr$findElement("id", "userEmail")$sendKeysToElement(list(user))
remDr$findElement("id", "userPassword")$sendKeysToElement(list(pw))
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
#remDr$navigate(paste0("https://www.glassdoor.co.uk/Reviews/Netflix-Reviews-E11891.htm?sort.sortType=RD&sort.ascending=false"))
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
company <- readline(prompt="Please enter the name of the company you want to scrape reviews from (PRH, BBC or Pearson, Netflix): ")
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
while (remDr$findElements("css", ".nextButton") %>% length() > 0) {
  current <- remDr$getPageSource()[[1]]
  reviews <- rbind(reviews, scrape_gd(current))
  remDr$findElement("css", ".nextButton")$sendKeysToElement(list(key="enter"))
  Sys.sleep(sample(seq(1, 2, by = 0.01), 1))
  if (remDr$findElements("css", ".nextButton") %>% length() == 0) {
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


