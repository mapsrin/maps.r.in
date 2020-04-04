library(rvest)
library(tidyverse)

dat <- read.csv("data/weed/StateNames.txt", header = F)
#print(data)
latlng <- read.table("data/weed/latlnglookup.txt", sep="\t", header=F)

#Specifying the url for desired website to be scraped
baseurl <- 'http://www.priceofweed.com/prices/India/'

get_page_data <- function(url) {
  print(url)
  medQAv <- url
  tryCatch({
    pagecontents <- xml2::read_html(medQAv)
    priceList <- pagecontents %>%
      html_nodes(xpath='//*[@id="contentdt"]/table[2]') %>%
      html_table() 
    
    return(priceList[[1]])
    
  }, warning = function(w) {
    return(NA)
  }, error = function(e) {
    return(NA)
  }
  )
}

vals <- c()

for(i in dat$V1){
  #Reading the HTML code from the website
  url <- paste0(baseurl,i,".html")
  vals <- rbind(vals,get_page_data(url))
}

valsbak <- vals
vals[2] <- data.frame(lapply(vals[2], function(x) {as.numeric(gsub("\\$", "", x))}))

vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Trivandrum", "Thiruvananthapuram", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Calcutta", "Kolkata", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Ahmadabad", "Ahmedabad", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Dehli", "New Delhi", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Delhi", "New Delhi", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Haora", "Howrah", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("New New Delhi", "New Delhi", x)}))
vals[1] <- data.frame(lapply(vals[1], function(x) {gsub("Madras", "Chennai", x)}))

vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("an ounce", "28.35", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("a gram", "1", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("5 grams", "5", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("10 grams", "10", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("a half ounce", "14.18", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("a quarter", "7.09", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("25 grams", "25", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("an eighth", "3.54", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("20 grams", "20", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {gsub("15 grams", "15", x)}))
vals[3] <- data.frame(lapply(vals[3], function(x) {as.numeric(as.character(x))}))
vals <- transform(vals, "Per gram" = X2 / X3)
vals <- vals[-c(1),-c(2,3,5)]
vals <- vals %>% filter(vals[2] != "<NA>")


valshighQ <- vals %>% filter( vals[2] == "high quality")
valshighQUniq <- valshighQ %>% group_by(X1) %>% summarize(avg=mean(Per.gram))
valshighQUniq["lat"] <- lapply(valshighQUniq[1], function(x) latlng[match(x, latlng$V1),2])
valshighQUniq["lng"] <- lapply(valshighQUniq[1], function(x) latlng[match(x, latlng$V1),3])

valsmedQ <- vals %>% filter( vals[2] == "medium quality")
valsmedQUniq <- valsmedQ %>% group_by(X1) %>% summarize(avg=mean(Per.gram))
valsmedQUniq["lat"] <- lapply(valsmedQUniq[1], function(x) latlng[match(x, latlng$V1),2])
valsmedQUniq["lng"] <- lapply(valsmedQUniq[1], function(x) latlng[match(x, latlng$V1),3])

valsQAgnos <- vals %>% group_by(X1) %>% summarize(avg=mean(Per.gram))
valsQAgnos["lat"] <- lapply(valsQAgnos[1], function(x) latlng[match(x, latlng$V1),2])
valsQAgnos["lng"] <- lapply(valsQAgnos[1], function(x) latlng[match(x, latlng$V1),3])


