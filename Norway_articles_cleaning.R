
###################################
# DATA CLEANING
###################################

articles_norway_2016<-read.csv("articles_norway_2016.csv",stringsAsFactors=FALSE) %>% mutate(year=2016)
articles_norway_2015<-read.csv("articles_norway_2015.csv",stringsAsFactors=FALSE) %>% mutate(year=2015)
articles_norway_2014<-read.csv("articles_norway_2014.csv",stringsAsFactors=FALSE) %>% mutate(year=2014)
articles_norway_2013<-read.csv("articles_norway_2013.csv",stringsAsFactors=FALSE) %>% mutate(year=2013)

## Happiness report works with surveys done in 2014-2016. I will keep 2013 as prior year events probably influence 2014 answers 

articles_N_2013_2016<- articles_norway_2016 %>%
  full_join(articles_norway_2015) %>%
  full_join(articles_norway_2014) %>%
  full_join(articles_norway_2013) 
articles_N_2013_2016 %>% write.csv('articles_N_2013_2016.csv', row.names = F)


#Encoding(raw$title) <- "UTF-8"  NOT NEEDED. Iconv leaves some numberes within <> but stripped with gsub later

## Clean merged 2013-2016 articles that mention Romania ##
#articles_RO_2013_2016_cleaned<-read.csv("articles_RO_2013_2016.csv",stringsAsFactors=FALSE)%>%
articles_N_2013_2016_cleaned<- articles_N_2013_2016 %>% 
  mutate(body = iconv(body, "", "ASCII", "byte")) %>%
  #mutate(body = iconv(body, "latin1", "ASCII", sub="")) %>%
  mutate(webTitle = iconv(webTitle, "", "ASCII", "byte")) %>% 
  #  filter(body!="") %>%  did not work
  filter(!grepl("^\\s*$", body)) %>% 
  filter(str_detect(webTitle, 'Norway') | str_detect(webTitle, 'Oslo') | str_detect(webTitle, 'Norwegian') )  ## Norwegian also refers to airline, but very few cases

## get rid of HTML tags in the text body   COULD IT BE DONE FOR TOKENIZATION WITH FORMAT='HTML' ???

library(rvest)  ## thanks @misrori (github) for showing me this amazing method for stripping html tags, as regex failed 

t <- articles_N_2013_2016_cleaned$body[1]  # entire text from body field
#gsub('<aside class=')
#articles_RO_2013_2016$webUrl[12]

get_doc_text <- function(t){
  tryCatch({
    body_text<- 
      read_html(t)%>%
      html_nodes("body p")%>%
      html_text()
    return(paste(body_text, collapse = " " ))
    
  }, error = function(e) {
    return("CheckThisItem")
  })
  
  
}

articles_N_2013_2016_cleaned$text <- lapply(articles_N_2013_2016_cleaned$body,get_doc_text )

#get_doc_text(articles_RO_2013_2016$webUrl[265]) check
## clean some left-over tags
articles_N_2013_2016_cleaned<- articles_N_2013_2016_cleaned  %>%  #  £-s also stripped from text!!
  mutate(text = gsub("<.*?>", "", text))  %>% 
  mutate(webTitle = gsub("<.*?>", "", webTitle))

articles_N_2013_2016_cleaned %>% write.csv('clean_articles/articles_N_2013_2016_cleaned.csv', row.names = F)
