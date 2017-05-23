library(xml2)
library(rvest)
library(jsonlite)

last <- data.frame()

imdb.2016 <- function(page){
  url <- paste('http://www.imdb.com/search/title?count=100&release_date=1970,2016&title_type=feature&page=', page ,'&ref_=adv_nxt',sep="")
  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the title section
  title_data_html <- html_nodes(webpage,'.lister-item-header a')
  
  #Converting the title data to text
  title_data <- html_text(title_data_html)
  
  #Using CSS selectors to scrap the description section
  description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
  
  #Converting the description data to text
  description_data <- html_text(description_data_html)
  
  #Data-Preprocessing: removing '\n'
  description_data<-gsub("\n","",description_data)
  
  #Using CSS selectors to scrap the Movie runtime section
  runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
  
  #Converting the runtime data to text
  runtime_data <- html_text(runtime_data_html)
  
  #Data-Preprocessing: removing mins and converting it to numerical
  runtime_data<-gsub(" min","",runtime_data)
  runtime_data<-as.numeric(runtime_data)
  
  #Using CSS selectors to scrap the Movie genre section
  genre_data_html <- html_nodes(webpage,'.genre')
  
  #Converting the genre data to text
  genre_data <- html_text(genre_data_html)
  
  #Data-Preprocessing: removing \n
  genre_data<-gsub("\n","",genre_data)
  
  #Data-Preprocessing: removing excess spaces
  genre_data<-gsub(" ","",genre_data)
  
  #taking only the first genre of each movie
  genre_data<-gsub(",.*","",genre_data)
  
  #Convering each genre from text to factor
  genre_data<-as.factor(genre_data)
  
  #Using CSS selectors to scrap the IMDB rating section
  rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
  
  #Converting the ratings data to text
  rating_data <- html_text(rating_data_html)
  
  #Data-Preprocessing: converting ratings to numerical
  rating_data<-as.numeric(rating_data)
  
  #Using CSS selectors to scrap the votes section
  votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
  
  #Converting the votes data to text
  votes_data <- html_text(votes_data_html)
  
  #Data-Preprocessing: removing commas
  votes_data<-gsub(",","",votes_data)
  
  #Data-Preprocessing: converting votes to numerical
  votes_data<-as.numeric(votes_data)
  
  #Using CSS selectors to scrap the directors section
  directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
  
  #Converting the directors data to text
  directors_data <- html_text(directors_data_html)
  
  #Data-Preprocessing: converting directors data into factors
  directors_data<-as.factor(directors_data)
  
  #Using CSS selectors to scrap the actors section
  actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
  
  #Converting the gross actors data to text
  actors_data <- html_text(actors_data_html)
  
  #Data-Preprocessing: converting actors data into factors
  actors_data<-as.factor(actors_data)
  
  if (length(title_data) == length(description_data) & length(title_data) == length(runtime_data) & length(title_data) == length(genre_data) & length(title_data) == length(rating_data) & length(title_data) == length(votes_data) & length(title_data) == length(directors_data) & length(title_data) == length(actors_data) ) {
    df = data.frame(title_data, description_data, runtime_data, genre_data, rating_data, votes_data, directors_data, actors_data)
    return(df)
  } else {
    return(FALSE)
  }
}

for (page.number in c(1:10)){
  new.page <- imdb.2016(page.number)
  if(new.page != FALSE) {
    last<-rbind(last, new.page)  
  }
  Sys.sleep(2)
}

#library(rjson)

#exportJson <- toJSON(last)
#write(exportJson, "movie.json")
#write.csv(last, file = "movie.csv")

movie <- toJSON(last)
cat(movie)
#movie <- fromJSON('C:/Users/toshiba/Desktop/movie.json')
x <- toJSON(as.data.frame((last)))
write(x, "movie.json")
