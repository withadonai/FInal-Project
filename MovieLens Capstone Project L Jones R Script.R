#Load the dataset 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(devtools)


#Identify the number of columns and rows in the edx data

ncol(edx)
nrow(edx)

#Look at a summary of the edx data 

summary(edx)

#Class identification for the edx data

class(edx)

#Review edx structure

str(edx)

#Identify the number of columns and rows in the validation data

ncol(validation)
nrow(validation)

#Look at a summary of the validation data 

summary(validation)


#Class identification for the validation data

class(validation)


#Review validation structure

str(validation)



library(dplyr)
length(edx$movieId)


#Identify the top 15 movies

top_15movies<-edx%>%group_by(title)%>%summarize(count=n())%>%top_n(15,count)%>%arrange(desc(count))
top_15movies

#plot Top 15 Movies

plottop15movies<-table(edx$title)
barplot(plottop15movies[order(plottop15movies,decreasing = TRUE)])


#Plot the Top 15 Movies by Ratings

top_15movies%>%ggplot(aes(x=reorder(title, count),y=count))+geom_bar(stat='identity',fill="orange")+coord_flip(y=c(0, 35000))+
  labs(x="", y="# of Ratings \n Figure 2")+geom_text(aes(label= count),hjust=-0.1, size=3)+labs(title="Top 15 Movies Titles")



edx %>% group_by(genres) %>%summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%filter(n >= 100000) 


top_15movies_by_genres<-edx%>%group_by(title,genres)%>%summarize(count=n())%>%top_n(15,count)%>%arrange(desc(count))
top_15movies_by_genres


#plot the movies by rating

hist(edx$rating,main="Movies by Ratings",xlab="Ratings\n Figure 3",col="yellow")


#plot users by rating

edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 10, color = "blue") +
  scale_x_log10() +
  ylab("# of Users") + 
  xlab("# of Ratings \n Figure 4") +
  ggtitle("# of Ratings by Users") 


hist(edx$movieId,main="Movies by MovieID",xlab="Ratings\n Figure 5",col="yellow")


#calculate baseline RMSE 

mu<-mean(edx$rating)
baseline_rmse <-RMSE(validation$rating, mu)
rmse_results <- data_frame(method = "Model - Average Movie Ratings", RMSE = baseline_rmse)
rmse_results

#Gererate the Movie Effect Method (MEM) by calculating the estimated deviation by calculating the averge (mean) of all movies (edx$rating) on the training set and then calculating the predicted rating. Note: "mu" already calculated above

avg_of_movie_ratings<-edx%>%group_by(movieId)%>%summarize(b_i=mean(rating-mu))
pred_movie_ratings_model1<-mu+validation%>%left_join(avg_of_movie_ratings,by="movieId")%>%.$b_i
mem_rmse<-RMSE(validation$rating,pred_movie_ratings_model1)
mem_rmse


#Gererate the Movie and User Effect Method (MUEM) by calculating the estimated deviation by calculating the averge (mean) of all moves(edx$rating) on the training set and then calculating the predicted rating. Note: "mu" already calculated above

avg_of_users<-edx%>%left_join(avg_of_movie_ratings,by="movieId")%>%group_by(userId)%>%summarize(b_u=mean(rating-mu-b_i))
pred_movie_ratings_model2<-validation%>%left_join(avg_of_movie_ratings,by="movieId")%>%left_join(avg_of_users,by="userId")%>%mutate(pred=mu+b_i+b_u)%>%.$pred
muem_rmse<-RMSE(validation$rating,pred_movie_ratings_model2)
muem_rmse


#Assess model complexity and minimize overfitting with regularization

rmse<-function(actual_rating,rating_prediction){sqrt(mean((actual_rating-rating_prediction)^2))}
lambdas<-seq(0,15,.25)

rmses<-sapply(lambdas,function(l){
  mu<-mean(edx$rating)
  b_i<-edx %>%group_by(movieId)%>%summarize(b_i=sum(rating-mu)/(n()+l))
  b_u<-edx %>%left_join(b_i,by="movieId")%>%group_by(userId) %>%summarize(b_u=sum(rating-b_i-mu)/(n()+l))
  prediction<-validation%>%left_join(b_i,by="movieId")%>%left_join(b_u,by="userId")%>%mutate(pred=mu+b_i+b_u)%>%pull(pred)
  return(RMSE(prediction,validation$rating))
})



qplot(lambdas,rmses)


lambda<-lambdas[which.min(rmses)]
lambda



rmse_finalresult<-min(rmses)

