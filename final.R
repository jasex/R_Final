library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

Movie_Data <- read.csv("./movies.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
Rating_Data <- read.csv("./ratings.csv")
#数据探索
str(Movie_Data)
summary(Movie_Data)
summary(Rating_Data)

#生成电影体裁的dataframe
Movie_Type <- as.data.frame(Movie_Data$genres, stringsAsFactors=FALSE)


#Data Exploration
library(data.table)

#处理字符串生成dataframe
Movie_Type2 <- as.data.frame(tstrsplit(Movie_Type[,1], '[|]',type.convert=TRUE),stringsAsFactors=FALSE)
colnames(Movie_Type2) <- c(1:10)
Type_On_List <- c("Action", "Adventure", "Animation", "Children",
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
Type_Mat1 <- matrix(0,9743,18)
Type_Mat1[1,] <- Type_On_List
colnames(Type_Mat1) <- Type_On_List
for (index in 1:nrow(Movie_Type2)) {
    for (col in 1:ncol(Movie_Type2)) {
        gen_col = which(Type_Mat1[1,] == Movie_Type2[index,col])
        Type_Mat1[index+1,gen_col] <- 1
    }
}
#生成一个矩阵包含每部电影对应的体裁
Type_Mat2 <- as.data.frame(Type_Mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(Type_Mat2)) {
    Type_Mat2[,col] <- as.integer(Type_Mat2[,col]) #将字符转换为数字
}
str(Type_Mat2)

#生成search_matrix
SearchMatrix <- cbind(Movie_Data[,1:2], Type_Mat2[])
head(SearchMatrix)    

#dcast:将长数据转换为宽数据
RatingMatrix <- dcast(Rating_Data, userId~movieId, value.var = "rating", na.rm=FALSE)
#将其转换为矩阵
RatingMatrix <- as.matrix(RatingMatrix[,-1]) #去除userIds
#将rating matrix 转换为recommenderlab sparse matrix
RatingMatrix <- as(RatingMatrix, "realRatingMatrix")
RatingMatrix

Recommendation_Model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

names(Recommendation_Model)
lapply(Recommendation_Model,"[[","description")

Recommendation_Model$IBCF_realRatingMatrix$parameters


Similarity_Matrix <- similarity(RatingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
#每行和列代表一个用户
as.matrix(Similarity_Matrix)
#用户相似度矩阵，图形化表示
image(as.matrix(Similarity_Matrix), main = "User's Similarities")

Movie_Simalarity<-similarity(RatingMatrix[,1:4],method="cosine",which="items")
as.matrix(Movie_Simalarity)
#电影相似度矩阵
image(as.matrix(Movie_Simalarity), main = "Movies similarity")

#将其转换为向量
Rating_Values <- as.vector(RatingMatrix@data)
unique(Rating_Values) # 查看独特的Rating_Values

#将其转换为table
Table_of_Ratings <- table(Rating_Values)
Table_of_Ratings

#最多观看的电影
Views_M <- colCounts(RatingMatrix) #获得每部电影观看的次数
#创建data frame
ViewTable <- data.frame(movie=names(Views_M),views=Views_M)
#根据观看次数排序
ViewTable <- ViewTable[order(ViewTable$views,
                                 decreasing = TRUE), ]
#插入电影名
ViewTable$title <- NA
for (index in 1:9724){
    ViewTable[index,3] <- as.character(subset(Movie_Data,
                                                Movie_Data$movieId == ViewTable[index,1])$title)
}
ViewTable[1:6,]

#可视化前6项
ggplot(ViewTable[1:6, ], aes(x = title, y = views)) +
    geom_bar(stat="identity", fill = 'steelblue') +
    geom_text(aes(label=views), vjust=-0.3, size=3.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Total Views of the Top Films")

image(RatingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

#执行预测
movie_ratings <- RatingMatrix[rowCounts(RatingMatrix) > 50,
                              colCounts(RatingMatrix) > 50]
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
      colCounts(movie_ratings) > minimum_users],
main = "Heatmap of the top users and movies")
#每位用户的average rating
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
    ggtitle("Distribution of the average rating")

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
      colCounts(normalized_ratings) > minimum_users],
main = "Normalized Ratings of the Top Users")

#当rating大于3的时候为1,否则为0
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
      colCounts(movie_ratings) > binary_minimum_users],
main = "The top movies and users")

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2)) #将数据分为80%训练集和20%测试集
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

#开始正餐
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)

#探索
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "The first rows and columns")


sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")
#为每个用户推荐十部电影
top_recommendations <- 10
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

#以第一个用户为例子
user1 <- predicted_recommendations@items[[1]]
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
#将数据转换为电影名
# for (index in 1:10){
#   movies_user2[index] <- as.character(subset(Movie_Data,
#                                          Movie_Data$movieId == movies_user1[index])$title)
# }
# movies_user2
#将数据转换为电影名的函数
convert_into_names<-function(movies_user){
    movies_user2<-movies_user
    for(index in 1:10){
        movies_user2[index]<- as.character(subset(Movie_Data,Movie_Data$movieId==movies_user[index])$title)
    }
    return (movies_user2)
}
movies_user2<-convert_into_names(movies_user1)
movies_user2


#生成含所有用户的矩阵
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) })
recommendation_matrix[,1:4]
movies_users<-matrix(data=NA,nrow=nrow(recommendation_matrix),ncol=ncol(recommendation_matrix))
for(index in 1:ncol(recommendation_matrix)){
    movies_users[,index]<-convert_into_names(recommendation_matrix[,index])
}
#查看每个用户的推荐列表
movies_users[,]
#生成词云
library(wordcloud2)
wordcloud2(demoFreq)
signal_user <- data.frame(Movie = movies_users[,1],Freq = 10:1)
wordcloud2(signal_user,size=0.3, color = "random-light", backgroundColor = "grey")

