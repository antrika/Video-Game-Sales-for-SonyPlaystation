library(forecast) 
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(rpart) 
library(rpart.plot)
library(lattice)
library(caret)
library(e1071)
library(randomForest)
library(FNN)
library(tidyr)
library(ranger)

#Reading the Data
Games.df <- read.csv("ViGaSa.csv")


#Collecting Sony Data
unique(Games.df$Platform)
keep <- c("PSP","PSV","PS","PS1","PS2","PS3","PS4")
Games.df <- Games.df[Games.df$Platform %in% keep, ]

#Checking for Null values and removing them
sum(Games.df$Name=="N/A")
sum(Games.df$Year_of_Release=="N/A")
sum(Games.df$Genre=="N/A")
sum(Games.df$Publisher=="N/A")
sum(Games.df$User_Score=="N/A")
sum(Games.df$Developer == "N/A")
sum(Games.df$Rating == "N/A")

sum(Games.df$Name=="NA")
sum(Games.df$Year_of_Release=="NA")
sum(Games.df$Genre=="NA")
sum(Games.df$Publisher=="NA")
sum(Games.df$User_Score=="NA")
sum(Games.df$Developer == "NA")
sum(Games.df$Rating == "NA")

sum(Games.df$Name=="")
sum(Games.df$Year_of_Release=="")
sum(Games.df$Genre=="")
sum(Games.df$Publisher=="")
sum(Games.df$User_Score=="")
sum(Games.df$Developer == "")
sum(Games.df$Rating == "")

#Removing Null Values 
Games.df <- Games.df[Games.df$Name != "N/A", ]
Games.df <- Games.df[Games.df$Year_of_Release != "N/A", ]
Games.df <- Games.df[Games.df$Genre != "N/A", ]
Games.df <- Games.df[Games.df$Publisher != "N/A", ]
Games.df <- Games.df[Games.df$User_Score != "N/A", ]
Games.df <- Games.df[Games.df$Developer != "N/A", ]
Games.df <- Games.df[Games.df$Rating != "N/A", ]

Games.df <- Games.df[Games.df$Name != "NA", ]
Games.df <- Games.df[Games.df$Year_of_Release != "NA", ]
Games.df <- Games.df[Games.df$Genre != "NA", ]
Games.df <- Games.df[Games.df$Publisher != "NA", ]
Games.df <- Games.df[Games.df$User_Score != "NA", ]
Games.df <- Games.df[Games.df$Developer != "NA", ]
Games.df <- Games.df[Games.df$Rating != "NA", ]

Games.df <- Games.df[Games.df$Name != "", ]
Games.df <- Games.df[Games.df$Year_of_Release != "", ]
Games.df <- Games.df[Games.df$Genre != "", ]
Games.df <- Games.df[Games.df$Publisher != "", ]
Games.df <- Games.df[Games.df$User_Score != "", ]
Games.df <- Games.df[Games.df$Developer != "", ]
Games.df <- Games.df[Games.df$Rating != "", ]

Games.df <- Games.df[Games.df$Critic_Score != "NA", ]
Games.df <- Games.df[Games.df$Critic_Count != "NA", ]

Games.df <- Games.df[complete.cases(Games.df), ]

#Check if there are any NULL Values
colSums(is.na(Games.df))


# Removed the non numeric value from User_Score and changed the column to numeric 
Games.df <- Games.df[Games.df$User_Score != "tbd", ]
Games.df$User_Score <- as.numeric(Games.df$User_Score)


#Changing the type of Year_of_Release to integer
Games.df$Year_of_Release <- as.integer(Games.df$Year_of_Release)


#Checking for outliers
unique(Games.df$Year_of_Release)
summary(Games.df$Global_Sales)
summary(Games.df$NA_Sales)
summary(Games.df$EU_Sales)
summary(Games.df$JP_Sales)
summary(Games.df$Other_Sales)
summary(Games.df$Critic_Score)
summary(Games.df$Critic_Count)
summary(Games.df$User_Count)
summary(Games.df$User_Score)

str(Games.df)
View(Games.df)


#Same Scale for User Score(out od 10) and Critic Score(out 100)
Games.df$User_Score <- Games.df$User_Score * 10

#Mutate Rating column
Games.df <- Games.df %>% mutate(Rating = ifelse(Rating == "AO", "M", Rating))
Games.df <- Games.df %>% mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
Games.df <- Games.df %>% mutate(Rating = ifelse(Rating == "RP", "E", Rating))


##Club platforms
df_uniq <- unique(Games.df$Name)
length(df_uniq)
length(Games.df$Name)
Games.df  <-  Games.df %>% group_by(Name) %>% dplyr::summarise(Platform_S = paste0(Platform, collapse = "/"),
                                                               n_platforms = n(),
                                                               Year_of_Release = min(Year_of_Release),
                                                               Genre = first(Genre),
                                                               Publisher = first(Publisher),
                                                               NA_Sales = sum(NA_Sales),
                                                               EU_Sales = sum(EU_Sales),
                                                               JP_Sales = sum(JP_Sales),
                                                               Other_Sales = sum(Other_Sales),
                                                               Global_Sales = sum(Global_Sales),
                                                               Global_Sales = sum(Global_Sales),
                                                               Critic_Score = mean(Critic_Score),
                                                               Critic_Count = max(Critic_Count),
                                                               User_Score = mean(User_Score),
                                                               User_Count = max(User_Count),
                                                               Developer = first(Developer),
                                                               Rating = first(Rating))

#Platform duplicates arranged
Games.df <- Games.df %>% mutate(Platform_S = case_when(
  Platform_S %in% c("PS/PS2", "PS2/PS") ~ "PS/PS2",
  Platform_S %in% c("PS/PSP", "PSP/PS") ~ "PS/PSP",
  Platform_S %in% c("PS2/PSP", "PSP/PS2") ~ "PS2/PSP",
  Platform_S %in% c("PS2/PS3", "PS3/PS2") ~ "PS2/PS3",
  Platform_S %in% c("PS2/PS3/PSP", "PS2/PSP/PS3","PS3/PS2/PSP","PS3/PSP/PS2","PSP/PS2/PS3","PSP/PS3/PS2") ~ "PS2/PS3/PSP",
  Platform_S %in% c("PS/PS3", "PS3/PS") ~ "PS/PS3",
  Platform_S %in% c("PS3/PS4", "PS4/PS3") ~ "PS3/PS4",
  Platform_S %in% c("PS3/PSP", "PSP/PS3") ~ "PS3/PSP",
  Platform_S %in% c("PS3/PSV", "PSV/PS3") ~ "PS3/PSV",
  Platform_S %in% c("PS3/PSV/PS3", "PS3/PS3/PSV","PSV/PS3/PS3") ~ "PS3/PSV",
  Platform_S %in% c("PS3/PS4/PSV", "PS3/PSV/PS4","PS4/PS3/PSV","PS4/PSV/PS3","PSV/PS3/PS4","PSV/PS4/PS3") ~ "PS3/PS4/PSV",
  Platform_S %in% c("PS4/PSV", "PSV/PS4") ~ "PS4/PSV",
  Platform_S %in% c("PSP/PS2", "PS2/PSP") ~ "PS2/PSP",
  Platform_S %in% c("PSP/PS2/PS3", "PSP/PS3/PS2","PS2/PS3/PSP","PS2/PSP/PS3","PS3/PS2/PSP","PS3/PSP/PS2") ~ "PS2/PS3/PSP",
  Platform_S %in% c("PSV/PSP", "PSP/PSV") ~ "PSV/PSP",
  Platform_S %in% c("PS") ~ "PS",
  Platform_S %in% c("PS2") ~ "PS2",
  Platform_S %in% c("PS3") ~ "PS3",
  Platform_S %in% c("PS4") ~ "PS4",
  Platform_S %in% c("PSV") ~ "PSV",
  Platform_S %in% c("PSP") ~ "PSP"
))

#Duplicate game correction
Games.df <- Games.df %>% mutate(n_platforms = ifelse(Name == "Madden NFL 13", 2, n_platforms))

#histogram of global sales = skewed 
ggplot(Games.df) + geom_histogram(aes(Global_Sales), fill = "blue")
#histogram of global sales = scaled 
ggplot(Games.df) + geom_histogram(aes(Global_Sales), fill = "blue") + scale_x_log10()

#Year of Release count
Games.df %>% group_by(Year_of_Release) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(Year_of_Release, n), stat = "identity", 
           fill = "blue") + theme(axis.text.x = element_text(angle = 90))

#Titles released and global sales compared
color <- c("Titles released" = "red", "Global sales" = "blue")
Games.df %>% group_by(Year_of_Release) %>% 
  summarise(Games.df = sum(Global_Sales), count = n()) %>% 
  ggplot() + geom_line(aes(Year_of_Release, count, group = 1, color = "Titles released")) + 
  geom_line(aes(Year_of_Release, Games.df, group = 1, color = "Global sales")) + 
  xlab("Year of Release") + ylab("Titles released") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  scale_color_manual(values = color) + labs(color = "")

#Titles released and region vise sales compared
Games.df %>% gather(area, Games.df, NA_Sales:Other_Sales, 
                    factor_key = TRUE) %>% 
  group_by(area, Year_of_Release) %>% 
  summarise(Games.df = sum(Games.df)) %>% 
  ggplot() + 
  geom_line(aes(Year_of_Release, Games.df, group = area, color = area)) + 
  xlab("Year of release") + ylab("Sales") + labs(color = "blue") + 
  theme(legend.text = element_text(size = 7), 
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))


#Console based global sales
Games.df %>% group_by(n_platforms, Year_of_Release) %>%
  summarise(Games.df = sum(Global_Sales)) %>% 
  ggplot() + 
  geom_line(aes(Year_of_Release, Games.df, group = n_platforms, color = n_platforms)) +
  xlab("Year of release") + ylab("Global Sales") + labs(color = "blue") + 
  theme(legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size = 6))

#Reading Pre-Processed data with isSeries Column
Games.df <- read.csv("ViGaSa_Processed_with_series.csv")

#View data
View(Games.df)

#Sales for each game by Genre
Games.df %>% group_by(Genre) %>%
  summarise(sales = sum(Global_Sales)) %>%  
  ggplot() + 
  geom_bar(aes(reorder(Genre, sales), sales), stat = "identity", 
           fill = "blue") + 
  ylab("Global Sales") + xlab("Genre") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5)) + 
  coord_flip()

#Sales by Developer
Games.df %>% group_by(Developer) %>% 
  summarise(sales = sum(Global_Sales)) %>% 
  arrange(desc(sales)) %>% slice(1:10) %>% 
  ggplot() + 
  geom_bar(aes(reorder(Developer, sales), sales), stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Developer") + ylab("Global sales")

# Creating lists for top publishers and developers
publishers_top <- (Games.df %>% group_by(Publisher) %>%
                     summarise(sales = sum(Global_Sales)) %>% arrange(desc(sales)) %>% 
                     top_n(10) %>% distinct(Publisher))$Publisher

developers_top <- (Games.df %>% group_by(Developer) %>%
                     summarise(sales = sum(Global_Sales)) %>% arrange(desc(sales)) %>% 
                     top_n(10) %>% distinct(Developer))$Developer


#Creating new columns publisher_top and developer_top
Games.df <- Games.df %>% 
  mutate(publisher_top = ifelse(Publisher %in% publishers_top, 1, 0),
         developer_top = ifelse(Developer %in% developers_top, 1, 0))

#Look at New Columns
View(Games.df)

#Sales by isSeries
Games.df %>% group_by(isSeries) %>% 
  summarise(sales = sum(Global_Sales)) %>% 
  arrange(desc(sales)) %>% slice(1:10) %>% 
  ggplot() + 
  geom_bar(aes(reorder(isSeries, sales), sales), stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("isSeries") + ylab("Global sales")

#Vizualizing critic score and user score against sales
colors <- c("Critic score" = "blue", "User score" = "red")
ggplot(Games.df) + 
  geom_smooth(aes(Critic_Score, Global_Sales, color = "Critic score")) + 
  geom_smooth(aes(User_Score, Global_Sales, color = "User score")) +
  labs(color = "") + xlab("Score") + ylab("Global sales") + 
  scale_color_manual(values = colors)


#Vizualizing platform and Genre against Sales
Games.df %>% group_by(Platform_S, Genre) %>% 
  summarise(sales = sum(Global_Sales)) %>% 
  ggplot() + geom_raster(aes(Genre, Platform_S, fill = sales)) + 
  ylab("") + xlab("") + 
  scale_fill_gradient(low = "#ebebe5", high = "blue") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 5),
        legend.text = element_text(size = 7)) + labs(fill = "Sales")

#Scaling all Numerical columns 
Scale_col <- c("n_platforms","Year_of_Release","Critic_Score","Critic_Count","User_Score","User_Count","isSeries")
Games.df[Scale_col] <- lapply(Games.df[Scale_col], scale)
factor_col <- c("Genre","Rating")
Games.df[factor_col] <- lapply(Games.df[factor_col], as.factor)
View(Games.df)

#Splitting data
set.seed(123, sample.kind = "Rounding")
#1982
test_index <- createDataPartition(Games.df$Global_Sales, p = 0.3, list = FALSE)
train_set <- Games.df[-test_index, ]
test_set <- Games.df[test_index, ]

#Defining RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Linear Regression model
model_lm <- train(log(Global_Sales) ~ Critic_Score + 
                    User_Score + Genre + 
                    Year_of_Release + Critic_Count +
                    User_Count + Rating + 
                    publisher_top + developer_top + 
                    isSeries, method = "lm", data = train_set)
#Test Lm model
test_set$predicted_lm <- predict(model_lm, test_set)

rmse_results <- data.frame(Method = "Linear regression", 
                           RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_lm))
rmse_results

#Random forest model
rf <- randomForest(log(Global_Sales) ~ (Critic_Score + 
                                          User_Score + Genre + 
                                          Year_of_Release + Critic_Count +
                                          User_Count + Rating + 
                                          publisher_top + developer_top + isSeries), data = train_set, 
                   ntree = 1000, mtry = 10, nodesize = 1, importance = TRUE, sampsize = 1000) 

#plot the variables by order of importance
varImpPlot(rf, type = 1)

#Test rf
test_set$predicted_rf <- predict(rf, test_set)

rmse_results <- rmse_results %>% add_row(Method = "Random forest", RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_rf))
rmse_results

#KNN Model
#Removing columns which are not required
knn.df <- Games.df[,-c(1,2,3,6,7,8,9,10,11,16)]
#Converting engineered columns to numeric
for (i in 1:dim(knn.df)[2])
{
  knn.df[,i] <- as.numeric(knn.df[,i])
}
#View data
head(knn.df)
#Scaling all columns 
knn.df1 <- sapply(knn.df[,1:(dim(knn.df)[2])],scale)
#Converting to a df
knn.df1 <- as.data.frame(knn.df1)
#Combining Global_sales with the other data
knn.df <- cbind(knn.df1,Games.df$Global_Sales)
names(knn.df)[11] = "Global_Sales"
#Splitting into train and test data
train.size <- floor(0.7*nrow(knn.df))
train.index <- sample(1:nrow(knn.df),train.size, replace = F)
train.set <- knn.df[train.index,]
test.set <- knn.df[-train.index,]
#Removing the dependent variable from the training data
train.x <- train.set[,-11]
#Dependent variable
train.y <- train.set[,11]
#Removing the dependent variable from the testing data
test.x <- test.set[,-11]
#Dependent variable
test.y <- test.set[,11]
#Modelling KNN
pred_003<- knn.reg(train = train.x, test = test.x, y = train.y, k = 3)

rmse_results <- rmse_results %>% add_row(Method = "KNN", RMSE = RMSE(log(test.y),pred_003$pred))
rmse_results

#Plotting the Comparison between models
rmse_results %>%
  ggplot()+
  geom_line(aes(y=RMSE, x=Method,group=1), color = "red") +
  geom_point(aes(y=RMSE, x=Method,group=1), color = "red") +
  xlab("Methods") + ylab("RMSE")

#checking error
ggplot(test_set) + geom_point(aes(log(test_set$Global_Sales) - test_set$predicted_rf, test_set$Global_Sales)) + 
  xlab("Error") + ylab("Global sales")

