a <- read.csv("Titanic.csv")
head(a)

str(a)
summary(a)

sum(is.na(a))

a_clean <- na.omit(a)

sum(is.na(a_clean)) 

colnames(a)

str(a)

unique(a$PassengerId)
unique(a$Pclass)
unique(a$Name)
unique(a$Sex)
unique(a$Age)
unique(a$SibSp)
unique(a$Parch)
unique(a$Cabin)
unique(a$Embarked)


a_clean$Pclass_Binary <- ifelse(a_clean$Pclass == 3, 1, 0)
a_clean$Sex_Binary <- ifelse(a_clean$Sex == "female", 0, 1)
a_clean$SibSp_Binary <- ifelse(a_clean$SibSp == 0, 0, 1)
a_clean$Parch_Binary <- ifelse(a_clean$Parch == 0, 0, 1)
a_clean$Cabin_Binary <- ifelse(!is.na(a_clean$Cabin), 1, 0)
a_clean$Embarked_Binary <- ifelse(a_clean$Embarked %in% c("C", "Q"), 0, 1)

set.seed(123)  # Para garantir reprodutibilidade
kmeans_result <- kmeans(a_clean[, c("Pclass_Binary", "Sex_Binary", "SibSp_Binary", "Parch_Binary", "Cabin_Binary", "Embarked_Binary")], centers = 3)
summary(kmeans_result)
str(a_clean)


summary(kmeans_result)

a_clean$Cluster <- kmeans_result$cluster

head(a_clean[, c("PassengerId", "Cluster")])

sapply(a_clean[, c("Pclass_Binary", "Sex_Binary", "SibSp_Binary", "Parch_Binary", "Cabin_Binary", "Embarked_Binary")], function(x) length(unique(x)))

a_clean_no_constant <- a_clean[, c("Pclass_Binary", "Sex_Binary", "SibSp_Binary", "Parch_Binary", "Embarked_Binary")]

pca <- prcomp(a_clean_no_constant, scale. = TRUE)

a_clean$PC1 <- pca$x[, 1]
a_clean$PC2 <- pca$x[, 2]

library(ggplot2)
ggplot(a_clean, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Clusters do K-means com PCA", color = "Cluster") +
  theme_minimal()


head(a_clean[, c("PC1", "PC2")])


head(a_clean[, c("PassengerId", "Cluster", "PC1", "PC2")])

install.packages("mclust")  
library(mclust)            

ari <- adjustedRandIndex(a_clean$Survived, a_clean$Cluster)
print(ari)

head(a_clean$Cluster)

ggplot(a_clean, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Clusters do K-means com PCA", color = "Cluster") +
  theme_minimal()

kmeans_result <- kmeans(a_clean_no_constant, centers = 4)
a_clean$Cluster <- kmeans_result$cluster

library(cluster)
silhouette_score <- silhouette(kmeans_result$cluster, dist(a_clean_no_constant))
plot(silhouette_score)
