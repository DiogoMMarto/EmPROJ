# Download and Load required libraries ClustOfVar , PCAmixdata and Cluster.
install.packages("ClustOfVar")
install.packages("PCAmixdata")
install.packages("cluster")
install.packages("mclust")
library(ClustOfVar)
library(PCAmixdata)
library(cluster)
library(mclust)

load_and_clean_data <- function(file_path) {
    # Load the dataset # nolint: indentation_linter.
    a <- read.csv(file_path)
    # drop collums with names Name , PassengerId , Ticket and Cabin
    a <- subset(a, select = -c(Name, PassengerId, Ticket, Cabin))
    # Preview the data
    cat("Preview of the first few rows of the dataset:\n")
    print(head(a))
    # Display summary of the dataset
    cat("\nSummary of the dataset:\n")
    print(summary(a))
    # Check for and remove rows with missing values
    cat("Checking for missing values and removing rows with NA...\n")
    rows_before <- nrow(a)
    a <- na.omit(a)
    rows_after <- nrow(a)
    cat(sprintf("\nRows before cleaning: %d\nRows after cleaning: %d\n", rows_before, rows_after)) # nolint: line_length_linter.
    # Return the cleaned data
    return(a)
}

data <- load_and_clean_data("Titanic.csv")
categorical_vars <- c("Survived", "Pclass", "Sex", "Embarked")
# convert Survived and Pclass to categorical variables
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass + 10)

numeric_vars <- c("Age", "SibSp", "Parch", "Fare")

# function for displaying PCAMIX
display_pcamix <- function(data, categorical_vars, numeric_vars) {
    result <- PCAmix(X.quanti = data[, numeric_vars], X.quali = data[, categorical_vars], 
        rename.level=TRUE, ndim = 10, graph = FALSE) 
    head(result$eig)
    plot(result, axes = 1:2, choice="cor", main="Correlation Circle")
    plot(result, axes = 1:2, choice="levels", main="Levels")
    plot(result, axes = 1:2, choice="ind", lim.cos2.plot=0.1, 
        main="Individuals component map")
    plot(result, axes = 1:2, choice="sqload", main="Squared loadings")
    return(result)
}

# Apply PCAmix
result <- display_pcamix(data, categorical_vars, numeric_vars)

# function to do kmeansvar
do_kmeansvar <- function(data, categorical_vars, numeric_vars, k) {
    kmeans_result <- kmeansvar(X.quanti = data[, numeric_vars], 
        X.quali = data[, categorical_vars], init = k, nstart = 10)
    return(kmeans_result)
}

# Apply kmeansvar
k <- do_kmeansvar(data, categorical_vars, numeric_vars, 4)

# function to calculate homogeneity
calculate_homogeneity <- function(kmeans_result) {
    return(min(kmeans_result$wss))
}

calculate_metrics <- function(kmeans_result) {
    min_homogeneity <- min(kmeans_result$wss)
    standart_H <- ( sum(kmeans_result$wss) - 1) / (length(numeric_vars)+length(categorical_vars)-1)
    return(c(min_homogeneity, standart_H))
}

# function to do kmeansvar with different k
do_kmeansvar_with_different_k <- function(data, categorical_vars, numeric_vars, ks, nstart) {
    metrics <- sapply(ks, function(k) {
        kmeans_result <- kmeansvar(X.quanti = data[, numeric_vars], 
            X.quali = data[, categorical_vars], init = k,nstart)
        return(calculate_metrics(kmeans_result))
    })
    return(metrics)
}

k2 <- do_kmeansvar_with_different_k(data, categorical_vars, 
    numeric_vars, ks = c(1, 2, 3, 4, 5, 6), nstart = 5)

do_kmeansvar_with_different_k_ntimes_avg <- function(data, categorical_vars, 
    numeric_vars, ks, ntimes , nstart) {
    # zero matrix with nrow = 2 and ncol = length(ks)
    res <- matrix(0, nrow = 2, ncol = length(ks))
    for(i in 1:ntimes) {
        metrics <- do_kmeansvar_with_different_k(data, categorical_vars, numeric_vars, ks,nstart)
        res <- res + metrics
    }
    return(res / ntimes)
}

# Apply kmeansvar with different k
ks = c(1, 2, 3, 4, 5, 6)
N = 100
nStart = 5
res <- do_kmeansvar_with_different_k_ntimes_avg(data, categorical_vars, 
    numeric_vars,ks,N,nStart)
# plot homogeneity vs k
homogeneities <- res[1,]
stH <- res[2,]
plot(ks, homogeneities, type = "b", xlab = "k", ylab = "Min Homogeneity", main = "Min Homogeneity vs k")
# plot standard homogeneity vs k
plot(ks, stH, type = "b", xlab = "k", ylab = "Standard Homogeneity", main = "Standard Homogeneity vs k")

# 3 Clusters are good

# for Pclass and Embarked calculate the percentage of each value in their column
pclass_percentage <- table(data$Pclass) / nrow(data) * 100
embarked_percentage <- table(data$Embarked) / nrow(data) * 100

# find most likely value for Pclass and Embarked
most_likely_pclass <- names(pclass_percentage)[which.max(pclass_percentage)]
most_likely_embarked <- names(embarked_percentage)[which.max(embarked_percentage)]

# transform Pclass and Embarked to binary variables based on their most likely value
data$Pclass_Binary <- ifelse(data$Pclass == most_likely_pclass, 21, 20)
data$Pclass_Binary <- as.factor(data$Pclass_Binary)
data$Embarked_Binary <- ifelse(data$Embarked == most_likely_embarked, 11, 10)
data$Embarked_Binary <- as.factor(data$Embarked_Binary)

categorical_vars <- c("Survived", "Pclass_Binary", "Sex", "Embarked_Binary")

# Apply kmeansvar with different k
ks = c(1, 2, 3, 4, 5, 6)
res <- do_kmeansvar_with_different_k_ntimes_avg(data, categorical_vars, numeric_vars,ks,N,nStart)
homogeneities <- res[1,]
stH <- res[2,]
# plot homogeneity vs k
plot(ks, homogeneities, type = "b", xlab = "k", ylab = "Min Homogeneity", main = "Min Homogeneity vs k")
# plot standard homogeneity vs k
plot(ks, stH, type = "b", xlab = "k", ylab = "Standard Homogeneity", main = "Standard Homogeneity vs k")


map_values <- function(data, categorical_var, prob) {
    # calculate the percentage of each value in their column
    percentage <- table(data[, categorical_var]) / nrow(data)
    # order table by percentage
    percentage <- sort(percentage, decreasing = FALSE)
    # put last value in first place
    percentage <- c(percentage[length(percentage)], percentage[-length(percentage)])
    # find where probability if you consider cumulative percentage
    cum_percentage <- cumsum(percentage)
    # find value which is above prob
    value <- which(cum_percentage > prob)[1]
    value_b <- which(cum_percentage > prob)[1]-1
    missing_prob <- (prob-ifelse(value_b==0,0,cum_percentage[value_b])) / (percentage[value])
    # convert classes of data[, categorical_var] to index of percentage
    data_index <- match(data[, categorical_var], names(percentage))
    # concatenate categorical_var with "_binary_prob"
    categorical_var2 <- paste0(categorical_var, "_Binary_Prob")
    data[, categorical_var2] <- ifelse(
        data_index < value, 0,
        ifelse(data_index > value, 1,
               ifelse(runif(nrow(data)) < missing_prob, 0, 1)
        )
    )
    # extract first value of data[, categorical_var] and the first character of it and convert to int
    categorical_var3 <- paste0(categorical_var, "_Binary")
    c <- as.integer(substr(data[, categorical_var3][1], 1, 1))
    data[, categorical_var2] <- as.factor(data[, categorical_var2]+c*100)
    return(data)
}   

data2 <- map_values(data, "Pclass",0.75)

# print "Pclass" and "Pclass_binary_prob" and "Pclass_binary"
data2[, c("Pclass", "Pclass_Binary_Prob", "Pclass_Binary")]
table(data2[,"Pclass_Binary_Prob"]) / nrow(data2)

# function to do kmeansvar with different prob
do_kmeansvar_with_different_prob <- function(data, categorical_vars, numeric_vars, prob, nstart , k, N = 10) {
    data <- map_values(data, "Pclass", prob)
    data <- map_values(data, "Embarked", prob)
    metrics <- do_kmeansvar_with_different_k_ntimes_avg(data, categorical_vars, numeric_vars,c(k),N,nstart)
    return(metrics)
}

do_kmeansvar_with_different_probs <- function(data, categorical_vars, 
                                             numeric_vars, probs, nstart, k , N) {
    res <- matrix(0, nrow = 2, ncol = length(probs))
    # for all probs
    for(i in 1:length(probs)) {
        res[,i] <- do_kmeansvar_with_different_prob(data, categorical_vars, 
                                                   numeric_vars, probs[i], nstart, k, N )
    }
    return(res)
}

# do kmeansvar with different prob 
k = 2
probs = c(seq(0.01, 0.99, 0.05),0.99)
categorical_vars = c("Survived", "Pclass_Binary_Prob", "Sex", "Embarked_Binary_Prob")
res <- do_kmeansvar_with_different_probs(data, categorical_vars, numeric_vars, probs, nStart, k , N)
# plot homogeneity vs prob
homogeneities <- res[1,]
stH <- res[2,]
plot(probs, homogeneities, type = "b", xlab = "prob", ylab = "Min Homogeneity", main = "Min Homogeneity vs prob")
# plot standard homogeneity vs prob
plot(probs, stH, type = "b", xlab = "prob", ylab = "Standard Homogeneity", main = "Standard Homogeneity vs prob")

# do kmeansvar with different prob 
k = 4
probs = c(seq(0.01, 0.99, 0.05),0.99)
categorical_vars = c("Survived", "Pclass_Binary_Prob", "Sex", "Embarked_Binary_Prob")
res <- do_kmeansvar_with_different_probs(data, categorical_vars, numeric_vars, probs, nStart, k , N)
# plot homogeneity vs prob
homogeneities <- res[1,]
stH <- res[2,]
plot(probs, homogeneities, type = "b", xlab = "prob", ylab = "Min Homogeneity", main = "Min Homogeneity vs prob")
# plot standard homogeneity vs prob
plot(probs, stH, type = "b", xlab = "prob", ylab = "Standard Homogeneity", main = "Standard Homogeneity vs prob")

