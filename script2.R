# Download and Load required libraries ClustOfVar , PCAmixdata and Cluster.
source("functions.r")

dataset <- list(
    name = "Titanic",
    file = c("data/Titanic-Dataset.csv"),
    ext = c("csv"),
    numeric_vars = c("Age", "SibSp", "Parch", "Fare"),
    categorical_vars = c("Survived", "Pclass", "Sex", "Embarked"),
    transform = c("Pclass", "Embarked")
)

ks = c(1, 2, 3, 4, 5, 6)
N = 2
nStart = 3
probs = c(seq(0.01, 0.99, 0.05),0.99)
k_prob = c(2,4)

dataset <- init_dataset(dataset)

c <- kmeansvar(
            X.quanti = dataset$data[, dataset$numeric_vars],
            X.quali = dataset$data[, dataset$categorical_vars], init = 3, nstart=2
        )
result <- display_pcamix(dataset$data, dataset$categorical_vars, dataset$numeric_vars)

dataset <- run(dataset, ks, N, nStart, probs, k_prob)
