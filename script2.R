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
N = 10
B_n = 5
nStart = 2
probs = c(seq(0.01, 0.99, 0.05),0.99)
k_prob = c(2,4)

dataset <- init_dataset(dataset)

result <- display_pcamix(dataset$data, dataset$categorical_vars, dataset$numeric_vars)

c <- kmeansvar(
            X.quanti = dataset$data[, dataset$numeric_vars],
            X.quali = dataset$data[, dataset$categorical_vars], init = 3, nstart=2
        )

dataset$metrics_normal <- do_kmeansvar_with_different_k_ntimes_avg(
    dataset$data, 
    dataset$categorical_vars, 
    dataset$numeric_vars,
    ks,
    N,
    nStart
)
plot_metrics(dataset$metrics_normal, ks,"k","Normal")

dataset$data <- map_values_likely(dataset$data, dataset$transform)
dataset$metrics_binary <- do_kmeansvar_with_different_k_ntimes_avg(
    dataset$data, 
    dataset$binary_labels, 
    dataset$numeric_vars,
    ks,
    N,
    nStart
)
plot_metrics(dataset$metrics_binary, ks,"k","Binary")

for(k in k_prob){
    name <- paste0("metrics_prob_",k)
    dataset[[name]] <- do_kmeansvar_with_different_probs_ntimes_avg(
        dataset$data, 
        dataset$prob_labels, 
        dataset$numeric_vars,
        probs,
        nStart,
        k,
        N
    )
    type <- paste0("Prob k=",k)
    plot_metrics(dataset$metrics_prob, probs,"prob",type)
}
