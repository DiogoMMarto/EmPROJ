source("functions.r")

datasets <- list(
    list(
        name = "Titanic",
        file = c("data/Titanic-Dataset.csv"),
        ext = c("csv"),
        numeric_vars = c("Age", "SibSp", "Parch", "Fare"),
        categorical_vars = c("Survived", "Pclass", "Sex", "Embarked"),
        transform = c("Pclass", "Embarked")
    ),
    list(
        name = "AdultCensusIncome",
        file = c("data/adult.csv"),
        ext = c("csv"),
        numeric_vars = c("age", "fnlwgt", "education.num", "capital.gain", "capital.loss", "hours.per.week"),
        categorical_vars = c("workclass", "education", "marital.status", "occupation", "relationship", "race" , "sex", "native.country" , "income"),
        transform = c("workclass", "education", "marital.status", "occupation", "race"),
    ),
    list(
        name = "BankMarketing",
        file = c("data/bank.csv"),
        ext = c("csv"),
        numeric_vars = c("age", "balance", "day", "pdays","duration", "campaign", "previous"),
        categorical_vars = c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "deposit"),
        transform = c("job", "marital", "education", "contact", "month", "poutcome")
    ),
    # list(
    #     name = "Marketing",
    #     file = c("data/marketing_campaign.csv"),
    #     ext = c("csv"),
    #     numeric_vars = c("age",)
    # )
)

ks = c(1, 2, 3, 4, 5, 6)
N = 100
nStart = 3
probs = c(seq(0.01, 0.99, 0.05),0.99)
k_prob = c(2,4)

for(dataset in datasets){
    print(paste("Running dataset", dataset$name))
    dataset <- run(dataset, ks, N, nStart, probs, k_prob)
}

# do plots where you have the metrics but each plot has multiple datasets
# for(type in c("metrics_normal", "metrics_binary","metrics_prob")){

#     plot(type="l",ylab="Min Homogeneity", xlab="K", title="Min Homogeneity")
#     for(dataset in datasets){
#         print(paste("Plotting", type, "for", dataset$name))
        
#     }
# }