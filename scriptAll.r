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
        transform = c("workclass", "education", "marital.status", "occupation", "race")
    ),
    list(
        name = "BankMarketing",
        file = c("data/bank.csv"),
        ext = c("csv"),
        numeric_vars = c("age", "balance", "day", "pdays","duration", "campaign", "previous"),
        categorical_vars = c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome"),
        transform = c("job", "marital", "education", "contact", "month", "poutcome")
    ),
    list(
        name = "Marketing",
        file = c("data/marketing_campaign.csv"),
        ext = c("csv"),
        numeric_vars = c("Year_Birth","Income","Kidhome",
                        "Teenhome","Recency","MntWines","MntFruits",
                        "MntMeatProducts","MntFishProducts",
                        "NumDealsPurchases","NumWebPurchases"),
        categorical_vars = c("Education","Marital_Status","Response"),
        transform = c("Education")
    )
    #, list(
    #     name = "OnlineRetail",
    #     file = "data/Online Retail.xlsx",
    #     ext = c("xlsx"),
    #     numeric_vars = c("Quantity","UnitPrice"),
    #     categorical_vars = c("StockCode", "Description","Country"),
    #     transform = c("Country")
    # )
)

ks = c(1, 2, 3, 4, 5, 6)
N = 100
nStart = 2
probs = c(seq(0.01, 0.99, 0.05),0.99)
k_prob = c(2,4)
min_freq = 100            

# for(dataset in datasets){
    # print(paste("Running dataset", dataset$name))
    # dataset <- run(dataset, ks, N, nStart, probs, k_prob,min_freq)
# }

datasets_with_metrics = list()
for(d in datasets){
    load(paste(d$name,"_results.rda"))
    datasets_with_metrics <- c(datasets_with_metrics,list(dataset))
}

print("Done")

# each dataset has dataset$metrics_normal 
# plot on same plot dataset$metrics_normal[1,] for all datasets
save_plot_as_png(paste0("Min_Homogeneity_vs_K_Normal.png"), function() {
    ind <- 1
    plot(NULL, type = "n", xlab = "Clusters", ylab = "Min Homogeneity", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_normal[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_normal[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        lines(datasets_with_metrics[[i]]$metrics_normal[ind,], type = "l", col = i,lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Homogeneity_vs_K_Normal.png"), function() {
    ind <- 2
    plot(NULL, type = "n", xlab = "Clusters", ylab = "Homogeneity", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_normal[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_normal[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        lines(datasets_with_metrics[[i]]$metrics_normal[ind,], type = "l", col = i,lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Silhouette_vs_K_Normal.png"), function() {
    ind <- 3
    plot(NULL, type = "n", xlab = "Clusters", ylab = "Silhouette Score Mean", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_normal[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) na.omit(x$metrics_normal[ind,]))))


    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_normal[ind,])
        lines(2:(length(line_data) + 1), line_data, type = "l", col = i, lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("ARI_vs_K_Normal.png"), function() {
    ind <- 4
    plot(NULL, type = "n", xlab = "Clusters", ylab = "ARI", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_normal[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_normal[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        lines(datasets_with_metrics[[i]]$metrics_normal[ind,], type = "l", col = i,lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

#############################

save_plot_as_png(paste0("Min_Homogeneity_vs_K_Binary.png"), function() {
    ind <- 1
    plot(NULL, type = "n", xlab = "Clusters", ylab = "Min Homogeneity", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_binary[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_binary[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        lines(datasets_with_metrics[[i]]$metrics_binary[ind,], type = "l", col = i,lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Homogeneity_vs_K_Binary.png"), function() {
    ind <- 2
    plot(NULL, type = "n", xlab = "Clusters", ylab = "Homogeneity", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_binary[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_binary[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        lines(datasets_with_metrics[[i]]$metrics_binary[ind,], type = "l", col = i,lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Silhouette_vs_K_Binary.png"), function() {
    ind <- 3
    plot(NULL, type = "n", xlab = "Clusters", ylab = "Silhouette Score Mean", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_binary[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) na.omit(x$metrics_binary[ind,]))))


    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_binary[ind,])
        lines(2:(length(line_data) + 1), line_data, type = "l", col = i, lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("ARI_vs_K_Binary.png"), function() {
    ind <- 4
    plot(NULL, type = "n", xlab = "Clusters", ylab = "ARI", 
        xlim = c(1, length(datasets_with_metrics[[1]]$metrics_binary[ind,])), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_binary[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        lines(datasets_with_metrics[[i]]$metrics_binary[ind,], type = "l", col = i,lwd = 2)
    }
    legend("topright", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

#############################

save_plot_as_png(paste0("Min_Homogeneity_vs_K_Prob_2.png"), function() {
    ind <- 1
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "Min Homogeneity", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_prob_2[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_2[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Homogeneity_vs_K_Prob_2.png"), function() {
    ind <- 2
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "Homogeneity", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_prob_2[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_2[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Silhouette_vs_K_Prob_2.png"), function() {
    ind <- 3
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "Silhouette Score Mean", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) na.omit(x$metrics_prob_2[ind,]))))


    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_2[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("ARI_vs_K_Prob_2.png"), function() {
    ind <- 4
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "ARI", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_prob_2[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_2[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

#############################

save_plot_as_png(paste0("Min_Homogeneity_vs_K_Prob_4.png"), function() {
    ind <- 1
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "Min Homogeneity", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_prob_4[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_4[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Homogeneity_vs_K_Prob_4.png"), function() {
    ind <- 2
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "Homogeneity", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_prob_4[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_4[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("Silhouette_vs_K_Prob_4.png"), function() {
    ind <- 3
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "Silhouette Score Mean", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) na.omit(x$metrics_prob_4[ind,]))))


    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_4[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})

save_plot_as_png(paste0("ARI_vs_K_Prob_4.png"), function() {
    ind <- 4
    plot(NULL, type = "n", xlab = "Probability of 0's", ylab = "ARI", 
        xlim = range(probs), 
        ylim = range(sapply(datasets_with_metrics, function(x) x$metrics_prob_4[ind,])))

    # Add lines for each dataset
    for (i in seq_along(datasets_with_metrics)) {
        line_data <- na.omit(datasets_with_metrics[[i]]$metrics_prob_4[ind,])
        lines(probs, line_data, type = "l", col = i, lwd = 2)
    }
    legend("bottomleft", legend = sapply(datasets, function(x) x$name), col = 1:length(datasets_with_metrics), lty = 1)
})