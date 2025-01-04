install.packages("ClustOfVar")
install.packages("PCAmixdata")
install.packages("cluster")
install.packages("mclust")
install.packages("readxl")
library(readxl)
library(ClustOfVar)
library(PCAmixdata)
library(cluster)
library(mclust)

load_and_clean_data <- function(file_path, ext, vars) {
    if (ext == "csv") {
        a <- read.csv(file_path)
    } else if (ext == "xlsx") {
        a <- read_excel(file_path)
    } else {
        stop("File format not supported")
    }
    a <- subset(a, select = c(vars))
    cat("Checking for missing values and removing rows with NA...\n")
    rows_before <- nrow(a)
    a <- na.omit(a)
    rows_after <- nrow(a)
    cat(sprintf("\nRows before cleaning: %d\nRows after cleaning: %d\n", rows_before, rows_after))
    return(a)
}

init_dataset <- function(dataset) {
    dataset$vars <- c(dataset$categorical_vars, dataset$numeric_vars)
    dataset$data <- load_and_clean_data(dataset$file, dataset$ext, dataset$vars)
    dataset$binary_labels <- c()
    dataset$prob_labels <- c()
    for (var in dataset$categorical_vars) {
        new_name <- var
        new_name2 <- var
        if (var %in% dataset$transform) {
            new_name <- paste0(var, "_Binary")
            new_name2 <- paste0(var, "_Prob")
        }
        dataset$binary_labels <- c(dataset$binary_labels, new_name)
        dataset$prob_labels <- c(dataset$prob_labels, new_name2)
    }

    # convert categorical variables to factors if they are not already
    letter = "A"
    for (var in dataset$categorical_vars) {
        if( is.numeric(dataset$data[[var]]) ){
            dataset$data[[var]] <- sapply(dataset$data[[var]], function(x) paste0(letter,x))
            dataset$data[[var]] <- as.factor(dataset$data[[var]])
            letter <- intToUtf8(utf8ToInt(letter) + 1)
        }
    }
    return(dataset)

}

display_pcamix <- function(data, categorical_vars, numeric_vars) {
    result <- PCAmix(
        X.quanti = data[, numeric_vars], X.quali = data[, categorical_vars],
        rename.level = TRUE, ndim = 10, graph = FALSE
    )
    head(result$eig)
    plot(result, axes = 1:2, choice = "cor", main = "Correlation Circle")
    plot(result, axes = 1:2, choice = "levels", main = "Levels")
    plot(result,
        axes = 1:2, choice = "ind", lim.cos2.plot = 0.1,
        main = "Individuals component map"
    )
    plot(result, axes = 1:2, choice = "sqload", main = "Squared loadings")
    return(result)
}

dist_mixed <- function(data) {
    # matrix of len(vars) x len(vars)
    dist_matrix <- matrix(0, nrow = ncol(data), ncol = ncol(data))
    for (i in 1:ncol(data)) {
        for (j in 1:ncol(data)) {
            dist_matrix[i, j] <- mixedVarSim(data[, i], data[, j])
        }
    }
    return(dist_matrix)
}

B_n <- 10
evaluate_partition_stability <- function(data, categorical_vars, numeric_vars, k, initial_hierarchy, B = B_n) {
    ari_values <- numeric(B)
    for (i in 1:B) {
        tryCatch({
        bootstrap_sample <- data[sample(1:nrow(data), replace = TRUE), ]
        kmeans_result <- kmeansvar(
            X.quanti = bootstrap_sample[, numeric_vars],
            X.quali = bootstrap_sample[, categorical_vars],
            init = k, nstart = 10
        )
        ari_values[i] <- adjustedRandIndex(initial_hierarchy, kmeans_result$cluster)
    }, error = function(e) {
        cat("Error in bootstrap sample\n" , i , k)
    })

    }
    mean_ari <- mean(ari_values)
    return(mean_ari)
}

NUM_METRICS <- 4
calculate_metrics <- function(kmeans_result, data, categorical_vars, numeric_vars,k) {
    min_homogeneity <- min(kmeans_result$wss/ kmeans_result$size)
    standart_H <- (sum(kmeans_result$wss) - 1) / (length(numeric_vars) + length(categorical_vars) - 1)
    dist_matrix <- dist_mixed(data)
    silhouette <- mean(silhouette(kmeans_result$cluster, dist_matrix))
    ari <- evaluate_partition_stability(data, categorical_vars, numeric_vars, k, kmeans_result$cluster)
    return(c(min_homogeneity, standart_H, silhouette, ari))
}

do_kmeansvar_with_different_k <- function(data, categorical_vars, numeric_vars, ks, nstart) {
    metrics <- sapply(ks, function(k) {
        kmeans_result <- kmeansvar(
            X.quanti = data[, numeric_vars],
            X.quali = data[, categorical_vars], init = k, nstart
        )
        return(calculate_metrics(kmeans_result, data, categorical_vars, numeric_vars, k))
    })
    return(metrics)
}

do_kmeansvar_with_different_k_ntimes_avg <- function(data, categorical_vars, numeric_vars, ks, ntimes, nstart) {
    res <- matrix(0, nrow = NUM_METRICS, ncol = length(ks))
    for (i in 1:ntimes) {
        print(i)
        metrics <- do_kmeansvar_with_different_k(data, categorical_vars, numeric_vars, ks, nstart)
        res <- res + metrics
    }
    return(res / ntimes)
}

map_values_likely <- function(data, transforming_vars) {
    C <- 10
    for (var in transforming_vars) {
        var_percent <- table(data[, var]) / nrow(data)
        most_likely_value <- names(var_percent)[which.max(var_percent)]
        new_name <- paste0(var, "_Binary")
        data[[new_name]] <- ifelse(data[, var] == most_likely_value, 1, 0)
        data[[new_name]] <- as.factor(data[[new_name]] + C)
        C <- C + 10
    }
    return(data)
}

p_tranform <- function(data, categorical_vars, numeric_vars, probs) {
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
    value_b <- which(cum_percentage > prob)[1] - 1
    missing_prob <- (prob - ifelse(value_b == 0, 0, cum_percentage[value_b])) / (percentage[value])
    # convert classes of data[, categorical_var] to index of percentage
    data_index <- match(data[, categorical_var], names(percentage))
    # concatenate categorical_var with "_Prob"
    categorical_var2 <- paste0(categorical_var, "_Prob")
    data[, categorical_var2] <- ifelse(
        data_index < value, 0,
        ifelse(data_index > value, 1,
            ifelse(runif(nrow(data)) < missing_prob, 0, 1)
        )
    )
    return(data)
}

map_values_p <- function(data, transformed_vars, prob) {
    C <- 1000
    for (var in transformed_vars) {
        transform <- p_tranform(data, var, prob)
        new_name <- paste0(var, "_Prob")
        data[[new_name]] <- new[[transform]]
        data[[new_name]] <- as.factor(data[[new_name]] + C)
        C <- C + 1000
    }
    return(data)
}

do_kmeansvar_with_different_prob <- function(data, categorical_vars, numeric_vars, transformed_vars, prob, nstart, k, N = 1) {
    data <- map_values_p(data, transformed_vars, prob)
    metrics <- do_kmeansvar_with_different_k_ntimes_avg(data, categorical_vars, numeric_vars, c(k), N, nstart)
    return(metrics)
}

do_kmeansvar_with_different_probs <- function(data, categorical_vars, numeric_vars, probs, nstart, k, N) {
    res <- matrix(0, nrow = NUM_METRICS, ncol = length(probs))
    # for all probs
    for (i in 1:length(probs)) {
        res[, i] <- do_kmeansvar_with_different_prob(
            data, categorical_vars,
            numeric_vars, probs[i], nstart, k, N
        )
    }
    return(res)
}

save_plot_as_png <- function(file_name, plot_function) {
    png(file_name)
    plot_function()
    dev.off()
}

plot_metrics <- function(metrics, x, x_name, type, name = "Titanic") {
    homogeneities <- metrics[1, ]
    stH <- metrics[2, ]
    silhouettes <- metrics[3, ]
    aris <- metrics[4, ]

    # create folder if it does not exist
    if (!file.exists(name)) {
        dir.create(name)
    }

    main <- paste0("Min Homogeneity vs ", x_name, " (", type, ")")
    save_plot_as_png(paste0(name, "/Min_Homogeneity_vs_", x_name, "_", type, ".png"), function() {
        plot(x, homogeneities, type = "b", xlab = x_name, ylab = "Min Homogeneity", main = main)
    })

    main <- paste0("Standard Homogeneity vs ", x_name, " (", type, ")")
    save_plot_as_png(paste0(name, "/Standard_Homogeneity_vs_", x_name, "_", type, ".png"), function() {
        plot(x, stH, type = "b", xlab = x_name, ylab = "Standard Homogeneity", main = main)
    })

    main <- paste0("Silhouette vs ", x_name, " (", type, ")")
    save_plot_as_png(paste0(name, "/Silhouette_vs_", x_name, "_", type, ".png"), function() {
        plot(x, silhouettes, type = "b", xlab = x_name, ylab = "Silhouette", main = main)
    })

    main <- paste0("ARI vs ", x_name, " (", type, ")")
    save_plot_as_png(paste0(name, "/ARI_vs_", x_name, "_", type, ".png"), function() {
        plot(x, aris, type = "b", xlab = x_name, ylab = "ARI", main = main)
    })
}
