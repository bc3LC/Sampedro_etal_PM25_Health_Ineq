
#' ml_clustering
#'
#' @description ML clustering and dendograms creation algorithm
#' @param data dataset
#' @param cluster_number desired number of clusters
ml_clustering <- function(data, cluster_number) {
  # Convert each country's values to ranks
  ranked_data <- data %>%
    dplyr::select(-country) %>%
    apply(1, rank) %>%
    t() %>%
    as.data.frame()

  # Assign row names
  rownames(ranked_data) <- data$country

  # Compute Spearman correlation matrix
  cor_matrix <- cor(t(ranked_data), method = "spearman")

  # Convert to distance matrix (1 - correlation)
  dist_matrix <- as.dist(1 - cor_matrix)

  # Perform hierarchical clustering
  hc <- hclust(dist_matrix, method = "ward.D2")

  # Plot dendrogram
  png(file.path("figures", paste0("ml_dendongram_cluster", cluster_number, ".png")), width = 3000, height = 800)
  dend <- as.dendrogram(hc)
  plot(dend,
       main = NULL,
       cex.main = 1.5,
       cex.lab = 1.5,
       cex.axis = 1.5,
       cex = 1.5,
       lwd = 2,
       xlab = "",
       axes = FALSE,
       frame.plot = FALSE
  )
  axis(2, cex.axis=1.25)
  mtext("Height", side = 2, line = 3, cex = 1.25)  # y-axis label
  dev.off()

  # Cut tree into clusters
  clusters <- cutree(hc, k = cluster_number)

  # Add cluster labels to the original data
  data$cluster <- clusters

  return(data)
}
