
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
       main = NULL,# paste("Hierarchical Clustering of countries by",title_tag,"ranking"),
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


#' ml_do_all
#'
#' @description function to run all the ML analysis
#' @param data dataset
#' @param cluster_number desired number of clusters
#' @param fig_legend figure's legend
#' @param fig_ox_label figure's ox label
ml_do_all <- function(data, cluster_number, fig_legend = NULL,
                      fig_ox_label = NULL) {
  ml_data <- ml_clustering(data, cluster_number)

  to_analyze = ml_data %>%
    dplyr::select(country, cluster) %>%
    dplyr::distinct()

  data_ml <- ml_data %>%
    tidyr::pivot_longer(cols = 2:11, names_to = 'decile', values_to = 'value') %>%
    dplyr::mutate(cluster = as.factor(cluster)) %>%
    dplyr::arrange(cluster) %>%
    dplyr::mutate(country = factor(country, levels = unique(country)))


  pl <- ggplot(data_ml,
               aes(y = factor(country), x = value, color = factor(decile))) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(
      values = pal_deciles,
      name = fig_legend
    ) +
    labs(x = fig_ox_label, y = "") +
    theme(axis.text = element_text(size = 12))
  ggsave(
    file = file.path("figures",paste0("ml_barchar_cluster",cluster_number,"_ordered.png")),
    height = 50, width = 20, units = "cm",
    plot = pl
  )


  world_cluster <- world %>%
    dplyr::select(country = name, geometry) %>%
    dplyr::left_join(
      to_analyze %>%
        mutate(country = ifelse(country == 'United States of America', 'United States', country)),
      by = "country"
    )
  world_cluster <- sf::st_sf(world_cluster, geometry = world_cluster$geometry)

  pl <- ggplot(data = world_cluster) +
    geom_sf(aes(fill = as.factor(cluster)), color = "white", size = 0.1) +
    scale_fill_brewer(palette = "Set3", na.value = "grey90", name = 'Cluster') +
    theme_void() +  # Clean background
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",  # Stack legends vertically
      legend.spacing = unit(1, "cm"),  # Increase vertical space between legends
      legend.title = element_text(vjust = 1.5, hjust = .5),  # Adjust legend title spacing
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # Center and bold title
    ) +
    labs(title = "")


}
