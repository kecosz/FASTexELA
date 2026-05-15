# FAST / ELA visualization support functions
# Generated from the user's notebook-style pasted code.
#
# Purpose
# -------
# This file collects helper functions for microbiome baseline analyses,
# stable-state diagram visualisation, community-shaping index plots,
# basin-size visualisation, 3D GELS surface plots, g_FAST coefficient plots,
# and signed interaction-network plots.
#
# How to use
# ----------
#   source("ela_visualization_support.R")
#   install_ela_visualization_dependencies()  # only if packages are missing
#   load_ela_visualization_packages()         # loads all available dependencies
#
# Notes
# -----
# * Notebook cell magics such as %%R were removed.
# * install.packages() calls from the original pasted code were removed.
#   Use install_ela_visualization_dependencies() explicitly when needed.
# * rELA is treated as an optional/local package. Functions that call
#   id2bin(), bin2id(), or rELA::SSestimate require rELA to be installed
#   and loaded in the R session.
# * Some functions are intentionally kept close to the original notebook
#   implementation to avoid changing analysis behaviour.

## ============================================================
## 00. Dependency management
##
## These helpers keep package installation separate from package loading.
## This avoids unexpected package updates when source() is called.
## ============================================================

ela_visualization_cran_packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "foreach",
  "doParallel",
  "RColorBrewer",
  "ggrepel",
  "patchwork",
  "ggraph",
  "igraph",
  "tidygraph",
  "tibble",
  "reshape2",
  "stringr",
  "vegan",
  "scales",
  "gridExtra",
  "plot3D",
  "grid"
)

ela_visualization_bioc_packages <- c(
  "ComplexHeatmap",
  "circlize"
)

ela_visualization_optional_packages <- c(
  "rELA"
)

install_ela_visualization_dependencies <- function(
  include_bioc = TRUE,
  include_optional = FALSE
) {
  cran_missing <- ela_visualization_cran_packages[
    !vapply(ela_visualization_cran_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(cran_missing) > 0) {
    utils::install.packages(cran_missing)
  }

  if (isTRUE(include_bioc)) {
    bioc_missing <- ela_visualization_bioc_packages[
      !vapply(ela_visualization_bioc_packages, requireNamespace, logical(1), quietly = TRUE)
    ]

    if (length(bioc_missing) > 0) {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        utils::install.packages("BiocManager")
      }

      BiocManager::install(
        bioc_missing,
        update = FALSE,
        ask = FALSE
      )
    }
  }

  if (isTRUE(include_optional)) {
    optional_missing <- ela_visualization_optional_packages[
      !vapply(ela_visualization_optional_packages, requireNamespace, logical(1), quietly = TRUE)
    ]

    if (length(optional_missing) > 0) {
      warning(
        "Optional/local packages are missing and were not installed automatically: ",
        paste(optional_missing, collapse = ", "),
        "\nInstall them manually if you need functions that depend on them.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

check_ela_visualization_dependencies <- function(
  include_bioc = TRUE,
  include_optional = FALSE,
  stop_if_missing = TRUE
) {
  pkgs <- ela_visualization_cran_packages

  if (isTRUE(include_bioc)) {
    pkgs <- c(pkgs, ela_visualization_bioc_packages)
  }

  if (isTRUE(include_optional)) {
    pkgs <- c(pkgs, ela_visualization_optional_packages)
  }

  missing <- pkgs[
    !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0 && isTRUE(stop_if_missing)) {
    stop(
      "Missing required packages: ",
      paste(missing, collapse = ", "),
      "\nRun install_ela_visualization_dependencies() and install optional/local packages if needed.",
      call. = FALSE
    )
  }

  invisible(missing)
}

load_ela_visualization_packages <- function(
  include_bioc = TRUE,
  include_optional = TRUE,
  fail_if_missing = FALSE
) {
  pkgs <- ela_visualization_cran_packages

  if (isTRUE(include_bioc)) {
    pkgs <- c(pkgs, ela_visualization_bioc_packages)
  }

  if (isTRUE(include_optional)) {
    pkgs <- c(pkgs, ela_visualization_optional_packages)
  }

  missing <- character(0)

  suppressPackageStartupMessages({
    for (pkg in pkgs) {
      if (requireNamespace(pkg, quietly = TRUE)) {
        library(pkg, character.only = TRUE)
      } else {
        missing <- c(missing, pkg)
      }
    }
  })

  if (length(missing) > 0) {
    msg <- paste0(
      "The following packages were not loaded because they are not installed: ",
      paste(missing, collapse = ", ")
    )

    if (isTRUE(fail_if_missing)) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  invisible(missing)
}

# Load whatever is already installed. Missing packages are warned about but
# do not prevent source() from completing. Call load_ela_visualization_packages(
# fail_if_missing = TRUE) if you want a strict dependency check.
try(load_ela_visualization_packages(fail_if_missing = FALSE), silent = TRUE)

## ============================================================
## 01. General file-output helpers
##
## These helpers standardise output paths and multi-format figure saving.
## Several downstream functions also keep their original local save helpers.
## ============================================================

ela_make_output_filename <- function(file, ext) {
  if (is.null(file)) {
    stop(
      "Please provide `file` without extension, e.g. file = 'ela_fast/figure_name'.",
      call. = FALSE
    )
  }

  base <- tools::file_path_sans_ext(file)
  filename <- paste0(base, ".", ext)

  out_dir <- dirname(filename)
  if (!dir.exists(out_dir) && out_dir != ".") {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  filename
}

ela_save_ggplot_multi <- function(
  plot,
  file,
  device = c("png", "eps"),
  width_in = 6.5,
  height_in = 6.5,
  res_dpi = 300,
  bg = "white"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  device <- match.arg(
    device,
    choices = c("png", "eps", "none"),
    several.ok = TRUE
  )
  device <- unique(device)

  output_files <- character(0)

  if ("png" %in% device) {
    png_file <- ela_make_output_filename(file, "png")
    ggplot2::ggsave(
      filename = png_file,
      plot = plot,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = res_dpi,
      bg = bg
    )
    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- ela_make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = plot,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = plot,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  invisible(output_files)
}



## ============================================================
## 02. Taxon-label and binary-matrix utilities
##
## These functions shorten long taxonomic strings and display binary
## community-composition matrices as simple black/white tile plots.
## ============================================================

shorten_taxon_label <- function(taxon_vec) {
  raw_labels <- sapply(taxon_vec, function(label) {
    # Replace trailing "__" with "__uc"
    label <- sub("__$", "__uc", label)

    # Split using taxonomic prefixes like d__, p__, etc.
    parts <- unlist(strsplit(label, "(^|\\.)[dpcofgs]__"))
    parts <- parts[parts != ""]

    # Reverse the parts for back-to-front search
    parts_rev <- rev(parts)

    # Define what is considered "uninformative"
    invalids <- c("uncultured", "human_gut", "gut_metagenome", "unclassified")
    is_valid <- function(x) {
      !(nchar(x) <= 4 || tolower(x) %in% invalids)
    }

    valid_pos <- which(sapply(parts_rev, is_valid))

    if (length(valid_pos) == 0) {
      return(NA_character_)
    }

    keep_parts <- parts_rev[1:valid_pos[1]]

    # Join and clean the final label
    final <- paste(rev(keep_parts), collapse = ".")
    final <- gsub("__", "", final)
    final <- sub("^\\.", "", final)

    return(final)
  }, USE.NAMES = FALSE)

  # Append suffixes to duplicates to make them unique
  counts <- ave(seq_along(raw_labels), raw_labels, FUN = seq_along)
  result <- ifelse(is.na(raw_labels), NA_character_,
                   ifelse(counts == 1, raw_labels, paste0(raw_labels, ".", counts)))

  return(result)
}

plot_binary_matrix <- function(mat) {
  mat <- as.matrix(mat)

  # Convert matrix to data frame
  df <- melt(mat)
  colnames(df) <- c("Row", "Column", "Value")

  # Convert row and column indices to numeric (for proper order and scale_y_reverse)
  df$Row <- as.numeric(df$Row)
  df$Column <- as.numeric(df$Column)

  # Plot
  ggplot(df, aes(x = Column, y = Row, fill = factor(Value))) +
    geom_tile(color = "grey") +
    scale_fill_manual(values = c("0" = "black", "1" = "white")) +
    scale_y_reverse() +  # Flip vertical axis (top to bottom like matrix)
    theme_minimal() +
    labs(title = "Binary Matrix Plot", fill = "Value") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    )
}


# depends on vegan, ggplot2, scales, dplyr
## ============================================================
## 03. Baseline microbiome analyses against a continuous phenotype
##
## This block computes alpha-diversity indices, taxon-wise Spearman
## correlations, RDA/CCA/NMDS/db-RDA ordinations, and PERMANOVA.
## It is intended for exploratory visualisation of microbiome structure
## along FAST score or another continuous environmental variable.
## ============================================================

microbiome_base_analysis <- function(community_matrix, env_matrix, env_name, distance="bray") {

  if (!(env_name %in% colnames(env_matrix))) {
    stop("The specified env_name does not exist in env_matrix.")
  }
  env_vector <- as.numeric(env_matrix[, env_name])

  if(length(env_vector) != nrow(community_matrix)) {
    stop("Length of env_vector must equal the number of samples (rows) in community_matrix")
  }

  community_matrix <- as.data.frame(community_matrix, check.names=FALSE)

  theme_large <- theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16)
  )

  ### 0. Alpha diversity (Shannon index)
  # Define a plotting function for each diversity index vs environment
  plot_alpha_div <- function(df, metric, env_name) {
    cor_test <- suppressWarnings(cor.test(df[[metric]], df$Env, method = "spearman"))
    corr_val <- sprintf("%.3f", cor_test$estimate)
    p_val    <- sprintf("%.3f", cor_test$p.value)

    ggplot(df, aes_string(x = "Env", y = metric)) +
      geom_point(color = "darkblue", size = 2.5) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      labs(
        x = expression(epsilon[FAST]),
        y = metric,
        title = paste0(metric, " vs ", env_name, "\n",
                      "Spearman \u03C1 = ", corr_val, ", p = ", p_val)
      ) +
      theme_minimal()
  }

  # Calculate alpha diversity indices
  shannon <- diversity(community_matrix, index = "shannon")
  simpson <- diversity(community_matrix, index = "simpson")
  observed_species <- specnumber(community_matrix)

  # Calculate Pielou's evenness: J = Shannon / log(S)
  pielou <- ifelse(observed_species > 1, shannon / log(observed_species), NA)

  # Combine all diversity indices into a single data frame (order adjusted)
  alpha_diversity_df <- data.frame(
    Sample = rownames(community_matrix),
    Observed = observed_species,
    Simpson = simpson,
    Shannon = shannon,
    Pielou = pielou,
    Env = env_vector
  )

  # Display the alpha diversity table
  print(alpha_diversity_df)

  # Generate individual plots (order adjusted)
  p1 <- plot_alpha_div(alpha_diversity_df, "Observed", env_name)
  p2 <- plot_alpha_div(alpha_diversity_df, "Simpson", env_name)
  p3 <- plot_alpha_div(alpha_diversity_df, "Shannon", env_name)
  p4 <- plot_alpha_div(alpha_diversity_df, "Pielou", env_name)

  # Arrange plots in a 2x2 grid
  alpha_grid <- grid.arrange(p1, p2, p3, p4, ncol = 2)

  # Perform Spearman correlation between each index and the environment variable (order adjusted)
  cor_results <- lapply(c("Observed", "Simpson", "Shannon", "Pielou"), function(metric) {
    ct <- suppressWarnings(cor.test(alpha_diversity_df[[metric]], alpha_diversity_df$Env, method = "spearman"))
    data.frame(Metric = metric, Spearman = ct$estimate, PValue = ct$p.value)
  })

  # Display the correlation results
  cor_results_df <- do.call(rbind, cor_results)
  print(cor_results_df)

  ### 1. Spearman correlation for each taxon
  rho <- numeric(ncol(community_matrix))
  pval <- numeric(ncol(community_matrix))
  for(i in seq_along(rho)) {
    xi <- community_matrix[[i]]
    test <- suppressWarnings(cor.test(xi, env_vector, method="spearman"))
    rho[i] <- if(!is.null(test$estimate)) test$estimate else NA
    pval[i] <- if(!is.null(test$p.value)) test$p.value else NA
  }
  padj <- p.adjust(pval, method="fdr")
  correlation_table <- data.frame(
    Taxon = colnames(community_matrix),
    SpearmanRho = rho,
    PValue = pval,
    FDR = padj,
    stringsAsFactors = FALSE
  )

  ### 1b. Correlation barplot (filtered by P < 0.1)
  cor_bar_data <- correlation_table %>%
    filter(PValue < 1.0) %>%
    mutate(Direction = ifelse(SpearmanRho > 0, "Positive", "Negative"),
           Color = ifelse(Direction == "Positive", "blue", "red")) %>%
    arrange(desc(SpearmanRho))

  cor_bar_data$Taxon <- factor(cor_bar_data$Taxon, levels = rev(cor_bar_data$Taxon))

  correlation_barplot <- ggplot(cor_bar_data, aes(x = SpearmanRho, y = Taxon, fill = Direction)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
    labs(x = "Spearman Correlation", y = "Taxon",
         title = "\u03C1_FAST") +
    theme_minimal()

  ### 2. RDA
  env_df <- data.frame(env=env_vector)
  colnames(env_df) <- env_name
  rda_model <- rda(community_matrix ~ ., data=env_df)
  rda_sites <- scores(rda_model, display="sites", choices=1:2, scaling=2)
  rda_sites_df <- as.data.frame(rda_sites)
  colnames(rda_sites_df) <- c("RDA1", "RDA2")
  rda_sites_df$Sample <- rownames(rda_sites_df)
  rda_sites_df$EnvValue <- env_vector
  rda_bp <- scores(rda_model, display="bp", choices=1:2, scaling=2)
  env_arrow <- as.data.frame(rda_bp)
  colnames(env_arrow) <- c("RDA1", "RDA2")
  env_arrow$Variable <- rownames(rda_bp)
  rda_plot <- ggplot() +
    geom_point(data=rda_sites_df, aes(x=RDA1, y=RDA2, color=EnvValue), size=3) +
    geom_segment(data=env_arrow, aes(x=0, y=0, xend=RDA1, yend=RDA2),
                 arrow=arrow(length=unit(0.3,"cm")), color="black") +
    geom_text(data=env_arrow, aes(x=RDA1, y=RDA2, label=Variable),
              color="black", vjust=-0.5, hjust=1, size=5) +
    scale_color_gradient(low="blue", high="red") +
    labs(title="RDA: Community ~ Continuous Variable", x="RDA1", y="RDA2", color=env_name) +
    theme_minimal() + theme_large


  ### 3. CCA
  cca_model <- cca(community_matrix ~ ., data=env_df)
  cca_sites <- scores(cca_model, display="sites", choices=1:2, scaling=2)
  cca_sites_df <- as.data.frame(cca_sites)
  colnames(cca_sites_df) <- c("CCA1", "CCA2")
  cca_sites_df$Sample <- rownames(cca_sites_df)
  cca_sites_df$EnvValue <- env_vector
  cca_bp <- scores(cca_model, display="bp", choices=1:2, scaling=2)
  env_arrow2 <- as.data.frame(cca_bp)
  colnames(env_arrow2) <- c("CCA1", "CCA2")
  env_arrow2$Variable <- rownames(cca_bp)
  cca_plot <- ggplot() +
    geom_point(data=cca_sites_df, aes(x=CCA1, y=CCA2, color=EnvValue), size=3) +
    geom_segment(data=env_arrow2, aes(x=0, y=0, xend=CCA1, yend=CCA2),
                 arrow=arrow(length=unit(0.3,"cm")), color="black") +
    geom_text(data=env_arrow2, aes(x=CCA1, y=CCA2, label=Variable),
              color="black", vjust=-0.5, hjust=1, size=5) +
    scale_color_gradient(low="blue", high="red") +
    labs(title="CCA: Community ~ Continuous Variable", x="CCA1", y="CCA2", color=env_name) +
    theme_minimal() + theme_large

  ### 4. NMDS + envfit
  nmds_model <- metaMDS(community_matrix, distance=distance, trace=FALSE)
  nmds_sites <- scores(nmds_model, display="sites")
  nmds_sites_df <- as.data.frame(nmds_sites)
  nmds_sites_df$Sample <- rownames(nmds_sites_df)
  nmds_sites_df$EnvValue <- env_vector
  env_fit <- envfit(nmds_model ~ ., data=env_df, perm=999)
  vf <- env_fit$vectors
  if(!is.null(vf$arrows)) {
    arrow_coords <- vf$arrows[1,] * sqrt(vf$r[1])
    arrow_df <- data.frame(NMDS1=arrow_coords[1], NMDS2=arrow_coords[2],
                           Variable=rownames(vf$arrows)[1])
  } else {
    arrow_df <- data.frame(NMDS1=numeric(), NMDS2=numeric(), Variable=character())
  }
  nmds_plot <- ggplot() +
    geom_point(data=nmds_sites_df, aes(x=NMDS1, y=NMDS2, color=EnvValue), size=3) +
    { if(nrow(arrow_df) > 0) geom_segment(data=arrow_df, aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
                 arrow=arrow(length=unit(0.3,"cm")), color="black") else NULL } +
    { if(nrow(arrow_df) > 0) geom_text(data=arrow_df, aes(x=NMDS1, y=NMDS2, label=Variable),
              color="black", vjust=-0.5, hjust=1, size=5) else NULL } +
    scale_color_gradient(low="blue", high="red") +
    labs(title=paste("NMDS (", distance, ") + Envfit", sep=""), x="NMDS1", y="NMDS2",
         subtitle=paste("Stress:", round(nmds_model$stress, 3)), color=env_name) +
    theme_minimal() + theme_large

  ### 5. Distance-based RDA (db-RDA)
  dbrda_model <- capscale(community_matrix ~ ., data=env_df, distance=distance)
  dbrda_sites <- scores(dbrda_model, display="sites", choices=1:2, scaling=1)
  dbrda_sites_df <- as.data.frame(dbrda_sites)
  colnames(dbrda_sites_df) <- c("dbRDA1", "dbRDA2")
  dbrda_sites_df$Sample <- rownames(dbrda_sites_df)
  dbrda_sites_df$EnvValue <- env_vector
  dbrda_bp <- scores(dbrda_model, display="bp", choices=1:2, scaling=1)
  env_arrow3 <- as.data.frame(dbrda_bp)
  colnames(env_arrow3) <- c("dbRDA1", "dbRDA2")
  env_arrow3$Variable <- rownames(dbrda_bp)
  dbrda_plot <- ggplot() +
    geom_point(data=dbrda_sites_df, aes(x=dbRDA1, y=dbRDA2, color=EnvValue), size=3) +
    geom_segment(data=env_arrow3, aes(x=0, y=0, xend=dbRDA1, yend=dbRDA2),
                 arrow=arrow(length=unit(0.3,"cm")), color="black") +
    geom_text(data=env_arrow3, aes(x=dbRDA1, y=dbRDA2, label=Variable),
              color="black", vjust=-0.5, hjust=1, size=5) +
    scale_color_gradient(low="blue", high="red") +
    labs(title=paste("db-RDA (", distance, " distance)", sep=""),
         x="dbRDA1", y="dbRDA2", color=env_name) +
    theme_minimal() + theme_large

  total_var <- dbrda_model$tot.chi
  cons_var <- dbrda_model$CCA$tot.chi
  R2 <- cons_var / total_var
  var_df <- data.frame(Component=c("Explained", "Residual"),
                       Variance=c(R2, 1-R2))
  dbrda_var_plot <- ggplot(var_df, aes(x=Component, y=Variance, fill=Component)) +
    geom_bar(stat="identity", width=0.6) +
    geom_text(aes(label=paste0(round(Variance*100,1), "%")), vjust=-0.5, size=6) +
    ylim(0,1) +
    labs(title="Variance Explained by Env (db-RDA)") +
    theme_minimal() + theme_large + theme(legend.position="none")

  ### 6. PERMANOVA
  dist_matrix <- vegdist(community_matrix, method=distance)
  adonis_res <- adonis2(dist_matrix ~ ., data=env_df, permutations=999)
  permanova_table <- as.data.frame(adonis_res)

  return(list(
    correlation_table = correlation_table,
    correlation_barplot = correlation_barplot,
    alpha_diversity_plot = alpha_grid,
    alpha_correlation = cor_results_df,
    rda_plot = rda_plot,
    cca_plot = cca_plot,
    nmds_plot = nmds_plot,
    dbrda_plot = dbrda_plot,
    dbrda_var_plot = dbrda_var_plot,
    permanova_table = permanova_table
  ))
}


# Required packages
#library(igraph)


#' Visualize a signed interaction network using spectral embedding
#'
#' @param mat Numeric matrix (square adjacency matrix with positive and negative weights)
#' @param edge_threshold Numeric | NULL: drop edges with |w| < threshold
#' @param normalize_coords Logical: normalize coordinates to 0–1 for comparison across networks
#' @param node_size Numeric: node size
#' @param edge_width_range Numeric vector: range of edge width (mapped to |weight|)
#' @param palette_posneg Vector of two colors (positive, negative)
#' @param title Plot title
#' @return A ggplot (ggraph) object
## ============================================================
## 04. Signed interaction network: spectral embedding
##
## This plot represents a signed interaction matrix as a graph and
## places nodes using a signed-Laplacian spectral embedding.
## Positive and negative edges are colored separately.
## ============================================================

plot_signed_network_spectral <- function(
  mat,
  edge_threshold = NULL,
  normalize_coords = FALSE,
  node_size = 3,
  label_size = 3,                        # NEW: node label text size
  edge_width_range = c(0.2, 2.5),
  palette_posneg = c("#D55E00", "#0072B2"), # positive:red / negative:blue
  title = "Signed network (spectral embedding)"
) {
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  mat <- as.matrix(mat)
  if (nrow(mat) != ncol(mat)) stop("mat must be a square matrix.")

  # Symmetrize and remove self-loops
  A <- (mat + t(mat)) / 2
  diag(A) <- 0

  # Thresholding (remove weak edges)
  if (!is.null(edge_threshold)) {
    A[abs(A) < edge_threshold] <- 0
  }

  if (all(A == 0)) stop("All edges are zero after thresholding.")

  # ---- Signed Laplacian ----
  # Signed degree matrix: D^+ = diag(rowSums(|A|))
  Dplus <- diag(rowSums(abs(A)))
  Ls <- Dplus - A  # Kunegis et al. (2010) definition

  # Eigen decomposition (symmetric matrix)
  ee <- eigen(Ls, symmetric = TRUE)
  ord <- order(ee$values, decreasing = FALSE)
  vals <- ee$values[ord]
  vecs <- ee$vectors[, ord, drop = FALSE]

  # Skip near-zero eigenvalues (trivial component)
  eps <- max(.Machine$double.eps, 1e-9)
  nontrivial_idx <- which(vals > eps)
  if (length(nontrivial_idx) < 2) {
    coords <- vecs[, 1:2, drop = FALSE]
  } else {
    k2 <- nontrivial_idx[1:2]
    coords <- vecs[, k2, drop = FALSE]
  }
  colnames(coords) <- c("x", "y")

  # Optional coordinate normalization (0–1 range)
  if (normalize_coords) {
    norm01 <- function(v) if (diff(range(v)) == 0) v else (v - min(v)) / diff(range(v))
    coords[, "x"] <- norm01(coords[, "x"])
    coords[, "y"] <- norm01(coords[, "y"])
  }

  # Node names
  node_names <- rownames(A)
  if (is.null(node_names)) node_names <- as.character(seq_len(nrow(A)))

  # Create igraph object
  g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
  V(g)$name <- node_names

  # Edge attributes
  E(g)$sign <- ifelse(E(g)$weight >= 0, "positive", "negative")
  E(g)$absw <- abs(E(g)$weight)

  # Convert to tidygraph
  tg <- as_tbl_graph(g) %>%
    activate(nodes) %>%
    mutate(x = coords[, "x"], y = coords[, "y"])

  # Visualization
  p <- ggraph(tg, layout = "manual", x = x, y = y) +
    geom_edge_link(aes(color = sign, width = absw), alpha = 0.6, show.legend = TRUE) +
    geom_node_point(size = node_size, color = "grey20") +
    geom_node_text(aes(label = name), repel = TRUE, size = label_size) +  # adjustable label size
    scale_edge_color_manual(values = c(positive = palette_posneg[1],
                                       negative = palette_posneg[2])) +
    scale_edge_width(range = edge_width_range, guide = "none") +
    labs(title = title, edge_color = "Sign") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),      # remove axis titles
      axis.text  = element_blank(),      # remove axis tick labels
      axis.ticks = element_blank(),      # remove axis ticks
      plot.margin = margin(5, 5, 5, 5)   # small margin
    )

  return(p)
}


## Helper functions
## ------------------------------------------------------------

## ============================================================
## 05. Stable-state diagram grouping utilities
##
## These helpers group stable-state fragments across environmental
## points using Hamming distance and trajectory continuity in energy.
## They support showSSD2(), which visualises stable-state energy
## trajectories and can filter groups by observed stable states.
## ============================================================

hamming_dist <- function(x, y) {
  sum(x != y)
}

na_range <- function(v) {
  inds <- which(!is.na(v))
  if (length(inds) == 0) return(NULL)
  range(inds)
}

is_consecutive <- function(en1, en2, ecrit) {
  range1 <- na_range(en1)
  range2 <- na_range(en2)

  if (is.null(range1) || is.null(range2)) return(FALSE)

  last1 <- range1[length(range1)]
  first2 <- range2[1]

  (first2 - last1 == 1) &&
    abs(en1[last1] - en2[first2]) < ecrit
}

max_consecutive_diff <- function(stats) {
  max_diffs <- sapply(stats, function(entry) {
    values <- entry[[2]]
    values <- values[!is.na(values)]

    if (length(values) < 2) return(NA_real_)

    max(abs(diff(values)))
  })

  max(max_diffs, na.rm = TRUE)
}

reorder_group_list <- function(group_list, ecrit = 0.1) {
  if (length(group_list) <= 1) return(group_list)

  ordering_info <- lapply(group_list, function(g) {
    energy_a <- g[[1]][[2]]
    range_a <- na_range(energy_a)

    energy_z <- g[[length(g)]][[2]]
    range_z <- na_range(energy_z)

    if (is.null(range_a) || is.null(range_z)) {
      return(c(Inf, -Inf))
    }

    c(range_a[1], range_z[length(range_z)] - range_a[1] + 1)
  })

  ordering_matrix <- do.call(rbind, ordering_info)
  ordering_index <- order(ordering_matrix[, 1], -ordering_matrix[, 2])

  group_list <- group_list[ordering_index]

  n <- length(group_list)

  if (n > 1) {
    for (i in seq_len(n - 1)) {
      ref_energy <- group_list[[i]][[length(group_list[[i]])]][[2]]

      for (j in seq.int(i + 1, n)) {
        test_energy <- group_list[[j]][[1]][[2]]

        if (is_consecutive(ref_energy, test_energy, ecrit)) {
          moving <- group_list[[j]]
          group_list <- append(group_list[-j], list(moving), after = i)
          break
        }
      }
    }
  }

  group_list
}

merge_energy_vectors <- function(gf_list) {
  energy_mat <- do.call(
    rbind,
    lapply(gf_list, function(x) x[[2]])
  )

  apply(energy_mat, 2, function(col) {
    vals <- col[!is.na(col)]
    if (length(vals) == 0) NA_real_ else max(vals)
  })
}

group_fragments <- function(parent_list, min_group_size = 1, min_distance = 1) {
  result <- list()

  the <- 1.2 * max_consecutive_diff(parent_list)

  while (length(parent_list) > 0) {
    current <- parent_list[[1]]
    parent_list <- parent_list[-1]

    group <- list(current)
    current_bin <- current[[1]]
    current_en <- current[[2]]

    i <- 1

    while (i <= length(parent_list)) {
      candidate <- parent_list[[i]]
      candidate_bin <- candidate[[1]]
      candidate_en <- candidate[[2]]

      if (
        hamming_dist(current_bin, candidate_bin) <= min_distance &&
          is_consecutive(current_en, candidate_en, the)
      ) {
        group <- append(group, list(candidate))

        current <- candidate
        current_bin <- current[[1]]
        current_en <- current[[2]]

        parent_list <- parent_list[-i]
        i <- 1
      } else {
        i <- i + 1
      }
    }

    len_group <- sum(unlist(lapply(group, function(x) {
      length(x[[2]][!is.na(x[[2]])])
    })))

    if (len_group >= min_group_size) {
      result <- append(result, list(group))
    }
  }

  reorder_group_list(result, the)
}

## ------------------------------------------------------------
## Helper functions for filtering groups by observed data in gstab
## ------------------------------------------------------------

find_matching_groups <- function(vlist, groups) {
  result <- integer(length(vlist))

  for (i in seq_along(vlist)) {
    target <- vlist[[i]]
    matched <- FALSE

    for (g in seq_along(groups)) {
      group <- groups[[g]]

      for (subg in seq_along(group)) {
        ref <- group[[subg]][[1]]

        if (length(target) == length(ref) && all(target == ref)) {
          result[i] <- g
          matched <- TRUE
          break
        }
      }

      if (matched) break
    }

    if (!matched) result[i] <- 0L
  }

  result
}

get_observed_group_ids_from_gstab <- function(gstab, groups, nspecies) {
  if (is.null(gstab)) {
    stop("gstab must be provided when filter_by_gstab = TRUE.")
  }

  if (!is.list(gstab) || length(gstab) < 1) {
    stop("gstab must be a list whose first element contains stable.state.id.")
  }

  gstab_df <- gstab[[1]]

  if (!("stable.state.id" %in% colnames(gstab_df))) {
    stop("gstab[[1]] must contain a column named 'stable.state.id'.")
  }

  stable_ids <- gstab_df$stable.state.id
  stable_ids <- stable_ids[!is.na(stable_ids)]

  if (length(stable_ids) == 0) {
    stop("No non-NA stable.state.id values were found in gstab.")
  }

  ssid_bin <- lapply(
    stable_ids,
    function(x) id2bin(x, nspecies)
  )

  observed_group_ids <- find_matching_groups(ssid_bin, groups)

  sort(unique(observed_group_ids[observed_group_ids != 0L]))
}

## ------------------------------------------------------------
## showSSD2 with optional filtering by gstab
## ------------------------------------------------------------

showSSD2 <- function(
  gela,
  sa,
  gstab = NULL,
  filter_by_gstab = FALSE,
  report_filter = TRUE,

  grouping        = FALSE,
  min_group_size  = 1,
  min_distance    = 1,

  device          = "screen",
  file            = NULL,
  width_px        = 1800,
  height_px       = 1800,
  res_dpi         = 300,
  bg              = "white",
  xlab            = NULL
) {

  ## ------------------------------------------------------------
  ## Device handling
  ## ------------------------------------------------------------

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  ## If "none" is combined with real devices, ignore "none"
  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Initial setup
  ## ------------------------------------------------------------

  gf <- NULL
  plot_obj <- NULL
  output_files <- character(0)

  nspecies <- length(sa[[1]][, 1])
  factor <- gela[[2]]

  mem <- foreach::foreach(i = gela[[1]]) %do% {
    i[[1]][[1]]
  }

  uqss <- unique(unlist(mem))

  memen <- foreach::foreach(i = gela[[1]]) %do% {
    i[[1]][[2]]
  }

  minen <- min(unlist(memen), na.rm = TRUE)
  maxen <- max(unlist(memen), na.rm = TRUE)

  max_set1 <- RColorBrewer::brewer.pal.info["Set1", "maxcolors"]
  cols_base <- RColorBrewer::brewer.pal(max_set1, "Set1")
  cols <- rep(cols_base, times = 100)

  yrange <- c(
    minen - abs(0.05 * minen),
    maxen + abs(0.05 * maxen)
  )

  ## ------------------------------------------------------------
  ## Group stable-state fragments
  ## ------------------------------------------------------------

  if (grouping) {

    stats <- foreach::foreach(j = uqss) %do% {
      po <- foreach::foreach(i = gela[[1]]) %do% {
        pp <- which(i[[1]][[1]] == j)
        if (identical(pp, integer(0))) NA else i[[1]][[2]][[pp]]
      }

      bing <- id2bin(j, nspecies)
      energy <- unlist(po)

      list(bing, energy)
    }

    gf <- group_fragments(
      stats,
      min_group_size = min_group_size,
      min_distance = min_distance
    )

    ## This was present in the previous version but is not used later.
    ## Kept commented out to avoid unnecessary memory use.
    ## invisible(lapply(gf, merge_energy_vectors))

    ## ------------------------------------------------------------
    ## Optional filtering by observed stable states in gstab
    ## ------------------------------------------------------------

    if (filter_by_gstab) {

      keep_group_ids <- get_observed_group_ids_from_gstab(
        gstab = gstab,
        groups = gf,
        nspecies = nspecies
      )

      if (length(keep_group_ids) == 0) {
        stop("No stable-state groups matched observed samples in gstab.")
      }

      if (report_filter) {
        cat("Keeping groups represented by observed samples in gstab:\n")
        cat(
          paste0(
            "  old C", keep_group_ids,
            " -> new C", seq_along(keep_group_ids),
            collapse = "\n"
          ),
          "\n"
        )
      }

      gf <- gf[keep_group_ids]

      ## Store original group IDs for later reference
      attr(gf, "original_group_id") <- keep_group_ids
    }

  } else {

    if (filter_by_gstab) {
      warning("filter_by_gstab is ignored because grouping = FALSE.")
    }
  }

  ## ------------------------------------------------------------
  ## Drawing function
  ## ------------------------------------------------------------

  draw_base_plot <- function() {

    cex.lab <- 1.4
    cex.axis <- 1.2
    cex.legend <- 1.0

    op <- par(xpd = TRUE, mar = c(6, 6, 3, 6))
    on.exit(par(op), add = TRUE)

    if (!grouping) {

      cc <- 0

      for (j in uqss) {

        cc <- cc + 1

        po <- foreach::foreach(i = gela[[1]]) %do% {
          pp <- which(i[[1]][[1]] == j)
          if (identical(pp, integer(0))) NA else i[[1]][[2]][[pp]]
        }

        energy <- unlist(po)

        par(new = TRUE)

        if (cc == 1) {

          env <- gela[[3]]

          plot(
            factor,
            energy,
            type = "o",
            ylim = yrange,
            col = cols[cc],
            xlab = names(env[is.na(env)]),
            ylab = "Energy",
            cex.lab = cex.lab,
            cex.axis = cex.axis
          )

        } else {

          plot(
            factor,
            energy,
            type = "o",
            ylim = yrange,
            ann = FALSE,
            col = cols[cc],
            cex.axis = cex.axis
          )
        }
      }

      legend(
        par()$usr[2] + 0.2,
        par()$usr[4],
        legend = uqss,
        col = cols,
        pch = 1,
        lty = 1,
        cex = cex.legend
      )

    } else {

      pch_set <- c(1, 16, 4, 2, 15)

      cc <- 0
      legend_labels <- c()
      legend_cols <- c()
      legend_pchs <- c()

      for (g in seq_along(gf)) {

        sublist <- gf[[g]]

        for (subg in seq_along(sublist)) {

          cc <- cc + 1

          energy <- sublist[[subg]][[2]]
          col_idx <- g
          pch_idx <- pch_set[(subg - 1) %% length(pch_set) + 1]

          par(new = TRUE)

          if (cc == 1) {

            env <- gela[[3]]

            if (is.null(xlab)) {
              local_xlab <- names(env[is.na(env)])
            } else {
              local_xlab <- xlab
            }

            plot(
              factor,
              energy,
              type = "o",
              ylim = yrange,
              col = cols[col_idx],
              pch = pch_idx,
              xlab = local_xlab,
              ylab = "Energy",
              cex.lab = cex.lab,
              cex.axis = cex.axis
            )

          } else {

            plot(
              factor,
              energy,
              type = "o",
              ylim = yrange,
              ann = FALSE,
              col = cols[col_idx],
              pch = pch_idx,
              cex.axis = cex.axis
            )
          }

          legend_labels <- c(legend_labels, paste0("C", g, ".", subg))
          legend_cols <- c(legend_cols, cols[col_idx])
          legend_pchs <- c(legend_pchs, pch_idx)
        }
      }

      legend(
        par()$usr[2] + 0.2,
        par()$usr[4],
        legend = legend_labels,
        col = legend_cols,
        pch = legend_pchs,
        lty = 1,
        cex = cex.legend
      )
    }
  }

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {

    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/ssd_observed_only'."
      )
    }

    ## If user passed .png or .eps, remove extension first
    base <- tools::file_path_sans_ext(file)

    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  width_in  <- width_px / res_dpi
  height_in <- height_px / res_dpi

  ## ------------------------------------------------------------
  ## Render
  ## ------------------------------------------------------------

  if ("screen" %in% device) {

    draw_base_plot()
    plot_obj <- grDevices::recordPlot()
  }

  if ("png" %in% device) {

    png_file <- make_output_filename(file, "png")

    grDevices::png(
      filename = png_file,
      width = width_px,
      height = height_px,
      res = res_dpi,
      units = "px",
      bg = bg
    )

    draw_base_plot()

    grDevices::dev.off()

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {

    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {

      grDevices::cairo_ps(
        filename = eps_file,
        width = width_in,
        height = height_in,
        onefile = FALSE,
        bg = bg,
        fallback_resolution = res_dpi
      )

    } else {

      grDevices::postscript(
        file = eps_file,
        width = width_in,
        height = height_in,
        onefile = FALSE,
        horizontal = FALSE,
        paper = "special",
        bg = bg
      )
    }

    draw_base_plot()

    grDevices::dev.off()

    output_files <- c(output_files, eps_file)
  }

  if (identical(device, "none")) {
    plot_obj <- NULL
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    groups = gf,
    plot = plot_obj,
    params = list(
      grouping = grouping,
      min_group_size = min_group_size,
      min_distance = min_distance,
      filter_by_gstab = filter_by_gstab,
      original_group_id = if (!is.null(gf)) attr(gf, "original_group_id") else NULL,
      device = device,
      file = file,
      output_files = output_files,
      width_px = width_px,
      height_px = height_px,
      res_dpi = res_dpi
    )
  )

  return(out)
}


########################################


## ============================================================
## 06. Stable-state group membership tile plot
##
## This plot displays which taxa are present in each stable-state
## subgroup. It is useful for comparing community compositions
## across grouped stable states.
## ============================================================

ss_group_plot <- function(
  ssd_grouping,
  order = NULL,

  device   = "screen",
  file     = NULL,
  width_px = 1800,
  height_px = 1800,
  res_dpi  = 300,
  bg       = "white",

  taxa_names = NULL
) {

  ## ------------------------------------------------------------
  ## Device handling
  ## ------------------------------------------------------------

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Build community-composition matrix
  ## ------------------------------------------------------------

  mats <- lapply(
    ssd_grouping,
    function(y) {
      t(do.call(rbind, unique(lapply(y, function(x) x[[1]]))))
    }
  )

  for (i in seq_along(mats)) {
    mat <- mats[[i]]
    colnames(mat) <- paste0("C", i, ".", seq_len(ncol(mat)))
    mats[[i]] <- mat
  }

  combined_mats <- do.call(cbind, mats)

  ## Taxa names
  if (is.null(taxa_names)) {
    if (exists("ocmat", inherits = TRUE)) {
      taxa_names <- colnames(get("ocmat", inherits = TRUE))
    } else {
      taxa_names <- paste0("Taxon_", seq_len(nrow(combined_mats)))
    }
  }

  rownames(combined_mats) <- taxa_names

  if (!is.null(order)) {
    combined_mats <- combined_mats[order, , drop = FALSE]
  }

  ## Remove taxa with all zero values
  ss.tab <- combined_mats[
    rowSums(combined_mats) > 0,
    ,
    drop = FALSE
  ]

  ## ------------------------------------------------------------
  ## Convert to long format
  ## ------------------------------------------------------------

  df <- reshape2::melt(ss.tab)

  df$group <- stringr::str_extract(df$Var2, "(?<=C)\\d+")

  df$fill_color <- ifelse(df$value == 0, "black", df$group)

  group_levels <- sort(unique(df$group))

  ## RColorBrewer Set1 supports up to 9 colors; extend if needed
  if (length(group_levels) <= 9) {
    group_palette <- RColorBrewer::brewer.pal(
      max(length(group_levels), 3),
      "Set1"
    )[seq_along(group_levels)]
  } else {
    group_palette <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(9, "Set1")
    )(length(group_levels))
  }

  names(group_palette) <- group_levels

  palette <- c(black = "black", group_palette)

  ## ------------------------------------------------------------
  ## Create plot
  ## ------------------------------------------------------------

  p <- ggplot(df, aes(x = Var2, y = Var1)) +
    geom_tile(aes(fill = fill_color), color = "black") +
    scale_fill_manual(values = palette, guide = "none") +
    theme_minimal() +
    labs(x = "Community composition", y = NULL) +
    scale_y_discrete(limits = unique(df$Var1), position = "right") +
    scale_x_discrete(limits = unique(df$Var2), position = "top") +
    theme(
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 12
      ),
      axis.text.y.right = element_text(
        angle = 0,
        hjust = 0,
        size = 12
      ),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.title.y = element_blank(),
      plot.background = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA)
    )

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/ss_group_plot'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  width_in  <- width_px / res_dpi
  height_in <- height_px / res_dpi

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(p)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggsave(
      filename = png_file,
      plot = p,
      width = width_px,
      height = height_px,
      units = "px",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggsave(
        filename = eps_file,
        plot = p,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggsave(
        filename = eps_file,
        plot = p,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  attr(p, "output_files") <- output_files

  return(p)
}


## ============================================================
## 07. SSD group Hamming distance/similarity heatmap
##
## This heatmap summarises pairwise Hamming distances or similarities
## among stable-state subgroup membership vectors.
## ============================================================

ssd_group_heatmap <- function(
  ssd_grouping,
  mode = c("distance", "similarity"),
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  show_row_dend = TRUE,
  show_column_dend = TRUE,
  name = NULL,

  device   = "screen",
  file     = NULL,
  width_px = 1600,
  height_px = 1400,
  res_dpi  = 300,
  bg       = "white"
) {

  ## ------------------------------------------------------------
  ## Required packages
  ## ------------------------------------------------------------
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("Package 'ComplexHeatmap' is required.")
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package 'circlize' is required.")
  }

  ## ------------------------------------------------------------
  ## Argument handling
  ## ------------------------------------------------------------
  mode <- match.arg(mode)

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )
  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Extract membership vectors and labels
  ## ------------------------------------------------------------
  memb_list <- list()
  labels <- character()

  for (g in seq_along(ssd_grouping)) {
    subgs <- ssd_grouping[[g]]

    for (s in seq_along(subgs)) {
      m <- as.integer(subgs[[s]][[1]])
      memb_list[[length(memb_list) + 1]] <- m
      labels[length(labels) + 1] <- paste0("C", g, ".", s)
    }
  }

  if (length(memb_list) == 0) {
    stop("ssd_grouping is empty.")
  }

  L <- length(memb_list[[1]])
  n <- length(memb_list)

  if (n == 0 || L == 0) {
    stop("No valid membership vectors were found.")
  }

  ## ------------------------------------------------------------
  ## Hamming distance matrix (normalized 0-1)
  ## ------------------------------------------------------------
  D <- matrix(0, n, n, dimnames = list(labels, labels))

  for (i in seq_len(n)) {
    for (j in i:n) {
      d <- sum(memb_list[[i]] != memb_list[[j]]) / L
      D[i, j] <- D[j, i] <- d
    }
  }

  ## Similarity
  S <- 1 - D

  ## ------------------------------------------------------------
  ## Choose matrix and color mapping
  ## ------------------------------------------------------------
  if (mode == "distance") {
    mat_to_plot <- D

    if (is.null(name)) {
      name <- "Hamming\n(distance)"
    }

    col_fun <- circlize::colorRamp2(
      c(0, 0.5, 1),
      c("#FFFFFF", "#1C9099", "#034E7B")
    )

  } else {
    mat_to_plot <- S

    if (is.null(name)) {
      name <- "Hamming\n(similarity)"
    }

    col_fun <- circlize::colorRamp2(
      c(0, 0.5, 1),
      c("#034E7B", "#1C9099", "#FFFFFF")
    )
  }

  ## ------------------------------------------------------------
  ## Build heatmap object
  ## ------------------------------------------------------------
  ht <- ComplexHeatmap::Heatmap(
    mat_to_plot,
    name = name,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    col = col_fun,
    show_row_dend = show_row_dend,
    show_column_dend = show_column_dend
  )

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------
  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/group_dist_map'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)
    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  width_in  <- width_px / res_dpi
  height_in <- height_px / res_dpi

  ht_drawn <- NULL

  ## ------------------------------------------------------------
  ## Screen output
  ## ------------------------------------------------------------
  if ("screen" %in% device) {
    ht_drawn <- ComplexHeatmap::draw(ht)
  }

  ## ------------------------------------------------------------
  ## PNG output
  ## ------------------------------------------------------------
  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    grDevices::png(
      filename = png_file,
      width = width_px,
      height = height_px,
      res = res_dpi,
      units = "px",
      bg = bg
    )
    ComplexHeatmap::draw(ht)
    grDevices::dev.off()

    output_files <- c(output_files, png_file)
  }

  ## ------------------------------------------------------------
  ## EPS output
  ## ------------------------------------------------------------
  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      grDevices::cairo_ps(
        filename = eps_file,
        width = width_in,
        height = height_in,
        onefile = FALSE,
        bg = bg,
        fallback_resolution = res_dpi
      )
    } else {
      grDevices::postscript(
        file = eps_file,
        width = width_in,
        height = height_in,
        onefile = FALSE,
        horizontal = FALSE,
        paper = "special",
        bg = bg
      )
    }

    ComplexHeatmap::draw(ht)
    grDevices::dev.off()

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------
  out <- list(
    distance_matrix = D,
    similarity_matrix = S,
    heatmap = ht,
    heatmap_drawn = ht_drawn,
    params = list(
      mode = mode,
      cluster_rows = cluster_rows,
      cluster_columns = cluster_columns,
      show_row_dend = show_row_dend,
      show_column_dend = show_column_dend,
      name = name,
      device = device,
      file = file,
      output_files = output_files,
      width_px = width_px,
      height_px = height_px,
      res_dpi = res_dpi
    )
  )

  return(out)
}

## ------------------------------------------------------------
## Reusable helper:
## Build group-level membership matrix from ssd_grouping
## ------------------------------------------------------------

## ============================================================
## 08. Community Shaping Index (CSI) heatmap
##
## These functions collapse stable-state subgroups to group-level
## membership, compute the Community Shaping Index from J, and visualise
## taxa-by-community CSI values as a heatmap.
## ============================================================

membership_from_ssd <- function(ssd_grouping) {
  groups <- lapply(seq_along(ssd_grouping), function(g) {
    subgs <- ssd_grouping[[g]]

    memb_list <- lapply(
      subgs,
      function(subg) as.integer(subg[[1]])
    )

    ## logical OR across subgroups -> group-level membership
    Reduce(
      function(a, b) as.integer((a + b) > 0),
      memb_list
    )
  })

  M <- do.call(cbind, groups)
  colnames(M) <- paste0("C", seq_along(groups))

  M
}


## ------------------------------------------------------------
## Reusable helper:
## Compute Community Shaping Index
## ------------------------------------------------------------

compute_community_shaping_index <- function(je, M) {
  je <- as.matrix(je)
  M  <- as.matrix(M)

  if (nrow(je) != ncol(je)) {
    stop("`je` must be a square matrix.")
  }

  if (nrow(M) != nrow(je)) {
    stop("nrow(M) must match nrow(je).")
  }

  diag(je) <- 0

  Tmat <- 2 * M - 1

  SI <- Tmat * (je %*% Tmat)

  rownames(SI) <- rownames(je)
  colnames(SI) <- colnames(M)

  SI
}


## ------------------------------------------------------------
## Plot CSI heatmap
## Uses external membership_from_ssd() and
## compute_community_shaping_index()
## ------------------------------------------------------------

plot_CSI_heatmap <- function(
  ssd_grouping,
  je,
  M = NULL,
  CSI = NULL,
  order = NULL,
  taxa_names = NULL,

  device   = "screen",
  file     = NULL,

  width_in  = 5.2,
  height_in = 5.2,
  res_dpi   = 300,
  bg        = "white",

  xlab = NULL,
  ylab = NULL,
  fill_lab = "CSI",

  low = NULL,
  mid = NULL,
  high = NULL,
  midpoint = 0,

  base_size = NULL,
  axis_text_x_size = NULL,
  axis_text_y_size = NULL,
  tile_border = FALSE
) {

  ## ------------------------------------------------------------
  ## Required packages
  ## ------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required.")
  }

  ## ------------------------------------------------------------
  ## Device handling
  ## ------------------------------------------------------------

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Prepare je, taxa names, M, and CSI
  ## ------------------------------------------------------------

  je <- as.matrix(je)

  if (is.null(taxa_names)) {
    if (!is.null(rownames(je))) {
      taxa_names <- rownames(je)
    } else if (exists("ocmat", inherits = TRUE)) {
      taxa_names <- colnames(get("ocmat", inherits = TRUE))
    } else {
      taxa_names <- paste0("Taxon_", seq_len(nrow(je)))
    }
  }

  rownames(je) <- taxa_names
  colnames(je) <- taxa_names

  if (is.null(M)) {
    M <- membership_from_ssd(ssd_grouping)
  }

  M <- as.matrix(M)
  rownames(M) <- taxa_names

  if (is.null(CSI)) {
    CSI <- compute_community_shaping_index(je, M)
  }

  CSI <- as.matrix(CSI)

  if (!is.null(order)) {
    CSI <- CSI[order, , drop = FALSE]
  }

  ## ------------------------------------------------------------
  ## Long format for ggplot
  ## ------------------------------------------------------------

  df_CSI <- as.data.frame(CSI)
  df_CSI <- tibble::rownames_to_column(df_CSI, "species")
  df_CSI <- tidyr::pivot_longer(
    df_CSI,
    cols = -species,
    names_to = "group",
    values_to = "CSI"
  )

  df_CSI$species <- factor(
    df_CSI$species,
    levels = rownames(CSI)
  )

  df_CSI$group <- factor(
    df_CSI$group,
    levels = colnames(CSI)
  )

  ## ------------------------------------------------------------
  ## Plot
  ## ------------------------------------------------------------

  tile_layer <- if (tile_border) {
    ggplot2::geom_tile(color = "grey80", linewidth = 0.2)
  } else {
    ggplot2::geom_tile()
  }

  theme_base <- if (is.null(base_size)) {
    ggplot2::theme_minimal()
  } else {
    ggplot2::theme_minimal(base_size = base_size)
  }

  p <- ggplot2::ggplot(
    df_CSI,
    ggplot2::aes(x = group, y = species, fill = CSI)
  ) +
    ggplot2::scale_x_discrete(position = "top") +
    tile_layer

  if (is.null(low) || is.null(mid) || is.null(high)) {
    p <- p +
      ggplot2::scale_fill_gradient2(name = fill_lab)
  } else {
    p <- p +
      ggplot2::scale_fill_gradient2(
        low = low,
        mid = mid,
        high = high,
        midpoint = midpoint,
        name = fill_lab
      )
  }

  p <- p +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    theme_base +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, color = NA),
      panel.background = ggplot2::element_rect(fill = bg, color = NA)
    )

  if (!is.null(axis_text_x_size) || !is.null(axis_text_y_size)) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size = axis_text_x_size,
          angle = 0,
          vjust = 0.5,
          hjust = 0.5
        ),
        axis.text.y = ggplot2::element_text(
          size = axis_text_y_size
        )
      )
  }

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/csi'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  width_px  <- width_in * res_dpi
  height_px <- height_in * res_dpi

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(p)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggplot2::ggsave(
      filename = png_file,
      plot = p,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    membership = M,
    CSI = CSI,
    data = df_CSI,
    plot = p,
    params = list(
      device = device,
      file = file,
      output_files = output_files,
      width_in = width_in,
      height_in = height_in,
      width_px = width_px,
      height_px = height_px,
      res_dpi = res_dpi
    )
  )

  return(out)
}

## Required packages


## ============================================================
## 09. CSI versus g_FAST multi-panel scatter plots
##
## This figure evaluates, for each community group, whether taxon-level
## CSI values relate to g_FAST coefficients. Empty panel slots are kept
## blank when the requested grid is larger than the number of panels.
## ============================================================

plot_CSI_gFAST_panels <- function(
  M,
  CSI,
  ge,
  panel_ids = NULL,

  panel_ncol = 4,
  panel_nrow = 2,

  panel_tags = TRUE,
  panel_tag_case = c("lower", "upper"),
  panel_tag_labels = NULL,

  common_axis = TRUE,
  xlim_range = NULL,
  ylim_range = NULL,
  pad_ratio = 0.01,

  point_color = "darkgreen",
  point_size = 2.5,
  label_size = 2.8,
  max_overlaps = 50,
  smooth = TRUE,

  device = "screen",
  file = NULL,
  width_px = 3600,
  height_px = 1950,
  res_dpi = 300,
  bg = "white"
) {

  ## ------------------------------------------------------------
  ## Required packages
  ## ------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required.")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required.")
  }

  ## ------------------------------------------------------------
  ## Argument handling
  ## ------------------------------------------------------------

  panel_tag_case <- match.arg(panel_tag_case)

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Input checks
  ## ------------------------------------------------------------

  M <- as.matrix(M)
  CSI <- as.matrix(CSI)

  if (nrow(M) != nrow(CSI)) {
    stop("M and CSI must have the same number of rows.")
  }

  if (ncol(M) != ncol(CSI)) {
    stop("M and CSI must have the same number of columns.")
  }

  if (length(ge) != nrow(M)) {
    stop("length(ge) must match nrow(M).")
  }

  if (is.null(rownames(M))) {
    rownames(M) <- paste0("Taxon_", seq_len(nrow(M)))
  }

  if (is.null(rownames(CSI))) {
    rownames(CSI) <- rownames(M)
  }

  if (is.null(colnames(M))) {
    colnames(M) <- paste0("C", seq_len(ncol(M)))
  }

  if (is.null(colnames(CSI))) {
    colnames(CSI) <- colnames(M)
  }

  ge <- as.numeric(ge)
  names(ge) <- rownames(M)

  if (is.null(panel_ids)) {
    panel_ids <- seq_len(ncol(M))
  }

  ## Allow either numeric indices or column names
  if (is.character(panel_ids)) {
    panel_ids <- match(panel_ids, colnames(M))
  }

  panel_ids <- panel_ids[!is.na(panel_ids)]
  panel_ids <- panel_ids[panel_ids >= 1 & panel_ids <= ncol(M)]

  if (length(panel_ids) == 0) {
    stop("No valid panel_ids were provided.")
  }

  panel_capacity <- panel_ncol * panel_nrow

  if (length(panel_ids) > panel_capacity) {
    stop(
      "Number of panels exceeds panel_ncol * panel_nrow. ",
      "Increase panel_ncol/panel_nrow or reduce panel_ids."
    )
  }

  ## ------------------------------------------------------------
  ## Panel tag labels: (a), (b), ...
  ## ------------------------------------------------------------

  make_panel_tags <- function(n, case = "lower") {
    alphabet <- if (case == "lower") letters else LETTERS

    vapply(seq_len(n), function(i) {
      x <- i
      tag <- ""

      while (x > 0) {
        r <- (x - 1) %% 26 + 1
        tag <- paste0(alphabet[r], tag)
        x <- (x - 1) %/% 26
      }

      paste0("(", tag, ")")
    }, character(1))
  }

  if (is.null(panel_tag_labels)) {
    panel_tag_labels <- make_panel_tags(length(panel_ids), panel_tag_case)
  } else {
    if (length(panel_tag_labels) < length(panel_ids)) {
      stop("panel_tag_labels must have at least length(panel_ids) elements.")
    }

    panel_tag_labels <- panel_tag_labels[seq_along(panel_ids)]
  }

  if (!isTRUE(panel_tags)) {
    panel_tag_labels <- rep("", length(panel_ids))
  }

  ## ------------------------------------------------------------
  ## Helper: build dataframe for one community
  ## ------------------------------------------------------------

  make_panel_df <- function(ci) {
    mm <- M[, ci]
    csi <- CSI[, ci]

    keep <- mm == 1

    df <- data.frame(
      CSI = as.numeric(csi[keep]),
      g_FAST = as.numeric(ge[keep]),
      rowname = rownames(M)[keep],
      group = colnames(M)[ci],
      stringsAsFactors = FALSE
    )

    df <- df[complete.cases(df$CSI, df$g_FAST), , drop = FALSE]

    df
  }

  panel_data <- lapply(panel_ids, make_panel_df)

  ## ------------------------------------------------------------
  ## Determine common axis ranges
  ## ------------------------------------------------------------

  if (common_axis) {
    all_x <- unlist(lapply(panel_data, function(df) df$g_FAST))
    all_y <- unlist(lapply(panel_data, function(df) df$CSI))

    all_x <- all_x[is.finite(all_x)]
    all_y <- all_y[is.finite(all_y)]

    if (!length(all_x) || !length(all_y)) {
      stop("No finite values were found for common axis ranges.")
    }

    if (is.null(xlim_range)) {
      xlim_range <- range(all_x, na.rm = TRUE)
      xpad <- diff(xlim_range) * pad_ratio
      if (!is.finite(xpad) || xpad == 0) xpad <- 0.1
      xlim_range <- c(xlim_range[1] - xpad, xlim_range[2] + xpad)
    }

    if (is.null(ylim_range)) {
      ylim_range <- range(all_y, na.rm = TRUE)
      ypad <- diff(ylim_range) * pad_ratio
      if (!is.finite(ypad) || ypad == 0) ypad <- 0.1
      ylim_range <- c(ylim_range[1] - ypad, ylim_range[2] + ypad)
    }
  }

  ## ------------------------------------------------------------
  ## Helper: create one panel
  ## ------------------------------------------------------------

  make_panel <- function(ci, df, panel_tag) {

    if (nrow(df) >= 3) {
      ct <- suppressWarnings(
        stats::cor.test(df$CSI, df$g_FAST, method = "spearman")
      )

      rho_val <- sprintf("%.3f", unname(ct$estimate))
      p_val <- sprintf("%.3f", ct$p.value)
    } else {
      rho_val <- "NA"
      p_val <- "NA"
    }

    panel_label <- colnames(M)[ci]

    title_text <- paste0(
      panel_label,
      " | \u03C1=",
      rho_val,
      ", p=",
      p_val
    )

    if (nzchar(panel_tag)) {
      title_text <- paste(panel_tag, title_text)
    }

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = g_FAST, y = CSI)
    ) +
      ggplot2::geom_point(
        color = point_color,
        size = point_size
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = rowname),
        size = label_size,
        max.overlaps = max_overlaps,
        box.padding = 0.3
      ) +
      ggplot2::labs(
        x = expression(g[FAST]),
        y = "CSI",
        title = title_text
      ) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(3, 3, 3, 3),
        plot.title = ggplot2::element_text(lineheight = 1.2),
        plot.background = ggplot2::element_rect(fill = bg, color = NA),
        panel.background = ggplot2::element_rect(fill = bg, color = NA)
      )

    if (smooth && nrow(df) >= 3) {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm",
          color = "black",
          se = TRUE
        )
    }

    if (common_axis) {
      p <- p +
        ggplot2::coord_cartesian(
          xlim = xlim_range,
          ylim = ylim_range
        )
    } else {
      if (!is.null(xlim_range) || !is.null(ylim_range)) {
        p <- p +
          ggplot2::coord_cartesian(
            xlim = xlim_range,
            ylim = ylim_range
          )
      }
    }

    p
  }

  ## ------------------------------------------------------------
  ## Make panel plots
  ## ------------------------------------------------------------

  plots <- Map(
    make_panel,
    panel_ids,
    panel_data,
    panel_tag_labels
  )

  n_blank <- panel_capacity - length(plots)

  if (n_blank > 0) {
    blank_plots <- replicate(
      n_blank,
      patchwork::plot_spacer(),
      simplify = FALSE
    )

    plots <- c(plots, blank_plots)
  }

  panel <- patchwork::wrap_plots(
    plots,
    ncol = panel_ncol,
    nrow = panel_nrow
  ) +
    patchwork::plot_layout(guides = "collect")

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/csi_ge_panels'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  width_in <- width_px / res_dpi
  height_in <- height_px / res_dpi

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(panel)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggplot2::ggsave(
      filename = png_file,
      plot = panel,
      width = width_px,
      height = height_px,
      units = "px",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = panel,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = panel,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    plot = panel,
    panel_data = panel_data,
    panel_ids = panel_ids,
    panel_tag_labels = panel_tag_labels,
    xlim_range = xlim_range,
    ylim_range = ylim_range,
    params = list(
      panel_ncol = panel_ncol,
      panel_nrow = panel_nrow,
      panel_capacity = panel_capacity,
      n_panels = length(panel_ids),
      n_blank = n_blank,
      panel_tags = panel_tags,
      panel_tag_case = panel_tag_case,
      device = device,
      file = file,
      output_files = output_files,
      width_px = width_px,
      height_px = height_px,
      res_dpi = res_dpi
    )
  )

  return(out)
}

## ------------------------------------------------------------
## Reusable helper:
## Match binary community compositions to SSD groups
## ------------------------------------------------------------

find_matching_groups <- function(vlist, groups) {
  result <- integer(length(vlist))

  for (i in seq_along(vlist)) {
    target <- as.integer(vlist[[i]])
    matched <- FALSE

    if (anyNA(target)) {
      result[i] <- 0L
      next
    }

    for (g in seq_along(groups)) {
      group <- groups[[g]]

      for (subg in seq_along(group)) {
        ref <- as.integer(group[[subg]][[1]])

        if (
          length(target) == length(ref) &&
          !anyNA(ref) &&
          all(target == ref)
        ) {
          result[i] <- g
          matched <- TRUE
          break
        }
      }

      if (matched) break
    }

    if (!matched) result[i] <- 0L
  }

  return(result)
}


## ------------------------------------------------------------
## Reusable helper:
## Match binary community compositions to SSD subgroups
## Returns "group.subgroup", e.g. "1.2"
## ------------------------------------------------------------

## ------------------------------------------------------------
## Fine-grained matching of binary community compositions to SSD subgroups.
## ------------------------------------------------------------

find_matching_groups_fine <- function(vlist, groups) {
  result <- character(length(vlist))

  for (i in seq_along(vlist)) {
    target <- as.integer(vlist[[i]])
    matched <- FALSE

    if (anyNA(target)) {
      result[i] <- "0"
      next
    }

    for (g in seq_along(groups)) {
      group <- groups[[g]]

      for (subg in seq_along(group)) {
        ref <- as.integer(group[[subg]][[1]])

        if (
          length(target) == length(ref) &&
          !anyNA(ref) &&
          all(target == ref)
        ) {
          result[i] <- paste0(g, ".", subg)
          matched <- TRUE
          break
        }
      }

      if (matched) break
    }

    if (!matched) result[i] <- "0"
  }

  return(result)
}


## ------------------------------------------------------------
## Plot observed community energy vs FAST score,
## colored by SSD group membership
## ------------------------------------------------------------

## ============================================================
## 10. Observed energy versus FAST score by SSD group
##
## This scatter plot maps observed samples to stable-state groups and
## displays their energy landscape values across FAST score.
## ============================================================

plot_energy_FAST_by_group <- function(
  gstab,
  ssd_grouping,
  enmat_fastscore,
  ocmat = NULL,
  nspecies = NULL,

  stable_state_id_col = "stable.state.id",
  energy_col = 4,

  device   = "screen",
  file     = NULL,
  width_in = 5.2,
  height_in = 5.2,
  res_dpi  = 300,
  bg       = "white",

  point_size = 3,

  title = "",
  xlab = expression(epsilon[FAST]),
  ylab = "Energy",
  color_lab = "Group ID",

  base_size = 14,
  zero_color = "gray",
  palette_name = "Set1"
) {

  ## ------------------------------------------------------------
  ## Required packages
  ## ------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' is required.")
  }

  ## ------------------------------------------------------------
  ## Device handling
  ## ------------------------------------------------------------

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Extract gStability table
  ## ------------------------------------------------------------

  if (is.list(gstab) && !is.data.frame(gstab)) {
    gstab_df <- gstab[[1]]
  } else {
    gstab_df <- gstab
  }

  if (!is.data.frame(gstab_df)) {
    gstab_df <- as.data.frame(gstab_df)
  }

  ## stable.state.id
  if (is.character(stable_state_id_col)) {
    if (!(stable_state_id_col %in% colnames(gstab_df))) {
      stop("`stable_state_id_col` was not found in gstab.")
    }
    stable_ids <- gstab_df[[stable_state_id_col]]
  } else {
    stable_ids <- gstab_df[, stable_state_id_col]
  }

  ## energy values
  if (is.character(energy_col)) {
    if (!(energy_col %in% colnames(gstab_df))) {
      stop("`energy_col` was not found in gstab.")
    }
    energy_values <- gstab_df[[energy_col]]
  } else {
    energy_values <- gstab_df[, energy_col]
  }

  energy_values <- as.numeric(energy_values)

  ## ------------------------------------------------------------
  ## FAST score vector
  ## ------------------------------------------------------------

  if (is.matrix(enmat_fastscore) || is.data.frame(enmat_fastscore)) {
    fast_values <- as.numeric(enmat_fastscore[, 1])
    fast_names <- rownames(enmat_fastscore)
  } else {
    fast_values <- as.numeric(enmat_fastscore)
    fast_names <- names(enmat_fastscore)
  }

  ## Align by rownames if possible
  if (
    !is.null(rownames(gstab_df)) &&
    !is.null(fast_names) &&
    all(rownames(gstab_df) %in% fast_names)
  ) {
    fast_values <- fast_values[match(rownames(gstab_df), fast_names)]
  }

  if (length(fast_values) != nrow(gstab_df)) {
    stop(
      "Length of FAST score vector does not match nrow(gstab[[1]]). ",
      "Check sample order or row names."
    )
  }

  ## ------------------------------------------------------------
  ## Determine number of species
  ## ------------------------------------------------------------

  if (is.null(nspecies)) {
    if (!is.null(ocmat)) {
      nspecies <- ncol(ocmat)
    } else {
      nspecies <- length(as.integer(ssd_grouping[[1]][[1]][[1]]))
    }
  }

  ## ------------------------------------------------------------
  ## Realized community composition -> basin stable state -> group ID
  ## ------------------------------------------------------------

  ssid_bin <- lapply(stable_ids, function(x) {
    if (is.na(x)) {
      rep(NA_integer_, nspecies)
    } else {
      id2bin(x, nspecies)
    }
  })

  ss_group_id <- find_matching_groups(
    vlist = ssid_bin,
    groups = ssd_grouping
  )

  ## ------------------------------------------------------------
  ## Build plot data
  ## ------------------------------------------------------------

  group_levels <- sort(unique(ss_group_id))
  group_levels_chr <- as.character(group_levels)

  df <- data.frame(
    FAST = fast_values,
    energy = energy_values,
    ss.group.id = factor(
      as.character(ss_group_id),
      levels = group_levels_chr
    ),
    stringsAsFactors = FALSE
  )

  df <- df[complete.cases(df$FAST, df$energy, df$ss.group.id), , drop = FALSE]

  ## ------------------------------------------------------------
  ## Color palette
  ## ------------------------------------------------------------

  set1_levels <- group_levels[group_levels != 0L]
  set1_levels_chr <- as.character(set1_levels)

  if (length(set1_levels) == 0) {
    group_palette <- character(0)
  } else if (length(set1_levels) <= 9) {
    group_palette <- RColorBrewer::brewer.pal(
      max(length(set1_levels), 3),
      palette_name
    )[seq_along(set1_levels)]
  } else {
    group_palette <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(9, palette_name)
    )(length(set1_levels))
  }

  names(group_palette) <- set1_levels_chr

  palette <- c("0" = zero_color, group_palette)

  ## Keep only colors used in the factor levels
  palette <- palette[names(palette) %in% group_levels_chr]

  ## ------------------------------------------------------------
  ## Plot
  ## ------------------------------------------------------------

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = FAST, y = energy, color = ss.group.id)
  ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = color_lab
    ) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.title = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      plot.background = ggplot2::element_rect(fill = bg, color = NA),
      panel.background = ggplot2::element_rect(fill = bg, color = NA)
    )

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/ene_env'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(p)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggplot2::ggsave(
      filename = png_file,
      plot = p,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    plot = p,
    data = df,
    stable_state_binary = ssid_bin,
    ss_group_id = ss_group_id,
    palette = palette,
    params = list(
      device = device,
      file = file,
      output_files = output_files,
      width_in = width_in,
      height_in = height_in,
      res_dpi = res_dpi,
      stable_state_id_col = stable_state_id_col,
      energy_col = energy_col,
      nspecies = nspecies
    )
  )

  return(out)
}

## ============================================================
## Functions for visualizing relative basin size
## ============================================================

## Binary vector signature
## Example: c(1,0,1,0) -> "1010"
## ------------------------------------------------------------

## ============================================================
## 11. Relative basin-size visualisation
##
## These functions estimate basin membership by repeatedly sampling
## stable states and plotting the relative size of each stable-state
## group across environmental points.
## ============================================================

bin_signature <- function(v) {
  v <- as.integer(v)

  if (length(v) == 0 || anyNA(v)) {
    return(NA_character_)
  }

  paste0(v, collapse = "")
}


## ------------------------------------------------------------
## Normalize SSD grouping object
## Accepts either ssd_grouping itself or ssd2-like object with $groups
## ------------------------------------------------------------

normalize_ssd_grouping <- function(ssd_grouping) {
  if (is.list(ssd_grouping) && !is.null(ssd_grouping$groups)) {
    return(ssd_grouping$groups)
  }

  ssd_grouping
}


## ------------------------------------------------------------
## Build lookup:
## binary membership signature -> SSD group ID
## ------------------------------------------------------------

make_group_signature_lookup <- function(ssd_grouping) {
  ssd_grouping <- normalize_ssd_grouping(ssd_grouping)

  keys <- character(0)
  vals <- integer(0)

  for (g in seq_along(ssd_grouping)) {
    group <- ssd_grouping[[g]]

    for (subg in seq_along(group)) {
      memb <- tryCatch(
        as.integer(group[[subg]][[1]]),
        error = function(e) NULL
      )

      if (is.null(memb)) next

      sig <- bin_signature(memb)

      if (is.na(sig)) next

      keys <- c(keys, sig)
      vals <- c(vals, g)
    }
  }

  keep <- !duplicated(keys)

  stats::setNames(vals[keep], keys[keep])
}


## ------------------------------------------------------------
## Stable-state ID key for pruning-rule matching
## ------------------------------------------------------------

id_key <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  out <- rep(NA_character_, length(x))

  suppressWarnings(x_num <- as.numeric(x))

  ok <- is.finite(x_num)

  out[ok] <- format(
    x_num[ok],
    scientific = FALSE,
    trim = TRUE,
    digits = 22
  )

  out
}


## ------------------------------------------------------------
## Robust pruning rule
## Returns character ID keys
## ------------------------------------------------------------

apply_pruning_rule <- function(id.vector, mapping.matrix) {
  id_chr <- id_key(id.vector)

  if (is.null(mapping.matrix) || length(mapping.matrix) == 0) {
    return(id_chr)
  }

  mapping.matrix <- as.matrix(mapping.matrix)

  if (nrow(mapping.matrix) == 0 || ncol(mapping.matrix) < 2) {
    return(id_chr)
  }

  before <- id_key(mapping.matrix[, 1])
  after  <- id_key(mapping.matrix[, 2])

  valid <- !is.na(before) & !is.na(after)

  if (!any(valid)) {
    return(id_chr)
  }

  mapping <- stats::setNames(after[valid], before[valid])

  out <- id_chr
  hit <- !is.na(id_chr) & id_chr %in% names(mapping)

  out[hit] <- mapping[id_chr[hit]]

  out
}


## ------------------------------------------------------------
## Safe id2bin wrapper
## ------------------------------------------------------------

safe_id2bin <- function(x, nspecies) {
  x_key <- id_key(x)

  if (length(x_key) == 0 || is.na(x_key[1])) {
    return(NULL)
  }

  x_num <- suppressWarnings(as.numeric(x_key[1]))

  if (!is.finite(x_num)) {
    return(NULL)
  }

  out <- tryCatch(
    id2bin(x_num, nspecies),
    error = function(e) NULL
  )

  if (is.null(out)) {
    return(NULL)
  }

  out <- as.integer(out)

  if (length(out) != nspecies || anyNA(out)) {
    return(NULL)
  }

  out
}


## ------------------------------------------------------------
## Compute relative basin size table
##
## Returns:
##   Row, Value, Count, Ratio, EnvValue
##
## Value:
##   SSD group ID.
##   0 means unmatched / outside displayed SSD groups.
## ------------------------------------------------------------

relative_basin_size_df <- function(
  gela,
  sa,
  ssd_grouping,
  itr = 256,
  threads = 1,
  seed = NULL,
  reporting = TRUE
) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required.")
  }
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required.")
  }
  if (!requireNamespace("doParallel", quietly = TRUE) && threads > 1) {
    stop("Package 'doParallel' is required when threads > 1.")
  }
  if (!requireNamespace("rELA", quietly = TRUE)) {
    stop("Package 'rELA' is required.")
  }

  suppressPackageStartupMessages(library(foreach))

  ssd_grouping <- normalize_ssd_grouping(ssd_grouping)

  group_lookup <- make_group_signature_lookup(ssd_grouping)

  if (length(group_lookup) == 0) {
    stop(
      "No valid group signatures could be built from ssd_grouping. ",
      "Check that ssd_grouping[[g]][[subg]][[1]] contains binary membership vectors."
    )
  }

  env.values <- as.numeric(gela[[2]])
  e.vec <- matrix(gela[[3]], ncol = 1)

  len.h.g <- length(e.vec) + 1
  n_env <- length(env.values)
  nspecies <- nrow(sa[[1]])

  if (reporting) {
    cat("Estimating basin sizes:\n")
    cat("  environmental points:", n_env, "\n")
    cat("  trials per point:", itr, "\n")
    cat("  threads:", threads, "\n")
    cat("  SSD group signature entries:", length(group_lookup), "\n")
  }

  bin2id_fun <- get("bin2id", mode = "function")

  worker_fun <- function(i) {
    if (!is.null(seed)) {
      set.seed(seed + i)
    }

    pruning_rule <- gela[[1]][[i]][[2]]

    e.vec.i <- replace(
      e.vec,
      is.na(e.vec),
      env.values[i]
    )

    h_ev <- as.numeric(
      sa[[1]][, 1] +
        (sa[[1]][, seq(2, len.h.g), drop = FALSE] %*% e.vec.i)
    )

    j_ev <- as.matrix(
      sa[[1]][, -seq_len(len.h.g), drop = FALSE]
    )

    if (any(!is.finite(h_ev))) {
      stop("Non-finite h_ev at environmental index ", i)
    }

    if (any(!is.finite(j_ev))) {
      stop("Non-finite j_ev at environmental index ", i)
    }

    ss.trials <- rELA::SSestimate(h_ev, j_ev, itr)

    ## Expected:
    ## binary-state columns + one extra column
    if (ncol(ss.trials) >= nspecies + 1) {
      state_mat <- ss.trials[, seq_len(nspecies), drop = FALSE]
    } else {
      stop(
        "Unexpected SSestimate output dimension at environmental index ",
        i,
        ": ncol(ss.trials) = ",
        ncol(ss.trials),
        ", nspecies = ",
        nspecies
      )
    }

    state_mat <- apply(state_mat, 2, as.integer)
    state_mat <- as.matrix(state_mat)

    id_vector <- apply(state_mat, 1, bin2id_fun)

    ## Apply pruning rule:
    ## sampled state ID -> basin stable-state ID
    ssids <- apply_pruning_rule(
      id.vector = id_vector,
      mapping.matrix = pruning_rule
    )

    group_ids <- integer(nrow(state_mat))

    for (k in seq_len(nrow(state_mat))) {

      ## First try pruned stable-state ID
      b_pruned <- safe_id2bin(ssids[k], nspecies)

      if (!is.null(b_pruned)) {
        sig <- bin_signature(b_pruned)

        if (!is.na(sig) && sig %in% names(group_lookup)) {
          group_ids[k] <- as.integer(group_lookup[[sig]])
          next
        }
      }

      ## Fallback:
      ## use raw sampled state directly
      sig_raw <- bin_signature(state_mat[k, ])

      if (!is.na(sig_raw) && sig_raw %in% names(group_lookup)) {
        group_ids[k] <- as.integer(group_lookup[[sig_raw]])
      } else {
        group_ids[k] <- 0L
      }
    }

    group_ids
  }

  ## ------------------------------------------------------------
  ## Run estimation
  ## ------------------------------------------------------------

  if (threads > 1) {
    cluster <- parallel::makeCluster(threads)

    parallel::clusterCall(
      cluster,
      function(x) .libPaths(x),
      .libPaths()
    )

    parallel::clusterEvalQ(cluster, {
      suppressPackageStartupMessages(library(foreach))
      suppressPackageStartupMessages(library(rELA))
      NULL
    })

    doParallel::registerDoParallel(cluster)

    on.exit(parallel::stopCluster(cluster), add = TRUE)

    results <- foreach::foreach(
      i = seq_len(n_env),
      .packages = c("rELA", "foreach"),
      .export = c(
        "bin_signature",
        "id_key",
        "apply_pruning_rule",
        "safe_id2bin",
        "group_lookup",
        "nspecies"
      )
    ) %dopar% {
      worker_fun(i)
    }

  } else {
    results <- lapply(seq_len(n_env), worker_fun)
  }

  ## ------------------------------------------------------------
  ## Combine into matrix
  ## ------------------------------------------------------------

  group.index.mat <- do.call(rbind, results)

  colnames(group.index.mat) <- paste0("trial_", seq_len(ncol(group.index.mat)))
  rownames(group.index.mat) <- paste0("env_", seq_len(nrow(group.index.mat)))

  if (reporting) {
    cat("Group assignment summary:\n")
    print(table(as.vector(group.index.mat), useNA = "ifany"))
  }

  ## ------------------------------------------------------------
  ## Convert to long format
  ## ------------------------------------------------------------

  df <- as.data.frame(group.index.mat)

  df <- dplyr::mutate(
    df,
    Row = dplyr::row_number()
  )

  df_long <- df |>
    tidyr::pivot_longer(
      cols = -Row,
      names_to = "Col",
      values_to = "Value"
    ) |>
    dplyr::group_by(Row, Value) |>
    dplyr::summarise(
      Count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(Row) |>
    dplyr::mutate(
      Ratio = Count / sum(Count),
      EnvValue = env.values[Row]
    ) |>
    dplyr::ungroup()

  attr(df_long, "group.index.mat") <- group.index.mat
  attr(df_long, "env.values") <- env.values
  attr(df_long, "itr") <- itr
  attr(df_long, "threads") <- threads
  attr(df_long, "group_lookup") <- group_lookup

  df_long
}


## ------------------------------------------------------------
## Plot relative basin size
##
## This reproduces the original stacked-bar style:
##   x = factor(Row)
##   y = Ratio
##   fill = Value
## ------------------------------------------------------------

plot_relative_basin_size <- function(
  gela = NULL,
  sa = NULL,
  ssd_grouping = NULL,
  basin_df = NULL,

  itr = 256,
  threads = 1,
  seed = NULL,

  env_label_values = NULL,
  x_break_by = 5,

  device = "screen",
  file = NULL,

  width_in = 5.8,
  height_in = 4.6,
  res_dpi = 300,
  bg = "white",

  xlab = expression(epsilon[FAST]),
  ylab = "Ratio",
  title = "Relative basin size",
  legend_title = "Category",

  zero_color = "gray",
  palette_name = "Set1",

  base_size = 16,
  title_size = 16,
  axis_title_size = 16,
  axis_text_size = 12,
  legend_title_size = 14,
  legend_text_size = 12,

  reporting = TRUE
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' is required.")
  }

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Compute basin_df if not supplied
  ## ------------------------------------------------------------

  if (is.null(basin_df)) {
    if (is.null(gela) || is.null(sa) || is.null(ssd_grouping)) {
      stop("If basin_df is NULL, gela, sa, and ssd_grouping must be provided.")
    }

    basin_df <- relative_basin_size_df(
      gela = gela,
      sa = sa,
      ssd_grouping = ssd_grouping,
      itr = itr,
      threads = threads,
      seed = seed,
      reporting = reporting
    )
  }

  basin_df <- as.data.frame(basin_df)

  if (!all(c("Row", "Value", "Count", "Ratio") %in% colnames(basin_df))) {
    stop("basin_df must contain Row, Value, Count, and Ratio columns.")
  }

  n_env <- length(unique(basin_df$Row))

  ## ------------------------------------------------------------
  ## Set stacking order: bottom to top = 0, 1, 2, ...
  ## ------------------------------------------------------------

  levels_order <- sort(unique(basin_df$Value), decreasing = FALSE)

  basin_df$Value <- factor(
    basin_df$Value,
    levels = levels_order
  )

  ## ------------------------------------------------------------
  ## Assign fill colors: gray for 0, Set1 for others
  ## ------------------------------------------------------------

  levels_chr <- as.character(levels_order)
  nonzero_levels <- setdiff(levels_chr, "0")

  if (length(nonzero_levels) == 0) {
    set_colors <- character(0)
  } else if (length(nonzero_levels) <= 9) {
    set_colors <- RColorBrewer::brewer.pal(
      max(length(nonzero_levels), 3),
      palette_name
    )[seq_along(nonzero_levels)]
  } else {
    set_colors <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(9, palette_name)
    )(length(nonzero_levels))
  }

  fill_colors <- c(
    "0" = zero_color,
    stats::setNames(set_colors, nonzero_levels)
  )

  fill_colors <- fill_colors[names(fill_colors) %in% levels_chr]

  ## ------------------------------------------------------------
  ## X-axis labels
  ## ------------------------------------------------------------

  if (is.null(env_label_values)) {
    if ("EnvValue" %in% colnames(basin_df)) {
      env_label_values <- tapply(
        basin_df$EnvValue,
        basin_df$Row,
        function(x) x[1]
      )
      env_label_values <- as.numeric(env_label_values)
    } else if (!is.null(attr(basin_df, "env.values"))) {
      env_label_values <- attr(basin_df, "env.values")
    } else if (!is.null(gela)) {
      env_label_values <- gela[[2]]
    } else {
      env_label_values <- seq_len(n_env)
    }
  }

  break_indices <- seq(1, n_env, by = x_break_by)

  env_labels <- rep("", n_env)
  n_lab <- min(length(env_label_values), n_env)
  env_labels[seq_len(n_lab)] <- round(env_label_values[seq_len(n_lab)], 2)

  ## ------------------------------------------------------------
  ## Plot
  ## ------------------------------------------------------------

  p <- ggplot2::ggplot(
    basin_df,
    ggplot2::aes(
      x = factor(Row),
      y = Ratio,
      fill = Value
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      name = legend_title,
      drop = FALSE
    ) +
    ggplot2::scale_x_discrete(
      breaks = as.character(break_indices),
      labels = env_labels[break_indices]
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = title_size,
        face = "bold"
      ),
      axis.title = ggplot2::element_text(size = axis_title_size),
      axis.text = ggplot2::element_text(size = axis_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      legend.text = ggplot2::element_text(size = legend_text_size),
      plot.background = ggplot2::element_rect(fill = bg, color = NA),
      panel.background = ggplot2::element_rect(fill = bg, color = NA)
    )

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/basin_size'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(p)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggplot2::ggsave(
      filename = png_file,
      plot = p,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    plot = p,
    data = basin_df,
    group.index.mat = attr(basin_df, "group.index.mat"),
    fill_colors = fill_colors,
    params = list(
      itr = itr,
      threads = threads,
      seed = seed,
      x_break_by = x_break_by,
      device = device,
      file = file,
      output_files = output_files,
      width_in = width_in,
      height_in = height_in,
      res_dpi = res_dpi
    )
  )

  return(out)
}


## Group-level alignment helpers
## Priority:
##   1. Same SSD group -> same community-composition x position
##   2. Group ordering uses:
##      rightward y-adjacency + Hamming distance + boundary energy + y-gap
## ============================================================

## ============================================================
## 12. 3D GELS surface construction and plotting
##
## These functions align grouped stable-state trajectories, build a
## 3D energy surface over community-composition group and FAST score,
## overlay stable-state paths, and save replayed 3D plots to multiple
## formats including PNG, EPS, and TIFF.
## ============================================================

.path_range <- function(path) {
  idx <- path$valid_idx
  if (!length(idx)) c(Inf, -Inf) else c(min(idx), max(idx))
}

.path_boundary_metrics <- function(path_a, path_b, nspecies) {
  ra <- .path_range(path_a)
  rb <- .path_range(path_b)

  ea <- path_a$energy
  eb <- path_b$energy

  if (is.finite(ra[2]) && is.finite(rb[1]) && ra[2] < rb[1]) {
    gap <- rb[1] - ra[2] - 1L
    dE <- abs(ea[ra[2]] - eb[rb[1]])
    dM <- sum(path_a$membership != path_b$membership) / nspecies
    return(list(gap = gap, dE = dE, dM = dM))
  }

  if (is.finite(rb[2]) && is.finite(ra[1]) && rb[2] < ra[1]) {
    gap <- ra[1] - rb[2] - 1L
    dE <- abs(eb[rb[2]] - ea[ra[1]])
    dM <- sum(path_a$membership != path_b$membership) / nspecies
    return(list(gap = gap, dE = dE, dM = dM))
  }

  ia <- path_a$valid_idx
  ib <- path_b$valid_idx

  if (!length(ia) || !length(ib)) {
    dM <- sum(path_a$membership != path_b$membership) / nspecies
    return(list(gap = Inf, dE = Inf, dM = dM))
  }

  da <- outer(ia, ib, function(x, y) abs(x - y))
  k <- which(da == min(da), arr.ind = TRUE)[1, ]

  gap <- max(0, abs(ia[k[1]] - ib[k[2]]) - 1L)
  dE <- abs(ea[ia[k[1]]] - eb[ib[k[2]]])
  dM <- sum(path_a$membership != path_b$membership) / nspecies

  list(gap = gap, dE = dE, dM = dM)
}

.group_first_index <- function(paths) {
  vals <- vapply(paths, function(p) .path_range(p)[1], numeric(1))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) Inf else min(vals)
}

.group_last_index <- function(paths) {
  vals <- vapply(paths, function(p) .path_range(p)[2], numeric(1))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) -Inf else max(vals)
}

.group_hamming_spacing <- function(paths_a, paths_b) {
  hd <- c()

  for (pa in paths_a) {
    for (pb in paths_b) {
      hd <- c(hd, sum(pa$membership != pb$membership))
    }
  }

  hd <- hd[is.finite(hd)]

  if (!length(hd)) 1 else max(1, stats::median(hd))
}

.build_group_adj_edges <- function(path_by_group) {
  group_ids <- names(path_by_group)

  from <- character(0)
  to <- character(0)

  if (length(group_ids) <= 1) {
    return(data.frame(from = from, to = to, stringsAsFactors = FALSE))
  }

  for (ga in group_ids) {
    for (gb in group_ids) {
      if (ga == gb) next

      paths_a <- path_by_group[[ga]]
      paths_b <- path_by_group[[gb]]

      adjacent <- FALSE

      for (pa in paths_a) {
        for (pb in paths_b) {
          ra <- .path_range(pa)
          rb <- .path_range(pb)

          if (
            is.finite(ra[2]) &&
            is.finite(rb[1]) &&
            ra[2] + 1L == rb[1]
          ) {
            adjacent <- TRUE
            break
          }
        }

        if (adjacent) break
      }

      if (adjacent) {
        from <- c(from, ga)
        to <- c(to, gb)
      }
    }
  }

  unique(data.frame(from = from, to = to, stringsAsFactors = FALSE))
}

.topo_order_groups <- function(nodes, edges, group_first) {
  nodes <- unique(as.character(nodes))

  adj <- setNames(vector("list", length(nodes)), nodes)
  indeg <- setNames(rep(0L, length(nodes)), nodes)

  if (nrow(edges)) {
    for (k in seq_len(nrow(edges))) {
      f <- edges$from[k]
      t <- edges$to[k]

      if (!(f %in% nodes) || !(t %in% nodes)) next

      adj[[f]] <- c(adj[[f]], t)
      indeg[[t]] <- indeg[[t]] + 1L
    }
  }

  out <- character(0)
  remaining <- nodes

  while (length(remaining)) {
    cand <- remaining[indeg[remaining] == 0L]

    if (!length(cand)) {
      cand <- remaining
    }

    pick <- cand[order(group_first[cand], as.numeric(cand), cand)][1]

    out <- c(out, pick)
    remaining <- setdiff(remaining, pick)

    for (t in adj[[pick]]) {
      if (t %in% remaining) {
        indeg[[t]] <- max(0L, indeg[[t]] - 1L)
      }
    }

    indeg[[pick]] <- 0L
  }

  out
}

.order_groups_rightward_by_y <- function(group_ids, path_by_group) {
  group_ids <- unique(as.character(group_ids))

  if (length(group_ids) <= 1) return(group_ids)

  edges <- .build_group_adj_edges(path_by_group)

  group_first <- setNames(
    vapply(group_ids, function(g) .group_first_index(path_by_group[[g]]), numeric(1)),
    group_ids
  )

  if (!nrow(edges)) {
    return(group_ids[order(group_first[group_ids], as.numeric(group_ids), group_ids)])
  }

  ## weak components of the directed adjacency graph
  und <- setNames(vector("list", length(group_ids)), group_ids)

  for (k in seq_len(nrow(edges))) {
    a <- edges$from[k]
    b <- edges$to[k]

    if (!(a %in% group_ids) || !(b %in% group_ids)) next

    und[[a]] <- c(und[[a]], b)
    und[[b]] <- c(und[[b]], a)
  }

  seen <- setNames(rep(FALSE, length(group_ids)), group_ids)
  comps <- list()

  for (v in group_ids) {
    if (seen[[v]]) next

    q <- c(v)
    seen[[v]] <- TRUE
    comp <- character(0)

    while (length(q)) {
      cur <- q[1]
      q <- q[-1]

      comp <- c(comp, cur)

      nei <- unique(und[[cur]])

      for (u in nei) {
        if (!seen[[u]]) {
          seen[[u]] <- TRUE
          q <- c(q, u)
        }
      }
    }

    comps[[length(comps) + 1]] <- comp
  }

  comp_first <- vapply(comps, function(cc) {
    min(group_first[cc], na.rm = TRUE)
  }, numeric(1))

  comps <- comps[order(comp_first)]

  ordered <- unlist(lapply(comps, function(cc) {
    sub_edges <- edges[
      edges$from %in% cc & edges$to %in% cc,
      ,
      drop = FALSE
    ]

    .topo_order_groups(cc, sub_edges, group_first)
  }))

  missing <- setdiff(group_ids, ordered)

  if (length(missing)) {
    ordered <- c(
      ordered,
      missing[order(group_first[missing], as.numeric(missing), missing)]
    )
  }

  ordered
}

.build_D_group <- function(
  group_ids,
  path_by_group,
  nspecies,
  w_adj = 20,
  w_energy = 2,
  w_mem = 1,
  w_gap = 5,
  adj_energy_scale = c("median", "iqr", "sd"),
  adj_only = TRUE
) {
  adj_energy_scale <- match.arg(adj_energy_scale)

  group_ids <- unique(as.character(group_ids))
  G <- length(group_ids)

  D <- matrix(0, G, G, dimnames = list(group_ids, group_ids))

  if (G <= 1) return(D)

  gap_mat <- matrix(Inf, G, G, dimnames = list(group_ids, group_ids))
  dE_mat <- matrix(Inf, G, G, dimnames = list(group_ids, group_ids))
  dM_mat <- matrix(0, G, G, dimnames = list(group_ids, group_ids))

  bd_diffs <- c()

  for (a in seq_len(G)) {
    for (b in a:G) {
      if (a == b) next

      ga <- group_ids[a]
      gb <- group_ids[b]

      paths_a <- path_by_group[[ga]]
      paths_b <- path_by_group[[gb]]

      metrics <- list()

      for (pa in paths_a) {
        for (pb in paths_b) {
          metrics[[length(metrics) + 1]] <- .path_boundary_metrics(pa, pb, nspecies)
        }
      }

      gaps <- vapply(metrics, function(x) x$gap, numeric(1))
      dEs <- vapply(metrics, function(x) x$dE, numeric(1))
      dMs <- vapply(metrics, function(x) x$dM, numeric(1))

      finite_gap <- is.finite(gaps)

      if (any(finite_gap)) {
        min_gap <- min(gaps[finite_gap], na.rm = TRUE)
        idx_gap <- which(finite_gap & gaps == min_gap)

        dE <- min(dEs[idx_gap], na.rm = TRUE)
        dM <- min(dMs[idx_gap], na.rm = TRUE)

        gap <- min_gap
      } else {
        gap <- Inf
        dE <- Inf
        dM <- min(dMs, na.rm = TRUE)
      }

      gap_mat[a, b] <- gap_mat[b, a] <- gap
      dE_mat[a, b] <- dE_mat[b, a] <- dE
      dM_mat[a, b] <- dM_mat[b, a] <- dM

      bd_diffs <- c(bd_diffs, dE)
    }
  }

  bd <- bd_diffs[is.finite(bd_diffs)]

  sc <- if (!length(bd)) {
    1
  } else if (adj_energy_scale == "median") {
    stats::median(bd)
  } else if (adj_energy_scale == "iqr") {
    stats::IQR(bd)
  } else {
    stats::sd(bd)
  }

  if (!is.finite(sc) || sc <= 0) sc <- 1

  dE_scaled <- dE_mat / sc

  for (a in seq_len(G)) {
    for (b in a:G) {
      if (a == b) next

      gap <- gap_mat[a, b]
      dE <- dE_scaled[a, b]
      dM <- dM_mat[a, b]

      adj_reward <- if (is.finite(gap) && gap == 0) w_adj else 0
      gap_pen <- if (is.finite(gap)) w_gap * gap else w_gap * 100

      d <- w_energy * dE + w_mem * dM + gap_pen
      d <- d - adj_reward

      if (adj_only && is.finite(gap) && gap == 0) {
        d <- min(d, 1e-6 + w_energy * dE + w_mem * dM)
      }

      D[a, b] <- D[b, a] <- d
    }
  }

  finite_vals <- D[is.finite(D) & D > 0]
  big <- if (length(finite_vals)) max(finite_vals) * 10 else 1e6

  D[!is.finite(D)] <- big

  D
}

.order_by_D <- function(D) {
  n <- nrow(D)

  if (n <= 1) return(seq_len(n))

  start <- which.min(rowMeans(D))
  remaining <- setdiff(seq_len(n), start)
  ord <- start

  while (length(remaining)) {
    last <- ord[length(ord)]
    nxt <- remaining[which.min(D[last, remaining])]

    ord <- c(ord, nxt)
    remaining <- setdiff(remaining, nxt)
  }

  if (n < 4) return(ord)

  cost <- function(o) {
    sum(D[cbind(o[-length(o)], o[-1])])
  }

  best <- ord
  best_cost <- cost(best)

  improved <- TRUE

  while (improved) {
    improved <- FALSE

    for (i in 2:(n - 2)) {
      for (k in (i + 1):(n - 1)) {
        cand <- best
        cand[i:k] <- rev(cand)

        ## correction for candidate reversal
        cand <- best
        cand[i:k] <- rev(best[i:k])

        cst <- cost(cand)

        if (cst + 1e-12 < best_cost) {
          best <- cand
          best_cost <- cst
          improved <- TRUE
        }
      }
    }
  }

  best
}

.order_groups_with_policy <- function(
  path_list,
  nspecies,
  w_adj = 20,
  w_energy = 2,
  w_mem = 1,
  w_gap = 5,
  adj_only = TRUE,
  adj_energy_scale = "median"
) {
  path_by_group <- split(path_list, vapply(path_list, function(x) as.character(x$group_id), character(1)))

  group_ids <- names(path_by_group)

  if (length(group_ids) <= 1) return(group_ids)

  D <- .build_D_group(
    group_ids = group_ids,
    path_by_group = path_by_group,
    nspecies = nspecies,
    w_adj = w_adj,
    w_energy = w_energy,
    w_mem = w_mem,
    w_gap = w_gap,
    adj_energy_scale = adj_energy_scale,
    adj_only = adj_only
  )

  ord_idx <- .order_by_D(D)
  group_nn <- group_ids[ord_idx]

  ## Enforce rightward order for y-contiguous trajectories at the group level
  .order_groups_rightward_by_y(group_nn, path_by_group)
}

## ============================================================
## GELS_build_l3dx
## Same group -> same x position
## Group order -> distance + rightward adjacency policy
## ============================================================

GELS_build_l3dx <- function(
  gela,
  sa,
  ssd_grouping,
  w_adj = 20,
  w_energy = 2,
  w_mem = 1,
  w_gap = 5,
  adj_only = TRUE,
  adj_energy_scale = "median"
) {
  de <- gela[[2]]

  if (is.list(sa) && !is.null(sa[[1]])) {
    nspecies <- nrow(sa[[1]])
  } else {
    stop("sa must be a list whose first element is the parameter matrix.")
  }

  if (is.null(ssd_grouping) || length(ssd_grouping) == 0) {
    warning("ssd_grouping is empty.")
    return(data.frame())
  }

  paste_sig <- function(v) {
    paste(as.integer(v), collapse = "|")
  }

  ## binary membership -> ssid map from gela
  sig2ssid <- list()

  for (i in seq_along(gela[[1]])) {
    ssids_i <- gela[[1]][[i]][[1]][[1]]

    if (length(ssids_i) == 0) next

    for (ssid in ssids_i) {
      b <- id2bin(ssid, nspecies)
      sig <- paste_sig(b)

      if (is.null(sig2ssid[[sig]])) {
        sig2ssid[[sig]] <- as.character(ssid)
      }
    }
  }

  ## Extract subgroup paths from ssd_grouping
  path_list <- list()

  for (g in seq_along(ssd_grouping)) {
    subgs <- ssd_grouping[[g]]

    for (subg in seq_along(subgs)) {
      memb <- as.integer(subgs[[subg]][[1]])
      energy <- as.numeric(subgs[[subg]][[2]])

      if (length(energy) != length(de)) {
        warning(
          paste0(
            "Skipping C", g, ".", subg,
            ": energy vector length does not match gela[[2]]."
          )
        )
        next
      }

      sig <- paste_sig(memb)
      ssid <- sig2ssid[[sig]]

      if (is.null(ssid)) {
        warning(
          paste0(
            "Skipping C", g, ".", subg,
            ": corresponding stable state ID was not found in gela."
          )
        )
        next
      }

      valid_idx <- which(is.finite(energy))

      if (!length(valid_idx)) next

      path_list[[length(path_list) + 1]] <- list(
        group_id = g,
        sub_id = subg,
        label = paste0("C", g, ".", subg),
        ssid = as.character(ssid),
        membership = memb,
        energy = energy,
        valid_idx = valid_idx
      )
    }
  }

  if (!length(path_list)) {
    warning("No valid paths were generated from ssd_grouping.")
    return(data.frame())
  }

  ## ------------------------------------------------------------
  ## Group-level order
  ## ------------------------------------------------------------

  group_ordered <- .order_groups_with_policy(
    path_list = path_list,
    nspecies = nspecies,
    w_adj = w_adj,
    w_energy = w_energy,
    w_mem = w_mem,
    w_gap = w_gap,
    adj_only = adj_only,
    adj_energy_scale = adj_energy_scale
  )

  path_by_group <- split(
    path_list,
    vapply(path_list, function(x) as.character(x$group_id), character(1))
  )

  ## ------------------------------------------------------------
  ## Convert group order to x positions.
  ## All subgroups in the same group receive the same x.
  ## Spacing between adjacent groups uses median pairwise Hamming distance.
  ## ------------------------------------------------------------

  if (length(group_ordered) > 1) {
    hh <- vapply(seq_len(length(group_ordered) - 1), function(i) {
      .group_hamming_spacing(
        path_by_group[[group_ordered[i]]],
        path_by_group[[group_ordered[i + 1]]]
      )
    }, numeric(1))

    hh[!is.finite(hh) | hh <= 0] <- 1

    raw_x <- c(0, cumsum(hh))
  } else {
    raw_x <- 0.5
  }

  if (length(raw_x) > 1 && max(raw_x) > min(raw_x)) {
    scaled_x <- (raw_x - min(raw_x)) / (max(raw_x) - min(raw_x))
  } else {
    scaled_x <- rep(0.5, length(raw_x))
  }

  group2x <- setNames(scaled_x, group_ordered)

  ## Reorder path_list for display
  path_order <- order(
    match(vapply(path_list, function(x) as.character(x$group_id), character(1)), group_ordered),
    vapply(path_list, function(x) x$sub_id, numeric(1))
  )

  path_list <- path_list[path_order]

  ## ------------------------------------------------------------
  ## Build l3dx
  ## ------------------------------------------------------------

  l3_list <- list()

  for (p in seq_along(path_list)) {
    path <- path_list[[p]]
    valid_idx <- path$valid_idx
    gid <- as.character(path$group_id)

    l3_list[[length(l3_list) + 1]] <- data.frame(
      Index = p,
      ssid = path$ssid,
      x = unname(group2x[gid]),
      y = de[valid_idx],
      z = path$energy[valid_idx],
      Cg_s = path$label,
      group_id = path$group_id,
      sub_id = path$sub_id,
      stringsAsFactors = FALSE
    )
  }

  l3dx <- dplyr::bind_rows(l3_list)

  l3dx$group_order <- match(as.character(l3dx$group_id), group_ordered)

  l3dx <- l3dx[
    order(l3dx$group_order, l3dx$sub_id, l3dx$y),
    ,
    drop = FALSE
  ]

  rownames(l3dx) <- NULL

  attr(l3dx, "group_ordered") <- group_ordered
  attr(l3dx, "group2x") <- group2x
  attr(l3dx, "alignment_policy") <- list(
    x_unit = "SSD group",
    same_group_same_x = TRUE,
    order_policy = "group-level distance ordering followed by rightward y-adjacency",
    w_adj = w_adj,
    w_energy = w_energy,
    w_mem = w_mem,
    w_gap = w_gap,
    adj_only = adj_only,
    adj_energy_scale = adj_energy_scale
  )

  return(l3dx)
}

## ============================================================
## Surface builder
## For surface seeds, multiple subgroups sharing the same group-x
## are collapsed by taking the lowest energy at each y and x.
## Overlay paths in l3dx are kept unchanged.
## ============================================================

GELS_build_surface_from_l3dx <- function(
  gela,
  l3dx,
  x_spline_points = 200,
  y_smooth_window = 0,
  x_offset_ratio = 0.05,
  pin_midpoints = TRUE,
  x_interp = c("linear", "spline")
) {
  x_interp <- match.arg(x_interp)

  de <- gela[[2]]
  nY <- length(de)

  get_tip_energy <- function(i, ssA, ssB) {
    a <- try(gela[[1]][[i]][[1]], silent = TRUE)

    if (inherits(a, "try-error")) return(NA_real_)

    ss_list <- a[[1]]

    idxA <- match(ssA, ss_list)
    idxB <- match(ssB, ss_list)

    if (is.na(idxA) || is.na(idxB) || idxA == idxB) {
      return(NA_real_)
    }

    r <- max(idxA, idxB)
    c <- min(idxA, idxB)

    tip <- gela[[1]][[i]][[1]][[4]]

    val <- tryCatch(tip[[r]][[c]], error = function(e) NA_real_)

    if (!is.finite(val)) return(NA_real_)

    as.numeric(val)
  }

  xr <- range(l3dx$x, na.rm = TRUE)
  pad <- x_offset_ratio * diff(xr)

  if (!is.finite(pad) || pad == 0) pad <- 0.05

  x_min_p <- xr[1] - pad
  x_max_p <- xr[2] + pad

  gx <- sort(unique(l3dx$x))

  mid_per_y <- lapply(split(l3dx$x, l3dx$y), function(xv) {
    xv <- sort(unique(xv))
    if (length(xv) >= 2) {
      (xv[-1] + xv[-length(xv)]) / 2
    } else {
      numeric(0)
    }
  })

  mid_all <- sort(unique(unlist(mid_per_y)))

  dense <- seq(
    x_min_p,
    x_max_p,
    length.out = max(x_spline_points, length(gx) + length(mid_all) + 2)
  )

  x_grid <- sort(unique(c(x_min_p, x_max_p, gx, mid_all, dense)))

  match_with_tol <- function(vals, grid, tol = 1e-9, fallback_nearest = TRUE) {
    vapply(vals, function(v) {
      if (!is.finite(v)) return(NA_integer_)

      hit <- which.min(abs(grid - v))

      if (length(hit) && abs(grid[hit] - v) <= tol) {
        hit
      } else if (fallback_nearest) {
        hit
      } else {
        NA_integer_
      }
    }, integer(1))
  }

  build_seeds_for_y <- function(i) {
    yi <- de[i]

    st_raw <- l3dx[
      l3dx$y == yi,
      c("ssid", "x", "y", "z", "group_id"),
      drop = FALSE
    ]

    ## Collapse multiple subgroups at the same group-x for the surface.
    ## The overlay trajectories are not collapsed.
    if (nrow(st_raw)) {
      st <- st_raw %>%
        dplyr::group_by(x, y, group_id) %>%
        dplyr::arrange(z, .by_group = TRUE) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        as.data.frame()

      st$type <- "stable"
    } else {
      st <- data.frame()
    }

    mids <- data.frame()

    if (nrow(st) >= 2) {
      st_ord <- st[order(st$x), , drop = FALSE]

      mids_list <- lapply(seq_len(nrow(st_ord) - 1), function(k) {
        ssA <- st_ord$ssid[k]
        ssB <- st_ord$ssid[k + 1]

        mz <- get_tip_energy(i, ssA, ssB)

        if (!is.finite(mz)) return(NULL)

        data.frame(
          ssid = paste0("MID:", ssA, "|", ssB),
          x = (st_ord$x[k] + st_ord$x[k + 1]) / 2,
          y = yi,
          z = mz,
          type = "midpoint",
          stringsAsFactors = FALSE
        )
      })

      mids <- dplyr::bind_rows(Filter(Negate(is.null), mids_list))
    }

    half <- floor(nY / 2)

    edgeL0 <- edgeR0 <- data.frame()

    if (half > 0) {
      if (i > nY - half) {
        edgeL0 <- data.frame(
          ssid = "EDGE_LEFT_ZERO",
          x = x_min_p,
          y = yi,
          z = 0,
          type = "edge_zero"
        )
      }

      if (i <= half) {
        edgeR0 <- data.frame(
          ssid = "EDGE_RIGHT_ZERO",
          x = x_max_p,
          y = yi,
          z = 0,
          type = "edge_zero"
        )
      }
    }

    edgeLh <- edgeRh <- data.frame()

    if (nrow(st) >= 1 && half > 0) {
      st_ord <- st[order(st$x), , drop = FALSE]

      if (i <= half) {
        z_left_half <- as.numeric(st_ord$z[1]) / 2

        if (is.finite(z_left_half)) {
          edgeLh <- data.frame(
            ssid = "EDGE_LEFT_HALF",
            x = x_min_p,
            y = yi,
            z = z_left_half,
            type = "edge_half"
          )
        }
      }

      if (i > nY - half) {
        z_right_half <- as.numeric(st_ord$z[nrow(st_ord)]) / 2

        if (is.finite(z_right_half)) {
          edgeRh <- data.frame(
            ssid = "EDGE_RIGHT_HALF",
            x = x_max_p,
            y = yi,
            z = z_right_half,
            type = "edge_half"
          )
        }
      }
    }

    dplyr::bind_rows(st, mids, edgeL0, edgeR0, edgeLh, edgeRh)
  }

  seeds_all <- lapply(seq_len(nY), build_seeds_for_y)

  interp_row_linear <- function(dat, yi) {
    dat <- dat[order(dat$x), , drop = FALSE]

    if (nrow(dat) == 1) {
      return(data.frame(
        x = x_grid,
        y = rep(yi, length(x_grid)),
        z = rep(dat$z[1], length(x_grid))
      ))
    }

    zhat <- rep(NA_real_, length(x_grid))

    for (k in seq_len(nrow(dat) - 1)) {
      x1 <- dat$x[k]
      z1 <- dat$z[k]

      x2 <- dat$x[k + 1]
      z2 <- dat$z[k + 1]

      if (!is.finite(x2 - x1) || abs(x2 - x1) < .Machine$double.eps^0.5) next

      j <- which(x_grid >= x1 & x_grid <= x2)

      if (length(j)) {
        tt <- (x_grid[j] - x1) / (x2 - x1)
        zhat[j] <- (1 - tt) * z1 + tt * z2
      }
    }

    zhat[is.na(zhat)] <- 0

    data.frame(
      x = x_grid,
      y = rep(yi, length(x_grid)),
      z = zhat
    )
  }

  interp_row_spline <- function(dat, yi) {
    dat <- dat[order(dat$x), , drop = FALSE]

    sf <- tryCatch(
      splinefun(dat$x, dat$z, method = "monoH.FC"),
      error = function(e) NULL
    )

    zhat <- if (is.null(sf)) {
      approx(dat$x, dat$z, xout = x_grid, rule = 2)$y
    } else {
      sf(x_grid)
    }

    data.frame(
      x = x_grid,
      y = rep(yi, length(x_grid)),
      z = as.numeric(zhat)
    )
  }

  build_row <- function(i) {
    yi <- de[i]

    dat <- seeds_all[[i]]
    dat <- dat[is.finite(dat$x) & is.finite(dat$z), , drop = FALSE]

    if (!nrow(dat)) {
      return(list(
        row = data.frame(
          x = x_grid,
          y = rep(yi, length(x_grid)),
          z = rep(0, length(x_grid))
        ),
        pins = data.frame(
          x_idx = integer(0),
          y_idx = integer(0),
          z = numeric(0),
          type = character(0)
        )
      ))
    }

    dat <- dat[order(dat$x), , drop = FALSE]

    row_df <- if (
      nrow(dat) == 1 ||
        length(unique(dat$x)) == 1 ||
        x_interp == "linear"
    ) {
      interp_row_linear(dat, yi)
    } else {
      interp_row_spline(dat, yi)
    }

    need_types <- if (pin_midpoints) {
      c("stable", "midpoint")
    } else {
      "stable"
    }

    pins <- dat[
      dat$type %in% need_types,
      c("x", "z", "type"),
      drop = FALSE
    ]

    xi <- if (nrow(pins)) {
      match_with_tol(
        pins$x,
        x_grid,
        tol = 1e-9,
        fallback_nearest = TRUE
      )
    } else {
      integer(0)
    }

    pins_df <- if (length(xi)) {
      data.frame(
        x_idx = xi,
        y_idx = i,
        z = pins$z,
        type = pins$type
      )
    } else {
      data.frame(
        x_idx = integer(0),
        y_idx = integer(0),
        z = numeric(0),
        type = character(0)
      )
    }

    list(row = row_df, pins = pins_df)
  }

  rows <- vector("list", nY)
  pins_all <- vector("list", nY)

  for (i in seq_len(nY)) {
    out <- build_row(i)
    rows[[i]] <- out$row
    pins_all[[i]] <- out$pins
  }

  surf3dx <- dplyr::bind_rows(rows)
  pins_all <- dplyr::bind_rows(pins_all)

  if (y_smooth_window >= 3 && y_smooth_window %% 2 == 1) {
    smoothed <- surf3dx

    for (j in seq_along(x_grid)) {
      idx <- which(abs(surf3dx$x - x_grid[j]) < .Machine$double.eps^0.5)
      idx <- idx[order(surf3dx$y[idx])]

      zvec <- smoothed$z[idx]

      if (length(zvec) >= y_smooth_window) {
        kernel <- rep(1 / y_smooth_window, y_smooth_window)
        zf <- stats::filter(zvec, kernel, sides = 2, circular = FALSE)

        first <- which(!is.na(zf))[1]
        last <- tail(which(!is.na(zf)), 1)

        if (!is.na(first)) {
          if (first > 1) zf[1:(first - 1)] <- zf[first]
          if (last < length(zf)) zf[(last + 1):length(zf)] <- zf[last]
        } else {
          zf[] <- 0
        }

        smoothed$z[idx] <- as.numeric(zf)
      }
    }

    surf3dx <- smoothed
  }

  if (nrow(pins_all)) {
    L <- length(x_grid)

    rows_to_fix <- (pins_all$y_idx - 1L) * L + pins_all$x_idx

    keep <- rows_to_fix >= 1 & rows_to_fix <= nrow(surf3dx)

    rows_to_fix <- rows_to_fix[keep]
    z_fix <- pins_all$z[keep]

    surf3dx$z[rows_to_fix] <- z_fix
  }

  return(surf3dx)
}

## ============================================================
## Main object builder
## ============================================================

GELSobj3 <- function(
  gela,
  sa,
  ssd_grouping,
  x_spline_points = 200,
  y_smooth_window = 0,
  x_offset_ratio = 0.05,
  pin_midpoints = TRUE,
  x_interp = c("linear", "spline"),

  ## group-level alignment controls
  w_adj = 20,
  w_energy = 2,
  w_mem = 1,
  w_gap = 5,
  adj_only = TRUE,
  adj_energy_scale = "median"
) {
  x_interp <- match.arg(x_interp)

  if (is.list(sa) && !is.null(sa[[1]])) {
    s <- nrow(sa[[1]])
  } else {
    stop("sa must be a list whose first element is the parameter matrix.")
  }

  l3dx <- GELS_build_l3dx(
    gela = gela,
    sa = sa,
    ssd_grouping = ssd_grouping,
    w_adj = w_adj,
    w_energy = w_energy,
    w_mem = w_mem,
    w_gap = w_gap,
    adj_only = adj_only,
    adj_energy_scale = adj_energy_scale
  )

  if (is.null(l3dx) || !nrow(l3dx)) {
    warning("GELSobj3: l3dx is empty. Returning empty surface.")

    empty_surface <- data.frame(
      x = numeric(0),
      y = numeric(0),
      z = numeric(0)
    )

    return(list(
      surf3dx = empty_surface,
      l3dx = l3dx,
      s = s
    ))
  }

  surf3dx <- GELS_build_surface_from_l3dx(
    gela = gela,
    l3dx = l3dx,
    x_spline_points = x_spline_points,
    y_smooth_window = y_smooth_window,
    x_offset_ratio = x_offset_ratio,
    pin_midpoints = pin_midpoints,
    x_interp = x_interp
  )

  return(list(
    surf3dx = surf3dx,
    l3dx = l3dx,
    s = s
  ))
}

## ============================================================
## Visualization helpers
## ============================================================

showL3DX <- function(
  l3dx,
  theta = 0,
  phi = 70,
  z_lift_ratio = 0.7,
  point_lwd = 4,
  label_col = "Cg_s",
  default_label_color = "blue"
) {
  stopifnot(all(c("Index", "x", "y", "z") %in% colnames(l3dx)))

  base_cols <- RColorBrewer::brewer.pal(9, "Set1")

  xr <- range(l3dx$x, na.rm = TRUE)
  yr <- range(l3dx$y, na.rm = TRUE)
  zr <- range(l3dx$z, na.rm = TRUE)

  z_lift <- z_lift_ratio * diff(zr)

  plot3D::perspbox(
    x = xr,
    y = yr,
    z = zr,
    theta = theta,
    phi = phi,
    bty = "b2",
    col = NULL,
    shade = 0,
    xlab = "X",
    ylab = "Y",
    zlab = "Energy",
    ticktype = "detailed"
  )

  sp <- split(l3dx, l3dx$Index)

  for (a_l3d in sp) {
    tp <- ifelse(length(a_l3d$x) == 1, "p", "l")

    xp <- unlist(a_l3d$x)
    yp <- unlist(a_l3d$y)
    zp <- unlist(a_l3d$z)

    col_i <- "#0072B2"

    if ("group_id" %in% names(a_l3d) && !all(is.na(a_l3d$group_id))) {
      g <- a_l3d$group_id[[1]]

      if (!is.na(g)) {
        col_i <- base_cols[(g - 1) %% length(base_cols) + 1]
      }
    }

    plot3D::scatter3D(
      xp,
      yp,
      zp,
      type = tp,
      add = TRUE,
      col = col_i,
      lwd = point_lwd
    )

    lbl <- NULL

    if (label_col %in% names(a_l3d) && !all(is.na(a_l3d[[label_col]]))) {
      lbl <- as.character(a_l3d[[label_col]][[1]])
    }

    if (!is.null(lbl) && nzchar(lbl)) {
      plot3D::text3D(
        mean(xp, na.rm = TRUE),
        mean(yp, na.rm = TRUE),
        mean(zp, na.rm = TRUE) + z_lift,
        labels = lbl,
        add = TRUE,
        cex = 1.2,
        col = default_label_color
      )
    }
  }
}

showGELS3D2 <- function(
  gelsobj,
  theta = 0,
  phi = 70,
  mode = c("shaded_gray", "wire_gray"),
  z_compress = 0.5,
  mesh_border = "gray50",
  alpha_surface = 0.5,
  alpha_paths = 1.0,
  x_lab = "Community composition group",
  y_lab = "y",
  return_replayer = FALSE
) {
  mode <- match.arg(mode)

  surf3d <- gelsobj[[1]]
  l3d <- gelsobj[[2]]

  .draw_once <- function() {
    GetMesh <- function(surf3d, stride_x = 5, stride_y = 1) {
      x_unique <- sort(unique(surf3d$x))
      y_unique <- sort(unique(surf3d$y))

      x_sel <- x_unique[seq(1, length(x_unique), by = stride_x)]
      y_sel <- y_unique[seq(1, length(y_unique), by = stride_y)]

      nx <- length(x_sel)
      ny <- length(y_sel)

      x <- matrix(0, nrow = nx, ncol = ny)
      y <- matrix(0, nrow = nx, ncol = ny)
      z <- matrix(0, nrow = nx, ncol = ny)

      for (i in seq_len(nx)) {
        for (j in seq_len(ny)) {
          idx <- which.min(
            abs(surf3d$x - x_sel[i]) +
              abs(surf3d$y - y_sel[j])
          )

          x[i, j] <- surf3d[idx, "x"]
          y[i, j] <- surf3d[idx, "y"]
          z[i, j] <- surf3d[idx, "z"]
        }
      }

      list(x, y, z)
    }

    mesh <- GetMesh(surf3d)

    x <- mesh[[1]]
    y <- mesh[[2]]
    z <- mesh[[3]]

    dz <- diff(range(z, na.rm = TRUE))
    z_lift <- 0.5 * dz

    gray_pal <- grDevices::colorRampPalette(c("white", "black"))(200)
    gray_pal_alpha <- grDevices::adjustcolor(
      gray_pal,
      alpha.f = alpha_surface
    )

    if (mode == "wire_gray") {
      plot3D::persp3D(
        x,
        y,
        z,
        theta = theta,
        phi = phi,
        expand = z_compress,
        col = NA,
        facets = NA,
        border = mesh_border,
        lwd = 0.6,
        colkey = FALSE,
        shade = 0,
        lighting = FALSE,
        box = TRUE,
        bty = "b2",
        ticktype = "detailed",
        main = "",
        xlab = x_lab,
        ylab = y_lab,
        zlab = "Energy"
      )
    } else {
      plot3D::persp3D(
        x,
        y,
        z,
        theta = theta,
        phi = phi,
        expand = z_compress,
        colvar = z,
        col = gray_pal_alpha,
        facets = TRUE,
        alpha = alpha_surface,
        border = mesh_border,
        lwd = 0.6,
        colkey = TRUE,
        shade = TRUE,
        lighting = TRUE,
        box = TRUE,
        bty = "b2",
        ticktype = "detailed",
        main = "",
        xlab = x_lab,
        ylab = y_lab,
        zlab = "Energy"
      )
    }

    base_cols <- RColorBrewer::brewer.pal(9, "Set1")

    for (a_l3d in split(l3d, l3d$Index)) {
      tp <- ifelse(length(a_l3d$x) == 1, "p", "l")

      xp <- unlist(a_l3d$x)
      yp <- unlist(a_l3d$y)
      zp <- unlist(a_l3d$z)

      col_i <- "#0072B2"

      if (!is.null(a_l3d$group_id) && !all(is.na(a_l3d$group_id))) {
        g <- a_l3d$group_id[[1]]

        if (!is.na(g)) {
          col_i <- base_cols[(g - 1) %% length(base_cols) + 1]
        }
      }

      col_i <- grDevices::adjustcolor(col_i, alpha.f = alpha_paths)

      plot3D::scatter3D(
        xp,
        yp,
        zp,
        type = tp,
        add = TRUE,
        col = col_i,
        lwd = 4
      )

      lbl <- if (!is.null(a_l3d$Cg_s) && !all(is.na(a_l3d$Cg_s))) {
        a_l3d$Cg_s[[1]]
      } else {
        ""
      }

      plot3D::text3D(
        mean(xp, na.rm = TRUE),
        mean(yp, na.rm = TRUE),
        mean(zp, na.rm = TRUE) + z_lift,
        labels = lbl,
        add = TRUE,
        cex = 1.2,
        col = "cyan"
      )
    }

    invisible(NULL)
  }

  .draw_once()

  if (isTRUE(return_replayer)) {
    replayer <- function() .draw_once()
    return(replayer)
  } else {
    return(invisible(NULL))
  }
}

# 多形式・保存用 ################
save_replayer_multi <- function(
  replayer,
  file,
  device = c("png", "eps", "tiff"),
  width_px = 2400,
  height_px = 2400,
  res_dpi = 300,
  bg = "white"
) {
  device <- match.arg(
    device,
    choices = c("png", "eps", "tiff"),
    several.ok = TRUE
  )

  make_output_filename <- function(file, ext) {
    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)
    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  width_in <- width_px / res_dpi
  height_in <- height_px / res_dpi

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    grDevices::png(
      filename = png_file,
      width = width_px,
      height = height_px,
      units = "px",
      res = res_dpi,
      bg = bg
    )

    replayer()
    grDevices::dev.off()

    output_files <- c(output_files, png_file)
  }

  if ("tiff" %in% device) {
    tiff_file <- make_output_filename(file, "tiff")

    grDevices::tiff(
      filename = tiff_file,
      width = width_px,
      height = height_px,
      units = "px",
      res = res_dpi,
      compression = "lzw",
      bg = bg
    )

    replayer()
    grDevices::dev.off()

    output_files <- c(output_files, tiff_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      grDevices::cairo_ps(
        filename = eps_file,
        width = width_in,
        height = height_in,
        onefile = FALSE,
        bg = bg,
        fallback_resolution = res_dpi
      )
    } else {
      grDevices::postscript(
        file = eps_file,
        width = width_in,
        height = height_in,
        onefile = FALSE,
        horizontal = FALSE,
        paper = "special",
        bg = bg
      )
    }

    replayer()
    grDevices::dev.off()

    output_files <- c(output_files, eps_file)
  }

  output_files
}

## ============================================================
## 13. g_FAST coefficient bar plot
##
## This horizontal bar plot shows taxon-level g_FAST coefficients,
## with positive coefficients at the top and negative coefficients below.
## ============================================================

plot_gFAST_bar <- function(
  ge,
  score_col = "FAST_score",
  order_decreasing = TRUE,

  device = "screen",
  file = NULL,

  width_in = 6,
  height_in = 6,
  res_dpi = 300,
  bg = "white",

  positive_color = "green",
  negative_color = "orange",

  xlab = NULL,
  ylab = expression("g"[FAST]),
  title = "",

  axis_text_y_size = 10,
  axis_title_y_size = 16,
  axis_title_x_size = 14
) {

  ## ------------------------------------------------------------
  ## Required package
  ## ------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  ## ------------------------------------------------------------
  ## Device handling
  ## ------------------------------------------------------------

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Prepare data
  ## ------------------------------------------------------------

  data <- as.data.frame(ge)

  if (!(score_col %in% colnames(data))) {
    if (ncol(data) == 1) {
      colnames(data)[1] <- score_col
    } else {
      stop("`score_col` was not found in `ge`.")
    }
  }

  if (is.null(rownames(data))) {
    rownames(data) <- paste0("Taxon_", seq_len(nrow(data)))
  }

  data$label <- rownames(data)
  data[[score_col]] <- as.numeric(data[[score_col]])

  data <- data[
    is.finite(data[[score_col]]),
    ,
    drop = FALSE
  ]

  ## High values first
  data_sorted <- data[
    order(data[[score_col]], decreasing = order_decreasing),
    ,
    drop = FALSE
  ]

  ## Reverse factor levels for coord_flip so that
  ## positive values appear at the top and negative at the bottom
  data_sorted$label <- factor(
    data_sorted$label,
    levels = rev(data_sorted$label)
  )

  data_sorted$positive <- data_sorted[[score_col]] > 0

  ## ------------------------------------------------------------
  ## Plot
  ## ------------------------------------------------------------

  p <- ggplot2::ggplot(
    data_sorted,
    ggplot2::aes(x = label, y = .data[[score_col]])
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      ggplot2::aes(fill = positive),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "FALSE" = negative_color,
        "TRUE" = positive_color
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = axis_text_y_size),
      axis.title.y = ggplot2::element_text(size = axis_title_y_size),
      axis.title.x = ggplot2::element_text(size = axis_title_x_size),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = bg, color = NA),
      panel.background = ggplot2::element_rect(fill = bg, color = NA)
    )

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/g_FAST_bar'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(p)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggplot2::ggsave(
      filename = png_file,
      plot = p,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    plot = p,
    data = data_sorted,
    params = list(
      score_col = score_col,
      order_decreasing = order_decreasing,
      device = device,
      file = file,
      output_files = output_files,
      width_in = width_in,
      height_in = height_in,
      res_dpi = res_dpi
    )
  )

  return(out)
}


## Interaction network plot with PNG / EPS output
## ============================================================

## ============================================================
## 14. Signed interaction matrix thresholding and circular network plot
##
## These functions threshold the interaction matrix J and visualise the
## strongest positive/negative associations as a circular network, with
## node color representing g_FAST.
## ============================================================

threshold_correlation_matrix <- function(
  cor_mat,
  X_ratio = 0.2,
  diag_zero = TRUE
) {
  if (!is.matrix(cor_mat)) {
    stop("Input must be a matrix.")
  }

  if (X_ratio < 0 || X_ratio > 1) {
    stop("X_ratio must be between 0 and 1.")
  }

  cor_mat <- as.matrix(cor_mat)

  if (nrow(cor_mat) != ncol(cor_mat)) {
    stop("Input matrix must be square.")
  }

  if (diag_zero) {
    diag(cor_mat) <- 0
  }

  cor_mat_filtered <- cor_mat

  ## Flatten the matrix excluding diagonal
  upper_vals <- cor_mat[upper.tri(cor_mat)]
  lower_vals <- cor_mat[lower.tri(cor_mat)]

  ## Positive values
  pos_vals <- upper_vals[is.finite(upper_vals) & upper_vals > 0]

  if (length(pos_vals) > 0) {
    pos_thresh <- stats::quantile(
      pos_vals,
      probs = X_ratio,
      na.rm = TRUE,
      names = FALSE
    )

    cor_mat_filtered[
      cor_mat > 0 & cor_mat < pos_thresh
    ] <- 0
  }

  ## Negative values
  neg_vals <- lower_vals[is.finite(lower_vals) & lower_vals < 0]

  if (length(neg_vals) > 0) {
    neg_thresh <- stats::quantile(
      neg_vals,
      probs = 1 - X_ratio,
      na.rm = TRUE,
      names = FALSE
    )

    cor_mat_filtered[
      cor_mat < 0 & cor_mat > neg_thresh
    ] <- 0
  }

  if (diag_zero) {
    diag(cor_mat_filtered) <- 0
  }

  return(cor_mat_filtered)
}


plot_interaction_network <- function(
  cor_mat,
  node_score,

  threshold_ratio = 0.8,
  apply_threshold = TRUE,
  diag_zero = TRUE,

  radius = 0.85,
  lab_lim = 1.02,
  coord_lim = 0.95,

  edge_alpha = 0.8,
  edge_width_range = c(0.2, 1.6),
  edge_low = "red",
  edge_mid = "gray80",
  edge_high = "blue",

  node_size = 4,
  node_low = "orange",
  node_mid = "gray90",
  node_high = "green",

  label_size = 3.5,
  point_padding = 0.2,
  box_padding = 0.25,
  max_overlaps = Inf,

  device = "screen",
  file = NULL,

  width_in = 6.5,
  height_in = 6.5,
  res_dpi = 300,
  bg = "white"
) {

  ## ------------------------------------------------------------
  ## Required packages
  ## ------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required.")
  }

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required.")
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.")
  }

  ## ------------------------------------------------------------
  ## Device handling
  ## ------------------------------------------------------------

  device <- match.arg(
    device,
    choices = c("screen", "png", "eps", "none"),
    several.ok = TRUE
  )

  device <- unique(device)

  if (length(device) > 1 && "none" %in% device) {
    device <- setdiff(device, "none")
  }

  ## ------------------------------------------------------------
  ## Prepare matrix
  ## ------------------------------------------------------------

  cor_mat <- as.matrix(cor_mat)

  if (nrow(cor_mat) != ncol(cor_mat)) {
    stop("cor_mat must be a square matrix.")
  }

  if (is.null(rownames(cor_mat))) {
    rownames(cor_mat) <- paste0("Taxon_", seq_len(nrow(cor_mat)))
  }

  if (is.null(colnames(cor_mat))) {
    colnames(cor_mat) <- rownames(cor_mat)
  }

  if (diag_zero) {
    diag(cor_mat) <- 0
  }

  if (apply_threshold) {
    cor_mat_filtered <- threshold_correlation_matrix(
      cor_mat,
      X_ratio = threshold_ratio,
      diag_zero = diag_zero
    )
  } else {
    cor_mat_filtered <- cor_mat
  }

  ## ------------------------------------------------------------
  ## Prepare node score
  ## ------------------------------------------------------------

  if (is.matrix(node_score) || is.data.frame(node_score)) {
    node_score_vec <- as.numeric(node_score[, 1])
    score_names <- rownames(node_score)
  } else {
    node_score_vec <- as.numeric(node_score)
    score_names <- names(node_score)
  }

  if (!is.null(score_names) && all(rownames(cor_mat_filtered) %in% score_names)) {
    node_score_vec <- node_score_vec[
      match(rownames(cor_mat_filtered), score_names)
    ]
  }

  if (length(node_score_vec) != nrow(cor_mat_filtered)) {
    stop("Length of node_score must match nrow(cor_mat).")
  }

  names(node_score_vec) <- rownames(cor_mat_filtered)

  if (any(!is.finite(node_score_vec))) {
    stop("node_score contains non-finite values.")
  }

  ## ------------------------------------------------------------
  ## Build edge list
  ## ------------------------------------------------------------

  edges <- which(
    abs(cor_mat_filtered) > 0 & upper.tri(cor_mat_filtered),
    arr.ind = TRUE
  )

  if (nrow(edges) > 0) {
    edge_df <- data.frame(
      from = rownames(cor_mat_filtered)[edges[, 1]],
      to = colnames(cor_mat_filtered)[edges[, 2]],
      weight = cor_mat_filtered[edges],
      stringsAsFactors = FALSE
    )
  } else {
    edge_df <- data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  ## ------------------------------------------------------------
  ## Node coordinates
  ## Sort node names by score, ascending
  ## Start from 12 o'clock and go clockwise
  ## ------------------------------------------------------------

  sorted_nodes <- names(sort(node_score_vec, decreasing = FALSE))

  n <- length(sorted_nodes)

  angles <- seq(
    pi / 2,
    pi / 2 - 2 * pi + 2 * pi / n,
    length.out = n
  )

  coords_df <- tibble::tibble(
    name = sorted_nodes,
    score = node_score_vec[sorted_nodes],
    angle = angles,
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

  ## ------------------------------------------------------------
  ## Build graph
  ## ------------------------------------------------------------

  g <- igraph::graph_from_data_frame(
    d = edge_df,
    vertices = coords_df,
    directed = FALSE
  )

  ## ------------------------------------------------------------
  ## Plot
  ## ------------------------------------------------------------

  p <- ggraph::ggraph(
    g,
    layout = "manual",
    x = igraph::V(g)$x,
    y = igraph::V(g)$y
  )

  if (igraph::ecount(g) > 0) {
    p <- p +
      ggraph::geom_edge_link(
        ggplot2::aes(
          color = weight,
          width = abs(weight)
        ),
        alpha = edge_alpha
      ) +
      ggraph::scale_edge_color_gradient2(
        low = edge_low,
        mid = edge_mid,
        high = edge_high,
        midpoint = 0
      ) +
      ggraph::scale_edge_width(
        range = edge_width_range,
        guide = "none"
      )
  }

  p <- p +
    ggraph::geom_node_point(
      ggplot2::aes(color = score),
      size = node_size
    ) +
    ggplot2::scale_color_gradient2(
      low = node_low,
      mid = node_mid,
      high = node_high,
      midpoint = 0
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      repel = TRUE,
      size = label_size,
      xlim = c(-lab_lim, lab_lim),
      ylim = c(-lab_lim, lab_lim),
      point.padding = point_padding,
      box.padding = box_padding,
      max.overlaps = max_overlaps
    ) +
    ggplot2::coord_equal(
      xlim = c(-coord_lim, coord_lim),
      ylim = c(-coord_lim, coord_lim),
      clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA)
    )

  ## ------------------------------------------------------------
  ## Helper for output filename
  ## ------------------------------------------------------------

  make_output_filename <- function(file, ext) {
    if (is.null(file)) {
      stop(
        "When saving png/eps, please provide `file` without extension, ",
        "e.g. file = 'ela_fast/network'."
      )
    }

    base <- tools::file_path_sans_ext(file)
    filename <- paste0(base, ".", ext)

    out_dir <- dirname(filename)

    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename
  }

  output_files <- character(0)

  ## ------------------------------------------------------------
  ## Render / save
  ## ------------------------------------------------------------

  if ("screen" %in% device) {
    print(p)
  }

  if ("png" %in% device) {
    png_file <- make_output_filename(file, "png")

    ggplot2::ggsave(
      filename = png_file,
      plot = p,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = res_dpi,
      bg = bg
    )

    output_files <- c(output_files, png_file)
  }

  if ("eps" %in% device) {
    eps_file <- make_output_filename(file, "eps")

    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = function(...) {
          grDevices::cairo_ps(
            ...,
            onefile = FALSE,
            fallback_resolution = res_dpi
          )
        },
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    } else {
      ggplot2::ggsave(
        filename = eps_file,
        plot = p,
        device = "eps",
        width = width_in,
        height = height_in,
        units = "in",
        bg = bg
      )
    }

    output_files <- c(output_files, eps_file)
  }

  ## ------------------------------------------------------------
  ## Return
  ## ------------------------------------------------------------

  out <- list(
    plot = p,
    graph = g,
    thresholded_matrix = cor_mat_filtered,
    edge_data = edge_df,
    node_data = coords_df,
    params = list(
      threshold_ratio = threshold_ratio,
      apply_threshold = apply_threshold,
      device = device,
      file = file,
      output_files = output_files,
      width_in = width_in,
      height_in = height_in,
      res_dpi = res_dpi
    )
  )

  return(out)
}
