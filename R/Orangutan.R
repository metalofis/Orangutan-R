# ----------------- Utilities -----------------
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# Filename sanitization
sanitize_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("[^A-Za-z0-9._-]", "_", x)
  x
}

# Species-aware color palette construction
build_palette <- function(species_vec, palette_name = "Paired", max_palette_colors = 12, custom_colors = NULL) {
  species_vec <- sort(unique(as.character(species_vec)))
  n <- length(species_vec)
  if (n <= 0) {
    return(character(0))
  }

  if (n == 1) {
    cols <- "#1B9E77"
  } else if (n == 2) {
    cols <- grDevices::hcl.colors(2, palette = "Set3")
  } else {
    n_pal <- min(max(3, n), max_palette_colors)
    cols_base <- RColorBrewer::brewer.pal(n_pal, palette_name)
    if (n_pal < n) {
      cols <- grDevices::colorRampPalette(cols_base)(n)
    } else {
      cols <- cols_base[1:n]
    }
  }

  cols_named <- setNames(cols, species_vec)

  if (!is.null(custom_colors)) {
    for (sp in names(custom_colors)) {
      if (sp %in% names(cols_named)) {
        cols_named[[sp]] <- custom_colors[[sp]]
      }
    }
  }

  cols_named
}

# Dynamic ggplot theme helper (dependent on label_aes)
univariate_theme_fn <- function(label_aes = list(axis_text_size = 10)) {
  axis_text_size <- if (!is.null(label_aes$axis_text_size)) label_aes$axis_text_size else 10

  list(
    theme_minimal(),
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ),
    theme(
      legend.position = "none",
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size),
      axis.text.y = element_text(size = axis_text_size)
    )
  )
}

# Factory for mean summary overlay layer (so mean aesthetics are configurable)
make_mean_layer <- function(mean_aes = list(size = 1.8, shape = 21, fill = "white", color = "black", stroke = 0.6)) {
  size <- if (!is.null(mean_aes$size)) mean_aes$size else 1.8
  shape <- if (!is.null(mean_aes$shape)) mean_aes$shape else 21
  fill <- if (!is.null(mean_aes$fill)) mean_aes$fill else "white"
  color <- if (!is.null(mean_aes$color)) mean_aes$color else "black"
  stroke <- if (!is.null(mean_aes$stroke)) mean_aes$stroke else 0.6

  stat_summary(
    fun = mean,
    geom = "point",
    shape = shape,
    size = size,
    fill = fill,
    color = color,
    stroke = stroke
  )
}

# Device-safe plot saving
save_plot <- function(plot, filename, width = 7.5, height = 6) {
  ext <- tolower(tools::file_ext(filename))
  if (ext != "pdf") {
    warning("save_plot: recommended filename extension is .pdf; saving as ", ext)
  }

  if (ext == "pdf") {
    png_dir <- file.path(dirname(filename), "pngs")
    if (!dir.exists(png_dir)) dir.create(png_dir, showWarnings = FALSE)
    png_path <- file.path(png_dir, sub("\\.pdf$", ".png", basename(filename), ignore.case = TRUE))
    if (requireNamespace("ragg", quietly = TRUE) && "agg_png" %in% ls(getNamespace("ragg"))) {
      ggsave(filename = png_path, plot = plot, width = width, height = height, device = getExportedValue("ragg", "agg_png"), bg = "white")
    } else {
      ggsave(filename = png_path, plot = plot, width = width, height = height, device = grDevices::png, bg = "white")
    }
  }

  if (requireNamespace("ragg", quietly = TRUE) && "agg_pdf" %in% ls(getNamespace("ragg"))) {
    ggsave(filename = filename, plot = plot, width = width, height = height, device = getExportedValue("ragg", "agg_pdf"))
    return(invisible(TRUE))
  }

  if (capabilities("cairo") && ext == "pdf") {
    ggsave(filename = filename, plot = plot, width = width, height = height, device = cairo_pdf)
    return(invisible(TRUE))
  }

  if (ext == "pdf") {
    ggsave(filename = filename, plot = plot, width = width, height = height, device = grDevices::pdf)
    return(invisible(TRUE))
  }

  if (requireNamespace("ragg", quietly = TRUE)) {
    dev_map <- list(png = getExportedValue("ragg", "agg_png"), tiff = getExportedValue("ragg", "agg_tiff"), tif = getExportedValue("ragg", "agg_tiff"), jpg = getExportedValue("ragg", "agg_jpeg"), jpeg = getExportedValue("ragg", "agg_jpeg"), svg = getExportedValue("ragg", "agg_svg"))
    devfun <- dev_map[[ext]]
    if (!is.null(devfun)) {
      ggsave(filename = filename, plot = plot, width = width, height = height, device = devfun)
      return(invisible(TRUE))
    }
  }

  if (ext %in% c("png", "jpeg", "jpg")) {
    ggsave(filename = filename, plot = plot, width = width, height = height, device = grDevices::png)
  } else {
    ggsave(filename = filename, plot = plot, width = width, height = height, device = grDevices::pdf)
  }

  invisible(TRUE)
}

# ----------------- I/O & basic preprocessing functions -----------------
load_data <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  d <- read.csv(path, stringsAsFactors = FALSE)
  rownames(d) <- seq_len(nrow(d))
  d
}

basic_clean <- function(data) {
  # Drop accidental X column from read.csv and require species
  if ("X" %in% colnames(data)) data <- data %>% select(-X)
  if (!"species" %in% colnames(data)) stop("The 'species' column is required in the dataset.")
  data$species <- as.character(data$species)
  cleaned <- data %>% arrange(species)
  species_levels <- sort(unique(cleaned$species))
  cleaned$species <- factor(cleaned$species, levels = species_levels)
  list(data = cleaned, species_levels = species_levels)
}

classify_traits <- function(df) {
  numeric_columns <- names(df)[sapply(df, is.numeric)]
  is_mensural <- sapply(numeric_columns, function(v) {
    col <- df[[v]]
    any((col %% 1) != 0, na.rm = TRUE)
  })
  mensural <- numeric_columns[is_mensural]
  meristic <- setdiff(numeric_columns, mensural)
  list(mensural = mensural, meristic = meristic)
}

# ----------------- Allometry -----------------
apply_allometric_transformation <- function(df, allometry_var) {
  if (!allometry_var %in% names(df)) stop("Allometry variable not found in data: ", allometry_var)
  if (any(df[[allometry_var]] <= 0, na.rm = TRUE)) stop("Allometry variable contains non-positive values.")

  # Mensural candidate columns: numeric & not the allometry_var
  mensural_cols <- names(df)[sapply(df, function(x) is.numeric(x) && any((x %% 1) != 0, na.rm = TRUE)) & names(df) != allometry_var]
  if (length(mensural_cols) == 0) {
    warning("No mensural variables detected for allometry; returning original df.")
    return(df)
  }

  df_adj <- df
  for (v in mensural_cols) {
    vals <- df[[v]]
    if (any(vals <= 0, na.rm = TRUE)) {
      warning("Skipping allometry for ", v, " due to non-positive values.")
      next
    }
    fit <- tryCatch(lm(log(vals) ~ log(df[[allometry_var]])), error = function(e) NULL)
    if (is.null(fit)) next
    df_adj[[paste0(v, "_adj")]] <- residuals(fit)
  }

  # Remove raw mensural columns and keep adjusted ones
  keep_cols <- setdiff(names(df_adj), mensural_cols)
  df_adj <- df_adj[, keep_cols, drop = FALSE]
  df_adj
}

# ----------------- Outlier removal -----------------
remove_outliers_by_species <- function(df, vars, tail_pct = 0.05, verbose = FALSE) {
  if (!all(vars %in% names(df))) stop("Some outlier variables not found in data.")
  if (!is.numeric(tail_pct) || tail_pct <= 0 || tail_pct >= 0.5) stop("tail_pct must be numeric and between 0 and 0.5 (exclusive).")

  removed_samples <- data.frame()
  data_out <- df
  any_removed <- FALSE

  for (v in vars) {
    if (!v %in% colnames(data_out)) {
      if (verbose) message("Variable not found (skipping): ", v)
      next
    }
    keep_global <- rep(TRUE, nrow(data_out))
    removed_total <- 0

    for (sp in levels(data_out$species)) {
      idx <- data_out$species == sp
      vals <- data_out[[v]][idx]
      if (all(is.na(vals))) next
      Q_low <- quantile(vals, tail_pct, na.rm = TRUE)
      Q_high <- quantile(vals, 1 - tail_pct, na.rm = TRUE)
      IQR_est <- Q_high - Q_low
      lower <- Q_low - 1.5 * IQR_est
      upper <- Q_high + 1.5 * IQR_est
      keep_sp <- is.na(vals) | (vals >= lower & vals <= upper)
      if (any(!keep_sp, na.rm = TRUE)) {
        removed_rows <- data_out[idx, , drop = FALSE][!keep_sp, ]
        removed_rows$removed_by_variable <- v
        removed_rows$removed_by_species <- sp
        removed_rows$tail_pct <- tail_pct
        removed_samples <- rbind(removed_samples, removed_rows)
        removed_total <- removed_total + sum(!keep_sp, na.rm = TRUE)
        keep_global[idx] <- keep_sp
        any_removed <- TRUE
      }
    }

    data_out <- data_out[keep_global, , drop = FALSE]
    if (verbose) message(sprintf("Variable '%s': removed %d rows (species-wise Tukey).", v, removed_total))
  }

  # Recompute species factor levels after removals
  if (any_removed && nrow(data_out) > 0) {
    data_out$species <- factor(as.character(data_out$species), levels = sort(unique(as.character(data_out$species))))
  }

  list(data = data_out, removed = removed_samples)
}

# ----------------- Summary statistics -----------------
compute_summary_stats <- function(df, output_dir) {
  numeric_vars <- colnames(df)[sapply(df, is.numeric)]
  summary_stats <- data.frame(Species = character(), Variable = character(), Summary = character(), stringsAsFactors = FALSE)

  for (var in numeric_vars) {
    for (sp in unique(as.character(df$species))) {
      subset_data <- df %>%
        filter(species == sp) %>%
        pull(var)
      mean_val <- mean(subset_data, na.rm = TRUE)
      std_dev <- sd(subset_data, na.rm = TRUE)
      min_val <- min(subset_data, na.rm = TRUE)
      max_val <- max(subset_data, na.rm = TRUE)

      if (all(is.na(subset_data))) {
        summary_string <- NA_character_
      } else {
        summary_string <- paste0(
          round(mean_val, 2), " +/- ", round(std_dev, 2),
          " (", round(min_val, 2), "-", round(max_val, 2), ")"
        )
      }

      summary_stats <- summary_stats %>% add_row(Species = sp, Variable = var, Summary = summary_string)
    }
  }

  summary_stats_reshaped <- summary_stats %>% pivot_wider(names_from = Variable, values_from = Summary)
  out <- file.path(output_dir, "06_summary_stats.csv")
  write.csv(summary_stats_reshaped, file = out, row.names = FALSE)
  out
}

# ----------------- Non-overlapping variables & plotting -----------------
check_no_overlap <- function(species1, species2, variable, df) {
  sp1_values <- na.omit(df[df$species == species1, variable, drop = TRUE])
  sp2_values <- na.omit(df[df$species == species2, variable, drop = TRUE])
  if (length(sp1_values) == 0 || length(sp2_values) == 0) {
    return(NULL)
  }

  sp1_min <- min(sp1_values)
  sp1_max <- max(sp1_values)
  sp2_min <- min(sp2_values)
  sp2_max <- max(sp2_values)
  overlap <- !(sp1_max < sp2_min || sp2_max < sp1_min)

  data.frame(
    species_1 = species1,
    species_2 = species2,
    variable = variable,
    sp1_min = sp1_min,
    sp1_max = sp1_max,
    sp2_min = sp2_min,
    sp2_max = sp2_max,
    overlap = overlap,
    stringsAsFactors = FALSE
  )
}

find_and_plot_nonoverlaps <- function(df, output_dir, custom_colors,
                                      mean_aes = NULL, point_aes = NULL, violin_aes = NULL, box_aes = NULL,
                                      label_aes = NULL, label_templates = NULL, save_plots = TRUE, verbose = FALSE) {
  mean_layer <- make_mean_layer(mean_aes)
  univariate_theme <- univariate_theme_fn(label_aes)

  # fallback defaults for point/violin/box aesthetics
  point_aes <- modifyList(list(point_size = 1.8, jitter_width = 0.1, jitter_alpha = 0.8, jitter_shape = 21, jitter_color = "black", jitter_stroke = 0.35), point_aes %||% list())
  violin_aes <- modifyList(list(alpha = 0.4), violin_aes %||% list())
  box_aes <- modifyList(list(alpha = 0.4, width = 0.15), box_aes %||% list())
  label_aes <- modifyList(list(text_size = 6, label_offset = 0.05), label_aes %||% list())

  variables <- colnames(df)[sapply(df, is.numeric)]
  no_overlap_list <- list()
  species_list <- unique(as.character(df$species))

  if (length(species_list) >= 2) {
    for (var in variables) {
      for (i in 1:(length(species_list) - 1)) {
        for (j in (i + 1):length(species_list)) {
          sp1 <- species_list[i]
          sp2 <- species_list[j]
          res <- tryCatch(check_no_overlap(sp1, sp2, var, df), error = function(e) NULL)
          if (!is.null(res) && res$overlap == FALSE) {
            no_overlap_list[[length(no_overlap_list) + 1]] <- res
          }
        }
      }
    }
  }

  if (length(no_overlap_list) > 0) {
    non_overlapping_pairs <- bind_rows(no_overlap_list)
  } else {
    non_overlapping_pairs <- data.frame(
      species_1 = character(0),
      species_2 = character(0),
      variable  = character(0),
      sp1_min   = numeric(0),
      sp1_max   = numeric(0),
      sp2_min   = numeric(0),
      sp2_max   = numeric(0),
      overlap   = logical(0)
    )
  }

  write.csv(non_overlapping_pairs, file.path(output_dir, "07_nonoverlaps_list.csv"), row.names = FALSE)

  if (nrow(non_overlapping_pairs) == 0) {
    if (verbose) message("All variables overlap for the species pairs. No non-overlap plots produced.")
    return(invisible(non_overlapping_pairs))
  }

  cleaned_long <- df %>% pivot_longer(cols = all_of(colnames(df)[sapply(df, is.numeric)]), names_to = "variable", values_to = "value")
  cleaned_long$species <- factor(cleaned_long$species, levels = names(custom_colors))

  # label template default
  nonoverlap_title_tpl <- if (!is.null(label_templates) && !is.null(label_templates$nonoverlap_title)) label_templates$nonoverlap_title else "Non-Overlapping Pair: %s vs %s for %s"

  for (i in seq_len(nrow(non_overlapping_pairs))) {
    pair <- non_overlapping_pairs[i, ]
    pair_data <- cleaned_long %>% filter(species %in% c(pair$species_1, pair$species_2), variable == pair$variable)
    if (nrow(pair_data) == 0) next

    base_plot <- ggplot(pair_data, aes(x = species, y = value, fill = species)) +
      labs(
        title = sprintf(nonoverlap_title_tpl, pair$species_1, pair$species_2, pair$variable),
        x = "Species",
        y = pair$variable
      ) +
      scale_fill_manual(values = custom_colors) +
      univariate_theme

    # Decide mensural/meristic by checking fractional values OR name ending _adj
    is_mensural <- any((pair_data$value %% 1) != 0, na.rm = TRUE) || grepl("_adj$", pair$variable)

    if (is_mensural) {
      layers <- list(
        geom_violin(trim = FALSE, alpha = violin_aes$alpha, color = NA),
        geom_boxplot(width = box_aes$width, outlier.shape = NA, color = "black", alpha = box_aes$alpha),
        geom_jitter(aes(fill = species), shape = point_aes$jitter_shape, color = point_aes$jitter_color, stroke = point_aes$jitter_stroke, size = point_aes$point_size, width = point_aes$jitter_width, alpha = point_aes$jitter_alpha),
        mean_layer
      )
    } else {
      layers <- list(
        geom_boxplot(width = box_aes$width, outlier.shape = NA, color = "black", alpha = box_aes$alpha),
        geom_jitter(aes(fill = species), shape = point_aes$jitter_shape, color = point_aes$jitter_color, stroke = point_aes$jitter_stroke, size = point_aes$point_size, width = point_aes$jitter_width, alpha = point_aes$jitter_alpha),
        mean_layer
      )
    }

    plot <- Reduce(`+`, c(list(base_plot), layers))

    # compute label position
    # per-plot labels (just default species ordering)
    plot <- plot + coord_cartesian(clip = "off")

    if (save_plots) {
      fname <- paste0("07_nonoverlap_plot_", sanitize_filename(pair$variable), "_", sanitize_filename(pair$species_1), "_vs_", sanitize_filename(pair$species_2), ".pdf")
      save_plot(plot, file.path(output_dir, fname))
    }
  }

  invisible(non_overlapping_pairs)
}

# ----------------- Multivariate tests (beta-dispersion & PERMANOVA) -----------------
#' Run multivariate statistical tests
#'
#' Performs beta-dispersion and PERMANOVA analyses.
#'
#' @param df A cleaned data frame containing morphometric traits.
#' @param output_dir Directory where results will be written.
#' @param seed_disp Optional integer; if provided, sets the random seed
#'   immediately before beta-dispersion permutation tests to ensure reproducibility.
#' @param seed_perm Optional integer; if provided, sets the random seed
#'   immediately before PERMANOVA permutation tests to ensure reproducibility.
#'
#' @return A list containing multivariate test results.

multivariate_tests <- function(df, output_dir, seed_disp = NULL, seed_perm = NULL) {
  numeric_idx <- sapply(df, is.numeric)
  variables_df <- df[, numeric_idx, drop = FALSE]
  vars_ok <- sapply(variables_df, function(x) {
    !all(is.na(x)) && length(unique(na.omit(x))) > 1
  })
  variables_df <- variables_df[, vars_ok, drop = FALSE]

  if (ncol(variables_df) < 1) stop("No numeric variables available for multivariate analyses after filtering.")
  na_rows <- apply(variables_df, 1, function(x) any(is.na(x)))
  if (any(na_rows)) {
    warning(sum(na_rows), " rows removed due to NA values in multivariate variables.")
    variables_df <- variables_df[!na_rows, , drop = FALSE]
    df <- df[!na_rows, , drop = FALSE]
  }

  variables_matrix <- as.matrix(variables_df)
  n_groups <- length(unique(df$species))
  if (n_groups < 2) stop("Need at least 2 species/groups for multivariate tests.")
  if (nrow(variables_matrix) < 2) stop("Not enough rows for multivariate analyses.")

  dist_matrix <- tryCatch(vegan::vegdist(variables_matrix, method = "euclidean"), error = function(e) stop("vegdist failed: ", e$message))
  mod_disp <- tryCatch(vegan::betadisper(dist_matrix, df$species), error = function(e) stop("betadisper failed: ", e$message))

  # Beta-dispersion permutation test (with optional seed)
  permutest_disp <- tryCatch(
    {
      if (!is.null(seed_disp)) {
        withr::with_seed(seed_disp, vegan::permutest(mod_disp, pairwise = TRUE, permutations = 999))
      } else {
        vegan::permutest(mod_disp, pairwise = TRUE, permutations = 999)
      }
    },
    error = function(e) {
      warning("betadisper permutation test failed: ", e$message)
      NULL
    }
  )

  # Write pairwise betadisper tests
  if (!is.null(permutest_disp) && !is.null(permutest_disp$pairwise)) {
    write.csv(
      permutest_disp$pairwise,
      file = file.path(output_dir, "08_multi_betadisper_pairwise_tests.csv"),
      row.names = TRUE
    )
  }

  betadisper_p <- NA
  permanova_valid <- NA
  permanova_note <- NA

  if (!is.null(permutest_disp) && !is.null(permutest_disp$tab)) {
    overall_tab <- as.data.frame(permutest_disp$tab)
    overall_tab$Term <- rownames(overall_tab)
    rownames(overall_tab) <- NULL
    write.csv(overall_tab, file = file.path(output_dir, "08_multi_betadisper_overall_test.csv"), row.names = FALSE)
    p_col <- grep("Pr\\(|p", colnames(overall_tab), ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(p_col)) {
      betadisper_p <- overall_tab[[p_col]][1]
      permanova_valid <- betadisper_p >= 0.05
      permanova_note <- if (permanova_valid) "Beta-dispersion not significant; PERMANOVA likely reflects centroid differences." else "Beta-dispersion significant; PERMANOVA may be influenced by dispersion differences."
    }
  }

  # PERMANOVA (with optional seed)
  permanova_res <- tryCatch(
    {
      if (!is.null(seed_perm)) {
        withr::with_seed(seed_perm, vegan::adonis2(dist_matrix ~ species, data = df, permutations = 999))
      } else {
        vegan::adonis2(dist_matrix ~ species, data = df, permutations = 999)
      }
    },
    error = function(e) stop("PERMANOVA failed: ", e$message)
  )

  permanova_df <- as.data.frame(permanova_res)
  permanova_df$Betadisper_p_value <- betadisper_p
  permanova_df$PERMANOVA_valid <- permanova_valid
  permanova_df$Interpretation_note <- permanova_note
  write.csv(permanova_df, file = file.path(output_dir, "08_multi_permanova_species_effect.csv"), row.names = TRUE)

  if (!is.na(permanova_valid) && !permanova_valid) warning("PERMANOVA flagged: significant beta-dispersion detected.")

  list(betadisper = mod_disp, permutest = permutest_disp, permanova = permanova_res)
}

# ----------------- PCA -----------------
pca_analysis <- function(df, output_dir, custom_colors = NULL, species_to_encircle = character(0),
                         point_aes = NULL, mean_aes = NULL, label_aes = NULL, label_templates = NULL) {
  point_aes <- modifyList(list(point_size = 3.5), point_aes %||% list())
  label_aes <- modifyList(list(axis_text_size = 10, title_size = 12), label_aes %||% list())

  numeric_idx <- sapply(df, is.numeric)
  variables_df <- df[, numeric_idx, drop = FALSE]
  vars_ok <- sapply(variables_df, function(x) {
    !all(is.na(x)) && length(unique(na.omit(x))) > 1
  })
  variables_df <- variables_df[, vars_ok, drop = FALSE]

  if (ncol(variables_df) < 2) {
    warning("Less than 2 numeric variables available for PCA. Skipping PCA.")
    return(NULL)
  }

  pca_obj <- tryCatch(prcomp(as.matrix(variables_df), center = TRUE, scale. = TRUE), error = function(e) stop("PCA failed: ", e$message))
  pca_df <- as.data.frame(pca_obj$x)
  pca_df$species <- df$species
  explained_variance <- round(pca_obj$sdev^2 / sum(pca_obj$sdev^2) * 100, 2)

  # Top contributors to PC1 and PC2 (positional, Orangutan-style)
  loadings <- pca_obj$rotation
  top_n <- 10
  top_loading_list <- list()

  n_pcs <- min(2, ncol(loadings)) # only extract PCs that exist

  for (i in seq_len(n_pcs)) {
    loading_scores <- loadings[, i, drop = TRUE]

    ranked_vars <- sort(abs(loading_scores), decreasing = TRUE, na.last = NA)
    top_vars <- names(ranked_vars)[seq_len(min(top_n, length(ranked_vars)))]

    df_pc <- data.frame(
      Variable = top_vars,
      Loading = loading_scores[top_vars],
      Abs_Loading = abs(loading_scores[top_vars]),
      PC = paste0("PC", i),
      stringsAsFactors = FALSE
    )

    df_pc <- df_pc[order(df_pc$Abs_Loading, decreasing = TRUE), ]
    top_loading_list[[i]] <- df_pc
  }

  top_loadings_df <- dplyr::bind_rows(top_loading_list)

  out_file <- file.path(output_dir, "09_multi_pca_top_loadings_PC1_PC2.csv")
  write.csv(top_loadings_df, out_file, row.names = FALSE)

  # ---- Top-loadings bar plot ----
  # Sort within each PC by absolute loading so the longest bar is at the top
  top_loadings_df$Direction <- ifelse(
    top_loadings_df$Loading >= 0, "Positive", "Negative"
  )
  top_loadings_df$Variable <- factor(
    top_loadings_df$Variable,
    levels = rev(unique(
      top_loadings_df$Variable[order(
        top_loadings_df$PC,
        top_loadings_df$Abs_Loading
      )]
    ))
  )

  loading_colors <- c(Positive = "steelblue", Negative = "firebrick")

  loadings_plot <- ggplot(
    top_loadings_df,
    aes(
      x = Variable,
      y = Loading,
      fill = Direction
    )
  ) +
    ggplot2::geom_col(width = 0.4, color = "grey20", linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    ggplot2::geom_text(
      aes(
        label = sprintf("%.2f", Loading),
        hjust = ifelse(Loading > 0, -0.1, 1.1)
      ),
      size = label_aes$axis_text_size * 0.25,
      color = "grey20"
    ) +
    scale_fill_manual(
      values = loading_colors,
      name   = "Direction"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.18, 0.18))) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~PC, scales = "free_y", ncol = 2) +
    labs(
      x = NULL,
      y = "Loading"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = label_aes$axis_text_size * 0.8),
      axis.text.y = element_text(size = label_aes$axis_text_size * 0.85),
      axis.title.x = element_text(
        size = label_aes$axis_text_size * 0.9,
        margin = ggplot2::margin(t = 6)
      ),
      strip.text = element_text(
        size = label_aes$axis_text_size,
        face = "bold"
      ),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, linewidth = 1),
      legend.position = "bottom",
      legend.title = element_text(size = label_aes$axis_text_size * 0.9),
      legend.text = element_text(size = label_aes$axis_text_size * 0.85)
    )

  save_plot(
    loadings_plot,
    file.path(
      output_dir,
      "09_multi_pca_top_loadings_PC1_PC2_plot.pdf"
    ),
    width = 8.5,
    height = 3.5
  )

  # Encircle polygons for specified species
  hull_list <- list()
  if (length(species_to_encircle) > 0) {
    for (sp in species_to_encircle) {
      df_sp <- pca_df %>% filter(species == sp)
      if (nrow(df_sp) >= 3) {
        idx <- chull(df_sp$PC1, df_sp$PC2)
        hull <- df_sp[c(idx, idx[1]), , drop = FALSE]
        hull_list[[length(hull_list) + 1]] <- hull
      } else if (nrow(df_sp) > 0) {
        hull_list[[length(hull_list) + 1]] <- df_sp
      }
    }
  }
  pca_df_polygon <- if (length(hull_list) > 0) bind_rows(hull_list) else NULL

  if (!is.null(custom_colors)) pca_df$species <- factor(pca_df$species, levels = names(custom_colors))

  # label templates
  pca_x_tpl <- if (!is.null(label_templates) && !is.null(label_templates$pca_x)) label_templates$pca_x else "PC1 (%s%%)"
  pca_y_tpl <- if (!is.null(label_templates) && !is.null(label_templates$pca_y)) label_templates$pca_y else "PC2 (%s%%)"

  pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = species)) +
    {
      if (!is.null(pca_df_polygon)) geom_polygon(data = pca_df_polygon, aes(x = PC1, y = PC2, color = species), fill = "lightgrey", alpha = 0.2, linewidth = 0.5)
    } +
    geom_point(size = point_aes$point_size) +
    scale_color_manual(breaks = levels(pca_df$species), values = custom_colors) +
    labs(
      x = sprintf(pca_x_tpl, explained_variance[1]),
      y = sprintf(pca_y_tpl, ifelse(length(explained_variance) >= 2, explained_variance[2], 0))
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = label_aes$axis_text_size),
      axis.text.y = element_text(size = label_aes$axis_text_size),
      plot.title = element_text(hjust = 0.5, size = label_aes$title_size),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, linewidth = 1)
    )

  save_plot(pca_plot, file.path(output_dir, "09_multi_pca_plot.pdf"))
  list(pca = pca_obj, pca_df = pca_df, plot = pca_plot)
}

# ----------------- PCA post-hoc -----------------
extract_significant_tukey_pairs <- function(tukey_obj, alpha = 0.05) {
  if (is.null(tukey_obj) || nrow(tukey_obj) == 0) {
    return("None")
  }
  sig <- tukey_obj[tukey_obj[, "p adj"] < alpha, , drop = FALSE]
  if (nrow(sig) == 0) {
    return("None")
  }
  paste(rownames(sig), collapse = "; ")
}

extract_significant_dunn_pairs <- function(dunn_obj, alpha = 0.05) {
  if (is.null(dunn_obj)) {
    return("None")
  }
  pvals <- dunn_obj$P.adjusted
  comps <- dunn_obj$comparisons
  if (is.null(pvals) || is.null(comps)) {
    return("None")
  }
  sig <- comps[pvals < alpha]
  if (length(sig) == 0) {
    return("None")
  }
  paste(sig, collapse = "; ")
}

pca_posthoc_tests <- function(pca_obj, pca_df, output_dir) {
  if (is.null(pca_obj)) {
    return(NULL)
  }
  eig <- pca_obj$sdev^2
  cum_var <- cumsum(eig) / sum(eig)
  n_pca_90 <- which(cum_var >= 0.90)[1]
  if (is.na(n_pca_90)) n_pca_90 <- length(eig)
  if (length(pcs_to_test <- seq_len(n_pca_90)) == 0) {
    warning("No PCA axes meet variance threshold for post-hoc testing.")
    return(NULL)
  }

  pca_posthoc_table <- data.frame(PC = character(), Test = character(), Statistic = numeric(), DF = character(), P_value = numeric(), Significant_Pairs = character(), stringsAsFactors = FALSE)

  for (pc in pcs_to_test) {
    pc_name <- paste0("PC", pc)
    scores <- pca_df[[pc_name]]
    groups <- pca_df$species
    shapiro_p <- tryCatch(shapiro.test(scores)$p.value, error = function(e) NA)
    bartlett_p <- tryCatch(bartlett.test(scores ~ groups)$p.value, error = function(e) NA)

    parametric_ok <- !is.na(shapiro_p) && shapiro_p > 0.05 && !is.na(bartlett_p) && bartlett_p > 0.05

    if (parametric_ok) {
      fit <- aov(scores ~ groups)
      aov_sum <- summary(fit)[[1]]
      p_val <- aov_sum[["Pr(>F)"]][1]
      stat <- aov_sum[["F value"]][1]
      df <- paste(aov_sum[["Df"]][1], aov_sum[["Df"]][2], sep = ", ")
      sig_pairs <- "None"
      if (!is.na(p_val) && p_val < 0.05) {
        tuk <- TukeyHSD(fit)[[1]]
        sig_pairs <- extract_significant_tukey_pairs(tuk)
      }
      pca_posthoc_table <- rbind(pca_posthoc_table, data.frame(PC = pc_name, Test = "ANOVA + Tukey", Statistic = stat, DF = df, P_value = p_val, Significant_Pairs = sig_pairs, stringsAsFactors = FALSE))
    } else {
      kw <- kruskal.test(scores ~ groups)
      sig_pairs <- "None"
      if (!is.na(kw$p.value) && kw$p.value < 0.05) {
        dunn <- dunn.test::dunn.test(scores, groups, method = "bonferroni", kw = FALSE, label = TRUE)
        sig_pairs <- extract_significant_dunn_pairs(dunn)
      }
      pca_posthoc_table <- rbind(pca_posthoc_table, data.frame(PC = pc_name, Test = "Kruskal-Wallis + Dunn", Statistic = as.numeric(kw$statistic), DF = as.character(kw$parameter), P_value = kw$p.value, Significant_Pairs = sig_pairs, stringsAsFactors = FALSE))
    }
  }

  write.csv(pca_posthoc_table, file = file.path(output_dir, "09_multi_pca_posthoc.csv"), row.names = FALSE)
  pca_posthoc_table
}

# ----------------- DAPC -----------------
dapc_analysis <- function(df, output_dir, custom_colors = NULL, species_to_encircle = character(0),
                          point_aes = NULL, mean_aes = NULL, label_aes = NULL, label_templates = NULL, verbose = FALSE) {
  point_aes <- modifyList(list(point_size = 3.5), point_aes %||% list())
  label_aes <- modifyList(list(axis_text_size = 10, title_size = 12), label_aes %||% list())

  df$species <- factor(df$species, levels = sort(unique(as.character(df$species))))
  numeric_idx <- sapply(df, is.numeric)
  variables_df <- df[, numeric_idx, drop = FALSE]
  vars_ok <- sapply(variables_df, function(x) {
    !all(is.na(x)) && length(unique(na.omit(x))) > 1
  })
  variables_df <- variables_df[, vars_ok, drop = FALSE]
  variables_matrix <- as.matrix(variables_df)

  if (ncol(variables_matrix) < 1) stop("Not enough numeric variables for DAPC.")
  n_groups <- length(unique(df$species))
  max_n_pca <- min(ncol(variables_matrix), max(1, nrow(variables_matrix) - n_groups))

  # default: retain enough PCs to reach 90% variance if possible
  pca_obj <- tryCatch(prcomp(variables_matrix, center = TRUE, scale. = TRUE), error = function(e) NULL)
  if (!is.null(pca_obj)) {
    eig <- pca_obj$sdev^2
    cum_var <- cumsum(eig) / sum(eig)
    n_pca_90 <- which(cum_var >= 0.90)[1]
    if (is.na(n_pca_90)) n_pca_90 <- length(eig)
  } else {
    n_pca_90 <- min(ncol(variables_matrix), max(1, nrow(variables_matrix) - n_groups))
  }

  max_n_pca <- min(ncol(variables_matrix), max(1, nrow(variables_matrix) - n_groups))
  n_pca_90 <- min(n_pca_90, max_n_pca)
  if (n_pca_90 < 1) n_pca_90 <- 1

  if (verbose) message("Number of PCs retained to reach ~90% variance (bounded): ", n_pca_90)
  n_da_dynamic <- max(1, n_groups - 1)

  dapc_result <- tryCatch(
    {
      adegenet::dapc(variables_matrix, n.pca = n_pca_90, grp = df$species, n.da = n_da_dynamic)
    },
    error = function(e) {
      if (verbose) message("dapc failed with n.pca=", n_pca_90, ": ", e$message)
      for (k in seq(n_pca_90 - 1, 1, by = -1)) {
        res <- tryCatch(adegenet::dapc(variables_matrix, n.pca = k, grp = df$species, n.da = n_da_dynamic), error = function(e2) NULL)
        if (!is.null(res)) {
          if (verbose) message("dapc succeeded with n.pca=", k)
          return(res)
        }
      }
      stop("DAPC failed after retrying with fewer PCs.")
    }
  )

  LD_scores <- as.data.frame(dapc_result$ind.coord)
  LD_scores$species <- df$species
  eigenvalues <- dapc_result$eig
  if (length(eigenvalues) == 0) {
    var_explained <- c(NA, NA)
  } else if (length(eigenvalues) == 1) {
    var_explained <- c(1, 0)
  } else {
    var_explained <- eigenvalues[1:2] / sum(eigenvalues)
  }

  # polygons
  LD_scores_polygon_list <- list()
  if (length(species_to_encircle) > 0) {
    for (sp in species_to_encircle) {
      df_sp <- LD_scores %>% filter(species == sp)
      if (nrow(df_sp) >= 3 && all(c("LD1", "LD2") %in% colnames(df_sp))) {
        idx <- chull(df_sp$LD1, df_sp$LD2)
        LD_scores_polygon_list[[length(LD_scores_polygon_list) + 1]] <- df_sp[c(idx, idx[1]), , drop = FALSE]
      } else if (nrow(df_sp) > 0 && "LD1" %in% colnames(df_sp) && "LD2" %in% colnames(df_sp)) {
        LD_scores_polygon_list[[length(LD_scores_polygon_list) + 1]] <- df_sp
      }
    }
  }
  LD_scores_polygon <- if (length(LD_scores_polygon_list) > 0) bind_rows(LD_scores_polygon_list) else NULL
  if (!is.null(custom_colors)) LD_scores$species <- factor(LD_scores$species, levels = names(custom_colors))

  # label templates
  dapc_x_tpl <- if (!is.null(label_templates) && !is.null(label_templates$dapc_x)) label_templates$dapc_x else "LD1 (%s%%)"
  dapc_y_tpl <- if (!is.null(label_templates) && !is.null(label_templates$dapc_y)) label_templates$dapc_y else "LD2 (%s%%)"
  dapc_title_1d <- if (!is.null(label_templates) && !is.null(label_templates$dapc_title_1d)) label_templates$dapc_title_1d else "Discriminant Analysis (1 Axis)"

  if (n_da_dynamic >= 2 && "LD2" %in% colnames(LD_scores)) {
    dapc_plot <- ggplot(LD_scores, aes(x = LD1, y = LD2, color = species)) +
      {
        if (!is.null(LD_scores_polygon)) geom_polygon(data = LD_scores_polygon, aes(x = LD1, y = LD2, color = species), fill = "lightgrey", alpha = 0.2, linewidth = 0.5)
      } +
      geom_point(size = point_aes$point_size) +
      scale_color_manual(values = custom_colors) +
      labs(
        x = sprintf(dapc_x_tpl, round(var_explained[1] * 100, 2)),
        y = sprintf(dapc_y_tpl, round(ifelse(is.na(var_explained[2]), 0, var_explained[2]) * 100, 2))
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank(), panel.border = element_rect(color = "grey", fill = NA, linewidth = 1))
  } else {
    dapc_plot <- ggplot(LD_scores, aes(x = LD1, fill = species)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = custom_colors) +
      labs(title = dapc_title_1d, x = "LD1") +
      theme_minimal()
  }

  save_plot(dapc_plot, file.path(output_dir, "10_multi_dapc_plot.pdf"))
  predicted_groups <- as.factor(dapc_result$assign)
  conf_table <- table(Actual = df$species, Predicted = predicted_groups)
  write.csv(conf_table, file = file.path(output_dir, "11_multi_dapc_confusion_matrix.csv"), row.names = TRUE)

  compute_metrics <- function(conf_mat, species) {
    TP <- if (species %in% rownames(conf_mat) && species %in% colnames(conf_mat)) conf_mat[species, species] else 0
    FN <- sum(conf_mat[species, ]) - TP
    FP <- sum(conf_mat[, species]) - TP
    TN <- sum(conf_mat) - TP - FN - FP
    sensitivity <- if ((TP + FN) == 0) NA else TP / (TP + FN)
    specificity <- if ((TN + FP) == 0) NA else TN / (TN + FP)
    TSS <- if (is.na(sensitivity) || is.na(specificity)) NA else sensitivity + specificity - 1
    data.frame(Species = species, Sensitivity = round(sensitivity, 3), Specificity = round(specificity, 3), TSS = round(TSS, 3), stringsAsFactors = FALSE)
  }

  dapc_metrics <- bind_rows(lapply(rownames(conf_table), compute_metrics, conf_mat = conf_table))
  write.csv(dapc_metrics, file = file.path(output_dir, "11_multi_dapc_performance_metrics.csv"), row.names = FALSE)

  sample_ids <- rownames(df)
  if (is.null(sample_ids) || any(sample_ids == "")) sample_ids <- seq_len(nrow(df))
  misclassified <- data.frame(Sample = sample_ids, True_species = as.character(df$species), Predicted_species = as.character(predicted_groups), stringsAsFactors = FALSE) %>% filter(True_species != Predicted_species)
  write.csv(misclassified, file = file.path(output_dir, "11_multi_dapc_misclassified_individuals.csv"), row.names = FALSE)
  if (verbose) {
    if (nrow(misclassified) > 0) message("Misclassified samples detected: ", nrow(misclassified)) else message("No misclassified samples detected.")
  }
  list(dapc = dapc_result, LD_scores = LD_scores, confusion = conf_table, metrics = dapc_metrics, misclassified = misclassified)
}

# ----------------- Univariate tests (ANOVA / Tukey; Kruskal / Dunn) -----------------
generate_label_df_from_tukey <- function(tukey_mat, species_levels = NULL) {
  if (is.null(tukey_mat)) {
    return(NULL)
  }
  if (is.list(tukey_mat) && length(tukey_mat) > 0) tukey_mat <- tukey_mat[[1]]
  pcol <- grep("p adj", colnames(tukey_mat), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(pcol)) {
    return(NULL)
  }
  pvals <- tukey_mat[, pcol]
  comps <- rownames(tukey_mat)

  if (is.null(species_levels)) {
    groups <- trimws(unique(unlist(strsplit(comps, "-"))))
    pairwise_p <- matrix(1, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
    for (i in seq_along(comps)) {
      pair <- trimws(unlist(strsplit(comps[i], "\\s*-\\s*")))
      if (length(pair) == 2) {
        pairwise_p[pair[1], pair[2]] <- pvals[i]
        pairwise_p[pair[2], pair[1]] <- pvals[i]
      }
    }
  } else {
    groups <- species_levels
    pairwise_p <- matrix(1, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
    for (i in seq_along(comps)) {
      comp <- comps[i]
      matched <- FALSE
      for (sp1 in groups) {
        for (sp2 in groups) {
          if (sp1 == sp2) next
          if (paste(sp2, sp1, sep = "-") == comp || paste(sp1, sp2, sep = "-") == comp) {
            pairwise_p[sp1, sp2] <- pvals[i]
            pairwise_p[sp2, sp1] <- pvals[i]
            matched <- TRUE
            break
          }
        }
        if (matched) break
      }
    }
  }

  letters <- multcompView::multcompLetters(pairwise_p)$Letters
  data.frame(treatment = names(letters), Letters = as.character(letters), stringsAsFactors = FALSE)
}

univariate_analyses <- function(df, output_dir, custom_colors = NULL,
                                mean_aes = NULL, point_aes = NULL, violin_aes = NULL, box_aes = NULL,
                                label_aes = NULL, label_templates = NULL) {
  # Prepare aesthetics with defaults that match previous behavior
  mean_layer <- make_mean_layer(mean_aes)
  point_aes <- modifyList(list(point_size = 1.8, jitter_width = 0.1, jitter_alpha = 0.8, jitter_shape = 21, jitter_color = "black", jitter_stroke = 0.35), point_aes %||% list())
  violin_aes <- modifyList(list(alpha = 0.4), violin_aes %||% list())
  box_aes <- modifyList(list(alpha = 0.4, width = 0.15), box_aes %||% list())
  label_aes <- modifyList(list(text_size = 6, axis_text_size = 10, label_offset = 0.05), label_aes %||% list())
  univariate_theme <- univariate_theme_fn(label_aes)

  failed_anova_assumptions <- character(0)
  anova_results_table <- data.frame(Variable = character(), F_value = numeric(), DF_between = numeric(), DF_within = numeric(), P_value = numeric(), Shapiro_p = numeric(), Bartlett_p = numeric(), Assumptions_Met = character(), Significant_Pairs = character(), stringsAsFactors = FALSE)
  TUKEY <- list()

  check_anova_assumptions <- function(data, variable) {
    formula <- as.formula(paste(variable, "~ species"))
    anova_result <- tryCatch(aov(formula, data = data), error = function(e) NULL)
    if (is.null(anova_result)) {
      return(NULL)
    }

    resids <- residuals(anova_result)
    shapiro_p <- if (length(na.omit(resids)) >= 3) tryCatch(shapiro.test(resids)$p.value, error = function(e) NA) else NA
    group_sizes <- table(data$species)
    bartlett_p <- if (sum(group_sizes > 1) >= 2) tryCatch(bartlett.test(data[[variable]], data$species)$p.value, error = function(e) NA) else NA

    a_sum <- summary(anova_result)[[1]]
    f_statistic <- a_sum[["F value"]][1]
    df_between <- a_sum[["Df"]][1]
    df_within <- a_sum[["Df"]][2]
    p_value <- a_sum[["Pr(>F)"]][1]

    assumptions_met <- !is.na(shapiro_p) && shapiro_p > 0.05 && !is.na(bartlett_p) && bartlett_p > 0.05
    if (!assumptions_met) failed_anova_assumptions <<- unique(c(failed_anova_assumptions, variable))

    sig_pairs <- NA_character_
    if (assumptions_met && !is.na(p_value) && p_value < 0.05) {
      tuk <- tryCatch(TukeyHSD(anova_result, "species"), error = function(e) NULL)
      if (!is.null(tuk) && !is.null(tuk[["species"]])) {
        TUKEY[[variable]] <<- tuk[["species"]]
        pcol <- grep("p adj", colnames(tuk[["species"]]), ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(pcol)) {
          sig_pairs <- paste(rownames(tuk[["species"]])[tuk[["species"]][, pcol] < 0.05], collapse = "; ")
          if (nchar(sig_pairs) == 0) sig_pairs <- "None"
        }
      }
    }

    anova_results_table <<- rbind(anova_results_table, data.frame(Variable = variable, F_value = f_statistic, DF_between = df_between, DF_within = df_within, P_value = p_value, Shapiro_p = shapiro_p, Bartlett_p = bartlett_p, Assumptions_Met = ifelse(assumptions_met, "Yes", "No"), Significant_Pairs = sig_pairs, stringsAsFactors = FALSE))
  }

  variable_names <- names(df)[sapply(df, is.numeric)]
  for (variable in variable_names) check_anova_assumptions(df, variable)
  write.csv(anova_results_table, file = file.path(output_dir, "12_uni_anova_summary.csv"), row.names = FALSE)

  # Plot ANOVA-passing variables
  anova_variables <- setdiff(variable_names, failed_anova_assumptions)

  get_fixed_label_y <- function(plot, offset = label_aes$label_offset) {
    gb <- ggplot_build(plot)
    yr <- gb$layout$panel_params[[1]]$y.range
    yr[2] + diff(yr) * offset
  }

  for (variable in anova_variables) {
    if (!is.null(TUKEY[[variable]])) {
      LABELS <- generate_label_df_from_tukey(TUKEY[[variable]], species_levels = names(custom_colors))
      if (!is.null(LABELS)) LABELS$treatment <- factor(LABELS$treatment, levels = names(custom_colors))
    } else {
      LABELS <- data.frame(treatment = names(custom_colors), Letters = "a", stringsAsFactors = FALSE)
      LABELS$treatment <- factor(LABELS$treatment, levels = names(custom_colors))
    }

    is_mensural <- any((df[[variable]] %% 1) != 0, na.rm = TRUE) || grepl("_adj$", variable)

    if (is_mensural) {
      p <- ggplot(df, aes(x = species, y = .data[[variable]], fill = species)) +
        geom_violin(trim = FALSE, alpha = violin_aes$alpha, color = NA) +
        geom_boxplot(width = box_aes$width, outlier.shape = NA, color = "black", alpha = box_aes$alpha) +
        geom_jitter(aes(fill = species), shape = point_aes$jitter_shape, color = point_aes$jitter_color, stroke = point_aes$jitter_stroke, size = point_aes$point_size, width = point_aes$jitter_width, alpha = point_aes$jitter_alpha) +
        mean_layer +
        scale_fill_manual(values = custom_colors) +
        univariate_theme
    } else {
      p <- ggplot(df, aes(x = species, y = .data[[variable]], fill = species)) +
        geom_boxplot(width = box_aes$width, outlier.shape = NA, color = "black", alpha = box_aes$alpha) +
        geom_jitter(aes(fill = species), shape = point_aes$jitter_shape, color = point_aes$jitter_color, stroke = point_aes$jitter_stroke, size = point_aes$point_size, width = point_aes$jitter_width, alpha = point_aes$jitter_alpha) +
        mean_layer +
        scale_fill_manual(values = custom_colors) +
        univariate_theme
    }

    label_y <- get_fixed_label_y(p, offset = label_aes$label_offset)
    p <- p + geom_text(data = LABELS, inherit.aes = FALSE, aes(x = treatment, y = label_y, label = Letters), size = label_aes$text_size, color = "black") + coord_cartesian(clip = "off")

    save_plot(p, file.path(output_dir, paste0("12_uni_anova_plot_", sanitize_filename(variable), ".pdf")))
  }

  # Kruskal-Wallis + Dunn for failed ANOVA variables
  generate_dunn_label_df <- function(dunn_test, species_levels = NULL) {
    if (is.null(dunn_test)) {
      return(NULL)
    }
    comparisons <- dunn_test$comparisons
    pvals <- if (!is.null(dunn_test$P.adjusted)) dunn_test$P.adjusted else if (!is.null(dunn_test$P.adj)) dunn_test$P.adj else if (!is.null(dunn_test$Z)) p.adjust(2 * pnorm(-abs(dunn_test$Z)), method = "bonferroni") else return(NULL)

    if (is.null(species_levels)) {
      groups <- sort(unique(unlist(strsplit(comparisons, "\\s*-\\s*"))))
      pairwise_p <- matrix(1, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
      for (i in seq_along(comparisons)) {
        pair <- trimws(unlist(strsplit(comparisons[i], "\\s*-\\s*")))
        if (length(pair) == 2) pairwise_p[pair[1], pair[2]] <- pairwise_p[pair[2], pair[1]] <- pvals[i]
      }
    } else {
      groups <- species_levels
      pairwise_p <- matrix(1, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
      for (i in seq_along(comparisons)) {
        comp <- comparisons[i]
        matched <- FALSE
        for (sp1 in groups) {
          for (sp2 in groups) {
            if (sp1 == sp2) next
            cand1 <- paste(sp1, sp2, sep = " - ")
            cand2 <- paste(sp2, sp1, sep = " - ")
            cand3 <- paste(sp1, sp2, sep = "-")
            cand4 <- paste(sp2, sp1, sep = "-")
            if (comp %in% c(cand1, cand2, cand3, cand4)) {
              pairwise_p[sp1, sp2] <- pvals[i]
              pairwise_p[sp2, sp1] <- pvals[i]
              matched <- TRUE
              break
            }
          }
          if (matched) break
        }
        if (!matched) {
          pair <- trimws(unlist(strsplit(comp, " - ")))
          if (length(pair) == 2 && all(pair %in% groups)) {
            pairwise_p[pair[1], pair[2]] <- pvals[i]
            pairwise_p[pair[2], pair[1]] <- pvals[i]
          }
        }
      }
    }
    letters <- multcompView::multcompLetters(pairwise_p)$Letters
    data.frame(species = names(letters), Letters = letters, stringsAsFactors = FALSE)
  }

  extract_significant_dunn_pairs <- function(dunn_test, alpha = 0.05) {
    if (is.null(dunn_test)) {
      return(NA_character_)
    }
    pvals <- if (!is.null(dunn_test$P.adjusted)) dunn_test$P.adjusted else if (!is.null(dunn_test$P.adj)) dunn_test$P.adj else return(NA_character_)
    sig <- which(pvals < alpha)
    if (length(sig) == 0) {
      return("None")
    }
    paste(dunn_test$comparisons[sig], collapse = "; ")
  }

  summary_table <- data.frame(Variable = character(), Kruskal_p_value = numeric(), Kruskal_Chi_Squared = numeric(), Significant_Pairs = character(), stringsAsFactors = FALSE)
  dunn_labels_list <- list()

  for (var in failed_anova_assumptions) {
    kruskal_test <- tryCatch(kruskal.test(as.formula(paste(var, "~ species")), data = df), error = function(e) NULL)
    if (is.null(kruskal_test)) {
      summary_table <- rbind(summary_table, data.frame(Variable = var, Kruskal_p_value = NA, Kruskal_Chi_Squared = NA, Significant_Pairs = "Error", stringsAsFactors = FALSE))
      next
    }
    sig_pairs <- NA_character_
    if (kruskal_test$p.value < 0.05) {
      dunn_test <- tryCatch(dunn.test::dunn.test(df[[var]], df$species, method = "bonferroni", kw = FALSE, label = TRUE), error = function(e) NULL)
      if (!is.null(dunn_test)) {
        dunn_labels_list[[var]] <- generate_dunn_label_df(dunn_test, species_levels = names(custom_colors))
        sig_pairs <- extract_significant_dunn_pairs(dunn_test)
      }
    }
    summary_table <- rbind(summary_table, data.frame(Variable = var, Kruskal_p_value = kruskal_test$p.value, Kruskal_Chi_Squared = as.numeric(kruskal_test$statistic), Significant_Pairs = sig_pairs, stringsAsFactors = FALSE))
  }

  write.csv(summary_table, file = file.path(output_dir, "13_uni_kruskalwallis_summary.csv"), row.names = FALSE)

  # Plot KW variables
  for (var in failed_anova_assumptions) {
    dunn_labels <- dunn_labels_list[[var]]
    if (is.null(dunn_labels)) dunn_labels <- data.frame(species = names(custom_colors), Letters = "a", stringsAsFactors = FALSE)
    dunn_labels$species <- factor(dunn_labels$species, levels = names(custom_colors))

    is_mensural <- any((df[[var]] %% 1) != 0, na.rm = TRUE) || grepl("_adj$", var)
    if (is_mensural) {
      p <- ggplot(df, aes(x = species, y = .data[[var]], fill = species)) +
        geom_violin(trim = FALSE, alpha = violin_aes$alpha, color = NA) +
        geom_boxplot(width = box_aes$width, outlier.shape = NA, color = "black", alpha = box_aes$alpha) +
        geom_jitter(aes(fill = species), shape = point_aes$jitter_shape, color = point_aes$jitter_color, stroke = point_aes$jitter_stroke, size = point_aes$point_size, width = point_aes$jitter_width, alpha = point_aes$jitter_alpha) +
        mean_layer +
        scale_fill_manual(values = custom_colors) +
        univariate_theme
    } else {
      p <- ggplot(df, aes(x = species, y = .data[[var]], fill = species)) +
        geom_boxplot(width = box_aes$width, outlier.shape = NA, color = "black", alpha = box_aes$alpha) +
        geom_jitter(aes(fill = species), shape = point_aes$jitter_shape, color = point_aes$jitter_color, stroke = point_aes$jitter_stroke, size = point_aes$point_size, width = point_aes$jitter_width, alpha = point_aes$jitter_alpha) +
        mean_layer +
        scale_fill_manual(values = custom_colors) +
        univariate_theme
    }

    label_y <- get_fixed_label_y(p, offset = label_aes$label_offset)
    p <- p + geom_text(data = dunn_labels, inherit.aes = FALSE, aes(x = species, y = label_y, label = Letters), size = label_aes$text_size, color = "black") + coord_cartesian(clip = "off")

    save_plot(p, file.path(output_dir, paste0("13_uni_kruskalwallis_plot_", sanitize_filename(var), ".pdf")))
  }

  invisible(list(anova_table = anova_results_table, kruskal_table = summary_table))
}

# ----------------- Categorical variables analyses -----------------
#' Categorical Data Analysis
#'
#' Performs Chi-squared tests and generates proportional bar plots for categorical variables.
#'
#' @param data Data frame containing the variables
#' @param cat_vars Character vector of categorical variable names
#' @param output_dir Directory to save results
#' @param palette_name RColorBrewer palette name for categorical fills (default: "Paired")
#' @param custom_colors Optional named vector of colors for species (not used for categorical fills)
#' @return A data frame with Chi-squared test results
#' @keywords internal
categorical_analyses <- function(data, cat_vars, output_dir, palette_name = "Paired", custom_colors = NULL) {
  if (length(cat_vars) == 0) {
    return(NULL)
  }

  cat_results <- data.frame(
    Variable = character(),
    ChiSq_Statistic = numeric(),
    P_value = numeric(),
    Method = character(),
    Significant_Pairs = character(),
    Notes = character(),
    stringsAsFactors = FALSE
  )


  for (v in cat_vars) {
    # Frequency table for species vs category
    tbl <- table(data$species, data[[v]])

    # Run Chi-Square test (with simulated p-value to handle sparse counts robustly)
    test_res <- tryCatch(
      stats::chisq.test(tbl, simulate.p.value = TRUE, B = 2000),
      error = function(e) NULL
    )

    sig_pairs_str <- NA_character_

    if (!is.null(test_res)) {
      # Force pairwise post-hoc tests regardless of global p-value
      species_present <- unique(as.character(data$species))
      n_species <- length(species_present)
      total_n <- sum(tbl)
      notes_vec <- character(0)

      if (total_n < 50) notes_vec <- c(notes_vec, "Small total sample size (N < 50)")
      if (any(tbl < 5)) notes_vec <- c(notes_vec, "Low cell counts observed (< 5)")

      if (n_species > 1) {
        # Generate all unique pairs
        pairs <- utils::combn(species_present, 2, simplify = FALSE)
        pair_names <- vector("character", length(pairs))
        pair_pvals <- vector("numeric", length(pairs))

        for (i in seq_along(pairs)) {
          sp1 <- pairs[[i]][1]
          sp2 <- pairs[[i]][2]
          pair_names[i] <- paste0(sp1, "-", sp2)

          # Subset contingency table to just these two species
          sub_tbl <- tbl[c(sp1, sp2), , drop = FALSE]
          # Remove empty categories that might throw off test
          sub_tbl <- sub_tbl[, colSums(sub_tbl) > 0, drop = FALSE]

          if (ncol(sub_tbl) > 1) {
            pair_test <- tryCatch(
              stats::chisq.test(sub_tbl, simulate.p.value = TRUE, B = 2000),
              error = function(e) list(p.value = NA)
            )
            pair_pvals[i] <- pair_test$p.value
          } else {
            pair_pvals[i] <- NA # No variance between the species for this trait
          }
        }

        # Adjust p-values using FDR
        valid_idx <- !is.na(pair_pvals)
        pair_pvals_adj <- rep(NA, length(pair_pvals))
        if (sum(valid_idx) > 0) {
          pair_pvals_adj[valid_idx] <- stats::p.adjust(pair_pvals[valid_idx], method = "fdr")

          # Extract significant pairs
          sig_idx <- which(pair_pvals_adj < 0.05)
          if (length(sig_idx) > 0) {
            sig_pairs_str <- paste(pair_names[sig_idx], collapse = "; ")
          } else {
            sig_pairs_str <- "None"
          }
        }
      }

      cat_results <- rbind(cat_results, data.frame(
        Variable = v,
        ChiSq_Statistic = test_res$statistic,
        P_value = test_res$p.value,
        Method = test_res$method,
        Significant_Pairs = sig_pairs_str,
        Notes = if (length(notes_vec) > 0) paste(notes_vec, collapse = "; ") else "Reliable",
        stringsAsFactors = FALSE
      ))
    }

    # Calculate proportions and percentages for export
    prop_tbl <- prop.table(tbl, margin = 1)
    df_plot <- as.data.frame(prop_tbl)
    colnames(df_plot) <- c("Species", "Category", "Proportion")
    df_plot$Percentage <- df_plot$Proportion * 100
    df_plot$Variable <- v

    # Cumulative percentages for summary CSV
    cat_percentages <- if (exists("cat_percentages")) {
      rbind(cat_percentages, df_plot[, c("Variable", "Species", "Category", "Percentage")])
    } else {
      df_plot[, c("Variable", "Species", "Category", "Percentage")]
    }

    # Ensure categorical palette differs from the user's species palette
    # Reverting purely to Set3 (the very first pastel set used after plasma)
    cat_pal <- if (palette_name %in% c("Set3", "Pastel1")) "Pastel2" else "Set3"

    # Plot
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Species, y = Proportion, fill = Category)) +
      # Base layer: Solid light gray frame
      ggplot2::geom_bar(stat = "identity", position = "fill", fill = "gray98", color = "white", linewidth = 0.4, width = 0.8) +
      # Overlay layer: Actual Category colors with lower transparency
      ggplot2::geom_bar(stat = "identity", position = "fill", color = NA, alpha = 0.60, width = 0.8) +
      # Label layer
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(Percentage > 0, sprintf("%.1f", Percentage), "")),
        position = ggplot2::position_fill(vjust = 0.5),
        size = 3.0,
        color = "grey20"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::labs(
        title = paste("Proportional Distribution of", v, "by Species"),
        x = "Species",
        y = "Proportion"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "italic", size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 11, face = "bold"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted", linewidth = 0.3),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12),
        legend.title = ggplot2::element_text(face = "bold"),
        legend.position = "right",
        aspect.ratio = 1.5
      ) +
      ggplot2::scale_fill_brewer(name = v, palette = cat_pal)

    fname <- paste0("14_categorical_barplot_", sanitize_filename(v), ".pdf")
    save_plot(p, file.path(output_dir, fname))
  }

  if (nrow(cat_results) > 0) {
    write.csv(cat_results, file.path(output_dir, "14_categorical_analysis_summary.csv"), row.names = FALSE)
  }

  if (exists("cat_percentages") && nrow(cat_percentages) > 0) {
    write.csv(cat_percentages, file.path(output_dir, "14_categorical_percentages_summary.csv"), row.names = FALSE)
  }

  return(cat_results)
}

# Capture all arguments passed to run_orangutan()
capture_run_config <- function(args) {
  cfg <- lapply(args, function(x) {
    if (is.atomic(x) || is.character(x) || is.logical(x) || is.numeric(x)) {
      x
    } else {
      deparse(substitute(x))
    }
  })

  cfg$timestamp <- Sys.time()
  cfg$r_version <- R.version.string
  cfg$packages <- names(sessionInfo()$otherPkgs)

  c(
    "# Orangutan run configuration",
    "# Use dget() to reload this as an R object",
    capture.output(dput(cfg))
  )
}

# ----------------- High-level wrapper -----------------
#' Run Orangutan
#'
#' Runs the full Orangutan morphometric analysis pipeline.
#'
#' @param data_path Path to input CSV file
#' @param output_dir Output directory for results
#' @param apply_allometry Logical; apply allometric correction
#' @param allometry_var Character; size variable for allometry
#' @param remove_outliers Logical; remove outliers
#' @param outlier_vars Variables used for outlier detection
#' @param outlier_tail_pct Tail proportion for Tukey filtering
#' @param species_to_encircle Species to encircle in multivariate plots
#' @param palette_name RColorBrewer palette name
#' @param custom_colors Optional named vector of hex colors for species (e.g., \code{c(A = "#FF0000")})
#' @param point_aes List of point aesthetics
#' @param mean_aes List of mean-point aesthetics
#' @param violin_aes List of violin aesthetics
#' @param box_aes List of boxplot aesthetics
#' @param label_aes List of label/text aesthetics
#' @param label_templates Optional plot label templates
#' @param seeds A named list of integer seeds for reproducibility, with elements:
#'   \code{betadisper} for beta-dispersion permutation tests and
#'   \code{permanova} for PERMANOVA permutation tests.
#'   Defaults to \code{list(betadisper = 123, permanova = 456)}.
#' @param verbose Logical; if TRUE, print progress messages. Defaults to FALSE.
#' @return A list containing results from all analyses
#'
#' @examples
#' \donttest{
#' # Create a tiny example dataset in a temporary file
#' tmp <- tempfile(fileext = ".csv")
#' toy_data <- data.frame(
#'   species = c("A", "A", "B", "B", "C", "C"),
#'   trait1  = c(1, 2, 5, 6, 9, 10),
#'   trait2  = c(3, 4, 7, 8, 11, 12),
#'   trait3  = c(2, 3, 6, 7, 10, 11)
#' )
#' write.csv(toy_data, tmp, row.names = FALSE)
#'
#' # Create a temporary output directory
#' out_dir <- tempdir()
#'
#' # Set a named list of seeds for reproducibility
#' seeds <- list(betadisper = 123, permanova = 456)
#'
#' # Run Orangutan on the toy dataset
#' res <- run_orangutan(
#'   data_path = tmp,
#'   output_dir = out_dir,
#'   seeds = seeds,
#'   verbose = FALSE
#' )
#'
#' # Inspect returned object
#' str(res)
#'
#' # Clean up temporary dataset file
#' unlink(tmp)
#' }
#' @export

run_orangutan <- function(data_path,
                          output_dir = file.path(dirname(data_path), paste0("orangutan_outputs_", format(Sys.time(), "%Y%m%d_%H%M%S"))),
                          apply_allometry = FALSE,
                          allometry_var = NULL,
                          remove_outliers = FALSE,
                          outlier_vars = NULL,
                          outlier_tail_pct = 0.05,
                          palette_name = "Paired",
                          custom_colors = NULL,
                          species_to_encircle = character(0),
                          seeds = list(betadisper = 123, permanova = 456),
                          # plotting customization:
                          point_aes = list(
                            point_size = 3.5, jitter_width = 0.1, jitter_alpha = 0.8,
                            jitter_shape = 21, jitter_color = "black", jitter_stroke = 0.35
                          ),
                          mean_aes = list(
                            size = 1.8, shape = 21, fill = "white",
                            color = "black", stroke = 0.6
                          ),
                          violin_aes = list(alpha = 0.4),
                          box_aes = list(alpha = 0.4, width = 0.15),
                          label_aes = list(
                            text_size = 6, axis_text_size = 10,
                            title_size = 12, label_offset = 0.05
                          ),
                          label_templates = NULL,
                          verbose = FALSE) {
  ## ---- Seed validation ----
  if (!is.null(seeds)) {
    if (!is.list(seeds)) {
      stop("`seeds` must be a named list or NULL.")
    }

    allowed <- c("betadisper", "permanova")
    bad <- setdiff(names(seeds), allowed)
    if (length(bad) > 0) {
      stop("Invalid seed names: ", paste(bad, collapse = ", "))
    }

    for (nm in names(seeds)) {
      if (!is.null(seeds[[nm]]) &&
        (!is.numeric(seeds[[nm]]) || length(seeds[[nm]]) != 1)) {
        stop("Seed '", nm, "' must be a single integer or NULL.")
      }
      seeds[[nm]] <- as.integer(seeds[[nm]])
    }
  }

  ## ---- capture arguments EXACTLY as passed ----
  args <- as.list(match.call(expand.dots = TRUE))[-1]

  ## ---- output directory ----
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ## ---- capture run configuration ----
  config_text <- capture_run_config(args)

  ## ---- find categorical variables early ----
  all_data <- load_data(data_path)
  # Variables that are character, logical, or factor, excluding 'species'
  cat_vars <- setdiff(
    names(all_data)[sapply(all_data, function(x) is.character(x) || is.factor(x) || is.logical(x))],
    "species"
  )
  num_only_vars <- setdiff(names(all_data), c(cat_vars, "species"))
  has_numerical <- length(num_only_vars) > 0

  ## ---- write human-readable methods summary ----
  writeLines(
    c(
      "Orangutan settings:",
      "",
      config_text,
      "",
      paste("- Input data file:", normalizePath(data_path)),
      paste("- Allometry applied:", apply_allometry),
      paste(
        "- Allometry variable:",
        ifelse(is.null(allometry_var), "None", allometry_var)
      ),
      paste("- Outlier removal:", remove_outliers),
      paste(
        "- Outlier variables:",
        ifelse(is.null(outlier_vars), "None",
          paste(outlier_vars, collapse = ", ")
        )
      ),
      paste("- Outlier tail proportion:", outlier_tail_pct),
      paste(
        "- Numerical variables detected:",
        ifelse(has_numerical, paste(num_only_vars, collapse = ", "), "None. Numerical analyses bypassed.")
      ),
      paste(
        "- Categorical variables detected:",
        ifelse(length(cat_vars) == 0, "None",
          paste(cat_vars, collapse = ", ")
        )
      ),
      paste("- Color palette:", palette_name),
      paste(
        "- Custom colors applied:",
        ifelse(is.null(custom_colors), "None",
          paste(names(custom_colors), collapse = ", ")
        )
      ),
      paste(
        "- Species encircled (PCA/DAPC):",
        ifelse(length(species_to_encircle) == 0, "None",
          paste(species_to_encircle, collapse = ", ")
        )
      ),
      paste("- PERMANOVA permutations: 999"),
      paste("- Beta-dispersion seed:", ifelse(is.null(seeds$betadisper), "None", seeds$betadisper)),
      paste("- PERMANOVA seed:", ifelse(is.null(seeds$permanova), "None", seeds$permanova)),
      paste("- Run timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("- R version:", R.version.string),
      "",
      "Cite as:",
      "Torres, J. (2026). Orangutan: An R Package for Analyzing and Visualizing Phenotypic Data in the Context of Species Descriptions and Population Comparisons. Ecology and Evolution, 16(2), e73111. https://doi.org/10.1002/ece3.73111"
    ),
    file.path(output_dir, "00_methods_summary.txt")
  )

  ## ---- pipeline starts here ----
  data <- all_data

  basic <- basic_clean(data)
  data_clean <- basic$data
  species_levels <- basic$species_levels
  user_colors <- custom_colors

  # Init results
  num_data_clean <- NULL
  removed_samples <- NULL
  mv_res <- NULL
  pca_res <- NULL
  dapc_res <- NULL

  custom_colors <- build_palette(species_levels, palette_name = palette_name, custom_colors = user_colors)
  if (length(custom_colors) < length(species_levels)) {
    custom_colors <- setNames(
      rep(unname(custom_colors),
        length.out = length(species_levels)
      ),
      species_levels
    )
  } else {
    custom_colors <- custom_colors[species_levels]
  }

  if (has_numerical) {
    num_vars <- c("species", num_only_vars)
    num_data_clean <- data_clean[, num_vars, drop = FALSE]

    ## ---- allometry ----
    if (apply_allometry) {
      if (is.null(allometry_var)) {
        stop("apply_allometry is TRUE but no allometry_var provided.")
      }
      if (verbose) message("Applying pooled allometric transformation using: ", allometry_var)
      num_data_clean <- apply_allometric_transformation(num_data_clean, allometry_var)
    }

    ## ---- outlier removal ----
    removed_samples <- NULL
    if (remove_outliers) {
      if (is.null(outlier_vars) || length(outlier_vars) == 0) {
        stop("remove_outliers TRUE but no outlier_vars provided.")
      }

      out_res <- remove_outliers_by_species(
        num_data_clean, outlier_vars,
        tail_pct = outlier_tail_pct, verbose = verbose
      )

      num_data_clean <- out_res$data
      removed_samples <- out_res$removed

      if (nrow(removed_samples) > 0) {
        write.csv(num_data_clean,
          file.path(output_dir, "05_data_cleaned_outliers_removed.csv"),
          row.names = FALSE
        )
        write.csv(removed_samples,
          file.path(output_dir, "05_qc_outlier_audit_log.csv"),
          row.names = FALSE
        )

        # Make sure the full `data_clean` also drops these rows (matched by row index if needed, or we just rely on filtering morphometrics only)
        # Simpler: If a row is dropped from morphometrics, we should drop it globally to stay aligned.
        # To do this safely assuming basic_clean sorts row order statically
        # A robust way is grabbing rownames:
        kept_rows <- rownames(num_data_clean)
        data_clean <- data_clean[kept_rows, , drop = FALSE]

        species_levels <- sort(unique(as.character(num_data_clean$species)))
        num_data_clean$species <- factor(num_data_clean$species, levels = species_levels)
        data_clean$species <- factor(data_clean$species, levels = species_levels)
        custom_colors <- build_palette(species_levels,
          palette_name = palette_name,
          custom_colors = user_colors
        )
        if (length(custom_colors) < length(species_levels)) {
          custom_colors <- setNames(
            rep(unname(custom_colors),
              length.out = length(species_levels)
            ),
            species_levels
          )
        } else {
          custom_colors <- custom_colors[species_levels]
        }
      } else {
        if (verbose) message("No outliers removed.")
      }
    }

    ## ---- trait classification ----
    classify_traits(num_data_clean)

    ## ---- summaries ----
    compute_summary_stats(num_data_clean, output_dir)

    ## ---- non-overlaps ----
    find_and_plot_nonoverlaps(
      num_data_clean, output_dir, custom_colors,
      mean_aes = mean_aes, point_aes = point_aes,
      violin_aes = violin_aes, box_aes = box_aes,
      label_aes = label_aes, label_templates = label_templates,
      save_plots = TRUE,
      verbose = verbose
    )

    ## ---- multivariate ----
    mv_res <- tryCatch(
      multivariate_tests(
        num_data_clean, output_dir,
        seed_disp = seeds$betadisper,
        seed_perm = seeds$permanova
      ),
      error = function(e) {
        warning("Multivariate tests failed: ", e$message)
        NULL
      }
    )

    ## ---- PCA ----
    pca_res <- tryCatch(
      pca_analysis(
        num_data_clean, output_dir,
        custom_colors = custom_colors,
        species_to_encircle = species_to_encircle,
        point_aes = point_aes,
        mean_aes = mean_aes,
        label_aes = label_aes,
        label_templates = label_templates
      ),
      error = function(e) {
        warning("PCA failed: ", e$message)
        NULL
      }
    )

    if (!is.null(pca_res)) {
      tryCatch(
        pca_posthoc_tests(pca_res$pca, pca_res$pca_df, output_dir),
        error = function(e) warning("PCA post-hoc failed: ", e$message)
      )
    }

    ## ---- DAPC ----
    dapc_res <- tryCatch(
      dapc_analysis(
        num_data_clean, output_dir,
        custom_colors = custom_colors,
        species_to_encircle = species_to_encircle,
        point_aes = point_aes,
        mean_aes = mean_aes,
        label_aes = label_aes,
        label_templates = label_templates,
        verbose = verbose
      ),
      error = function(e) {
        warning("DAPC failed: ", e$message)
        NULL
      }
    )

    ## ---- univariate ----
    tryCatch(
      univariate_analyses(
        num_data_clean, output_dir,
        custom_colors = custom_colors,
        mean_aes = mean_aes, point_aes = point_aes,
        violin_aes = violin_aes, box_aes = box_aes,
        label_aes = label_aes,
        label_templates = label_templates
      ),
      error = function(e) warning("Univariate analyses failed: ", e$message)
    )
  }

  ## ---- categorical analysis ----
  cat_res <- NULL
  if (length(cat_vars) > 0) {
    if (verbose) message("Running Categorical Analyses on variables: ", paste(cat_vars, collapse = ", "))
    cat_res <- tryCatch(
      categorical_analyses(
        data_clean, cat_vars, output_dir,
        palette_name = palette_name,
        custom_colors = custom_colors
      ),
      error = function(e) {
        warning("Categorical analyses failed: ", e$message)
        NULL
      }
    )
  }

  if (verbose) message("\nGenerating HTML Interpretation Report...")
  tryCatch(
    generate_html_report(output_dir),
    error = function(e) warning("Failed to generate HTML report: ", e$message)
  )

  if (verbose) message("\nOrangutan run completed.\nAll output files are saved in:\n", output_dir, "\n")

  invisible(list(
    data = data_clean,
    num_data = num_data_clean,
    removed_samples = removed_samples,
    multivariate = mv_res,
    pca = pca_res,
    dapc = dapc_res,
    categorical = cat_res
  ))
}

# End of script
