#' Generate HTML interpretation report
#'
#' @param output_dir Directory where output files are saved.
#' @keywords internal
generate_html_report <- function(output_dir) {
  report_file <- file.path(output_dir, "orangutan_report.html")

  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'>",
    "<title>Orangutan Analysis Report</title>",
    "<style>",
    "body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; line-height: 1.6; color: #333; max-width: 1100px; margin: 0 auto; padding: 20px; }",
    "h1 { color: #2c3e50; border-bottom: 2px solid #eee; padding-bottom: 10px; }",
    "h2 { color: #34495e; margin-top: 30px; }",
    "h3 { color: #7f8c8d; }",
    ".section { background: #f9f9f9; padding: 20px; border-radius: 5px; margin-bottom: 30px; }",
    ".plot-container { text-align: center; margin: 20px 0; }",
    ".plot-container img { max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; padding: 5px; }",
    ".interpretation { background: #e8f4f8; padding: 15px; border-left: 5px solid #2980b9; margin-bottom: 20px; }",
    ".warning { background: #fdf5e6; padding: 15px; border-left: 5px solid #e67e22; margin-bottom: 20px; }",
    "table { border-collapse: collapse; width: 100%; margin: 15px 0; font-size: 0.88em; overflow-x: auto; display: block; }",
    "th { background: #34495e; color: #fff; padding: 8px 10px; text-align: left; white-space: nowrap; }",
    "td { padding: 6px 10px; border-bottom: 1px solid #ddd; white-space: nowrap; }",
    "tr:nth-child(even) { background: #f2f2f2; }",
    "tr:hover { background: #ddeeff; }",
    "</style>",
    "</head>",
    "<body>",
    "<h1>Orangutan Phenotypic Analysis Report</h1>",
    paste0("<p>Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>")
  )

  # ---- helpers ----
  read_csv_safe <- function(filename) {
    path <- file.path(output_dir, filename)
    if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE) else NULL
  }

  get_png_path <- function(pdf_filename) {
    paste0(file.path("pngs", sub("\\.pdf$", ".png", pdf_filename, ignore.case = TRUE)), "?v=", as.numeric(Sys.time()))
  }

  # Render a data.frame as an HTML table string (vector of lines)
  df_to_html_table <- function(df, digits = 4) {
    if (is.null(df) || nrow(df) == 0) return(character(0))
    fmt_val <- function(x) {
      if (is.numeric(x)) {
        ifelse(is.na(x), "NA", formatC(signif(x, digits), format = "g", flag = "#"))
      } else {
        ifelse(is.na(x), "NA", as.character(x))
      }
    }
    header <- paste0("<th>", colnames(df), "</th>", collapse = "")
    rows <- apply(df, 1, function(r) {
      cells <- vapply(seq_along(r), function(j) {
        raw <- r[[j]]
        col <- df[[j]]
        val <- if (is.numeric(col)) fmt_val(suppressWarnings(as.numeric(raw))) else ifelse(is.na(raw), "NA", raw)
        paste0("<td>", val, "</td>")
      }, character(1))
      paste0("<tr>", paste(cells, collapse = ""), "</tr>")
    })
    c("<div style='overflow-x:auto;'>",
      "<table>",
      paste0("<thead><tr>", header, "</tr></thead>"),
      "<tbody>",
      rows,
      "</tbody></table>",
      "</div>")
  }

  # ---- 1. Summary Statistics ----
  summary_stats <- read_csv_safe("06_summary_stats.csv")
  if (!is.null(summary_stats)) {
    html <- c(html, "<div class='section'>", "<h2>1. Summary Statistics</h2>",
              "<div class='interpretation'>Means &plusmn; standard deviations (min&ndash;max) for the analyzed numeric traits, broken down by species.</div>",
              df_to_html_table(summary_stats),
              "</div>")
  }

  # ---- 2. Non-overlapping Traits ----
  nonoverlaps <- read_csv_safe("07_nonoverlaps_list.csv")
  if (!is.null(nonoverlaps) && nrow(nonoverlaps) > 0) {
    html <- c(html, "<div class='section'>", "<h2>2. Diagnostic Non-overlapping Traits</h2>")
    interp <- paste0("Found <strong>", nrow(nonoverlaps), "</strong> non-overlapping trait pairs. These are strictly diagnostic ranges &mdash; an individual's measurement can perfectly identify it between the two given species without overlap.")
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))
    html <- c(html, df_to_html_table(nonoverlaps))

    for (i in seq_len(nrow(nonoverlaps))) {
      pair     <- nonoverlaps[i, ]
      san_var  <- gsub("[^A-Za-z0-9._-]", "_", gsub("\\s+", "_", pair$variable))
      san_sp1  <- gsub("[^A-Za-z0-9._-]", "_", gsub("\\s+", "_", pair$species_1))
      san_sp2  <- gsub("[^A-Za-z0-9._-]", "_", gsub("\\s+", "_", pair$species_2))
      pdf_name <- paste0("07_nonoverlap_plot_", san_var, "_", san_sp1, "_vs_", san_sp2, ".pdf")
      html <- c(html, paste0("<div style='display:inline-block; width:45%; margin:2%; vertical-align: top;'>"))
      html <- c(html, paste0("<h3>", pair$variable, "</h3>"))
      html <- c(html, paste0("<p><strong>", pair$species_1, "</strong> vs <strong>", pair$species_2, "</strong></p>"))
      html <- c(html, paste0("<div class='plot-container'><img src='", get_png_path(pdf_name), "' alt='Non-overlap plot'></div></div>"))
    }
    html <- c(html, "</div>")
  }

  # ---- 3. Multivariate PERMANOVA ----
  permanova <- read_csv_safe("08_multi_permanova_species_effect.csv")
  if (!is.null(permanova)) {
    html <- c(html, "<div class='section'>", "<h2>3. Multivariate PERMANOVA</h2>")
    p_col <- grep("Pr\\(|p", colnames(permanova), ignore.case = TRUE)[1]
    p_val <- permanova[1, p_col]
    if (!is.na(p_val) && p_val < 0.05) {
      interp <- "<strong>PERMANOVA is significant (p &lt; 0.05)</strong>: There is strong statistical evidence that overall morphospace globally differs among the species groups."
    } else {
      interp <- "<strong>PERMANOVA is not significant (p &ge; 0.05)</strong>: No strong global evidence for morphological divergence among the species groups."
    }
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))

    beta_valid <- permanova$PERMANOVA_valid[1]
    if (!is.na(beta_valid) && !beta_valid) {
      html <- c(html, "<div class='warning'><strong>Warning:</strong> Beta-dispersion is significant. The PERMANOVA result might be confounded by groups having structurally different amounts of morphological variation (dispersion).</div>")
    }

    html <- c(html, "<h3>PERMANOVA table</h3>", df_to_html_table(permanova))

    betadisper_tbl <- read_csv_safe("08_multi_betadisper_overall_test.csv")
    if (!is.null(betadisper_tbl)) {
      html <- c(html, "<h3>Beta-dispersion test</h3>", df_to_html_table(betadisper_tbl))
    }
    html <- c(html, "</div>")
  }

  # ---- 4. PCA ----
  pca_posthoc <- read_csv_safe("09_multi_pca_posthoc.csv")
  if (!is.null(pca_posthoc)) {
    html <- c(html, "<div class='section'>", "<h2>4. Principal Component Analysis (PCA)</h2>")
    sig_axs <- pca_posthoc[!is.na(pca_posthoc$P_value) & pca_posthoc$P_value < 0.05, "PC"]
    if (length(sig_axs) > 0) {
      interp <- paste0("PCA axes that significantly separate species (using post-hoc tests): <strong>", paste(sig_axs, collapse = ", "), "</strong>.")
    } else {
      interp <- "No PCA axes significantly separated the species groups."
    }
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))
    html <- c(html, paste0("<div class='plot-container'><img src='", get_png_path("09_multi_pca_plot.pdf"), "' alt='PCA Plot' style='max-width:600px;'></div>"))

    html <- c(html, "<h3>Top variable loadings (PC1 &amp; PC2)</h3>")
    html <- c(html, "<div class='interpretation'>The chart below shows the variables that contribute most to PC1 and PC2. Bar direction indicates the sign of the loading; absolute length reflects contribution magnitude.</div>")
    html <- c(html, paste0("<div class='plot-container'><img src='", get_png_path("09_multi_pca_top_loadings_PC1_PC2_plot.pdf"), "' alt='PCA Top Loadings Bar Chart' style='max-width:800px;'></div>"))

    html <- c(html, "<h3>Post-hoc tests per PC axis</h3>", df_to_html_table(pca_posthoc))

    pca_loadings <- read_csv_safe("09_multi_pca_top_loadings_PC1_PC2.csv")
    if (!is.null(pca_loadings)) {
      html <- c(html, "<h3>Top loadings data table</h3>", df_to_html_table(pca_loadings))
    }
    html <- c(html, "</div>")
  }

  # ---- 5. DAPC ----
  dapc_metrics <- read_csv_safe("11_multi_dapc_performance_metrics.csv")
  if (!is.null(dapc_metrics)) {
    html <- c(html, "<div class='section'>", "<h2>5. Discriminant Analysis of Principal Components (DAPC)</h2>")

    mean_sens <- mean(dapc_metrics$Sensitivity, na.rm = TRUE)
    mean_spec <- mean(dapc_metrics$Specificity, na.rm = TRUE)
    interp <- paste0("DAPC constructs axes that specifically maximize discrimination between species. The model accurately assigned species with an average sensitivity of <strong>", round(mean_sens * 100, 1), "%</strong> and specificity of <strong>", round(mean_spec * 100, 1), "%</strong>. ")

    misclass <- read_csv_safe("11_multi_dapc_misclassified_individuals.csv")
    if (!is.null(misclass) && nrow(misclass) > 0) {
      interp <- paste0(interp, "However, there were <strong>", nrow(misclass), " misclassified</strong> individuals.")
    } else {
      interp <- paste0(interp, "<strong>All individuals were correctly classified</strong> by the model.")
    }
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))
    html <- c(html, paste0("<div class='plot-container'><img src='", get_png_path("10_multi_dapc_plot.pdf"), "' alt='DAPC Plot' style='max-width:600px;'></div>"))

    html <- c(html, "<h3>Per-species performance metrics</h3>", df_to_html_table(dapc_metrics))

    if (!is.null(misclass) && nrow(misclass) > 0) {
      html <- c(html, "<h3>Misclassified individuals</h3>", df_to_html_table(misclass))
    }

    conf_tbl <- read_csv_safe("11_multi_dapc_confusion_matrix.csv")
    if (!is.null(conf_tbl)) {
      html <- c(html, "<h3>Confusion matrix</h3>", df_to_html_table(conf_tbl))
    }
    html <- c(html, "</div>")
  }

  # ---- 6. Univariate Analyses (Parametric) ----
  anova_res <- read_csv_safe("12_uni_anova_summary.csv")
  if (!is.null(anova_res) && nrow(anova_res) > 0) {
    html <- c(html, "<div class='section'>", "<h2>6. Univariate Analyses (Parametric)</h2>")
    sig_vars <- anova_res[!is.na(anova_res$P_value) & anova_res$P_value < 0.05 & anova_res$Assumptions_Met == "Yes", "Variable"]
    if (length(sig_vars) > 0) {
      interp <- paste0("The following variables met normality/variance assumptions and show <strong>significant parametric differences</strong> (ANOVA p &lt; 0.05) among species: <strong>", paste(sig_vars, collapse = ", "), "</strong>. Pairwise differences are indicated by the lowercase letters on the plots (boxes sharing a letter do not significantly differ).")
    } else {
      interp <- "No variables showed significant parametric differences between species, or variables failed ANOVA assumptions."
    }
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))
    html <- c(html, "<h3>ANOVA summary table</h3>", df_to_html_table(anova_res))

    for (v in sig_vars) {
      san_v    <- gsub("[^A-Za-z0-9._-]", "_", gsub("\\s+", "_", v))
      png_path <- get_png_path(paste0("12_uni_anova_plot_", san_v, ".pdf"))
      html <- c(html, paste0("<div class='plot-container' style='display:inline-block; width:45%; margin:2%; vertical-align: top;'>"))
      html <- c(html, paste0("<h4>", v, "</h4>"))
      html <- c(html, paste0("<img src='", png_path, "' alt='ANOVA ", v, "'></div>"))
    }
    html <- c(html, "</div>")
  }

  # ---- 7. Univariate Analyses (Non-Parametric) ----
  kw_res <- read_csv_safe("13_uni_kruskalwallis_summary.csv")
  if (!is.null(kw_res) && nrow(kw_res) > 0) {
    html <- c(html, "<div class='section'>", "<h2>7. Univariate Analyses (Non-Parametric)</h2>")
    sig_vars <- kw_res[!is.na(kw_res$Kruskal_p_value) & kw_res$Kruskal_p_value < 0.05, "Variable"]
    if (length(sig_vars) > 0) {
      interp <- paste0("The following variables did not meet ANOVA assumptions, but show <strong>significant non-parametric differences</strong> (Kruskal-Wallis p &lt; 0.05): <strong>", paste(sig_vars, collapse = ", "), "</strong>. Pairwise differences are indicated by lowercase letters.")
    } else {
      interp <- "No variables analyzed non-parametrically showed significant differences."
    }
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))
    html <- c(html, "<h3>Kruskal-Wallis summary table</h3>", df_to_html_table(kw_res))

    for (v in sig_vars) {
      san_v    <- gsub("[^A-Za-z0-9._-]", "_", gsub("\\s+", "_", v))
      png_path <- get_png_path(paste0("13_uni_kruskalwallis_plot_", san_v, ".pdf"))
      html <- c(html, paste0("<div class='plot-container' style='display:inline-block; width:45%; margin:2%; vertical-align: top;'>"))
      html <- c(html, paste0("<h4>", v, "</h4>"))
      html <- c(html, paste0("<img src='", png_path, "' alt='Kruskal ", v, "'></div>"))
    }
    html <- c(html, "</div>")
  }

  # ---- 8. Categorical Variable Analyses ----
  cat_res <- read_csv_safe("14_categorical_analysis_summary.csv")
  if (!is.null(cat_res) && nrow(cat_res) > 0) {
    html <- c(html, "<div class='section'>", "<h2>8. Categorical Variable Analyses</h2>")

    sig_cat <- cat_res[!is.na(cat_res$P_value) & cat_res$P_value < 0.05, "Variable"]
    if (length(sig_cat) > 0) {
      interp <- paste0("The following categorical variables show <strong>significant differences</strong> in distribution among species (Chi-squared p &lt; 0.05): <strong>",
                       paste(sig_cat, collapse = ", "), "</strong>.")
    } else {
      interp <- "No categorical variables showed significant distributional differences among species (Chi-squared p &ge; 0.05 for all)."
    }
    html <- c(html, paste0("<div class='interpretation'>", interp, "</div>"))
    html <- c(html, "<h3>Chi-squared summary table</h3>", df_to_html_table(cat_res))

    for (i in seq_len(nrow(cat_res))) {
      row           <- cat_res[i, ]
      v             <- row$Variable
      p             <- if (!is.na(row$P_value)) round(row$P_value, 4) else "NA"
      sig_pairs_txt <- if (!is.na(row$Significant_Pairs) && row$Significant_Pairs != "" && row$Significant_Pairs != "None") row$Significant_Pairs else "None"
      notes_txt     <- if ("Notes" %in% colnames(row) && !is.na(row$Notes)) row$Notes else ""

      san_v    <- gsub("[^A-Za-z0-9._-]", "_", gsub("\\s+", "_", v))
      png_path <- get_png_path(paste0("14_categorical_barplot_", san_v, ".pdf"))
      html <- c(html, paste0("<div style='display:inline-block; width:45%; margin:2%; vertical-align: top;'>"))
      html <- c(html, paste0("<h3>", v, "</h3>"))
      html <- c(html, paste0("<p>Chi-squared p = <strong>", p, "</strong>"))
      if (sig_pairs_txt != "None") {
        html <- c(html, paste0("; Significant pairs: <strong>", sig_pairs_txt, "</strong>"))
      }
      html <- c(html, "</p>")
      if (nchar(notes_txt) > 0) {
        html <- c(html, paste0("<div class='warning'><strong>Note:</strong> ", notes_txt, "</div>"))
      }
      html <- c(html, paste0("<div class='plot-container'><img src='", png_path, "' alt='Categorical bar plot for ", v, "'></div>"))
      html <- c(html, "</div>")
    }

    html <- c(html, "</div>")
  }

  html <- c(html, "</body>", "</html>")
  writeLines(html, report_file)
  invisible(report_file)
}
