# =============================================================================
# Variable Clustering - Shiny Server
# Server logic for K-Means variable clustering application
# =============================================================================

library(shiny)
library(DT)
library(R6)

# Server function
function(input, output, session) {
    # =========================================================================
    # REACTIVE VALUES - Store application state
    # =========================================================================
    rv <- reactiveValues(
        data = NULL, # Uploaded data
        numeric_vars = NULL, # Available numeric variables
        categorical_vars = NULL, # Available categorical variables
        dataset_type = NULL, # Type of dataset ("numeric", "categorical", "mixed")
        model = NULL, # Fitted K-means model
        results = NULL, # Clustering results
        optimal_k = NULL, # Auto-detected optimal k
        k_plot_data = NULL # Data for k selection plot
    )

    # Disable result tabs and buttons at startup
    observe({
        shinyjs::disable(selector = "a[data-value='🎯 Results']")
        shinyjs::disable(selector = "a[data-value='📈 Visualizations']")
        shinyjs::disable(selector = "a[data-value='📐 Metrics']")
        shinyjs::disable(selector = "a[data-value='🔮 Predict New Variables']")
        shinyjs::disable("run_clustering")
        shinyjs::disable("download_results")
        shinyjs::disable("download_plot")
    })

    # Reactive: selected dataset (centralized)
    selected_data <- reactive({
        req(rv$data, input$selected_vars)
        rv$data[, input$selected_vars, drop = FALSE]
    })

    # =========================================================================
    # DATA LOADING
    # =========================================================================

    # Load example data
    observeEvent(input$load_example, {
        tryCatch(
            {
                rv$data <- load_example_data("../../tests/testthat/College_Data")
                rv$numeric_vars <- extract_numeric_vars(rv$data)
                rv$categorical_vars <- extract_categorical_vars(rv$data)
                rv$dataset_type <- detect_dataset_type(rv$data)
                # Disable result tabs when new data is loaded
                shinyjs::disable(selector = "a[data-value='🎯 Results']")
                shinyjs::disable(selector = "a[data-value='📈 Visualizations']")
                shinyjs::disable(selector = "a[data-value='📐 Metrics']")
                shinyjs::disable(selector = "a[data-value='🔮 Predict New Variables']")
                # Enable run_clustering button when data is loaded
                shinyjs::enable("run_clustering")
                # Disable downloads until clustering is run
                shinyjs::disable("download_results")
                shinyjs::disable("download_plot")
                # Switch to Data tab immediately
                updateTabsetPanel(session, "main_tabs", selected = "📄 Data")
                showNotification("✅ Example data loaded successfully!",
                    type = "message", duration = 3
                )
            },
            error = function(e) {
                showNotification(paste("❌ Error loading example:", e$message),
                    type = "error", duration = 5
                )
            }
        )
    })

    # Load uploaded data
    observeEvent(input$data_file, {
        req(input$data_file)

        tryCatch(
            {
                # Use auto-detect if enabled, otherwise use manual selection
                sep_to_use <- if (input$auto_sep) NULL else input$sep

                rv$data <- load_uploaded_data(
                    input$data_file$datapath,
                    header = input$header,
                    sep = sep_to_use
                )

                # Extract all types of variables
                rv$numeric_vars <- extract_numeric_vars(rv$data)
                rv$categorical_vars <- extract_categorical_vars(rv$data)
                rv$dataset_type <- detect_dataset_type(rv$data)

                # Build informative message
                n_num <- length(rv$numeric_vars)
                n_cat <- length(rv$categorical_vars)

                msg_parts <- c()
                if (n_num > 0) msg_parts <- c(msg_parts, paste(n_num, "numeric"))
                if (n_cat > 0) msg_parts <- c(msg_parts, paste(n_cat, "categorical"))

                msg <- paste0(
                    "✅ Data loaded: ", nrow(rv$data), " rows, ",
                    paste(msg_parts, collapse = " + "), " variables"
                )

                # Disable result tabs when new data is loaded
                shinyjs::disable(selector = "a[data-value='🎯 Results']")
                shinyjs::disable(selector = "a[data-value='📈 Visualizations']")
                shinyjs::disable(selector = "a[data-value='📐 Metrics']")
                shinyjs::disable(selector = "a[data-value='🔮 Predict New Variables']")
                # Enable run_clustering button when data is loaded
                shinyjs::enable("run_clustering")
                # Disable downloads until clustering is run
                shinyjs::disable("download_results")
                shinyjs::disable("download_plot")
                # Switch to Data tab immediately
                updateTabsetPanel(session, "main_tabs", selected = "📄 Data")

                showNotification(msg, type = "message", duration = 3)
            },
            error = function(e) {
                showNotification(paste("❌ Error loading file:", e$message),
                    type = "error", duration = 5
                )
            }
        )
    })

    # =========================================================================
    # UI OUTPUTS - Dynamic elements
    # =========================================================================

    # Algorithm selector (dynamic based on data type)
    output$algorithm_selector <- renderUI({
        if (is.null(rv$dataset_type)) {
            # Default before data is loaded
            return(selectInput("algorithm",
                "Algorithm:",
                choices = c(
                    "K-Means" = "kmeans",
                    "HAC" = "hac",
                    "ACM" = "acm"
                ),
                selected = "kmeans"
            ))
        }

        # Filter algorithms based on dataset type
        if (rv$dataset_type == "numeric") {
            choices <- c("K-Means" = "kmeans", "HAC" = "hac")
            default <- "kmeans"
        } else if (rv$dataset_type == "categorical") {
            choices <- c("ACM" = "acm")
            default <- "acm"
        } else {
            # Mixed: show all with warning
            choices <- c("K-Means" = "kmeans", "HAC" = "hac", "ACM" = "acm")
            default <- "kmeans"
        }

        selectInput("algorithm",
            "Algorithm:",
            choices = choices,
            selected = default
        )
    })

    # Method selector (only for numeric algorithms)
    output$method_selector <- renderUI({
        algo <- input$algorithm
        if (is.null(algo)) algo <- "kmeans"

        # ACM doesn't use correlation/euclidean distance
        if (algo == "acm") {
            return(NULL)
        }

        selectInput("method",
            "Distance Method:",
            choices = c(
                "Correlation" = "correlation",
                "Euclidean" = "euclidean"
            ),
            selected = "correlation"
        )
    })

    # Variable selector
    output$variable_selector <- renderUI({
        req(rv$data)

        # Determine which variables to show based on selected algorithm
        algo <- input$algorithm
        if (is.null(algo)) algo <- "kmeans"

        if (algo == "acm") {
            # ACM: only categorical variables
            available_vars <- rv$categorical_vars
            if (length(available_vars) == 0) {
                return(helpText("⚠️ No categorical variables available for ACM"))
            }
        } else {
            # KMeans/HAC: only numeric variables
            available_vars <- rv$numeric_vars
            if (length(available_vars) == 0) {
                return(helpText("⚠️ No numeric variables available for this algorithm"))
            }
        }

        checkboxGroupInput(
            "selected_vars",
            "Select variables to cluster:",
            choices = available_vars,
            selected = available_vars
        )
    })

    # Algorithm-specific options (rendered dynamically)
    output$algo_options <- renderUI({
        req(input$algorithm)

        if (input$algorithm == "kmeans") {
            tagList(
                numericInput("k",
                    "Number of Clusters (k):",
                    value = 3,
                    min = 2,
                    max = 10,
                    step = 1
                ),
                checkboxInput("auto_k", "Auto-detect optimal k (elbow method)", FALSE),
                conditionalPanel(
                    condition = "input.auto_k == true",
                    numericInput("max_k",
                        "Max k to test:",
                        value = 10,
                        min = 3,
                        max = 20,
                        step = 1
                    )
                ),
                numericInput("nstart",
                    "Number of random starts (nstart):",
                    value = 10,
                    min = 1,
                    max = 50,
                    step = 1
                ),
                numericInput("seed",
                    "Random seed (optional):",
                    value = NA,
                    min = NA,
                    max = NA,
                    step = 1
                ),
                helpText("📝 K-Means++ initialization with multi-starts for stability")
            )
        } else if (input$algorithm == "hac") {
            tagList(
                numericInput("hac_k",
                    "Number of Clusters (k):",
                    value = 3,
                    min = 2,
                    max = 10,
                    step = 1
                ),
                selectInput("hac_linkage",
                    "Linkage Method:",
                    choices = c(
                        "Ward's Method" = "ward.D2",
                        "Complete" = "complete",
                        "Average" = "average",
                        "Single" = "single"
                    ),
                    selected = "ward.D2"
                ),
                helpText("📝 HAC uses hierarchical clustering with the specified linkage method")
            )
        } else if (input$algorithm == "acm") {
            tagList(
                numericInput("acm_k",
                    "Number of Clusters (k):",
                    value = 3,
                    min = 2,
                    max = 10,
                    step = 1
                ),
                numericInput("acm_max_iter",
                    "Maximum iterations:",
                    value = 30,
                    min = 10,
                    max = 100,
                    step = 1
                ),
                numericInput("acm_tol",
                    "Convergence tolerance:",
                    value = 1e-4,
                    min = 1e-6,
                    max = 1e-2,
                    step = 1e-5
                ),
                checkboxInput("acm_verbose", "Show iteration details", FALSE),
                helpText("📝 ACM uses Multiple Correspondence Analysis for categorical variables")
            )
        } else {
            helpText("Unknown algorithm")
        }
    })

    # Data info
    output$data_info <- renderText({
        req(rv$data)

        paste0(
            "Dataset Information:\n",
            "-------------------\n",
            "Rows: ", nrow(rv$data), "\n",
            "Columns: ", ncol(rv$data), "\n",
            "Numeric variables: ", length(rv$numeric_vars), "\n"
        )
    })

    # Data preview
    output$data_preview <- DT::renderDataTable({
        req(rv$data)

        DT::datatable(
            rv$data,
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = "frtip"
            ),
            rownames = TRUE
        )
    })

    # =========================================================================
    # CLUSTERING EXECUTION
    # =========================================================================

    observeEvent(input$run_clustering, {
        req(rv$data, input$selected_vars)

        if (length(input$selected_vars) < 2) {
            showNotification("⚠️ Please select at least 2 variables!",
                type = "warning", duration = 4
            )
            return()
        }

        # Show progress
        withProgress(message = "Running clustering...", value = 0, {
            tryCatch(
                {
                    X <- selected_data()
                    incProgress(0.2, detail = "Preparing data...")

                    # Prepare algorithm-specific parameters
                    algorithm <- if (is.null(input$algorithm)) "kmeans" else input$algorithm
                    params <- prepare_algo_parameters(algorithm, input)

                    incProgress(0.3, detail = "Running clustering workflow...")

                    # Run clustering workflow (delegates to helper)
                    result <- run_clustering_workflow(
                        X,
                        algorithm = algorithm,
                        params = params
                    )

                    incProgress(0.8, detail = "Storing results...")

                    # Store results
                    rv$model <- result$model
                    rv$results <- result$results
                    rv$optimal_k <- result$optimal_k
                    rv$k_plot_data <- result$k_plot_data

                    # Update UI if optimal k was found
                    if (!is.null(rv$optimal_k) && algorithm == "kmeans") {
                        updateNumericInput(session, "k", value = rv$optimal_k)
                    }

                    incProgress(1, detail = "Done!")

                    # Enable result tabs after successful clustering
                    shinyjs::enable(selector = "a[data-value='🎯 Results']")
                    shinyjs::enable(selector = "a[data-value='📈 Visualizations']")
                    shinyjs::enable(selector = "a[data-value='📐 Metrics']")
                    shinyjs::enable(selector = "a[data-value='🔮 Predict New Variables']")
                    # Enable download buttons after successful clustering
                    shinyjs::enable("download_results")
                    shinyjs::enable("download_plot")

                    showNotification(
                        paste0("✅ Clustering completed! (", rv$results$algorithm, ") ", rv$results$k, " clusters found."),
                        type = "message", duration = 4
                    )

                    # Switch to results tab
                    updateTabsetPanel(session, "main_tabs", selected = "🎯 Results")
                },
                error = function(e) {
                    showNotification(
                        paste("❌ Clustering error:", e$message),
                        type = "error", duration = 6
                    )
                }
            )
        })
    })

    # =========================================================================
    # RESULTS DISPLAY
    # =========================================================================

    output$results_ui <- renderUI({
        req(rv$results)

        tagList(
            div(
                class = "success-box",
                h4(paste0("✅ Clustering Completed - ", rv$results$k, " Clusters")),
                p(paste0("Algorithm: ", toupper(rv$results$algorithm), " — Method: ", toupper(rv$results$method))),
                p(paste0("Variables analyzed: ", rv$results$n_vars))
            ),
            br(),
            h4("📊 Cluster Assignments"),

            # Create cluster tables
            lapply(seq_along(rv$results$clusters), function(i) {
                cluster_vars <- rv$results$clusters[[i]]

                div(
                    style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
                    h5(paste0("Cluster ", i, " (", length(cluster_vars), " variables)")),
                    tags$div(
                        style = "margin-top: 10px;",
                        lapply(cluster_vars, function(v) {
                            tags$span(
                                class = "cluster-label",
                                style = sprintf(
                                    "background-color: hsl(%.0f, 70%%, 85%%);",
                                    (i * 360 / rv$results$k) %% 360
                                ),
                                v
                            )
                        })
                    )
                )
            }),
            br(),
            h4("📋 Detailed Model Summary"),
            verbatimTextOutput("model_summary_text")
        )
    })

    output$model_summary_text <- renderText({
        req(rv$results)
        paste(rv$results$model_summary, collapse = "\n")
    })

    # =========================================================================
    # VISUALIZATIONS
    # =========================================================================

    # Cluster sizes barplot
    output$plot_sizes <- renderPlot({
        req(rv$results)
        plot_cluster_sizes(rv$results$clusters)
    })

    # Variable distribution pie chart
    output$plot_distribution <- renderPlot({
        req(rv$results)
        plot_cluster_distribution(rv$results$clusters)
    })

    # Correlation heatmap or eta² heatmap for ACM
    output$plot_heatmap <- renderPlot({
        req(rv$data, input$selected_vars, rv$model)
        if (!is.null(rv$results$algorithm) && rv$results$algorithm == "acm") {
            # ACM: plot eta² heatmap
            source("R/visualizations.R", local = TRUE)
            plot_eta2_heatmap(rv$model)
        } else {
            plot_correlation_heatmap(rv$data, rv$results$clusters, input$selected_vars)
        }
    })

    # K selection plot
    output$plot_k_selection <- renderPlot({
        req(rv$k_plot_data)
        plot_k_selection(rv$k_plot_data, rv$optimal_k)
    })

    # =========================================================================
    # PREDICTION ON NEW VARIABLES
    # =========================================================================

    observeEvent(input$run_prediction, {
        req(input$predict_file, rv$model)

        tryCatch(
            {
                # Run prediction workflow (delegates to helper)
                predictions <- run_prediction_workflow(
                    rv$model,
                    input$predict_file$datapath,
                    header = input$header,
                    sep = input$sep
                )

                # Display results
                output$prediction_results <- renderUI({
                    tagList(
                        div(
                            class = "success-box",
                            h4("✅ Classification Complete"),
                            p(paste0("Classified ", nrow(predictions), " variables"))
                        ),
                        br(),
                        DT::dataTableOutput("prediction_table")
                    )
                })

                output$prediction_table <- DT::renderDataTable({
                    DT::datatable(
                        predictions,
                        options = list(pageLength = 10),
                        rownames = FALSE
                    ) %>%
                        DT::formatRound(columns = "distance", digits = 4)
                })

                showNotification("✅ Prediction completed!",
                    type = "message", duration = 3
                )
            },
            error = function(e) {
                showNotification(paste("❌ Prediction error:", e$message),
                    type = "error", duration = 5
                )
            }
        )
    })

    # =========================================================================
    # DOWNLOAD HANDLERS
    # =========================================================================

    output$download_results <- downloadHandler(
        filename = function() {
            paste0("clustering_results_", Sys.Date(), ".csv")
        },
        content = function(file) {
            req(rv$results)

            # Create results dataframe
            results_df <- data.frame(
                Variable = unlist(rv$results$clusters),
                Cluster = rep(
                    seq_along(rv$results$clusters),
                    sapply(rv$results$clusters, length)
                ),
                stringsAsFactors = FALSE
            )

            write.csv(results_df, file, row.names = FALSE)
        }
    )

    output$download_plot <- downloadHandler(
        filename = function() {
            paste0("clustering_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            req(rv$results)

            png(file, width = 1200, height = 800, res = 120)

            sizes <- sapply(rv$results$clusters, length)
            names(sizes) <- paste0("Cluster ", seq_along(sizes))

            par(mar = c(5, 5, 3, 2))
            bp <- barplot(sizes,
                col = hcl.colors(length(sizes), "Set 2"),
                main = "Number of Variables per Cluster",
                ylab = "Number of Variables",
                xlab = "Cluster",
                border = NA,
                ylim = c(0, max(sizes) * 1.2)
            )

            text(bp, sizes, labels = sizes, pos = 3, cex = 1.2, font = 2)

            dev.off()
        }
    )

    # =========================================================================
    # METRICS TAB OUTPUT
    # =========================================================================
    output$metrics_ui <- renderUI({
        req(rv$results, rv$data, input$selected_vars)
        # Load metrics module
        source("R/metrics.R", local = TRUE)
        X <- rv$data[, input$selected_vars, drop = FALSE]
        clusters <- rv$results$clusters
        method <- rv$results$method
        metrics <- compute_simple_metrics(X, clusters, method)
        # Cophenetic (HAC) or Q (ACM)
        extra_metric <- NULL
        if (rv$results$algorithm == "hac" && !is.null(rv$model$model)) {
            extra_metric <- tryCatch(
                {
                    compute_cophenetic(rv$model$model)
                },
                error = function(e) NA
            )
        } else if (rv$results$algorithm == "acm") {
            extra_metric <- tryCatch(
                {
                    extract_acm_Q(rv$model)
                },
                error = function(e) NA
            )
        }
        tagList(
            h4("Simple Clustering Metrics"),
            fluidRow(
                column(
                    6,
                    tags$b("Homogeneity (intra-cluster):"),
                    verbatimTextOutput("metric_homogeneity"),
                    tags$div(
                        class = "metric-info",
                        "→ Mesure la cohésion des variables dans chaque cluster. Plus proche de 1 = variables très similaires. NA si cluster singleton."
                    )
                ),
                column(
                    6,
                    tags$b("Separation (inter-cluster):"),
                    verbatimTextOutput("metric_separation"),
                    tags$div(
                        class = "metric-info",
                        "→ Mesure la différence entre clusters. Plus faible = clusters bien séparés."
                    )
                )
            ),
            br(),
            tags$b("Mean Silhouette (variables):"),
            verbatimTextOutput("metric_silhouette"),
            tags$div(
                class = "metric-info",
                "→ Indique la qualité d’affectation des variables à leur cluster. >0.5 = bonne séparation, <0 = variables mal classées."
            ),
            if (!is.null(extra_metric)) {
                if (rv$results$algorithm == "hac") {
                    tagList(
                        tags$b("Cophenetic correlation (HAC):"),
                        verbatimTextOutput("metric_cophenetic"),
                        tags$div(
                            class = "metric-info",
                            "→ Corrélation entre distances originales et structure du dendrogramme. Proche de 1 = structure fidèle. NA si non calculable."
                        )
                    )
                } else if (rv$results$algorithm == "acm") {
                    tagList(
                        tags$b("Q criterion (ACM):"),
                        verbatimTextOutput("metric_acmQ"),
                        tags$div(
                            class = "metric-info",
                            "→ Qualité globale de la partition ACM (somme des η²). Plus élevé = clusters plus explicatifs."
                        )
                    )
                }
            }
        )
    })

    output$metric_homogeneity <- renderText({
        req(rv$results, rv$data, input$selected_vars)
        source("R/metrics.R", local = TRUE)
        X <- rv$data[, input$selected_vars, drop = FALSE]
        clusters <- rv$results$clusters
        method <- rv$results$method
        metrics <- compute_simple_metrics(X, clusters, method)
        if (!is.null(metrics$message)) {
            metrics$message
        } else {
            paste(round(metrics$homogeneity, 3), collapse = ", ")
        }
    })
    output$metric_separation <- renderText({
        req(rv$results, rv$data, input$selected_vars)
        source("R/metrics.R", local = TRUE)
        X <- rv$data[, input$selected_vars, drop = FALSE]
        clusters <- rv$results$clusters
        method <- rv$results$method
        metrics <- compute_simple_metrics(X, clusters, method)
        if (!is.null(metrics$message)) {
            metrics$message
        } else {
            round(metrics$separation, 3)
        }
    })
    output$metric_silhouette <- renderText({
        req(rv$results, rv$data, input$selected_vars)
        source("R/metrics.R", local = TRUE)
        X <- rv$data[, input$selected_vars, drop = FALSE]
        clusters <- rv$results$clusters
        method <- rv$results$method
        metrics <- compute_simple_metrics(X, clusters, method)
        if (!is.null(metrics$message)) {
            metrics$message
        } else {
            round(metrics$silhouette_mean, 3)
        }
    })
    output$metric_cophenetic <- renderText({
        req(rv$results, rv$model$model)
        source("R/metrics.R", local = TRUE)
        val <- tryCatch(
            {
                compute_cophenetic(rv$model$model)
            },
            error = function(e) NA
        )
        round(val, 3)
    })
    output$metric_acmQ <- renderText({
        req(rv$results, rv$model)
        source("R/metrics.R", local = TRUE)
        val <- tryCatch(
            {
                extract_acm_Q(rv$model)
            },
            error = function(e) NA
        )
        round(val, 3)
    })

    # =========================================================================
    # RESET FUNCTIONALITY
    # =========================================================================

    observeEvent(input$reset, {
        rv$data <- NULL
        rv$numeric_vars <- NULL
        rv$categorical_vars <- NULL
        rv$dataset_type <- NULL
        rv$model <- NULL
        rv$results <- NULL
        rv$optimal_k <- NULL
        rv$k_plot_data <- NULL

        # Reset file input UI
        shinyjs::reset("data_file")

        # Disable result tabs on reset
        shinyjs::disable(selector = "a[data-value='🎯 Results']")
        shinyjs::disable(selector = "a[data-value='📈 Visualizations']")
        shinyjs::disable(selector = "a[data-value='📐 Metrics']")
        shinyjs::disable(selector = "a[data-value='🔮 Predict New Variables']")
        # Disable buttons on reset
        shinyjs::disable("run_clustering")
        shinyjs::disable("download_results")
        shinyjs::disable("download_plot")

        updateTabsetPanel(session, "main_tabs", selected = "📄 Data")

        showNotification("🔄 Application reset", type = "message", duration = 2)
    })
}
