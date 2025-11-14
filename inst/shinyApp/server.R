# =============================================================================
# Variable Clustering - Shiny Server
# Server logic for K-Means variable clustering application
# =============================================================================

library(shiny)
library(DT)
library(R6)

# Load algorithm classes with error handling
has_hac <- FALSE
has_acm <- FALSE
tryCatch(
    {
        source("../../R/algorithms/KMeansVariablesR6.R", local = TRUE)
    },
    error = function(e) {
        message(paste("Failed to load KMeans class:", e$message))
    }
)
tryCatch(
    {
        source("../../R/algorithms/HACVariablesR6.R", local = TRUE)
        has_hac <<- TRUE
    },
    error = function(e) {
        has_hac <<- FALSE
    }
)
tryCatch(
    {
        source("../../R/algorithms/ACMVariablesR6.R", local = TRUE)
        has_acm <<- TRUE
    },
    error = function(e) {
        has_acm <<- FALSE
    }
)

# Load helper modules
source("../../R/alg_helpers.R", local = TRUE)
source("R/data_handlers.R", local = TRUE)
source("R/clustering_logic.R", local = TRUE)
source("R/visualizations.R", local = TRUE)
source("R/algo_parameter_helpers.R", local = TRUE)
source("R/predictions.R", local = TRUE)

# Server function
function(input, output, session) {
    # =========================================================================
    # REACTIVE VALUES - Store application state
    # =========================================================================
    rv <- reactiveValues(
        data = NULL, # Uploaded data
        numeric_vars = NULL, # Available numeric variables
        model = NULL, # Fitted K-means model
        results = NULL, # Clustering results
        optimal_k = NULL, # Auto-detected optimal k
        k_plot_data = NULL # Data for k selection plot
    )

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
                rv$data <- load_uploaded_data(
                    input$data_file$datapath,
                    header = input$header,
                    sep = input$sep
                )

                # Extract numeric variables
                rv$numeric_vars <- extract_numeric_vars(rv$data)

                if (length(rv$numeric_vars) == 0) {
                    showNotification("⚠️ No numeric variables found in the dataset!",
                        type = "warning", duration = 5
                    )
                } else {
                    showNotification(
                        paste0(
                            "✅ Data loaded: ", nrow(rv$data), " rows, ",
                            length(rv$numeric_vars), " numeric variables"
                        ),
                        type = "message", duration = 3
                    )
                }
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

    # Variable selector
    output$variable_selector <- renderUI({
        req(rv$numeric_vars)

        checkboxGroupInput(
            "selected_vars",
            "Select variables to cluster:",
            choices = rv$numeric_vars,
            selected = rv$numeric_vars
        )
    })

    # Algorithm-specific options (rendered dynamically)
    output$algo_options <- renderUI({
        req(input$algorithm)

        if (input$algorithm == "kmeans") {
            tagList(
                numericInput("nstart",
                    "kmeans: number of random starts (nstart):",
                    value = 25,
                    min = 1,
                    max = 1000,
                    step = 1
                ),
                numericInput("seed",
                    "Optional random seed (integer, leave blank for none):",
                    value = NA,
                    min = NA,
                    max = NA,
                    step = 1
                ),
                numericInput("k",
                    "Number of Clusters (k):",
                    value = 3,
                    min = 2,
                    max = 10,
                    step = 1
                ),
                checkboxInput("auto_k", "Auto-detect optimal k", FALSE),
                conditionalPanel(
                    condition = "input.auto_k == true",
                    selectInput("k_method",
                        "Method:",
                        choices = c(
                            "Silhouette" = "silhouette",
                            "Gap Statistic" = "gap"
                        ),
                        selected = "silhouette"
                    ),
                    numericInput("max_k",
                        "Max k to test:",
                        value = 8,
                        min = 3,
                        max = 15,
                        step = 1
                    )
                )
            )
        } else if (input$algorithm == "hac") {
            tagList(
                helpText("Ici mettre les options spécifiques pour HAC (par ex. linkage, cut height)"),
                # selectInput("hac_linkage", "Linkage:", choices = c("complete","average","single"), selected = "average"),
                # numericInput("hac_cut", "Cut height:", value = NA)
            )
        } else if (input$algorithm == "acm") {
            tagList(
                helpText("Ici mettre les options spécifiques pour ACM (par ex. affinity, thresholds)"),
                # selectInput("acm_affinity", "Affinity:", choices = c("euclidean","cosine"), selected = "cosine")
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

    # Correlation heatmap
    output$plot_heatmap <- renderPlot({
        req(rv$data, input$selected_vars, rv$model)
        plot_correlation_heatmap(rv$data, rv$results$clusters, input$selected_vars)
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
    # RESET FUNCTIONALITY
    # =========================================================================

    observeEvent(input$reset, {
        rv$data <- NULL
        rv$numeric_vars <- NULL
        rv$model <- NULL
        rv$results <- NULL
        rv$optimal_k <- NULL
        rv$k_plot_data <- NULL

        updateTabsetPanel(session, "main_tabs", selected = "📄 Data")

        showNotification("🔄 Application reset", type = "message", duration = 2)
    })
}
