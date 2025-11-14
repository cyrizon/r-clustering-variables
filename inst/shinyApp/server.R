# =============================================================================
# Variable Clustering - Shiny Server
# Server logic for K-Means variable clustering application
# =============================================================================

library(shiny)
library(DT)
library(R6)

# Load the KMeans clustering class
source("../../R/algorithms/KMeansVariablesR6.R", local = TRUE)

# Server function
function(input, output, session) {
    
    # =========================================================================
    # REACTIVE VALUES - Store application state
    # =========================================================================
    rv <- reactiveValues(
        data = NULL,              # Uploaded data
        numeric_vars = NULL,      # Available numeric variables
        model = NULL,             # Fitted K-means model
        results = NULL,           # Clustering results
        optimal_k = NULL,         # Auto-detected optimal k
        k_plot_data = NULL        # Data for k selection plot
    )
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    # Load example data
    observeEvent(input$load_example, {
        tryCatch({
            # Try to load College_Data
            data_path <- "../../tests/testthat/College_Data"
            if (file.exists(data_path)) {
                rv$data <- read.csv(data_path, row.names = 1)
                showNotification("✅ Example data loaded successfully!", 
                               type = "message", duration = 3)
            } else {
                # Generate synthetic data if example not found
                set.seed(42)
                n <- 100
                rv$data <- data.frame(
                    Var1 = rnorm(n),
                    Var2 = rnorm(n) + rnorm(n, sd = 0.3),
                    Var3 = rnorm(n),
                    Var4 = rnorm(n) * 2,
                    Var5 = rnorm(n) + rnorm(n, sd = 0.5),
                    Var6 = rnorm(n)
                )
                showNotification("✅ Synthetic example data generated!", 
                               type = "message", duration = 3)
            }
            
            # Extract numeric variables
            rv$numeric_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
            
        }, error = function(e) {
            showNotification(paste("❌ Error loading example:", e$message), 
                           type = "error", duration = 5)
        })
    })
    
    # Load uploaded data
    observeEvent(input$data_file, {
        req(input$data_file)
        
        tryCatch({
            rv$data <- read.table(
                input$data_file$datapath,
                header = input$header,
                sep = input$sep,
                stringsAsFactors = FALSE
            )
            
            # Extract numeric variables
            rv$numeric_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
            
            if (length(rv$numeric_vars) == 0) {
                showNotification("⚠️ No numeric variables found in the dataset!", 
                               type = "warning", duration = 5)
            } else {
                showNotification(
                    paste0("✅ Data loaded: ", nrow(rv$data), " rows, ", 
                          length(rv$numeric_vars), " numeric variables"),
                    type = "message", duration = 3
                )
            }
            
        }, error = function(e) {
            showNotification(paste("❌ Error loading file:", e$message), 
                           type = "error", duration = 5)
        })
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
                dom = 'frtip'
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
                           type = "warning", duration = 4)
            return()
        }
        
        # Show progress
        withProgress(message = 'Running clustering...', value = 0, {
            
            tryCatch({
                # Get selected data
                X <- rv$data[, input$selected_vars, drop = FALSE]
                
                incProgress(0.2, detail = "Preparing data...")
                
                # Auto-detect optimal k if requested
                if (input$auto_k) {
                    incProgress(0.3, detail = "Finding optimal k...")
                    
                    temp_model <- KMeansVariablesR6$new(method = input$method)
                    
                    if (requireNamespace("cluster", quietly = TRUE)) {
                        rv$optimal_k <- temp_model$suggest_k_automatic(
                            X, 
                            max_k = input$max_k,
                            method = input$k_method
                        )
                        
                        # Store k selection data for plotting
                        if (input$k_method == "silhouette") {
                            sil_data <- numeric(input$max_k - 1)
                            for (k in 2:input$max_k) {
                                temp_model$k <- k
                                temp_model$fit(X)
                                if (input$method == "correlation") {
                                    cor_mat <- cor(scale(X))
                                    dist_mat <- 1 - abs(cor_mat)
                                    sil <- cluster::silhouette(temp_model$model$cluster, 
                                                              as.dist(dist_mat))
                                } else {
                                    sil <- cluster::silhouette(temp_model$model$cluster, 
                                                              dist(t(scale(X))))
                                }
                                sil_data[k - 1] <- mean(sil[, 3])
                            }
                            rv$k_plot_data <- data.frame(
                                k = 2:input$max_k,
                                value = sil_data,
                                type = "Silhouette Width"
                            )
                        }
                        
                        updateNumericInput(session, "k", value = rv$optimal_k)
                        k_to_use <- rv$optimal_k
                    } else {
                        showNotification(
                            "⚠️ Package 'cluster' not available. Using manual k value.",
                            type = "warning", duration = 4
                        )
                        k_to_use <- input$k
                    }
                } else {
                    k_to_use <- input$k
                }
                
                incProgress(0.5, detail = "Fitting model...")
                
                # Create and fit model
                rv$model <- KMeansVariablesR6$new(
                    k = k_to_use,
                    method = input$method
                )
                
                rv$model$fit(X)
                
                incProgress(0.8, detail = "Generating results...")
                
                # Extract results
                rv$results <- list(
                    clusters = rv$model$clusters,
                    k = k_to_use,
                    method = input$method,
                    n_vars = length(input$selected_vars),
                    model_summary = capture.output(rv$model$summary())
                )
                
                incProgress(1, detail = "Done!")
                
                showNotification(
                    paste0("✅ Clustering completed! ", k_to_use, " clusters found."),
                    type = "message", duration = 4
                )
                
                # Switch to results tab
                updateTabsetPanel(session, "main_tabs", selected = "🎯 Results")
                
            }, error = function(e) {
                showNotification(
                    paste("❌ Clustering error:", e$message),
                    type = "error", duration = 6
                )
            })
        })
    })
    
    # =========================================================================
    # RESULTS DISPLAY
    # =========================================================================
    
    output$results_ui <- renderUI({
        req(rv$results)
        
        tagList(
            div(class = "success-box",
                h4(paste0("✅ Clustering Completed - ", rv$results$k, " Clusters")),
                p(paste0("Method: ", toupper(rv$results$method))),
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
                                    "background-color: hsl(%d, 70%%, 85%%);",
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
        
        sizes <- sapply(rv$results$clusters, length)
        names(sizes) <- paste0("Cluster ", seq_along(sizes))
        
        par(mar = c(5, 5, 3, 2))
        bp <- barplot(sizes,
                col = hcl.colors(length(sizes), "Set 2"),
                main = "Number of Variables per Cluster",
                ylab = "Number of Variables",
                xlab = "Cluster",
                border = NA,
                ylim = c(0, max(sizes) * 1.2))
        
        # Add value labels
        text(bp, sizes, labels = sizes, pos = 3, cex = 1.2, font = 2)
    })
    
    # Variable distribution pie chart
    output$plot_distribution <- renderPlot({
        req(rv$results)
        
        sizes <- sapply(rv$results$clusters, length)
        labels <- paste0("Cluster ", seq_along(sizes), "\n(", sizes, ")")
        
        par(mar = c(2, 2, 3, 2))
        pie(sizes,
            labels = labels,
            col = hcl.colors(length(sizes), "Set 2"),
            main = "Variable Distribution Across Clusters",
            border = "white",
            cex = 1.1)
    })
    
    # Correlation heatmap
    output$plot_heatmap <- renderPlot({
        req(rv$data, input$selected_vars, rv$model)
        
        X <- rv$data[, input$selected_vars, drop = FALSE]
        cor_mat <- cor(X)
        
        # Order by clusters
        cluster_order <- unlist(rv$results$clusters)
        cor_mat_ordered <- cor_mat[cluster_order, cluster_order]
        
        # Plot heatmap
        par(mar = c(10, 10, 4, 2))
        image(1:ncol(cor_mat_ordered), 
              1:nrow(cor_mat_ordered),
              t(cor_mat_ordered[nrow(cor_mat_ordered):1, ]),
              col = hcl.colors(50, "RdBu", rev = TRUE),
              xlab = "", ylab = "",
              main = "Correlation Matrix (ordered by clusters)",
              axes = FALSE)
        
        axis(1, at = 1:ncol(cor_mat_ordered), 
             labels = colnames(cor_mat_ordered), 
             las = 2, cex.axis = 0.8)
        axis(2, at = 1:nrow(cor_mat_ordered), 
             labels = rev(rownames(cor_mat_ordered)), 
             las = 2, cex.axis = 0.8)
        
        # Add cluster boundaries
        cum_sizes <- cumsum(sapply(rv$results$clusters, length))
        for (i in seq_along(cum_sizes)[-length(cum_sizes)]) {
            abline(v = cum_sizes[i] + 0.5, col = "black", lwd = 2)
            abline(h = nrow(cor_mat_ordered) - cum_sizes[i] + 0.5, 
                   col = "black", lwd = 2)
        }
    })
    
    # K selection plot
    output$plot_k_selection <- renderPlot({
        req(rv$k_plot_data)
        
        par(mar = c(5, 5, 3, 2))
        plot(rv$k_plot_data$k, rv$k_plot_data$value,
             type = "b", pch = 19, col = "#667eea",
             lwd = 2, cex = 1.5,
             xlab = "Number of Clusters (k)",
             ylab = rv$k_plot_data$type[1],
             main = paste("Optimal k Selection -", rv$k_plot_data$type[1]),
             xaxt = "n")
        
        axis(1, at = rv$k_plot_data$k)
        grid(col = "gray80")
        
        # Highlight optimal k
        if (!is.null(rv$optimal_k)) {
            abline(v = rv$optimal_k, col = "red", lty = 2, lwd = 2)
            points(rv$optimal_k, 
                   rv$k_plot_data$value[rv$k_plot_data$k == rv$optimal_k],
                   col = "red", pch = 19, cex = 2)
            text(rv$optimal_k, 
                 par("usr")[4] * 0.95,
                 paste("Optimal k =", rv$optimal_k),
                 col = "red", font = 2)
        }
    })
    
    # =========================================================================
    # PREDICTION ON NEW VARIABLES
    # =========================================================================
    
    observeEvent(input$run_prediction, {
        req(input$predict_file, rv$model)
        
        tryCatch({
            # Load prediction data
            pred_data <- read.table(
                input$predict_file$datapath,
                header = input$header,
                sep = input$sep,
                stringsAsFactors = FALSE
            )
            
            # Get numeric variables
            pred_numeric <- pred_data[, sapply(pred_data, is.numeric), drop = FALSE]
            
            if (ncol(pred_numeric) == 0) {
                showNotification("⚠️ No numeric variables in prediction file!", 
                               type = "warning")
                return()
            }
            
            # Make predictions
            predictions <- rv$model$predict(pred_numeric)
            
            # Display results
            output$prediction_results <- renderUI({
                tagList(
                    div(class = "success-box",
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
                           type = "message", duration = 3)
            
        }, error = function(e) {
            showNotification(paste("❌ Prediction error:", e$message),
                           type = "error", duration = 5)
        })
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
                Cluster = rep(seq_along(rv$results$clusters), 
                            sapply(rv$results$clusters, length)),
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
                    ylim = c(0, max(sizes) * 1.2))
            
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
