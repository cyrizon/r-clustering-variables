# =============================================================================
# Variable Clustering - Shiny Application
# Interactive interface for K-Means variable clustering
# =============================================================================

library(shiny)
library(shinyjs)

# UI Definition
fluidPage(
    # Enable shinyjs for dynamic UI updates
    useShinyjs(),
    # Custom CSS (external file)
    tags$head(
        includeCSS("www/style.css")
    ),

    # Application header
    div(
        class = "main-header",
        h1("📊 Variable Clustering"),
        p("Interactive tool for clustering variables using different algorithms")
    ),

    # Sidebar layout
    sidebarLayout(
        # =====================================================================
        # SIDEBAR PANEL - Controls
        # =====================================================================
        sidebarPanel(
            width = 3,
            h3("⚙️ Configuration"),

            # Step 1: Data Upload
            h4("1️⃣ Load Data"),
            fileInput("data_file",
                "Choose CSV/TSV/Parquet File",
                accept = c(".csv", ".tsv", ".txt", ".parquet"),
                placeholder = "No file selected"
            ),
            tags$div(
                class = "upload-info",
                tags$small("Max upload: 300 MB. For very large datasets prefer Parquet.")
            ),
            checkboxInput("header", "File has header", TRUE),
            checkboxInput("auto_sep", "Auto-detect separator", TRUE),
            conditionalPanel(
                condition = "input.auto_sep == false",
                radioButtons("sep", "Separator:",
                    choices = c(
                        "Comma" = ",",
                        "Semicolon" = ";",
                        "Tab" = "\t"
                    ),
                    selected = ","
                )
            ),
            actionButton("load_example", "📂 Load Example Data",
                class = "btn-info btn-sm",
                style = "margin-bottom: 15px;"
            ),
            hr(),

            # Step 2: Variable Selection
            h4("2️⃣ Select Variables"),
            uiOutput("variable_selector"),
            hr(),

            # Step 3: Clustering Parameters
            h4("3️⃣ Clustering Settings"),
            uiOutput("algorithm_selector"),
            uiOutput("method_selector"),
            # Algorithm-specific options rendered from server
            uiOutput("algo_options"),
            hr(),

            # Action Buttons
            actionButton("run_clustering",
                "🚀 Run Clustering",
                class = "btn-primary btn-lg btn-block",
                style = "margin-bottom: 10px;"
            ),
            actionButton("reset",
                "🔄 Reset",
                class = "btn-warning btn-block"
            ),
            hr(),

            # Download Results
            h4("💾 Export"),
            downloadButton("download_results",
                "Download Results",
                class = "btn-success btn-block"
            ),
            downloadButton("download_plot",
                "Download Plot",
                class = "btn-success btn-block",
                style = "margin-top: 5px;"
            )
        ),

        # =====================================================================
        # MAIN PANEL - Results
        # =====================================================================
        mainPanel(
            width = 9,
            tabsetPanel(
                id = "main_tabs",

                # --- TAB 1: Data Preview ---
                tabPanel(
                    "📄 Data",
                    br(),
                    h3("Data Preview"),
                    verbatimTextOutput("data_info"),
                    br(),
                    DT::dataTableOutput("data_preview")
                ),

                # --- TAB 2: Clustering Results ---
                tabPanel(
                    "🎯 Results",
                    br(),
                    uiOutput("results_ui")
                ),

                # --- TAB 3: Visualizations ---
                tabPanel(
                    "📈 Visualizations",
                    br(),
                    h3("Clustering Visualizations"),
                    fluidRow(
                        column(
                            6,
                            plotOutput("plot_sizes", height = "300px")
                        ),
                        column(
                            6,
                            plotOutput("plot_distribution", height = "300px")
                        )
                    ),
                    br(),
                    fluidRow(
                        column(
                            12,
                            plotOutput("plot_heatmap", height = "500px")
                        )
                    ),
                    br(),
                    conditionalPanel(
                      condition = "input.algorithm == 'hac'",
                      h3("Hierarchical Clustering Specifics"),
                      fluidRow(
                        column(12,
                               plotOutput("plot_dendrogram", height = "500px")
                        )
                      ),
                      br(),
                      fluidRow(
                        column(12,
                               plotOutput("plot_heights", height = "400px")
                        )
                      )
                    ),
                    conditionalPanel(
                        condition = "input.auto_k == true",
                        h4("Optimal k Selection"),
                        plotOutput("plot_k_selection", height = "400px")
                    )
                ),

                # --- TAB 4: Metrics ---
                tabPanel(
                    "📐 Metrics",
                    br(),
                    h3("Clustering Metrics"),
                    uiOutput("metrics_ui")
                ),

                # --- TAB 5: Prediction ---
                tabPanel(
                    "🔮 Predict New Variables",
                    br(),
                    h3("Classify New Variables"),
                    div(
                        class = "info-box",
                        p("Upload a dataset with new variables to classify them into existing clusters."),
                        p("⚠️ The new dataset must have the same number of observations (rows) as the training data.")
                    ),
                    fileInput("predict_file",
                        "Upload New Variables (CSV/TSV/Parquet):",
                        accept = c(".csv", ".tsv", ".txt", ".parquet")
                    ),
                    tags$div(
                        class = "upload-info",
                        tags$small("Max upload: 300 MB. Use Parquet for faster uploads and reuse.")
                    ),
                    actionButton("run_prediction",
                        "🔮 Classify Variables",
                        class = "btn-primary"
                    ),
                    br(), br(),
                    uiOutput("prediction_results")
                ),

                # --- TAB 6: Help ---
                tabPanel(
                    "ℹ️ Help",
                    br(),
                    h3("How to Use This Application"),
                    div(
                        class = "info-box",
                        h4("Step 1: Load Your Data"),
                        p("• Upload a CSV or TSV file containing your dataset"),
                        p("• Or click 'Load Example Data' to use the College dataset"),
                        p("• Supports numeric variables (K-Means, HAC) and categorical variables (ACM)"),
                        p("• Auto-detection of separator and data type")
                    ),
                    div(
                        class = "info-box",
                        h4("Step 2: Select Variables"),
                        p("• Choose which variables to include in the clustering"),
                        p("• You can select all or a subset of variables"),
                        p("• Variable selection adapts automatically based on the chosen algorithm")
                    ),
                    div(
                        class = "info-box",
                        h4("Step 3: Choose Algorithm & Configure"),
                        p("Three algorithms are available:"),
                        p(strong("K-Means:"), " Iterative clustering for numeric variables"),
                        p("  - Distance: Correlation (pattern similarity) or Euclidean"),
                        p("  - K-Means++ initialization with multi-starts (nstart)"),
                        p("  - Auto-detect optimal k: Elbow method with distance-to-line detection"),
                        p("  - Set random seed for reproducibility"),
                        p(strong("HAC (Hierarchical Agglomerative Clustering):"), " Tree-based clustering for numeric variables"),
                        p("  - Distance: Correlation or Euclidean"),
                        p("  - Linkage: Ward's method, Complete, Average, or Single"),
                        p("  - Cut tree at specified k to obtain clusters"),
                        p(strong("ACM (Multiple Correspondence Analysis):"), " Clustering for categorical variables"),
                        p("  - Iterative optimization based on χ² (chi-squared) association test"),
                        p("  - Convergence controlled by max iterations and tolerance"),
                        p("  - Q criterion tracks overall clustering quality")
                    ),
                    div(
                        class = "info-box",
                        h4("Step 4: Run and Analyze"),
                        p("• Click 'Run Clustering' to perform the analysis"),
                        p("• Results tab: View cluster assignments and model summary"),
                        p("• Visualizations tab: Cluster sizes, distribution, correlation/χ² heatmaps"),
                        p("• Metrics tab: Homogeneity, separation, silhouette, cophenetic correlation (HAC), Q criterion (ACM)"),
                        p("• Predict tab: Classify new variables into existing clusters"),
                        p("• Export: Download results as CSV or plots as images")
                    ),
                    div(
                        class = "info-box",
                        h4("Interpreting Metrics"),
                        p(strong("Homogeneity:"), " Cohesion within clusters (higher = more similar variables)"),
                        p(strong("Separation:"), " Distance between clusters (lower = better separated)"),
                        p(strong("Silhouette:"), " Quality of assignments (>0.5 good, <0 poor)"),
                        p(strong("Cophenetic (HAC):"), " Fidelity of dendrogram structure (closer to 1 = better)"),
                        p(strong("Q criterion (ACM):"), " Overall quality of categorical clustering (higher = better)")
                    )
                )
            )
        )
    )
)
