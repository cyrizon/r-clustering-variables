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

    # Custom CSS
    tags$head(
        tags$style(HTML("
            .main-header {
                background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                color: white;
                padding: 20px;
                border-radius: 5px;
                margin-bottom: 20px;
            }
            .info-box {
                background-color: #f8f9fa;
                border-left: 4px solid #667eea;
                padding: 15px;
                margin: 10px 0;
                border-radius: 4px;
            }
            .success-box {
                background-color: #d4edda;
                border-left: 4px solid #28a745;
                padding: 15px;
                margin: 10px 0;
                border-radius: 4px;
            }
            .error-box {
                background-color: #f8d7da;
                border-left: 4px solid #dc3545;
                padding: 15px;
                margin: 10px 0;
                border-radius: 4px;
            }
            .cluster-label {
                font-weight: bold;
                padding: 5px 10px;
                border-radius: 3px;
                display: inline-block;
                margin: 2px;
            }
        "))
    ),

    # Application header
    div(
        class = "main-header",
        h1("📊 Variable Clustering with K-Means"),
        p("Interactive tool for clustering variables based on correlation or euclidean distance")
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
                "Choose CSV/TSV File",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "No file selected"
            ),
            checkboxInput("header", "File has header", TRUE),
            radioButtons("sep", "Separator:",
                choices = c(
                    "Comma" = ",",
                    "Semicolon" = ";",
                    "Tab" = "\t"
                ),
                selected = ","
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
            selectInput("method",
                "Distance Method:",
                choices = c(
                    "Correlation" = "correlation",
                    "Euclidean" = "euclidean"
                ),
                selected = "correlation"
            ),
            conditionalPanel(
                condition = "input.method == 'correlation'",
                selectInput("dist_strategy",
                    "Correlation strategy:",
                    choices = c("PAM (partitioning around medoids)" = "pam", "MDS + kmeans" = "mds"),
                    selected = "pam"
                )
            ),
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
            ),
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
                            h4("Cluster Sizes"),
                            plotOutput("plot_sizes", height = "300px")
                        ),
                        column(
                            6,
                            h4("Variable Distribution"),
                            plotOutput("plot_distribution", height = "300px")
                        )
                    ),
                    br(),
                    fluidRow(
                        column(
                            12,
                            h4("Correlation Heatmap"),
                            plotOutput("plot_heatmap", height = "500px")
                        )
                    ),
                    br(),
                    conditionalPanel(
                        condition = "input.auto_k == true",
                        h4("Optimal k Selection"),
                        plotOutput("plot_k_selection", height = "400px")
                    )
                ),

                # --- TAB 4: Prediction ---
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
                        "Upload New Variables (CSV/TSV):",
                        accept = c(".csv", ".tsv", ".txt")
                    ),
                    actionButton("run_prediction",
                        "🔮 Classify Variables",
                        class = "btn-primary"
                    ),
                    br(), br(),
                    uiOutput("prediction_results")
                ),

                # --- TAB 5: Help ---
                tabPanel(
                    "ℹ️ Help",
                    br(),
                    h3("How to Use This Application"),
                    div(
                        class = "info-box",
                        h4("Step 1: Load Your Data"),
                        p("• Upload a CSV or TSV file containing your dataset"),
                        p("• Or click 'Load Example Data' to use the College dataset"),
                        p("• Make sure your data contains numeric variables")
                    ),
                    div(
                        class = "info-box",
                        h4("Step 2: Select Variables"),
                        p("• Choose which variables to include in the clustering"),
                        p("• You can select all or a subset of variables"),
                        p("• Only numeric variables can be clustered")
                    ),
                    div(
                        class = "info-box",
                        h4("Step 3: Configure Clustering"),
                        p("• Distance Method:"),
                        p("  - Correlation: Groups variables with similar patterns"),
                        p("  - Euclidean: Groups variables with similar values"),
                        p("• Number of Clusters (k): How many groups to create"),
                        p("• Auto-detect optimal k: Automatically find the best k")
                    ),
                    div(
                        class = "info-box",
                        h4("Step 4: Run and Analyze"),
                        p("• Click 'Run Clustering' to perform the analysis"),
                        p("• View results in the Results tab"),
                        p("• Explore visualizations in the Visualizations tab"),
                        p("• Download results as CSV or plots as images")
                    ),
                    div(
                        class = "info-box",
                        h4("About K-Means Variable Clustering"),
                        p("Unlike traditional K-means which clusters observations (rows),
                          this algorithm clusters VARIABLES (columns). It groups variables
                          that behave similarly across observations."),
                        p("Use cases:"),
                        p("• Data reduction: Identify redundant variables"),
                        p("• Feature selection: Select representative variables from each cluster"),
                        p("• Data exploration: Understand variable relationships")
                    )
                )
            )
        )
    )
)
