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
                "Choose CSV/TSV File",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "No file selected"
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
