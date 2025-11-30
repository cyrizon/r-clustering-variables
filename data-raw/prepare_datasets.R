## Script to convert raw dataset files in `datasets/` into package data (data/*.rda)
## Run this script from the package root (project directory).

if (!dir.exists("data")) dir.create("data")

# Helper to try reading a file with sensible defaults
read_maybe <- function(path) {
    # Try data.table::fread first (it auto-detects separators and headers)
    if (requireNamespace("data.table", quietly = TRUE)) {
        res <- tryCatch(
            {
                data.table::fread(path, encoding = "UTF-8", data.table = FALSE)
            },
            error = function(e) NULL
        )
        if (!is.null(res)) {
            message("  -> read with data.table::fread()")
            return(as.data.frame(res, stringsAsFactors = FALSE))
        }
    }

    # If fread not available or failed, try common separators
    seps <- c(",", ";", "\t", "|")
    for (s in seps) {
        df <- tryCatch(
            {
                utils::read.table(path, header = TRUE, sep = s, stringsAsFactors = FALSE, fill = TRUE, check.names = FALSE)
            },
            error = function(e) NULL
        )
        if (!is.null(df)) {
            # If header produced fewer names than columns, try to re-read forcing fill
            if (ncol(df) >= 1) {
                message(sprintf("  -> read with sep='%s' (header=TRUE)", s))
                return(as.data.frame(df, stringsAsFactors = FALSE))
            }
        }
    }

    # Last resort: read without header and create synthetic names
    df_noheader <- tryCatch(
        {
            utils::read.table(path, header = FALSE, sep = "\t", stringsAsFactors = FALSE, fill = TRUE, check.names = FALSE)
        },
        error = function(e) NULL
    )
    if (is.null(df_noheader)) {
        df_noheader <- tryCatch(
            {
                utils::read.table(path, header = FALSE, stringsAsFactors = FALSE, fill = TRUE, check.names = FALSE)
            },
            error = function(e) NULL
        )
    }
    if (!is.null(df_noheader)) {
        nc <- ncol(df_noheader)
        colnames(df_noheader) <- paste0("V", seq_len(nc))
        message("  -> read without header, synthetic column names created")
        return(as.data.frame(df_noheader, stringsAsFactors = FALSE))
    }

    # Give up
    return(NULL)
}

# List of source files (relative to package root)
files <- list(
    acp_voitures = "datasets/acp_voitures.csv",
    AUTOS2005subset = "datasets/AUTOS2005subset.txt",
    jobrate = "datasets/jobrate.csv",
    protein_dataset = "datasets/protein_dataset.csv",
    vehicle = "datasets/vehicle.csv",
    vote_catvarclus = "datasets/vote_catvarclus.csv",
    tea_data = "datasets/tea_data.csv"
)

for (nm in names(files)) {
    path <- files[[nm]]
    if (!file.exists(path)) {
        message(sprintf("Skipping %s: file not found (%s)", nm, path))
        next
    }
    message(sprintf("Reading %s -> %s", path, nm))
    df <- read_maybe(path)
    if (is.null(df)) {
        message(sprintf("Failed to read %s", path))
        next
    }
    assign(nm, df)
    save(list = nm, file = file.path("data", paste0(nm, ".rda")), compress = "xz")
    message(sprintf("Saved data/%s.rda", nm))
}

message("Done. Run devtools::document() to generate documentation and NAMESPACE.")
