# Shared daily inputs and summary helpers for the monthly Q6 strategies.
options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- local({
  files <- vapply(sys.frames(), function(f) if (is.null(f$ofile)) "" else f$ofile, character(1))
  script <- tail(files[nzchar(files)], 1)
  if (!length(script)) script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  if (length(script)) dirname(dirname(normalizePath(script, winslash = "/"))) else getwd()
})
sample <- read.csv(file.path(root, "results_tej_r", "sample.csv"),
  colClasses = c(code = "character"), fileEncoding = "UTF-8-BOM")
index <- read.csv(file.path(root, "data", "taiex_daily.csv"), fileEncoding = "UTF-8-BOM")
index$date <- as.Date(index$date)
index <- index[order(index$date), ]
stopifnot(!anyDuplicated(index$date), all(is.finite(index$close) & index$close > 0))
raw <- read.delim(Sys.getenv("TEJ_DATA_FILE", unset = "data/tej_daily.csv"),
  fileEncoding = "UTF-16LE", colClasses = "character", na.strings = c("", "."), check.names = FALSE)
code <- sub("[[:space:]].*", "", trimws(raw[["證券代碼"]]))
keep <- code %in% sample$code
d <- data.frame(code = code[keep], date = as.Date(raw[["年月日"]][keep], "%Y%m%d"),
  r = as.numeric(raw[["報酬率％"]][keep]) / 100,
  value = as.numeric(raw[["成交值(千元)"]][keep]) / 1000)
rm(raw, code)
d <- d[order(d$date, d$code), ]
stopifnot(!anyDuplicated(d[c("code", "date")]), all(is.finite(d$r)), all(d$r > -1))
dates <- sort(unique(d$date))
stopifnot(identical(dates, index$date[index$date >= min(dates) & index$date <= max(dates)]),
  nrow(d) == 500 * length(dates), all(table(d$code) == length(dates)))
d$amihud <- ifelse(is.finite(d$value) & d$value > 0, abs(d$r) / d$value, NA_real_)
train <- d[d$date <= as.Date("2024-10-10"), ]
test <- d[d$date > as.Date("2024-10-10"), ]
cost <- .003
avg <- function(x) mean(x[is.finite(x)])
std <- function(x) sd(x[is.finite(x)])
pct <- function(x) sprintf("%.2f%%", 100 * x)
save_table <- function(x, name) {
  con <- file(file.path(out, name), "wt", encoding = "UTF-8")
  on.exit(close(con))
  writeLines("\ufeff", con, sep = "", useBytes = TRUE)
  write.csv(x, con, row.names = FALSE, na = "")
}
mean_test <- function(x, lag = 5) {
  n <- length(x); u <- x - mean(x)
  variance_sum <- sum(u^2)
  for (k in 1:min(lag, n - 1)) {
    variance_sum <- variance_sum + 2 * (1 - k / (lag + 1)) * sum(u[(k + 1):n] * u[1:(n - k)])
  }
  se <- sqrt(max(0, variance_sum) * n / (n - 1)) / n
  c(mean = mean(x), se = se, p = 2 * pt(-abs(mean(x) / se), df = n - 1))
}
