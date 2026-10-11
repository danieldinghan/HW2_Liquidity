# Independent checks after running homework_TEJ.R. Run from the project folder.
options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
out <- "results_tej_r"
read_result <- function(file) read.csv(file.path(out, file), fileEncoding = "UTF-8-BOM")
sample <- read_result("sample.csv")
top <- read_result("top750.csv")
coverage <- read_result("coverage.csv")
firm <- read_result("companies.csv")
reg <- read_result("regressions.csv")
state <- read_result("market_states.csv")
settings <- read_result("settings.csv")
stopifnot(settings$roll_window == 30)
raw <- read.delim(Sys.getenv("TEJ_DATA_FILE", unset = "data/tej_daily.csv"),
                  fileEncoding = "UTF-16LE", colClasses = "character", na.strings = c("", "."), check.names = FALSE)
code <- sub("[[:space:]].*", "", trimws(raw[["證券代碼"]]))
date <- as.Date(raw[["年月日"]], "%Y%m%d")
close <- as.numeric(raw[["收盤價(元)"]])
days <- sort(unique(date))
counts <- table(code)
valid <- tapply(is.finite(close) & close > 0, code, sum)
eligible <- names(counts)[counts == length(days) & valid[names(counts)] == length(days)]
last <- which(date == max(days) & code %in% eligible & grepl("^[0-9]{4}$", code))
cap <- as.numeric(raw[["市值(百萬元)"]][last])
ok <- is.finite(cap) & cap > 0
last <- last[ok]; cap <- cap[ok]
ordered <- last[order(-cap, code[last])]
expected_top <- head(code[ordered], 750)
expected_rank <- unlist(lapply(0:4, function(g) g * 150 + 1:100))
stopifnot(identical(as.character(top$code), expected_top), nrow(sample) == 500,
          setequal(as.character(sample$code), expected_top[expected_rank]),
          all(table(sample$size) == 100), all(table(sample$selection_group) == 100),
          all(sample$cap_rank %in% expected_rank),
          all(coverage$eligible == (coverage$days == length(days) & coverage$valid_prices == length(days))))
z <- data.frame(code = code, date = date, r = as.numeric(raw[["報酬率％"]]) / 100,
                cap = as.numeric(raw[["市值(百萬元)"]]),
                turnover = as.numeric(raw[["週轉率％"]]) / 100)
z <- z[z$code %in% sample$code, ]; z <- z[order(z$code, z$date), ]
stopifnot(nrow(z) == 500 * length(days), all(table(z$code) == length(days)))
risk <- tapply(z$turnover, z$code, sd, na.rm = TRUE)
stopifnot(isTRUE(all.equal(firm$turnover_risk, as.numeric(risk[as.character(firm$code)]))))
volume <- as.numeric(raw[["成交量(千股)"]]) / 1000
volume_sd <- tapply(volume, code, sd, na.rm = TRUE)
stopifnot(isTRUE(all.equal(firm$kyle_sigma_u_proxy,
                          as.numeric(volume_sd[as.character(firm$code)]))))
return_sd <- tapply(as.numeric(raw[["報酬率％"]]) / 100, code, sd, na.rm = TRUE)
expected_a <- as.numeric(return_sd[as.character(firm$code)])
stopifnot(isTRUE(all.equal(firm$kyle_sigma_a_proxy, expected_a)),
          isTRUE(all.equal(firm$kyle, expected_a^2 / firm$kyle_sigma_u_proxy^2)))
cm <- read.csv(file.path(out, "correlation_matrix.csv"), fileEncoding = "UTF-8-BOM", row.names = 1, check.names = FALSE)
stopifnot(nrow(cm) == 5, ncol(cm) == 5, "Kyle*" %in% names(cm))
# Independently reconstruct the 30-day Modified estimates for three firms.
for (id in as.character(head(firm$code, 3))) {
  rows <- which(code == id); rows <- rows[order(date[rows])]
  p <- close[rows]; value <- as.numeric(raw[["成交值(千元)"]][rows]) / 1000
  m <- rep(NA_real_, length(rows))
  for (t in 30:length(rows)) {
    w <- (t - 29):t
    if (!all(is.finite(p[w]) & p[w] > 0 & is.finite(value[w]) & value[w] >= 0)) next
    delta <- diff(p[w]); cv <- cov(head(delta, -1), tail(delta, -1))
    if (cv < 0 && mean(value[w]) > 0) m[t] <- .5 * sqrt(-cv) / mean(value[w])
  }
  j <- match(id, firm$code)
  stopifnot(isTRUE(all.equal(firm$modified[j], mean(m, na.rm = TRUE))),
            isTRUE(all.equal(firm$modified_risk[j], sd(m, na.rm = TRUE))))
}
z$lagcap <- ave(z$cap, z$code, FUN = function(x) c(NA_real_, head(x, -1)))
market <- tapply(seq_len(nrow(z)), z$date, function(rows) {
  a <- z[rows, ]; ok <- is.finite(a$r) & is.finite(a$lagcap) & a$lagcap > 0
  if (any(ok)) sum(a$r[ok] * a$lagcap[ok]) / sum(a$lagcap[ok]) * 100 else NA_real_
})
z$market <- unname(market[as.character(z$date)])
z$industry <- sample$industry[match(z$code, sample$code)]
z$return_pct <- z$r * 100; z$logcap <- log(z$cap)
fit <- lm(return_pct ~ turnover + logcap + factor(industry) + market, data = z)
reported <- reg$estimate[reg$measure == "turnover" & reg$kind == "level" & reg$term == "x"]
stopifnot(isTRUE(all.equal(unname(coef(fit)["turnover"]), reported, tolerance = 1e-9)),
          nobs(fit) == reg$n[reg$measure == "turnover" & reg$kind == "level" & reg$term == "x"],
          sum(reg$term == "x") == 8, nrow(state) == 4,
          all(abs(state$down - state$up - state$difference) < 1e-10))
# Rebuild Q5 from current down_t, current cap and the full-period firm risk.
z$down <- as.integer(z$market < 0)
z$risk <- unname(risk[z$code])
fit_down <- lm(return_pct ~ risk * down + logcap + factor(industry) + market, data = z)
reported_state <- state[state$measure == "turnover", ]
stopifnot(abs(coef(fit_down)["risk"] - reported_state$up) < 1e-9,
          abs(coef(fit_down)["risk:down"] - reported_state$difference) < 1e-9,
          all(z$logcap == log(z$cap)),
          all(is.na(z$down) | z$down == as.integer(z$market < 0)))
# Exercise only the export helper, without rerunning the analysis.
for (e in parse("homework_TEJ.R", encoding = "UTF-8")) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(e[[2]], as.name("save_table"))) eval(e)
}
x <- read_result("q1_size.csv")
out <- tempdir()
save_table(x, "hw2_encoding_test.csv")
path <- file.path(out, "hw2_encoding_test.csv")
y <- read.csv(path, fileEncoding = "UTF-8-BOM")
stopifnot(identical(readBin(path, "raw", n = 3), as.raw(c(239, 187, 191))),
          identical(names(x), names(y)), isTRUE(all.equal(x, y)))
unlink(path)
cat("PASS: coverage, top750, 500 selections, units, 30-day Roll, Kyle proxies, risk, OLS, Q5 and CSV encoding checks.\n")
hw <- readLines("HW2.md", encoding = "UTF-8")
stopifnot(all(vapply(1:6, function(q) sum(grepl(paste0("^## Q", q, "："), hw)) == 1, logical(1))),
          !any(grepl("Q1–Q5 以下保留舊版|Kyle 與 Q2 尚待補上|2023-01-03 至 2025-12-30", hw)),
          any(grepl("364,000", hw, fixed = TRUE)), any(grepl("0.065", hw, fixed = TRUE)))
q6_start <- grep("^## Q6", hw)[1]
q6_before <- hw[q6_start:length(hw)]
source("analysis/report_tej.R", encoding = "UTF-8")
updated <- readLines("HW2.md", encoding = "UTF-8")
q6_start <- grep("^## Q6", updated)[1]
stopifnot(identical(q6_before, updated[q6_start:length(updated)]), identical(hw, updated))
stopifnot(any(grepl("Industry_{i,T}", updated, fixed = TRUE)),
          any(grepl("Risk_i^{L,\\mathrm{full}}", updated, fixed = TRUE)),
          any(grepl("MCap_{i,t-1}", updated, fixed = TRUE)),
          any(grepl("\\log(MCap_{it})", updated, fixed = TRUE)),
          any(grepl("\\bar A_{i,t-1}^{(60)}", updated, fixed = TRUE)),
          any(grepl("w_{i,t-1}^{p}r_{it}", updated, fixed = TRUE)))
cat("PASS: current Q1-Q6 report, repeatable generation and exact preservation of Q6.\n")
cat("PASS: Q4/Q5 contemporaneous controls, lagged market weights and explicit Q6 signal timing.\n")
