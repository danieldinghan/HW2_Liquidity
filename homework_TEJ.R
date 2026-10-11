rm(list=ls())
# HW2 Liquidity: current TEJ data; Q6 compares monthly pure liquidity strategies.
# Run with source("FULL_PATH/homework_TEJ.R", encoding = "UTF-8").
# Sample: complete daily price histories, top 750 by final-day market cap,
# then the largest 100 in each consecutive block of 150 (500 in total).
# Fixed size groups: 1 smallest, 5 largest. Industry is fixed at the final date.
# Daily liquidity risk = each company's full-period SD, not rolling risk.
# Roll spread uses 30 closing prices and is updated daily.
# Kyle noise-risk proxy = full-period SD of daily volume (million shares).
# Kyle asset-risk proxy = full-period SD of daily decimal returns.
# Lecture p18: lambda proxy = sigma_a^2 / sigma_u^2.
# Kyle is constant per company; Q3-Q5 retain the four daily measures.
# Modified follows the lecture provisionally.
options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- local({
  files <- vapply(sys.frames(), function(f) if (is.null(f$ofile)) "" else f$ofile, character(1))
  file <- tail(files[nzchar(files)], 1)
  if (!length(file)) file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  if (length(file) && file.exists(file)) dirname(normalizePath(file, winslash = "/")) else getwd()
})
out <- file.path(root, "results_tej_r")
dir.create(out, showWarnings = FALSE)
save_table <- function(x, file, row.names = FALSE) {
  con <- file(file.path(out, file), open = "wt", encoding = "UTF-8")
  on.exit(close(con))
  # Excel uses the BOM to recognize UTF-8 Chinese headers on direct open.
  writeLines("\ufeff", con, sep = "", useBytes = TRUE)
  write.csv(x, con, row.names = row.names, na = "")
}
table_out <- function(x, digits = 4) print(x, digits = digits, row.names = FALSE)
p_fmt <- function(x) ifelse(x < .001, "<0.001", formatC(x, format = "f", digits = 3))
avg <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
std <- function(x) if (sum(is.finite(x)) > 1) sd(x[is.finite(x)]) else NA_real_
quintile <- function(x) ceiling(5 * rank(x, ties.method = "first", na.last = "keep") / sum(is.finite(x)))
measures <- c("amihud", "modified", "volume", "turnover")
labels <- c("Amihud", "Modified*", "Volume", "Turnover")
colors <- c("#007F73", "#497BB5", "#9972AD", "#DA9840", "#C94F59")

# data
data_file <- Sys.getenv("TEJ_DATA_FILE", unset = "data/tej_daily.csv")
raw <- read.delim(data_file, fileEncoding = "UTF-16LE", colClasses = "character",
                  na.strings = c("", "."), check.names = FALSE)
required <- c("證券代碼", "年月日", "收盤價(元)", "成交值(千元)", "報酬率％",
              "週轉率％", "成交量(千股)", "市值(百萬元)", "股價淨值比-TSE", "TSE 產業別")
stopifnot(all(required %in% names(raw)))
d <- data.frame(
  code = sub("[[:space:]].*", "", trimws(raw[["證券代碼"]])),
  date = as.Date(raw[["年月日"]], "%Y%m%d"),
  close = as.numeric(raw[["收盤價(元)"]]),
  value = as.numeric(raw[["成交值(千元)"]]) / 1000,
  r = as.numeric(raw[["報酬率％"]]) / 100,
  turnover = as.numeric(raw[["週轉率％"]]) / 100,
  volume = as.numeric(raw[["成交量(千股)"]]) / 1000,
  cap = as.numeric(raw[["市值(百萬元)"]]),
  pb = as.numeric(raw[["股價淨值比-TSE"]]),
  industry = trimws(raw[["TSE 產業別"]]))
stopifnot(!anyNA(d$date), !anyDuplicated(d[c("code", "date")]))
all_dates <- sort(unique(d$date))
# A company must appear on every sample trading day with a valid closing price.
# This also rejects incomplete histories; missing rows alone do not prove delisting.
coverage <- do.call(rbind, lapply(split(d, d$code), function(z) {
  data.frame(code = z$code[1], first = min(z$date), last = max(z$date),
             days = nrow(z), valid_prices = sum(is.finite(z$close) & z$close > 0))
}))
coverage$eligible <- coverage$days == length(all_dates) &
                     coverage$valid_prices == length(all_dates)
save_table(coverage, "coverage.csv")
eligible <- coverage$code[coverage$eligible]
selection_date <- max(d$date)
pool <- d[d$date == selection_date & d$code %in% eligible &
          grepl("^[0-9]{4}$", d$code) & is.finite(d$cap) & d$cap > 0, ]
pool <- pool[order(-pool$cap, pool$code), ]
stopifnot(nrow(pool) >= 750)
pool <- head(pool, 750)
pool$cap_rank <- seq_len(750)
pool$selection_group <- rep(1:5, each = 150)
save_table(pool[c("code", "cap", "cap_rank", "selection_group")], "top750.csv")
sample <- pool[unlist(lapply(1:5, function(g) head(which(pool$selection_group == g), 100))),
               c("code", "cap", "industry", "cap_rank", "selection_group")]
stopifnot(nrow(sample) == 500, all(is.finite(sample$cap) & sample$cap > 0),
          all(!is.na(sample$industry) & nzchar(sample$industry)))
sample <- sample[order(sample$cap, sample$code), ]
sample$size <- rep(1:5, each = 100)
d <- d[d$code %in% sample$code, ]
d$size <- sample$size[match(d$code, sample$code)]
d$industry <- sample$industry[match(d$code, sample$code)]
d <- d[order(d$code, d$date), ]; rownames(d) <- NULL
company_rows <- split(seq_len(nrow(d)), d$code)
date_rows <- split(seq_len(nrow(d)), d$date)
market_day <- match(d$date, sort(unique(d$date)))
stopifnot(all(lengths(company_rows) == length(all_dates)),
          nrow(d) == 500 * length(all_dates))
rm(raw, pool)
table_out(data.frame(起日 = min(d$date), 迄日 = max(d$date), 公司數 = length(company_rows),
                     交易日數 = length(date_rows), 觀察數 = nrow(d)))

# measures
d$amihud <- ifelse(d$value > 0 & is.finite(d$r), abs(d$r) / d$value, NA_real_)
modified <- roll_cov <- rep(NA_real_, nrow(d))
window <- 30
for (rows in company_rows) {
  if (length(rows) < window) next
  for (end in seq.int(window, length(rows))) {
    days <- rows[(end - window + 1):end]
    p <- d$close[days]; value <- d$value[days]
    if (!all(diff(market_day[days]) == 1) || !all(is.finite(p) & p > 0) ||
        !all(is.finite(value) & value >= 0)) next
    change <- diff(p)
    cv <- cov(change[-1], head(change, -1))
    roll_cov[rows[end]] <- cv
    if (cv < 0 && mean(value) > 0) modified[rows[end]] <- .5 * sqrt(-cv) / mean(value)
  }
}
d$modified <- modified; d$roll_cov <- roll_cov
firm <- aggregate(d[c(measures, "cap", "close", "pb")], list(code = d$code), avg)
firm$size <- sample$size[match(firm$code, sample$code)]
firm$industry <- sample$industry[match(firm$code, sample$code)]
firm$return_sd <- unname(tapply(d$r, d$code, std)[firm$code])
for (v in measures) firm[[paste0(v, "_risk")]] <- unname(tapply(d[[v]], d$code, std)[firm$code])
# Agreed proxy, not separately identified noise-trader order flow.
firm$kyle_sigma_u_proxy <- firm$volume_risk
firm$kyle_sigma_a_proxy <- firm$return_sd
firm$kyle <- ifelse(firm$kyle_sigma_u_proxy > 0,
                    firm$kyle_sigma_a_proxy^2 / firm$kyle_sigma_u_proxy^2, NA_real_)
d$kyle <- firm$kyle[match(d$code, firm$code)]
q1_measures <- c(measures, "kyle")
q1_labels <- c(labels, "Kyle*")

# correlation
cm <- diag(1, 5); dimnames(cm) <- list(q1_labels, q1_labels)
for (pair in combn(1:5, 2, simplify = FALSE)) {
  rho <- vapply(date_rows, function(rows) {
    x <- d[[q1_measures[pair[1]]]][rows]; y <- d[[q1_measures[pair[2]]]][rows]
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
    cor(x[ok], y[ok], method = "spearman")
  }, numeric(1))
  cm[pair[1], pair[2]] <- cm[pair[2], pair[1]] <- avg(rho)
}
table_out(cm, 3)

# size-table
size_table <- aggregate(firm[q1_measures], list(Size = firm$size), median, na.rm = TRUE)
size_table$turnover <- size_table$turnover * 100
names(size_table) <- c("市值組", "Amihud", "Modified*", "成交量（百萬股）", "週轉率（%）", "Kyle*")
table_out(size_table, 6)

# size-plot
png(file.path(out, "size-plot_%02d.png"), width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 3), mar = c(5, 5, 3, 1))
for (j in 1:5) {
  y <- firm[[q1_measures[j]]]
  y <- if (q1_measures[j] == "turnover") y * 100 else log10(y)
  ok <- is.finite(y)
  boxplot(y[ok] ~ factor(firm$size[ok], levels = 1:5), col = colors, outline = FALSE,
          main = q1_labels[j], xlab = "Size (1 smallest, 5 largest)",
          ylab = if (j == 4) "Daily turnover (%)" else if (j == 5) "log10(Kyle proxy)" else "log10(company daily mean)")
}
dev.off()

# characteristics-plot
png(file.path(out, "characteristics-plot_%02d.png"), width = 1500, height = 1200, res = 150)
par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
for (v in c("cap", "return_sd", "close", "pb")) {
  x <- log10(firm[[v]]); y <- log10(firm$amihud)
  ok <- is.finite(x) & is.finite(y)
  plot(x[ok], y[ok], pch = 16, col = adjustcolor(colors[firm$size[ok]], .6),
       xlab = paste0("log10(", v, ")"), ylab = "log10(Amihud)")
  if (sum(ok) > 2) lines(lowess(x[ok], y[ok]), lwd = 2)
}
dev.off()

# industry-plot
png(file.path(out, "industry-plot_%02d.png"), width = 1500, height = 1200, res = 150)
industry <- aggregate(amihud ~ industry, firm, median)
industry$n <- as.integer(table(firm$industry)[industry$industry])
industry <- industry[industry$n >= 10, ]; industry <- industry[order(industry$amihud), ]
par(mfrow = c(1, 1), mar = c(5, 7, 3, 2))
plot(log10(industry$amihud), seq_len(nrow(industry)), pch = 16, yaxt = "n",
     xlab = "log10(industry median Amihud)", ylab = "", col = colors[1])
axis(2, seq_len(nrow(industry)), paste0("TSE ", industry$industry, " (n=", industry$n, ")"), las = 1)
dev.off()

# return-groups
groups <- list()
for (v in measures) {
  risk <- firm[[paste0(v, "_risk")]]
  risk_bin <- ave(risk, firm$size, FUN = quintile)
  for (day in names(date_rows)) for (kind in c("level", "risk")) {
    rows <- date_rows[[day]]
    z <- d[rows, c("code", "size", "r")]
    x <- if (kind == "level") d[[v]][rows] else risk[match(z$code, firm$code)]
    if (kind == "level" && v %in% c("volume", "turnover")) x <- -x
    z$x <- x; z <- z[is.finite(z$r) & is.finite(z$x), ]
    if (!nrow(z)) next
    z$bin <- if (kind == "level") ave(z$x, z$size, FUN = quintile) else risk_bin[match(z$code, firm$code)]
    a <- aggregate(r ~ size + bin, z, mean)
    a$date <- day; a$measure <- v; a$kind <- kind
    groups[[length(groups) + 1]] <- a
  }
}
daily_groups <- do.call(rbind, groups)
groups <- aggregate(r ~ size + bin + measure + kind, daily_groups, mean)
risk_table <- aggregate(firm[paste0(measures, "_risk")], list(Size = firm$size), median, na.rm = TRUE)
risk_table$turnover_risk <- risk_table$turnover_risk * 100
names(risk_table) <- c("市值組", "Amihud 風險", "Modified* 風險", "成交量風險（百萬股）", "週轉率風險（百分點）")
table_out(risk_table, 6)

# returns-plot
png(file.path(out, "returns-plot_%02d.png"), width = 1500, height = 1200, res = 150)
for (kind in c("level", "risk")) {
  par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
  for (j in 1:4) {
    z <- groups[groups$measure == measures[j] & groups$kind == kind, ]
    plot(1:5, rep(0, 5), type = "n", ylim = range(c(0, z$r * 100)),
         xlab = if (kind == "level") "Illiquidity group (low to high)" else "Risk group (low to high)",
         ylab = "Mean same-day return (%)", main = paste(labels[j], kind))
    abline(h = 0, lty = 2, col = "grey70")
    for (g in 1:5) {
      zz <- z[z$size == g, ]; zz <- zz[order(zz$bin), ]
      lines(zz$bin, zz$r * 100, type = "b", col = colors[g], pch = 16)
    }
    if (j == 1) legend("topleft", paste("Size", 1:5), col = colors, lty = 1, cex = .7, bty = "n")
  }
}
dev.off()

# regression
cluster_table <- function(fit, z) {
  X <- model.matrix(fit); score <- X * residuals(fit)
  n <- nrow(X); k <- ncol(X)
  meat_by <- function(group) {
    sums <- rowsum(score, group, reorder = FALSE); g <- nrow(sums)
    crossprod(sums) * g / (g - 1) * (n - 1) / (n - k)
  }
  # Inclusion-exclusion prevents counting each individual observation twice.
  meat <- meat_by(z$code) + meat_by(z$date) - crossprod(score) * n / (n - k)
  bread <- solve(crossprod(X))
  se <- sqrt(pmax(0, diag(bread %*% meat %*% bread)))
  df <- min(length(unique(z$code)), length(unique(z$date))) - 1
  data.frame(term = names(coef(fit)), estimate = unname(coef(fit)), se = se,
             p = 2 * pt(-abs(coef(fit) / se), df), n = n)
}
# Market return at t uses market-cap weights from trading day t-1.
d$lagcap <- ave(d$cap, d$code, FUN = function(x) c(NA_real_, head(x, -1)))
market <- vapply(date_rows, function(rows) {
  z <- d[rows, ]; ok <- is.finite(z$r) & is.finite(z$lagcap) & z$lagcap > 0
  if (any(ok)) weighted.mean(z$r[ok], z$lagcap[ok]) * 100 else NA_real_
}, numeric(1))
d$market <- unname(market[as.character(d$date)])
# Contemporaneous regressions: current cap and down_t, not lagged controls.
d$down <- as.integer(d$market < 0)
d$return_pct <- d$r * 100
d$logcap <- ifelse(d$cap > 0, log(d$cap), NA_real_)
regressions <- states <- list()
for (v in measures) for (kind in c("level", "risk")) {
  z <- d[c("code", "date", "return_pct", "logcap", "industry", "market", "down")]
  z$x <- if (kind == "level") d[[v]] else firm[[paste0(v, "_risk")]][match(d$code, firm$code)]
  z <- z[complete.cases(z) & is.finite(z$x), ]
  fit <- lm(return_pct ~ x + logcap + factor(industry) + market, data = z)
  a <- cluster_table(fit, z); a$measure <- v; a$kind <- kind
  regressions[[paste(v, kind)]] <- a
  if (kind == "risk") {
    fit_down <- lm(return_pct ~ x * down + logcap + factor(industry) + market, data = z)
    b <- cluster_table(fit_down, z); delta <- b[b$term == "x:down", ]
    states[[v]] <- data.frame(measure = v, up = coef(fit_down)["x"],
                              down = coef(fit_down)["x"] + delta$estimate,
                              difference = delta$estimate, p = delta$p, n = nrow(z))
    rm(fit_down)
  }
  rm(fit); invisible(gc())
}
regressions <- do.call(rbind, regressions)
states <- do.call(rbind, states)
q4 <- regressions[regressions$term == "x", ]
q4_table <- data.frame(指標 = labels[match(q4$measure, measures)],
                       模型 = ifelse(q4$kind == "level", "當日流動性", "公司流動性風險"),
                       係數 = q4$estimate, p值 = p_fmt(q4$p), 觀察數 = q4$n)
table_out(q4_table, 6)

# market-states
q5_table <- data.frame(指標 = labels[match(states$measure, measures)],
                       上漲日係數 = states$up, 下跌日係數 = states$down,
                       差異 = states$difference, p值 = p_fmt(states$p), 觀察數 = states$n)
table_out(q5_table, 6)

# Save the current results without mixing them with earlier versions.
save_table(sample, "sample.csv")
save_table(data.frame(roll_window = window, volume_unit = "million shares",
                     kyle_noise_proxy = "SD(daily trading volume), full sample",
                     kyle_asset_proxy = "SD(daily decimal return), full sample",
                     kyle_formula = "sigma_a^2 / sigma_u^2; lecture p18",
                     kyle_scope = "Q1 only; constant firm proxy has no daily time-series risk"), "settings.csv")
save_table(firm, "companies.csv")
save_table(cm, "correlation_matrix.csv", row.names = TRUE)
save_table(size_table, "q1_size.csv")
save_table(risk_table, "q3_risk.csv")
save_table(daily_groups, "q3_returns_daily.csv")
save_table(regressions, "regressions.csv")
save_table(q4_table, "q4.csv")
save_table(states, "market_states.csv")
save_table(q5_table, "q5.csv")
cat("Q1-Q5 tables and figures:", out, "\nRunning Q6...\n")
source(file.path(root, "analysis", "strategy_search.R"), encoding = "UTF-8")
source(file.path(root, "analysis", "report_tej.R"), encoding = "UTF-8")

