# Q6: four monthly pure liquidity strategies versus official TAIEX.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
base <- local({
  files <- vapply(sys.frames(), function(f) if (is.null(f$ofile)) "" else f$ofile, character(1))
  script <- tail(files[nzchar(files)], 1)
  if (!length(script)) script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  if (length(script)) dirname(dirname(normalizePath(script, winslash = "/"))) else getwd()
})
source(file.path(base, "analysis", "strategy_data.R"), encoding = "UTF-8")
out <- file.path(root, "results_tej_r", "q6_search")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dates <- sort(unique(d$date))
codes <- sort(unique(d$code))
d <- d[order(d$date, d$code), ]
returns <- matrix(d$r, nrow = length(dates), byrow = TRUE)
illiq <- matrix(d$amihud, nrow = length(dates), byrow = TRUE)
index_close <- index$close[match(dates, index$date)]

# Signals end at yesterday's close. Trades occur at today's close.
signal_at <- function(t) {
  window <- illiq[(t - 60):(t - 1), , drop = FALSE]
  valid <- colSums(is.finite(window)) >= 40
  level <- apply(window, 2, avg)
  risk <- apply(window, 2, std)
  list(level = level, risk = risk, valid = valid)
}
grid <- expand.grid(filter = c("level", "risk"), holdings = c(10, 30))
# Retain original IDs for traceability.
grid$id <- c("S01", "S02", "S05", "S06")
stopifnot(nrow(grid) == 4)

run_portfolio <- function(config, start, end, phase) {
  times <- which(dates >= start & dates <= end)
  stopifnot(min(times) > 126)
  month <- format(dates[times], "%Y-%m")
  rebalance <- c(TRUE, month[-1] != head(month, -1))
  w <- rep(0, length(codes)); wealth <- 1; total_turnover <- 0
  history <- vector("list", length(times)); trades <- list()
  for (k in seq_along(times)) {
    t <- times[k]
    if (k > 1) {
      growth <- sum(w * (1 + returns[t, ]))
      wealth <- wealth * growth
      w <- w * (1 + returns[t, ]) / growth
    }
    if (rebalance[k]) {
      sig <- signal_at(t)
      eligible <- which(sig$valid & is.finite(sig[[config$filter]]))
      chosen <- head(eligible[order(sig[[config$filter]][eligible], codes[eligible])], config$holdings)
      stopifnot(length(chosen) == config$holdings)
      target <- rep(0, length(codes)); target[chosen] <- 1 / length(chosen)
      turnover <- sum(abs(target - w))
      wealth <- wealth * (1 - cost * turnover)
      total_turnover <- total_turnover + turnover
      w <- target
      trades[[length(trades) + 1]] <- data.frame(strategy = config$id, phase = phase,
        trade_date = dates[t], signal_end = dates[t - 1], code = codes[chosen], weight = w[chosen])
    }
    if (k == length(times)) {
      wealth <- wealth * (1 - cost)
      total_turnover <- total_turnover + 1
    }
    history[[k]] <- data.frame(date = dates[t], strategy = config$id, net = wealth)
  }
  series <- do.call(rbind, history)
  series$daily_return <- series$net / c(1, head(series$net, -1)) - 1
  benchmark <- index_close[times] / index_close[times[1]]
  excess <- series$daily_return - (benchmark / c(1, head(benchmark, -1)) - 1)
  stat <- mean_test(excess)
  summary <- data.frame(strategy = config$id, phase = phase,
    cumulative_net = tail(series$net, 1) - 1, benchmark_return = tail(benchmark, 1) - 1,
    excess_cumulative = tail(series$net, 1) - tail(benchmark, 1),
    volatility = sd(series$daily_return) * sqrt(252),
    drawdown = min(c(1, series$net) / cummax(c(1, series$net)) - 1),
    information_ratio = mean(excess) / sd(excess) * sqrt(252),
    excess_daily = unname(stat["mean"]), excess_p = unname(stat["p"]),
    turnover = total_turnover)
  stopifnot(abs(prod(1 + series$daily_return) - tail(series$net, 1)) < 1e-10)
  list(summary = summary, daily = series, trades = do.call(rbind, trades))
}

# Keep the previous formation start for an unchanged performance comparison.
formation_start <- dates[127]
formation_runs <- lapply(seq_len(nrow(grid)), function(j)
  run_portfolio(grid[j, ], formation_start, max(train$date), "formation"))
formation_result <- do.call(rbind, lapply(formation_runs, `[[`, "summary"))
selected <- formation_result$strategy[which.max(formation_result$information_ratio)]
test_runs <- lapply(seq_len(nrow(grid)), function(j)
  run_portfolio(grid[j, ], min(test$date), max(test$date), "test"))
result <- do.call(rbind, lapply(test_runs, `[[`, "summary"))
result <- merge(grid, result, by.x = "id", by.y = "strategy", sort = FALSE)
result$selected_in_formation <- result$id == selected
result <- result[order(-result$cumulative_net), ]
daily <- do.call(rbind, lapply(test_runs, `[[`, "daily"))
trades <- do.call(rbind, lapply(test_runs, `[[`, "trades"))
stopifnot(all(trades$signal_end < trades$trade_date))
weight_sums <- aggregate(weight ~ strategy + trade_date, trades, sum)
stopifnot(all(abs(weight_sums$weight - 1) < 1e-12))
save_table(result, "comparison.csv")
save_table(merge(grid, formation_result, by.x = "id", by.y = "strategy"), "formation.csv")
save_table(daily, "daily_performance.csv")
save_table(trades, "holdings.csv")
winner <- result$id[1]
show_ids <- grid$id
plot_labels <- c("Low Amihud: 10 stocks", "Low liquidity risk: 10 stocks",
                 "Low Amihud: 30 stocks", "Low liquidity risk: 30 stocks")
plot_labels[show_ids == selected] <- paste0(plot_labels[show_ids == selected], " (formation choice)")
benchmark <- index_close[match(sort(unique(test$date)), dates)]
benchmark <- benchmark / benchmark[1]
curves <- cbind(sapply(show_ids, function(id) daily$net[daily$strategy == id]), benchmark)
colnames(curves) <- c(show_ids, "benchmark")
save_table(data.frame(date = sort(unique(test$date)), curves), "plotted_wealth.csv")
stopifnot(all(vapply(show_ids, function(id)
  abs(tail(curves[, id], 1) - 1 - result$cumulative_net[match(id, result$id)]) < 1e-12, logical(1))))
plot_colors <- c("#497BB5", "#DA9840", "#007F73", "#C94F59", "#555555")
png(file.path(out, "comparison.png"), width = 1600, height = 1100, res = 150)
layout(matrix(c(1, 2), ncol = 1), heights = c(.18, 1))
par(mar = c(0, 0, 0, 0)); plot.new()
legend("center", c(plot_labels, "TAIEX price index (no costs)"),
  col = plot_colors, lty = c(1, 1, 1, 1, 2), lwd = 2, bty = "n", ncol = 2, cex = .8)
par(mar = c(5, 5, 3, 1))
matplot(sort(unique(test$date)), (curves - 1) * 100, type = "l", col = plot_colors,
  lty = c(1, 1, 1, 1, 2), lwd = 2, xlab = "Date", ylab = "Cumulative return (%)",
  xaxt = "n", main = "Q6: monthly pure liquidity strategies (after costs)")
abline(h = 0, col = "grey80", lty = 3)
axis.Date(1, at = seq(min(test$date), max(test$date), by = "6 months"), format = "%Y-%m")
dev.off()

table_rows <- vapply(seq_len(nrow(result)), function(j) {
  z <- result[j, ]
  paste0("| ", z$id, if (z$selected_in_formation) "（形成期選定）" else "", " | ",
    if (z$filter == "level") "低 Amihud" else "低 Amihud 風險",
    " | ", z$holdings, " | ", pct(z$cumulative_net),
    " | ", sprintf("%.2f", 100 * z$excess_cumulative), " | ", pct(z$volatility), " | ", pct(z$drawdown), " |")
}, character(1))
rationale <- c("### 為什麼這樣建構？", "",
  "1. **直接用流動性選股。** Amihud 為每日絕對報酬除以成交金額；平均值低，代表每單位成交金額伴隨的價格變動較小，是較容易交易的代理。這是檢驗高流動性組合能否獲利，不是認定高流動性必然有高報酬；不流動性若有報酬補償，方向也可能相反。",
  "2. **另測流動性是否穩定。** 過去 60 日 Amihud 的標準差低，代表這個指標變化較小。因此另建低流動性風險組合；標準差低不保證平均 Amihud 低，也不等於股價風險低，兩種排序分開比較。",
  "3. **60 日摘要、每月換股。** 約三個月的歷史可降低單日異常成交的影響，又能隨近期流動性更新；每月而非每日換股可限制換手成本。這是簡單可解釋的設定，不是已證明最適的期間，也不同於 Modified Amihud 中 Roll 的 30 日窗口。",
  "4. **比較 10 家與 30 家，採等權。** 10 家較集中，30 家較分散，用來比較集中程度；等權避免少數大型股權重主導。不再強制五個市值組各占 20%，所以結果也可能包含市值與產業配置差異，不能全歸因於流動性。",
  "5. **只用交易前已知訊號。** 月底排序，下一個交易日收盤換股；買入日不計新持股當日報酬，之後按每日報酬累積並扣成本，再與 TAIEX 比較。目的在實測，不是先假定策略必定勝過大盤。", "")
report <- c("## Q6：純流動性月度策略", "",
  "本題改以四個純流動性月度策略作為正式比較，不再使用原本固定持有版本；不加入動能。先前已看過測試期，因此本次仍屬事後探索，不能稱為全新、未使用的樣本外驗證。", "",
  "### 策略規則", "",
  "每月第一個交易日收盤換股；訊號截至前一個交易日。過去 60 個交易日的 Amihud 平均值代表流動性，標準差代表流動性風險，至少需 40 個有效值。",
  "令 $t$ 為換股交易日，$\\mathcal W_{i,t-1}$ 為 $t-60$ 至 $t-1$ 之間 Amihud 有效的交易日集合，$n_{i,t-1}=|\\mathcal W_{i,t-1}|\\ge40$。選股訊號的下標必須是 $t-1$：", "",
  "$$\\bar A_{i,t-1}^{(60)}=\\frac{1}{n_{i,t-1}}\\sum_{s\\in\\mathcal W_{i,t-1}}A_{is},\\qquad SignalRisk_{i,t-1}^{A,(60)}=\\sqrt{\\frac{\\sum_{s\\in\\mathcal W_{i,t-1}}(A_{is}-\\bar A_{i,t-1}^{(60)})^2}{n_{i,t-1}-1}}.$ $", "",
  "這裡的 60 日風險只用到前一交易日，與 Q3–Q5 的全期 $Risk_i^{L,\\mathrm{full}}$ 不同；不是把全期風險加上 $t-1$ 下標便拿來交易。",
  "在目前 500 家母樣本中，按 Amihud 平均值或標準差由低至高排序，直接選前 10／30 家。每月換股時等權，月中權重隨股價變動；不放空、不槓桿、不加入動能，不強制五個市值組各占 20%。測試期首日另依前日訊號建立初始持股。",
  "每次換股先計算當日舊持股報酬，再於收盤調整。成本為 0.3% × 買賣權重變動絕對值合計，初始買入與期末賣出也扣成本；這是簡化的換手成本模型。TAIEX 為不扣成本、未計股利再投資的官方價格指數。策略採 TEJ 報酬，配息口徑待確認，因此不宣稱完全同口徑的總報酬比較。",
  "若 $w_{i,t-1}^{p}$ 是前一交易日收盤換股完成後的持股權重，換股前的當日組合毛報酬為：", "",
  "$$r_{p,t}^{\\mathrm{gross}}=\\sum_i w_{i,t-1}^{p}r_{it}.$ $", "",
  "$t$ 日收盤新買入的股票，從 $t+1$ 日起才貢獻報酬。首日原先持有現金，僅扣初始買入成本，不計新持股首日報酬；每日淨報酬另外反映當日換手成本，期末再扣賣出成本。",
  "指數來源：[臺灣證券交易所](https://www.twse.com.tw/zh/indices/taiex/mi-5min-hist.html)，原始資料保存在 data/taiex_monthly。", "",
  paste0("形成期：", formation_start, " 至 ", max(train$date), "。依形成期資訊比率選定 **", selected,
    "**。測試期：", min(test$date), " 至 ", max(test$date), "；全部策略採相同日期。"), "",
  rationale, "### 實測結果", "",
  paste0("TAIEX 同期累積報酬：**", pct(tail(benchmark, 1) - 1), "**。"), "",
  "| 策略 | 流動性篩選 | 持股數 | 扣成本累積報酬 | 超過大盤（百分點） | 年化波動 | 最大回撤 |",
  "|---|---|---:|---:|---:|---:|---:|", table_rows,
  paste0("| TAIEX | 大盤價格指數 | — | ", pct(tail(benchmark, 1) - 1), " | 0.00 | ",
    pct(sd(benchmark / c(1, head(benchmark, -1)) - 1) * sqrt(252)), " | ",
    pct(min(c(1, benchmark) / cummax(c(1, benchmark)) - 1)), " |"), "",
  paste0("事後測試期最佳為 **", winner, "**；形成期選定的是 **", selected, "**，必須分開解讀。超過大盤欄為兩個累積報酬相減，不是年化 alpha。"), "",
  "![績效比較](comparison.png)", "",
  "圖中四條實線為已扣成本的策略，虛線為 TAIEX。縱軸是累積報酬率，例如 210.50% 表示資金約變成 3.105 倍，不是 2.105 倍；買入日已扣初始成本，策略起點因此略低於零。所有曲線使用相同進出日期，未從績效終點回推繪圖。", "",
  paste0("形成期選定的 ", selected, " 在測試期超過大盤，但平均日超額報酬檢定 p=",
    sprintf("%.3f", result$excess_p[match(selected, result$id)]),
    "，未達 5% 顯著水準。事後最佳的低 Amihud、30 家組合不能當成事前選定；其波動與最大回撤也高於大盤。"), "",
  "### 解讀限制", "",
  "目前只比較 4 個純流動性規則，但先前曾比較其他規則的事實並未消失。仍存在挑選贏家偏誤，未做多重比較修正；新的未使用期間才可再驗證。",
  "母樣本仍按期末市值及完整三年資料選定，有存續與事後選樣偏誤；未模擬成交限制、價格衝擊及停牌，配息口徑也未確認。少量持股有集中風險，不能以累積報酬高就認定較穩健。",
  "comparison.csv 包含波動、最大回撤、資訊比率及平均日超額報酬雙尾 Newey–West 5 階 p 值；holdings.csv 可核對每次持股與訊號截止日。")
report <- gsub("\\$ \\$", "$$", report)
report <- gsub("^\\$\\$(.*)\\$\\$$", "```math\n\\1\n```", report)
report <- gsub("(?<!\\$)\\$([^$\n]+)\\$(?!\\$)", "$`\\1`$", report, perl = TRUE)
writeLines(enc2utf8(report), file.path(out, "README.md"), useBytes = TRUE)
hw <- readLines(file.path(root, "HW2.md"), encoding = "UTF-8")
q6_start <- grep("^## Q6", hw)[1]
stopifnot(is.finite(q6_start))
body_report <- report
body_report[body_report == "![績效比較](comparison.png)"] <- paste0("![績效比較](",
  "results_tej_r/q6_search/comparison.png", ")")
writeLines(enc2utf8(c(hw[seq_len(q6_start - 1)], body_report)), file.path(root, "HW2.md"), useBytes = TRUE)
cat("PASS: lagged signals, weight sums and compounded returns.\nFormation choice:", selected, "\n")
print(result)
