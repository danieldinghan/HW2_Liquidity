# HW2 Liquidity

股票流動性組作業，TEJ 每日資料期間為 2023-10-11 至 2026-10-08，最終 500 家公司。

## 閱讀

- [HW2.md](HW2.md)：Q1–Q6 正文、公式、圖表及結論。
- [結果說明](results_tej_r/README.md)：選樣、單位及結果檔閱讀方式。
- [Q6 月度策略](results_tej_r/q6_search/README.md)：四個純流動性策略與 TAIEX。

Q1 包含 Kyle 風險代理、Amihud、30 日 Modified、成交量及週轉率。Q3–Q5 使用四項每日指標，風險為公司全期標準差。Q6 使用截至前日的 60 日 Amihud 平均或標準差，每月選 10／30 家等權持有，不加入動能。

## 重跑

只需 R 內建功能。TEJ 原始資料受使用權限制，不隨儲存庫提供；請自行取得同欄位、UTF-16LE、tab 分隔的檔案。

在專案根目錄的 R 工作階段執行：

```r
Sys.setlocale("LC_CTYPE", "English_United States.utf8") # Windows
Sys.setenv(TEJ_DATA_FILE = "D:/your-data/tej_daily.csv")
source("homework_TEJ.R", encoding = "UTF-8")
source("analysis/test_tej.R", encoding = "UTF-8")
```

也可將原始檔放在 `data/tej_daily.csv`，此檔已由 .gitignore 排除。完整入口會重算結果與正文。只重算 Q6 可執行 `source("analysis/strategy_search.R", encoding = "UTF-8")`；只更新 Q1–Q5 正文可執行 `source("analysis/report_tej.R", encoding = "UTF-8")`。

Python 獨立核對 Q6：在同樣設有 TEJ_DATA_FILE 環境變數的終端執行 `python analysis/test_strategy_search.py`，只用標準函式庫。

## 檔案範圍與限制

本儲存庫只含目前使用的程式、報告、摘要表、圖表、Q6 每日績效與持股，以及官方 TAIEX 資料。不含教授講義、TEJ 原始資料或舊版本。Q3 的大型每日分組中間檔 `q3_returns_daily.csv` 由主程式重建，不另外上傳。

Q6 是看過測試期後的探索；形成期選定者與測試期最佳者分開標示。母樣本有期末選樣與存續偏誤，交易成本為簡化模型；TEJ 配息口徑尚待確認，TAIEX 是不扣成本且不含股利再投資的價格指數。不得將這段回測當成未來必勝的證據。
