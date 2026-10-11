# Q6 大盤資料

- 來源：臺灣證券交易所「發行量加權股價指數歷史資料」。
- 官方頁面：https://www.twse.com.tw/zh/indices/taiex/mi-5min-hist.html
- 使用網址：`https://www.twse.com.tw/indicesReport/MI_5MINS_HIST?date=YYYYMM01&response=json`。
- `taiex_monthly/`：2023-10 至 2026-10，共 37 份官方 JSON 回應。
- `taiex_daily.csv`：依日期排序的官方收盤指數與來源網址，UTF-8 BOM；包含月份邊界額外日期，策略程式會精確對齊使用。
- 重建：在 PowerShell 執行 `analysis/fetch_taiex.ps1`。已有 JSON 優先讀取快取；需更新某月時，先確認需求再移除對應快取並重跑。

這是價格指數，不是股利再投資的報酬指數，也不是 ETF。Q6 大盤資產曲線為「當日收盤指數 ÷ 買入日收盤指數」，沒有假設可直接買入指數，也不扣指數交易成本。不得用它宣稱與股票策略完全同口徑的總報酬比較。
