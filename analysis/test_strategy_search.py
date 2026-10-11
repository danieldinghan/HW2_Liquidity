"""Independently reconstruct every monthly portfolio from raw TEJ and holdings."""
import csv
import os
import math
import statistics
from collections import defaultdict
from pathlib import Path

root = Path(__file__).resolve().parents[1]
out = root / "results_tej_r" / "q6_search"


def read(path):
    with path.open(encoding="utf-8-sig", newline="") as file:
        return list(csv.DictReader(file))


codes = sorted(row["code"] for row in read(root / "results_tej_r" / "sample.csv"))
code_set = set(codes)
raw = defaultdict(dict)
with Path(os.environ.get("TEJ_DATA_FILE", "data/tej_daily.csv")).open(
    encoding="utf-16", newline=""
) as file:
    for row in csv.DictReader(file, delimiter="\t"):
        code = row["證券代碼"].split()[0]
        if code not in code_set:
            continue
        date = row["年月日"]
        date = f"{date[:4]}-{date[4:6]}-{date[6:]}"
        ret = float(row["報酬率％"]) / 100
        value = float(row["成交值(千元)"]) / 1000
        raw[date][code] = (ret, abs(ret) / value if value > 0 else math.nan)
dates = sorted(raw)
positions = {date: t for t, date in enumerate(dates)}
holdings = defaultdict(dict)
for row in read(out / "holdings.csv"):
    assert row["signal_end"] < row["trade_date"]
    holdings[(row["strategy"], row["trade_date"])][row["code"]] = float(row["weight"])
daily = defaultdict(list)
for row in read(out / "daily_performance.csv"):
    daily[row["strategy"]].append(row)
index = {row["date"]: float(row["close"]) for row in read(root / "data" / "taiex_daily.csv")}
comparison = read(out / "comparison.csv")
assert len(comparison) == 4
assert {row["id"] for row in comparison} == {"S01", "S02", "S05", "S06"}
assert all("momentum" not in row for row in comparison)
for config in comparison:
    strategy = config["id"]
    series = daily[strategy]
    weights, wealth, previous = {}, 1.0, 1.0
    for k, row in enumerate(series):
        date = row["date"]
        if k:
            growth = sum(w * (1 + raw[date][code][0]) for code, w in weights.items())
            wealth *= growth
            weights = {code: w * (1 + raw[date][code][0]) / growth for code, w in weights.items()}
        if (strategy, date) in holdings:
            target = holdings[(strategy, date)]
            t = positions[date]
            scores = {}
            for code in codes:
                window = [raw[dates[j]][code][1] for j in range(t - 60, t)]
                valid = [x for x in window if math.isfinite(x)]
                if len(valid) < 40:
                    continue
                scores[code] = statistics.mean(valid) if config["filter"] == "level" else statistics.stdev(valid)
            expected = sorted(scores, key=lambda code: (scores[code], code))[:int(config["holdings"])]
            assert set(expected) == set(target), (strategy, date, "signal selection")
            assert abs(sum(target.values()) - 1) < 1e-12
            turnover = sum(abs(target.get(code, 0) - weights.get(code, 0)) for code in set(target) | set(weights))
            wealth *= 1 - .003 * turnover
            weights = target
        if k == len(series) - 1:
            wealth *= .997
        assert abs(wealth - float(row["net"])) < 1e-9, (strategy, date, "wealth")
        assert abs(wealth / previous - 1 - float(row["daily_return"])) < 1e-10
        previous = wealth
    assert abs(wealth - 1 - float(config["cumulative_net"])) < 1e-9
    assert abs(index[series[-1]["date"]] / index[series[0]["date"]] - 1 - float(config["benchmark_return"])) < 1e-10
formation = read(out / "formation.csv")
selected = max(formation, key=lambda row: float(row["information_ratio"]))["id"]
assert [row["id"] for row in comparison if row["selected_in_formation"] == "TRUE"] == [selected]
plotted = read(out / "plotted_wealth.csv")
assert set(plotted[0]) == {"date", "S01", "S02", "S05", "S06", "benchmark"}
assert len(plotted) == len(daily[selected])
for k, row in enumerate(plotted):
    for strategy in daily:
        assert row["date"] == daily[strategy][k]["date"]
        assert abs(float(row[strategy]) - float(daily[strategy][k]["net"])) < 1e-12
    assert abs(float(row["benchmark"]) - index[row["date"]] / index[plotted[0]["date"]]) < 1e-12
hw = (root / "HW2.md").read_text(encoding="utf-8")
q6 = hw[hw.index("## Q6") :]
assert "q6_search/comparison.png" in q6 and "/q6/comparison.png" not in q6
assert "Q6 延伸" not in q6 and "210.50%" in q6
print("PASS: 4 pure liquidity strategies; raw-data selection, lagged trades, daily wealth, costs, benchmark and formation choice.")
print("PASS: all five plotted curves match daily results and index; Q6 uses only the monthly strategy report.")
