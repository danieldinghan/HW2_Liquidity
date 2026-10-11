# Download official monthly TAIEX closing prices; keep source responses for audit.
$ErrorActionPreference = 'Stop'
$root = Split-Path $PSScriptRoot -Parent
$data = Join-Path $root 'data'
$cache = Join-Path $data 'taiex_monthly'
New-Item -ItemType Directory -Path $cache -Force | Out-Null
$rows = @()
$month = [datetime]'2023-10-01'
while ($month -le [datetime]'2026-10-01') {
    $key = $month.ToString('yyyyMMdd')
    $url = "https://www.twse.com.tw/indicesReport/MI_5MINS_HIST?date=$key&response=json"
    $path = Join-Path $cache "$key.json"
    if (Test-Path -LiteralPath $path) {
        $json = Get-Content -LiteralPath $path -Raw -Encoding UTF8
    } else {
        $response = Invoke-WebRequest -Uri $url -TimeoutSec 60
        $json = $response.Content
    }
    $result = $json | ConvertFrom-Json
    if ($result.stat -ne 'OK' -or -not $result.data) { throw "Invalid TWSE response: $key" }
    [System.IO.File]::WriteAllText($path, $json, [System.Text.UTF8Encoding]::new($false))
    foreach ($row in $result.data) {
        $parts = $row[0].Split('/')
        $date = [datetime]::new(([int]$parts[0] + 1911), [int]$parts[1], [int]$parts[2])
        $close = [double]::Parse($row[4].Replace(',', ''), [System.Globalization.CultureInfo]::InvariantCulture)
        if ($close -le 0) { throw "Invalid TAIEX close: $date" }
        $rows += [pscustomobject]@{ date = $date.ToString('yyyy-MM-dd'); close = $close; source_url = $url }
    }
    Write-Output "TAIEX $key OK"
    $month = $month.AddMonths(1)
    Start-Sleep -Milliseconds 300
}
$rows = $rows | Sort-Object date
if (($rows | Group-Object date | Where-Object Count -gt 1).Count -gt 0) { throw 'Duplicate TAIEX dates' }
$rows | Export-Csv -LiteralPath (Join-Path $data 'taiex_daily.csv') -NoTypeInformation -Encoding utf8BOM
Write-Output "Saved $($rows.Count) official index dates to data/taiex_daily.csv"
