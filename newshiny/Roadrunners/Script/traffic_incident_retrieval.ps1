$headers = @{}
$headers.Add("AccountKey","ix23KZcYSeiEbkqWoH2mGQ==")
$headers.Add("Accept","application/json")
$uri = 'http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents'

$timer = (Get-Date -Format yyy-mm-dd-hhmm)
$filepath = "C:\temp\" + $timer + ".csv"

$data = Invoke-RestMethod -Method Get -Uri $uri -Headers $headers
$data.value | Export-Csv -Append -Path $filepath