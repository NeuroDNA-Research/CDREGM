param(
    [string]$Remote = "cesar@dna",
    [string]$Dest = "~"
)

Get-ChildItem -Path . -Filter *.html | ForEach-Object {
    $file = $_.Name
    Write-Host "Copying $file..."
    Get-Content $_.FullName -Raw | ssh $Remote "cat > $Dest/$file"
    if ($?) {
        Write-Host "  OK: $file"
    } else {
        Write-Host "  FAILED: $file"
    }
}
