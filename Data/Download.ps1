[CmdletBinding()]
param(

)

$Repos = @{
    "kml-samples" = "https://github.com/googlemaps/kml-samples.git"
}

foreach ($repo in $Repos.Keys) {
    if (-not (Test-Path -Path $repo -PathType Container)) {
        $target = $Repos[$repo]
        Write-Verbose -Message "Target $repo does not existing; cloning from $target"
        git clone --recursive $target $repo
    }
}