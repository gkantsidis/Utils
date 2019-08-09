
function Update-PaketScripts {
    <#
    .SYNOPSIS
    Create load scripts for the nuget packages referenced with paket

    .DESCRIPTION
    Invoke paket to create the load scripts for the packages used with paket.
    As an optimization, the scripts will try to avoid regenerating the load
    scripts, unless the dependencies have changed. To do so, it stores the
    timestamp of the paket.lock file in a temporary file (by default
    '.paket-lock-timestamp', which should not be checked in), and updates
    only if it detectes an updated timestamp.

    .PARAMETER Frameworks
    The frameworks for which to generate script files (e.g. "netstandard2.0", "net47").

    .PARAMETER Type
    The type of load script files to generate (either 'fsx' for F#, 'csx' for C#, or both)

    .PARAMETER TimeStampFile
    The temporary file where to store the timestamp of the paket.lock file

    .PARAMETER Force
    Force load script generation.

    .NOTES
    Do not commit to your source control system the temporary file
    (by default ".paket-lock-timestamp").

    #>

    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [string[]] $Frameworks = @("netstandard2.0", "net47"),

        [ValidateSet("fsx", "csx")]
        [string[]] $Type = @("fsx"),

        [ValidateNotNullOrEmpty()]
        [string] $TimeStampFile = ".paket-lock-timestamp",

        [switch] $Force
    )

    $location = Get-Command -Name paket
    $root = Split-Path -Path $location.Source -Parent | Split-Path -Parent
    Write-Verbose -Message "Running in $root"

    $paket_script_timestamp_file = Join-Path -Path $root -ChildPath $TimeStampFile
    $paket_lock = Join-Path -Path $root -ChildPath "paket.lock"
    $current_lock = Get-Item -Path $paket_lock
    $last_update = $current_lock.LastWriteTimeUtc

    if ($Force) {
        $shouldUpdate = $true
    } elseif (Test-Path -Path $paket_script_timestamp_file -PathType Leaf) {
        $last_generation_string = Get-Content -Path $paket_script_timestamp_file
        $last_generation = [DateTime]::Parse($last_generation_string.Trim())

        $shouldUpdate = $last_update -ne $last_generation
    } else {
        Write-Verbose -Message "Timestamp file does not exist, creating"
        $shouldUpdate = $true
    }

    if ($shouldUpdate) {
        Write-Verbose -Message "Recreating startup scripts"
        Push-Location -Path $root

        $arguments = @()

        $Frameworks | ForEach-Object -Process {
            $framework = $_
            $arguments += '-f'
            $arguments += $framework
        }

        $Type | ForEach-Object -Process {
            $type = $_
            $arguments += '-t'
            $arguments += $type
        }

        try {
            paket generate-load-scripts @arguments
            $timestamp = $last_update.ToString().Trim()
            $timestamp | Out-File -FilePath $paket_script_timestamp_file -Encoding ascii
        }
        finally {
            Pop-Location
        }
    }
}