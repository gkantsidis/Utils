[CmdletBinding()]
param(

)

function ExecuteInPath
{
    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [string]$Command,

        [ValidateNotNullOrEmpty()]
        [ValidateScript({Test-Path -Path $_ -PathType Container})]
        [string]$Path
    )

    Push-Location -Path $Path
    . $Command
    Pop-Location
}


$Executables = @(
    [PSCustomObject]@{
        Name = "paket.exe"
        Path = (Join-Path -Path PL -ChildPath DotNet | Join-Path -ChildPath .paket)
        Downloader = "paket.bootstrapper.exe"
    }
)

foreach ($executable in $Executables) {
    $path = Join-Path -Path $PSScriptRoot -ChildPath $executable.Path
    $name = $executable.Name

    $target = Join-Path -Path $path -ChildPath $name
    if (Test-Path -Path $target -PathType Leaf) {
        Write-Verbose -Message "Target $target exists"
    } else {
        Write-Verbose -Message "Executable $target does not exist"
        ExecuteInPath -Command $executable.Downloader -Path $path
    }

    $current = Get-Command -Name $name -ErrorAction SilentlyContinue
    if ($current -eq $null) {
        Write-Verbose -Message "Command $name not in path, adding.."
        $Env:PATH += ";$path"
    } else {
        $currentPath = Split-Path $current.Source -Parent
        if ($currentPath.Equals($path, [StringComparison]::InvariantCultureIgnoreCase)) {
            Write-Verbose -Message "Command $name already pointing to correct executable"
        }
        else {
            Write-Verbose -Message "Command $name in path, but not pointing to this version"
            $Env:PATH = $path + ";" + $Env:PATH
        }
    }
}