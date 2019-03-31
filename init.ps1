<#
.SYNOPSIS

Initializes the environment for programming and testing

.DESCRIPTION

It will check that the required executables exist, and will add them to the path.
It will also setup other environmental variables correctly.

#>
[CmdletBinding()]
param(
)

function ExecuteInPath
{
    # Executes a command in a specified directory
    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [string]
        # The name of the executable to use
        $Command,

        [ValidateNotNullOrEmpty()]
        [ValidateScript({Test-Path -Path $_ -PathType Container})]
        [string]
        # The path where to invoke the executable
        $Path
    )

    Push-Location -Path $Path
    . $Command
    Pop-Location
}

# This is the set of executables that we need to have.
# The executables may need to be downloaded.
$Executables = @(
    [PSCustomObject]@{
        Name = "paket.exe"
        Path = (Join-Path -Path PL -ChildPath DotNet | Join-Path -ChildPath .paket)
        Downloader = "paket.bootstrapper.exe"
    }
)

# Check that the executables exist locally and that the environment points to them.
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