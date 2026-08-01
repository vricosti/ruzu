# SPDX-FileCopyrightText: 2026 ruzu contributors
# SPDX-License-Identifier: GPL-3.0-or-later

param(
    [Parameter(Mandatory = $true)]
    [string]$SourceDirectory,

    [Parameter(Mandatory = $true)]
    [string]$OutputFile
)

$ErrorActionPreference = "Stop"

$sourceRoot = (Resolve-Path -LiteralPath $SourceDirectory).Path
$zoneinfoRoot = Join-Path $sourceRoot "zoneinfo"
if (-not (Test-Path -LiteralPath (Join-Path $sourceRoot "binaryList.txt") -PathType Leaf) -or
    -not (Test-Path -LiteralPath (Join-Path $sourceRoot "version.txt") -PathType Leaf) -or
    -not (Test-Path -LiteralPath $zoneinfoRoot -PathType Container)) {
    throw "SourceDirectory is not an extracted nx_tzdb archive"
}

$builder = [System.Text.StringBuilder]::new()
[void]$builder.AppendLine("// SPDX-FileCopyrightText: 2023 yuzu Emulator Project")
[void]$builder.AppendLine("// SPDX-License-Identifier: GPL-2.0-or-later")
[void]$builder.AppendLine("//")
[void]$builder.AppendLine("// Generated from the upstream nx_tzdb 221202 archive.")
[void]$builder.AppendLine("// Regenerate with scripts/generate_nx_tzdb.ps1; do not edit by hand.")
[void]$builder.AppendLine()
[void]$builder.AppendLine("pub(crate) struct EmbeddedFile {")
[void]$builder.AppendLine("    pub(crate) name: &'static str,")
[void]$builder.AppendLine("    pub(crate) data: &'static [u8],")
[void]$builder.AppendLine("}")
[void]$builder.AppendLine()

function Add-Group {
    param(
        [string]$ConstantName,
        [System.IO.FileInfo[]]$Files
    )

    [void]$builder.AppendLine("pub(crate) static ${ConstantName}: &[EmbeddedFile] = &[")
    foreach ($file in ($Files | Sort-Object Name)) {
        $escapedName = $file.Name.Replace("\", "\\").Replace('"', '\"')
        [void]$builder.AppendLine("    EmbeddedFile {")
        [void]$builder.AppendLine("        name: `"$escapedName`",")
        [void]$builder.AppendLine("        data: &[")
        $bytes = [System.IO.File]::ReadAllBytes($file.FullName)
        for ($offset = 0; $offset -lt $bytes.Length; $offset += 24) {
            $end = [Math]::Min($offset + 24, $bytes.Length)
            $line = [System.Text.StringBuilder]::new("            ")
            for ($index = $offset; $index -lt $end; $index++) {
                [void]$line.AppendFormat("0x{0:X2}, ", $bytes[$index])
            }
            [void]$builder.AppendLine($line.ToString())
        }
        [void]$builder.AppendLine("        ],")
        [void]$builder.AppendLine("    },")
    }
    [void]$builder.AppendLine("];")
    [void]$builder.AppendLine()
}

$baseFiles = @(
    Get-Item -LiteralPath (Join-Path $sourceRoot "binaryList.txt")
    Get-Item -LiteralPath (Join-Path $sourceRoot "version.txt")
)
Add-Group "BASE" $baseFiles
Add-Group "ZONEINFO" @(Get-ChildItem -LiteralPath $zoneinfoRoot -File)

$groups = [ordered]@{
    "AFRICA" = "Africa"
    "AMERICA" = "America"
    "AMERICA_ARGENTINA" = "America\Argentina"
    "AMERICA_INDIANA" = "America\Indiana"
    "AMERICA_KENTUCKY" = "America\Kentucky"
    "AMERICA_NORTH_DAKOTA" = "America\North_Dakota"
    "ANTARCTICA" = "Antarctica"
    "ARCTIC" = "Arctic"
    "ASIA" = "Asia"
    "ATLANTIC" = "Atlantic"
    "AUSTRALIA" = "Australia"
    "BRAZIL" = "Brazil"
    "CANADA" = "Canada"
    "CHILE" = "Chile"
    "ETC" = "Etc"
    "EUROPE" = "Europe"
    "INDIAN" = "Indian"
    "MEXICO" = "Mexico"
    "PACIFIC" = "Pacific"
    "US" = "US"
}

foreach ($entry in $groups.GetEnumerator()) {
    $directory = Join-Path $zoneinfoRoot $entry.Value
    Add-Group $entry.Key @(Get-ChildItem -LiteralPath $directory -File)
}

$outputParent = Split-Path -Parent $OutputFile
if (-not (Test-Path -LiteralPath $outputParent -PathType Container)) {
    throw "Output directory does not exist: $outputParent"
}

[System.IO.File]::WriteAllText(
    $OutputFile,
    $builder.ToString(),
    [System.Text.UTF8Encoding]::new($false)
)
