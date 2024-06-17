param (
  [switch]$Dev,
  [string]$Version = "2.2.0~beta3",
  [string]$OpamBinDir = ""
)

$DevVersion = "2.2.0~beta3"
$DefaultBinDir = "$Env:ProgramFiles\opam\bin"

Function GetArch {
  switch ($Env:PROCESSOR_ARCHITECTURE) {
    "AMD64" { "x86_64" }
    "Arm64" { "arm64" }
    "x86" { "i686" }
    Default { throw "Unknown architecture" }
  }
}

Function BinSHA512 {
  param (
    [string]$OpamBinName
  )

  switch ($OpamBinName) {
    "opam-2.2.0-beta2-x86_64-windows.exe" { "74f034ccc30ef0b2041283ff125be2eab565d4019e79f946b515046c4c290a698266003445f38b91321a9ef931093651f861360906ff06c076c24d18657e2aaf" }
    "opam-2.2.0-beta3-x86_64-windows.exe" { "f09337d94e06cedb379c5bf45a50a79cf2b2e529d7c2bb9b35c8a56d40902ff8c7e3f4de9c75fb5c8dd8272b87b2a2645b14e40ef965376ef0d19afd923acf3b" }
    Default { throw "no sha" }
  }
}

Function CheckSHA512 {
  param (
    [string]$OpamBinTmpLoc,
    [string]$OpamBinName
  )

  $Hash = (CertUtil -hashfile "$OpamBinTmpLoc" SHA512)[1]
  $HashTarget = BinSHA512 -OpamBinName "$OpamBinName"

  if ("$Hash" -ne "$HashTarget") {
    throw "Checksum does not match"
  }
}

Function DownloadAndCheck {
  param (
    [string]$OpamBinUrl,
    [string]$OpamBinTmpLoc,
    [string]$OpamBinName
  )

  Start-BitsTransfer -Source "$OpamBinUrl" -Destination "$OpamBinTmpLoc"
  CheckSHA512 -OpamBinTmpLoc "$OpamBinTmpLoc" -OpamBinName "$OpamBinName"
}

if ($Dev.IsPresent) {
  $Version = $DevVersion
}

$Tag = $Version -creplace "~", "-"
$Arch = GetArch
$OS = "windows"
$OpamBinUrlBase = "https://github.com/ocaml/opam/releases/download/"
$OpamBinName = "opam-${Tag}-${Arch}-${OS}.exe"
$OpamBinUrl = "${OpamBinUrlBase}${Tag}/${OpamBinName}"

$OpamBinTmpLoc = "$Env:TMP\$OpamBinName"
DownloadAndCheck -OpamBinUrl "$OpamBinUrl" -OpamBinTmpLoc "$OpamBinTmpLoc" -OpamBinName "$OpamBinName"

if ($OpamBinDir -eq "") {
  $OpamBinDir = Read-Host "## Where should it be installed? [$DefaultBinDir]"
  if ($OpamBinDir -eq "") {
    $OpamBinDir = $DefaultBinDir
  }
}

if (($OpamBinDir -contains "'") -or ($OpamBinTmpLoc -contains "'") -or ($OpamBinDir -contains '"')) {
  throw "String contains unsupported characters"
}

Start-Process -FilePath powershell -Verb RunAs -ArgumentList '-NoExit', '-Command', @"
if (-not (Test-Path -Path '$OpamBinDir' -PathType Container)) {
  New-Item -Path '$OpamBinDir' -Type Directory -Force
}
[void](Move-Item -Force -Path '$OpamBinTmpLoc' -Destination '${OpamBinDir}\opam.exe')

`$PATH = [Environment]::GetEnvironmentVariable('PATH', 'MACHINE')
if (-not (`$PATH -split ';' -contains '$OpamBinDir')) {
  [Environment]::SetEnvironmentVariable('PATH', '${OpamBinDir};'+`$PATH, 'MACHINE')
}
Exit
"@
