param (
  [bool]$Dev = $false,
  [string]$Version = "2.2.0~beta2"
)

$DevVersion = "2.2.0~beta2"
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
    [string]$OpamBin
  )

  switch ($OpamBin) {
    "opam-2.2.0-beta2-x86_64-windows.exe" { "74f034ccc30ef0b2041283ff125be2eab565d4019e79f946b515046c4c290a698266003445f38b91321a9ef931093651f861360906ff06c076c24d18657e2aaf" }
    Default { throw "no sha" }
  }
}

$Tag = $Version -creplace "~", "-"
$Arch = GetArch
$OS = "windows"
$OpamBinUrlBase = "https://github.com/ocaml/opam/releases/download/"
$OpamBin = "opam-${Tag}-${Arch}-${OS}.exe"
$OpamBinUrl = "${OpamBinUrlBase}${Tag}/${OpamBin}"

Function CheckSHA512 {
  param (
    [string]$OpamBinLoc
  )

  $Hash = (CertUtil -hashfile "$OpamBinLoc" SHA512)[1]
  $HashTarget = BinSHA512 -OpamBin "$OpamBin"

  if ("$Hash" -ne "$HashTarget") {
    throw "Checksum does not match"
  }
}

Function DownloadAndCheck {
  param (
    [string]$OpamBinLoc
  )

  Start-BitsTransfer -Source "$OpamBinUrl" -Destination "$OpamBinLoc"
  CheckSHA512 -OpamBinLoc "$OpamBinLoc"
}

if ($Dev) {
  $Version = $DevVersion
}

$OpamBinLoc = "$Env:TMP\$OpamBin"
DownloadAndCheck -OpamBinLoc "$OpamBinLoc"

$OpamBinDir = Read-Host "## Where should it be installed? [$DefaultBinDir]"
if ($OpamBinDir -eq "") {
  $OpamBinDir = $DefaultBinDir
}
# TODO: check if OpamBinDir or OpamBinLoc contains single quotes

if ( -not (Test-Path -Path "$OpamBinDir" -PathType Container) ) {
  Start-Process -FilePath powershell -Verb RunAs -ArgumentList ('-Command', "New-Item '$OpamBinDir' -Type Directory -Force")
}

Start-Process -FilePath powershell -Verb RunAs -ArgumentList ('-Command', "Move-Item -Path '$OpamBinLoc' -Destination '$OpamBinDir\opam.exe'")
# TODO: modify Path
