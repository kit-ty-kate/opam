param (
  [bool]$Dev = $false,
  [string]$Version = "2.2.0~beta2"
)

$DevVersion = "2.2.0~beta2"
$DefaultBinDir = $Env:windir

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
    "opam-2.2.0-beta2-x86_64-windows.exe" { "1a7cc74d04951aa8b5ca8722d8403a509c35ac31d06cc32f5f6aa792d925dc76ad6821f0980a2c9cf5299e5faa9e2a09d2d6de4d5ab9b5333d686356507279f7" }
    Default { throw "no sha" }
  }
}

$Tag = $Version -creplace "~", "-"
$Arch = GetArch
$OS = "windows"
$OpamBinUrlBase = "https://github.com/ocaml/opam/releases/download/"
$OpamBin = "opam-${Tag}-${Arch}-${OS}.exe"
$OpamBinUrl = "${OPAM_BIN_URL_BASE}${TAG}/${OPAM_BIN}"

Function CheckSHA512 {
  param (
    [string]$OpamBinLoc
  )

  $Hash = (CertUtil -hashfile "$BinDirLoc" MD5)[1]
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

$BinDir = Read-Host "## Where should it be installed? [$DefaultBinDir]"
if ($BinDir -eq "") {
  $BinDir = $DefaultBinDir
}
if ( -not (Test-Path -Path "$BinDir" -PathType Container) ) {
  New-Item "$BinDir" -Type Directory
}

Move-Item -Path "$OpamBinLoc" -Destination "$BinDir"
