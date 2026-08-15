# Keep the Windows shell close to fish without adding another prompt framework.
if (Get-Module -ListAvailable -Name PSReadLine) {
    Import-Module PSReadLine
    Set-PSReadLineOption -EditMode Emacs
    if ($Host.UI.SupportsVirtualTerminal -and -not [Console]::IsOutputRedirected) {
        Set-PSReadLineOption -PredictionSource History -PredictionViewStyle InlineView
    }
    Set-PSReadLineKeyHandler -Key Ctrl+a -Function BeginningOfLine
    Set-PSReadLineKeyHandler -Key Ctrl+e -ScriptBlock {
        param($key, $arg)

        $beforeLine = $null
        $beforeCursor = 0
        [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$beforeLine, [ref]$beforeCursor)
        [Microsoft.PowerShell.PSConsoleReadLine]::AcceptSuggestion($key, $arg)

        $afterLine = $null
        $afterCursor = 0
        [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$afterLine, [ref]$afterCursor)
        if ($afterLine -eq $beforeLine -and $afterCursor -eq $beforeCursor) {
            [Microsoft.PowerShell.PSConsoleReadLine]::EndOfLine($key, $arg)
        }
    }
    Set-PSReadLineKeyHandler -Key Ctrl+w -Function BackwardKillWord
    Set-PSReadLineKeyHandler -Key Ctrl+u -Function BackwardDeleteLine
    Set-PSReadLineKeyHandler -Key Ctrl+r -Function ReverseSearchHistory
}

# A few names shared with the Linux/fish setup.
function l { Get-ChildItem @args }
function ll { Get-ChildItem -Force @args }
function .. { Set-Location .. }
function gs { git status @args }
function which { Get-Command @args }

function gg {
    $root = git rev-parse --show-toplevel 2>$null
    if ($LASTEXITCODE -eq 0) {
        Set-Location $root
    }
}

function prompt {
    $directory = Split-Path -Leaf -Path $PWD
    if ([string]::IsNullOrEmpty($directory)) {
        $directory = $PWD
    }

    $branch = if (Get-Command git -ErrorAction SilentlyContinue) {
        git branch --show-current 2>$null
    }
    $gitPrompt = if ($branch) { " $($PSStyle.Dim)($branch)$($PSStyle.Reset)" }

    "$($PSStyle.Foreground.BrightCyan)$directory$($PSStyle.Reset)$gitPrompt $($PSStyle.Dim)❯$($PSStyle.Reset) "
}
