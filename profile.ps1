Set-PSReadLineOption -EditMode Emacs

function ch {
    & "$HOME\bin\git-branch-deleter.exe" @args
}

function amend {
    git commit --amend  @args
}
function aamend {
    git commit -a --amend  @args
}
function push {
    git push @args
}
function pushf {
    git push -f @args
}
function one {
    git log --oneline @args
}
function graph {
    git log --oneline --graph @args
}
function wip {
    git add .
    $msg = $args -join ' '
    git commit -m "wip $msg $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
}
function wipp {
    wip @args
    push
}

function ibn {
    $currentBranch = git rev-parse --abbrev-ref HEAD

    if ($currentBranch -match '^(.*[^0-9])([0-9]+)$') {
        $prefix = $Matches[1]
        $number = [int]$Matches[2]
        $newBranch = "$prefix$($number + 1)"
    }
    else {
        $newBranch = "$currentBranch-1"
    }

    git branch -m $newBranch
    Write-Host "Renamed branch '$currentBranch' to '$newBranch'"
}

function prompt {
    $esc = [char]27
    $reset = "$esc[0m"
    $bold = "$esc[1m"
    $yellow = "$esc[33m"
    $red = "$esc[31m"
    $green = "$esc[32m"
    $magenta = "$esc[35m"
    $branchStyle = "$esc[42m$esc[30m$bold"

    $userName = if ($env:USERNAME) { $env:USERNAME } else { $env:USER }
    $hostName = [System.Net.Dns]::GetHostName()
    $timestamp = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'

    $result = "`n`n`n$userName @ $hostName $timestamp`n"
    $result += "$bold$($PWD.Path)$reset"

    # Git call 1: branch name + staged/unstaged changes
    $status = @(git --no-optional-locks status `
        --porcelain=v2 --branch --untracked-files=no 2>$null)

    if ($LASTEXITCODE -eq 0) {
        $branch = $null
        $oid = $null
        $staged = $false
        $unstaged = $false

        foreach ($line in $status) {
            if ($line.StartsWith('# branch.head ')) {
                $branch = $line.Substring(14)
            }
            elseif ($line.StartsWith('# branch.oid ')) {
                $oid = $line.Substring(13)
            }
            elseif ($line -match '^[12u] (..) ') {
                $xy = $Matches[1]
                if ($xy[0] -ne '.') { $staged = $true }
                if ($xy[1] -ne '.') { $unstaged = $true }
            }
        }

        # Git call 2: latest commit + remote/tag decorations
        $log = git log -1 --ignore-submodules `
            '--format=%h%x1f%s%x1f%D' `
            --decorate=short `
            '--decorate-refs=refs/remotes/*' `
            '--decorate-refs=refs/tags/*' 2>$null

        $commit = ''
        $refs = ''

        if ($log) {
            $parts = "$log".Split([char]31)
            $commit = "$($parts[0]) $($parts[1])"

            if ($commit.Length -gt 50) {
                $commit = $commit.Substring(0, 50)
            }

            if ($parts.Count -ge 3) {
                $refs = $parts[2]
            }
        }

        # Detached HEAD: display the abbreviated commit
        if ($branch -eq '(detached)') {
            if ($oid -and $oid -ne '(initial)') {
                $branch = $oid.Substring(0, [Math]::Min(7, $oid.Length))
            }
            else {
                $branch = 'HEAD'
            }
        }

        # Git call 3: find the Git directory
        $gitDir = git rev-parse --absolute-git-dir 2>$null

        $action = $null
        if ($gitDir) {
            if ((Test-Path (Join-Path $gitDir 'rebase-merge')) -or
                (Test-Path (Join-Path $gitDir 'rebase-apply'))) {
                $action = 'rebase'
            }
            elseif (Test-Path (Join-Path $gitDir 'MERGE_HEAD')) {
                $action = 'merge'
            }
            elseif (Test-Path (Join-Path $gitDir 'CHERRY_PICK_HEAD')) {
                $action = 'cherry-pick'
            }
            elseif (Test-Path (Join-Path $gitDir 'REVERT_HEAD')) {
                $action = 'revert'
            }
        }

        # Assemble Git line
        $gitLine = "`n"

        if ($commit) {
            $gitLine += "$yellow$commit$reset"
        }

        if ($branch) {
            $gitLine += " $branchStyle $branch $reset"
        }

        if ($unstaged) {
            $gitLine += " ${red}U${reset}"
        }

        if ($staged) {
            $gitLine += " ${green}S${reset}"
        }

        if ($action) {
            $gitLine += " ${magenta}$action${reset}"
        }

        if ($refs) {
            $gitLine += " ($refs)"
        }

        $result += $gitLine
    }

    $result += "`n`$ "
    return $result
}

Set-PSReadLineKeyHandler -Chord "Ctrl+g,Ctrl+b" -ScriptBlock {
    $text = git branch --show-current

    if ($text) {
        [Microsoft.PowerShell.PSConsoleReadLine]::Insert($text)
    }
}
Set-PSReadLineKeyHandler -Chord "Ctrl+g,Ctrl+h" -ScriptBlock {
    $text = git log -1 --format=%h

    if ($text) {
        [Microsoft.PowerShell.PSConsoleReadLine]::Insert($text)
    }
}
