Set-PSReadLineOption -EditMode Emacs

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
