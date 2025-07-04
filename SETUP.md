# Haskell Environment Setup for Windows

## Install Stack (Recommended)

1. Download and install Stack from: https://docs.haskellstack.org/en/stable/install_and_upgrade/
   - Or via PowerShell: `Invoke-WebRequest -Uri https://get.haskellstack.org/ -OutFile stack.ps1; .\stack.ps1`

2. Verify installation:
   ```powershell
   stack --version
   ```

## Alternative: Install via Chocolatey

```powershell
# Install chocolatey first if you don't have it
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Then install haskell stack
choco install haskell-stack
```

## Alternative: Install GHCup (Official Haskell Installer)

1. Open PowerShell as Administrator
2. Run: `Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }`

## Building and Running

Once Stack is installed:

```powershell
# Build the project
stack build

# Run the interpreter in REPL mode
stack exec hlox

# Run with a file
stack exec hlox examples/test.lox

# Run specific test files
stack exec hlox examples/test_control_flow.lox
stack exec hlox examples/test_functions.lox  
stack exec hlox examples/test_classes.lox
stack exec hlox examples/test_expressions.lox

# Run tests
stack test
```

## Test Files

The project includes several test files to demonstrate Lox language features:

- **`test.lox`** - Basic language features (variables, arithmetic, strings, booleans)
- **`test_control_flow.lox`** - If statements, loops, and block scoping
- **`test_functions.lox`** - Function definitions, calls, recursion, and closures
- **`test_classes.lox`** - Classes, objects, inheritance, and methods
- **`test_expressions.lox`** - Complex expressions and operator precedence

## IDE Setup

Recommended IDEs for Haskell:
- VS Code with Haskell Language Server extension
- IntelliJ IDEA with Haskell plugin
- Emacs with haskell-mode
- Vim with haskell-vim
