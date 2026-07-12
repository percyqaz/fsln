# FSLN

Is a terminal-based solution explorer for F# solutions and projects  
It exists because hardly any IDEs have good built-in support for reordering files or folders up or down (file/build order matters for F# projects)  

I've made it very vim-like in design philosophy so it can scale to my various needs and I can hot-wire it to do new stuff on the go

Features:
- Detects and opens a solution in the working directory, displaying it as a tree
- Tree can be navigated with collapsible nodes, etc, in general I am aiming to replicate all functions of Jetbrains Rider's Solution Explorer panel
- Vim-like commands and configuration for all appearance and keybinds

This project is in very early stages but I am already dogfooding it while I work  
It it made specifically for me, your mileage may vary

## Installing

1. Clone the repo

2. Run `./update.sh` to install as a dotnet tool

3. Run with `fsln`
