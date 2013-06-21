Grafické prostředí pro testování agentů
=======================================

Tento software umožňuje vývoj a testování agentů s umělou inteligencí.
K simulaci prostředí se šestiúhelnou mřížkou poskytuje grafické rozhraní.
Návod k použití se nachází v souboru doc/hex/Usage.pdf

Obsah
-----

    doc
    doc/hex             ...  popis nových her s šestiúhelnou mřížkou
    doc/hex/Usage.pdf   ...  návod k použití ve formátu PDF
    run.lisp            ...  zaváděcí Lisp soubor
    loadfiles.lisp      ...  soubory, které se načítají při spuštění
    run.sh              ...  skript na spuštění programu CLISP
    quicklisp.lisp      ...  sestavovací nástroj
    SDL.dll             ...  32-bitová distribuce knihovny SDL pro Windows
    SDL_gfx.dll         ...  32-bitová distribuce knihovny SDL_gfx pro Windows
    base                ...  původní zdrojové kódy
    hex                 ...  implementace nového prostředí
    agent               ...  implementace agentů
    test                ...  testovací skripty
    test/run-agent.sh   ...  test referenčního agenta v grafickém prostředí




Graphical environment for agent testing
=======================================

This software provides support for development and testing of intelligent
agents. It provides a graphical interface to the environment simulation.
Usage manual can be found in file doc/hex/Usage.pdf

Contents
--------

    doc
    doc/hex             ...  rules of the new games with a hexagonal grid
    doc/hex/Usage.pdf   ...  usage manual in PDF
    run.lisp            ...  initialization Lisp file
    loadfiles.lisp      ...  files, which should be loaded on initialization
    run.sh              ...  script to run CLISP
    quicklisp.lisp      ...  library manager
    SDL.dll             ...  32-bit distribution of SDL for Windows
    SDL_gfx.dll         ...  32-bit distribution of SDL_gfx for Windows
    base                ...  original source code
    hex                 ...  implementation of the new environment
    agent               ...  agent implementation
    test                ...  test scripts
    test/run-agent.sh   ...  test of the reference agent in the GUI
