# FPIndia Site

[Ema](https://ema.srid.ca/) static site for https://functionalprogramming.in/

[Install Nix](https://flakular.in/install) and then run `bin/run` to get the site up and running locally. See further below for static site generation.

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://zero-to-flakes.com/install/)
- Run `nix develop -c haskell-language-server` to sanity check your environment
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run the command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` in terminal, to launch the Ema dev server, and navigate to http://localhost:8081/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/tutorial) next.

## Note

- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as Tailwind+Blaze as CSS utility and HTML DSL. Even though the author highly recommends them, you are of course free to swap them out for the library of your choice.
  - Tailwind CSS is compiled, alongside Ghcid, via foreman (see `./Procfile`)
- Configuration:
  - To change the port (or the Ema CLI arguments, used by `bin/run`), see `./.ghcid` (if you leave out `--port` a random port will be used)
  - To update Ema to the latest Git revision, run `nix flake lock --update-input ema` or just `nix flake update` (the latter updates all Nix inputs)
    - Be sure to check https://ema.srid.ca/start/upgrade for changes needed.
  - To add/remove Haskell dependencies, see https://zero-to-flakes.com/haskell-flake/dependency/
- To generate the site, run:
  ```sh
  mkdir /tmp/site
  nix run . -- gen /tmp/site
  ```
  - You might want to change or remove the `<base>` tag in `Main.hs` depending on where you will be deploying the site.

## Non-Nix workflow

To use this repository without Nix, such as with plain Cabal or Stack, you need to have the following installed manually:

- ghcid (used by `bin/run-haskell` which `./Procfile` invokes)
- [tailwind runner](https://hackage.haskell.org/package/tailwind) along with [tailwind CLI](https://tailwindcss.com/docs/installation)
- [foreman](http://ddollar.github.io/foreman/) (or one of its rewrites)

Once all the above are installed, run `foreman start` to start the Ema live server.
