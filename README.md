# FPIndia Site

[Ema](https://ema.srid.ca/) static site for https://functionalprogramming.in/

## Getting Started

## With Nix

[Install Nix][install-nix] and then run `bin/run` to get the site up and running locally. See further below for static site generation.

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix][install-nix]
- Run `nix develop -c haskell-language-server` to sanity check your environment
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run the command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` in terminal, to launch the Ema dev server, and navigate to http://localhost:8081/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/tutorial) next.

[install-nix]: https://nixos.asia/en/install

#### Generate the site

```sh
mkdir /tmp/site
nix run . -- gen /tmp/site
```

## Without Nix

Working on this repository without Nix is supported.

#### Basic setup with Cabal

Some dependencies have been vendored in `vendor/` for non-nix workflows. The setup has been tested to work with GHC 9.4.8.

```
cabal build
```

#### Tailwind CSS support

Use NPM and package.json to [install tailwind packages](https://tailwindcss.com/docs/installation).

```
npm install
```

#### Run the server with hot code reloading

Use NPM to start everything. This will concurrently start ghcid, as well as the tailwind watcher. Hot code reloading should work automatically.

```
npm start
```

#### Generate the site

```sh
mkdir /tmp/site
cabal run fpindia-site -- gen /tmp/site
```

Or simply -

```
npm run gen
```

## Note

- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as Tailwind+Blaze as CSS utility and HTML DSL.
- Configuration:
  - To change the port (or the Ema CLI arguments, used by `bin/run`), see `./.ghcid` (if you leave out `--port` a random port will be used)
  - To update Ema to the latest Git revision -
    - Nix instructions: run `nix flake lock --update-input ema` or just `nix flake update` (the latter updates all Nix inputs)
      Be sure to check https://ema.srid.ca/start/upgrade for changes needed.
    - Non nix instructions: Update the copy of Ema in the `vendor` folder
- You might want to change or remove the `<base>` tag in `Main.hs` depending on where you will be deploying the site.

