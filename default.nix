{ # Fetch haskell.nix and import its default.nix
haskellNix ? (import (import ./nix/sources.nix)."haskell.nix" { })

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs, compiler-nix-name ? "ghc884" }:
let
  pkgs = import nixpkgsSrc nixpkgsArgs;
  srcs = {
    # Cabal doesn't fetch submodules so we need to override the sources to get the c-source submodule.
    dear-imgui = pkgs.fetchgit {
      url = "https://github.com/haskell-game/dear-imgui.hs";
      rev = "2fbe257c2497ff0e2193289f33ca276baf016f93";
      sha256 = "1n23sgmxb839n19sgvpmm3ysr9fdgq2xddagyk8hg04lpr5d6wb2";
    };
  };
in pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "dear-imgui-reflex";
    src = ./.;
  };
  modules = [{ packages.dear-imgui.src = srcs.dear-imgui; }];
}