{ # Fetch haskell.nix and import its default.nix
haskellNix ? (import (import ./nix/sources.nix)."haskell.nix" { })

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
# see: https://input-output-hk.github.io/haskell.nix/reference/supported-ghc-versions/
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs, compiler-nix-name ? "ghc884" }:
let
  pkgs = import nixpkgsSrc nixpkgsArgs;
  srcs = {
    # Cabal doesn't fetch submodules so we need to override the sources to get the c-source submodule.
    dear-imgui = pkgs.fetchgit {
      url = "https://github.com/haskell-game/dear-imgui.hs";
      rev = "cb687b8f0171eb262731bdbcef4267d81d1e70b7";
      sha256 = "0ragypvqkp7zkqw3xvhcy2wfaasl19f4xf5mhhszlxiadnrbf434";
    };
  };
in pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  src = ./.;
  cabalProjectLocal = ''
    source-repository-package
      type: git
      location: https://github.com/haskell-game/dear-imgui.hs
      tag: cb687b8f0171eb262731bdbcef4267d81d1e70b7
      --sha256: 0ragypvqkp7zkqw3xvhcy2wfaasl19f4xf5mhhszlxiadnrbf434
  '';
  pkg-def-extras = [
    (
      hackage: {
        dear-imgui = import (
          pkgs.haskell-nix.callCabalToNix {
            name = "dear-imgui";
            src = srcs.dear-imgui;
          }
        );
      }
    )
  ];
}
