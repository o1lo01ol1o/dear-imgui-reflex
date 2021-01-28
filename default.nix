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
      rev = "d227561885550cceb19e6a4fed854c7774b2fa1b";
      sha256 = "0fc0hby6hxd0s996gfbrkn4p1hrqmmb02szh562b9a2vg6m863jr";
    };
  };
in pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  src = ./.;
  cabalProjectLocal = ''
    source-repository-package
      type: git
      location: https://github.com/haskell-game/dear-imgui.hs
      tag: d227561885550cceb19e6a4fed854c7774b2fa1b
      --sha256: 0fc0hby6hxd0s996gfbrkn4p1hrqmmb02szh562b9a2vg6m863jr
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
