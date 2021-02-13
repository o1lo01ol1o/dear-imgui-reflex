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
      rev = "06921defb1d2d5005bc1f8b578efb795729356d1";
      sha256 = "136vxgicz5qmv21y3r8srcrprxxvybw9vldr05q55ynsrxxq4lx9";
    };
  };
in pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  src = ./.;
  cabalProjectLocal = ''
    source-repository-package
      type: git
      location: https://github.com/haskell-game/dear-imgui.hs
      tag: 06921defb1d2d5005bc1f8b578efb795729356d1
      --sha256: 136vxgicz5qmv21y3r8srcrprxxvybw9vldr05q55ynsrxxq4lx9
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
