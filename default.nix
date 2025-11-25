let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      fourmolu
      haskell-language-server
      hlint
      hoogle
      ormolu
      stylish-haskell
    ]);
}
