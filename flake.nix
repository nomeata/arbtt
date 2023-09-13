{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
  description = "Haskell development environment";
  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux =
     with nixpkgs.legacyPackages.x86_64-linux;
     let
       myGhc = ghc.withPackages(p : with p; [
        pcre-light
      ]);
     in mkShell rec {
       name = "env";
       packages = [ myGhc ghcid haskell-ci ];
       shellHook = ''
         export NIX_GHC=${myGhc}/bin/ghc
         export NIX_GHC_LIBDIR=${myGhc}/lib/ghc-${myGhc.version}
       '';
     };
  };
}
