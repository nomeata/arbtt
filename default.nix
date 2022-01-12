{ rev    ? "bd11019686adcec876af13ab8c1902575eb383ba" # release-21.11

, pkgs   ?
    import (builtins.fetchTarball {
     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) {
       config.allowUnfree = true;
       config.allowBroken = false;
    }
, returnShellEnv ? pkgs.lib.inNixShell
}:

pkgs.haskellPackages.developPackage {
  name = "winter";
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
  });

  inherit returnShellEnv;
}
