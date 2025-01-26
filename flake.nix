{
  description = "time-machines";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              time-machines = hfinal.callCabal2nix "time-machines" ./. { };
            };
        };
        time-machines = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.time-machines;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = time-machines-shell;
            time-machines-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.time-machines ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = time-machines;
            time-machines = pkgs.time-machines;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
