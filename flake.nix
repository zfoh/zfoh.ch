{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach =
        xs: f:
        with lib;
        foldr recursiveUpdate { } (
          if isList xs then
            map f xs
          else if isAttrs xs then
            mapAttrsToList f xs
          else
            throw "foreach: expected list or attrset but got ${typeOf xs}"
        );
      hsSrc =
        root:
        with lib.fileset;
        toSource {
          inherit root;
          fileset = fileFilter (
            file:
            any file.hasExt [
              "cabal"
              "hs"
              "md"
            ]
            || file.name == "LICENSE"
            || file.type == "directory"
          ) ./.;
        };
      pname = "zfoh";
      overlay =
        final: prev:
        lib.pipe prev [
          (prev: {
            haskell = prev.haskell // {
              packageOverrides = lib.composeManyExtensions [
                prev.haskell.packageOverrides
                (
                  hfinal: hprev: with prev.haskell.lib.compose; {
                    ${pname} = doJailbreak (hfinal.callCabal2nix pname (hsSrc ./.) { });
                  }
                )
              ];
            };
          })
        ];
    in
    foreach inputs.nixpkgs.legacyPackages (
      system: pkgs':
      let
        pkgs = pkgs'.extend overlay;
      in
      {
        formatter.${system} = pkgs.nixfmt-rfc-style;
        packages.${system}.default = pkgs.haskellPackages.${pname};
        devShells.${system}.default = pkgs.haskellPackages.shellFor {
          packages = ps: [ ps.${pname} ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            stack
            stylish-haskell
          ];
        };
      }
    );
}
