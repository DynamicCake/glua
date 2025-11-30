{
  description = "Glua dev shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      allSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems =
        fn: nixpkgs.lib.genAttrs allSystems (system: fn { pkgs = import nixpkgs { inherit system; }; });
    in
    {
      devShells = forAllSystems (
        { pkgs }:
        {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              gleam
              erlang
              rebar3
              beam27Packages.elixir
              beam27Packages.hex
            ];
          };
        }
      );
    };
}
