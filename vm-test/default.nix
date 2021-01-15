{
  pkgs ? (import ../default.nix {}).pkgs
}:
{
  inherit pkgs;
}
