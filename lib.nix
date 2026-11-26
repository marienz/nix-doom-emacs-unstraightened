let
  # Convert a Nix expression to a `doom!` block suitable for init.el.
  #
  # Input: a nested attribute set.
  # The keys of the first level are categories (like `lang`).
  # The keys of the second level are module names (like `nix`).
  # The values are lists of module flags, or `true` for no flags.
  toInit =
    lib:
    let
      inherit (lib)
        concatLines
        concatStringsSep
        isList
        mapAttrsToList
        toPretty
        ;
    in
    attrs:
    concatLines (
      [ "(doom!" ]
      ++ (mapAttrsToList (
        cat: modules:
        (concatLines (
          [ (":" + cat) ]
          ++
            (mapAttrsToList (
              mod: value:
              if value == true then
                mod
              else if isList value then
                "(${mod} ${concatStringsSep " " value})"
              else
                abort "${toPretty value} not supported"
            ))
              modules
        ))
      ) attrs)
      ++ [ ")" ]
    );

in
{
  inherit toInit;
}
