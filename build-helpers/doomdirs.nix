# Copyright 2024 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

{
  callPackage,
  doomSource,
  emacs,
  lib,
  stdenv,
  writeTextDir,
}:
let
  # https://github.com/NixOS/nixpkgs/blob/f4501c9681b56c207927782b4c76a07190a58ab9/pkgs/development/compilers/gnat-bootstrap/default.nix
  # We might be able to drop this once nixpkgs defaults to gnat 14.
  adaUnsupported = stdenv.hostPlatform.system == "aarch64-darwin";
  commonArgs = "-o $out" + (lib.optionalString adaUnsupported " -s '(:lang ada)'");
  allModules = callPackage ./doomscript.nix {
    name = "doom-full-init";
    inherit doomSource emacs;
    script = ./full-init;
    scriptArgs = commonArgs;
  };

  allModulesAndFlags = callPackage ./doomscript.nix {
    name = "doom-full-init";
    inherit doomSource emacs;
    script = ./full-init;
    scriptArgs = "--flags " + commonArgs;
  };

  # Hack, but given how this is used it's good enough even if the replacement misfires.
  #
  # Goal is to disable eglot and flymake, because doing so enables lsp-mode and flycheck
  # respectively. We also want to keep most other flags enabled. But even if Doom introduces a flag
  # starting with "eglot" or "flymake", disabling it and enabling a nonsensical flag is not really
  # an issue.
  allModulesMostFlags = writeTextDir "init.el" (
    lib.replaceStrings
      [
        " +eglot"
        " +flymake"
      ]
      [
        ""
        ""
      ]
      (lib.readFile "${allModulesAndFlags}/init.el")
  );
in
{
  inherit allModules allModulesAndFlags allModulesMostFlags;
}
