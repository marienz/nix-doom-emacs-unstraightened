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
  writeTextDir,
}:
let
  allModules = callPackage ./doomscript.nix {
    name = "doom-full-init";
    inherit doomSource emacs;
    script = ./full-init;
    scriptArgs = "-o $out";
  };

  allModulesAndReallyAllFlags = callPackage ./doomscript.nix {
    name = "doom-full-init";
    inherit doomSource emacs;
    script = ./full-init;
    scriptArgs = "--flags -o $out";
  };

  # HACK: drop roam v1. See https://github.com/marienz/nix-doom-emacs-unstraightened/issues/39
  allModulesAndFlags = writeTextDir "init.el" (
    lib.replaceStrings [ " +roam " ] [ " " ] (lib.readFile "${allModulesAndReallyAllFlags}/init.el")
  );

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
