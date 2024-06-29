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
}:
{
  allModules = callPackage ./doomscript.nix {
    name = "doom-full-init";
    inherit doomSource emacs;
    script = ./full-init;
    scriptArgs = "-o $out";
  };

  allModulesAndFlags = callPackage ./doomscript.nix {
    name = "doom-full-init";
    inherit doomSource emacs;
    script = ./full-init;
    scriptArgs = "--flags -o $out";
  };
}
