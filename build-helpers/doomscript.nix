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
  script,
  scriptArgs,
  doomSource,
  debug ? false,
  name ? "doomscript",
  extraArgs ? { },

  emacs,
  lib,
  runCommandLocal,
  runtimeShell,
  gitMinimal,
}:
runCommandLocal name
  (
    {
      inherit doomSource runtimeShell script;
      EMACS = lib.getExe emacs;
    }
    // (lib.optionalAttrs debug { DEBUG = "1"; })
    // extraArgs
  )
  # Set DOOMLOCALDIR somewhere harmless to stop Doom from trying to create it somewhere read-only.
  # Provide git because Doom uses it when printing backtraces:
  # omitting it makes for needlessly confusing error messages.
  ''
    mkdir $out doomlocaldir
    export DOOMLOCALDIR="$PWD/doomlocaldir"
    export PATH=${lib.getBin gitMinimal}/bin:$PATH
    $runtimeShell $doomSource/bin/doomscript $script ${scriptArgs}
  ''
