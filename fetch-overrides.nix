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

# Data loaded by doom.nix.
{
  extraUrls = {
    # Straight recipe from el-get
    font-lock-ext = "https://github.com/sensorflo/font-lock-ext.git";
    sln-mode = "https://github.com/sensorflo/sln-mode.git";
    # Straight recipe from emacsmirror-mirror
    nose = "https://github.com/emacsattic/nose.git";
    # In nixpkgs, but uses codeberg, for which nixpkgs uses fetchzip.
    # TODO: consider parsing origEPkg.src.url instead.
    tree-sitter-indent = "https://codeberg.org/FelipeLema/tree-sitter-indent.el.git";
    undo-fu = "https://codeberg.org/ideasman42/emacs-undo-fu.git";
    undo-fu-session = "https://codeberg.org/ideasman42/emacs-undo-fu-session.git";
    # nixpkgs uses a release from nongnu ELPA.
    corfu-terminal = "https://codeberg.org/akib/emacs-corfu-terminal";
  };

  # Pins for packages not pinned by Doom and not in nixpkgs or emacs-overlay.
  extraPins = {
    # Looks stable enough we can get away with pinning it.
    "sly-stepper" = "da84e3bba8466c2290c2dc7c27d7f4c48c27b39e";
  };
}
