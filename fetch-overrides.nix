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

# Data loaded by default.nix.
{
  extraUrls = {
    # Straight recipe from el-get
    font-lock-ext = "https://github.com/sensorflo/font-lock-ext.git";
    sln-mode = "https://github.com/sensorflo/sln-mode.git";
    # Straight recipe from emacsmirror-mirror
    # (emacsmirror-mirror includes emacsattic, emacs-overlay does not...)
    nose = "https://github.com/emacsattic/nose.git";
    ob-ammonite = "https://github.com/emacsattic/ob-ammonite.git";
    ammonite-term-repl = "https://github.com/emacsattic/ammonite-term-repl.git";
    ob-clojure-literate = "https://github.com/emacsattic/ob-clojure-literate.git";
    # sourcehut fetcher doesn't work for the same reason codeberg doesn't.
    fennel-mode = "https://git.sr.ht/~technomancy/fennel-mode";
    # nixpkgs uses a release from nongnu ELPA.
    corfu-terminal = "https://codeberg.org/akib/emacs-corfu-terminal";
    # Dropped from Melpa, see https://github.com/melpa/melpa/pull/8106#issuecomment-2425152728
    lean-mode = "https://github.com/leanprover/lean3-mode";
    company-lean = "https://github.com/leanprover/lean3-mode";
  };

  # Pins for packages not pinned by Doom and not in nixpkgs or emacs-overlay.
  extraPins = {
    # Looks stable enough we can get away with pinning it.
    sly-stepper = "da84e3bba8466c2290c2dc7c27d7f4c48c27b39e";
    # In emacsattic, so shouldn't change underneath us.
    ammonite-term-repl = "b552fe21977e005c1c460bf6607557e67241a6b6";
  };

  # :files passed in to melpa2nix (currently only if not already present in recipe).
  extraFiles = {
    # These build from the same repository. Without this, lean-mode does not build because
    # we try to build company-lean without depending on company.
    lean-mode = ''("lean-*.el")'';
    company-lean = ''("company-lean.el")'';
  };
}
