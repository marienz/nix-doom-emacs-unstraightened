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
    # In nixpkgs, but uses codeberg, for which nixpkgs uses fetchzip.
    # TODO: consider parsing origEPkg.src.url instead.
    tree-sitter-indent = "https://codeberg.org/FelipeLema/tree-sitter-indent.el.git";
    undo-fu = "https://codeberg.org/ideasman42/emacs-undo-fu.git";
    undo-fu-session = "https://codeberg.org/ideasman42/emacs-undo-fu-session.git";
    consult-notmuch = "https://codeberg.org/jao/consult-notmuch.git";
    # sourcehut fetcher doesn't work for the same reason codeberg doesn't.
    fennel-mode = "https://git.sr.ht/~technomancy/fennel-mode";
    ol-notmuch = "https://git.sr.ht/~tarsius/ol-notmuch";
    # nixpkgs uses a release from nongnu ELPA.
    corfu-terminal = "https://codeberg.org/akib/emacs-corfu-terminal";

    # Melpa and emacs-overlay updated to https://gitlab.com/emacs-ansible/emacs-ansible,
    # but Doom seems to use the old location via el-get.
    # TODO: check if this becomes obsolete and/or el-get/doom need updating later.
    ansible = "https://github.com/k1LoW/emacs-ansible";
  };

  # Pins for packages not pinned by Doom and not in nixpkgs or emacs-overlay.
  extraPins = {
    # Looks stable enough we can get away with pinning it.
    sly-stepper = "da84e3bba8466c2290c2dc7c27d7f4c48c27b39e";
    # In emacsattic, so shouldn't change underneath us.
    ammonite-term-repl = "b552fe21977e005c1c460bf6607557e67241a6b6";

    # Temporarily override git-commit, which Doom pins to an earlier commit than magit
    # (built from the same repo). When building with straight, the magit pin seems to "win",
    # making this a non-issue for Doom (since there have been no significant changes to git-commit).
    # CI should fail again the next time Doom updates magit, so this won't go stale.
    git-commit = "ea0f07e54967197ac0b072a69ba314314a4080c1";

    # Temporarily pin opencl-mode. It renamed to opencl-c-mode, which triggers a build failure:
    #
    # error("%s" "Single file opencl-c-mode.el does not match package name opencl-mode")
    opencl-mode = "15091eff92c33ee0d1ece40eb99299ef79fee92d";
  };
}
