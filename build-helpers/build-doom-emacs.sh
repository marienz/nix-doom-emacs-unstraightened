# -*- mode: sh; sh-shell: bash -*-

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

commonArgs=(
    --set DOOMPROFILELOADFILE $doomProfile/loader/init
    --set DOOMPROFILE "$profileName"
    --set-default DOOMLOCALDIR "$doomLocalDir"
    --set DOOMDIR $doomProfile/doomdir
)
if [[ -n $lspUsePlists ]]; then
    commonArgs+=(
        --set LSP_USE_PLISTS 1
    )
fi

makeWrapper $emacsWithPackages/bin/emacs $out/bin/doom-emacs \
    "${commonArgs[@]}" \
    --add-flags "--init-directory=$doomSource"
makeWrapper $doomSource/bin/doomscript $out/bin/doomscript \
    --set EMACS $emacsWithPackages/bin/emacs \
    "${commonArgs[@]}"
makeWrapper $doomSource/bin/doom $out/bin/doom \
    --set EMACS $emacsWithPackages/bin/emacs \
    "${commonArgs[@]}"
