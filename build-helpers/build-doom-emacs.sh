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

if [[ -z "$profileName" ]]; then
    profileArgs=()
else
    profileArgs=(
        --set DOOMPROFILELOADFILE $doomProfile/loader/init.el
        --set DOOMPROFILE "$profileName"
    )
fi
common=()
if [[ -n "$binPath" ]]; then
    common+=(
        --suffix PATH : "$binPath"
    )
fi

makeWrapper $emacsWithPackages/bin/emacs $out/bin/doom-emacs \
    "${profileArgs[@]}" \
    "${common[@]}" \
    --set DOOMDIR $doomProfile/doomdir \
    --set-default DOOMLOCALDIR "$doomLocalDir" \
    --add-flags "--init-directory=$doomSource"
makeWrapper $doomSource/bin/doomscript $out/bin/doomscript \
    "${common[@]}" \
    --set EMACS $emacsWithPackages/bin/emacs \
    --set-default DOOMLOCALDIR "$doomLocalDir"
makeWrapper $doomSource/bin/doom $out/bin/doom \
    "${common[@]}" \
    --set EMACS $emacsWithPackages/bin/emacs \
    "${profileArgs[@]}" \
    --set DOOMDIR $doomProfile/doomdir \
    --set-default DOOMLOCALDIR "$doomLocalDir"
