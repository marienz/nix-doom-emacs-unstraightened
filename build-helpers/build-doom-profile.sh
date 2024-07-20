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

mkdir $out $out/loader $out/doomdir $out/profile $out/straight doomlocaldir home
ln -s $doomDir/* $out/doomdir/
# yasnippet logs an error at startup if snippets/ does not exist.
if ! [[ -e $out/doomdir/snippets ]]; then
    mkdir $out/doomdir/snippets
fi
rm $out/doomdir/init.el
if [[ -z "$profileName" ]]; then
    maybeSetProfileDir="(setq doom-profile-dir \"$out/profile\")"
else
    maybeSetProfileDir=""
fi
substitute $initEl $out/doomdir/init.el \
    --subst-var maybeSetProfileDir \
    --subst-var profileName \
    --subst-var-by userInit "$doomDir/init.el" \
    --subst-var-by straightBaseDir $out
ln -sf $doomIntermediates/packages.el $out/doomdir/
export DOOMDIR=$out/doomdir

# DOOMLOCALDIR must be writable, Doom creates some subdirectories.
export DOOMLOCALDIR="$PWD/doomlocaldir"
if [[ -n "$profileName" ]]; then
    export DOOMPROFILELOADFILE=$out/loader/init.el
    $runtimeShell $doomSource/bin/doomscript $buildProfileLoader \
        ${noProfileHack:+-u} -n "$profileName" -b "$out"

    # With DOOMPROFILE set, doom-state-dir and friends are HOME-relative.
    export HOME="$PWD/home"
    export DOOMPROFILE="$profileName";
fi
$runtimeShell $doomSource/bin/doomscript $buildProfile \
    -l $deps/share/emacs/site-lisp

# Similar to audit-tmpdir.sh in nixpkgs.
if grep -q -F "$TMPDIR/" -r $out; then
    echo "Doom profile contains a forbidden reference to $TMPDIR/"
    exit 1
fi
