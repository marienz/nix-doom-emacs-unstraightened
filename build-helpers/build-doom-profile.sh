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
ln -sf $doomIntermediates/packages.el $out/doomdir/
export DOOMDIR=$out/doomdir

# DOOMLOCALDIR must be writable, Doom creates some subdirectories.
export DOOMLOCALDIR="$PWD/doomlocaldir"
$runtimeShell $doomSource/bin/doomscript $buildProfileLoader \
    ${noProfileHack:+-u} -n "$profileName" -m "$doomModules" -b "$out"

# With DOOMPROFILE set, doom-state-dir and friends are HOME-relative.
export HOME="$PWD/home"
export DOOMPROFILE="$profileName";
export DOOMPROFILELOADFILE=$out/loader/init
$runtimeShell $doomSource/bin/doomscript $buildProfile \
    -i $initEl \
    -l $deps/share/emacs/site-lisp

# Similar to audit-tmpdir.sh in nixpkgs.
if grep -q -F "$TMPDIR/" -r $out; then
    echo "Doom profile contains a forbidden reference to $TMPDIR/"
    exit 1
fi

# Nix normalizes the permissions of regular files in $out to a world-readable
# mode after the build, but (deliberately) leaves symlinks alone, since Linux
# does not support chmod'ing (or even meaningfully storing the mode of) a
# symlink. Any symlink created with a restrictive umask (e.g. by Emacs' own
# `with-file-modes' in doom-profile-generate) therefore ships into the store
# exactly as created.
#
# That is harmless on Linux (permission bits on symlinks are not checked
# there). It is also harmless in the common case on macOS, because merely
# *following* a symlink (e.g. opening the file it points to) never checks its
# permission bits either, on any OS. It only matters on macOS when something
# calls readlink(2) to retrieve the link's literal target string instead of
# following it, which is exactly what building a NAR to push to a binary
# cache requires: `cachix push' (and any other NAR-producing tool) fails to
# read such a symlink when run as anyone other than its owner or group, e.g.
# the invoking CI user reading a path built by the nix-daemon build user.
# Fail the build if that happens again instead of shipping a broken store
# path silently.
if find "$out" -type l ! -perm -004 -print | grep -q .; then
    echo "Doom profile contains symlinks that are not world-readable:"
    find "$out" -type l ! -perm -004 -ls
    exit 1
fi
