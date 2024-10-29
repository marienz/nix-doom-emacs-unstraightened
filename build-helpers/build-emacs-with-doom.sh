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

mkdir -p "$out/bin"
ln -s "$emacs"/bin/* "$out/bin/"
rm "$out"/bin/emacs-*
ln -sf "$doomEmacs/bin/doom-emacs" "$out/bin/emacs"
ln -sf "$doomEmacs"/bin/{doom,doomscript} "$out/bin/"

mkdir -p "$out/share"
# Don't link everything: the systemd units would still refer to normal Emacs.
# This links the same stuff emacsWithPackages does.
for dir in applications icons info man; do
    ln -s "$emacs/share/$dir" "$out/share/$dir"
done

if [ -d "$emacs/Applications" ]; then 
    cp -R "$emacs/Applications" "$out/Applications"
    chmod -R u+w "$out/Applications"
    ln -sf "$out/bin/emacs" "$out/Applications/Emacs.app/Contents/MacOS/Emacs"
fi
