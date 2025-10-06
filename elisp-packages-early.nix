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

# Overrides applied before Doom's pins.
#
# Use sparingly: these override the original derivation even if Doom's pins are not applied to it
# (if the package is used as a transitive dependency without Doom's module for it being enabled).
# This means packages defined here must build as-is / must still build without src overridden.

{
  eself,
  esuper,
}:
{
  # Upstream renamed from opencl-mode to opencl-c-mode. melpa2nix requires single-file-package file
  # names match the package name. So rename the package (not the file, just in case someone loads it
  # explicitly).
  opencl-mode = esuper.opencl-c-mode;
}
