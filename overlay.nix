/*
   Copyright 2016-2017 SiFive, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

final: prev: {
  mill = prev.mill.overrideAttrs (oldAttrs: rec {
    version = "0.11.1";
    src = prev.fetchurl {
      url = "https://github.com/com-lihaoyi/mill/releases/download/${version}/${version}-assembly";
      hash = "sha256-qG+Ddn0BHUZX1VX5hO84exgRz8YuUgYF/fH6MmgkrXE=";
    };
  });

  verilator-5016 = prev.verilator.overrideAttrs (oldAttrs: rec {
    pname = "verilator";
    version = "5.016";
    src = prev.fetchFromGitHub {
      owner = pname;
      repo = pname;
      rev = "v${version}";
      hash = "sha256-MVQbAZXSIdzX7+yKbSrFLLd0j6dfLSXpES3uu6bcPt8=";
    };
  });
  
}
