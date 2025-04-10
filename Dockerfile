#***************************************************************************************
# Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************

FROM verilator/verilator:latest

# override ENTRYPOINT in verilator image
ENTRYPOINT [ "/bin/bash" ]

#
# Prepare envrionments for chisel/mill
#
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive \
    && apt-get install --no-install-recommends -y \
			default-jdk \
			python3 \
			curl \
			make \
			time \
			git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

#
# Mill
#
RUN curl -L https://github.com/com-lihaoyi/mill/releases/download/0.12.3/0.12.3 -o /usr/bin/mill \
	&& chmod +x /usr/bin/mill \
	&& mill --version

#
# XiangShan workspace
#
WORKDIR /work
VOLUME /work/out
VOLUME /work/build

#
# download IvyDeps
#
RUN --mount=type=bind,source=.,target=/work,readonly \
    --mount=type=tmpfs,destination=/work/out,rw <<EOF
mill -i _.resolvedIvyDeps
mill -i utility.compile
EOF
