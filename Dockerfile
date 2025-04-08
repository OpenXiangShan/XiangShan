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

FROM ghcr.io/openxiangshan/xs-env:latest

# override ENTRYPOINT in verilator image
ENTRYPOINT [ "/bin/bash" ]
ENV VERILATOR=/usr/local/bin/verilator-wrap.sh

# explict set LC_ALL to C.UTF-8
ENV LC_ALL=C.UTF-8

#
# Mill
#
COPY .mill-version /tmp
RUN cd /tmp && mill -i --version && rm -rf .mill-version out

#
# XiangShan workspace
#
WORKDIR /work
VOLUME /work/out
VOLUME /work/build

#
# download dependencies
#
RUN --mount=type=bind,source=.,target=/work,readonly \
    --mount=type=tmpfs,destination=/work/out,rw <<EOF
make deps
EOF
