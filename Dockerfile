# Monocle.
# Copyright (C) 2021 Monocle authors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# base image
FROM quay.io/change-metrics/builder

# optional build argument
ARG MONOCLE_COMMIT

# Build project
COPY haskell/ /build
RUN cabal v2-install -v1 exe:monocle

# web build
FROM registry.fedoraproject.org/fedora:35

RUN dnf update -y && dnf install -y nodejs git

# set working directory
WORKDIR /monocle-webapp
ENV PATH /monocle-webapp/node_modules/.bin:$PATH

# copy files to install js modules
COPY web/package.json /monocle-webapp/
COPY web/bsconfig.json /monocle-webapp/
COPY web/package-lock.json /monocle-webapp/

# install dependencies
RUN npm install

# copy source files
COPY web/build.js /monocle-webapp/
COPY web/public /monocle-webapp/public/
COPY web/src /monocle-webapp/src/

# install dependencies (sed is ugly hack to make warning errors)
RUN sed -e 's|-bs-no-version-header|-bs-no-version-header", "-warn-error -a+5+6+27+101+109|' -i bsconfig.json && npm run build

################################################################################
FROM registry.fedoraproject.org/fedora:35

COPY --from=0 /root/.cabal/bin/monocle /bin/
COPY --from=1 /monocle-webapp/build /usr/share/monocle/webapp/
