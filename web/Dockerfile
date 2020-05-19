# Monocle.
# Copyright (C) 2019-2020 Monocle authors
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
FROM node:12-alpine

RUN apk --no-cache add bash

# set working directory
WORKDIR /app

# add `/app/node_modules/.bin` to $PATH
ENV PATH /app/node_modules/.bin:$PATH

# copy files to install js modules
COPY package.json /app/
COPY package-lock.json /app/

# install dependencies
RUN npm install

# copy source files
COPY public /app/public/
COPY src /app/src/

# install dependencies
RUN npm run build

################################################################################

FROM node:12-alpine

# set working directory
WORKDIR /app

# install serve globally
RUN npm install -g serve

COPY --from=0 /usr/lib/ /usr/lib/
COPY --from=0 /bin/ /bin/
COPY --from=0 /app/build /app

# start app
CMD bash -c "set -x; test -z \"$REACT_APP_API_URL\" || sed -i -e \"s@__API_URL__@$REACT_APP_API_URL@\" /app/index.html; test -z \"$REACT_APP_TITLE\" || sed -i -e \"s@__TITLE__@$REACT_APP_TITLE@\" /app/index.html; serve -s /app -l 3000"
