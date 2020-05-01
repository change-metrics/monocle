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

FROM python:3.7-alpine
WORKDIR /code
COPY requirements.txt requirements.txt
RUN apk --no-cache add build-base libffi-dev openssl-dev
RUN pip install -r requirements.txt
COPY monocle monocle
COPY setup.py setup.py
RUN python setup.py install

FROM python:3.7-alpine
RUN mkdir /etc/monocle
COPY --from=0 /usr/local/lib/python3.7/site-packages /usr/local/lib/python3.7/site-packages
COPY --from=0 /usr/local/bin /usr/local/bin
