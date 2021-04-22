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

FROM registry.fedoraproject.org/fedora:33
RUN dnf update -y && dnf install -y python3-pip python3-devel openssl-devel gcc && dnf clean all
WORKDIR /code
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY monocle monocle
COPY setup.py setup.py
RUN mkdir /etc/monocle && python setup.py install
ENV LANG=C.UTF-8
