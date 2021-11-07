#!/bin/sh -x
# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

# so we don't use anything in /app that doesn't have permission
temp_file=$(mktemp)
# this does basically what `sed -i` does, just without writing a
# intermedeiary file in the target folder
# with `sed -i` the output is:
# "sed: couldn't open temporary file XXX: Permission denied"
sed_temporal() {
  local expression=$1
  local target_file=$2
  sed -e "${expression}" "${target_file}" > "${temp_file}"
  cp "${temp_file}" "${target_file}"
  rm "${temp_file}"
}


# Apply runtime settings for the web interface:
test -z "$REACT_APP_API_URL" || sed_temporal "s@__API_URL__@$REACT_APP_API_URL@" /app/index.html;
test -z "$REACT_APP_TITLE"   || sed_temporal "s@__TITLE__@$REACT_APP_TITLE@" /app/index.html;

# Start the web server
exec nginx -g "daemon off;"
