// Monocle.
// Copyright (C) 2019-2020 Monocle authors

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

import axios from 'axios'
import moment from 'moment'

var server = process.env.REACT_APP_API_URL || 'http://localhost:9876'
var baseurl = server + '/api/0'

console.log('BaseURL=' + baseurl)

function setQueryParams (
  {
    gte = undefined, lte = undefined,
    type = undefined,
    excludeAuthors = undefined,
    authors = undefined,
    from = undefined,
    size = undefined,
    changeIds = undefined
  }) {
  var params = new URLSearchParams()
  if (gte) {
    params.append('gte', gte)
  } else {
    params.append(
      'gte', moment().subtract(3, 'months')
        .format('YYYY-MM-DD'))
  }
  if (lte) {
    params.append('lte', lte)
  }
  if (type) {
    params.append('type', type)
  }
  if (excludeAuthors) {
    params.append('exclude_authors', excludeAuthors)
  }
  if (authors) {
    params.append('authors', authors)
  }
  if (from) {
    params.append('from', from)
  }
  if (size) {
    params.append('size', size)
  }
  if (changeIds) {
    params.append('change_ids', changeIds)
  }
  return params
}

function getQueryResults (
  {
    name, repository, index, gte = undefined,
    lte = undefined, type = undefined,
    excludeAuthors = undefined,
    authors = undefined, from = undefined, size = undefined,
    changeIds = undefined
  }) {
  const url = baseurl + '/query/' + name
  var params = setQueryParams(
    {
      gte: gte,
      lte: lte,
      type: type,
      excludeAuthors: excludeAuthors,
      authors: authors,
      from: from,
      size: size,
      changeIds: changeIds
    }
  )
  params.append('repository', repository)
  params.append('index', index)
  params.append('ec_same_date', true)
  return axios.get(
    url, {
      params: params
    })
}

export {
  getQueryResults
}
