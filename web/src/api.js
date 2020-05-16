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

const server = window.API_URL !== '__API_URL__' ? window.API_URL : (process.env.REACT_APP_API_URL || 'http://localhost:9876')
const baseurl = server + '/api/0'

console.log('BaseURL=' + baseurl)

function getQueryResults (queryParams) {
  const params = { ...queryParams }
  const url = baseurl + '/query/' + params.name

  params.ec_same_date = true
  delete params.name
  delete params.graph_type

  if (!params.gte) {
    params.gte = moment().subtract(3, 'months').format('YYYY-MM-DD')
  }

  if (params.changeIds) {
    params.change_ids = params.changeIds
    delete params.changeIds
  }

  if (params.excludeAuthors) {
    params.exclude_authors = params.excludeAuthors
    delete params.excludeAuthors
  }

  if (params.branch) {
    params.target_branch = params.branch
    delete params.branch
  }

  return axios.get(
    url, {
      params: params,
      withCredentials: true
    })
}

function getIndices () {
  const url = baseurl + '/indices'
  return axios.get(url, {
    withCredentials: true
  })
}

function getLoggedUser () {
  const url = baseurl + '/whoami'

  return axios.get(
    url, {
      params: {},
      withCredentials: true
    })
}

export {
  getQueryResults,
  getIndices,
  getLoggedUser,
  baseurl
}
