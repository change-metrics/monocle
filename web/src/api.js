import axios from 'axios'

var server = 'http://localhost:9876'
var baseurl = server + '/api/0'

function setQueryParams(
  { gte = undefined, lte = undefined,
    type = undefined, interval = undefined }) {
  var params = new URLSearchParams()
  if (gte) {
    params.append('gte', gte)
  }
  if (lte) {
    params.append('lte', lte)
  }
  if (type) {
    params.append('type', type)
  }
  if (interval) {
    params.append('interval', interval)
  }
  return params
}

function get_query_results(
  { name, repository, gte = undefined,
    lte = undefined, type = undefined,
    interval = undefined}) {
  const url = baseurl + '/query/' + name
  var params = setQueryParams(
    {
      'gte': gte,
      'lte': lte,
      'type': type,
      'interval': interval,
      'ec-same-date': true,
      'interval': '7d',
    }
  )
  params.append('repository', repository)
  return axios.get(
    url, {
    params: params
  })
}

export {
  get_query_results,
}
