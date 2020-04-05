import axios from 'axios'

var server = process.env.REACT_APP_API_URL || 'http://localhost:9876'
var baseurl = server + '/api/0'

console.log('BaseURL=' + baseurl)

function setQueryParams(
  { gte = undefined, lte = undefined,
    type = undefined, interval = undefined,
    exclude_authors = undefined,
    authors = undefined,
    from = undefined,
    size = undefined,
  }) {
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
  if (exclude_authors) {
    params.append('exclude_authors', exclude_authors)
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
  return params
}

function get_query_results(
  { name, repository, index, gte = undefined,
    lte = undefined, type = undefined,
    interval = undefined, exclude_authors = undefined,
    authors = undefined, from = undefined, size = undefined}) {
  const url = baseurl + '/query/' + name
  var params = setQueryParams(
    {
      'gte': gte,
      'lte': lte,
      'type': type,
      'interval': interval,
      'exclude_authors': exclude_authors,
      'authors': authors,
      'from': from,
      'size': size,
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
  get_query_results,
}
