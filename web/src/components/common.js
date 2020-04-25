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

import React from 'react'
import { Link } from 'react-router-dom'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import PropTypes from 'prop-types'

function changeUrl (index, x, name = null) {
  if (!name) {
    name = x.change_id
  }
  return <Link to={`/${index}/change/${x.change_id}${window.location.search}`}>{name}</Link>
}

function addUrlField (field, value) {
  var url = new URL(window.location.href)

  url.searchParams.set(field, value)

  return url.pathname + url.search
}

function newRelativeUrl (dest) {
  var url = new URL(window.location.href)

  url.pathname += dest
  return url.search ? url.pathname + url.search : url.pathname
}

function addS (count, s = 's') {
  if (count > 1) {
    return s
  } else {
    return ''
  }
}

class LoadingBox extends React.Component {
  render () {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <h1>
                loading
              </h1>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

class ErrorBox extends React.Component {
  render () {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <h1>
                Error: code: {this.props.error.status},
                message: {this.props.error.data}
              </h1>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

ErrorBox.propTypes = {
  error: PropTypes.shape({
    status: PropTypes.number.isRequired,
    data: PropTypes.string.isRequired
  })
}

class BaseQueryComponent extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      name: null, // must be set by sub-class
      graph_type: null, // must be set by sub-class
      pageSize: 10,
      selectedPage: 0,
      forceAllAuthors: false // could be set by sub-class
    }
    this.handlePageChange.bind(this)
    this.queryBackend.bind(this)
    if (this.props.history !== undefined) {
      this.props.history.listen((location, action) => {
        this.queryBackend()
      })
    }
  }

  componentDidMount () {
    this.queryBackend(this.state.selectedPage)
  }

  handlePageChange (obj, pageData) {
    obj.setState({ selectedPage: pageData.selected })
    obj.queryBackend(pageData.selected)
  }

  queryBackend (start = 0) {
    const params = (new URL(window.location.href)).searchParams
    this.props.handleQuery({
      index: this.props.index,
      name: this.state.name,
      repository: params.get('repository') || '.*',
      gte: params.get('gte'),
      lte: params.get('lte'),
      excludeAuthors: params.get('exclude_authors'),
      authors: this.state.forceAllAuthors ? null : params.get('authors'),
      graph_type: this.state.graph_type,
      from: start * this.state.pageSize,
      size: this.state.pageSize,
      changeIds: this.props.changeIds
    })
  }
}

BaseQueryComponent.propTypes = {
  index: PropTypes.string.isRequired,
  filter_repository: PropTypes.string,
  filter_gte: PropTypes.string,
  filter_lte: PropTypes.string,
  filter_authors: PropTypes.string,
  filter_exclude_authors: PropTypes.string,
  filter_loaded_from_url: PropTypes.bool,
  handleQuery: PropTypes.func.isRequired,
  changeIds: PropTypes.array,
  history: PropTypes.object.isRequired
}

export {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent,
  changeUrl,
  addUrlField,
  newRelativeUrl,
  addS
}
