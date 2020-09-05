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
import Alert from 'react-bootstrap/Alert'
import Badge from 'react-bootstrap/Badge'
import Spinner from 'react-bootstrap/Spinner'
import PropTypes from 'prop-types'

import { query } from '../reducers/query'

function getWindowDimensions () {
  const { innerWidth: width, innerHeight: height } = window
  return {
    width,
    height
  }
}

function hasSmallWidth () {
  const { width } = getWindowDimensions()

  return width <= 500
}

function changeUrl (index, x, name = null) {
  if (!name) {
    name = x.change_id
  }
  return <Link to={`/${index}/change/${x.change_id}${window.location.search}`}>{name}</Link>
}

function addUrlField (field, value) {
  const url = new URL(window.location.href)

  url.searchParams.set(field, value)

  return url.pathname + url.search
}

function indexUrl (index, dest) {
  const url = new URL(window.location.href)

  url.pathname = `/${index}${dest}`
  return url.search ? url.pathname + url.search : url.pathname
}

function addS (count, s = 's') {
  if (count > 1) {
    return s
  } else {
    return ''
  }
}

function addMap (dict, reducer, name) {
  dict[name + '_loading'] = reducer[name + '_loading']
  dict[name + '_result'] = reducer[name + '_result']
  dict[name + '_error'] = reducer[name + '_error']
  return dict
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

function chooseApprovalBadgeStyle (app, idx = 0) {
  if (app === null) {
    return ''
  }
  if (app === 'REVIEW_REQUIRED' || app === 'CHANGES_REQUESTED' || app === 'APPROVED') {
    let approvalCat = 'success'
    if (app === 'REVIEW_REQUIRED') {
      approvalCat = 'info'
    }
    if (app === 'CHANGES_REQUESTED') {
      approvalCat = 'danger'
    }
    return <Badge variant={approvalCat} key={idx}>{app}</Badge>
  } else {
    const patt = new RegExp('.*-.$')
    let approvalCat = 'success'
    if (patt.test(app)) {
      approvalCat = 'danger'
    }
    if (app.includes('+0')) {
      approvalCat = 'info'
    }
    return <Badge variant={approvalCat} key={idx}>{app}</Badge>
  }
}

class MergeableStatusBadge extends React.Component {
  render () {
    const mergeable = this.props.mergeable.toLowerCase()
    switch (mergeable) {
      case 'mergeable':
        return <Badge variant="success">{mergeable}</Badge>
      case 'conflicting':
        return <Badge variant="warning">{mergeable}</Badge>
      case 'unknown':
        return <Badge variant="secondary">{mergeable}</Badge>
      default:
        return null
    }
  }
}

MergeableStatusBadge.propTypes = {
  mergeable: PropTypes.string.isRequired
}

class ChangeStatus extends React.Component {
  render () {
    if (this.props.data.state === 'OPEN' && this.props.data.draft) {
      return <Badge variant="dark">Draft</Badge>
    }
    switch (this.props.data.state) {
      case 'OPEN':
        return (
          <span>
            <Badge variant="success">Open</Badge>{' '}
            <MergeableStatusBadge mergeable={this.props.data.mergeable} />
          </span>
        )
      case 'MERGED':
        return <Badge variant="primary">Merged</Badge>
      case 'CLOSED':
        return <Badge variant="danger">Abandoned</Badge>
      default:
        return null
    }
  }
}

ChangeStatus.propTypes = {
  data: PropTypes.object
}

class LoadingBox extends React.Component {
  render () {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <Spinner animation="border" role="status" /> <span>Loading...</span>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

class ErrorBox extends React.Component {
  render () {
    const style = { textAlign: 'center' }
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <p style={style}>
                Error: code: {this.props.error ? this.props.error.status : 'none'},
                message: {this.props.error ? this.props.error.data : 'none'}
              </p>
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

class SmallSizeWarning extends React.Component {
  render () {
    const { width } = getWindowDimensions()
    return <React.Fragment>
      {
        (width < 1024)
          ? <Row>
            <Col>
              <Alert
                variant='warning'
                className='text-center'
              >
                To get the best experience please use a device
                with a larger screen or rotate your device.
              </Alert>
            </Col>
          </Row>
          : ''
      }

    </React.Fragment>
  }
}

class BaseQueryComponent extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      pathname: null,
      name: null, // must be set by sub-class
      graph_type: null, // must be set by sub-class
      pageSize: 10,
      selectedPage: 0,
      forceAllAuthors: false, // could be set by sub-class
      state: null // could be set by sub-class
    }
    this.handlePageChange.bind(this)
    this.queryBackend.bind(this)
  }

  componentDidMount () {
    this.setState({ pathname: window.location.pathname })
    this.queryBackend(this.state.selectedPage)
    this.unlisten = this.props.history.listen((location, action) => {
      // send a query only when the filter has changed parameters and
      // we are still on the same page
      if (this.state.pathname === location.pathname) {
        this.queryBackend()
      }
    })
  }

  componentWillUnmount () {
    if (this.unlisten) {
      this.unlisten()
    }
  }

  handlePageChange (obj, pageData) {
    obj.setState({ selectedPage: pageData.selected })
    obj.queryBackend(pageData.selected)
  }

  queryBackend (start = 0) {
    const params = (new URL(window.location.href)).searchParams
    this.setState({ state: params.get('state') })
    var queryParams = {}
    // if we have a changeIds, don't pass other non mandatory filters
    if (this.props.changeIds) {
      queryParams = {
        index: this.props.index,
        name: this.state.name,
        graph_type: this.state.graph_type,
        repository: params.get('repository') || '.*',
        branch: params.get('branch'),
        gte: params.get('gte'),
        changeIds: this.props.changeIds
      }
    } else {
      queryParams = {
        index: this.props.index,
        name: this.state.name,
        repository: params.get('repository') || '.*',
        branch: params.get('branch'),
        files: params.get('files'),
        gte: params.get('gte'),
        lte: params.get('lte'),
        excludeAuthors: params.get('exclude_authors'),
        authors: this.state.forceAllAuthors ? null : params.get('authors'),
        graph_type: this.state.graph_type,
        from: start * this.state.pageSize,
        size: this.state.pageSize
      }
      if (['last_changes', 'repos_top', 'authors_top', 'approvals_top'].includes(this.state.name)) {
        // Merge both associative arrays
        queryParams = {
          ...queryParams,
          ...{
            approvals: params.get('approvals'),
            excludeApprovals: params.get('exclude_approvals'),
            state: params.get('state') === 'SELF-MERGED' ? 'MERGED' : params.get('state'),
            selfMerged: params.get('state') === 'SELF-MERGED'
          }
        }
      }
    }
    this.props.handleQuery(queryParams)
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
  changeIds: PropTypes.string,
  history: PropTypes.object.isRequired
}

export {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent,
  changeUrl,
  addUrlField,
  indexUrl,
  addS,
  addMap,
  mapDispatchToProps,
  chooseApprovalBadgeStyle,
  getWindowDimensions,
  SmallSizeWarning,
  hasSmallWidth,
  ChangeStatus
}
