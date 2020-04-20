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

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'
import ReactPaginate from 'react-paginate'
import PropTypes from 'prop-types'

import moment from 'moment'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  changeUrl,
  addUrlField,
  newRelativeUrl
} from './common'

import ComplexityGraph from './complexity_graph'
import DurationComplexityGraph from './duration_complexity_graph'

class RepoChangesTable extends React.Component {
  render () {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    <th className="text-center">Repository</th>
                    <th className="text-center">Number of changes</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items.map((item, index) =>
                    <tr key={index}>
                      <td align="center"><Link to={addUrlField('repository', item.key)}>{item.key}</Link></td>
                      <td align="center">{item.doc_count}</td>
                    </tr>)}
                </tbody>
              </Table>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

RepoChangesTable.propTypes = {
  title: PropTypes.string.isRequired,
  data: PropTypes.shape({
    items: PropTypes.array
  })
}

class RepoChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'repos_top_merged'
    this.state.graph_type = 'repos_top_merged'
  }

  render () {
    if (!this.props.repos_top_merged_loading) {
      const data = this.props.repos_top_merged_result
      return (
        <RepoChangesTable
          data={data}
          title="Merged changes by repository"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class ChangesTable extends React.Component {
  render () {
    let paginationElement
    let graphElement

    if (!this.props.data || !this.props.data.items) {
      return <ErrorBox error={{ status: 0, data: 'Invalid data' }}/>
    }

    if (this.props.graph) {
      graphElement = <React.Fragment>{this.props.graph}<br/></React.Fragment>
    }

    if (this.props.pageChangeCallback && this.props.pageCount > 1) {
      paginationElement = <ReactPaginate
        forcePage={this.props.selectedPage}
        pageCount={this.props.pageCount}
        pageRangeDisplayed={5}
        marginPagesDisplayed={4}
        onPageChange={data => this.props.pageChangeCallback(this.props.pageChangeTarget, data)}
        breakClassName={'page-item'}
        breakLinkClassName={'page-link'}
        containerClassName={'pagination'}
        pageClassName={'page-item'}
        pageLinkClassName={'page-link'}
        previousClassName={'page-item'}
        previousLinkClassName={'page-link'}
        nextClassName={'page-item'}
        nextLinkClassName={'page-link'}
        activeClassName={'active'}
      />
    }
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              {graphElement}
              {paginationElement}
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    {this.props.created ? <th className="text-center">Created</th> : null}
                    {this.props.updated ? <th className="text-center">Updated</th> : null}
                    {this.props.merged ? <th className="text-center">Merged</th> : null}
                    {this.props.duration ? <th className="text-center">Duration</th> : null}
                    <th className="text-center">Repository</th>
                    <th className="text-center">Author</th>
                    <th className="text-center">Title</th>
                    {this.props.mergeable ? <th className="text-center">Mergeable</th> : null}
                    <th className="text-center">Complexity</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items && this.props.data.items.map((x, index) =>
                    <tr key={index}>
                      {this.props.created ? <td align="center">{moment(x.created_at).fromNow()}</td> : null}
                      {this.props.updated ? <td align="center">{moment(x.updated_at).fromNow()}</td> : null}
                      {this.props.merged ? <td align="center">{moment(x.merged_at).fromNow()}</td> : null}
                      {this.props.duration ? <td align="center">{moment.duration(x.duration, 'seconds').humanize()}</td> : null}
                      <td align="center"><Link to={addUrlField('repository', x.repository_fullname)}>{x.repository_fullname}</Link></td>
                      <td align="center"><Link to={addUrlField('authors', x.author)}>{x.author}</Link></td>
                      <td>{changeUrl(this.props.index, x, x.title)}</td>
                      {this.props.mergeable ? <td align="center">{x.mergeable}</td> : null}
                      <td align="center">{ x.complexity }</td>
                    </tr>)}
                </tbody>
              </Table>
              {paginationElement}
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

ChangesTable.propTypes = {
  title: PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.element
  ]).isRequired,
  data: PropTypes.shape({
    items: PropTypes.array
  }),
  pageCount: PropTypes.number,
  selectedPage: PropTypes.number,
  pageChangeCallback: PropTypes.func,
  pageChangeTarget: PropTypes.object,
  created: PropTypes.bool,
  updated: PropTypes.bool,
  merged: PropTypes.bool,
  mergeable: PropTypes.bool,
  duration: PropTypes.bool,
  graph: PropTypes.element,
  index: PropTypes.string.isRequired
}

class HotChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'hot_changes'
    this.state.graph_type = 'hot_changes'
  }

  render () {
    if (!this.props.hot_changes_loading) {
      if (this.props.hot_changes_error) {
        return <ErrorBox
          error={this.props.hot_changes_error}
        />
      }
      const data = this.props.hot_changes_result
      return (
        <ChangesTable
          index={this.props.index}
          data={data}
          title="Hot changes"
          created={true}
          updated={true}
          mergeable={true}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class ColdChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'cold_changes'
    this.state.graph_type = 'cold_changes'
  }

  render () {
    if (!this.props.cold_changes_loading) {
      if (this.props.cold_changes_error) {
        return <ErrorBox
          error={this.props.cold_changes_error}
        />
      }
      const data = this.props.cold_changes_result
      return (
        <ChangesTable
          index={this.props.index}
          data={data}
          title="Cold changes"
          created={true}
          updated={true}
          mergeable={true}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class LastChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_state_changed_changes'
    this.state.graph_type = 'last_changes'
  }

  render () {
    if (!this.props.last_changes_loading) {
      if (this.props.last_changes_error) {
        return <ErrorBox
          error={this.props.last_changes_error}
        />
      }
      const data = this.props.last_changes_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Recently Merged/Opened changes</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <ChangesTable
                      index={this.props.index}
                      data={data.merged_changes}
                      title={<Link to={newRelativeUrl('/merged-changes')}>Recently Merged Changes</Link>}
                      merged={true}
                      duration={true}
                    />
                  </Col>
                </Row>
                <Row><Col><p></p></Col></Row>
                <Row>
                  <Col>
                    <ChangesTable
                      index={this.props.index}
                      data={data.opened_changes}
                      title={<Link to={newRelativeUrl('/opened-changes')}>Recently Opened Changes</Link>}
                      created={true}
                      mergeable={true}
                    />
                  </Col>
                </Row>
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )
    } else {
      return <LoadingBox />
    }
  }
}

class AbstractLastChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.graph_type = 'last_changes'
    this.state.name = 'last_state_changed_changes'
    this.state.pageSize = 100
    this.state.created = false
    this.state.updated = false
    this.state.merged = false
    this.state.duration = false
  }

  render () {
    if (!this.props.last_changes_loading) {
      if (!this.props.last_changes_result) {
        return <ErrorBox error={{ status: 0, data: 'No data' }}/>
      }
      const data = this.extractData(this.props.last_changes_result)
      if (!data || data.items.length === 0) {
        return <ErrorBox error={{ status: 1, data: 'Invalid data' }}/>
      }
      return (
        <React.Fragment>
          <Row>
            <Col>
              <ChangesTable
                index={this.props.index}
                graph={this.state.duration
                  ? <DurationComplexityGraph
                    history={this.props.history}
                    data={data}
                    timeFunc={this.extractTime}
                    index={this.props.index}
                  />
                  : <ComplexityGraph
                    history={this.props.history}
                    data={data}
                    timeFunc={this.extractTime}
                    index={this.props.index}
                  />
                }
                data={data}
                title={data.total + ' ' + this.state.title}
                selectedPage={this.state.selectedPage}
                pageCount={Math.ceil(data.total / this.state.pageSize)}
                pageChangeCallback={this.handlePageChange}
                pageChangeTarget={this}
                created={this.state.created}
                updated={this.state.updated}
                merged={this.state.merged}
                duration={this.state.duration}
              />
            </Col>
          </Row>
        </React.Fragment>
      )
    } else {
      return <LoadingBox />
    }
  }
}

AbstractLastChanges.propTypes = {
  last_changes_loading: PropTypes.bool,
  last_changes_result: PropTypes.shape({
    merged_changes: PropTypes.shape({
      items: PropTypes.array
    }),
    opened_changes: PropTypes.shape({
      items: PropTypes.array
    })
  })
}

class LastMergedChanges extends AbstractLastChanges {
  constructor (props) {
    super(props)
    this.state.title = 'Merged Changes'
    this.state.merged = true
    this.state.duration = true
  }

  extractTime = x => x.merged_at
  extractData = x => x.merged_changes
}

class LastOpenedChanges extends AbstractLastChanges {
  constructor (props) {
    super(props)
    this.state.title = 'Opened Changes'
    this.state.created = true
    this.state.updated = true
  }

  extractTime = x => x.created_at
  extractData = x => x.opened_changes
}

class AbandonedChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_abandoned_changes'
    this.state.graph_type = 'last_abandoned_changes'
  }

  render () {
    if (!this.props.last_abandoned_changes_loading) {
      if (this.props.last_abandoned_changes_error) {
        return <ErrorBox
          error={this.props.last_abandoned_changes_error}
        />
      }
      const data = this.props.last_abandoned_changes_result
      return (
        <ChangesTable
          index={this.props.index}
          data={data}
          title={<Link to={newRelativeUrl('/abandoned-changes')}>Last Abandoned Changes</Link>}
          created={true}
          duration={true}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class AbandonedChangesFull extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_abandoned_changes'
    this.state.graph_type = 'last_abandoned_changes'
    this.state.pageSize = 100
  }

  extractTime = x => x.created_at

  render () {
    if (!this.props.last_abandoned_changes_loading) {
      if (this.props.last_abandoned_changes_error) {
        return <ErrorBox
          error={this.props.last_abandoned_changes_error}
        />
      }
      const data = this.props.last_abandoned_changes_result
      return (
        <ChangesTable
          index={this.props.index}
          data={data}
          title={data.total + ' Abandoned Changes'}
          created={true}
          duration={true}
          graph={<DurationComplexityGraph
            data={data}
            timeFunc={this.extractTime}
            index={this.props.index}
          />}
          selectedPage={this.state.selectedPage}
          pageCount={Math.ceil(data.total / this.state.pageSize)}
          pageChangeCallback={this.handlePageChange}
          pageChangeTarget={this}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = state => {
  return {
    repos_top_merged_loading: state.QueryReducer.repos_top_merged_loading,
    repos_top_merged_result: state.QueryReducer.repos_top_merged_result,
    repos_top_merged_error: state.QueryReducer.repos_top_merged_error,
    hot_changes_loading: state.QueryReducer.hot_changes_loading,
    hot_changes_result: state.QueryReducer.hot_changes_result,
    hot_changes_error: state.QueryReducer.hot_changes_error,
    cold_changes_loading: state.QueryReducer.cold_changes_loading,
    cold_changes_result: state.QueryReducer.cold_changes_result,
    cold_changes_error: state.QueryReducer.cold_changes_error,
    last_abandoned_changes_loading: state.QueryReducer.last_abandoned_changes_loading,
    last_abandoned_changes_result: state.QueryReducer.last_abandoned_changes_result,
    last_abandoned_changes_error: state.QueryReducer.last_abandoned_changes_error,
    last_changes_loading: state.QueryReducer.last_changes_loading,
    last_changes_result: state.QueryReducer.last_changes_result,
    last_changes_error: state.QueryReducer.last_changes_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CRepoChanges = connect(mapStateToProps, mapDispatchToProps)(RepoChanges)
const CHotChanges = connect(mapStateToProps, mapDispatchToProps)(HotChanges)
const CColdChanges = connect(mapStateToProps, mapDispatchToProps)(ColdChanges)
const CAbandonedChanges = connect(mapStateToProps, mapDispatchToProps)(AbandonedChanges)
const CAbandonedChangesFull = connect(mapStateToProps, mapDispatchToProps)(AbandonedChangesFull)
const CLastChanges = connect(mapStateToProps, mapDispatchToProps)(LastChanges)
const CLastMergedChanges = connect(mapStateToProps, mapDispatchToProps)(LastMergedChanges)
const CLastOpenedChanges = connect(mapStateToProps, mapDispatchToProps)(LastOpenedChanges)

export {
  CRepoChanges,
  CHotChanges,
  CColdChanges,
  CAbandonedChanges,
  CAbandonedChangesFull,
  CLastChanges,
  CLastMergedChanges,
  CLastOpenedChanges
}
