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

import { connect } from 'react-redux'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'
import ReactPaginate from 'react-paginate'
import PropTypes from 'prop-types'
import { withRouter, Link } from 'react-router-dom'

import moment from 'moment'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  changeUrl,
  addUrlField,
  indexUrl,
  mapDispatchToProps,
  addMap,
  chooseBadgeStyle
} from './common'

import ComplexityGraph from './complexity_graph'
import DurationComplexityGraph from './duration_complexity_graph'

class RepoChangesTable extends React.Component {
  render () {
    if (!this.props.data || !this.props.data.items) {
      return <ErrorBox error={{ status: 0, data: 'Invalid data' }} />
    }
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

const reposTopMergedMapStateToProps = state => addMap({}, state.QueryReducer, 'repos_top_merged')

const CRepoChanges = withRouter(connect(reposTopMergedMapStateToProps, mapDispatchToProps)(RepoChanges))

class ChangesTable extends React.Component {
  render () {
    let paginationElement
    let graphElement

    if (!this.props.data || !this.props.data.items) {
      return <ErrorBox error={{ status: 0, data: 'Invalid data' }} />
    }

    if (this.props.graph) {
      graphElement = <React.Fragment>{this.props.graph}<br /></React.Fragment>
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
                    {this.props.approval ? <th className="text-center">Approval</th> : null}
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
                      {this.props.mergeable ? <td align="center">{x.draft ? 'DRAFT' : x.mergeable}</td> : null}
                      <td align="center">{x.complexity}</td>
                      {this.props.approval
                        ? <td align="center">
                          {
                            x.approval.map((app, idx) => {
                              return <div key={idx}>{chooseBadgeStyle(app, idx)}</div>
                            })
                          }
                        </td> : null}
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
  approval: PropTypes.bool,
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

const hotChangesMapStateToProps = state => addMap({}, state.QueryReducer, 'hot_changes')

const CHotChanges = withRouter(connect(hotChangesMapStateToProps, mapDispatchToProps)(HotChanges))

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

const coldChangesMapStateToProps = state => addMap({}, state.QueryReducer, 'cold_changes')

const CColdChanges = withRouter(connect(coldChangesMapStateToProps, mapDispatchToProps)(ColdChanges))

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
                      title={<Link to={indexUrl(this.props.index, '/merged-changes')}>Recently Merged Changes</Link>}
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
                      title={<Link to={indexUrl(this.props.index, '/opened-changes')}>Recently Opened Changes</Link>}
                      created={true}
                      mergeable={true}
                      approval={true}
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

const lastChangesMapStateToProps = state => addMap({}, state.QueryReducer, 'last_changes')

const CLastChanges = withRouter(connect(lastChangesMapStateToProps, mapDispatchToProps)(LastChanges))

class AbstractLastChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_changes'
    this.state.pageSize = 100
    this.state.created = false
    this.state.updated = false
    this.state.merged = false
    this.state.duration = false
    this.state.approval = false
  }

  render () {
    if (!this.props[this.state.graph_type + '_loading']) {
      if (!this.props[this.state.graph_type + '_result']) {
        return <ErrorBox error={{ status: 0, data: 'No data' }} />
      }
      const data = this.props[this.state.graph_type + '_result']
      if (!data || data.items.length === 0) {
        return <ErrorBox error={{ status: 1, data: 'Invalid data' }} />
      }
      const LocalComplexityGraph = this.state.complexityGraph
      return (
        <React.Fragment>
          <Row>
            <Col>
              <ChangesTable
                index={this.props.index}
                graph={<LocalComplexityGraph
                  history={this.props.history}
                  data={data}
                  timeFunc={this.extractTime}
                  index={this.props.index}
                />}
                data={data}
                title={data.total + ' ' + this.state.title}
                selectedPage={this.state.selectedPage}
                pageCount={Math.ceil(data.total / this.state.pageSize)}
                pageChangeCallback={this.handlePageChange}
                pageChangeTarget={this}
                created={this.state.created}
                updated={this.state.updated}
                merged={this.state.merged}
                mergeable={this.state.mergeable}
                duration={this.state.duration}
                approval={this.state.approval}
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
    this.state.graph_type = 'last_merged_changes'
    this.state.state = 'MERGED'
    this.state.title = 'Merged Changes'
    this.state.merged = true
    this.state.duration = true
    this.state.complexityGraph = DurationComplexityGraph
  }

  extractTime = x => x.merged_at
}

const lastMergedMapStateToProps = state => addMap({}, state.QueryReducer, 'last_merged_changes')

const CLastMergedChanges = withRouter(connect(lastMergedMapStateToProps, mapDispatchToProps)(LastMergedChanges))

class LastOpenedChanges extends AbstractLastChanges {
  constructor (props) {
    super(props)
    this.state.graph_type = 'last_opened_changes'
    this.state.state = 'OPEN'
    this.state.title = 'Opened Changes'
    this.state.created = true
    this.state.updated = true
    this.state.mergeable = true
    this.state.approval = true
    this.state.complexityGraph = ComplexityGraph
  }

  extractTime = x => x.created_at
}

const lastOpenedChangesMapStateToProps = state => addMap({}, state.QueryReducer, 'last_opened_changes')

const CLastOpenedChanges = withRouter(connect(lastOpenedChangesMapStateToProps, mapDispatchToProps)(LastOpenedChanges))

class AbandonedChangesFull extends AbstractLastChanges {
  constructor (props) {
    super(props)
    this.state.graph_type = 'full_last_abandoned_changes'
    this.state.state = 'CLOSED'
    this.state.pageSize = 100
    this.state.title = 'Abandoned Changes'
    this.state.complexityGraph = DurationComplexityGraph
    this.state.created = true
    this.state.duration = true
  }

  extractTime = x => x.created_at
}

const abandonedChangesFullMapStateToProps = state => addMap({}, state.QueryReducer, 'full_last_abandoned_changes')

const CAbandonedChangesFull = withRouter(connect(abandonedChangesFullMapStateToProps, mapDispatchToProps)(AbandonedChangesFull))

class AbandonedChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_changes'
    this.state.graph_type = 'last_abandoned_changes'
    this.state.state = 'CLOSED'
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
          title={<Link to={indexUrl(this.props.index, '/abandoned-changes')}>Last Abandoned Changes</Link>}
          created={true}
          duration={true}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

const abandonedChangesMapStateToProps = state => addMap({}, state.QueryReducer, 'last_abandoned_changes')

const CAbandonedChanges = withRouter(connect(abandonedChangesMapStateToProps, mapDispatchToProps)(AbandonedChanges))

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
