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
  chooseApprovalBadgeStyle,
  ChangeStatus
} from './common'

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
              <Table striped responsive bordered hover size="sm">
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
    this.state.name = 'repos_top'
    this.state.graph_type = 'repos_top_merged'
    this.state.state = 'MERGED'
  }

  render () {
    if (!this.props.repos_top_merged_loading) {
      if (this.props.repos_top_merged_error) {
        return <ErrorBox
          error={this.props.repos_top_merged_error}
        />
      }
      const data = this.props.repos_top_merged_result
      console.log(this.props)
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
    var ChangeRowStyle = {
      paddingTop: '5px',
      paddingBottom: '5px',
      backgroundColor: '#f7f7f7',
      backgroundClip: 'content-box'
    }
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              {this.props.graph !== '' ? <React.Fragment>{this.props.graph}</React.Fragment> : null}
              {paginationElement}
              {this.props.data.items.map((change, index) =>
                <Row key={index} style={ChangeRowStyle}>
                  <Col>
                    <Row>
                      <Col md={9}>
                        <ChangeStatus data={change} />{' '}
                        {' - '}
                        <Link to={addUrlField('repository', change.repository_fullname)}>{change.repository_fullname}</Link>
                        {' - '}
                        {changeUrl(this.props.index, change, change.title)}
                      </Col>
                      <Col md={3}>
                        Complexity: {change.complexity}
                      </Col>
                    </Row>
                    <Row>
                      <Col md={9}>
                        Created {moment(change.created_at).fromNow()} by <Link className='span' to={addUrlField('authors', change.author)}>{change.author}</Link>
                        {' - '}
                        Updated {moment(change.updated_at).fromNow()}
                      </Col>
                      {change.state === 'MERGED' || change.state === 'CLOSED' ? <Col>Duration: {moment.duration(change.duration, 'seconds').humanize()}</Col> : null}
                    </Row>
                    {change.approval.length > 0
                      ? <Row>
                        <Col>
                          Review approvals:{' '}
                          {
                            change.approval.map((app, idx) => {
                              return <span key={idx}>{chooseApprovalBadgeStyle(app, idx)}{' '}</span>
                            })
                          }
                        </Col>
                      </Row>
                      : ''}
                  </Col>
                </Row>)}
            </Card.Body>
          </Card>
        </Col>
      </Row >
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
  }

  render () {
    if (!this.props[this.state.graph_type + '_loading']) {
      if (this.props[this.state.graph_type + '_error']) {
        return <ErrorBox error={this.props[this.state.graph_type + '_error']} />
      }
      const data = this.props[this.state.graph_type + '_result']
      var graph = <div></div>
      if (this.props.showComplexityGraph && (this.state.state === 'MERGED' || this.state.state === 'CLOSED')) {
        graph = <DurationComplexityGraph
          history={this.props.history}
          data={data}
          timeFunc={this.extractTime}
          index={this.props.index}
        />
      }
      return (
        <React.Fragment>
          <Row>
            <Col>
              <ChangesTable
                index={this.props.index}
                graph={graph}
                data={data}
                title={data.total + ' ' + this.state.title}
                selectedPage={this.state.selectedPage}
                pageCount={Math.ceil(data.total / this.state.pageSize)}
                pageChangeCallback={this.handlePageChange}
                pageChangeTarget={this}
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

class LastChangesNG extends AbstractLastChanges {
  constructor (props) {
    super(props)
    this.state.graph_type = 'last_changes'
    this.state.title = 'Changes'
  }

  extractTime = x => x.created_at
}

const lastChangesNGMapStateToProps = state => addMap({}, state.QueryReducer, 'last_changes')

const CLastChangesNG = withRouter(connect(lastChangesNGMapStateToProps, mapDispatchToProps)(LastChangesNG))

export {
  CRepoChanges,
  CHotChanges,
  CColdChanges,
  CLastChanges,
  CLastChangesNG
}
