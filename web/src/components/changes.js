import React from 'react'

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

import { ComplexityGraph, DurationComplexityGraph } from './complexity_graph'

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
                    <th>Repository</th>
                    <th>Number of changes</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items.map((item, index) =>
                    <tr key={index}>
                      <td><a href={addUrlField('repository', item.key)}>{item.key}</a></td>
                      <td>{item.doc_count}</td>
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
                    {this.props.created ? <th>Created</th> : null}
                    {this.props.updated ? <th>Updated</th> : null}
                    {this.props.merged ? <th>Merged</th> : null}
                    {this.props.duration ? <th>Duration</th> : null}
                    <th>Id</th>
                    <th>Author</th>
                    <th>Title</th>
                    {this.props.mergeable ? <th>Mergeable</th> : null}
                    <th>Complexity</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items && this.props.data.items.map((x, index) =>
                    <tr key={index}>
                      {this.props.created ? <td>{moment(x.created_at).fromNow()}</td> : null}
                      {this.props.updated ? <td>{moment(x.updated_at).fromNow()}</td> : null}
                      {this.props.merged ? <td>{moment(x.merged_at).fromNow()}</td> : null}
                      {this.props.duration ? <td>{moment.duration(x.duration, 'seconds').humanize()}</td> : null}
                      <td><a href={addUrlField('repository', x.change_id)}>{x.change_id}</a></td>
                      <td><a href={addUrlField('authors', x.author)}>{x.author}</a></td>
                      <td>{changeUrl(this.props.index, x, x.title)}</td>
                      {this.props.mergeable ? <td>{x.mergeable}</td> : null}
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
                      title={<a href={newRelativeUrl('/merged-changes')}>Recently merged changes</a>}
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
                      title={<a href={newRelativeUrl('/opened-changes')}>Recently opened changes</a>}
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
                    data={data}
                    timeFunc={this.extractTime}
                    index={this.props.index}
                  />
                  : <ComplexityGraph
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
    merged_changes: ({
      items: PropTypes.array
    }),
    opened_changes: ({
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
    this.state.name = 'abandoned_changes'
    this.state.graph_type = 'abandoned_changes'
  }

  render () {
    if (!this.props.abandoned_changes_loading) {
      if (this.props.abandoned_changes_error) {
        return <ErrorBox
          error={this.props.abandoned_changes_error}
        />
      }
      const data = this.props.abandoned_changes_result
      return (
        <ChangesTable
          index={this.props.index}
          data={data}
          title="Abandoned changes"
          created={true}
          duration={true}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = state => {
  return {
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    filter_gte: state.FiltersReducer.filter_gte,
    filter_lte: state.FiltersReducer.filter_lte,
    filter_repository: state.FiltersReducer.filter_repository,
    filter_index: state.FiltersReducer.filter_index,
    filter_interval: state.FiltersReducer.filter_interval,
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    filter_authors: state.FiltersReducer.filter_authors,
    repos_top_merged_loading: state.QueryReducer.repos_top_merged_loading,
    repos_top_merged_result: state.QueryReducer.repos_top_merged_result,
    repos_top_merged_error: state.QueryReducer.repos_top_merged_error,
    hot_changes_loading: state.QueryReducer.hot_changes_loading,
    hot_changes_result: state.QueryReducer.hot_changes_result,
    hot_changes_error: state.QueryReducer.hot_changes_error,
    cold_changes_loading: state.QueryReducer.cold_changes_loading,
    cold_changes_result: state.QueryReducer.cold_changes_result,
    cold_changes_error: state.QueryReducer.cold_changes_error,
    abandoned_changes_loading: state.QueryReducer.abandoned_changes_loading,
    abandoned_changes_result: state.QueryReducer.abandoned_changes_result,
    abandoned_changes_error: state.QueryReducer.abandoned_changes_error,
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
const CLastChanges = connect(mapStateToProps, mapDispatchToProps)(LastChanges)
const CLastMergedChanges = connect(mapStateToProps, mapDispatchToProps)(LastMergedChanges)
const CLastOpenedChanges = connect(mapStateToProps, mapDispatchToProps)(LastOpenedChanges)

export {
  CRepoChanges,
  CHotChanges,
  CColdChanges,
  CAbandonedChanges,
  CLastChanges,
  CLastMergedChanges,
  CLastOpenedChanges
}
