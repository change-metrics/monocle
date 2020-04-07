import React from 'react'

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'
import ReactPaginate from 'react-paginate'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  change_url,
  add_url_field,
  new_relative_url
} from './common'

var moment = require('moment')

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
                      <td><a href={add_url_field('repository', item.key)}>{item.key}</a></td>
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
              {paginationElement}
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    {this.props.created ? <th>created</th> : null}
                    {this.props.updated ? <th>updated</th> : null}
                    {this.props.merged ? <th>merged</th> : null}
                    <th>id</th>
                    <th>author</th>
                    <th>title</th>
                    {this.props.mergeable ? <th>mergeable</th> : null}
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items && this.props.data.items.map((x, index) =>
                    <tr key={index}>
                      {this.props.created ? <td>{moment(x.created_at).fromNow()}</td> : null}
                      {this.props.updated ? <td>{moment(x.updated_at).fromNow()}</td> : null}
                      {this.props.merged ? <td>{moment(x.merged_at).fromNow()}</td> : null}
                      <td><a href={add_url_field('repository', x.repository_fullname)}>{x.repository_fullname_and_number}</a></td>
                      <td><a href={add_url_field('authors', x.author)}>{x.author}</a></td>
                      <td>{change_url(x, x.title)}</td>
                      {this.props.mergeable ? <td>{x.mergeable}</td> : null}
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
                      data={data.merged_changes}
                      title={<a href={new_relative_url('/merged-changes')}>Recently merged changes</a>}
                      merged={true}
                    />
                  </Col>
                </Row>
                <Row><Col><p></p></Col></Row>
                <Row>
                  <Col>
                    <ChangesTable
                      data={data.opened_changes}
                      title={<a href={new_relative_url('/opened-changes')}>Recently opened changes</a>}
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

class LastMergedChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_state_changed_changes'
    this.state.graph_type = 'last_changes'
  }

  render () {
    if (!this.props.last_changes_loading) {
      const data = this.props.last_changes_result
      return (
        <Row>
          <Col>
            <ChangesTable
              data={data.merged_changes}
              title="Recently Merged Changes"
              selectedPage={this.state.selectedPage}
              pageCount={Math.ceil(data.merged_changes.total / this.state.pageSize)}
              pageChangeCallback={this.handlePageChange}
              pageChangeTarget={this}
              merged={true}
            />
          </Col>
        </Row>
      )
    } else {
      return <LoadingBox />
    }
  }
}

class LastOpenedChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'last_state_changed_changes'
    this.state.graph_type = 'last_changes'
  }

  render () {
    if (!this.props.last_changes_loading) {
      const data = this.props.last_changes_result
      return (
        <Row>
          <Col>
            <ChangesTable
              data={data.opened_changes}
              title="Recently Opened Changes"
              mergeable={true}
              selectedPage={this.state.selectedPage}
              pageCount={Math.ceil(data.opened_changes.total / this.state.pageSize)}
              pageChangeCallback={this.handlePageChange}
              pageChangeTarget={this}
              created={true}
            />
          </Col>
        </Row>
      )
    } else {
      return <LoadingBox />
    }
  }
}

export {
  RepoChanges,
  HotChanges,
  ColdChanges,
  LastChanges,
  LastOpenedChanges
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
const CLastChanges = connect(mapStateToProps, mapDispatchToProps)(LastChanges)
const CLastMergedChanges = connect(mapStateToProps, mapDispatchToProps)(LastMergedChanges)
const CLastOpenedChanges = connect(mapStateToProps, mapDispatchToProps)(LastOpenedChanges)

export {
  CRepoChanges,
  CHotChanges,
  CColdChanges,
  CLastChanges,
  CLastMergedChanges,
  CLastOpenedChanges
}
