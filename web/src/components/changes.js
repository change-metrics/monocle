import React from 'react';

import { connect } from 'react-redux';
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'

import {
    BaseQueryComponentNG,
    LoadingBox,
} from './common'

class HotChangesTable extends React.Component {
  render() {
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
                    <th>hot score</th>
                    <th>id</th>
                    <th>author</th>
                    <th>created/updated</th>
                    <th>title</th>
                    <th>mergeable</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>{x.hot_score}</td>
                      <td>{x.repository_fullname_and_number}</td>
                      <td>{x.author}</td>
                      <td>
                        <div>{x.created_at}</div>
                        <div>{x.updated_at}</div>
                      </td>
                      <td>{x.title}</td>
                      <td>{x.mergeable}</td>
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

class ColdChangesTable extends React.Component {
  render() {
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
                    <th>created/updated</th>
                    <th>id</th>
                    <th>author</th>
                    <th>title</th>
                    <th>mergeable</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>
                        <div>{x.created_at}</div>
                        <div>{x.updated_at}</div>
                      </td>
                      <td>{x.repository_fullname_and_number}</td>
                      <td>{x.author}</td>
                      <td>{x.title}</td>
                      <td>{x.mergeable}</td>
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

class LastChangesTable extends React.Component {
  render() {
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
                    <th>created/updated</th>
                    <th>id</th>
                    <th>author</th>
                    <th>title</th>
                    <th>mergeable</th>
                    <th>state</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>
                        <div>{x.created_at}</div>
                        <div>{x.updated_at}</div>
                      </td>
                      <td>{x.repository_fullname_and_number}</td>
                      <td>{x.author}</td>
                      <td>{x.title}</td>
                      <td>{x.mergeable}</td>
                      <td>{x.state}</td>
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

class HotChanges extends BaseQueryComponentNG {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'hot_changes',
      'hot_changes',
      this.props.handleQuery)
  }
  render() {
    if (!this.props.hot_changes_loading) {
      const data = this.props.hot_changes_result
      return (
        <HotChangesTable
          data={data.slice(0, 10)}
          title="Hot changes"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}


class ColdChanges extends BaseQueryComponentNG {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'cold_changes',
      'cold_changes',
      this.props.handleQuery)
  }
  render() {
    if (!this.props.cold_changes_loading) {
      const data = this.props.cold_changes_result
      return (
        <ColdChangesTable
          data={data.slice(0, 10)}
          title="Cold changes"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class LastChanges extends BaseQueryComponentNG {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'last_state_changed_changes',
      'last_changes',
      this.props.handleQuery)
  }
  render() {
    if (!this.props.last_changes_loading) {
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
                    <LastChangesTable
                      data={data.merged_changes.slice(0, 10)}
                      title="Recently merged changes"
                    />
                  </Col>
                </Row>
                <Row><Col><p></p></Col></Row>
                <Row>
                  <Col>
                    <LastChangesTable
                      data={data.opened_changes.slice(0, 10)}
                      title="Recently opened changes"
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

export {
    HotChanges,
    ColdChanges,
    LastChanges,
}

const mapStateToProps = state => {
  return {
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    filter_gte: state.FiltersReducer.filter_gte,
    filter_lte: state.FiltersReducer.filter_lte,
    filter_repository: state.FiltersReducer.filter_repository,
    filter_interval: state.FiltersReducer.filter_interval,
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    hot_changes_loading: state.QueryReducer.hot_changes_loading,
    hot_changes_result: state.QueryReducer.hot_changes_result,
    hot_changes_error: state.QueryReducer.hot_changes_error,
    cold_changes_loading: state.QueryReducer.cold_changes_loading,
    cold_changes_result: state.QueryReducer.cold_changes_result,
    cold_changes_error: state.QueryReducer.cold_changes_error,
    last_changes_loading: state.QueryReducer.last_changes_loading,
    last_changes_result: state.QueryReducer.last_changes_result,
    last_changes_error: state.QueryReducer.last_changes_error,
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params)),
  }
}

const CHotChanges = connect(mapStateToProps, mapDispatchToProps)(HotChanges);
const CColdChanges = connect(mapStateToProps, mapDispatchToProps)(ColdChanges);
const CLastChanges = connect(mapStateToProps, mapDispatchToProps)(LastChanges);

export {
    CHotChanges,
    CColdChanges,
    CLastChanges,
}
