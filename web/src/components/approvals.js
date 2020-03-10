import React from 'react';

import { connect } from 'react-redux';
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import {
  LoadingBox, 
  BaseQueryComponentNG
} from './common'

import { Pie } from 'react-chartjs-2';

class ChangeApprovals extends React.Component {
  prepare_data_set(_data) {
    const ignored_approval = [
      'Code-Review+0',
      'Verified+0',
      'Workflow+0',
      'COMMENTED',
    ]
    const palette = {
      'Code-Review+2': '#00ff9f',
      'Code-Review+1': '#B6FCD5',
      'Code-Review-1': '#CA5462',
      'Code-Review-2': '#AB0000',
      'Workflow+1': '#00ff9f',
      'Workflow-1': '#AB0000',
      'APPROVED': '#00ff9f',
      'DISMISSED': '#AB0000',
    }
    const tops = _data.tops.filter(x => !ignored_approval.includes(x.key))
    let data = {
      labels: tops.map(x => x.key),
      datasets: [{
        label: 'Approvals',
        data: tops.map(x => x.doc_count),
        backgroundColor: tops.map(x => palette[x.key])
      }]
    }
    return data
  }
  render() {
    const data = this.prepare_data_set(this.props.data)
    return <Pie data={data} />
  }
}

class ApprovalStats extends BaseQueryComponentNG {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'changes_top_approval',
      'approval_stats',
      this.props.handleQuery)
  }
  render() {
    if (!this.props.approval_stats_loading) {
      const data = this.props.approval_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Approvals dispersion stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <ChangeApprovals
                      data={data}
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

const mapStateToProps = state => {
  return {
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    filter_gte: state.FiltersReducer.filter_gte,
    filter_lte: state.FiltersReducer.filter_lte,
    filter_repository: state.FiltersReducer.filter_repository,
    filter_interval: state.FiltersReducer.filter_interval,
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    approval_stats_loading: state.QueryReducer.approval_stats_loading,
    approval_stats_result: state.QueryReducer.approval_stats_result,
    approval_stats_error: state.QueryReducer.approval_stats_error,
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params)),
  }
}

const CApprovalStats = connect(mapStateToProps, mapDispatchToProps)(ApprovalStats);


export {
  CApprovalStats,
}
