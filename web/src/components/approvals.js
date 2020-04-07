import React from 'react'

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import PropTypes from 'prop-types'

import {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent
} from './common'

import { Pie } from 'react-chartjs-2'

class ChangeApprovals extends React.Component {
  prepareDataSet (_data) {
    const ignoredApproval = [
      'Code-Review+0',
      'Verified+0',
      'Workflow+0',
      'COMMENTED'
    ]
    const palette = {
      'Code-Review+2': '#00ff9f',
      'Code-Review+1': '#B6FCD5',
      'Code-Review-1': '#CA5462',
      'Code-Review-2': '#AB0000',
      'Workflow+1': '#00ff9f',
      'Workflow-1': '#AB0000',
      APPROVED: '#00ff9f',
      DISMISSED: '#AB0000'
    }
    const tops = _data.items.filter(x => !ignoredApproval.includes(x.key))
    const data = {
      labels: tops.map(x => x.key),
      datasets: [{
        label: 'Approvals',
        data: tops.map(x => x.doc_count),
        backgroundColor: tops.map(x => palette[x.key])
      }]
    }
    return data
  }

  render () {
    const data = this.prepareDataSet(this.props.data)
    return <Pie data={data} />
  }
}

ChangeApprovals.propTypes = {
  data: PropTypes.shape({
    items: PropTypes.array
  })
}

class ApprovalStats extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'changes_top_approval'
    this.state.graph_type = 'approval_stats'
  }

  render () {
    if (!this.props.approval_stats_loading) {
      if (this.props.approval_stats_error) {
        return <ErrorBox
          error={this.props.approval_stats_error}
        />
      }
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
    filter_index: state.FiltersReducer.filter_index,
    filter_interval: state.FiltersReducer.filter_interval,
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    filter_authors: state.FiltersReducer.filter_authors,
    approval_stats_loading: state.QueryReducer.approval_stats_loading,
    approval_stats_result: state.QueryReducer.approval_stats_result,
    approval_stats_error: state.QueryReducer.approval_stats_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CApprovalStats = connect(mapStateToProps, mapDispatchToProps)(ApprovalStats)

export {
  CApprovalStats
}
