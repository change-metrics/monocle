import React from 'react'

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'

import {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent
} from './common'

import Pie from './pie'

class ReposMergedPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'repos_top_merged'
    this.state.graph_type = 'repos_top_merged'
  }

  render () {
    if (!this.props.repos_top_merged_loading) {
      if (this.props.repos_top_merged_error) {
        return <ErrorBox
          error={this.props.repos_top_merged_error}
        />
      }
      if (!this.props.repos_top_merged_result) {
        return <ErrorBox
          error={{ data: 'No data for ReposMergedPie', status: 0 }}
        />
      }
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Merged Changes per repository</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      data={this.props.repos_top_merged_result}
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
    repos_top_merged_loading: state.QueryReducer.repos_top_merged_loading,
    repos_top_merged_result: state.QueryReducer.repos_top_merged_result,
    repos_top_merged_error: state.QueryReducer.repos_top_merged_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CReposMergedPie = connect(mapStateToProps, mapDispatchToProps)(ReposMergedPie)

export default CReposMergedPie
