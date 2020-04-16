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

class ReposOpenedPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'repos_top_opened'
    this.state.graph_type = 'repos_top_opened'
  }

  render () {
    if (!this.props.repos_top_opened_loading) {
      if (this.props.repos_top_opened_error) {
        return <ErrorBox
          error={this.props.repos_top_opened_error}
        />
      }
      if (!this.props.repos_top_opened_result) {
        return <ErrorBox
          error={{ data: 'No data for ReposOpenedPie', status: 0 }}
        />
      }
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Opened Changes per repository</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      data={this.props.repos_top_opened_result}
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
    repos_top_opened_loading: state.QueryReducer.repos_top_opened_loading,
    repos_top_opened_result: state.QueryReducer.repos_top_opened_result,
    repos_top_opened_error: state.QueryReducer.repos_top_opened_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CReposOpenedPie = connect(mapStateToProps, mapDispatchToProps)(ReposOpenedPie)

export default CReposOpenedPie
