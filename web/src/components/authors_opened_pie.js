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

class AuthorsOpenedPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'authors_top_opened'
    this.state.graph_type = 'authors_top_opened'
  }

  render () {
    if (!this.props.authors_top_opened_loading) {
      if (this.props.authors_top_opened_error) {
        return <ErrorBox
          error={this.props.authors_top_opened_error}
        />
      }
      if (!this.props.authors_top_opened_result) {
        return <ErrorBox
          error={{ data: 'No data for AuthorsOpenedPie', status: 0 }}
        />
      }
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Opened Changes per author</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      data={this.props.authors_top_opened_result}
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
    authors_top_opened_loading: state.QueryReducer.authors_top_opened_loading,
    authors_top_opened_result: state.QueryReducer.authors_top_opened_result,
    authors_top_opened_error: state.QueryReducer.authors_top_opened_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CAuthorsOpenedPie = connect(mapStateToProps, mapDispatchToProps)(AuthorsOpenedPie)

export default CAuthorsOpenedPie
