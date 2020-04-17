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

class AuthorsMergedPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'authors_top_merged'
    this.state.graph_type = 'authors_top_merged'
  }

  render () {
    if (!this.props.authors_top_merged_loading) {
      if (this.props.authors_top_merged_error) {
        return <ErrorBox
          error={this.props.authors_top_merged_error}
        />
      }
      if (!this.props.authors_top_merged_result) {
        return <ErrorBox
          error={{ data: 'No data for AuthorsMergedPie', status: 0 }}
        />
      }
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Merged Changes per author</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      data={this.props.authors_top_merged_result}
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
    authors_top_merged_loading: state.QueryReducer.authors_top_merged_loading,
    authors_top_merged_result: state.QueryReducer.authors_top_merged_result,
    authors_top_merged_error: state.QueryReducer.authors_top_merged_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CAuthorsMergedPie = connect(mapStateToProps, mapDispatchToProps)(AuthorsMergedPie)

export default CAuthorsMergedPie
