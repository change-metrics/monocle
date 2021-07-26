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
import PropTypes from 'prop-types'
import { withRouter, Link } from 'react-router-dom'

import ConnectionDiagram from './connection_diagram'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addUrlField,
  mapDispatchToProps,
  addMap
} from './common'

class TopEventsTable extends React.Component {
  rowStyleFormat(average, value) {
    if (value >= average) {
      return { color: 'green' }
    }
  }

  render() {
    if (!this.props.data || !this.props.data.items) {
      return <ErrorBox error={{ status: 0, data: 'Invalid data' }} />
    }
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
                    <th>#</th>
                    <th>ID</th>
                    <th>count</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items.map((x, index) => (
                    <tr key={index}>
                      <td
                        style={this.rowStyleFormat(
                          this.props.data.count_avg,
                          x.doc_count
                        )}
                      >
                        {index + 1}
                      </td>
                      <td>
                        <Link to={addUrlField('authors', x.key)}>{x.key}</Link>
                      </td>
                      <td>{x.doc_count}</td>
                    </tr>
                  ))}
                </tbody>
              </Table>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

TopEventsTable.propTypes = {
  title: PropTypes.string.isRequired,
  data: PropTypes.shape({
    items: PropTypes.array,
    count_avg: PropTypes.number
  })
}

class NewContributorsStats extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'new_contributors'
    this.state.graph_type = 'new_contributors'
  }

  render() {
    if (!this.props.new_contributors_loading) {
      if (this.props.new_contributors_error) {
        return <ErrorBox error={this.props.new_contributors_error} />
      }
      const data = this.props.new_contributors_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>New contributors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <TopEventsTable data={data} title="Active Authors" />
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



const mapStateToProps = (state) => {
  const map = {}

  addMap(map, state.QueryReducer, 'new_contributors')

  return map
}

const CNewContributorsStats = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(NewContributorsStats)
)

export {
  CNewContributorsStats,
}
