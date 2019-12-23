import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'


class BaseTopEventsAuthor extends React.Component {
  render_component(graph_type, title) {
    if (!this.props[graph_type + '_loading']) {
      const data = this.props[graph_type + '_result'][2]
      return (
        <Row>
          <Col>
            <Card>
              <Card.Body>
                <Table striped responsive bordered hover>
                  <thead>
                    <tr>
                      <th>#</th>
                      <th>ID</th>
                      <th>Review count</th>
                    </tr>
                  </thead>
                  <tbody>
                    {data.map((x, index) =>
                      <tr>
                        <td>{index}</td>
                        <td>{x.key}</td>
                        <td>{x.doc_count}</td>
                      </tr>)}
                  </tbody>
                </Table>
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )
    } else {
      return (
        <Row>
          <Col>
            <Card>
              <Card.Body>
                <h1>
                  loading
                </h1>
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )
    }
  }
}

class TopReviewers extends BaseTopEventsAuthor {
  componentDidUpdate() {
    if (this.props.filter_loaded_from_url && !this.props.top_reviewers_result) {
      this.props.query({
        'org': 'kubernetes',
        'name': 'events_top_authors',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'type': 'ChangeReviewedEvent',
        'graph_type': 'top_reviewers',
      })
    }
  }
  render() {
    return this.render_component('top_reviewers', 'Top reviewers')
  }
}

class TopCreators extends BaseTopEventsAuthor {
  componentDidUpdate() {
    if (this.props.filter_loaded_from_url && !this.props.top_creators_result) {
      this.props.query({
        'org': 'kubernetes',
        'name': 'events_top_authors',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'type': 'ChangeCreatedEvent',
        'graph_type': 'top_creators',
      })
    }
  }
  render() {
    return this.render_component('top_creators', 'Top authors')
  }
}

export {
  TopReviewers,
  TopCreators,
}
