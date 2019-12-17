import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'

import { Bar } from 'react-chartjs-2';

class AllEventsHisto extends React.Component {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '31d',
      'graph_type': 'all_events',
    })
  }
  render() {
    if (!this.props.all_events_loading) {
      console.log(this.props.all_events_result)
      const data = {
        labels: this.props.all_events_result[2].map(x => x.key_as_string),
        datasets: [
          {
            label: 'Events histo',
            data: this.props.all_events_result[2].map(x => x.doc_count)
          }
        ]
      };
      return (
        <Row>
          <Col md={{ span: 8, offset: 2 }}>
            <Card>
              <Card.Body>
                <Bar
                  data={data}
                  width={100}
                  height={50}
                />
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )
    } else {
      return (
        <Row>
          <Col md={{ span: 8, offset: 2 }}>
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

class CloseEventsHisto extends React.Component {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '31d',
      'type': 'PRClosedEvent',
      'graph_type': 'close_events',
    })
  }
  render() {
    if (!this.props.close_events_loading) {
      console.log(this.props.close_events_result)
      const data = {
        labels: this.props.close_events_result[2].map(x => x.key_as_string),
        datasets: [
          {
            label: 'Events histo',
            data: this.props.close_events_result[2].map(x => x.doc_count)
          }
        ]
      };
      return (
        <Row>
          <Col md={{ span: 8, offset: 2 }}>
            <Card>
              <Card.Body>
                <Bar
                  data={data}
                  width={100}
                  height={50}
                />
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )
    } else {
      return (
        <Row>
          <Col md={{ span: 8, offset: 2 }}>
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

export {
  AllEventsHisto,
  CloseEventsHisto
}
