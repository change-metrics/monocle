import React from 'react';
import ReactLoading from 'react-loading'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Alert from 'react-bootstrap/Alert'
import { Link } from 'react-router-dom'

import { Bar } from 'react-chartjs-2';

class EventsHisto extends React.Component {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '31d',
    })
  }
  render() {
    if (!this.props.loading) {
      console.log(this.props.result)
      const data = {
        labels: this.props.result[2].map(x => x.key_as_string),
        datasets: [
          {
            label: 'Events histo',
            data: this.props.result[2].map(x => x.doc_count)
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

export default EventsHisto
