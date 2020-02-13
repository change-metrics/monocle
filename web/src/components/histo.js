import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'

import { Line } from 'react-chartjs-2';

class EventsHisto extends React.Component {
  prepare_data_set(histos) {
    const _histos = Object.entries(histos)
    let data = {
      labels: _histos[0][1][0].map(x => x.key_as_string),
      datasets: []
    }
    _histos.forEach(histo => {
      data.datasets.push(
        {
          label: histo[0],
          data: histo[1][0].map(x => x.doc_count)
        }
      )
    });
    console.log(data)
    return data
  }
  render() {
    const data = this.prepare_data_set(this.props.data)
    return (
      <Row>
        {/* <Col md={{ span: 8, offset: 2 }}> */}
        <Col>
          <Card>
            <Card.Body>
              <Line
                data={data}
                width={100}
                height={50}
              />
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

export {
  EventsHisto,
}
