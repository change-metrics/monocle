import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'

import { Line } from 'react-chartjs-2';

class EventsHisto extends React.Component {
  prepare_data_set(histos) {
    const event_name_mapping = {
      ChangeAbandonedEvent: {
        label: 'Changes abandoned',
        pointBorderColor: 'rgba(92,92,92,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(92,92,92,0.4)',
        borderColor: 'rgba(92,92,92,1)',
      },
      ChangeCreatedEvent: {
        label: 'Changes created',
        pointBorderColor: 'rgba(135,255,149,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(135,255,149,0.4)',
        borderColor: 'rgba(135,255,149,1)',
      },
      ChangeMergedEvent: {
        label: 'Changes merged',
        pointBorderColor: 'rgba(169,135,255,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(169,135,255,0.4)',
        borderColor: 'rgba(169,135,255,1)',
      },
    }
    const _histos = Object.entries(histos)
    let data = {
      labels: _histos[0][1][0].map(x => x.key_as_string),
      datasets: []
    }
    _histos.forEach(histo => {
      data.datasets.push(
        {
          label: event_name_mapping[histo[0]].label,
          data: histo[1][0].map(x => x.doc_count),
          lineTension: 0.5,
          pointBorderColor: event_name_mapping[histo[0]].pointBorderColor,
          pointBackgroundColor: event_name_mapping[histo[0]].pointBackgroundColor,
          backgroundColor: event_name_mapping[histo[0]].backgroundColor,
          borderColor: event_name_mapping[histo[0]].borderColor,
        }
      )
    });
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
