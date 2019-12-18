import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'

import { Bar } from 'react-chartjs-2';

class BaseEventsHisto extends React.Component {
  prepare_data_set(data, title) {
    const dataset = {
      labels: data.map(x => x.key_as_string),
      datasets: [
        {
          label: title,
          data: data.map(x => x.doc_count)
        }
      ]
    }
    return dataset
  }
  render_component(graph_type, title) {
    if (!this.props[graph_type + '_loading']) {
      const data = this.prepare_data_set(
        this.props[graph_type + '_result'][2], title)
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

class AllEventsHisto extends BaseEventsHisto {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '15d',
      'gte': '2019-07-01',
      'graph_type': 'all_events',
    })
  }
  render() {
    return this.render_component('all_events', 'All events histogram')
  }
}

class CloseEventsHisto extends BaseEventsHisto {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '15d',
      'gte': '2019-07-01',
      'type': 'PRClosedEvent',
      'graph_type': 'close_events',
    })
  }
  render() {
    return this.render_component('close_events', 'Pull Request close events histogram')
  }
}

class CommentEventsHisto extends BaseEventsHisto {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '15d',
      'gte': '2019-07-01',
      'type': 'PRCommentedEvent',
      'graph_type': 'comment_events',
    })
  }
  render() {
    return this.render_component('comment_events', 'Pull Request comment events histogram')
  }
}

class CreateEventsHisto extends BaseEventsHisto {
  componentDidMount() {
    this.props.query({
      'org': 'kubernetes',
      'name': 'events_histo',
      'interval': '15d',
      'gte': '2019-07-01',
      'type': 'PRCreatedEvent',
      'graph_type': 'create_events',
    })
  }
  render() {
    return this.render_component('create_events', 'Pull Request create events histogram')
  }
}

export {
  AllEventsHisto,
  CreateEventsHisto,
  CloseEventsHisto,
  CommentEventsHisto,
}
