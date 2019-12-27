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
          {/* <Col md={{ span: 8, offset: 2 }}> */}
          <Col>
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

class AllEventsHisto extends BaseEventsHisto {
  componentDidUpdate() {
    if (this.props.filter_loaded_from_url && !this.props.all_events_result) {
      this.props.query({
        'repository': 'kubernetes/kubernetes',
        'name': 'events_histo',
        'interval': '15d',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'graph_type': 'all_events',
      })
    }
  }
  render() {
    return this.render_component('all_events', 'All events histogram')
  }
}

class CloseEventsHisto extends BaseEventsHisto {
  componentDidUpdate() {
    if (this.props.filter_loaded_from_url && !this.props.close_events_result) {
      this.props.query({
        'repository': 'kubernetes/kubernetes',
        'name': 'events_histo',
        'interval': '15d',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'type': 'ChangeClosedEvent',
        'graph_type': 'close_events',
      })
    }
  }
  render() {
    return this.render_component('close_events', 'Change close events histogram')
  }
}

class CommentEventsHisto extends BaseEventsHisto {
  componentDidUpdate() {
    if (this.props.filter_loaded_from_url && !this.props.comment_events_result) {
      this.props.query({
        'repository': 'kubernetes/kubernetes',
        'name': 'events_histo',
        'interval': '15d',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'type': 'ChangeCommentedEvent',
        'graph_type': 'comment_events',
      })
    }
  }
  render() {
    return this.render_component('comment_events', 'Change comment events histogram')
  }
}

class CreateEventsHisto extends BaseEventsHisto {
  componentDidUpdate() {
    if (this.props.filter_loaded_from_url && !this.props.create_events_result) {
      this.props.query({
        'repository': 'kubernetes/kubernetes',
        'name': 'events_histo',
        'interval': '15d',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'type': 'ChangeCreatedEvent',
        'graph_type': 'create_events',
      })
    }
  }
  render() {
    return this.render_component('create_events', 'Change create events histogram')
  }
}

export {
  AllEventsHisto,
  CreateEventsHisto,
  CloseEventsHisto,
  CommentEventsHisto,
}
