import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import ListGroup from 'react-bootstrap/ListGroup'
import {
  EventsHisto,
} from './histo'

class InfoEvents extends React.Component {
  componentDidUpdate() {
    console.log(this.props)
    if (this.props.filter_loaded_from_url &&
      !this.props.changes_events_counters_result) {
      this.props.query({
        'repository': this.props.filter_repository,
        'name': 'changes_events_counters',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'graph_type': 'changes_events_counters',
      })
    }
  }
  render() {
    if (!this.props.changes_events_counters_loading) {
      const data = this.props.changes_events_counters_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Body>
                <ListGroup>
                  <ListGroup.Item>
                    {data.ChangeCreatedEvent.events_count} changes created by {data.ChangeCreatedEvent.authors_count} authors
                  </ListGroup.Item>
                  <ListGroup.Item>
                    {data.ChangeCommentedEvent.events_count} changes commented by {data.ChangeCommentedEvent.authors_count} authors
                  </ListGroup.Item>
                  <ListGroup.Item>
                    {data.ChangeReviewedEvent.events_count} changes reviewed by {data.ChangeReviewedEvent.authors_count} authors
                  </ListGroup.Item>
                  <ListGroup.Item>
                    {data.ChangeMergedEvent.events_count} changes merged by {data.ChangeMergedEvent.authors_count} authors
                  </ListGroup.Item>
                  <ListGroup.Item>
                    {data.ChangeAbandonedEvent.events_count} changes abandoned by {data.ChangeAbandonedEvent.authors_count} authors
                  </ListGroup.Item>
                </ListGroup>
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

class ChangesLifeCycleStats extends React.Component {
  componentDidUpdate() {
    console.log(this.props)
    if (this.props.filter_loaded_from_url &&
      !this.props.changes_lifecycle_stats_result) {
      this.props.query({
        'repository': this.props.filter_repository,
        'name': 'changes_lifecycle_stats',
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'graph_type': 'changes_lifecycle_stats',
      })
    }
  }
  render() {
    if (!this.props.changes_lifecycle_stats_loading) {
      const data = this.props.changes_lifecycle_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Body>
                <EventsHisto
                  data={data.histos}
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

export {
  InfoEvents,
  ChangesLifeCycleStats,
}
