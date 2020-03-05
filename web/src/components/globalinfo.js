import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import ListGroup from 'react-bootstrap/ListGroup'

import {
    BaseQueryComponent,
    LoadingBox,
} from './common'

class GlobalInfo extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'changes_events_counters',
      'changes_events_counters')
  }
  render() {
    if (!this.props.changes_events_counters_loading) {
      const data = this.props.changes_events_counters_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Global events overview</Card.Title>
              </Card.Header>
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
      return <LoadingBox />
    }
  }
}

export {
  GlobalInfo,
}
