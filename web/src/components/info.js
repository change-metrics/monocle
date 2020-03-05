import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import ListGroup from 'react-bootstrap/ListGroup'
import {
  ChangeLifeCycleEventsHisto,
  ChangeReviewEventsHisto,
} from './histo'
import {
  TopEventsTable,
  TopStrengthsTable,
  HotChangesTable,
  ColdChangesTable,
  LastChangesTable,
} from './top'
import {
  ChangeApprovals,
} from './approval'

class LoadingBox extends React.Component {
  render() {
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

class BaseQueryComponent extends React.Component {
  queryBackend(prevProps, name, graph_type) {
    // Usefull snippet
    // Object.entries(this.props).forEach(([key, val]) =>
    //   prevProps[key] !== val && console.log(`Prop '${key}' changed`)
    // );
    if (this.props.filter_loaded_from_url !== prevProps.filter_loaded_from_url) {
      this.props.query({
        'repository': this.props.filter_repository,
        'name': name,
        'gte': this.props.filter_gte,
        'lte': this.props.filter_lte,
        'interval': this.props.filter_interval,
        'exclude_authors': this.props.filter_exclude_authors,
        'graph_type': graph_type,
      })
    }
  }
}

class InfoEvents extends BaseQueryComponent {
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

class ChangesLifeCycleStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'changes_lifecycle_stats',
      'changes_lifecycle_stats')
  }
  render() {
    if (!this.props.changes_lifecycle_stats_loading) {
      const data = this.props.changes_lifecycle_stats_result
      const int = this.props.filter_interval
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Changes lifecycle stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col md={4}>
                    <ListGroup>
                      <ListGroup.Item>
                        Changes abandoned: {data.ratios['abandoned/created']}%
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Changes merged: {data.ratios['merged/created']}%
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Change abandoned every {int}: {data.avgs['ChangeAbandonedEvent']}
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Change created every {int}: {data.avgs['ChangeCreatedEvent']}
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Change merged every {int}: {data.avgs['ChangeMergedEvent']}
                      </ListGroup.Item>
                    </ListGroup>
                  </Col>
                  <Col md={8}>
                    <ChangeLifeCycleEventsHisto
                      data={data.histos}
                    />
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

class ChangesReviewStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'changes_review_stats',
      'changes_review_stats')
  }
  render() {
    if (!this.props.changes_review_stats_loading) {
      const data = this.props.changes_review_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Changes review stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col md={4}>
                    <ListGroup>
                      <ListGroup.Item>
                        Average delay for the first comment:{' '}
                        {data.first_event_delay.comment.first_event_delay_avg} secs
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Average delay for the first review:{' '}
                        {data.first_event_delay.review.first_event_delay_avg} secs
                      </ListGroup.Item>
                    </ListGroup>
                  </Col>
                  <Col md={8}>
                    <ChangeReviewEventsHisto
                      data={data.histos}
                    />
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


class MostActiveAuthorsStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'most_active_authors_stats',
      'most_active_authors_stats')
  }
  render() {
    if (!this.props.most_active_authors_stats_loading) {
      const data = this.props.most_active_authors_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Most active authors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeCreatedEvent.tops}
                      title="Changes"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeCommentedEvent.tops}
                      title="Comments"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeReviewedEvent.tops}
                      title="Reviews"
                    />
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

class MostReviewedAuthorsStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'most_reviewed_authors_stats',
      'most_reviewed_authors_stats')
  }
  render() {
    if (!this.props.most_reviewed_authors_stats_loading) {
      const data = this.props.most_reviewed_authors_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Most reviewed authors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <TopEventsTable
                      data={data.reviewed.tops}
                      title="Reviews"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.commented.tops}
                      title="Comments"
                    />
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

class ApprovalStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'changes_top_approval',
      'approval_stats')
  }
  render() {
    if (!this.props.approval_stats_loading) {
      const data = this.props.approval_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Approvals dispersion stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <ChangeApprovals
                      data={data}
                    />
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

class AuthorsPeersStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'peers_exchange_strength',
      'authors_peers_stats')
  }
  render() {
    if (!this.props.authors_peers_stats_loading) {
      const data = this.props.authors_peers_stats_result
      return (
        <TopStrengthsTable
          data={data.slice(0, 10)}
          title="Peers strength"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class HotChanges extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'hot_changes',
      'hot_changes')
  }
  render() {
    if (!this.props.hot_changes_loading) {
      const data = this.props.hot_changes_result
      return (
        <HotChangesTable
          data={data.slice(0, 10)}
          title="Hot changes"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}


class ColdChanges extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'cold_changes',
      'cold_changes')
  }
  render() {
    if (!this.props.cold_changes_loading) {
      const data = this.props.cold_changes_result
      return (
        <ColdChangesTable
          data={data.slice(0, 10)}
          title="Cold changes"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

class LastChanges extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      'last_state_changed_changes',
      'last_changes')
  }
  render() {
    if (!this.props.last_changes_loading) {
      const data = this.props.last_changes_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Recently Merged/Opened changes</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <LastChangesTable
                      data={data.merged_changes.slice(0, 10)}
                      title="Recently merged changes"
                    />
                  </Col>
                </Row>
                <Row><Col><p></p></Col></Row>
                <Row>
                  <Col>
                    <LastChangesTable
                      data={data.opened_changes.slice(0, 10)}
                      title="Recently opened changes"
                    />
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

export {
  InfoEvents,
  ChangesLifeCycleStats,
  ChangesReviewStats,
  MostActiveAuthorsStats,
  ApprovalStats,
  MostReviewedAuthorsStats,
  AuthorsPeersStats,
  HotChanges,
  ColdChanges,
  LastChanges,
}
