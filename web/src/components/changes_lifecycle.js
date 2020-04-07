import React from 'react'

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import ListGroup from 'react-bootstrap/ListGroup'

import { Line } from 'react-chartjs-2'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox
} from './common'

class ChangeLifeCycleEventsHisto extends React.Component {
  prepare_data_set (histos) {
    const event_name_mapping = {
      ChangeAbandonedEvent: {
        label: 'Changes abandoned',
        pointBorderColor: 'rgba(92,92,92,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(92,92,92,0.4)',
        borderColor: 'rgba(92,92,92,1)'
      },
      ChangeCreatedEvent: {
        label: 'Changes created',
        pointBorderColor: 'rgba(135,255,149,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(135,255,149,0.4)',
        borderColor: 'rgba(135,255,149,1)'
      },
      ChangeMergedEvent: {
        label: 'Changes merged',
        pointBorderColor: 'rgba(169,135,255,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(169,135,255,0.4)',
        borderColor: 'rgba(169,135,255,1)'
      }
    }
    const _histos = Object.entries(histos)
    const data = {
      labels: histos.ChangeCreatedEvent[0].map(x => x.key_as_string),
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
          borderColor: event_name_mapping[histo[0]].borderColor
        }
      )
    })
    return data
  }

  render () {
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

class ChangesLifeCycleStats extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'changes_lifecycle_stats'
    this.state.graph_type = 'changes_lifecycle_stats'
  }

  render () {
    if (!this.props.changes_lifecycle_stats_loading) {
      if (this.props.changes_lifecycle_stats_error) {
        return <ErrorBox
          error={this.props.changes_lifecycle_stats_error}
        />
      }
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
                        {data.ChangeCreatedEvent.events_count} changes created by {data.ChangeCreatedEvent.authors_count} authors
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.ChangeMergedEvent.events_count} changes merged by {data.ChangeMergedEvent.authors_count} authors
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.ChangeAbandonedEvent.events_count} changes abandoned by {data.ChangeAbandonedEvent.authors_count} authors
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Changes abandoned: {data.ratios['abandoned/created']}%
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Changes merged: {data.ratios['merged/created']}%
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Change abandoned every {int}: {data.avgs.ChangeAbandonedEvent}
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Change created every {int}: {data.avgs.ChangeCreatedEvent}
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Change merged every {int}: {data.avgs.ChangeMergedEvent}
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

const mapStateToProps = state => {
  return {
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    filter_gte: state.FiltersReducer.filter_gte,
    filter_lte: state.FiltersReducer.filter_lte,
    filter_repository: state.FiltersReducer.filter_repository,
    filter_index: state.FiltersReducer.filter_index,
    filter_interval: state.FiltersReducer.filter_interval,
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    filter_authors: state.FiltersReducer.filter_authors,
    changes_lifecycle_stats_loading: state.QueryReducer.changes_lifecycle_stats_loading,
    changes_lifecycle_stats_result: state.QueryReducer.changes_lifecycle_stats_result,
    changes_lifecycle_stats_error: state.QueryReducer.changes_lifecycle_stats_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CChangesLifeCycleStats = connect(mapStateToProps, mapDispatchToProps)(ChangesLifeCycleStats)

export {
  CChangesLifeCycleStats
}
