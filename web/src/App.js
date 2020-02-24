import React from 'react';
import { connect } from "react-redux";

import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';

import { query } from './reducers/query'

import { Switch, Route } from 'react-router-dom'

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'

import TopMenu from './components/menu'
import {
  InfoEvents,
  ChangesLifeCycleStats,
  ChangesReviewStats,
  MostActiveAuthorsStats,
} from './components/info'
import {
  FiltersForm,
} from './components/filtersform'


class RootView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <TopMenu />
        <Container>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <FiltersForm
                handleFilterGteChange={this.props.handleFilterGteChange}
                handleFilterLteChange={this.props.handleFilterLteChange}
                handleFilterRepositoryChange={this.props.handleFilterRepositoryChange}
                handleFilterIntervalChange={this.props.handleFilterIntervalChange}
                setQueryParamsLoaded={this.props.setQueryParamsLoaded}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                filter_interval={this.props.filter_interval}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <InfoEvents
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                filter_interval={this.props.filter_interval}
                changes_events_counters_loading={this.props.changes_events_counters_loading}
                changes_events_counters_result={this.props.changes_events_counters_result}
                changes_events_counters_error={this.props.changes_events_counters_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <ChangesLifeCycleStats
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                filter_interval={this.props.filter_interval}
                changes_lifecycle_stats_loading={this.props.changes_lifecycle_stats_loading}
                changes_lifecycle_stats_result={this.props.changes_lifecycle_stats_result}
                changes_lifecycle_stats_error={this.props.changes_lifecycle_stats_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <ChangesReviewStats
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                filter_interval={this.props.filter_interval}
                changes_review_stats_loading={this.props.changes_review_stats_loading}
                changes_review_stats_result={this.props.changes_review_stats_result}
                changes_review_stats_error={this.props.changes_review_stats_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <MostActiveAuthorsStats
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                filter_interval={this.props.filter_interval}
                most_active_authors_stats_loading={this.props.most_active_authors_stats_loading}
                most_active_authors_stats_result={this.props.most_active_authors_stats_result}
                most_active_authors_stats_error={this.props.most_active_authors_stats_error}
              />
            </Col>
          </Row>
        </Container>
      </React.Fragment>)
  }
}

class App extends React.Component {
  render() {
    return (
      <Switch>
        <Route exact path='/'>
          <RootView
            handleQuery={this.props.handleQuery}
            handleFilterGteChange={this.props.handleFilterGteChange}
            handleFilterLteChange={this.props.handleFilterLteChange}
            handleFilterRepositoryChange={this.props.handleFilterRepositoryChange}
            handleFilterIntervalChange={this.props.handleFilterIntervalChange}
            setQueryParamsLoaded={this.props.setQueryParamsLoaded}
            filter_loaded_from_url={this.props.filter_loaded_from_url}
            filter_gte={this.props.filter_gte}
            filter_lte={this.props.filter_lte}
            filter_repository={this.props.filter_repository}
            filter_interval={this.props.filter_interval}
            changes_events_counters_loading={this.props.changes_events_counters_loading}
            changes_events_counters_result={this.props.changes_events_counters_result}
            changes_events_counters_error={this.props.changes_events_counters_error}
            changes_lifecycle_stats_loading={this.props.changes_lifecycle_stats_loading}
            changes_lifecycle_stats_result={this.props.changes_lifecycle_stats_result}
            changes_lifecycle_stats_error={this.props.changes_lifecycle_stats_error}
            changes_review_stats_loading={this.props.changes_review_stats_loading}
            changes_review_stats_result={this.props.changes_review_stats_result}
            changes_review_stats_error={this.props.changes_review_stats_error}
            most_active_authors_stats_loading={this.props.most_active_authors_stats_loading}
            most_active_authors_stats_result={this.props.most_active_authors_stats_result}
            most_active_authors_stats_error={this.props.most_active_authors_stats_error}
          />
        </Route>
      </Switch>
    )
  }
}

const mapStateToProps = state => {
  return {
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    filter_gte: state.FiltersReducer.filter_gte,
    filter_lte: state.FiltersReducer.filter_lte,
    filter_repository: state.FiltersReducer.filter_repository,
    filter_interval: state.FiltersReducer.filter_interval,
    changes_events_counters_loading: state.QueryReducer.changes_events_counters_loading,
    changes_events_counters_result: state.QueryReducer.changes_events_counters_result,
    changes_events_counters_error: state.QueryReducer.changes_events_counters_error,
    changes_lifecycle_stats_loading: state.QueryReducer.changes_lifecycle_stats_loading,
    changes_lifecycle_stats_result: state.QueryReducer.changes_lifecycle_stats_result,
    changes_lifecycle_stats_error: state.QueryReducer.changes_lifecycle_stats_error,
    changes_review_stats_loading: state.QueryReducer.changes_review_stats_loading,
    changes_review_stats_result: state.QueryReducer.changes_review_stats_result,
    changes_review_stats_error: state.QueryReducer.changes_review_stats_error,
    most_active_authors_stats_loading: state.QueryReducer.most_active_authors_stats_loading,
    most_active_authors_stats_result: state.QueryReducer.most_active_authors_stats_result,
    most_active_authors_stats_error: state.QueryReducer.most_active_authors_stats_error,
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params)),
    handleFilterGteChange: (date) => dispatch(
      {
        type: 'FILTER_GTE_CHANGE',
        value: date
      }
    ),
    handleFilterLteChange: (date) => dispatch(
      {
        type: 'FILTER_LTE_CHANGE',
        value: date
      }
    ),
    setQueryParamsLoaded: () => dispatch(
      {
        type: 'FILTER_PARAMS_LOADED',
        value: true
      }
    ),
    handleFilterRepositoryChange: (value) => dispatch(
      {
        type: 'FILTER_REPOSITORY_CHANGE',
        value: value
      }
    ),
    handleFilterIntervalChange: (value) => dispatch(
      {
        type: 'FILTER_INTERVAL_CHANGE',
        value: value
      }
    ),
  }
}
export default connect(mapStateToProps, mapDispatchToProps)(App);
