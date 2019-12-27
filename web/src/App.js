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
  AllEventsHisto,
  CloseEventsHisto,
  CommentEventsHisto,
  CreateEventsHisto,
} from './components/histo'
import {
  TopReviewers,
  TopCreators,
} from './components/top'
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
                setQueryParamsLoaded={this.props.setQueryParamsLoaded}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <AllEventsHisto
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                all_events_loading={this.props.all_events_loading}
                all_events_result={this.props.all_events_result}
                all_events_error={this.props.all_events_error}
              />
            </Col>
            <Col>
              <CreateEventsHisto
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                create_events_loading={this.props.create_events_loading}
                create_events_result={this.props.create_events_result}
                create_events_error={this.props.create_events_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CommentEventsHisto
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                comment_events_loading={this.props.comment_events_loading}
                comment_events_result={this.props.comment_events_result}
                comment_events_error={this.props.comment_events_error}
              />
            </Col>
            <Col>
              <CloseEventsHisto
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                close_events_loading={this.props.close_events_loading}
                close_events_result={this.props.close_events_result}
                close_events_error={this.props.close_events_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <TopReviewers
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                top_reviewers_loading={this.props.top_reviewers_loading}
                top_reviewers_result={this.props.top_reviewers_result}
                top_reviewers_error={this.props.top_reviewers_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <TopCreators
                query={this.props.handleQuery}
                filter_loaded_from_url={this.props.filter_loaded_from_url}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                top_creators_loading={this.props.top_creators_loading}
                top_creators_result={this.props.top_creators_result}
                top_creators_error={this.props.top_creators_error}
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
            setQueryParamsLoaded={this.props.setQueryParamsLoaded}
            filter_loaded_from_url={this.props.filter_loaded_from_url}
            filter_gte={this.props.filter_gte}
            filter_lte={this.props.filter_lte}
            filter_repository={this.props.filter_repository}
            all_events_loading={this.props.all_events_loading}
            all_events_result={this.props.all_events_result}
            all_events_error={this.props.all_events_error}
            close_events_loading={this.props.close_events_loading}
            close_events_result={this.props.close_events_result}
            close_events_error={this.props.close_events_error}
            comment_events_loading={this.props.comment_events_loading}
            comment_events_result={this.props.comment_events_result}
            comment_events_error={this.props.comment_events_error}
            create_events_loading={this.props.create_events_loading}
            create_events_result={this.props.create_events_result}
            create_events_error={this.props.create_events_error}
            top_reviewers_loading={this.props.top_reviewers_loading}
            top_reviewers_result={this.props.top_reviewers_result}
            top_reviewers_error={this.props.top_reviewers_error}
            top_creators_loading={this.props.top_creators_loading}
            top_creators_result={this.props.top_creators_result}
            top_creators_error={this.props.top_creators_error}
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
    all_events_loading: state.QueryReducer.all_events_loading,
    all_events_result: state.QueryReducer.all_events_result,
    all_events_error: state.QueryReducer.all_events_error,
    close_events_loading: state.QueryReducer.close_events_loading,
    close_events_result: state.QueryReducer.close_events_result,
    close_events_error: state.QueryReducer.close_events_error,
    comment_events_loading: state.QueryReducer.comment_events_loading,
    comment_events_result: state.QueryReducer.comment_events_result,
    comment_events_error: state.QueryReducer.comment_events_error,
    create_events_loading: state.QueryReducer.create_events_loading,
    create_events_result: state.QueryReducer.create_events_result,
    create_events_error: state.QueryReducer.create_events_error,
    top_reviewers_loading: state.QueryReducer.top_reviewers_loading,
    top_reviewers_result: state.QueryReducer.top_reviewers_result,
    top_reviewers_error: state.QueryReducer.top_reviewers_error,
    top_creators_loading: state.QueryReducer.top_creators_loading,
    top_creators_result: state.QueryReducer.top_creators_result,
    top_creators_error: state.QueryReducer.top_creators_error,
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
  }
}
export default connect(mapStateToProps, mapDispatchToProps)(App);
