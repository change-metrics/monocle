import React from 'react';
import { connect } from "react-redux";

import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';

import { query } from './reducers/query'

import { Switch, Route } from 'react-router-dom'

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Tab from 'react-bootstrap/Tab'
import Tabs from 'react-bootstrap/Tabs'

import TopMenu from './components/menu'
import { CGlobalInfo } from './components/globalinfo'
import { CChangesLifeCycleStats } from './components/changes_lifecycle'
import { CChangesReviewStats } from './components/changes_review'
import {
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats,
} from './components/top'
import {
  HotChanges,
  ColdChanges,
  LastChanges,
} from './components/changes'
import {
  CApprovalStats,
} from './components/approvals'
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
                handleFilterExcludeAuthorsChange={this.props.handleFilterExcludeAuthorsChange}
                setQueryParamsLoaded={this.props.setQueryParamsLoaded}
                filter_gte={this.props.filter_gte}
                filter_lte={this.props.filter_lte}
                filter_repository={this.props.filter_repository}
                filter_interval={this.props.filter_interval}
                filter_exclude_authors={this.props.filter_exclude_authors}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Tabs defaultActiveKey="events stats" id="uncontrolled-tab-example">
            <Tab eventKey="events stats" title="Events stats">
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CGlobalInfo />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CChangesLifeCycleStats />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CChangesReviewStats />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CMostActiveAuthorsStats />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CApprovalStats />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CMostReviewedAuthorsStats />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CAuthorsPeersStats />
                </Col>
              </Row>
            </Tab>
            <Tab eventKey="changes" title="Changes status">
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <LastChanges
                    query={this.props.handleQuery}
                    filter_loaded_from_url={this.props.filter_loaded_from_url}
                    filter_gte={this.props.filter_gte}
                    filter_lte={this.props.filter_lte}
                    filter_repository={this.props.filter_repository}
                    filter_interval={this.props.filter_interval}
                    filter_exclude_authors={this.props.filter_exclude_authors}
                    last_changes_loading={this.props.last_changes_loading}
                    last_changes_result={this.props.last_changes_result}
                    last_changes_error={this.props.last_changes_error}
                  />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <HotChanges
                    query={this.props.handleQuery}
                    filter_loaded_from_url={this.props.filter_loaded_from_url}
                    filter_gte={this.props.filter_gte}
                    filter_lte={this.props.filter_lte}
                    filter_repository={this.props.filter_repository}
                    filter_interval={this.props.filter_interval}
                    filter_exclude_authors={this.props.filter_exclude_authors}
                    hot_changes_loading={this.props.hot_changes_loading}
                    hot_changes_result={this.props.hot_changes_result}
                    hot_changes_error={this.props.hot_changes_error}
                  />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <ColdChanges
                    query={this.props.handleQuery}
                    filter_loaded_from_url={this.props.filter_loaded_from_url}
                    filter_gte={this.props.filter_gte}
                    filter_lte={this.props.filter_lte}
                    filter_repository={this.props.filter_repository}
                    filter_interval={this.props.filter_interval}
                    filter_exclude_authors={this.props.filter_exclude_authors}
                    cold_changes_loading={this.props.cold_changes_loading}
                    cold_changes_result={this.props.cold_changes_result}
                    cold_changes_error={this.props.cold_changes_error}
                  />
                </Col>
              </Row>
            </Tab>
          </Tabs>
        </Container>
      </React.Fragment>)
  }
}


class App extends React.Component {
  render() {
    return (
      <Switch>
        <Route exact path='/'>
          <CRootView />
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
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    hot_changes_loading: state.QueryReducer.hot_changes_loading,
    hot_changes_result: state.QueryReducer.hot_changes_result,
    hot_changes_error: state.QueryReducer.hot_changes_error,
    cold_changes_loading: state.QueryReducer.cold_changes_loading,
    cold_changes_result: state.QueryReducer.cold_changes_result,
    cold_changes_error: state.QueryReducer.cold_changes_error,
    last_changes_loading: state.QueryReducer.last_changes_loading,
    last_changes_result: state.QueryReducer.last_changes_result,
    last_changes_error: state.QueryReducer.last_changes_error,
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
    handleFilterExcludeAuthorsChange: (value) => dispatch(
      {
        type: 'FILTER_EXCLUDE_AUTHORS_CHANGE',
        value: value
      }
    ),
  }
}

const CRootView = connect(mapStateToProps, mapDispatchToProps)(RootView);
export default App;
