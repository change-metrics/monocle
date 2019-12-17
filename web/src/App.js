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
import EventsHisto from './components/status'


class RootView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <TopMenu />
        <Container>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <EventsHisto
                query={this.props.handleQuery}
                loading={this.props.query_loading}
                result={this.props.query_result}
                error={this.props.query_error}
              />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <EventsHisto
                query={this.props.handleQuery}
                loading={this.props.query_loading}
                result={this.props.query_result}
                error={this.props.query_error}
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
            query_result={this.props.query_result}
            query_loading={this.props.query_loading}
            query_error={this.props.query_error}
          />
        </Route>
      </Switch>
    )
  }
}

const mapStateToProps = state => {
  return {
    query_result: state.QueryReducer.result,
    query_error: state.QueryReducer.error_response,
    query_loading: state.QueryReducer.loading,
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))}
}
export default connect(mapStateToProps, mapDispatchToProps)(App);
