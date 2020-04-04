import React from 'react';

import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';

import { Switch, Route } from 'react-router-dom'

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Tab from 'react-bootstrap/Tab'
import Tabs from 'react-bootstrap/Tabs'

import TopMenu from './components/menu'
import { CChangesLifeCycleStats } from './components/changes_lifecycle'
import { CChangesReviewStats } from './components/changes_review'
import {
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats,
} from './components/top'
import {
  CRepoChanges,
  CHotChanges,
  CColdChanges,
  CLastChanges,
} from './components/changes'
import {
  CApprovalStats,
} from './components/approvals'
import {
  CFiltersForm,
} from './components/filtersform'


class RootView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <TopMenu  index={this.props.match.params.index}/>
        <Container>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CFiltersForm index={this.props.match.params.index}/>
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Tabs defaultActiveKey="events stats" id="uncontrolled-tab-example">
            <Tab eventKey="events stats" title="Events stats">
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
                  <CLastChanges />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CRepoChanges />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CHotChanges />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CColdChanges />
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
        <Route exact path='/:index' component={RootView} />
      </Switch>
    )
  }
}

export default App;
