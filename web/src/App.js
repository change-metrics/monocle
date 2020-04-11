import React from 'react'

import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css'

import { Switch, Route } from 'react-router-dom'

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Tab from 'react-bootstrap/Tab'
import Tabs from 'react-bootstrap/Tabs'
import PropTypes from 'prop-types'

import TopMenu from './components/menu'
import { CChangesLifeCycleStats } from './components/changes_lifecycle'
import { CChangesReviewStats } from './components/changes_review'
import {
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats
} from './components/top'
import {
  CRepoChanges,
  CHotChanges,
  CColdChanges,
  CLastChanges,
  CLastMergedChanges,
  CLastOpenedChanges
} from './components/changes'
import {
  CApprovalStats
} from './components/approvals'
import {
  CFiltersForm
} from './components/filtersform'

class RootView extends React.Component {
  render () {
    return (
      <React.Fragment>
        <TopMenu index={this.props.match.params.index} />
        <Container>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CFiltersForm />
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Tabs defaultActiveKey="events stats" id="uncontrolled-tab-example">
            <Tab eventKey="events stats" title="Events stats">
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CChangesLifeCycleStats
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CChangesReviewStats
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CMostActiveAuthorsStats
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CApprovalStats
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CMostReviewedAuthorsStats
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CAuthorsPeersStats
                    index={this.props.match.params.index} />
                </Col>
              </Row>
            </Tab>
            <Tab eventKey="changes" title="Changes status">
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CLastChanges
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CRepoChanges
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CHotChanges
                    index={this.props.match.params.index} />
                </Col>
              </Row>
              <Row><Col><p></p></Col></Row>
              <Row>
                <Col>
                  <CColdChanges
                    index={this.props.match.params.index} />
                </Col>
              </Row>
            </Tab>
          </Tabs>
        </Container>
      </React.Fragment>)
  }
}

RootView.propTypes = {
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class MergedChangesView extends React.Component {
  render () {
    return (
      <React.Fragment>
        <TopMenu index={this.props.match.params.index}/>
        <Container>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CFiltersForm index={this.props.match.params.index}/>
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CLastMergedChanges index={this.props.match.params.index} />
            </Col>
          </Row>
        </Container>
      </React.Fragment>)
  }
}

MergedChangesView.propTypes = {
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class OpenChangesView extends React.Component {
  render () {
    return (
      <React.Fragment>
        <TopMenu index={this.props.match.params.index}/>
        <Container>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CFiltersForm index={this.props.match.params.index}/>
            </Col>
          </Row>
          <Row><Col><p></p></Col></Row>
          <Row><Col><p></p></Col></Row>
          <Row>
            <Col>
              <CLastOpenedChanges index={this.props.match.params.index} />
            </Col>
          </Row>
        </Container>
      </React.Fragment>)
  }
}

OpenChangesView.propTypes = {
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class App extends React.Component {
  render () {
    return (
      <Switch>
        <Route exact path='/:index' component={RootView} />
        <Route path='/:index/merged-changes' component={MergedChangesView} />
        <Route path='/:index/opened-changes' component={OpenChangesView} />
      </Switch>
    )
  }
}

export default App
