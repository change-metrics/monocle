// Monocle.
// Copyright (C) 2019-2020 Monocle authors

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

import React from 'react'
import { Switch, Route } from 'react-router-dom'

import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import PropTypes from 'prop-types'

import TopMenu from './components/menu'
import Footer from './components/footer'
import { LoginView, CUserView } from './components/user'
import { CChangesLifeCycleStats } from './components/changes_lifecycle'
import { CChangesReviewStats } from './components/changes_review'
import { CAuthorsHistoStats } from './components/authors_histo'
import {
  CNewContributorsStats,
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats
} from './components/top'
import { CHotChanges, CColdChanges, CLastChangesNG } from './components/changes'
import { CRepoChanges } from './components/repos_summary'
import { CApprovalsPie } from './components/approvals'
import CFiltersForm from './components/filtersform'
import { CChange } from './components/change'
import CReposPie from './components/repos_pie'
import CChangesAuthorsPie from './components/changes_authors_pie'
import TopView from './top_view'

class RootView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CChangesLifeCycleStats index={this.props.match.params.index} />
          </Col>
        </Row>
        <div className="pb-2 mb-2 border-bottom"></div>
        <Row>
          <Col>
            <CChangesReviewStats index={this.props.match.params.index} />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}

RootView.propTypes = {
  history: PropTypes.object.isRequired,
  location: PropTypes.object.isRequired,
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class PeopleView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CAuthorsHistoStats index={this.props.match.params.index} />
          </Col>
        </Row>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CMostActiveAuthorsStats
              search={this.props.location.search}
              index={this.props.match.params.index}
            />
          </Col>
        </Row>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CMostReviewedAuthorsStats index={this.props.match.params.index} />
          </Col>
        </Row>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CAuthorsPeersStats index={this.props.match.params.index} />
          </Col>
        </Row>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CNewContributorsStats index={this.props.match.params.index} />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}

PeopleView.propTypes = {
  history: PropTypes.object.isRequired,
  location: PropTypes.object.isRequired,
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class ReposView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CRepoChanges index={this.props.match.params.index} />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}
ReposView.propTypes = {
  history: PropTypes.object.isRequired,
  location: PropTypes.object.isRequired,
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class HotChangesView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CHotChanges index={this.props.match.params.index} />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}
HotChangesView.propTypes = {
  history: PropTypes.object.isRequired,
  location: PropTypes.object.isRequired,
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class ColdChangesView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CColdChanges index={this.props.match.params.index} />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}
ColdChangesView.propTypes = {
  history: PropTypes.object.isRequired,
  location: PropTypes.object.isRequired,
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class ChangesView extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      open: true
    }
  }

  render() {
    return (
      <React.Fragment>
        {this.state.open ? (
          <React.Fragment>
            <Row>
              <p></p>
            </Row>
            <Row className="row row-cols-1 row-cols-md-3 mb-2 text-left">
              <Col>
                <CChangesAuthorsPie
                  history={this.props.history}
                  index={this.props.match.params.index}
                />
              </Col>
              <Col>
                <CReposPie
                  history={this.props.history}
                  index={this.props.match.params.index}
                />
              </Col>
              <Col>
                <CApprovalsPie
                  history={this.props.history}
                  index={this.props.match.params.index}
                />
              </Col>
            </Row>
          </React.Fragment>
        ) : null}
        <Row>
          <Col>
            <CLastChangesNG
              history={this.props.history}
              index={this.props.match.params.index}
              showComplexityGraph={this.state.open}
            />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}

ChangesView.propTypes = {
  history: PropTypes.object.isRequired,
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class ChangeView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
        <Row>
          <Col>
            <CChange
              index={this.props.match.params.index}
              changeIds={this.props.match.params.change}
            />
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}

ChangeView.propTypes = {
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string,
      change: PropTypes.string
    })
  })
}

class FiltersFormBox extends React.Component {
  render() {
    return (
      <Switch>
        <Route exact path="/" />
        <Route path="/:index/change/" />
        <Route path="/:index/repos/">
          <CFiltersForm />
        </Route>
        <Route path="/:index/changes/">
          <CFiltersForm showChangeParams={true} />
        </Route>
        <Route path="/:index/hot-changes/">
          <CFiltersForm />
        </Route>
        <Route path="/:index/cold-changes/">
          <CFiltersForm />
        </Route>
        <Route path="/" component={CFiltersForm} />
      </Switch>
    )
  }
}

class App extends React.Component {
  render() {
    return (
      <React.Fragment>
        <div className="container-fluid">
          <TopMenu />

          <Switch>
            <Route exact path="/">
              <TopView />
            </Route>
          </Switch>

          <Row className="row">
            <FiltersFormBox />

            <Col className="col-md-8 border-left mb-5">
              <Switch>
                <Route exact path="/login" component={LoginView} />
                <Route exact path="/user" component={CUserView} />
                <Route exact path="/:index/people" component={PeopleView} />
                <Route exact path="/:index/repos" component={ReposView} />
                <Route exact path="/:index" component={RootView} />
                <Route path="/:index/changes" component={ChangesView} />
                <Route path="/:index/hot-changes" component={HotChangesView} />
                <Route
                  path="/:index/cold-changes"
                  component={ColdChangesView}
                />
                <Route path="/:index/change/:change" component={ChangeView} />
              </Switch>
            </Col>
          </Row>
        </div>
        <Footer />
      </React.Fragment>
    )
  }
}

export default App
