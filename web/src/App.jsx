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

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import PropTypes from 'prop-types'

import TopMenu from './components/menu'
import Footer from './components/footer'
import { CChangesLifeCycleStats } from './components/changes_lifecycle'
import { CChangesReviewStats } from './components/changes_review'
import { CAuthorsHistoStats } from './components/authors_histo'
import {
  CNewContributorsStats,
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats
} from './components/top'
import { CHotChanges, CColdChanges } from './components/changes'
import { CRepoChanges } from './components/repos_summary'
import CFiltersForm from './components/filtersform'
import { CChange } from './components/change'
import Indices from './components/Indices.bs.js'
import ChangesView from './components/ChangesView.bs.js'
import Board from './components/Board.bs.js'
import Store from './components/Store.bs.js'

class RootView extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Row>
          <Col>
            <CFiltersForm index={this.props.match.params.index} />
          </Col>
        </Row>
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
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
        <Row>
          <Col>
            <p></p>
          </Col>
        </Row>
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
            <CFiltersForm index={this.props.match.params.index} />
          </Col>
        </Row>
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
        <CFiltersForm index={this.props.match.params.index} />
        <CRepoChanges index={this.props.match.params.index} />
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
        <CFiltersForm index={this.props.match.params.index} />
        <CHotChanges index={this.props.match.params.index} />
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
        <CFiltersForm index={this.props.match.params.index} />
        <CColdChanges index={this.props.match.params.index} />
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

class ChangesViewRoute extends React.Component {
  render() {
    return (
      <>
        <CFiltersForm
          index={this.props.match.params.index}
          showChangeParams={true}
        />
        <ChangesView index={this.props.match.params.index} />
      </>
    )
  }
}

ChangesViewRoute.propTypes = {
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

class ChangeView extends React.Component {
  render() {
    return (
      <CChange
        index={this.props.match.params.index}
        changeIds={this.props.match.params.change}
      />
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

const BoardView = (data) => <Board store={data.store} />

BoardView.propTypes = {
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

const LegacyApp = (data) => (
  <Container>
    <Row>
      <Col>
        <p></p>
      </Col>
    </Row>
    <Switch>
      <Route
        exact
        path="/"
        render={(routeProps) => <Indices store={data.store} {...routeProps} />}
      />
      <Route exact path="/:index/people" component={PeopleView} />
      <Route exact path="/:index/repos" component={ReposView} />
      <Route exact path="/:index" component={RootView} />
      <Route path="/:index/changes" component={ChangesViewRoute} />
      <Route path="/:index/hot-changes" component={HotChangesView} />
      <Route path="/:index/cold-changes" component={ColdChangesView} />
      <Route path="/:index/change/:change" component={ChangeView} />
      <Route path="/:index/board" component={BoardView} />
    </Switch>
    <Row>
      <Col>
        <p></p>
      </Col>
    </Row>
    <Row>
      <Col>
        <p></p>
      </Col>
    </Row>
    <Row>
      <Col>
        <p></p>
      </Col>
    </Row>
    <Row>
      <Col>
        <p></p>
      </Col>
    </Row>
    <Row>
      <Col>
        <p></p>
      </Col>
    </Row>
  </Container>
)

const App = () => {
  const [_, index] = window.location.pathname.split('/')
  const store = Store.use(index)
  return (
    <React.Fragment>
      <TopMenu store={store} />
      <Switch>
        <Route
          path="/:index/board"
          render={() => <BoardView store={store} />}
        />
        <Route path="/*" render={() => <LegacyApp store={store} />} />
      </Switch>
      <Footer />
    </React.Fragment>
  )
}

export default App
