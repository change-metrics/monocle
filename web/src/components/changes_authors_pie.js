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

import { connect } from 'react-redux'

import Card from 'react-bootstrap/Card'
import { withRouter } from 'react-router-dom'

import {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent,
  mapDispatchToProps,
  addMap
} from './common'

import Pie from './pie'

class ChangesAuthorsPie extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'authors_top'
    this.state.graph_type = 'authors_top'
  }

  render() {
    if (!this.props.authors_top_loading) {
      if (this.props.authors_top_error) {
        return <ErrorBox error={this.props.authors_top_error} />
      }
      if (!this.props.authors_top_result) {
        return (
          <ErrorBox error={{ data: 'No data for authors top', status: 0 }} />
        )
      }
      return (
        <Card className="rounded border-0 border-top">
          <Card.Header className="bg-white text-center">
            <Card.Title>Changes per author</Card.Title>
          </Card.Header>
          <Card.Body>
            <Pie
              field="authors"
              history={this.props.history}
              data={this.props.authors_top_result}
            />
          </Card.Body>
        </Card>
      )
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = (state) => addMap({}, state.QueryReducer, 'authors_top')

const CChangesAuthorsPie = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(ChangesAuthorsPie)
)

export default CChangesAuthorsPie
