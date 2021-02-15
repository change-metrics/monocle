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

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'
import PropTypes from 'prop-types'
import { withRouter, Link } from 'react-router-dom'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addUrlField,
  mapDispatchToProps,
  addMap
} from './common'

class RepoChangesTable extends React.Component {
  createLink (index, name, type, field) {
    const search = new URLSearchParams(window.location.search)
    search.set('state', type)
    search.set('repository', name)
    const newurl = '/' + index + '/changes?' + search.toString()
    const linkName = this.props.data.summary[name][field]
    return <Link to={newurl}>{linkName}</Link>
  }

  render () {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover size="sm">
                <thead>
                  <tr>
                    <th className="text-center">Repository</th>
                    <th className="text-center">Total changes</th>
                    <th className="text-center">Open changes</th>
                    <th className="text-center">Merged changes</th>
                    <th className="text-center">Abandoned changes</th>
                  </tr>
                </thead>
                <tbody>
                  {Object.keys(this.props.data.summary).map((name, index) =>
                    <tr key={index}>
                      <td align="center"><Link to={addUrlField('repository', name)}>{name}</Link></td>
                      <td align="center">{this.createLink(this.props.index, name, 'ALL', 'changes')}</td>
                      <td align="center">{this.createLink(this.props.index, name, 'OPEN', 'changes_open')}</td>
                      <td align="center">{this.createLink(this.props.index, name, 'MERGED', 'changes_merged')}</td>
                      <td align="center">{this.createLink(this.props.index, name, 'CLOSED', 'changes_abandoned')}</td>
                    </tr>)}
                </tbody>
              </Table>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

RepoChangesTable.propTypes = {
  title: PropTypes.string.isRequired,
  index: PropTypes.string.isRequired,
  data: PropTypes.shape({
    summary: PropTypes.object
  })
}

class RepoChanges extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'repos_summary'
    this.state.graph_type = 'repos_summary'
  }

  render () {
    if (!this.props.repos_summary_loading) {
      if (this.props.repos_summary_error) {
        return <ErrorBox
          error={this.props.repos_summary_error}
        />
      }
      const data = this.props.repos_summary_result
      return (
        <RepoChangesTable
          index={this.props.index}
          data={data}
          title="Repositories summary"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

const reposTopMergedMapStateToProps = state => addMap({}, state.QueryReducer, 'repos_summary')

const CRepoChanges = withRouter(connect(reposTopMergedMapStateToProps, mapDispatchToProps)(RepoChanges))

export {
  CRepoChanges
}
