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
import Badge from 'react-bootstrap/Badge'
import Button from 'react-bootstrap/Button'
import Popover from 'react-bootstrap/Popover'
import OverlayTrigger from 'react-bootstrap/OverlayTrigger'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'

import Interweave from 'interweave'
import { UrlMatcher } from 'interweave-autolink'

import moment from 'moment'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addS,
  mapDispatchToProps,
  addMap,
  chooseApprovalBadgeStyle,
  ChangeStatus
} from './common'

import TimelineGraph from './timeline'
import CommitsTimelineGraph from './commits_timeline'

class ChangeTable extends React.Component {
  render () {
    if (!this.props.data || this.props.data.items.length === 0) {
      return <ErrorBox error={{ status: 0, data: 'Invalid change' }} />
    }
    const changes = this.props.data.items.filter(x => x.type === 'Change')
    if (changes.length === 0) {
      return <ErrorBox error={{ status: 1, data: 'No change found' }} />
    }
    const change = changes[0]
    const events = this.props.data.items.filter(x => x.type !== 'Change')
    const popover = <Popover id="popover-basic">
      <Popover.Title>Changed File{addS(change.changed_files_count)}</Popover.Title>
      <Popover.Content>
        {change.changed_files.map((f, idx) => <React.Fragment key={idx}>{f.path} (+{f.additions}-{f.deletions})<br /></React.Fragment>)}
      </Popover.Content>
    </Popover>
    const labels = change.labels.map((l, idx) => <Badge variant="warning" key={idx}>{l}</Badge>)
    change.issue_tracker_links.forEach(e => {
      change.title = change.title.replace(e[0], '<a href=' + e[1] + '>' + e[0] + '</a>')
      change.text = change.text.replace(e[0], '<a href=' + e[1] + '>' + e[0] + '</a>')
    })

    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>
                <Interweave
                  content={change.title}
                  disableLineBreaks={false}
                  matchers={[new UrlMatcher('url')]} />
                <br />
                <br />
                <ChangeStatus data={change} /> {change.author.muid} authored {moment(change.created_at).fromNow()} <span key={0} style={{ float: 'right' }}>{change.approval.map((app, idx) => <span key={idx + 1}>{chooseApprovalBadgeStyle(app, idx + 1)} </span>)}</span></Card.Title>
              <Table striped responsive bordered hover size="sm">
                <tbody>
                  <tr key={0}>
                    <td align="center">Repository: {change.repository_fullname} | Branch: {change.target_branch}</td>
                  </tr>
                  <tr key={1}>
                    <td align="center"><a href={change.url} target="_blank" rel="noopener noreferrer">{change.url}</a></td>
                  </tr>
                  <tr key={2}>
                    <td align="center">Complexity of {change.complexity} in {change.commit_count} commit{addS(change.commit_count)} changing<OverlayTrigger trigger="click" placement="right" overlay={popover}><Button variant="link">{change.changed_files_count} file{addS(change.changed_files_count)}</Button></OverlayTrigger>
                    </td>
                  </tr>
                  <tr key={3}>
                    <td align="center">Changed lines: +{change.additions}-{change.deletions}</td>
                  </tr>
                  {labels.length !== 0
                    ? <tr key={4}>
                      <td align="center">{labels}</td>
                    </tr> : null
                  }
                  <tr key={5}>
                    <td align="center">{change.tests_included ? <font color="DarkGreen">Has tests</font> : <font color="red">Does not have tests</font>}</td>
                  </tr>
                  <tr key={6}>
                    <td align="center">{change.has_issue_tracker_links ? <font color="DarkGreen">Has an issue link</font> : <font color="red">Does not have an issue link</font>}</td>
                  </tr>
                </tbody>
              </Table>
            </Card.Header>
            <Card.Body>
              {change.text.split('\n').map((line, idx) => {
                return <Interweave
                  key={idx}
                  tagName="div"
                  content={line}
                  disableLineBreaks={false}
                  matchers={[new UrlMatcher('url')]} />
              })
              }
              <Row>
                <Col>
                  <TimelineGraph data={events} />
                </Col>
                <Col>
                  <CommitsTimelineGraph data={change.commits} />
                </Col>
              </Row>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

ChangeTable.propTypes = {
  data: PropTypes.object,
  index: PropTypes.string.isRequired
}

class Change extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'changes_and_events'
    this.state.graph_type = 'changes_and_events'
    this.state.pageSize = 100
    this.state.forceAllAuthors = true
  }

  render () {
    if (!this.props.changes_and_events_loading) {
      if (this.props.changes_and_events_error) {
        return <ErrorBox
          error={this.props.changes_and_events_error}
        />
      }
      const data = this.props.changes_and_events_result
      return (
        <ChangeTable
          data={data}
          index={this.props.index}
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

Change.propTypes = {
  index: PropTypes.string.isRequired
}

const mapStateToProps = state => addMap({}, state.QueryReducer, 'changes_and_events')

const CChange = withRouter(connect(mapStateToProps, mapDispatchToProps)(Change))

export {
  CChange
}
