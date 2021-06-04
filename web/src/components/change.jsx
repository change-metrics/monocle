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
  ErrorBox,
  addS,
  chooseApprovalBadgeStyle,
  ChangeStatus
} from './common'

import TimelineGraph from './timeline'
import CommitsTimelineGraph from './commits_timeline'
import arrayFromList from './ArrayFromList.bs.js'

class ChangeTable extends React.Component {
  render() {
    if (!this.props.change || this.props.events.length === 0) {
      return <ErrorBox error={{ status: 0, data: 'Invalid change' }} />
    }
    const change = this.props.change
    const events = this.props.events
    const popover = (
      <Popover id="popover-basic">
        <Popover.Title>
          Changed File{addS(change.changed_files_count)}
        </Popover.Title>
        <Popover.Content>
          {arrayFromList(change.changed_files).map((f, idx) => (
            <React.Fragment key={idx}>
              {f.path} (+{f.additions}-{f.deletions})<br />
            </React.Fragment>
          ))}
        </Popover.Content>
      </Popover>
    )
    const labels = arrayFromList(change.labels).map((l, idx) => (
      <Badge variant="warning" key={idx}>
        {l}
      </Badge>
    ))

    /* See: https://github.com/change-metrics/monocle/issues/533
    change.issue_tracker_links.forEach((e) => {
      change.title = change.title.replace(
        e[0],
        '<a href=' + e[1] + '>' + e[0] + '</a>'
      )
      change.text = change.text.replace(
        e[0],
        '<a href=' + e[1] + '>' + e[0] + '</a>'
      )
    })
    */
    const approvals = (
      <span key={0} style={{ float: 'right' }}>
        {arrayFromList(change.approval).map((app, idx) => (
          <span key={idx + 1}>{chooseApprovalBadgeStyle(app, idx + 1)} </span>
        ))}
      </span>
    )

    // complexicity from Change.res
    const complexity =
      change.changed_files_count + change.additions + change.deletions

    const testIncluded = <></>
    /* See: https://github.com/change-metrics/monocle/issues/534
                  <tr key={5}>
                    <td align="center">
                      {change.tests_included ? (
                        <font color="DarkGreen">Has tests</font>
                      ) : (
                        <font color="red">Does not have tests</font>
                      )}
                    </td>
                  </tr>
    */
    const hasIssue = <></>
    /* See: https://github.com/change-metrics/monocle/issues/533
                  <tr key={6}>
                    <td align="center">
                      {change.has_issue_tracker_links ? (
                        <font color="DarkGreen">Has an issue link</font>
                      ) : (
                        <font color="red">Does not have an issue link</font>
                      )}
                    </td>
                  </tr>
    */
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>
                <Interweave
                  content={change.title}
                  disableLineBreaks={false}
                  matchers={[new UrlMatcher('url')]}
                />
                <br />
                <br />
                <ChangeStatus data={change} /> {change.author} authored{' '}
                {moment(change.created_at).fromNow()} {approvals}
              </Card.Title>
              <Table striped responsive bordered hover size="sm">
                <tbody>
                  <tr key={0}>
                    <td align="center">
                      Repository: {change.repository_fullname} | Branch:{' '}
                      {change.target_branch}
                    </td>
                  </tr>
                  <tr key={1}>
                    <td align="center">
                      <a
                        href={change.url}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        {change.url}
                      </a>
                    </td>
                  </tr>
                  <tr key={2}>
                    <td align="center">
                      Complexity of {complexity} in {change.commits_count}{' '}
                      commit{addS(change.commits_count)} changing
                      <OverlayTrigger
                        trigger="click"
                        placement="right"
                        overlay={popover}
                      >
                        <Button variant="link">
                          {change.changed_files_count} file
                          {addS(change.changed_files_count)}
                        </Button>
                      </OverlayTrigger>
                    </td>
                  </tr>
                  <tr key={3}>
                    <td align="center">
                      Changed lines: +{change.additions}-{change.deletions}
                    </td>
                  </tr>
                  {labels.length !== 0 ? (
                    <tr key={4}>
                      <td align="center">{labels}</td>
                    </tr>
                  ) : null}
                  {testIncluded}
                  {hasIssue}
                </tbody>
              </Table>
            </Card.Header>
            <Card.Body>
              {change.text.split('\n').map((line, idx) => {
                return (
                  <Interweave
                    key={idx}
                    tagName="div"
                    content={line}
                    disableLineBreaks={false}
                    matchers={[new UrlMatcher('url')]}
                  />
                )
              })}
              <Row>
                <Col>
                  <TimelineGraph data={events} />
                </Col>
                <Col>
                  <CommitsTimelineGraph data={arrayFromList(change.commits)} />
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
  index: PropTypes.string.isRequired,
  change: PropTypes.any,
  events: PropTypes.any
}

export default ChangeTable
