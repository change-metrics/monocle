import React from 'react'

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'
import Badge from 'react-bootstrap/Badge'
import Button from 'react-bootstrap/Button'
import Popover from 'react-bootstrap/Popover'
import OverlayTrigger from 'react-bootstrap/OverlayTrigger'
import PropTypes from 'prop-types'

import moment from 'moment'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addS
} from './common'

import { TimelineGraph } from './timeline'

class ChangeStatus extends React.Component {
  render () {
    switch (this.props.data.state) {
      case 'OPEN':
        return <Badge variant="success">Open</Badge>
      case 'MERGED':
        return <Badge variant="primary">Merged</Badge>
      case 'CLOSED':
        return <Badge variant="danger">Closed</Badge>
      default:
        return null
    }
  }
}

ChangeStatus.propTypes = {
  data: PropTypes.object
}

class ChangeTable extends React.Component {
  processText (index, change) {
    var loop = 0
    const processLine = (line) => {
      const words = line.split(' ')
      const githubRegexp = /(([^/ :]+)\/([^/]+)#([0-9]+))/
      const urlRegexp = /(https?:\/\/.*)/
      const processWord = (w) => {
        if (w.match(githubRegexp)) {
          loop++
          return <a key={loop} href={w.replace(githubRegexp, `/${index}/change/$2@$3@$4`)}>{w.replace(githubRegexp, '$1 ')}</a>
        }
        if (w.match(urlRegexp)) {
          loop++
          return <a key={loop} href={w} target="_blank" rel="noopener noreferrer">{w }</a>
        }
        return w + ' '
      }
      return words.map(processWord)
    }
    return change.text.split('\n').map((line, idx) => <React.Fragment key={idx}>{processLine(line)}<br/></React.Fragment>)
  }

  render () {
    if (!this.props.data || this.props.data.items.length === 0) {
      return <ErrorBox error={{ status: 0, data: 'Invalid change' }}/>
    }
    const changes = this.props.data.items.filter(x => x.type === 'Change')
    if (changes.length === 0) {
      return <ErrorBox error={{ status: 1, data: 'No change found' }}/>
    }
    const change = changes[0]
    const events = this.props.data.items.filter(x => x.type !== 'Change')
    const popover = <Popover id="popover-basic">
      <Popover.Title>Changed File{addS(change.changed_files_count)}</Popover.Title>
      <Popover.Content>
        {change.changed_files.map((f, idx) => <React.Fragment key={idx}>{f.path} (+{f.additions}-{f.deletions})<br/></React.Fragment>)}
      </Popover.Content>
    </Popover>
    const labels = change.labels.map((l, idx) => <Badge variant="warning" key={idx}>{l}</Badge>)

    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{change.title}<br/><ChangeStatus data={change} /> {change.author} authored {moment(change.created_at).fromNow()}</Card.Title>
              <Table>
                <tbody>
                  <tr key={0}>
                    <td align="center">Repository: {change.repository_fullname}</td>
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
                    <td align="center">{change.hasTests ? <font color="DarkGreen">Has tests</font> : <font color="red">Does not have tests</font>}</td>
                  </tr>
                  <tr key={6}>
                    <td align="center">{change.hasLinks ? <font color="DarkGreen">Has a potential issue link</font> : <font color="red">Does not have an issue link</font>}</td>
                  </tr>
                </tbody>
              </Table>
            </Card.Header>
            <Card.Body>
              <Card.Text>{this.processText(this.props.index, change)}</Card.Text>
              <TimelineGraph data={events} index={this.props.index} />
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
    this.pageSize = 100
  }

  componentDidMount () {
    this.queryBackend()
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

const mapStateToProps = state => {
  return {
    // we do not use filters
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    changes_and_events_loading: state.QueryReducer.changes_and_events_loading,
    changes_and_events_result: state.QueryReducer.changes_and_events_result,
    changes_and_events_error: state.QueryReducer.changes_and_events_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CChange = connect(mapStateToProps, mapDispatchToProps)(Change)

export {
  CChange
}
