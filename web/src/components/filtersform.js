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

import Col from 'react-bootstrap/Col'
import Row from 'react-bootstrap/Row'
import Card from 'react-bootstrap/Card'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Collapse from 'react-bootstrap/Collapse'
import Dropdown from 'react-bootstrap/Dropdown'
import DropDownButton from 'react-bootstrap/DropdownButton'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'

import DatePicker from 'react-datepicker'
import 'react-datepicker/dist/react-datepicker.css'
import moment from 'moment'

class DateFormBox extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      selected: '3-months'
    }
  }

  getDateString = (count, unit) => {
    return moment().subtract(count, unit)
      .format('YYYY-MM-DD')
  }

  handleClick = (e) => {
    const selected = e.target.attributes.value.value
    this.setState({ selected: selected })
    switch (selected) {
      case '1-week':
        this.props.handleChange(
          'gte', this.getDateString(7, 'days'))
        this.props.handleChange('lte', '')
        break
      case '2-weeks':
        this.props.handleChange(
          'gte', this.getDateString(14, 'days'))
        this.props.handleChange('lte', '')
        break
      case '1-month':
        this.props.handleChange(
          'gte', this.getDateString(1, 'months'))
        this.props.handleChange('lte', '')
        break
      case '3-months':
        this.props.handleChange(
          'gte', this.getDateString(3, 'months'))
        this.props.handleChange('lte', '')
        break
      case '6-months':
        this.props.handleChange(
          'gte', this.getDateString(6, 'months'))
        this.props.handleChange('lte', '')
        break
      case '1-year':
        this.props.handleChange(
          'gte', this.getDateString(1, 'years'))
        this.props.handleChange('lte', '')
        break
      case '2-years':
        this.props.handleChange(
          'gte', this.getDateString(2, 'years'))
        this.props.handleChange('lte', '')
        break
      case '3-years':
        this.props.handleChange(
          'gte', this.getDateString(3, 'years'))
        this.props.handleChange('lte', '')
        break
      default:
        break
    }
  }

  render () {
    return (
      <React.Fragment>
        <Row>
          <Col lg={6}>
            <Form.Group controlId='formFromDate'>
              <DatePicker
                selected={
                  this.props.gte
                    ? moment(this.props.gte).toDate() : ''}
                onChange={v => this.props.handleChange('gte', v)}
                dateFormat='yyyy-MM-dd'
                placeholderText='From date'
                showYearDropdown
              />
            </Form.Group>
            <Form.Group controlId='formToDate'>
              <DatePicker
                selected={
                  this.props.lte
                    ? moment(this.props.lte).toDate() : ''}
                onChange={v => this.props.handleChange('lte', v)}
                dateFormat='yyyy-MM-dd'
                placeholderText='To date'
                showYearDropdown
              />
            </Form.Group>
          </Col>
          <Col lg={6}>
            <Form.Group controlId='formToDate'>
              <DropDownButton
                title={'Relative date: ' + this.state.selected}
                size="sm"
                variant="secondary"
              >
                {[
                  ['1-week', '1 week'],
                  ['2-weeks', '2 weeks'],
                  ['1-month', '1 month'],
                  ['3-months', '3 months'],
                  ['6-months', '6 months'],
                  ['1-year', '1 year'],
                  ['2-years', '2 years'],
                  ['3-years', '3 years']].map(
                  (entry) => {
                    return <Dropdown.Item
                      key={entry[0]}
                      value={entry[0]}
                      active={entry[0] === this.state.selected}
                      onClick={this.handleClick}
                    >
                      {entry[1]}
                    </Dropdown.Item>
                  }
                )
                }
              </DropDownButton>
            </Form.Group>
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}

DateFormBox.propTypes = {
  gte: PropTypes.string.isRequired,
  lte: PropTypes.string.isRequired,
  handleChange: PropTypes.func.isRequired
}

class FiltersForm extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      gte: '',
      lte: '',
      repository: '',
      branch: '',
      files: '',
      excludeAuthors: '',
      authors: '',
      open: false,
      approvals: '',
      excludeApprovals: '',
      state: 'open'
    }
  }

  componentDidMount () {
    this.fetchQueryParams()
    this.unlisten = this.props.history.listen((location, action) => {
      this.fetchQueryParams()
    })
  }

  componentWillUnmount () {
    if (this.unlisten) {
      this.unlisten()
    }
  }

  handleChange = (key, e) => {
    let val = (e && e.target) ? e.target.value : e
    const assoc = {}
    if (key === 'gte' || key === 'lte') {
      val = moment(val).format('YYYY-MM-DD')
      if (val === 'Invalid date') {
        val = ''
      }
    } else if (!e) {
      val = ''
    }
    assoc[key] = val
    this.setState(assoc)
  }

  handleChangeState = (e) => {
    const state = e.target.attributes.value.value
    this.setState({ state: state })
  }

  fetchQueryParams = () => {
    const params = new URLSearchParams(this.props.history.location.search)
    this.setState({
      lte: params.get('lte') || '',
      gte: params.get('gte') || moment().subtract(3, 'months')
        .format('YYYY-MM-DD'),
      repository: params.get('repository') || '',
      branch: params.get('branch') || '',
      files: params.get('files') || '',
      excludeAuthors: params.get('exclude_authors') || '',
      authors: params.get('authors') || '',
      approvals: params.get('approvals') || '',
      excludeApprovals: params.get('exclude_approvals') || '',
      state: params.get('state') || ''
    })
  }

  updateHistoryURL = (params) => {
    const urlparams = new URLSearchParams(this.props.history.location.search)
    Object.keys(params).forEach(element => {
      if (params[element]) {
        urlparams.set(element, params[element])
      } else {
        urlparams.delete(element)
      }
    })
    if (this.props.history.location.search !== '?' + urlparams.toString()) {
      const newsearch = this.props.history.location.pathname + '?' + urlparams.toString()
      this.props.history.push(newsearch)
    }
  }

  handleSubmit = (event) => {
    this.updateHistoryURL({
      gte: this.state.gte,
      lte: this.state.lte,
      repository: this.state.repository,
      branch: this.state.branch,
      files: this.state.files,
      exclude_authors: this.state.excludeAuthors,
      authors: this.state.authors,
      approvals: this.state.approvals,
      exclude_approvals: this.state.excludeApprovals,
      state: this.state.state
    })
    event.preventDefault()
  }

  getFilterResume = () => {
    const params = new URLSearchParams(this.props.history.location.search)
    const resume = <b>Your are filtering on Changes or Changes&apos; events with filters:
      {' created between ' + (params.get('gte') || moment().subtract(3, 'months').format('YYYY-MM-DD')) + ' and ' + (params.get('lte') || 'now')}
      {params.get('repository') ? ', repositories named re(' + params.get('repository') + ')' : ''}
      {params.get('branch') ? ', branches named re(' + params.get('branch') + ')' : ''}
      {params.get('files') ? ', files named re(' + params.get('files') + ')' : ''}
      {params.get('authors') ? ', authors named ' + params.get('authors') : ''}
      {params.get('exclude_authors') ? ', excluded authors named ' + params.get('exclude_authors') : ''}
      {params.get('approvals') && this.props.showChangeParams ? ', approvals ' + params.get('approvals') : ''}
      {params.get('exclude_approvals') && this.props.showChangeParams ? ', excluded approvals named ' + params.get('exclude_approvals') : ''}
      {params.get('state') && this.props.showChangeParams ? ', change state ' + params.get('state') : ''}
      {'.'}
    </b>
    return resume
  }

  render () {
    const resumeStyle = {
      textAlign: 'center'
    }
    return (
      <React.Fragment>
        <Row>
          <Col>
            <Card>
              <Form onSubmit={this.handleSubmit}>
                <Card.Header>
                  <Row>
                    <Col>
                      <Card.Title>
                        Filters
                      </Card.Title>
                    </Col>
                    <Col>
                      <Button
                        className='float-right'
                        onClick={() => this.setState({ open: !this.state.open })}
                        aria-controls='example-collapse-text'
                        aria-expanded={this.state.open}
                        variant='outline-secondary'
                        size='sm'
                      >
                        {this.state.open
                          ? <svg className="bi bi-chevron-double-up" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                            <path fillRule="evenodd" d="M7.646 2.646a.5.5 0 01.708 0l6 6a.5.5 0 01-.708.708L8 3.707 2.354 9.354a.5.5 0 11-.708-.708l6-6z" clipRule="evenodd" />
                            <path fillRule="evenodd" d="M7.646 6.646a.5.5 0 01.708 0l6 6a.5.5 0 01-.708.708L8 7.707l-5.646 5.647a.5.5 0 01-.708-.708l6-6z" clipRule="evenodd" />
                          </svg>
                          : <svg className="bi bi-chevron-double-down" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                            <path fillRule="evenodd" d="M1.646 6.646a.5.5 0 01.708 0L8 12.293l5.646-5.647a.5.5 0 01.708.708l-6 6a.5.5 0 01-.708 0l-6-6a.5.5 0 010-.708z" clipRule="evenodd" />
                            <path fillRule="evenodd" d="M1.646 2.646a.5.5 0 01.708 0L8 8.293l5.646-5.647a.5.5 0 01.708.708l-6 6a.5.5 0 01-.708 0l-6-6a.5.5 0 010-.708z" clipRule="evenodd" />
                          </svg>
                        }
                      </Button>
                    </Col>
                  </Row>
                </Card.Header>
                <Collapse in={this.state.open}>
                  <Card.Body id="example-collapse-text">
                    <Row>
                      <Col md={6}>
                        <DateFormBox
                          gte={this.state.gte}
                          lte={this.state.lte}
                          handleChange={this.handleChange}
                        />
                      </Col>
                      <Col md={6}>
                        <Row>
                          <Col md={6}>
                            <Form.Group controlId='formAuthorsInput'>
                              <Form.Control
                                type='text'
                                value={this.state.authors}
                                placeholder="Authors"
                                onChange={v => this.handleChange('authors', v)}
                              />
                            </Form.Group>
                            <Form.Group controlId='formExcludeAuthorsInput'>
                              <Form.Control
                                type='text'
                                value={this.state.excludeAuthors}
                                placeholder="Exclude Authors"
                                onChange={v => this.handleChange('excludeAuthors', v)}
                              />
                            </Form.Group>
                            {this.props.showChangeParams
                              ? <Form.Group controlId='formApprovalsInput'>
                                <Form.Control
                                  type='text'
                                  value={this.state.approvals}
                                  placeholder="Approvals"
                                  onChange={v => this.handleChange('approvals', v)}
                                />
                              </Form.Group> : null}
                            {this.props.showChangeParams
                              ? <Form.Group controlId='formExcludeApprovalsInput'>
                                <Form.Control
                                  type='text'
                                  value={this.state.excludeApprovals}
                                  placeholder="Exclude Approvals"
                                  onChange={v => this.handleChange('excludeApprovals', v)}
                                />
                              </Form.Group> : null}
                          </Col>
                          <Col md={6}>
                            <Form.Group controlId='formRepositoryInput'>
                              <Form.Control
                                type='text'
                                value={this.state.repository}
                                placeholder="Repositories regexp"
                                onChange={v => this.handleChange('repository', v)}
                              />
                            </Form.Group>
                            <Form.Group controlId='formBranchInput'>
                              <Form.Control
                                type='text'
                                value={this.state.branch}
                                placeholder="Branch regexp"
                                onChange={v => this.handleChange('branch', v)}
                              />
                            </Form.Group>
                            <Form.Group controlId='formFilesInput'>
                              <Form.Control
                                type='text'
                                value={this.state.files}
                                placeholder="Files regexp"
                                onChange={v => this.handleChange('files', v)}
                              />
                            </Form.Group>
                            {this.props.showChangeParams
                              ? <Form.Group controlId='changeStateInput'>
                                <DropDownButton
                                  title={this.state.state ? 'Change state: ' + this.state.state : 'Change state: ALL'}
                                  size="sm"
                                  variant="secondary"
                                >
                                  {[
                                    ['', 'All'],
                                    ['OPEN', 'Open'],
                                    ['CLOSED', 'Closed'],
                                    ['MERGED', 'Merged']
                                  ].map(
                                    (entry) => {
                                      return <Dropdown.Item
                                        key={entry[0]}
                                        value={entry[0]}
                                        active={entry[0] === this.state.state}
                                        onClick={this.handleChangeState}
                                      >
                                        {entry[1]}
                                      </Dropdown.Item>
                                    }
                                  )
                                  }
                                </DropDownButton></Form.Group> : null}
                            <Form.Group controlId='formSubmit'>
                              <Button
                                className='float-right'
                                variant='primary'
                                type='submit'
                                size='sm'
                              >Apply</Button>
                            </Form.Group>
                          </Col>
                        </Row>
                      </Col>
                    </Row>
                  </Card.Body>
                </Collapse>
              </Form>
            </Card >
          </Col>
        </Row>
        <Row><Col><p></p></Col></Row>
        <Row>
          <Col>
            <p style={resumeStyle}>
              <b>
                {this.getFilterResume()}
              </b>
            </p>
          </Col>
        </Row>
      </React.Fragment>
    )
  }
}

FiltersForm.propTypes = {
  history: PropTypes.object.isRequired,
  showChangeParams: PropTypes.bool
}

FiltersForm.defaultProps = {
  showChangeParams: false
}

const CFiltersForm = withRouter(FiltersForm)

export default CFiltersForm
