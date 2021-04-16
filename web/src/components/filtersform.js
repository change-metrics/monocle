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
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import Dropdown from 'react-bootstrap/Dropdown'
import DropDownButton from 'react-bootstrap/DropdownButton'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'

/* import DatePicker from 'react-datepicker'  */
import 'react-datepicker/dist/react-datepicker.css'
import moment from 'moment'

class DateFormBox extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      selected: '3-months'
    }
  }

  getDateString = (count, unit) => {
    return moment().subtract(count, unit).format('YYYY-MM-DD')
  }

  handleClick = (e) => {
    const selected = e.target.attributes.value.value
    this.setState({ selected: selected })
    switch (selected) {
      case '1-week':
        this.props.handleChange('gte', this.getDateString(7, 'days'))
        this.props.handleChange('lte', '')
        break
      case '2-weeks':
        this.props.handleChange('gte', this.getDateString(14, 'days'))
        this.props.handleChange('lte', '')
        break
      case '1-month':
        this.props.handleChange('gte', this.getDateString(1, 'months'))
        this.props.handleChange('lte', '')
        break
      case '3-months':
        this.props.handleChange('gte', this.getDateString(3, 'months'))
        this.props.handleChange('lte', '')
        break
      case '6-months':
        this.props.handleChange('gte', this.getDateString(6, 'months'))
        this.props.handleChange('lte', '')
        break
      case '1-year':
        this.props.handleChange('gte', this.getDateString(1, 'years'))
        this.props.handleChange('lte', '')
        break
      case '2-years':
        this.props.handleChange('gte', this.getDateString(2, 'years'))
        this.props.handleChange('lte', '')
        break
      case '3-years':
        this.props.handleChange('gte', this.getDateString(3, 'years'))
        this.props.handleChange('lte', '')
        break
      default:
        break
    }
  }

  render() {
    return (
      <React.Fragment>
        <Form.Group controlId="formFromDate">
          <Row>
            <Form.Label>From date:</Form.Label>
            <Form.Control
              type="date"
              selected={this.props.gte ? moment(this.props.gte).toDate() : ''}
              onChange={(v) => this.props.handleChange('gte', v)}
              pattern="yyyy-MM-dd"
              value={this.props.gte}
            />
          </Row>
        </Form.Group>
        <Form.Group controlId="formToDate">
          <Row>
            <Form.Label>To date:</Form.Label>
            <Form.Control
              type="date"
              selected={this.props.lte ? moment(this.props.lte).toDate() : ''}
              onChange={(v) => this.props.handleChange('lte', v)}
              pattern="yyyy-MM-dd"
              value={this.props.lte}
            />
          </Row>
        </Form.Group>
        <Form.Group controlId="formToDate">
          <Row>
            <DropDownButton
              title={'Relative date: ' + this.state.selected}
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
                ['3-years', '3 years']
              ].map((entry) => {
                return (
                  <Dropdown.Item
                    key={entry[0]}
                    value={entry[0]}
                    active={entry[0] === this.state.selected}
                    onClick={this.handleClick}
                  >
                    {entry[1]}
                  </Dropdown.Item>
                )
              })}
            </DropDownButton>
          </Row>
        </Form.Group>
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
  constructor(props) {
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
      state: 'open',
      selfApproved: false
    }
  }

  componentDidMount() {
    this.fetchQueryParams()
    this.unlisten = this.props.history.listen((location, action) => {
      this.fetchQueryParams()
    })
  }

  componentWillUnmount() {
    if (this.unlisten) {
      this.unlisten()
    }
  }

  handleChange = (key, e) => {
    let val = e && e.target ? e.target.value : e
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
      gte:
        params.get('gte') ||
        moment().subtract(3, 'months').format('YYYY-MM-DD'),
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
    Object.keys(params).forEach((element) => {
      if (params[element]) {
        urlparams.set(element, params[element])
      } else {
        urlparams.delete(element)
      }
    })
    if (this.props.history.location.search !== '?' + urlparams.toString()) {
      const newsearch =
        this.props.history.location.pathname + '?' + urlparams.toString()
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
    const resume = (
      <b>
        Your are filtering on Changes or Changes&apos; events with filters:
        {' created between ' +
          (params.get('gte') ||
            moment().subtract(3, 'months').format('YYYY-MM-DD')) +
          ' and ' +
          (params.get('lte') || 'now')}
        {params.get('repository')
          ? ', repositories named re(' + params.get('repository') + ')'
          : ''}
        {params.get('branch')
          ? ', branches named re(' + params.get('branch') + ')'
          : ''}
        {params.get('files')
          ? ', files named re(' + params.get('files') + ')'
          : ''}
        {params.get('authors')
          ? ', authors named ' + params.get('authors')
          : ''}
        {params.get('exclude_authors')
          ? ', excluded authors named ' + params.get('exclude_authors')
          : ''}
        {params.get('approvals') && this.props.showChangeParams
          ? ', approvals ' + params.get('approvals')
          : ''}
        {params.get('exclude_approvals') && this.props.showChangeParams
          ? ', excluded approvals named ' + params.get('exclude_approvals')
          : ''}
        {params.get('state') && this.props.showChangeParams
          ? ', change state ' + params.get('state')
          : ''}
        {'.'}
      </b>
    )
    return resume
  }

  clearFormular = () => {
    this.props.history.push(this.props.history.location.pathname)
  }

  render() {
    const resumeStyle = {
      textAlign: 'center'
    }
    return (
      <Col className="col-md-2 bg-white pt-4">
        <React.Fragment>
          <Form onSubmit={this.handleSubmit}>
            <h4>Filters:</h4>
            <div className="pb-2 mb-2 border-bottom"></div>
            <Col>
              <DateFormBox
                gte={this.state.gte}
                lte={this.state.lte}
                handleChange={this.handleChange}
              />
              <Form.Group controlId="formAuthorsInput">
                <Row>
                  <Form.Control
                    type="text"
                    value={this.state.authors}
                    placeholder="Authors"
                    onChange={(v) => this.handleChange('authors', v)}
                  />
                </Row>
              </Form.Group>
              <Form.Group controlId="formExcludeAuthorsInput">
                <Row>
                  <Form.Control
                    type="text"
                    value={this.state.excludeAuthors}
                    placeholder="Exclude Authors"
                    onChange={(v) => this.handleChange('excludeAuthors', v)}
                  />
                </Row>
              </Form.Group>
              {this.props.showChangeParams ? (
                <Form.Group controlId="formApprovalsInput">
                  <Row>
                    <Form.Control
                      type="text"
                      value={this.state.approvals}
                      placeholder="Approvals"
                      onChange={(v) => this.handleChange('approvals', v)}
                    />
                  </Row>
                </Form.Group>
              ) : null}
              {this.props.showChangeParams ? (
                <Form.Group controlId="formExcludeApprovalsInput">
                  <Row>
                    <Form.Control
                      type="text"
                      value={this.state.excludeApprovals}
                      placeholder="Exclude Approvals"
                      onChange={(v) => this.handleChange('excludeApprovals', v)}
                    />
                  </Row>
                </Form.Group>
              ) : null}
              <Form.Group controlId="formRepositoryInput">
                <Row>
                  <Form.Control
                    type="text"
                    value={this.state.repository}
                    placeholder="Repositories regexp"
                    onChange={(v) => this.handleChange('repository', v)}
                  />
                </Row>
              </Form.Group>
              <Form.Group controlId="formBranchInput">
                <Row>
                  <Form.Control
                    type="text"
                    value={this.state.branch}
                    placeholder="Branch regexp"
                    onChange={(v) => this.handleChange('branch', v)}
                  />
                </Row>
              </Form.Group>
              <Form.Group controlId="formFilesInput">
                <Row>
                  <Form.Control
                    type="text"
                    value={this.state.files}
                    placeholder="Files regexp"
                    onChange={(v) => this.handleChange('files', v)}
                  />
                </Row>
              </Form.Group>
              {this.props.showChangeParams ? (
                <Form.Group controlId="changeStateInput">
                  <DropDownButton
                    title={
                      this.state.state
                        ? 'Change state: ' + this.state.state
                        : 'Change state: ALL'
                    }
                    variant="secondary"
                  >
                    {[
                      ['', 'All'],
                      ['OPEN', 'Open'],
                      ['CLOSED', 'Closed'],
                      ['MERGED', 'Merged'],
                      ['SELF-MERGED', 'Self-merged']
                    ].map((entry) => {
                      return (
                        <Dropdown.Item
                          key={entry[0]}
                          value={entry[0]}
                          active={entry[0] === this.state.state}
                          onClick={this.handleChangeState}
                        >
                          {entry[1]}
                        </Dropdown.Item>
                      )
                    })}
                  </DropDownButton>
                </Form.Group>
              ) : null}
              <Row>
                <Col>
                  <Form.Group controlId="formSubmit">
                    <Button
                      className="float-left btn-lg"
                      variant="primary"
                      type="submit"
                    >
                      {' '}
                      Apply
                    </Button>
                  </Form.Group>
                </Col>
                <Col>
                  {/* add clear formular */}
                  <Form.Group controlId="formSubmitClear">
                    <Button
                      className="float-right btn-lg"
                      variant="danger"
                      type="submit"
                      onClick={this.clearFormular}
                    >
                      Clear
                    </Button>
                  </Form.Group>
                </Col>
              </Row>
            </Col>
          </Form>
          <Row>
            <Col>
              <p></p>
            </Col>
          </Row>
          <Row>
            <Col>
              <p style={resumeStyle}>
                <b>{this.getFilterResume()}</b>
              </p>
            </Col>
          </Row>
        </React.Fragment>
      </Col>
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
