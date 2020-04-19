import React from 'react'

import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'
import PropTypes from 'prop-types'

import DatePicker from 'react-datepicker'
import 'react-datepicker/dist/react-datepicker.css'

import moment from 'moment'

class DateFormBox extends React.Component {
  render () {
    return (
      <React.Fragment>
        <Col>
          <Form.Group controlId='formFromDate'>
            <Form.Label>From date</Form.Label>
            <br />
            <DatePicker
              selected={
                this.props.gte
                  ? moment(this.props.gte).toDate() : ''}
              onChange={v => this.props.handleChange('gte', v)}
              dateFormat='yyyy-MM-dd'
              placeholderText='Set a from date boundary'
              showYearDropdown
            />
          </Form.Group>
        </Col>
        <Col>
          <Form.Group controlId='formToDate'>
            <Form.Label>To date</Form.Label>
            <br />
            <DatePicker
              selected={
                this.props.lte
                  ? moment(this.props.lte).toDate() : ''}
              onChange={v => this.props.handleChange('lte', v)}
              dateFormat='yyyy-MM-dd'
              placeholderText='Set a to date boundary'
              showYearDropdown
            />
          </Form.Group>
        </Col>
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
      exclude_authors: '',
      authors: ''
    }
  }

  componentDidMount () {
    this.fetchQueryParams()
  }

  handleChange = (key, e) => {
    let val = null
    if (e !== null) {
      val = e.target ? e.target.value : e
    }
    const assoc = {}
    if (key === 'gte' || key === 'lte') {
      val = moment(val).format('YYYY-MM-DD')
      if (val === 'Invalid date') {
        val = ''
      }
    }
    assoc[key] = val
    this.setState(assoc)
  }

  fetchQueryParams = () => {
    const params = new URLSearchParams(window.location.search)
    this.setState({
      lte: params.get('lte') || '',
      gte: params.get('gte') || '',
      repository: params.get('repository') || '',
      excludeAuthors: params.get('exclude_authors') || '',
      authors: params.get('authors') || ''
    })
  }

  updateHistoryURL = (params) => {
    const baseurl = `/r${window.location.pathname}`
    const urlparams = new URLSearchParams(window.location.search)
    Object.keys(params).forEach(element => {
      if (params[element]) {
        urlparams.set(element, params[element])
      } else {
        urlparams.delete(element)
      }
    })
    const newsearch = urlparams.toString()
    // do not push a new url if the search didn't change
    if (`?${newsearch}` !== window.location.search) {
      const newurl = newsearch === '' ? baseurl : baseurl + '?' + newsearch
      this.props.history.push(newurl)
    }
  }

  handleSubmit = (event) => {
    this.updateHistoryURL({
      gte: this.state.gte,
      lte: this.state.lte,
      repository: this.state.repository,
      exclude_authors: this.state.exclude_authors,
      authors: this.state.authors
    })
    event.preventDefault()
  }

  render () {
    return (
      <Card>
        <Card.Header>
          <b>Filters</b>
        </Card.Header>
        <Card.Body>
          <Form onSubmit={this.handleSubmit}>
            <Form.Row>
              <Col>
                <Form.Group controlId='formRepositoryInput'>
                  <Form.Label>Repository regexp</Form.Label>
                  <Form.Control
                    type='text'
                    value={this.state.repository}
                    onChange={v => this.handleChange('repository', v)}
                  />
                </Form.Group>
              </Col>
              <DateFormBox
                gte={this.state.gte}
                lte={this.state.lte}
                handleChange={this.handleChange}
              />
              <Col>
                <Form.Group controlId='formExcludeAuthorsInput'>
                  <Form.Label>Exclude Authors</Form.Label>
                  <Form.Control
                    type='text'
                    value={this.state.exclude_authors}
                    onChange={v => this.handleChange('exclude_authors', v)}
                  />
                </Form.Group>
              </Col>
              <Col>
                <Form.Group controlId='formAuthorsInput'>
                  <Form.Label>Authors</Form.Label>
                  <Form.Control
                    type='text'
                    value={this.state.authors}
                    onChange={v => this.handleChange('authors', v)}
                  />
                </Form.Group>
              </Col>
            </Form.Row>
            <Form.Row>
              <Col>
                <Button variant='primary' type='submit'>
                  Apply filters
                </Button>
              </Col>
            </Form.Row>
          </Form>
        </Card.Body>
      </Card >
    )
  }
}

FiltersForm.propTypes = {
  history: PropTypes.object.isRequired
}

const CFiltersForm = FiltersForm

export {
  CFiltersForm
}
