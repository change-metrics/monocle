import React from 'react';

import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Form from 'react-bootstrap/Form'
import Button from 'react-bootstrap/Button'

import DatePicker from "react-datepicker"
import "react-datepicker/dist/react-datepicker.css";

import moment from 'moment'

class FiltersForm extends React.Component {

  componentDidMount() {
    this.fetchQueryParams()
  }

  onGteChange = (date) => {
    var date_str = moment(date).format("YYYY-MM-DD")
    this.props.handleFilterGteChange(date_str)

  }
  onLteChange = (date) => {
    var date_str = moment(date).format("YYYY-MM-DD")
    this.props.handleFilterLteChange(date_str)
  }

  onRepositoryChange = (e) => {
    this.props.handleFilterRepositoryChange(e.target.value)
  }

  onIntervalChange = (e) => {
    this.props.handleFilterIntervalChange(e.target.value)
  }

  fetchQueryParams = () => {
    const params = new URLSearchParams(window.location.search)
    var lte = params.get('lte')
    var gte = params.get('gte')
    var repository = params.get('repository')
    var interval = params.get('interval')
    if (lte) {
      this.props.handleFilterLteChange(lte)
    }
    if (gte) {
      this.props.handleFilterGteChange(gte)
    }
    if (repository) {
      this.props.handleFilterRepositoryChange(repository)
    }
    if (interval) {
      this.props.handleFilterIntervalChange(interval)
    }
    this.props.setQueryParamsLoaded()
  }

  updateHistoryURL = (params) => {
    const baseurl = window.location.origin + window.location.pathname
    var urlparams = new URLSearchParams(window.location.search)
    Object.keys(params).forEach(element => {
      if (params[element]) {
        urlparams.set(element, params[element])
      }
    });
    const newurl = baseurl + '?' + urlparams.toString()
    window.history.pushState({}, null, newurl)
  }

  handleSubmit = (event) => {
    this.updateHistoryURL({
      'gte': this.props.filter_gte,
      'lte': this.props.filter_lte,
      'repository': this.props.filter_repository,
      'interval': this.props.filter_interval,
    })
    window.location.reload(true)
    event.preventDefault()
  }

  render() {
    return (
      <Card>
        <Card.Header>
          <b>Filters</b>
        </Card.Header>
        <Card.Body>
          <Form onSubmit={this.handleSubmit}>
            <Form.Row>
              <Col>
                <Form.Group controlId="formRepositoryInput">
                  <Form.Label>Repository regexp</Form.Label>
                  <Form.Control
                    type="text"
                    value={this.props.filter_repository}
                    onChange={this.onRepositoryChange}
                  />
                </Form.Group>
              </Col>
              <Col>
                <Form.Group controlId="formFromDate">
                  <Form.Label>From date</Form.Label>
                  <br />
                  <DatePicker
                    selected={
                      this.props.filter_gte ?
                        moment(this.props.filter_gte).toDate() : ""}
                    onChange={this.onGteChange}
                    dateFormat="yyyy-MM-dd"
                    placeholderText="Set a from date boundary"
                    showYearDropdown
                  />
                </Form.Group>
              </Col>
              <Col>
                <Form.Group controlId="formToDate">
                  <Form.Label>To date</Form.Label>
                  <br />
                  <DatePicker
                    selected={
                      this.props.filter_lte ?
                        moment(this.props.filter_lte).toDate() : ""}
                    onChange={this.onLteChange}
                    dateFormat="yyyy-MM-dd"
                    placeholderText="Set a to date boundary"
                    showYearDropdown
                  />
                </Form.Group>
              </Col>
              <Col>
                <Form.Group controlId="formIntervalInput">
                  <Form.Label>Interval</Form.Label>
                  <Form.Control
                    type="text"
                    value={this.props.filter_interval}
                    onChange={this.onIntervalChange}
                  />
                </Form.Group>
              </Col>
            </Form.Row>
            <Form.Row>
              <Col>
                <Button variant="primary" type="submit">
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

export {
  FiltersForm
}
