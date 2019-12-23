import React from 'react';

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

  fetchQueryParams = () => {
    const params = new URLSearchParams(window.location.search)
    var lte = params.get('lte')
    var gte = params.get('gte')
    if (lte) {
      this.props.handleFilterLteChange(lte)
    }
    if (gte) {
      this.props.handleFilterGteChange(gte)
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
            <Form.Group controlId="formFromDate">
              <Form.Label><b>From date</b></Form.Label>
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
            <Form.Group controlId="formToDate">
              <Form.Label><b>To date</b></Form.Label>
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
            <Button variant="primary" type="submit">
              Apply filters
            </Button>
          </Form>
        </Card.Body>
      </Card >
    )
  }
}

export {
  FiltersForm
}
