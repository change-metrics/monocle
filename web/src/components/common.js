import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'


function change_url(x, name = null) {
  if (!name) {
    name = x.repository_fullname_and_number;
  }
  return <a href={x.url} target="_blank" rel="noopener noreferrer">{name}</a>;
}

function add_url_field(field, value) {
  var url = new URL(window.location.href);

  url.searchParams.set(field, value);

  return url.href;
}

function new_relative_url(dest) {
  var url = new URL(window.location.href);

  url.pathname += dest;

  return url.href;
}

class LoadingBox extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <h1>
                loading
                </h1>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

class ErrorBox extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <h1>
                Error: code: {this.props.error.status},
                message: {this.props.error.data}
              </h1>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

class BaseQueryComponent extends React.Component {
  constructor(props) {
    super(props);
    this.state = {'name': null, // must be set by sub-class
                  'graph_type': null, // must be set by sub-class
                  'pageSize': 10,
                  'selectedPage': 0,
                 };
    this.handlePageChange.bind(this);
    this.queryBackend.bind(this);
  }

  componentDidUpdate(prevProps) {
    if (this.props.filter_loaded_from_url !== prevProps.filter_loaded_from_url) {
      this.queryBackend();
    }
  }

  handlePageChange(obj, pageData) {
    console.log(`handlePageChange page ${pageData.selected}`);
    obj.setState({'selectedPage': pageData.selected});
    obj.queryBackend(pageData.selected);
  }

  queryBackend(start=0) {
    // Usefull snippet
    // Object.entries(this.props).forEach(([key, val]) =>
    //   prevProps[key] !== val && console.log(`Prop '${key}' changed`)
    // );
    console.log("Query with index: " + this.props.index)
    this.props.handleQuery({
      'repository': this.props.filter_repository,
      'index': this.props.index,
      'name': this.state.name,
      'gte': this.props.filter_gte,
      'lte': this.props.filter_lte,
      'interval': this.props.filter_interval,
      'exclude_authors': this.props.filter_authors ? null : this.props.filter_exclude_authors,
      'authors': this.props.filter_authors,
      'graph_type': this.state.graph_type,
      'from': start * this.state.pageSize,
      'size': this.state.pageSize,
    })
  }
}

export {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent,
  change_url,
  add_url_field,
  new_relative_url,
}
