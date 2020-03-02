import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'


class TopEventsTable extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>ID</th>
                    <th>count</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>{index}</td>
                      <td>{x.key}</td>
                      <td>{x.doc_count}</td>
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

class TopStrengthsTable extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>Peers</th>
                    <th>Strength</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>{index}</td>
                      <td>{x[0][0]} and {x[0][1]}</td>
                      <td>{x[1]}</td>
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

class HotChangesTable extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    <th>hot score</th>
                    <th>id</th>
                    <th>author</th>
                    <th>created/updated</th>
                    <th>title</th>
                    <th>mergeable</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>{x.hot_score}</td>
                      <td>{x.repository_fullname_and_number}</td>
                      <td>{x.author}</td>
                      <td>
                        <div>{x.created_at}</div>
                        <div>{x.updated_at}</div>
                      </td>
                      <td>{x.title}</td>
                      <td>{x.mergeable}</td>
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

class ColdChangesTable extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    <th>created/updated</th>
                    <th>id</th>
                    <th>author</th>
                    <th>title</th>
                    <th>mergeable</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>
                        <div>{x.created_at}</div>
                        <div>{x.updated_at}</div>
                      </td>
                      <td>{x.repository_fullname_and_number}</td>
                      <td>{x.author}</td>
                      <td>{x.title}</td>
                      <td>{x.mergeable}</td>
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

export {
  TopEventsTable,
  TopStrengthsTable,
  HotChangesTable,
  ColdChangesTable,
}
