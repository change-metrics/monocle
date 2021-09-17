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
import ListGroup from 'react-bootstrap/ListGroup'
import PropTypes from 'prop-types'

import { Line } from 'react-chartjs-2'

import {hasSmallWidth} from './common'

class AuthorsHisto extends React.Component {
  prepareDataSet(data) {
    const createdColor = '135,255,149'
    const reviewedColor = '153,102,102'
    const commentedColor = '169,135,255'
    const eventNameMapping = {
      change_histo: {
        label: 'Changes authors',
        pointBorderColor: 'rgba(' + createdColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + createdColor + ',0.4)',
        borderColor: 'rgba(' + createdColor + ',1)'
      },
      review_histo: {
        label: 'Reviews authors',
        pointBorderColor: 'rgba(' + reviewedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + reviewedColor + ',0.4)',
        borderColor: 'rgba(' + reviewedColor + ',1)'
      },
      comment_histo: {
        label: 'Comments authors',
        pointBorderColor: 'rgba(' + commentedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + commentedColor + ',0.4)',
        borderColor: 'rgba(' + commentedColor + ',1)'
      }
    }

    const metaData = Object.entries(eventNameMapping)
    const ret = {
      labels: data.change_histo.map((x) => x.date),
      datasets: []
    }
    metaData.forEach((desc) => {
      ret.datasets.push({
        label: desc[1].label,
        data: data[desc[0]].map((x) => x.count),
        lineTension: 0.5,
        pointBorderColor: desc[1].pointBorderColor,
        pointBackgroundColor: desc[1].pointBackgroundColor,
        backgroundColor: desc[1].backgroundColor,
        borderColor: desc[1].borderColor
      })
    })
    return ret
  }

  render() {
    const data = this.prepareDataSet(this.props.data)
    return (
      <Row>
        <Col md={3}>
          <ListGroup>
            <ListGroup.Item>
              Change authors:{' '}
              {this.props.data.change_authors}
            </ListGroup.Item>
            <ListGroup.Item>
              Review authors:{' '}
              {this.props.data.review_authors}
            </ListGroup.Item>
            <ListGroup.Item>
              Comment authors:{' '}
              {this.props.data.comment_authors}
            </ListGroup.Item>
          </ListGroup>
        </Col>
        <Col md={9}>
          <Card>
            <Card.Body>
                <Line
                  data={data}
                  width={50}
                  // On small screen the legend takes the whole height so detect and adjust
                  height={hasSmallWidth() ? 90 : 30}
                  options={{
                    legend: {
                      labels: {
                        boxWidth: 30
                      }
                    }
                  }}
                />
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

AuthorsHisto.propTypes = {
  data: PropTypes.shape({
    change_histo: PropTypes.any,
    comment_histo: PropTypes.any,
    review_histo: PropTypes.any,
    change_authors: PropTypes.any,
    comment_authors: PropTypes.any,
    review_authors: PropTypes.any,
  })
}

const CAuthorsHistoStats = (prop) => <AuthorsHisto data={prop} />

export { CAuthorsHistoStats }
