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
import PropTypes from 'prop-types'
import { Bubble } from 'react-chartjs-2'
import moment from 'moment'

class ComplexityGraph extends React.Component {
  constructor(props) {
    super(props)
    this.state = { xScaleType: 'linear' }
    this.handleClick.bind(this)
  }

  tooltipLabel(item, data) {
    return data[item.index].complexity + ': ' + data[item.index].title
  }

  handleClick(item) {
    this.props.onClick(item.change_id)
  }

  getData(x) {
    return { x: moment(x.created_at).format('X'), y: x.complexity, r: 5 }
  }

  xTickToLabel(q) {
    for (const tick in q.ticks) {
      q.ticks[tick] = moment.unix(q.ticks[tick]).format('YYYY-MM-DD HH:mm')
    }
  }

  render() {
    const data = this.props.data.map((x) => this.getData(x))
    const bubbleData = {
      datasets: [
        {
          label: 'Complexity',
          fill: false,
          lineTension: 0.1,
          backgroundColor: 'rgba(75,192,192,0.4)',
          borderColor: 'rgba(75,192,192,1)',
          borderCapStyle: 'butt',
          borderDash: [],
          borderDashOffset: 0.0,
          borderJoinStyle: 'miter',
          pointBorderColor: 'rgba(75,192,192,1)',
          pointBackgroundColor: '#fff',
          pointBorderWidth: 1,
          pointHoverRadius: 5,
          pointHoverBackgroundColor: 'rgba(75,192,192,1)',
          pointHoverBorderColor: 'rgba(220,220,220,1)',
          pointHoverBorderWidth: 2,
          pointRadius: 1,
          pointHitRadius: 10,
          data: data
        }
      ]
    }

    const options = {
      tooltips: {
        callbacks: {
          label: (i, d) => this.tooltipLabel(i, this.props.data)
        }
      },
      scales: {
        xAxes: [
          {
            type: this.state.xScaleType,
            afterTickToLabelConversion: this.xTickToLabel
          }
        ],
        yAxes: [
          {
            type: 'logarithmic',
            afterTickToLabelConversion: function (q) {
              for (const tick in q.ticks) {
                if (q.ticks[tick] !== '') {
                  q.ticks[tick] = parseFloat(q.ticks[tick])
                }
              }
            }
          }
        ]
      }
    }

    return (
      <Bubble
        data={bubbleData}
        getElementsAtEvent={(elems) => {
          this.handleClick(this.props.data[elems[0]._index])
        }}
        options={options}
      />
    )
  }
}

ComplexityGraph.propTypes = {
  data: PropTypes.any,
  onClick: PropTypes.any
}

export default ComplexityGraph
