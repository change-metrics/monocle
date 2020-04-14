import React from 'react'
import PropTypes from 'prop-types'
import { Bubble } from 'react-chartjs-2'
import moment from 'moment'

class ComplexityGraph extends React.Component {
  constructor (props) {
    super(props)
    this.state = { xScaleType: 'linear' }
  }

  tooltipLabel (item, data) {
    return data[item.index].complexity + ': ' + data[item.index].title
  }

  handleClick (item) {
    window.location.href = '/' + this.props.index + '/change/' + item.change_id
  }

  getData (func, x) {
    return { x: moment(func(x)).format('X'), y: x.complexity, r: 5 }
  }

  xTickToLabel (q) {
    for (var tick in q.ticks) {
      q.ticks[tick] = moment.unix(q.ticks[tick]).format('YYYY-MM-DD HH:mm')
    }
  }

  render () {
    const data = this.props.data.items.map(
      x => { return this.getData(this.props.timeFunc, x) }
    )
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
          label: (i, d) => this.tooltipLabel(i, this.props.data.items)
        }
      },
      scales: {
        xAxes: [{
          type: this.state.xScaleType,
          afterTickToLabelConversion: this.xTickToLabel
        }],
        yAxes: [{
          type: 'logarithmic',
          afterTickToLabelConversion: function (q) {
            for (var tick in q.ticks) {
              if (q.ticks[tick] !== '') {
                q.ticks[tick] = parseFloat(q.ticks[tick])
              }
            }
          }
        }]
      }
    }

    return (
      <Bubble data={bubbleData}
        getElementsAtEvent={(elems) => { this.handleClick(this.props.data.items[elems[0]._index]) }}
        options={options}
      />
    )
  }
}

ComplexityGraph.propTypes = {
  data: PropTypes.shape({
    items: PropTypes.array
  }).isRequired,
  timeFunc: PropTypes.func.isRequired,
  index: PropTypes.string.isRequired
}

class DurationComplexityGraph extends ComplexityGraph {
  constructor (props) {
    super(props)
    this.state.xScaleType = 'logarithmic'
  }

  getData (func, x) {
    return { x: x.duration, y: x.complexity, r: 5 }
  }

  xTickToLabel (q) {
    for (var tick in q.ticks) {
      if (q.ticks[tick] !== '') {
        q.ticks[tick] = moment.duration(parseFloat(q.ticks[tick]), 'seconds').humanize()
      }
    }
  }
}

export {
  ComplexityGraph,
  DurationComplexityGraph
}
