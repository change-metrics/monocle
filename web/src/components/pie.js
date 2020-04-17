import React from 'react'

import PropTypes from 'prop-types'

import {
  ErrorBox
} from './common'

import { Pie as BasePie } from 'react-chartjs-2'

class Pie extends React.Component {
  prepareDataSet (obj) {
    const labels = obj.items.map(x => x.key)
    const data = obj.items.map(x => x.doc_count)
    const sum = data.reduce((a, b) => a + b, 0)
    if (sum < obj.total_hits) {
      labels.push('Others')
      data.push(obj.total_hits - sum)
    }
    let palette
    if (this.props.palette) {
      palette = labels.map(l => this.props.palette[l])
    } else {
      palette = ['#247ba0', '#70c1b3', '#b2dbbf', '#f3ffbd', '#ff1654',
        '#247ba0', '#70c1b3', '#b2dbbf', '#f3ffbd', '#ff1654', '#b2dbbf']
    }
    const pieData = {
      labels: labels,
      datasets: [{
        data: data,
        backgroundColor: palette
      }]
    }
    return pieData
  }

  render () {
    const data = this.prepareDataSet(this.props.data)
    if (!data) {
      return <ErrorBox
        error="No data for Pie"
      />
    }
    return <BasePie data={data} />
  }
}

Pie.propTypes = {
  data: PropTypes.shape({
    items: PropTypes.array.isRequired,
    total_hits: PropTypes.number.isRequired
  }),
  filteredItems: PropTypes.array,
  palette: PropTypes.object
}

export default Pie
