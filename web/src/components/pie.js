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

import {
  addUrlField,
  ErrorBox,
  hasSmallWidth
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

  handleClick (obj, elems) {
    if (obj.props.field && obj.props.history && elems && elems.length > 0 && elems[0]._index < obj.props.data.items.length) {
      const key = obj.props.data.items[elems[0]._index].key
      obj.props.history.push(addUrlField(obj.props.field, key))
    }
  }

  render () {
    const data = this.prepareDataSet(this.props.data)
    if (!data) {
      return <ErrorBox
        error="No data for Pie"
      />
    }
    return <BasePie
      getElementsAtEvent={elems => this.handleClick(this, elems)}
      // on small screen the legend takes the whole height so detect and adjust
      height={(hasSmallWidth()) ? 300 : 200}
      options={
        {
          legend: {
            labels: {
              boxWidth: 30
            }
          }
        }
      }
      data={data} />
  }
}

Pie.propTypes = {
  history: PropTypes.object,
  field: PropTypes.string,
  data: PropTypes.shape({
    items: PropTypes.array.isRequired,
    total_hits: PropTypes.number.isRequired
  }),
  filteredItems: PropTypes.array,
  palette: PropTypes.object
}

export default Pie
