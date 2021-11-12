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
import { Line } from 'react-chartjs-2'
import { hasSmallWidth } from './common'

class ChangesLifeCycleHisto extends React.Component {
  prepareDataSet(histos) {
    const createdColor = '135,255,149'
    const updatedColor = '153,102,102'
    const mergedColor = '169,135,255'
    const abandonedColor = '92,92,92'
    const eventNameMapping = {
      ChangeCreatedEvent: {
        label: 'Created',
        pointBorderColor: 'rgba(' + createdColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + createdColor + ',0.4)',
        borderColor: 'rgba(' + createdColor + ',1)'
      },
      ChangeUpdatedEvent: {
        label: 'Updated',
        pointBorderColor: 'rgba(' + updatedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + updatedColor + ',0.4)',
        borderColor: 'rgba(' + updatedColor + ',1)'
      },
      ChangeMergedEvent: {
        label: 'Merged',
        pointBorderColor: 'rgba(' + mergedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + mergedColor + ',0.4)',
        borderColor: 'rgba(' + mergedColor + ',1)'
      },
      ChangeAbandonedEvent: {
        label: 'Abandoned',
        pointBorderColor: 'rgba(' + abandonedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + abandonedColor + ',0.4)',
        borderColor: 'rgba(' + abandonedColor + ',1)'
      }
    }

    const _histos = Object.entries(histos)
    const data = {
      labels: histos.ChangeCreatedEvent.map((x) => x.date),
      datasets: []
    }
    _histos.forEach((histo) => {
      data.datasets.push({
        label: eventNameMapping[histo[0]].label,
        data: histo[1].map((x) => x.count),
        lineTension: 0.5,
        pointBorderColor: eventNameMapping[histo[0]].pointBorderColor,
        pointBackgroundColor: eventNameMapping[histo[0]].pointBackgroundColor,
        backgroundColor: eventNameMapping[histo[0]].backgroundColor,
        borderColor: eventNameMapping[histo[0]].borderColor
      })
    })
    return data
  }

  render() {
    const data = this.prepareDataSet({
      ChangeCreatedEvent: this.props.histos.created,
      ChangeUpdatedEvent: this.props.histos.updated,
      ChangeMergedEvent: this.props.histos.merged,
      ChangeAbandonedEvent: this.props.histos.abandoned
    })
    return (
      <Line
        data={data}
        width={100}
        // on small screen the legend takes the whole height so detect and adjust
        height={hasSmallWidth() ? 90 : 68}
        options={{
          legend: {
            labels: {
              boxWidth: 30
            }
          }
        }}
      />
    )
  }
}


const CChangesLifeCycleHisto = (prop) => <ChangesLifeCycleHisto histos={prop} />

export { CChangesLifeCycleHisto }

