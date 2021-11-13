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
import { hasSmallWidth } from './common'
import { Line } from 'react-chartjs-2'

class ChangeReviewEventsHisto extends React.Component {
  prepareDataSet(histos) {
    const eventNameMapping = {
      ChangeCommentedEvent: {
        label: 'Commented',
        pointBorderColor: 'rgba(247,242,141,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(247,242,141,0.4)',
        borderColor: 'rgba(247,242,141,1)'
      },
      ChangeReviewedEvent: {
        label: 'Reviewed',
        pointBorderColor: 'rgba(247,141,141,1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(247,141,141,0.4)',
        borderColor: 'rgba(247,141,141,1)'
      }
    }
    const _histos = Object.entries(histos)
    const data = {
      labels: histos.ChangeCommentedEvent.map((x) => x.date),
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
      ChangeCommentedEvent: this.props.data.comment_histo,
      ChangeReviewedEvent: this.props.data.review_histo
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
      />)
  }
}

const CChangeReviewEventsHisto = (prop) => <ChangeReviewEventsHisto data={prop} />

export { CChangeReviewEventsHisto }