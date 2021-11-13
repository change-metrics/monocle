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
import moment from 'moment'
import ComplexityGraph from './complexity_graph'

class DurationComplexityGraph extends ComplexityGraph {
  constructor(props) {
    super(props)
    this.state.xScaleType = 'logarithmic'
  }

  getData(x) {
    // complexicity from Change.res
    x.duration = (x.merged_at - x.created_at) / 1000
    x.complexity = x.changed_files_count + x.additions + x.deletions
    return { x: x.duration, y: x.complexity, r: 5 }
  }

  xTickToLabel(q) {
    for (const tick in q.ticks) {
      if (q.ticks[tick] !== '') {
        q.ticks[tick] = moment
          .duration(parseFloat(q.ticks[tick]), 'seconds')
          .humanize()
      }
    }
  }
}

const ChangesReviewStats = (prop) => (
  <DurationComplexityGraph data={prop.data} onClick={prop.onClick} />
)

export default ChangesReviewStats
