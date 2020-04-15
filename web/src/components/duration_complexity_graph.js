import moment from 'moment'

import ComplexityGraph from './complexity_graph'

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

export default DurationComplexityGraph
