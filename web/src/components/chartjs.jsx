// Monocle.
// Copyright (C) 2019-2021 Monocle authors

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
import { Line, Bubble, Pie } from 'react-chartjs-2'
import { Timeline, TimelineEvent } from 'react-event-timeline'
import ChordDiagram from 'react-chord-diagram'
import moment from 'moment'
import Interweave from "interweave";
import { UrlMatcher } from "interweave-autolink";

function getWindowDimensions() {
  const { innerWidth: width, innerHeight: height } = window;
  return {
    width,
    height,
  };
}

function hasSmallWidth() {
  const { width } = getWindowDimensions();

  return width <= 500;
}

class InterweaveContent extends React.Component {
  render() {
    return (<Interweave
      content={this.props.content}
      disableLineBreaks={false}
      matchers={[new UrlMatcher("url")]}
    />)
  }
}

class SingleLineHisto extends React.Component {
  prepareData(histo, label, isDuration) {
    var display_label
    var display_data
    if (isDuration) {
      // Add annotation
      display_label = label + " (in days)"
      // Duration is a value in second (here ensure days to ease readability)
      display_data = histo.map(bucket => bucket.count / (24 * 3600))
    } else {
      display_label = label
      display_data = histo.map(bucket => bucket.count)
    }
    const data = {
      labels: histo.map(bucket => bucket.date),
      datasets: [
        {
          label: display_label,
          data: display_data,
          lineTension: 0.5,
          pointBorderColor: 'rgba(247,242,141,1)',
          pointBackgroundColor: '#fff',
          backgroundColor: 'rgba(247,242,141,0.4)',
          borderColor: 'rgba(247,242,141,1)'

        }
      ]
    }
    return data
  }

  render() {
    const data = this.prepareData(this.props.data.histo, this.props.data.label, this.props.data.isDuration)
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
      />)
  }
}

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

class DurationComplexityGraph extends ComplexityGraph {
  constructor(props) {
    super(props)
    this.state.xScaleType = 'logarithmic'
  }

  getData(x) {
    // complexity from Change.res
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

class ConnectionDiagram extends React.Component {
  prepareData(data) {
    const labels = []
    const assoc = {}
    const strippedLabels = []
    // Extract the labels and create a hash table for each pair
    data.forEach((elt) => {
      if (!labels.includes(elt.a1)) {
        labels.push(elt.a1)
      }
      if (!labels.includes(elt.a2)) {
        labels.push(elt.a2)
      }
      assoc[[elt.a1, elt.a2].sort()] = elt.s
    })
    // Build the matrix from the labels and the hash table
    const matrix = []
    let line
    labels.forEach((a) => {
      line = []
      labels.forEach((b) => {
        if (a === b) {
          line.push(0)
        } else {
          const key = [a, b].sort()
          if (key in assoc) {
            line.push(assoc[key])
          } else {
            line.push(0)
          }
        }
      })
      matrix.push(line)
    })
    labels.forEach((label) => {
      strippedLabels.push(label.substring(0, 12) + '...')
    })
    return { matrix: matrix, labels: strippedLabels }
  }

  render() {
    const data = this.prepareData(this.props.data)
    const graphStyle = {
      font: '50% sans-serif'
    }
    return (
      <ChordDiagram
        matrix={data.matrix}
        componentId={1}
        groupLabels={data.labels}
        groupColors={[
          '#003f5c',
          '#374c80',
          '#7a5195',
          '#bc5090',
          '#ef5675',
          '#ff764a',
          '#ffa600'
        ]}
        outerRadius={200}
        innerRadius={170}
        style={graphStyle}
        resizeWithWindow={true}
      />
    )
  }
}

class PieChart extends React.Component {
  constructor(props) {
    super(props)
    this.maxSize = props.maxSize || 7
    this.other_label = props.other_label || 'Others'
  }

  prepareDataSet(obj) {
    const labels = obj.items.map((x) => x.key).slice(0, this.maxSize)
    const data = obj.items.map((x) => x.doc_count).slice(0, this.maxSize)
    const sum = data.reduce((a, b) => a + b, 0)
    if (sum < obj.total_hits) {
      labels.push(this.other_label)
      data.push(obj.total_hits - sum)
    }
    let palette
    if (this.props.namedPalette) {
      palette = labels.map((l, i) => this.props.namedPalette[l] || this.props.palette[i])
    } else {
      palette = this.props.palette
    }
    const pieData = {
      labels: labels,
      datasets: [
        {
          data: data,
          backgroundColor: palette
        }
      ]
    }
    return pieData
  }

  handleClick(obj, elems) {
    if (
      elems &&
      elems.length > 0 &&
      elems[0]._index < obj.props.data.items.length
    ) {
      const key = obj.props.data.items[elems[0]._index].key
      obj.props.handleClick(key)
    }
  }

  render() {
    const data = this.prepareDataSet(this.props.data)
    return (
      <Pie
        getElementsAtEvent={(elems) => this.handleClick(this, elems)}
        // on small screen the legend takes the whole height so detect and adjust
        height={hasSmallWidth() ? 300 : 200}
        options={{
          legend: {
            display: false
          }
        }}
        data={data}
      />
    )
  }
}

class TimelineGraph extends React.Component {
  TYPE_TO_NAME = {
    ChangeCreatedEvent: 'Change created',
    ChangeMergedEvent: 'Change merged',
    ChangeAbandonedEvent: 'Change abandoned',
    ChangeCommitPushedEvent: 'Commits pushed',
    ChangeCommitForcePushedEvent: 'Commits force pushed',
    ChangeReviewedEvent: 'Change reviewed',
    ChangeCommentedEvent: 'Change commented'
  }

  TYPE_TO_COLOR = {
    ChangeCreatedEvent: 'green',
    ChangeMergedEvent: 'blue',
    ChangeAbandonedEvent: 'red',
    ChangeCommitPushedEvent: 'orange',
    ChangeCommitForcePushedEvent: 'DarkOrange',
    ChangeReviewedEvent: 'purple',
    ChangeCommentedEvent: 'purple'
  }
  getTitle(event) {
    return this.TYPE_TO_NAME[event.type_] + ' by ' + event.author
  }

  getDate(event) {
    return moment(event.created_at).fromNow()
  }

  render() {
    return (
      <Timeline>
        {this.props.data.map((event, idx) => (
          <TimelineEvent
            title={this.getTitle(event)}
            createdAt={this.getDate(event)}
            key={idx}
            iconColor={this.TYPE_TO_COLOR[event.type_]}
          />
        ))}
      </Timeline>
    )
  }
}

class CommitsTimelineGraph extends React.Component {
  getTitle(commit) {
    return commit.title
  }

  getDate(commit) {
    return moment(commit.authored_at).fromNow() + ' by ' + commit.author
  }

  render() {
    return (
      <Timeline>
        {this.props.data.map((commit, idx) => (
          <TimelineEvent
            title={this.getTitle(commit)}
            createdAt={this.getDate(commit)}
            key={idx}
            iconColor="green"
          />
        ))}
      </Timeline>
    )
  }
}

const CAuthorsHistoStats = (prop) => <AuthorsHisto data={prop} />
const CChangeReviewEventsHisto = (prop) => <ChangeReviewEventsHisto data={prop} />
const CChangesLifeCycleHisto = (prop) => <ChangesLifeCycleHisto histos={prop} />
const ChangesReviewStats = (prop) => (
  <DurationComplexityGraph data={prop.data} onClick={prop.onClick} />
)
const SimpleHisto = (prop) => <SingleLineHisto data={prop} />

export {
  CAuthorsHistoStats,
  CChangeReviewEventsHisto,
  CChangesLifeCycleHisto,
  ChangesReviewStats,
  SimpleHisto,
  ConnectionDiagram,
  PieChart,
  TimelineGraph,
  CommitsTimelineGraph,
  InterweaveContent
}
