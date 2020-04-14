import React from 'react'
import PropTypes from 'prop-types'
import { Timeline, TimelineEvent } from 'react-event-timeline'
import moment from 'moment'

const TYPE_TO_NAME = {
  ChangeCreatedEvent: 'Change created',
  ChangeMergedEvent: 'Change merged',
  ChangeAbandonedEvent: 'Change abandoned',
  ChangeCommitPushedEvent: 'Commits pushed',
  ChangeCommitForcePushedEvent: 'Commits force pushed',
  ChangeReviewedEvent: 'Change reviewed',
  ChangeCommentedEvent: 'Change commented'
}

const TYPE_TO_COLOR = {
  ChangeCreatedEvent: 'green',
  ChangeMergedEvent: 'blue',
  ChangeAbandonedEvent: 'red',
  ChangeCommitPushedEvent: 'orange',
  ChangeCommitForcePushedEvent: 'DarkOrange',
  ChangeReviewedEvent: 'purple',
  ChangeCommentedEvent: 'purple'
}

class TimelineGraph extends React.Component {
  getTitle (event) {
    return TYPE_TO_NAME[event.type] + ' by ' + event.author
  }

  getDate (event) {
    return moment(event.created_at).fromNow()
  }

  render () {
    return <Timeline>
      {this.props.data.map((event, idx) => <TimelineEvent
        title={this.getTitle(event)}
        createdAt={this.getDate(event)}
        key={idx}
        iconColor={TYPE_TO_COLOR[event.type]}
      />
      )
      }
    </Timeline>
  }
}

TimelineGraph.propTypes = {
  data: PropTypes.array.isRequired,
  index: PropTypes.string.isRequired
}

export {
  TimelineGraph
}
