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
  getTitle(event) {
    return TYPE_TO_NAME[event.type] + ' by ' + event.author.muid
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
            iconColor={TYPE_TO_COLOR[event.type]}
          />
        ))}
      </Timeline>
    )
  }
}

TimelineGraph.propTypes = {
  data: PropTypes.array.isRequired
}

export default TimelineGraph
