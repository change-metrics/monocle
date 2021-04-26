// Monocle.
// Copyright (C) 2020 Monocle authors

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

class CommitsTimelineGraph extends React.Component {
  getTitle(commit) {
    return commit.title
  }

  getDate(commit) {
    return moment(commit.authored_at).fromNow() + ' by ' + commit.author.muid
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

CommitsTimelineGraph.propTypes = {
  data: PropTypes.array.isRequired
}

export default CommitsTimelineGraph
