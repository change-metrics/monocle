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
import { Redirect } from 'react-router-dom'
import PropTypes from 'prop-types'

import { connect } from 'react-redux'
import { query } from './reducers/indices'

import {
  ErrorBox,
  LoadingBox
} from './components/common'

class TopView extends React.Component {
  componentDidMount () {
    this.props.handleQuery()
  }

  render () {
    if (!this.props.indices) {
      return <LoadingBox />
    }

    if (this.props.indices.length === 0) {
      return <ErrorBox error={{ status: 0, data: 'Please create an index.' }}/>
    }

    const index = this.props.indices[0]
    return (
      <Redirect to={`/${index}`} />
    )
  }
}

TopView.propTypes = {
  handleQuery: PropTypes.func,
  indices: PropTypes.array
}

const mapStateToProps = state => {
  return {
    indices: state.IndicesReducer.indices,
    error: state.IndicesReducer.indices_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: () => dispatch(query())
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(TopView)
