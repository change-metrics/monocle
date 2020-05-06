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

import { getQueryResults } from '../api'

const addStates = (init, name) => {
  init[name + '_result'] = null
  init[name + '_loading'] = true
  init[name + '_error'] = null
}

const initialState = {}

addStates(initialState, 'changes_lifecycle_stats')
addStates(initialState, 'changes_review_stats')
addStates(initialState, 'most_active_authors_stats')
addStates(initialState, 'approval_stats')
addStates(initialState, 'most_reviewed_authors_stats')
addStates(initialState, 'authors_peers_stats')
addStates(initialState, 'repos_top_merged')
addStates(initialState, 'repos_top_opened')
addStates(initialState, 'repos_top_abandoned')
addStates(initialState, 'hot_changes')
addStates(initialState, 'cold_changes')
addStates(initialState, 'last_changes')
addStates(initialState, 'last_opened_changes')
addStates(initialState, 'last_merged_changes')
addStates(initialState, 'last_abandoned_changes')
addStates(initialState, 'full_last_abandoned_changes')
addStates(initialState, 'changes_and_events')
addStates(initialState, 'new_contributors')
addStates(initialState, 'authors_top_merged')
addStates(initialState, 'authors_top_opened')
addStates(initialState, 'authors_top_abandoned')
addStates(initialState, 'opened_changes_by_file_map')
addStates(initialState, 'merged_changes_by_file_map')

const enhanceData = (x) => {
  if (x.type === 'Change') {
    x.complexity = x.changed_files_count + x.additions + x.deletions
    x.hasLinks = x.text && x.text.match(/https?:\/\/.*\/.*|#[0-9]+/i)
  }
}

const queryReducer = (state = initialState, action) => {
  const newState = { ...state }

  // console.log(action)

  if (action.type.endsWith('_QUERY_LOADING')) {
    const graphType = action.type.replace('_QUERY_LOADING', '')
    newState[graphType + '_loading'] = true
  }

  if (action.type.endsWith('_QUERY_SUCCESS')) {
    const graphType = action.type.replace('_QUERY_SUCCESS', '')

    // Compute and inject complexity
    switch (graphType) {
      case 'hot_changes':
      case 'cold_changes':
      case 'last_opened_changes':
      case 'last_merged_changes':
      case 'last_abandoned_changes':
      case 'full_last_abandoned_changes':
      case 'changes_and_events':
        action.value.items.forEach(enhanceData)
        break
      case 'last_changes':
        action.value.merged_changes.items.forEach(enhanceData)
        action.value.opened_changes.items.forEach(enhanceData)
        break
      default:
        break
    }
    newState[graphType + '_loading'] = false
    newState[graphType + '_error'] = null
    newState[graphType + '_result'] = action.value
  }

  if (action.type.endsWith('_QUERY_ERROR')) {
    const graphType = action.type.replace('_QUERY_ERROR', '')
    newState[graphType + '_loading'] = false
    newState[graphType + '_error'] = action.value
    newState[graphType + '_result'] = null
  }

  return newState
}

function query (params) {
  return (dispatch) => {
    dispatch({ type: params.graph_type + '_QUERY_LOADING' })
    return getQueryResults(params)
      .then(response => {
        dispatch(
          {
            type: params.graph_type + '_QUERY_SUCCESS',
            value: response.data
          }
        )
      })
      .catch(error => {
        console.log(error)
        dispatch(
          {
            type: params.graph_type + '_QUERY_ERROR',
            value: error.response
          }
        )
      })
  }
}

export default queryReducer

export {
  query
}
