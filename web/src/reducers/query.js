import { getQueryResults } from '../api'

const initialState = {
  changes_lifecycle_stats_result: null,
  changes_lifecycle_stats_loading: true,
  changes_lifecycle_stats_error: null,
  changes_review_stats_result: null,
  changes_review_stats_loading: true,
  changes_review_stats_error: null,
  most_active_authors_stats_result: null,
  most_active_authors_stats_loading: true,
  most_active_authors_stats_error: null,
  approval_stats_result: null,
  approval_stats_loading: true,
  approval_stats_error: null,
  most_reviewed_authors_stats_result: null,
  most_reviewed_authors_stats_loading: true,
  most_reviewed_authors_stats_error: null,
  authors_peers_stats_result: null,
  authors_peers_stats_loading: true,
  authors_peers_stats_error: null,
  repos_top_merged_result: null,
  repos_top_merged_loading: true,
  repos_top_opened_error: null,
  repos_top_opened_result: null,
  repos_top_opened_loading: true,
  repos_top_merged_error: null,
  hot_changes_result: null,
  hot_changes_loading: true,
  hot_changes_error: null,
  cold_changes_result: null,
  cold_changes_loading: true,
  cold_changes_error: null,
  last_changes_result: null,
  last_changes_loading: true,
  last_changes_error: null,
  last_abandoned_changes_result: null,
  last_abandoned_changes_loading: true,
  last_abandoned_changes_error: null,
  changes_and_events_result: null,
  changes_and_events_loading: true,
  changes_and_events_error: null,
  new_contributors_result: null,
  new_contributors_loading: true,
  new_contributors_error: null,
  authors_top_merged_result: null,
  authors_top_merged_loading: true,
  authors_top_merged_error: null,
  authors_top_opened_result: null,
  authors_top_opened_loading: true,
  authors_top_opened_error: null
}

const enhanceData = (x) => {
  if (x.type === 'Change') {
    x.complexity = x.changed_files_count + x.additions + x.deletions
    x.hasTests = (x.changed_files.filter(x => x.path.match(/.*[/].*test/i)).length !== 0)
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
      case 'last_abandoned_changes':
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
