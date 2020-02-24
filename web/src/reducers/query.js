import { get_query_results } from '../api'

const initialState = {
  changes_events_counters_result: null,
  changes_events_counters_loading: true,
  changes_events_counters_error: {},
  changes_lifecycle_stats_result: null,
  changes_lifecycle_stats_loading: true,
  changes_lifecycle_stats_error: {},
  changes_review_stats_result: null,
  changes_review_stats_loading: true,
  changes_review_stats_error: {},
  most_active_authors_stats_result: null,
  most_active_authors_stats_loading: true,
  most_active_authors_stats_error: {},
};

const query_reducer = (state = initialState, action) => {
  const newState = { ...state };
  if (action.type.endsWith('_QUERY_LOADING')) {
    const graph_type = action.type.replace('_QUERY_LOADING', '')
    newState[graph_type + '_loading'] = true;
  }
  if (action.type.endsWith('_QUERY_SUCCESS')) {
    const graph_type = action.type.replace('_QUERY_SUCCESS', '')
    newState[graph_type + '_loading'] = false;
    newState[graph_type + '_error'] = {};
    newState[graph_type + '_result'] = action.value;
  }
  if (action.type.endsWith('_QUERY_ERROR')) {
    const graph_type = action.type.replace('_QUERY_ERROR', '')
    newState[graph_type + '_loading'] = false;
    newState[graph_type + '_error'] = action.value;
    newState[graph_type + '_result'] = null;
  }
  return newState;
}

function query(params) {
  return (dispatch) => {
    dispatch({ type: params.graph_type + '_QUERY_LOADING' });
    return get_query_results(params)
      .then(response => {
        dispatch(
          {
            type: params.graph_type + '_QUERY_SUCCESS',
            value: response.data,
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

export default query_reducer
export {
  query
}
