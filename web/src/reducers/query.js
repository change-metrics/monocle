import { get_query_results } from '../api'

const initialState = {
  all_events_result: null,
  all_events_loading: true,
  all_events_error: {},
  close_events_result: null,
  close_events_loading: true,
  close_events_error: {},
  comment_events_result: null,
  comment_events_loading: true,
  comment_events_error: {},
  create_events_result: null,
  create_events_loading: true,
  create_events_error: {},
};

const query_reducer = (state = initialState, action) => {
  const newState = { ...state };
  console.log(newState)
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
