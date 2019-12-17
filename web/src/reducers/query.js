import { get_query_results } from '../api'

const initialState = {
  result: null,
  loading: true,
  error: {}
};

const query_reducer = (state = initialState, action) => {
  const newState = { ...state };
  if (action.type === 'QUERY_LOADING') {
    newState.loading = true;
  }
  if (action.type === 'QUERY_SUCCESS') {
    newState.result = action.value;
    newState.error = {}
    newState.loading = false;
  }
  if (action.type === 'QUERY_ERROR') {
    newState.result = null;
    newState.error = action.value;
    newState.loading = false;
  }
  return newState;
}

function query(params) {
  return (dispatch) => {
    dispatch({ type: 'QUERY_LOADING' });
    return get_query_results(params)
      .then(response => {
        dispatch(
          {
            type: 'QUERY_SUCCESS',
            value: response.data,
          }
        )
      })
      .catch(error => {
        dispatch(
          {
            type: 'QUERY_ERROR',
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
