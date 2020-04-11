const initialState = {
  filter_gte: '',
  filter_lte: '',
  filter_loaded_from_url: false,
  filter_repository: '.*',
  filter_interval: '7d',
  filter_exclude_authors: '',
  filter_authors: ''
}

const reducer = (state = initialState, action) => {
  const newState = { ...state }
  if (action.type === 'FILTER_GTE_CHANGE') {
    newState.filter_gte = action.value
  }
  if (action.type === 'FILTER_LTE_CHANGE') {
    newState.filter_lte = action.value
  }
  if (action.type === 'FILTER_PARAMS_LOADED') {
    newState.filter_loaded_from_url = action.value
  }
  if (action.type === 'FILTER_REPOSITORY_CHANGE') {
    newState.filter_repository = action.value
  }
  if (action.type === 'FILTER_INTERVAL_CHANGE') {
    newState.filter_interval = action.value
  }
  if (action.type === 'FILTER_EXCLUDE_AUTHORS_CHANGE') {
    newState.filter_exclude_authors = action.value
  }
  if (action.type === 'FILTER_AUTHORS_CHANGE') {
    newState.filter_authors = action.value
  }
  return newState
}

export default reducer
