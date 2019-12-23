const initialState = {
  filter_gte: null,
  filter_lte: null,
  filter_loaded_from_url: false,
};

const reducer = (state = initialState, action) => {
  const newState = { ...state };
  if (action.type === 'FILTER_GTE_CHANGE') {
    newState.filter_gte = action.value;
  }
  if (action.type === 'FILTER_LTE_CHANGE') {
    newState.filter_lte = action.value;
  }
  if (action.type === 'FILTER_PARAMS_LOADED') {
    newState.filter_loaded_from_url = action.value;
  }
  return newState;
}

export default reducer
