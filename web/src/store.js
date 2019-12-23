import { createStore, applyMiddleware, combineReducers } from 'redux';
import thunk from 'redux-thunk';
import QueryReducer from './reducers/query';
import FiltersReducer from './reducers/filters';

function createMyStore() {

  const rootReducer = combineReducers({
    QueryReducer: QueryReducer,
    FiltersReducer: FiltersReducer,
  })

  return createStore(
    rootReducer, applyMiddleware(thunk))
}

export {
  createMyStore
}
