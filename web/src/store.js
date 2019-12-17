import { createStore, applyMiddleware, combineReducers } from 'redux';
import thunk from 'redux-thunk';
import QueryReducer from './reducers/query';

function createMyStore() {

  const rootReducer = combineReducers({
    QueryReducer: QueryReducer,
  })

  return createStore(
    rootReducer, applyMiddleware(thunk))
}

export {
  createMyStore
}
