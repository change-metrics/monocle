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

import { createStore, applyMiddleware, combineReducers } from 'redux'
import thunk from 'redux-thunk'
import QueryReducer from './reducers/query'
import LoggedUserReducer from './reducers/user'

function createMyStore() {
  const rootReducer = combineReducers({
    QueryReducer: QueryReducer,
    LoggedUserReducer: LoggedUserReducer
  })

  return createStore(rootReducer, applyMiddleware(thunk))
}

export { createMyStore }
