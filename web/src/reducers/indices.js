// Monocle.
// Copyright (C) 2020 Monocle authors

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

import { getIndices } from '../api'

const initialState = {
  indices: null,
  error: undefined
}

const queryReducer = (state = initialState, action) => {
  const newState = { ...state }

  if (action.type === 'indices_loading') {
    newState.indices = null
  } else if (action.type === 'indices') {
    newState.indices = action.value
  } else if (action.type === 'indices_error') {
    newState.error = action.value
    newState.indices = []
  }

  return newState
}

function query() {
  return (dispatch) => {
    dispatch({ type: 'indices_loading' })
    return getIndices()
      .then((response) => {
        dispatch({
          type: 'indices',
          value: response.data
        })
      })
      .catch((error) => {
        dispatch({
          type: 'indices_error',
          value: error.response
        })
      })
  }
}

export default queryReducer

export { query }
