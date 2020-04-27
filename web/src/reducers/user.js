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

import { getLoggedUser } from '../api'

const addStates = (init, name) => {
  init[name + '_result'] = null
  init[name + '_loading'] = true
  init[name + '_error'] = null
}

const initialState = {}

addStates(initialState, 'logged_user')

const LoggedUserReducer = (state = initialState, action) => {
  const newState = { ...state }

  // console.log(action)

  if (action.type === 'LOGGED_USER_LOADING') {
    newState.logged_user_loading = true
  }

  if (action.type === 'LOGGED_USER_SUCCESS') {
    newState.logged_user_result = action.value
    newState.logged_user_loading = false
  }

  if (action.type === 'LOGGED_USER_ERROR') {
    newState.logged_user_error = true
    newState.logged_user_loading = false
    newState.logged_user_result = null
  }

  console.log(newState)
  return newState
}

function loggedUser () {
  // console.log(params)
  return (dispatch) => {
    dispatch({ type: 'LOGGED_USER_LOADING' })
    return getLoggedUser()
      .then(response => {
        dispatch(
          {
            type: 'LOGGED_USER_SUCCESS',
            value: response.data
          }
        )
      })
      .catch(error => {
        dispatch(
          {
            type: 'LOGGED_USER_ERROR',
            value: error.response
          }
        )
      })
  }
}

export default LoggedUserReducer

export {
  loggedUser
}
