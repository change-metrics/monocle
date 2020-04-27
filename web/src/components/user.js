import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { loggedUser } from '../reducers/user'
import { withRouter, Link } from 'react-router-dom'

var server = process.env.REACT_APP_API_URL || 'http://localhost:9876'

class LoginView extends React.Component {
  render () {
    return (
      <a href={server + '/api/0/login'}>Login to Github</a>
    )
  }
}

const UserViewMapStateToProps = state => {
  return {
    logged_user_loading: state.LoggedUserReducer.logged_user_loading,
    logged_user_result: state.LoggedUserReducer.logged_user_result,
    logged_user_error: state.LoggedUserReducer.logged_user_error
  }
}

const UserViewDispatchToProps = dispatch => {
  return {
    getLoggedUser: () => dispatch(loggedUser())
  }
}

class UserView extends React.Component {
  componentDidMount () {
    this.props.getLoggedUser()
  }

  render () {
    if (!this.props.logged_user_loading && this.props.logged_user_result) {
      return (
        <Link className="nav-link" to="/">Hello {this.props.logged_user_result}</Link>
      )
    } else {
      return ''
    }
  }
}

UserView.propTypes = {
  logged_user_loading: PropTypes.bool,
  logged_user_result: PropTypes.string,
  logged_user_error: PropTypes.string,
  getLoggedUser: PropTypes.func
}

const CUserView = withRouter(
  connect(UserViewMapStateToProps, UserViewDispatchToProps)(UserView))

export {
  CUserView,
  LoginView
}
