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

import React from 'react'

import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'

import FiltersForm from './FiltersForm.bs.js'

class FiltersFormLegacy extends React.Component {
  render() {
    // This function is the bridge between pure rescript world and the js history subscribe system
    const updateFilters = (filters) => {
      console.log(`"Updating filters"`, filters)
      const current = this.props.history.location.search
      const newQs = '?' + filters
      if (current !== newQs) {
        this.props.history.push(this.props.history.location.pathname + newQs)
      }
    }
    return (
      <FiltersForm
        updateFilters={updateFilters}
        showChangeParams={this.props.showChangeParams}
      />
    )
  }
}

FiltersFormLegacy.propTypes = {
  history: PropTypes.object.isRequired,
  showChangeParams: PropTypes.bool
}

FiltersFormLegacy.defaultProps = {
  showChangeParams: false
}

export default withRouter(FiltersFormLegacy)
