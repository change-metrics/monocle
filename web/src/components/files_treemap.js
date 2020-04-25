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

import React from 'react'
import PropTypes from 'prop-types'

import { connect } from 'react-redux'

import TreeMap from 'react-d3-treemap'
// Include its styles in you build process as well
import 'react-d3-treemap/dist/react.d3.treemap.css'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addMap,
  mapDispatchToProps
} from './common'

class FilesTreeMap extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'changes_by_file_map'
  }

  getChildren (pair) {
    const res = { name: pair[0] }
    // filename, count
    if (Number.isInteger(pair[1])) {
      res.value = pair[1]
    } else {
      // directory, [assoc(filename, count)]
      const assoc = {}
      Object.entries(pair[1]).forEach(a => {
        const p = a[1]
        // path, count
        const path = p[0].split('/')
        // file
        if (path.length === 1) {
          assoc[path[0]] = p[1]
        } else {
          // directory
          const val = [path.slice(1).join('/'), p[1]]
          if (path[0] in assoc) {
            assoc[path[0]].push(val)
          } else {
            assoc[path[0]] = [val]
          }
        }
      })
      res.children = Object.entries(assoc).map(p => this.getChildren(p))
    }
    return res
  }

  prepareData (data) {
    const proj = {}
    Object.entries(data.changes).forEach((f, v) => {
      const [repo, file] = f[0].split(':', 2)
      if (repo in proj) {
        proj[repo].push([file, v])
      } else {
        proj[repo] = [[file, v]]
      }
    })
    return {
      name: 'repositories',
      children: Object.entries(proj).map(pair => this.getChildren(pair))
    }
  }

  renderData (data) {
    const graphStyle = {
      font: '50% sans-serif'
    }
    return (
      <TreeMap
        data={this.prepareData(data)}
        height={500}
        width={1100}
        hideValue={true}
        style={graphStyle}
      />
    )
  }
}

FilesTreeMap.propTypes = {
  index: PropTypes.string.isRequired,
  data: PropTypes.object
}

class OpenedFilesTreeMap extends FilesTreeMap {
  constructor (props) {
    super(props)
    this.state.graph_type = 'opened_changes_by_file_map'
    this.state.state = 'OPEN'
  }

  render () {
    if (!this.props.opened_changes_by_file_map_loading) {
      if (this.props.opened_changes_by_file_map_error || !this.props.opened_changes_by_file_map_result || !this.props.opened_changes_by_file_map_result.changes) {
        return <ErrorBox
          error={this.props.opened_changes_by_file_map_error}
        />
      }
      return this.renderData(this.props.opened_changes_by_file_map_result)
    } else {
      return <LoadingBox />
    }
  }
}

class MergedFilesTreeMap extends FilesTreeMap {
  constructor (props) {
    super(props)
    this.state.graph_type = 'merged_changes_by_file_map'
    this.state.state = 'MERGED'
  }

  render () {
    if (!this.props.merged_changes_by_file_map_loading) {
      if (this.props.merged_changes_by_file_map_error || !this.props.merged_changes_by_file_map_result || !this.props.merged_changes_by_file_map_result.changes) {
        return <ErrorBox
          error={this.props.merged_changes_by_file_map_error}
        />
      }
      return this.renderData(this.props.merged_changes_by_file_map_result)
    } else {
      return <LoadingBox />
    }
  }
}

const openedFilesTreeMapMapStateToProps = state => addMap({}, state.QueryReducer, 'opened_changes_by_file_map')
const mergedFilesTreeMapMapStateToProps = state => addMap({}, state.QueryReducer, 'merged_changes_by_file_map')

const COpenedFilesTreeMap = connect(openedFilesTreeMapMapStateToProps, mapDispatchToProps)(OpenedFilesTreeMap)
const CMergedFilesTreeMap = connect(mergedFilesTreeMapMapStateToProps, mapDispatchToProps)(MergedFilesTreeMap)

export {
  COpenedFilesTreeMap,
  CMergedFilesTreeMap
}
