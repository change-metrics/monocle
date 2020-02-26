import React from 'react';
import { Pie } from 'react-chartjs-2';

class ChangeApprovals extends React.Component {
  prepare_data_set(_data) {
    const ignored_approval = [
      'Code-Review+0',
      'Verified+0',
      'Workflow+0',
      'COMMENTED',
    ]
    const palette = {
      'Code-Review+2': '#00ff9f',
      'Code-Review+1': '#B6FCD5',
      'Code-Review-1': '#CA5462',
      'Code-Review-2': '#AB0000',
      'Workflow+1': '#00ff9f',
      'Workflow-1': '#AB0000',
      'APPROVED': '#00ff9f',
      'DISMISSED': '#AB0000',
    }
    const tops = _data.tops.filter(x => !ignored_approval.includes(x.key))
    let data = {
      labels: tops.map(x => x.key),
      datasets: [{
        label: 'Approvals',
        data: tops.map(x => x.doc_count),
        backgroundColor: tops.map(x => palette[x.key])
      }]
    }
    return data
  }
  render() {
    const data = this.prepare_data_set(this.props.data)
    return <Pie data={data} />
  }
}

export {
  ChangeApprovals,
}
