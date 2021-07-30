%%raw(`
import '@patternfly/react-core/dist/styles/base.css'
import '@patternfly/react-styles/css/components/Table/table.css'
import './index.css'
`)

// TODO: replace the remaining bootstrap with patternfly, then drop the css
%%raw(`
import 'bootstrap/dist/css/bootstrap.min.css'
`)

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("can't find root element")
}
