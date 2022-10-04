%%raw(`
import '@patternfly/react-core/dist/styles/base.css'
import '@patternfly/react-styles/css/components/Table/table.css'
// This global import is need for having access to the REScriptRouter from injected script (HTMX)
globalThis.RescriptReactRouter = require("@rescript/react/src/RescriptReactRouter.bs.js");
import './index.css'
import 'htmx.org';
`)

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("can't find root element")
}
