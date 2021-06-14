# Monocle web interface

## Available Scripts

In the project directory, you can run:

### `npm start`

Runs the app in the development mode.<br />
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.<br />
You will also see any lint errors in the console.

You can start a local nginx proxy suitable for OAuth authentication:

- Add fqdn to /etc/hosts if necessary (e.g. `127.0.0.1 localhost changemetrics.local`)
- Create tls key if necessary (e.g. `sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /etc/ssl/private/nginx-selfsigned.key -out /etc/ssl/certs/nginx-selfsigned.crt`)
- Start local proxy: `e.g. sudo nginx -c $(pwd)/local-nginx.conf -g "daemon off;"`

Then open [https://changemetrics.local:29418](https://changemetrics.local:29418).

- Accept the certicates if necessary
- Set the OAuth callback to `https://changemetrics.local:29418/`


### `npm run test`

Launches the test runner in the interactive watch mode.<br />
See the section about [running tests](https://facebook.github.io/create-react-app/docs/running-tests) for more information.

### `npm run build`

Builds the app for production to the `build` folder.<br />
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.<br />
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

### `npm run clean`

Cleanup build artifacts

### `NODE_ENV='"designer"' npm start`

Runs the standalone application [Designer](./src/Designer.res) to help style the components without a backend.
