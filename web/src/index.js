import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from "react-redux";
import { createMyStore } from './store'
import './index.css';
import App from './App';
import { BrowserRouter } from 'react-router-dom';

const store = createMyStore()

ReactDOM.render(
    <Provider store={store}>
        <BrowserRouter><App /></BrowserRouter>
    </Provider>,
    document.getElementById('root')
);
