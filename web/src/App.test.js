import React from 'react'
import { render } from '@testing-library/react'
import { Provider } from 'react-redux'
import { createMyStore } from './store'
import App from './App'
import { MemoryRouter } from 'react-router-dom'
import {
  toBeInTheDocument,
  toHaveClass
} from '@testing-library/jest-dom'

expect.extend({ toBeInTheDocument, toHaveClass })

test('renders Monocle link', () => {
  const store = createMyStore()
  const { getByText } = render(
    <Provider store={store}>
      <MemoryRouter initialEntries={['/index']}>
        <App />
      </MemoryRouter>
    </Provider>
  )
  const linkElement = getByText(/Monocle/i)
  expect(linkElement).toBeInTheDocument()
  expect(linkElement).toHaveClass('navbar-brand')
})
