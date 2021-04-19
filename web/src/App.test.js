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
import { render } from '@testing-library/react'
import { Provider } from 'react-redux'
import { createMyStore } from './store'
import App from './App'
import { MemoryRouter } from 'react-router-dom'
import { toBeInTheDocument, toHaveClass } from '@testing-library/jest-dom'

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
  const linkElement = getByText(/^Monocle$/i)
  expect(linkElement).toBeInTheDocument()
  expect(linkElement).toHaveClass('navbar-brand')
})
