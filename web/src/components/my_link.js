import React from 'react'
import { __RouterContext as RouterContext } from 'react-router'
import PropTypes from 'prop-types'
import invariant from 'tiny-invariant'
import { createLocation } from 'history'

// React 15 compat
const forwardRefShim = C => C
let { forwardRef } = React
if (typeof forwardRef === 'undefined') {
  forwardRef = forwardRefShim
}

const __DEV__ = true

export const resolveToLocation = (to, currentLocation) =>
  typeof to === 'function' ? to(currentLocation) : to

export const normalizeToLocation = (to, currentLocation) => {
  return typeof to === 'string'
    ? createLocation(to, null, null, currentLocation)
    : to
}

function isModifiedEvent (event) {
  return !!(event.metaKey || event.altKey || event.ctrlKey || event.shiftKey)
}

const MyLinkAnchor = forwardRef(
  (
    {
      innerRef, // TODO: deprecate
      navigate,
      onClick,
      ...rest
    },
    forwardedRef
  ) => {
    const { target } = rest

    const props = {
      ...rest,
      onClick: event => {
        try {
          if (onClick) onClick(event)
        } catch (ex) {
          event.preventDefault()
          throw ex
        }

        if (
          !event.defaultPrevented && // onClick prevented default
          event.button === 0 && // ignore everything but left clicks
          (!target || target === '_self') && // let browser handle "target=_blank" etc.
          !isModifiedEvent(event) // ignore clicks with modifier keys
        ) {
          event.preventDefault()
          navigate()
        }
      }
    }

    // React 15 compat
    if (forwardRefShim !== forwardRef) {
      props.ref = forwardedRef || innerRef
    } else {
      props.ref = innerRef
    }

    return <a {...props} />
  }
)

if (__DEV__) {
  MyLinkAnchor.displayName = 'MyLinkAnchor'

  const refType = PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.func,
    PropTypes.shape({ current: PropTypes.any })
  ])

  MyLinkAnchor.propTypes = {
    innerRef: refType,
    onClick: PropTypes.func,
    navigate: PropTypes.func
  }
}

/**
 * The public API for rendering a history-aware <a>.
 */
const MyLink = forwardRef(
  (
    {
      component = MyLinkAnchor,
      replace,
      to,
      innerRef, // TODO: deprecate
      ...rest
    },
    forwardedRef
  ) => {
    return (
      <RouterContext.Consumer>
        {context => {
          invariant(context, 'You should not use <MyLink> outside a <Router>')

          const { history } = context

          const location = normalizeToLocation(
            resolveToLocation(to, context.location),
            context.location
          )

          const href = location ? history.createHref(location) : ''
          const props = {
            ...rest,
            href,
            navigate () {
              const location = resolveToLocation(to, context.location)
              const method = replace ? history.replace : history.push
              const current = new URL(window.location.href)
              current.pathname = `r${current.pathname}`
              window.history.replaceState(window.history.state, '', current.href)
              method(location)
            }
          }

          // React 15 compat
          if (forwardRefShim !== forwardRef) {
            props.ref = forwardedRef || innerRef
          } else {
            props.innerRef = innerRef
          }

          return React.createElement(component, props)
        }}
      </RouterContext.Consumer>
    )
  }
)

if (__DEV__) {
  MyLink.displayName = 'MyLink'

  const toType = PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.object,
    PropTypes.func
  ])
  const refType = PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.func,
    PropTypes.shape({ current: PropTypes.any })
  ])

  MyLink.propTypes = {
    innerRef: refType,
    onClick: PropTypes.func,
    replace: PropTypes.bool,
    target: PropTypes.string,
    component: PropTypes.element,
    to: toType.isRequired
  }
}

export default MyLink
