/* @flow */

import cast from '../typecast'
import {changeValue} from '../form'
import {fetchSafeDocumentFragment} from '../fetch'
import {observe} from '../observe'

function performHealthCheck(container, repoName) {
  const formCheck = cast(document.querySelector('.js-repo-health-check'), HTMLFormElement)
  const nameToCheck = cast(formCheck.querySelector('.js-repo-health-name'), HTMLInputElement)
  nameToCheck.value = repoName

  const completedIndicator = cast(container.querySelector('.js-repo-health-check-completed'), HTMLInputElement)
  changeValue(completedIndicator, '')
  container.classList.remove('d-none')
  container.classList.add('is-loading')

  return fetchSafeDocumentFragment(document, formCheck.action, {
    method: 'POST',
    body: new FormData(formCheck),
  }).then(html => {
    const results = cast(container.querySelector('.js-repo-health-results'), HTMLElement)
    results.innerHTML = ''
    results.appendChild(html)

    container.classList.remove('is-loading')
    changeValue(completedIndicator, '1')
  })
}

observe('.js-repo-health', function(container: HTMLElement) {
  const form = cast(container.closest('form'), HTMLFormElement)
  const repoInput = cast(form.querySelector('.js-repo-name'), HTMLInputElement)

  if (repoInput.type === 'hidden') {
    const description = cast(form.querySelector('.js-comment-field'), HTMLTextAreaElement)
    description.addEventListener('focus', () => {
      performHealthCheck(container, repoInput.value)
    })
  } else {
    repoInput.addEventListener('change', () => {
      performHealthCheck(container, repoInput.value)
    })
  }
})
