/* @flow */

import $ from '../jquery'
import cast from '../typecast'
import {on} from 'delegated-events'
import {submit} from '../form'

let allowSubmit = false

function performHealthcheck() {
  const repoName = cast(document.getElementById('showcase_item_name_with_owner'), HTMLInputElement).value
  const submitButton = cast(document.getElementById('submit'), HTMLButtonElement)
  const hiddenForm = cast(document.getElementsByClassName('js-submit-health-check')[0], HTMLFormElement)
  const targetRepoName = cast(document.getElementById('repo_name'), HTMLInputElement)
  document.getElementById("js-health").innerHTML = "Performing health check..."
  targetRepoName.value = repoName
  submitButton.disabled = false
  allowSubmit = true
  submit(hiddenForm)
}

on('submit', '#new_showcase_item', function(e) {
  if (!allowSubmit) { e.preventDefault() }
})

$(document).on('ajaxSuccess', '.js-health', function(event, xhr, settings, data) {
  this.innerHTML = data
})

on('focusout', '#showcase_item_name_with_owner', function() {
  performHealthcheck()
})

on('focusin', '#showcase_item_body', function() {
  if (cast(document.getElementById('showcase_item_name_with_owner'), HTMLInputElement).type === 'hidden') {
    performHealthcheck()
  }
})
