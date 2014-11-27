'use strict';
// ----------------------------------------------------------------------------
// Form Submission logic
// ----------------------------------------------------------------------------

window.onload = function() {
  setupCursor();
  setupListeners();
};

/**
 * Sets-up global event listeners
 */

function setupListeners() {
  document
    .getElementById('url-button')
    .addEventListener('click', handleClick);
}

/**
 * Handles an url submission
 */

function handleClick() {
  var urlEl = document.getElementById('url-input');
  var url = urlEl.value;

  submitUrl(url, function(err, id) {
    if(err) {
      updateStatus('Errored with: "' + err.message + '"');
    } else {
      updateStatus('Shortened as: ' + id);
    }
  });

  updateStatus('Shortening');
}

/**
 * Submits an URL to the URL shortening API
 *
 * @param {String} url The URL to submit
 * @param {Function<Error, String>} cb A callback function
 */

function submitUrl(url, cb) {
  var xhr = new XMLHttpRequest();
  xhr.open('post', '/urls');

  xhr.onreadystatechange = function() {
    if(xhr.readyState === 4) {
      if(xhr.status < 400) {
        return cb(null, xhr.responseText);
      } else {
        var err = new Error(xhr.responseText);
        return cb(err);
      }
    }
  };

  xhr.send(url);
}

/**
 * Updates the current URL shortening request status.
 *
 * @param {String} statusStr
 */

function updateStatus(statusStr) {
  var statusEl = document.getElementById('url-status');
  statusEl.innerHTML = statusStr;
}

// ----------------------------------------------------------------------------
// Visitor count set-up
// ----------------------------------------------------------------------------

/**
 * Sets-up the visitor counter
 */

function setupCursor() {
}
