'use strict';

window.addEventListener('load', function() {
  const annotateInput = document.getElementById('annotate-input');
  const annotateResult = document.getElementById('annotate-result');
  const annotateButton = document.getElementById('annotate-button');

  var annotateHtml = null;
  var requestNum = 0;

  function display() {
    if (annotateHtml === null) {
      annotateInput.style.display = 'block';

      annotateResult.style.display = 'none';
      annotateResult.innerHTML = '';

      annotateButton.disabled = false;
      annotateButton.textContent = 'Annotate';
    } else {
      annotateInput.style.display = 'none';

      annotateResult.innerHTML = annotateHtml;
      annotateResult.style.display = 'block';

      annotateButton.disabled = false;
      annotateButton.textContent = 'Edit';
    }
  }

  function startRequest() {
    const num = ++requestNum;
    annotateButton.disabled = true;

    fetch('/api/annotate/html', {
      method: 'POST',
      body: annotateInput.value,
    })
      .then(response => response.text())
      .then(text => {
        if (requestNum === num) {
          annotateHtml = text;
          display();
        }
      })
      .catch(error => alert('Error: ' + error));
  }

  annotateButton.onclick = function() {
    if (annotateHtml === null) {
      startRequest();
    } else {
      annotateHtml = null;
      display();
    }
  };
});
