'use strict';

window.addEventListener('load', function() {
  const sizeLimit = 8192;

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
    const text = annotateInput.value.trimEnd();
    if (!text) {
      alert('Please enter some Tamil text to annotate.');
      return;
    }

    const blob = new Blob([text]);
    if (blob.size > sizeLimit) {
      alert('Your text is too long, try something shorter (up to 8 KiB).');
      return;
    }

    const num = ++requestNum;
    annotateButton.disabled = true;
    annotateButton.textContent = 'Loading...';

    fetch('/api/annotate', {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain; charset=utf-8',
      },
      body: blob,
    })
      .then(response => {
        if (response.ok) {
          return response.text()
            .then(text => {
              if (requestNum === num) {
                annotateHtml = text;
                display();
              }
            });
        } else {
          const status = `${response.status} ${response.statusText}`;
          alert(`Could not load annotations (${status}). Please try again later.`);
        }
      })
      .catch(error => {
        display();
        alert('Could not load annotations. Please check your network connection and try again.');
      });
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
