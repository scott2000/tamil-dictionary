window.addEventListener('load', function() {
  const consonants = 'கஙசஞடணதநபமயரலவழளறனஜஷஸஹஶ';
  const delay = 300;
  const count = 6;

  const searchForm = document.getElementById('search-form');
  const searchField = document.getElementById('search-field');
  const autocomplete = document.getElementById('autocomplete');
  const cache = new Map();

  var currentQuery = '';
  var updateTimeout = null;
  var request = null;

  var focused = false;
  var composing = null;
  var results = [];

  function display() {
    if (focused && results.length) {
      autocomplete.style.display = 'block';
    } else {
      autocomplete.style.display = 'none';
    }
  }

  function setResults(newResults) {
    results = newResults;
    request = null;

    autocomplete.innerHTML = '';
    for (const result of results) {
      const div = document.createElement('div');
      div.className = 'suggestion';

      div.addEventListener('mousedown', function(event) {
        autocomplete.classList.add('autocomplete-selected');
        event.preventDefault();
        event.stopPropagation();
      });

      div.onmouseup = function() {
        window.location = result.uri;
      };

      const innerDiv = document.createElement('div');
      innerDiv.innerText = result.word;
      div.appendChild(innerDiv);

      autocomplete.appendChild(div);
    }

    display();
  }

  function generalizeQuery(query) {
    if (composing && query.endsWith(composing)) {
      var lastCharacter = query.slice(-1);
      if (consonants.indexOf(lastCharacter) !== -1) {
        return query + '\u0bcd';
      }
    }

    return query;
  }

  function startRequest(query) {
    var cached = cache.get(query);
    if (cached) {
      setResults(cached);
      return;
    }

    request = new XMLHttpRequest();
    request.open('POST', '/api/suggest', true);
    request.setRequestHeader('Content-Type', 'application/json');

    function error() {
      console.error(request.statusText);
      setResults([]);
    }

    request.onload = function() {
      if (request.readyState !== 4 || query !== currentQuery) {
        return;
      }

      if (request.status !== 200) {
        error();
        return;
      }

      var response = JSON.parse(request.responseText);
      cache.set(query, response);
      setResults(response);
    };

    request.onerror = error;
    request.send(JSON.stringify({
      count,
      query,
    }));
  }

  function setQuery(query) {
    query = generalizeQuery(query.trim());

    if (query === currentQuery) {
      return;
    }

    currentQuery = query;

    if (updateTimeout !== null) {
      clearTimeout(updateTimeout);
      updateTimeout = null;
    }

    if (request !== null) {
      request.abort();
      request = null;
    }

    if (query === '') {
      setResults([]);
      return;
    }

    updateTimeout = setTimeout(function() {
      updateTimeout = null;
      startRequest(query);
    }, delay);
  }

  searchField.onfocus = function() {
    focused = true;
    display();
  };

  searchField.onblur = function() {
    focused = false;
    display();
  };

  searchField.oninput = function() {
    setQuery(searchField.value);
  };

  searchField.addEventListener('compositionupdate', function(event) {
    composing = event.data;
    setQuery(searchField.value);
  });

  searchField.addEventListener('compositionend', function(event) {
    composing = null;
    setQuery(searchField.value);
  });

  window.addEventListener('mouseup', function() {
    autocomplete.classList.remove('autocomplete-selected');
  });

  window.addEventListener('unload', function() {
    searchField.value = '';
    setQuery('');
  });

  setQuery(searchField.value);
});
