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

  var originalValue = null;
  var selected = null;

  function display() {
    if (focused && results.length) {
      autocomplete.style.display = 'block';
    } else {
      autocomplete.style.display = 'none';
    }
  }

  searchField.onfocus = function() {
    focused = true;
    display();
  };

  searchField.onblur = function() {
    focused = false;
    display();
  };

  function setSelected(index) {
    if (selected === index) {
      return;
    }

    if (selected !== null) {
      results[selected].div.classList.remove('suggestion-selected');
    } else {
      originalValue = searchField.value;
    }

    selected = index;

    if (selected !== null) {
      const result = results[selected];
      result.div.classList.add('suggestion-selected');
      searchField.value = result.completion;
    } else if (originalValue !== null) {
      searchField.value = originalValue;
    }
  }

  function setResults(newResults) {
    originalValue = null;
    setSelected(null);
    results = newResults;

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

      if (result.word === ':') {
        div.classList.add('suggestion-def');

        const querySpan = document.createElement('span');
        querySpan.className = 'suggestion-def-query';
        querySpan.innerText = result.completion + ' ';
        innerDiv.appendChild(querySpan);

        const textSpan = document.createElement('span');
        textSpan.className = 'suggestion-def-text';
        textSpan.innerText = '(definition search)';
        innerDiv.appendChild(textSpan);
      } else {
        innerDiv.innerText = result.word;
      }

      div.appendChild(innerDiv);

      result.div = div;
      autocomplete.appendChild(div);
    }

    display();
  }

  function generalizeQuery(query) {
    if (composing !== null && query.endsWith(composing)) {
      const lastCharacter = query.slice(-1);
      if (consonants.indexOf(lastCharacter) !== -1) {
        return query + '\u0bcd';
      }
    }

    return query;
  }

  function startRequest(query) {
    const cached = cache.get(query);
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
      request = null;
    }

    request.onload = function() {
      if (request.readyState !== 4 || query !== currentQuery) {
        return;
      }

      if (request.status !== 200) {
        error();
        return;
      }

      const response = JSON.parse(request.responseText);
      cache.set(query, response);
      setResults(response);

      request = null;
    };

    request.onerror = error;
    request.send(JSON.stringify({
      count,
      query,
    }));
  }

  function cancelQuery() {
    currentQuery = '';

    if (updateTimeout !== null) {
      clearTimeout(updateTimeout);
      updateTimeout = null;
    }

    if (request !== null) {
      request.abort();
      request = null;
    }
  }

  function setQuery(query, noDelay) {
    query = generalizeQuery(query.trim());

    if (query === currentQuery) {
      return;
    }

    cancelQuery();
    currentQuery = query;

    if (query === '') {
      setResults([]);
      return;
    }

    if (noDelay) {
      startRequest(query);
      return;
    }

    updateTimeout = setTimeout(function() {
      updateTimeout = null;
      startRequest(query);
    }, delay);
  }

  function refreshQuery() {
    setQuery(searchField.value, false);
  }

  searchField.oninput = refreshQuery;

  searchField.addEventListener('compositionupdate', function(event) {
    composing = event.data;
    refreshQuery();
  });

  searchField.addEventListener('compositionend', function(event) {
    composing = null;
    refreshQuery();
  });

  function up() {
    if (selected === null) {
      setSelected(results.length - 1);
      cancelQuery();
    } else if (selected === 0) {
      setSelected(null);
      refreshQuery();
    } else {
      setSelected(selected - 1);
      cancelQuery();
    }
  }

  function down() {
    if (selected === null) {
      setSelected(0);
      cancelQuery();
    } else if (selected >= results.length - 1) {
      setSelected(null);
      refreshQuery();
    } else {
      setSelected(selected + 1);
      cancelQuery();
    }
  }

  function insert() {
    if (selected === null) {
      return false;
    }

    const completion = results[selected].completion;
    searchField.value = completion;
    setQuery(completion, true);

    return true;
  }

  searchField.addEventListener('keydown', function(event) {
    if (composing !== null) {
      return;
    }

    switch (event.code) {
      case 'Escape':
        setSelected(null);
        refreshQuery();
        event.preventDefault();
        break;
      case 'ArrowUp':
        up();
        event.preventDefault();
        break;
      case 'ArrowDown':
        down();
        event.preventDefault();
        break;
      case 'ArrowRight':
      case 'Space':
      case 'Tab':
        if (insert()) {
          event.preventDefault();
        }
        break;
    }
  });

  window.addEventListener('mouseup', function() {
    autocomplete.classList.remove('autocomplete-selected');
  });

  window.addEventListener('unload', function() {
    searchField.value = '';
    setQuery('');
  });

  refreshQuery();
});
