'use strict';

window.addEventListener('load', function() {
  const delay = 300;
  const count = 6;

  const searchForm = document.getElementById('search-form');
  const searchField = document.getElementById('search-field');
  const autocomplete = document.getElementById('autocomplete');
  const cache = new Map();

  var currentQuery = null;
  var updateTimeout = null;

  var focused = false;
  var composing = false;
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

  function setSelected(index) {
    if (selected === index) {
      return;
    }

    if (selected !== null) {
      results[selected].row.classList.remove('suggestion-selected');
    } else {
      originalValue = searchField.value;
    }

    selected = index;

    if (selected !== null) {
      const result = results[selected];
      result.row.classList.add('suggestion-selected');
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
      const row = document.createElement('a');
      row.classList.add('suggestion');
      row.classList.add('plain');
      row.href = result.uri;

      const innerDiv = document.createElement('div');

      if (result.word === ':') {
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

      row.appendChild(innerDiv);
      autocomplete.appendChild(row);
      result.row = row;
    }

    display();
  }

  function isInvalidRequest(query) {
    if (query[0] === ':') {
      return true;
    }

    return query.indexOf(' ') != -1 && query.search(/[\[\](){}]/) == -1;
  }

  function startRequest(query) {
    const cached = cache.get(query);
    if (cached !== undefined) {
      if (cached !== null) {
        setResults(cached);
      }
      return;
    }

    if (isInvalidRequest(query)) {
      setResults([]);
      return;
    }

    cache.set(query, null);

    const params = new URLSearchParams();
    params.append('q', query);
    params.append('n', count);

    const request = new XMLHttpRequest();
    request.open('GET', '/api/suggest?' + params, true);
    request.setRequestHeader('Accept', 'application/json');

    function error() {
      console.error(request.statusText);
      if (query === currentQuery) {
        setResults([]);
      }
    }

    request.onload = function() {
      if (request.readyState !== 4) {
        return;
      }

      if (request.status !== 200) {
        error();
        return;
      }

      const response = JSON.parse(request.responseText);
      cache.set(query, response);

      if (query === currentQuery) {
        setResults(response);
      }
    };

    request.onerror = error;
    request.send(null);
  }

  function cancelQuery() {
    currentQuery = null;

    if (updateTimeout !== null) {
      clearTimeout(updateTimeout);
      updateTimeout = null;
    }
  }

  function setQuery(query, noDelay) {
    query = query.trim();

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

  searchField.onfocus = function() {
    focused = true;
    if (currentQuery === null) {
      refreshQuery();
    } else {
      display();
    }
  };

  searchField.onblur = function() {
    focused = false;
    display();
  };

  searchField.oninput = refreshQuery;

  searchField.addEventListener('compositionupdate', function(event) {
    composing = true;
  });

  searchField.addEventListener('compositionend', function(event) {
    setTimeout(function() {
      composing = false;
    }, 0);
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
    if (composing || !results.length) {
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

  autocomplete.addEventListener('mousedown', function(event) {
    autocomplete.classList.add('autocomplete-selected');
    event.preventDefault();
    event.stopPropagation();
  });

  window.addEventListener('mouseup', function() {
    autocomplete.classList.remove('autocomplete-selected');
  });

  window.addEventListener('keydown', function(event) {
    if (!focused && event.code === 'Slash') {
      searchField.focus();
      searchField.select();
      searchField.scrollIntoView(false);
      event.preventDefault();
      event.stopPropagation();
    }
  });

  window.addEventListener('unload', function() {
    searchField.value = '';
    setQuery('');
  });

  if (document.activeElement == searchField) {
    focused = true;
    refreshQuery();
  }
});
