'use strict';

window.addEventListener('load', function() {
  const delay = 300;
  const count = 6;

  const searchWord = document.getElementById('search-word');
  const autocomplete = document.getElementById('autocomplete');
  const cache = new Map();

  var currentQuery = null;
  var updateTimeout = null;

  var focused = false;
  var composing = false;
  var results = [];

  var originalValue = null;
  var selected = null;

  const searchSimple = document.getElementById('search-simpl');
  const searchAdvanced = document.getElementsByClassName('search-adv');

  var advanced = searchSimple.style.display === 'none';

  const searchAdvLink = document.getElementById('search-adv-link');
  const searchClearLink = document.getElementById('search-clear-link');

  const searchDefinition = document.getElementById('search-def');
  const searchKinds = document.getElementsByClassName('search-kind');

  function display() {
    if (focused && results.length && !advanced) {
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
      originalValue = searchWord.value;
    }

    selected = index;

    if (selected !== null) {
      const result = results[selected];
      result.row.classList.add('suggestion-selected');
      searchWord.value = result.completion;
    } else if (originalValue !== null) {
      searchWord.value = originalValue;
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
      innerDiv.innerText = result.word;

      row.appendChild(innerDiv);
      autocomplete.appendChild(row);
      result.row = row;
    }

    display();
  }

  function isInvalidRequest(query) {
    return query.indexOf('"') != -1;
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
    if (advanced) {
      return;
    }

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

  function showAdvanced() {
    if (advanced) {
      return;
    }

    advanced = true;
    cancelQuery();

    searchSimple.style.display = 'none';
    for (const elem of searchAdvanced) {
      elem.style.display = 'block';
    }
    searchDefinition.disabled = false;
  }

  function canTrapColon(query) {
    return query.search(/[{}]/) === -1;
  }

  function refreshQuery() {
    if (advanced) {
      return;
    }

    const query = searchWord.value;

    const colonIndex = query.indexOf(':');
    if (colonIndex !== -1 && canTrapColon(query)) {
      showAdvanced();
      searchWord.value = query.slice(0, colonIndex).trim();
      searchDefinition.value = query.slice(colonIndex + 1).trim();
      setTimeout(function() {
        searchDefinition.focus();
        searchDefinition.setSelectionRange(0, 0);
      }, 0);
      return;
    }

    setQuery(query, false);
  }

  searchWord.onfocus = function() {
    focused = true;
    if (currentQuery === null) {
      refreshQuery();
    } else {
      display();
    }
  };

  searchWord.onblur = function() {
    focused = false;
    display();
  };

  searchWord.oninput = refreshQuery;

  searchWord.addEventListener('compositionupdate', function(event) {
    composing = true;
  });

  searchWord.addEventListener('compositionend', function(event) {
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
    searchWord.value = completion;
    setQuery(completion, true);

    return true;
  }

  searchWord.addEventListener('keydown', function(event) {
    if (composing || advanced || !results.length) {
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
      searchWord.focus();
      searchWord.select();
      searchWord.scrollIntoView(false);
      event.preventDefault();
      event.stopPropagation();
    }
  });

  window.addEventListener('unload', function() {
    searchWord.value = '';
    setQuery('');
  });

  searchAdvLink.onclick = function() {
    showAdvanced();

    return false;
  };

  searchClearLink.onclick = function() {
    searchSimple.style.display = 'block';
    for (const elem of searchAdvanced) {
      elem.style.display = 'none';
    }

    searchDefinition.value = '';
    searchDefinition.disabled = true;
    for (const elem of searchKinds) {
      elem.checked = false;
      elem.disabled = false;
    }

    setTimeout(function() {
      searchWord.focus();
      searchWord.setSelectionRange(0, searchWord.value.length, 'backward');
    }, 0);

    advanced = false;
    refreshQuery();

    return false;
  };

  const checkboxRelations = [
    {
      src: [
        document.getElementById('kind-v'),
      ],
      dst: [
        document.getElementById('kind-tv'),
      ]
    },
    {
      src: [
        document.getElementById('kind-p'),
        document.getElementById('kind-pa'),
      ],
      dst: [
        document.getElementById('kind-sp'),
        document.getElementById('kind-vp'),
      ]
    },
    {
      src: [
        document.getElementById('kind-i'),
      ],
      dst: [
        document.getElementById('kind-ii'),
      ]
    },
  ];

  function checkboxUpdate(relation) {
    var checked = true;
    for (const checkbox of relation.src) {
      if (!checkbox.checked) {
        checked = false;
        break;
      }
    }

    for (const checkbox of relation.dst) {
      if (checkbox.disabled === checked) {
        continue;
      }

      if (checked) {
        checkbox.oldState = checkbox.checked;
        checkbox.checked = true;
        checkbox.disabled = true;
      } else {
        checkbox.disabled = false;
        checkbox.checked = checkbox.oldState;
      }
    }
  }

  for (const relation of checkboxRelations) {
    for (const checkbox of relation.src) {
      checkbox.addEventListener('input', function(event) {
        checkboxUpdate(relation);
      });
    }

    checkboxUpdate(relation);
  }

  if (document.activeElement === searchWord) {
    focused = true;
    refreshQuery();
  }
});
