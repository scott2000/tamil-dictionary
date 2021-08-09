window.addEventListener('load', function() {
  var link = document.getElementById('other-link');
  var msg = document.getElementById('other-msg');
  var div = document.getElementById('other');

  link.onclick = function() {
    msg.style.display = 'none';
    div.style.display = 'block';
    div.scrollIntoView({
      behavior: 'smooth',
      block: 'start',
      inline: 'start',
    });
    return false;
  };
});
