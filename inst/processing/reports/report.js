// Optional enhancement: the report remains readable without JavaScript.
document.addEventListener('DOMContentLoaded', () => {
  if (typeof HTMLDialogElement !== 'undefined') {
    const dialog = document.createElement('dialog');
    dialog.className = 'report-dialog';
    dialog.setAttribute('aria-label', 'Enlarged report figure');
    const close = document.createElement('button');
    close.type = 'button';
    close.className = 'report-dialog-close';
    close.textContent = 'Close figure';
    close.addEventListener('click', () => dialog.close());
    const content = document.createElement('div');
    dialog.append(close, content);
    document.body.append(dialog);
    document.querySelectorAll('[data-enlarge-figure]').forEach(button => {
      button.hidden = false;
      button.addEventListener('click', () => {
        const figure = button.closest('figure');
        content.replaceChildren(figure.querySelector('.metlip-figure').cloneNode(true));
        const caption = figure.querySelector('figcaption').cloneNode(true);
        caption.removeAttribute('id');
        content.append(caption);
        dialog.showModal();
        close.focus();
      });
    });
  }
  let collapsed = [];
  window.addEventListener('beforeprint', () => {
    collapsed = Array.from(document.querySelectorAll('details:not([open])'));
    collapsed.forEach(element => { element.open = true; });
  });
  window.addEventListener('afterprint', () => {
    collapsed.forEach(element => { element.open = false; });
    collapsed = [];
  });
});
