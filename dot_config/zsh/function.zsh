# Create and then navigate to new dir
mkd() {
  mkdir -p "$1" && cd "$1"
}

# ecc: interactive emacsclient - reports server status, then opens a frame
ecc() {
  if emacsclient -e t &>/dev/null; then
    echo "server: running"
  else
    echo "server: not found - starting daemon..."
    emacs --daemon
  fi
  local server_name
  server_name=$(emacsclient -e 'server-name' | tr -d '"')
  echo "--- emacs state ---"
  emacs --batch --eval "(progn (require 'server) (princ (server-eval-at \"$server_name\" '(let* ((frames (seq-filter (lambda (f) (frame-parameter f 'client)) (frame-list))) (lines (seq-map-indexed (lambda (f i) (format \"frame %d:\\n  workspace: %s\\n  windows (active buffer): %s\" (1+ i) (with-selected-frame f (persp-name (get-current-persp))) (string-join (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list f)) \", \"))) frames))) (string-join lines \"\\n\")))))" 2>/dev/null
  emacsclient -c -n
  echo "\n\nnew frame opened"
}
