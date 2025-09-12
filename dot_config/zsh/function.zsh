# Create and then navigate to new dir
mkd() {
  mkdir -p "$1" && cd "$1"
}
