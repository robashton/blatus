use_nix
export IGNORE_EMACS=
export ERL_AFLAGS="-kernel shell_history enabled"

# EDTS needs to know where erlexec is
export PATH=$PATH:$(dirname "$(which erl)")/../lib/erlang/erts-10.4/bin/
