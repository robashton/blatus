# History is nice in shells, let's have that
export ERL_AFLAGS="-kernel shell_history enabled"

# EDTS needs to know where erlexec is
export PATH=$PATH:$(dirname "$(which erl)")/../lib/erlang/erts-10.4/bin/

export LIBVA_DRIVER_NAME=iHD
export LIBVA_DRIVERS_PATH=/run/opengl-driver/lib/dri

# Use nix set-up
source_env ../common/nix.sh
