pure_unit::script_path() {
  # BASH
  if [[ -n "${BASH_SOURCE[0]}" ]]; then
    echo "${BASH_SOURCE[0]}"
    return
  fi

  # ZSH
  # shellcheck disable=2154
  echo "${(%):-%x}"
  return
}

pure_unit::establish_environment() {
  local this_dir
  local proj_dir
  this_dir=$(dirname "$(pure_unit::script_path)")
  proj_dir=$(readlink --canonicalize "${this_dir}/..")

  printf "Running from %s, the project directory is %s...\n" "${this_dir}" "${proj_dir}"

  if [[ -f "${this_dir}/${USER}/dev.sh" ]]; then
    source_env "${this_dir}/${USER}/dev.sh"
  fi
}

pure_unit::establish_environment

unset -f pure_unit::establish_environment
unset -f pure_unit::script_path

