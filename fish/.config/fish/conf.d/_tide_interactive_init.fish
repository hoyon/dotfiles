status is-interactive || exit

# Set things that wont change
_tide_remove_unusable_items
_tide_detect_os

set -g _tide_left_prompt_display_var _tide_left_prompt_display_$fish_pid
set -gx _tide_right_prompt_display_var _tide_right_prompt_display_$fish_pid

function _tide_background_job --on-event fish_prompt --on-variable fish_bind_mode
    _tide_last_status=$status _tide_last_pipestatus=$pipestatus _tide_jobs_number=(jobs --pid | count) fish --command "
        set CMD_DURATION $CMD_DURATION
        set COLUMNS $COLUMNS
        set fish_bind_mode $fish_bind_mode
        set fish_term24bit $fish_term24bit
        set -U $_tide_left_prompt_display_var (_tide_prompt)" </dev/null &
    # Remove </dev/null in Fish 3.3.0, see https://github.com/fish-shell/fish-shell/issues/7842

    builtin disown

    command kill $_tide_last_pid 2>/dev/null
    set -g _tide_last_pid (jobs --last --pid)
end

function _tide_refresh_prompt --on-variable $_tide_left_prompt_display_var --on-variable $_tide_right_prompt_display_var
    commandline --function repaint
end

# Double underscores to avoid erasing this function on uninstall
function __tide_on_fish_exit --on-event fish_exit
    set -e $_tide_left_prompt_display_var $_tide_right_prompt_display_var
end
