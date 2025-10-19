#!/usr/bin/env bash
# fzf-wrapper-exe

function display_help() {
    echo "usage: $(basename "${0}") <kitty?>" 
    echo "Use fzf to select and run scripts in ~/.local/bin."
    echo "If a positional arguement is provided it will run the script in a"
    echo "tmux session to allow the terminal that called it to exit."
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

function cfzf() {
    fzf --print-query | tail -1
}

bin="$HOME/.local/bin"
exclude="fzf|init-bash-script.sh"

# Select program
prog=$(ls -1 "${bin}" | grep -Ev "${exclude}" | cfzf)
args=""

# Provide follow-on gui selections for relevant programs
case ${prog} in
    "display-switch.sh" )
        args+=" $(echo -e "Living Room\nDesktop" | cfzf)"
        ;;
    "sink-switch.sh" )
        sel=$(echo -e "Living Room\nDesktop" | cfzf)
        # Convert selection to string for sink
        case ${sel} in
            "Living Room" )
                args+=" Dragon"
                ;;
            "Desktop" )
                args+=" Starship"
                ;;
            * )
                args+=" ${sel}"
        esac
        ;;
    "subwoofer-volume.sh" )
        args+=" $(echo -e "-20\n-12\n-9\n-3" | cfzf)"
        ;;
esac

# Run in tmux session if any positional arguement is provided
if [ "$1" ]; then
    tmux kill-session -t fzf-wrapper-exe
    tmux new-session -d -s fzf-wrapper-exe "${bin}/${prog} ${args} | xargs -I {} notify-send {}" 
else
    "${bin}/${prog}" "${args}"
fi
