function gb --description "List branches sorted by most recent commit"
    git branch --sort=-committerdate --format='%(committerdate:relative)|%(refname:short)|%(subject)' | while read -d '|' -l date branch subject
        printf '\033[32m%-20s\033[0m %-70s %s\n' $date $branch $subject
    end
end
