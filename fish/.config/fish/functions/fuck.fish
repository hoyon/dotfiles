function fuck
	if type -q thefuck
      thefuck --alias | source
      fuck
  else
      echo "thefuck not found"
  end
end
