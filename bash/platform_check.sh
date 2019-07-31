check() {
    platform=$(uname -s)
    if [[ $platform -eq "Darwin" ]]; then
        echo "The platform is supported:" $platform
    elif [[ $platform -eq "Linux" ]]; then
         echo "The platform is supported:" $platform
    else
        echo "Unsupported platform:" $platform
    fi
}

check
