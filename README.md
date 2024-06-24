# IO\_Utils
**A terminal IO library for Ada.**
## About
IO\_Utils is a library containing a plethora of terminal procedures and
functions for creating CLI tools in Ada. At this time, these are the following
features:
- ANSI Escape Codes (most of them at least)
    - Include `IO_Utils.Ansi`
- ANSI Escape Code Wrapper for Strings
    - Include `IO_Utils.Strings`
- Checked User Input Collection
    - Include `IO_Utils.User_IO`

## Installation
### Alire Project
0. If you don't have a local crate index, create one
1. Download `manifest.toml` from this repository
2. Move `manifest.toml` to `<path>/<to>/<local>/<index>/io/io_utils-1.0.0/`
3. In project directory:
```
alr index add=<path>/<to>/<local>/<index> name=local # Add the local index
alr with io_utils                                    # Add IO_Utils to project
```

### Building from Source
**Requirements:**
- Alire ([Ada LIbrary Repository](https://alire.ada.dev/))

In the project root directory:
```
$ alr build
```

## License
IO\_Utils is released under the [GPL-3.0 License](https://github.com/KvanQuilst/IO_Utils/blob/main/LICENSE)
