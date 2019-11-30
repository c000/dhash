# dhash

![](https://github.com/c000/dhash/workflows/build/badge.svg)
[![Build status](https://ci.appveyor.com/api/projects/status/mqdnlfphuxcvwc7r/branch/master?svg=true)](https://ci.appveyor.com/project/c000/dhash/branch/master)
[![wercker status](https://app.wercker.com/status/ca38032abdc879e766172810b656e8ed/s/master "wercker status")](https://app.wercker.com/project/byKey/ca38032abdc879e766172810b656e8ed)

## Usage

```
dhash - directory hash

Usage: dhash.exe [--version] [--help] [-v|--verbose]
                 [-h|--hash (SHA512|SHA384|SHA256|SHA224|SHA1|MD5)] FILE...
                 [-r|--recursive]
  take hashes of directory contents

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -h,--hash (SHA512|SHA384|SHA256|SHA224|SHA1|MD5)
                           Hash algorithm (default: MD5)
  FILE...                  Input files
  -r,--recursive           Traverse directory recursively
  -c,--connection-string DSN
                           SQLite database connection string
  --template TEMPLATE      Mustache template for console
                           output (default: "{{{type}}}\t{{{path}}}\t{{{hash}}}\t{{{size}}}")
  -n                       Do not output the trailing newline
```

## Example

```
> dhash -r src
File    src\Util.hs     f39aa0c41633075d9ef841215b164301        190
File    src\Types.hs    592456d885dde6db02a356cbadf417f0        1501
File    src\SQLiteDriver.hs     aa462b1ad0c219c80ebd52238793b881        1306
File    src\Run.hs      f84aded7e5b2f54ca0f7831eba4f7724        588
File    src\Import.hs   133a5d11515cbc3a4431cec93d6a51df        116
File    src\Hash.hs     d629b14b1d91a98e95379f97328dea80        981
File    src\Hash\Algorithms.hs  5001c7f0b26239d849fec839ac0a60cc        296
Directory       src\Hash                296
File    src\FileWalker.hs       5e971452af788a660f4e688f5e643dc4        1537
File    src\ConsoleDriver.hs    92cf7709782d831c0173b5bf053c556e        707
Directory       src             7222
```
