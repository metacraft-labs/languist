# Package

version       = "0.1.0"
author        = "Zahary Karadjov, Alexander Ivanov"
description   = "A framework for translation"
license       = "MIT"
skipDirs      = @["test"]
bin           = @["languist.nim"]
# Dependencies

requires "nim >= 0.19.1", "https://github.com/alehander42/Nim#fix-renderer"

