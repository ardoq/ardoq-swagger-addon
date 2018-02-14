#/bin/bash
circleci tests glob 'test/**/*' | circleci tests split --split-by=filesize | lein test2junit
