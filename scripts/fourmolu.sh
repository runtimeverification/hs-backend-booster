#!/usr/bin/env bash
set -euxo pipefail
fourmolu=${FOURMOLU:-$(which fourmolu)} || { echo 'No fourmolu!' ; exit 1 ; }
git ls-files | grep '.*\.hs$' | xargs ${fourmolu} -o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms -o -XBangPatterns -o -XOverloadedRecordDot -i
